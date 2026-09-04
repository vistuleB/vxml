//// Core VXML tree types, parsers, validators, and serializers.
////
//// VXML is a generic XML-like tree with two node kinds: element nodes (`V`)
//// and text nodes (`T`). The serialized VXML functions validate VXML-format
//// invariants and return `Result`; the XML, HTML, and JSX-like emitters target
//// their respective output formats and return output directly. This module
//// also includes XML/HTML parsing helpers.

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp
import gleam/result
import gleam/string.{inspect as ins}
import glentities/decoder as html_entities
import glentities/named_encoder as html_named_entities
import on
import simplifile
import vxml/blame.{type Blame, prepend_comment as pc} as bl
import vxml/internal/html_repair
import vxml/internal/xml_name
import vxml/io_lines.{type InputLine, type OutputLine, InputLine, OutputLine} as io_l
import vxml/xml_streamer as xs

/// The number of spaces per level in serialized VXML.
pub const vxml_indent = 2

/// The delimiter surrounding text-line contents in serialized VXML.
pub const vxml_line_delimiter = "'"

/// An attribute on a VXML element node.
pub type Attr {
  Attr(blame: Blame, key: String, val: String)
}

/// One line of text inside a VXML text node.
pub type Line {
  Line(blame: Blame, content: String)
}

/// A generic XML-like tree.
///
/// `V` is an element node. `T` is a text node containing one or more lines.
pub type VXML {
  V(blame: Blame, tag: String, attrs: List(Attr), children: List(VXML))
  T(blame: Blame, lines: List(Line))
}

/// A reason an attribute key cannot be represented in serialized VXML.
pub type BadKey {
  /// The key is empty.
  EmptyKey
  /// Contains the complete key followed by the offending character.
  IllegalKeyCharacter(String, String)
}

/// A reason an attribute value cannot be represented in serialized VXML.
pub type BadValue {
  /// Contains the complete value followed by the offending character.
  IllegalValueCharacter(String, String)
  /// Contains the complete value followed by its trailing space or tab.
  TrailingWhitespace(String, String)
}

/// A reason a text node's lines cannot be represented in serialized VXML.
pub type BadLines {
  /// The text node contains no lines.
  NoLines
  /// Identifies the line and character within the text node, followed by the
  /// complete line content and offending character.
  IllegalLineCharacter(
    line_no: Int,
    char_no: Int,
    content: String,
    character: String,
  )
}

/// A reason an element tag does not satisfy the VXML tag grammar.
pub type BadTag {
  /// The tag is empty.
  EmptyTag
  /// Contains the complete tag followed by the required grammar.
  MalformedTag(String, String)
}

/// An error encountered while parsing serialized VXML.
pub type VXMLParseError {
  /// An attribute line contains no `=` assignment marker.
  AttributeAssignmentMissing(blame: Blame, line: String)
  /// An element has a tag that does not satisfy the VXML tag grammar.
  BadTag(blame: Blame, reason: BadTag)
  /// An attribute key is invalid.
  BadAttributeKey(blame: Blame, reason: BadKey)
  /// An attribute value is invalid.
  BadAttributeValue(blame: Blame, reason: BadValue)
  /// A line is indented more deeply than its current syntactic position permits.
  UnexpectedIndentation(blame: Blame, expected: Int, actual: Int, line: String)
  /// A tab occurs where serialized VXML permits only space-based indentation.
  TabInIndentation(blame: Blame, line: String)
  /// A text line does not begin with the VXML line delimiter.
  TextLineOpeningQuoteMissing(blame: Blame, line: String)
  /// A text line does not end with the VXML line delimiter.
  TextLineClosingQuoteMissing(blame: Blame, line: String)
  /// A text-node marker is not followed by any text lines.
  TextNodeLinesMissing(blame: Blame)
  /// A node line does not begin with the `<>` marker.
  NodeMarkerMissing(blame: Blame, line: String)
  /// A singular parser received a number of roots other than one.
  ExpectedOneRoot(actual: Int)
}

/// An I/O or document error encountered while parsing a VXML file.
pub type VXMLParsePathError {
  /// Reading the path failed.
  IOError(simplifile.FileError)
  /// The file was read but its contents were not valid serialized VXML.
  DocumentError(VXMLParseError)
}

/// An error encountered while parsing XML-like input.
pub type XMLParseError {
  XMLParseError(blame: Blame, message: String)
}

/// An invalid exception passed to an HTML-entity normalization helper.
pub type HTMLEntityNormalizationError {
  /// Exception strings must have the form `&name;`, `&#decimal;`, or `&#xhex;`.
  MalformedHTMLEntityException(String)
  /// The exception has entity syntax but is not recognized as an HTML entity.
  UnrecognizedHTMLEntityException(String)
}

/// The invalid part of a VXML value found during validation or serialization.
pub type VXMLInvalidityReason {
  TagIsBad(BadTag)
  AttributeKeyIsBad(BadKey)
  AttributeValueIsBad(BadValue)
  LinesAreBad(BadLines)
}

/// A VXML tree-validation failure and the provenance of the invalid value.
pub type VXMLValidationError {
  VXMLValidationError(blame: Blame, reason: VXMLInvalidityReason)
}

/// A serialized-VXML failure with the valid output produced before it.
pub type VXMLSerializationError {
  VXMLSerializationError(
    partial: List(OutputLine),
    blame: Blame,
    reason: VXMLInvalidityReason,
  )
}

const vxml_illegal_attr_key_characters = ["=", " ", "\t", "\n", "\r"]

const vxml_illegal_attr_value_characters = ["\n", "\r"]

const vxml_illegal_text_characters = ["\n", "\r"]

fn contains_chars(thing: String, substrings: List(String)) -> String {
  case substrings {
    [] -> ""

    [first, ..rest] -> {
      case string.contains(thing, first) {
        True -> first
        False -> contains_chars(thing, rest)
      }
    }
  }
}

/// Validates an attribute key for the VXML text format.
pub fn validate_key(key: String) -> Result(String, BadKey) {
  case key {
    "" -> Error(EmptyKey)
    _ -> {
      let bad_char = contains_chars(key, vxml_illegal_attr_key_characters)
      case bad_char == "" {
        True -> Ok(key)
        False -> Error(IllegalKeyCharacter(key, bad_char))
      }
    }
  }
}

/// Validates an attribute value for the VXML text format.
pub fn validate_value(value: String) -> Result(String, BadValue) {
  case contains_chars(value, vxml_illegal_attr_value_characters) {
    "" ->
      case string.ends_with(value, " "), string.ends_with(value, "\t") {
        True, _ -> Error(TrailingWhitespace(value, " "))
        _, True -> Error(TrailingWhitespace(value, "\t"))
        False, False -> Ok(value)
      }
    illegal_character -> Error(IllegalValueCharacter(value, illegal_character))
  }
}

fn validate_line(content: String, line_no: Int) -> Result(String, BadLines) {
  let illegal_character =
    content
    |> string.to_graphemes
    |> list.index_map(fn(character, index) { #(index, character) })
    |> list.find(fn(indexed) {
      list.contains(vxml_illegal_text_characters, indexed.1)
    })
  case illegal_character {
    Error(_) -> Ok(content)
    Ok(#(index, character)) ->
      Error(IllegalLineCharacter(line_no, index + 1, content, character))
  }
}

/// Validates an element tag for the VXML text format.
pub fn validate_tag(tag: String) -> Result(String, BadTag) {
  case tag, xml_name.is_name(tag) {
    "", _ -> Error(EmptyTag)
    _, True -> Ok(tag)
    _, False -> Error(MalformedTag(tag, xml_name.grammar))
  }
}

fn validate_attrs(attrs: List(Attr)) -> Result(Nil, VXMLValidationError) {
  list.try_each(attrs, fn(attr) {
    use _ <- result.try(
      validate_key(attr.key)
      |> result.map_error(fn(problem) {
        VXMLValidationError(attr.blame, AttributeKeyIsBad(problem))
      }),
    )
    use _ <- result.try(
      validate_value(attr.val)
      |> result.map_error(fn(problem) {
        VXMLValidationError(attr.blame, AttributeValueIsBad(problem))
      }),
    )
    Ok(Nil)
  })
}

fn validate_lines(
  blame: Blame,
  lines: List(Line),
) -> Result(Nil, VXMLValidationError) {
  case lines {
    [] -> Error(VXMLValidationError(blame, LinesAreBad(NoLines)))
    _ ->
      lines
      |> list.index_map(fn(line, index) { #(line, index) })
      |> list.try_each(fn(indexed) {
        let #(line, index) = indexed
        validate_line(line.content, index + 1)
        |> result.map(fn(_) { Nil })
        |> result.map_error(fn(problem) {
          VXMLValidationError(line.blame, LinesAreBad(problem))
        })
      })
  }
}

fn validate_vxmls(vxmls: List(VXML)) -> Result(Nil, VXMLValidationError) {
  list.try_each(vxmls, validate)
}

/// Validates a complete VXML tree against the VXML text-format rules.
///
/// This checks element tags, attribute keys and values, text contents, and the
/// requirement that every text node contain at least one line. Leading spaces
/// and tabs in attribute values are preserved; trailing spaces and tabs are
/// rejected.
pub fn validate(vxml: VXML) -> Result(Nil, VXMLValidationError) {
  case vxml {
    T(blame, lines) -> validate_lines(blame, lines)
    V(blame, tag, attrs, children) -> {
      use _ <- result.try(
        validate_tag(tag)
        |> result.map_error(fn(problem) {
          VXMLValidationError(blame, TagIsBad(problem))
        }),
      )
      use _ <- result.try(validate_attrs(attrs))
      validate_vxmls(children)
    }
  }
}

// ************************************************************
// serialized VXML parser
// ************************************************************

fn reject_tab_indentation(
  blame: Blame,
  suffix: String,
) -> Result(Nil, VXMLParseError) {
  case string.starts_with(suffix, "\t") {
    True -> Error(TabInIndentation(blame, suffix))
    False -> Ok(Nil)
  }
}

fn parse_text_lines_at_indent(
  indent: Int,
  head: List(InputLine),
) -> Result(#(List(Line), List(InputLine)), VXMLParseError) {
  // no lines left
  use InputLine(blame, suffix_indent, suffix), rest <- on.empty_nonempty(
    head,
    fn() { Ok(#([], head)) },
  )

  // empty suffix
  use <- on.true_false(suffix == "", fn() {
    parse_text_lines_at_indent(indent, rest)
  })

  use _ <- result.try(reject_tab_indentation(blame, suffix))

  // indent too large
  use <- on.true_false(suffix_indent > indent, fn() {
    Error(UnexpectedIndentation(blame, indent, suffix_indent, suffix))
  })

  // indent too small
  use <- on.true_false(suffix_indent < indent, fn() { Ok(#([], head)) })

  let suffix = string.trim_end(suffix)

  // missing opening quote
  use <- on.false_true(suffix |> string.starts_with(vxml_line_delimiter), fn() {
    Error(TextLineOpeningQuoteMissing(blame, suffix))
  })

  let content = suffix |> string.drop_start(1)

  // missing closing quote
  use <- on.false_true(content |> string.ends_with(vxml_line_delimiter), fn() {
    Error(TextLineClosingQuoteMissing(blame, suffix))
  })

  let content = content |> string.drop_end(1)
  let line = Line(blame, content)
  use #(lines, after) <- on.ok(parse_text_lines_at_indent(indent, rest))
  Ok(#([line, ..lines], after))
}

fn parse_attributes_at_indent(
  indent: Int,
  head: List(InputLine),
) -> Result(#(List(Attr), List(InputLine)), VXMLParseError) {
  // no lines left
  use InputLine(blame, suffix_indent, suffix), rest <- on.empty_nonempty(
    head,
    fn() { Ok(#([], head)) },
  )

  // empty suffix
  use <- on.true_false(suffix == "", fn() {
    parse_attributes_at_indent(indent, rest)
  })

  use _ <- result.try(reject_tab_indentation(blame, suffix))

  // indent too large
  use <- on.true_false(suffix_indent > indent, fn() {
    Error(UnexpectedIndentation(blame, indent, suffix_indent, suffix))
  })

  // indent too small
  use <- on.true_false(suffix_indent < indent, fn() { Ok(#([], head)) })

  // tag
  use <- on.true_false(suffix |> string.starts_with("<>"), fn() {
    Ok(#([], head))
  })

  // missing '='
  use #(key, val) <- on.error_ok(suffix |> string.split_once("="), fn(_) {
    Error(AttributeAssignmentMissing(blame, suffix))
  })

  // bad key
  use _ <- on.error_ok(validate_key(key), fn(e) {
    Error(BadAttributeKey(blame, e))
  })

  // bad value
  use val <- on.error_ok(validate_value(val), fn(e) {
    Error(BadAttributeValue(blame, e))
  })

  let attr = Attr(blame, key, val)
  use #(attrs, after) <- on.ok(parse_attributes_at_indent(indent, rest))
  let attrs = [attr, ..attrs]
  Ok(#(attrs, after))
}

fn parse_nodes_at_indent(
  indent: Int,
  head: List(InputLine),
) -> Result(#(List(VXML), List(InputLine)), VXMLParseError) {
  // no lines left
  use InputLine(blame, suffix_indent, suffix), rest <- on.empty_nonempty(
    head,
    fn() { Ok(#([], head)) },
  )

  // empty suffix
  use <- on.true_false(suffix == "", fn() {
    parse_nodes_at_indent(indent, rest)
  })

  use _ <- result.try(reject_tab_indentation(blame, suffix))

  // indent too large
  use <- on.true_false(suffix_indent > indent, fn() {
    Error(UnexpectedIndentation(blame, indent, suffix_indent, suffix))
  })

  // indent too small
  use <- on.true_false(suffix_indent < indent, fn() { Ok(#([], head)) })

  // not a tag
  use <- on.false_true(suffix |> string.starts_with("<>"), fn() {
    Error(NodeMarkerMissing(blame, suffix))
  })

  let tag = suffix |> string.drop_start(2) |> string.trim

  case tag {
    // text node
    "" -> {
      use #(lines, after) <- on.ok(parse_text_lines_at_indent(
        indent + vxml_indent,
        rest,
      ))
      case lines {
        [] -> Error(TextNodeLinesMissing(blame))
        _ -> {
          let node = T(blame, lines)
          use #(nodes, after) <- on.ok(parse_nodes_at_indent(indent, after))
          Ok(#([node, ..nodes], after))
        }
      }
    }
    // tag
    _ -> {
      use _ <- on.error_ok(validate_tag(tag), fn(e) { Error(BadTag(blame, e)) })
      use #(attrs, after) <- on.ok(parse_attributes_at_indent(
        indent + vxml_indent,
        rest,
      ))
      use #(children, after) <- on.ok(parse_nodes_at_indent(
        indent + vxml_indent,
        after,
      ))
      let node = V(blame |> bl.set_anchored, tag, attrs, children)
      use #(nodes, after) <- on.ok(parse_nodes_at_indent(indent, after))
      Ok(#([node, ..nodes], after))
    }
  }
}

// ************************************************************
// HTML entity normalization
// ************************************************************

/// HTML syntax entities commonly worth excluding from broad normalization.
pub const html_syntax_entities = ["&amp;", "&lt;", "&gt;", "&quot;", "&apos;"]

/// HTML spacing entities commonly worth keeping visible in source text.
pub const html_spacing_entities = ["&nbsp;", "&ensp;", "&emsp;", "&thinsp;"]

/// HTML invisible entities commonly worth keeping visible in source text.
pub const html_invisible_entities = ["&shy;", "&zwj;", "&zwnj;"]

/// HTML spacing and invisible entities commonly worth keeping as entities.
pub const html_layout_entities = [
  "&nbsp;", "&ensp;", "&emsp;", "&thinsp;", "&shy;", "&zwj;", "&zwnj;",
]

fn normalize_vxml_strings(
  vxml: VXML,
  normalize_line: fn(String) -> String,
  normalize_attr_val: fn(String) -> String,
) -> VXML {
  case vxml {
    T(blame, lines) ->
      T(
        blame,
        lines
          |> list.map(fn(line) {
            Line(..line, content: normalize_line(line.content))
          }),
      )
    V(blame, tag, attrs, children) ->
      V(
        blame,
        tag,
        attrs
          |> list.map(fn(attr) {
            Attr(..attr, val: normalize_attr_val(attr.val))
          }),
        children
          |> list.map(normalize_vxml_strings(
            _,
            normalize_line,
            normalize_attr_val,
          )),
      )
  }
}

fn decode_html_entity_name(name: String) -> Result(String, Nil) {
  case name {
    "#x" <> rest | "#X" <> rest -> {
      let decoded = html_entities.decode_hex(rest)
      case decoded == rest {
        True -> Error(Nil)
        False -> Ok(decoded)
      }
    }
    "#" <> rest -> {
      let decoded = html_entities.decode_dec(rest)
      case decoded == rest {
        True -> Error(Nil)
        False -> Ok(decoded)
      }
    }
    _ -> {
      let decoded = html_entities.decode_named(name)
      case decoded == "&" <> name <> ";" {
        True -> Error(Nil)
        False -> Ok(decoded)
      }
    }
  }
}

fn validate_html_entity_exception(
  exception: String,
) -> Result(#(String, String), HTMLEntityNormalizationError) {
  case string.starts_with(exception, "&") && string.ends_with(exception, ";") {
    False -> Error(MalformedHTMLEntityException(exception))
    True -> {
      let name =
        exception
        |> string.drop_start(1)
        |> string.drop_end(1)
      use decoded <- result.try(
        decode_html_entity_name(name)
        |> result.map_error(fn(_) { UnrecognizedHTMLEntityException(exception) }),
      )
      Ok(#(exception, decoded))
    }
  }
}

fn validate_html_entity_exceptions(
  exceptions: List(String),
) -> Result(List(#(String, String)), HTMLEntityNormalizationError) {
  list.try_map(exceptions, validate_html_entity_exception)
}

fn html_entities_to_unicode_string_loop(
  graphemes: List(String),
  exceptions: List(String),
  previous: List(String),
) -> String {
  case graphemes {
    [] -> previous |> list.reverse |> string.concat
    ["&", ..rest] -> {
      let #(name, rest, closed) = take_html_entity_candidate(rest, [])
      let spelling = case closed {
        True -> "&" <> name <> ";"
        False -> "&" <> name
      }
      let replacement = case closed {
        False -> spelling
        True ->
          case list.contains(exceptions, spelling) {
            True -> spelling
            False ->
              case decode_html_entity_name(name) {
                Ok(decoded) -> decoded
                Error(_) -> spelling
              }
          }
      }
      html_entities_to_unicode_string_loop(rest, exceptions, [
        replacement,
        ..previous
      ])
    }
    [first, ..rest] ->
      html_entities_to_unicode_string_loop(rest, exceptions, [first, ..previous])
  }
}

fn html_entities_to_unicode_string(
  content: String,
  exceptions: List(String),
) -> String {
  content
  |> string.to_graphemes
  |> html_entities_to_unicode_string_loop(exceptions, [])
}

fn unicode_to_named_html_entities_string_loop(
  graphemes: List(String),
  except_characters: List(String),
  previous: List(String),
) -> String {
  case graphemes {
    [] -> previous |> list.reverse |> string.concat
    [first, ..rest] -> {
      let replacement = case list.contains(except_characters, first) {
        True -> first
        False -> html_named_entities.encode(first)
      }
      unicode_to_named_html_entities_string_loop(rest, except_characters, [
        replacement,
        ..previous
      ])
    }
  }
}

fn unicode_to_named_html_entities_string(
  content: String,
  except_characters: List(String),
) -> String {
  content
  |> string.to_graphemes
  |> unicode_to_named_html_entities_string_loop(except_characters, [])
}

/// Decodes recognized HTML entities in text lines and attribute values.
///
/// Exceptions should be supplied as literal HTML entity strings, such as
/// `&ensp;` or `&#xA0;`; exact occurrences of those spellings are preserved.
pub fn html_entities_to_unicode(
  vxml: VXML,
  except exceptions: List(String),
) -> Result(VXML, HTMLEntityNormalizationError) {
  use _ <- result.try(validate_html_entity_exceptions(exceptions))
  Ok(
    normalize_vxml_strings(
      vxml,
      html_entities_to_unicode_string(_, exceptions),
      html_entities_to_unicode_string(_, exceptions),
    ),
  )
}

/// Encodes Unicode characters in text lines and attribute values as named HTML
/// entities whenever `glentities` has a named entity for the character.
///
/// Exceptions should be supplied as literal HTML entity strings, such as
/// `&ensp;`; their decoded Unicode characters are preserved.
pub fn unicode_to_named_html_entities(
  vxml: VXML,
  except exceptions: List(String),
) -> Result(VXML, HTMLEntityNormalizationError) {
  use exception_pairs <- result.try(validate_html_entity_exceptions(exceptions))
  let except_characters = exception_pairs |> list.map(fn(pair) { pair.1 })
  Ok(
    normalize_vxml_strings(
      vxml,
      unicode_to_named_html_entities_string(_, except_characters),
      unicode_to_named_html_entities_string(_, except_characters),
    ),
  )
}

// ************************************************************
// debug annotating VXML blames (esoteric)
// ************************************************************

/// Adds structural descriptions to blame comments throughout a VXML tree.
/// This is intended for diagnostic tables. Blame identities and tree contents
/// are otherwise unchanged.
pub fn annotate_blames(vxml: VXML) -> VXML {
  case vxml {
    T(blame, lines) -> {
      T(
        blame |> pc("T"),
        list.index_map(lines, fn(line, i) {
          Line(
            line.blame
              |> pc("T > Line(" <> ins(i + 1) <> ")"),
            line.content,
          )
        }),
      )
    }
    V(blame, tag, attrs, children) -> {
      V(
        blame |> pc("V"),
        tag,
        list.index_map(attrs, fn(attr, i) {
          Attr(
            attr.blame |> pc("Attr(" <> ins(i + 1) <> ")"),
            attr.key,
            attr.val,
          )
        }),
        list.map(children, annotate_blames),
      )
    }
  }
}

fn delimit(s: String) -> String {
  vxml_line_delimiter <> s <> vxml_line_delimiter
}

fn serialize_text_lines(
  lines: List(Line),
  line_no: Int,
  indentation: Int,
  partial_reversed: List(OutputLine),
) -> Result(List(OutputLine), VXMLSerializationError) {
  case lines {
    [] -> Ok(partial_reversed)
    [line, ..rest] ->
      case validate_line(line.content, line_no) {
        Error(error) ->
          Error(VXMLSerializationError(
            list.reverse(partial_reversed),
            line.blame,
            LinesAreBad(error),
          ))
        Ok(content) ->
          serialize_text_lines(rest, line_no + 1, indentation, [
            OutputLine(line.blame, indentation, delimit(content)),
            ..partial_reversed
          ])
      }
  }
}

fn serialize_attributes(
  attrs: List(Attr),
  indentation: Int,
  partial_reversed: List(OutputLine),
) -> Result(List(OutputLine), VXMLSerializationError) {
  case attrs {
    [] -> Ok(partial_reversed)
    [attr, ..rest] ->
      case validate_key(attr.key) {
        Error(error) ->
          Error(VXMLSerializationError(
            list.reverse(partial_reversed),
            attr.blame,
            AttributeKeyIsBad(error),
          ))
        Ok(key) ->
          case validate_value(attr.val) {
            Error(error) ->
              Error(VXMLSerializationError(
                list.reverse(partial_reversed),
                attr.blame,
                AttributeValueIsBad(error),
              ))
            Ok(value) ->
              serialize_attributes(rest, indentation, [
                OutputLine(attr.blame, indentation, key <> "=" <> value),
                ..partial_reversed
              ])
          }
      }
  }
}

fn serialize_vxmls(
  vxmls: List(VXML),
  indentation: Int,
  partial_reversed: List(OutputLine),
) -> Result(List(OutputLine), VXMLSerializationError) {
  case vxmls {
    [] -> Ok(partial_reversed)
    [vxml, ..rest] -> {
      use partial_reversed <- result.try(vxml_to_output_lines_internal(
        vxml,
        indentation,
        partial_reversed,
      ))
      serialize_vxmls(rest, indentation, partial_reversed)
    }
  }
}

fn vxml_to_output_lines_internal(
  vxml: VXML,
  indentation: Int,
  partial_reversed: List(OutputLine),
) -> Result(List(OutputLine), VXMLSerializationError) {
  case vxml {
    T(blame, lines) ->
      case lines {
        [] ->
          Error(VXMLSerializationError(
            list.reverse(partial_reversed),
            blame,
            LinesAreBad(NoLines),
          ))
        _ ->
          serialize_text_lines(lines, 1, indentation + vxml_indent, [
            OutputLine(blame, indentation, "<>"),
            ..partial_reversed
          ])
      }

    V(blame, tag, attrs, children) ->
      case validate_tag(tag) {
        Error(error) ->
          Error(VXMLSerializationError(
            list.reverse(partial_reversed),
            blame,
            TagIsBad(error),
          ))
        Ok(tag) -> {
          let partial_reversed = [
            OutputLine(blame, indentation, "<> " <> tag),
            ..partial_reversed
          ]
          use partial_reversed <- result.try(serialize_attributes(
            attrs,
            indentation + vxml_indent,
            partial_reversed,
          ))
          serialize_vxmls(children, indentation + vxml_indent, partial_reversed)
        }
      }
  }
}

// ************************************************************
// VXML -> List(OutputLine) api
// ************************************************************

/// Serializes one VXML node to VXML text-format output lines.
pub fn vxml_to_output_lines(
  vxml: VXML,
) -> Result(List(OutputLine), VXMLSerializationError) {
  vxml_to_output_lines_internal(vxml, 0, [])
  |> result.map(list.reverse)
}

/// Serializes VXML nodes to VXML text-format output lines in the same order.
pub fn vxmls_to_output_lines(
  vxmls: List(VXML),
) -> Result(List(OutputLine), VXMLSerializationError) {
  serialize_vxmls(vxmls, 0, [])
  |> result.map(list.reverse)
}

// ************************************************************
// VXML -> String api
// ************************************************************

/// Serializes one VXML node to the VXML text format.
pub fn vxml_to_string(vxml: VXML) -> Result(String, VXMLSerializationError) {
  vxml
  |> vxml_to_output_lines
  |> result.map(io_l.output_lines_to_string)
}

/// Serializes VXML nodes to the VXML text format in the same order.
pub fn vxmls_to_string(
  vxmls: List(VXML),
) -> Result(String, VXMLSerializationError) {
  vxmls
  |> vxmls_to_output_lines
  |> result.map(io_l.output_lines_to_string)
}

// ************************************************************
// VXML debug table
// ************************************************************

/// Renders one VXML tree as a blame-annotated diagnostic table.
pub fn vxml_table(
  vxml: VXML,
  banner: String,
  indent: Int,
) -> Result(String, VXMLSerializationError) {
  vxml
  |> vxml_to_output_lines
  |> result.map(io_l.output_lines_table(_, banner, indent))
}

/// Parses input lines containing zero or more VXML roots.
///
/// Blank physical lines are ignored.
pub fn input_lines_to_vxmls(
  lines: List(io_l.InputLine),
) -> Result(List(VXML), VXMLParseError) {
  use #(vxmls, after) <- on.ok(parse_nodes_at_indent(0, lines))
  assert after == []
  Ok(vxmls)
}

/// Parses input lines containing exactly one VXML root.
///
/// Blank physical lines are ignored. Any root count other than one returns
/// `ExpectedOneRoot`.
pub fn input_lines_to_vxml(
  lines: List(io_l.InputLine),
) -> Result(VXML, VXMLParseError) {
  use vxmls <- result.try(input_lines_to_vxmls(lines))
  case vxmls {
    [vxml] -> Ok(vxml)
    _ -> Error(ExpectedOneRoot(list.length(vxmls)))
  }
}

// ************************************************************
// String -> VXML
// ************************************************************

/// Parses a string containing zero or more VXML roots.
///
/// `filename` is recorded in source blame and has no other semantic effect.
/// Blank physical lines are ignored.
pub fn string_to_vxmls(
  source: String,
  filename: String,
) -> Result(List(VXML), VXMLParseError) {
  source
  |> io_l.string_to_input_lines(filename, 0)
  |> input_lines_to_vxmls
}

/// Parses a string containing exactly one VXML root.
///
/// `filename` is recorded in source blame and has no other semantic effect.
/// Blank physical lines are ignored. Any root count other than one returns
/// `ExpectedOneRoot`.
pub fn string_to_vxml(
  source: String,
  filename: String,
) -> Result(VXML, VXMLParseError) {
  source
  |> io_l.string_to_input_lines(filename, 0)
  |> input_lines_to_vxml
}

// ************************************************************
// Path -> VXML
// ************************************************************

/// Parses a path containing zero or more VXML roots.
///
/// Returns `IOError` when reading fails and `DocumentError` when parsing fails.
pub fn path_to_vxmls(path: String) -> Result(List(VXML), VXMLParsePathError) {
  use contents <- on.error_ok(simplifile.read(path), fn(io_error) {
    Error(IOError(io_error))
  })

  string_to_vxmls(contents, path)
  |> result.map_error(fn(e) { DocumentError(e) })
}

/// Parses a path containing exactly one VXML root.
///
/// Returns `IOError` when reading fails and `DocumentError` when parsing fails.
/// A root count other than one is a `DocumentError(ExpectedOneRoot(..))`.
pub fn path_to_vxml(path: String) -> Result(VXML, VXMLParsePathError) {
  use contents <- on.error_ok(simplifile.read(path), fn(io_error) {
    Error(IOError(io_error))
  })

  string_to_vxml(contents, path)
  |> result.map_error(fn(e) { DocumentError(e) })
}

fn jsx_text_escape(content: String) -> String {
  content
  |> string.replace("&", "&amp;")
  |> string.replace("{", "&#123;")
  |> string.replace("}", "&#125;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
}

fn jsx_attribute_escape(content: String) -> String {
  content
  |> jsx_text_escape
  |> string.replace("\"", "&quot;")
}

fn is_decimal_integer(content: String) -> Bool {
  let assert Ok(re) = regexp.from_string("^-?[0-9]+$")
  regexp.check(re, content)
}

fn jsx_key_val(attr: Attr) -> String {
  case
    attr.val == "false" || attr.val == "true" || is_decimal_integer(attr.val)
  {
    True -> attr.key <> "={" <> attr.val <> "}"
    False -> attr.key <> "=\"" <> jsx_attribute_escape(attr.val) <> "\""
  }
}

fn jsx_attr_output_line(attr: Attr, indent: Int) -> OutputLine {
  OutputLine(blame: attr.blame, indent: indent, suffix: jsx_key_val(attr))
}

fn jsx_tag_close_output_lines(
  blame: Blame,
  tag: String,
  indent: Int,
) -> List(OutputLine) {
  [OutputLine(blame: blame, indent: indent, suffix: "</" <> tag <> ">")]
}

fn jsx_tag_open_output_lines(
  blame: Blame,
  tag: String,
  indent: Int,
  closing_same_line: String,
  closing_different_line: String,
  attrs: List(Attr),
  indentation: Int,
) -> List(OutputLine) {
  case attrs {
    [] -> [
      OutputLine(
        blame: blame,
        indent: indent,
        suffix: "<" <> tag <> closing_same_line,
      ),
    ]
    [first] -> [
      OutputLine(
        blame: blame,
        indent: indent,
        suffix: "<" <> tag <> " " <> jsx_key_val(first) <> closing_same_line,
      ),
    ]
    _ -> {
      [
        [OutputLine(blame: blame, indent: indent, suffix: "<" <> tag)],
        attrs
          |> list.map(jsx_attr_output_line(_, indent + indentation)),
        [
          OutputLine(
            blame: blame,
            indent: indent,
            suffix: closing_different_line,
          ),
        ],
      ]
      |> list.flatten
    }
  }
}

fn bool_2_jsx_space(b: Bool) -> String {
  case b {
    True -> "{\" \"}"
    False -> ""
  }
}

fn vxml_to_jsx_output_lines_internal(
  vxml: VXML,
  indent: Int,
  indentation: Int,
) -> List(OutputLine) {
  case vxml {
    T(_, lines) -> {
      let n = list.length(lines)
      lines
      |> list.index_map(fn(t, i) {
        OutputLine(blame: t.blame, indent: indent, suffix: {
          let content = jsx_text_escape(t.content)
          let start =
            {
              i == 0
              && {
                string.starts_with(content, " ") || string.is_empty(content)
              }
            }
            |> bool_2_jsx_space
          let end =
            {
              i == n - 1
              && { string.ends_with(content, " ") || string.is_empty(content) }
            }
            |> bool_2_jsx_space
          start <> content <> end
        })
      })
    }

    V(blame, tag, attrs, children) -> {
      case list.is_empty(children) {
        False ->
          [
            jsx_tag_open_output_lines(
              blame,
              tag,
              indent,
              ">",
              ">",
              attrs,
              indentation,
            ),
            children
              |> list.map(vxml_to_jsx_output_lines_internal(
                _,
                indent + indentation,
                indentation,
              ))
              |> list.flatten,
            jsx_tag_close_output_lines(blame, tag, indent),
          ]
          |> list.flatten

        True ->
          jsx_tag_open_output_lines(
            blame,
            tag,
            indent,
            " />",
            "/>",
            attrs,
            indentation,
          )
      }
    }
  }
}

// ************************************************************
// VXML -> jsx blamed lines
// ************************************************************

/// Serializes one VXML node to JSX-like output lines.
pub fn vxml_to_jsx_output_lines(
  vxml: VXML,
  starting_indent: Int,
  indentation: Int,
) -> List(OutputLine) {
  vxml_to_jsx_output_lines_internal(vxml, starting_indent, indentation)
}

/// Serializes VXML nodes to JSX-like output lines in the same order.
pub fn vxmls_to_jsx_output_lines(
  vxmls: List(VXML),
  starting_indent: Int,
  indentation: Int,
) -> List(OutputLine) {
  vxmls
  |> list.map(vxml_to_jsx_output_lines_internal(_, starting_indent, indentation))
  |> list.flatten
}

// ************************************************************
// VXML -> jsx string
// ************************************************************

/// Serializes one VXML node to a JSX-like string.
pub fn vxml_to_jsx(
  vxml: VXML,
  starting_indent: Int,
  indentation: Int,
) -> String {
  vxml
  |> vxml_to_jsx_output_lines(starting_indent, indentation)
  |> io_l.output_lines_to_string
}

/// Serializes VXML nodes to one JSX-like string in the same order.
pub fn vxmls_to_jsx(
  vxmls: List(VXML),
  starting_indent: Int,
  indentation: Int,
) -> String {
  vxmls
  |> vxmls_to_jsx_output_lines(starting_indent, indentation)
  |> io_l.output_lines_to_string
}

// ************************************************************
// VXML -> XML
// ************************************************************

type CompactXMLLine {
  CompactXMLLine(blame: Blame, content: String)
}

fn xml_text_escape(content: String) -> String {
  content
  |> string.replace("&", "&amp;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
}

fn xml_attribute_escape(content: String) -> String {
  content
  |> xml_text_escape
  |> string.replace("\"", "&quot;")
  |> string.replace("\t", "&#9;")
  |> string.replace("\n", "&#10;")
  |> string.replace("\r", "&#13;")
}

fn html_entity_name_is_known(name: String) -> Bool {
  html_entities.decode_named(name) != "&" <> name <> ";"
}

fn html_entity_name_is_valid_numeric(name: String) -> Bool {
  case name {
    "#x" <> rest | "#X" <> rest -> html_entities.decode_hex(rest) != rest
    "#" <> rest -> html_entities.decode_dec(rest) != rest
    _ -> False
  }
}

fn html_entity_name_is_valid(name: String) -> Bool {
  html_entity_name_is_known(name) || html_entity_name_is_valid_numeric(name)
}

fn html_entity_name_character(character: String) -> Bool {
  let assert Ok(re) = regexp.from_string("^[A-Za-z0-9#xX]$")
  regexp.check(re, character)
}

fn take_html_entity_candidate(
  rest: List(String),
  previous: List(String),
) -> #(String, List(String), Bool) {
  case rest {
    [] -> #(previous |> list.reverse |> string.concat, [], False)
    [";", ..rest] -> #(previous |> list.reverse |> string.concat, rest, True)
    [first, ..rest] ->
      case html_entity_name_character(first) {
        True -> take_html_entity_candidate(rest, [first, ..previous])
        False -> #(
          previous |> list.reverse |> string.concat,
          [first, ..rest],
          False,
        )
      }
  }
}

fn html_ampersand_escape_preserving_entities_loop(
  graphemes: List(String),
  previous: List(String),
) -> String {
  case graphemes {
    [] -> previous |> list.reverse |> string.concat
    ["&", ..rest] -> {
      let #(name, rest, closed) = take_html_entity_candidate(rest, [])
      let valid = case closed {
        True -> html_entity_name_is_valid(name)
        False -> False
      }
      let suffix = case closed {
        True -> name <> ";"
        False -> name
      }
      let replacement = case valid {
        True -> "&" <> suffix
        False -> "&amp;" <> suffix
      }
      html_ampersand_escape_preserving_entities_loop(rest, [
        replacement,
        ..previous
      ])
    }
    [first, ..rest] ->
      html_ampersand_escape_preserving_entities_loop(rest, [first, ..previous])
  }
}

fn html_ampersand_escape_preserving_entities(content: String) -> String {
  content
  |> string.to_graphemes
  |> html_ampersand_escape_preserving_entities_loop([])
}

fn html_text_escape(content: String) -> String {
  let content = html_ampersand_escape_preserving_entities(content)
  content
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
}

fn html_attribute_escape(content: String) -> String {
  content
  |> html_text_escape
  |> string.replace("\"", "&quot;")
  |> string.replace("\t", "&#9;")
  |> string.replace("\n", "&#10;")
  |> string.replace("\r", "&#13;")
}

fn xml_attrs(attrs: List(Attr)) -> String {
  attrs
  |> list.map(fn(attr) {
    " " <> attr.key <> "=\"" <> xml_attribute_escape(attr.val) <> "\""
  })
  |> string.concat
}

fn append_compact_xml_lines(
  before: List(CompactXMLLine),
  after: List(CompactXMLLine),
) -> List(CompactXMLLine) {
  case before, after {
    [], _ -> after
    _, [] -> before
    _, [first_after, ..rest_after] -> {
      let assert [last_before, ..rest_before_reversed] = list.reverse(before)
      list.append(list.reverse(rest_before_reversed), [
        CompactXMLLine(
          last_before.blame,
          last_before.content <> first_after.content,
        ),
        ..rest_after
      ])
    }
  }
}

fn compact_xml_vxmls(vxmls: List(VXML)) -> List(CompactXMLLine) {
  list.fold(vxmls, [], fn(lines, vxml) {
    append_compact_xml_lines(lines, compact_xml_vxml(vxml))
  })
}

fn compact_xml_vxml(vxml: VXML) -> List(CompactXMLLine) {
  case vxml {
    T(_, lines) ->
      list.map(lines, fn(line) {
        CompactXMLLine(line.blame, xml_text_escape(line.content))
      })

    V(blame, tag, attrs, []) -> [
      CompactXMLLine(blame, "<" <> tag <> xml_attrs(attrs) <> "/>"),
    ]

    V(blame, tag, attrs, children) -> {
      let opening = [
        CompactXMLLine(blame, "<" <> tag <> xml_attrs(attrs) <> ">"),
      ]
      let closing = [CompactXMLLine(blame, "</" <> tag <> ">")]
      opening
      |> append_compact_xml_lines(compact_xml_vxmls(children))
      |> append_compact_xml_lines(closing)
    }
  }
}

fn compact_xml_lines_to_output_lines(
  lines: List(CompactXMLLine),
  starting_indent: Int,
) -> List(OutputLine) {
  list.index_map(lines, fn(line, index) {
    OutputLine(
      line.blame,
      case index == 0 {
        True -> starting_indent
        False -> 0
      },
      line.content,
    )
  })
}

fn has_only_element_children(children: List(VXML)) -> Bool {
  list.all(children, fn(child) {
    case child {
      V(..) -> True
      T(..) -> False
    }
  })
}

fn vxml_to_xml_output_lines_internal(
  vxml: VXML,
  indent: Int,
  indentation: Int,
) -> List(OutputLine) {
  case vxml {
    V(blame, tag, attrs, []) -> [
      OutputLine(blame, indent, "<" <> tag <> xml_attrs(attrs) <> "/>"),
    ]

    V(blame, tag, attrs, children) ->
      case has_only_element_children(children) {
        True ->
          [
            [OutputLine(blame, indent, "<" <> tag <> xml_attrs(attrs) <> ">")],
            children
              |> list.map(vxml_to_xml_output_lines_internal(
                _,
                indent + indentation,
                indentation,
              ))
              |> list.flatten,
            [OutputLine(blame, indent, "</" <> tag <> ">")],
          ]
          |> list.flatten
        False ->
          vxml
          |> compact_xml_vxml
          |> compact_xml_lines_to_output_lines(indent)
      }

    T(..) ->
      vxml
      |> compact_xml_vxml
      |> compact_xml_lines_to_output_lines(indent)
  }
}

/// Serializes one VXML node to XML output lines.
///
/// Element-only content is indented. Mixed content remains compact so that
/// formatting does not introduce text whitespace.
pub fn vxml_to_xml_output_lines(
  vxml: VXML,
  starting_indent: Int,
  indentation: Int,
) -> List(OutputLine) {
  vxml_to_xml_output_lines_internal(vxml, starting_indent, indentation)
}

/// Serializes VXML nodes to XML output lines.
pub fn vxmls_to_xml_output_lines(
  vxmls: List(VXML),
  starting_indent: Int,
  indentation: Int,
) -> List(OutputLine) {
  case has_only_element_children(vxmls) {
    True ->
      vxmls
      |> list.map(vxml_to_xml_output_lines_internal(
        _,
        starting_indent,
        indentation,
      ))
      |> list.flatten
    False ->
      vxmls
      |> compact_xml_vxmls
      |> compact_xml_lines_to_output_lines(starting_indent)
  }
}

/// Serializes one VXML node to XML.
pub fn vxml_to_xml(
  vxml: VXML,
  starting_indent: Int,
  indentation: Int,
) -> String {
  vxml
  |> vxml_to_xml_output_lines(starting_indent, indentation)
  |> io_l.output_lines_to_string
}

/// Serializes VXML nodes to XML.
pub fn vxmls_to_xml(
  vxmls: List(VXML),
  starting_indent: Int,
  indentation: Int,
) -> String {
  vxmls
  |> vxmls_to_xml_output_lines(starting_indent, indentation)
  |> io_l.output_lines_to_string
}

fn html_string_processor(content: String) -> String {
  html_text_escape(content)
}

type StickyLine {
  StickyLine(
    blame: Blame,
    indent: Int,
    content: String,
    sticky_start: Bool,
    sticky_end: Bool,
  )
}

type StickyTree {
  StickyTree(
    opening_lines: List(StickyLine),
    children: List(StickyTree),
    closing_lines: List(StickyLine),
  )
}

fn sticky_2_blamed(stickie: StickyLine) -> OutputLine {
  OutputLine(stickie.blame, stickie.indent, stickie.content)
}

fn concat_sticky_lines_internal(
  already_stuck: List(StickyLine),
  working_on: StickyLine,
  upcoming: List(StickyLine),
) -> List(StickyLine) {
  case upcoming {
    [] -> {
      [working_on, ..already_stuck] |> list.reverse
    }
    [next, ..rest] -> {
      case working_on.sticky_end && next.sticky_start {
        True ->
          concat_sticky_lines_internal(
            already_stuck,
            StickyLine(
              ..working_on,
              content: working_on.content <> next.content,
              sticky_end: next.sticky_end,
            ),
            rest,
          )
        False ->
          concat_sticky_lines_internal(
            [working_on, ..already_stuck],
            next,
            rest,
          )
      }
    }
  }
}

fn concat_sticky_lines(lines: List(StickyLine)) -> List(StickyLine) {
  case lines {
    [] -> []
    [first, ..rest] -> concat_sticky_lines_internal([], first, rest)
  }
}

fn pour(to: List(a), from: List(a)) -> List(a) {
  case from {
    [] -> to
    [first, ..rest] -> pour([first, ..to], rest)
  }
}

fn sticky_trees_2_sticky_lines(
  already_stuck: List(StickyLine),
  subtrees: List(StickyTree),
) -> List(StickyLine) {
  case subtrees {
    [] -> already_stuck
    [first, ..rest] ->
      sticky_trees_2_sticky_lines(
        sticky_tree_2_sticky_lines(already_stuck, first),
        rest,
      )
  }
}

fn sticky_tree_2_sticky_lines(
  already_stuck: List(StickyLine),
  subtree: StickyTree,
) -> List(StickyLine) {
  let StickyTree(opening_lines, children, closing_lines) = subtree
  let already_stuck = pour(already_stuck, opening_lines)
  let already_stuck = sticky_trees_2_sticky_lines(already_stuck, children)
  pour(already_stuck, closing_lines)
}

fn attrs_to_sticky_lines(
  attrs: List(Attr),
  indent: Int,
  inline: Bool,
) -> List(StickyLine) {
  let space = case inline {
    True -> " "
    False -> ""
  }
  attrs
  |> list.map(fn(t) {
    StickyLine(
      blame: t.blame,
      indent: indent,
      content: space <> t.key <> "=\"" <> html_attribute_escape(t.val) <> "\"",
      sticky_start: inline,
      sticky_end: inline,
    )
  })
}

const sticky_tags = [
  "a", "span", "i", "b", "strong", "em", "code", "tt", "br", "img",
]

const self_closing_tags = ["img", "br", "hr"]

fn opening_tag_to_sticky_lines(
  t: VXML,
  indent: Int,
  spaces: Int,
  pre: Bool,
) -> List(StickyLine) {
  let assert V(blame, tag, attrs, _) = t
  let indent = case pre {
    True -> 0
    False -> indent
  }
  let sticky_outside = list.contains(sticky_tags, tag)
  let sticky_inside = list.length(attrs) <= 1
  list.flatten([
    [StickyLine(blame, indent, "<" <> tag, sticky_outside, sticky_inside)],
    attrs_to_sticky_lines(attrs, indent + spaces, sticky_inside),
    [StickyLine(blame, indent, ">", sticky_inside, sticky_outside)],
  ])
}

fn closing_tag_to_sticky_lines(
  t: VXML,
  indent: Int,
  pre: Bool,
) -> List(StickyLine) {
  let assert V(blame, tag, _, _) = t
  let indent = case pre {
    True -> 0
    False -> indent
  }
  let sticky_outside = list.contains(sticky_tags, tag)
  [
    StickyLine(
      blame,
      indent,
      "</" <> tag <> ">",
      sticky_outside,
      sticky_outside,
    ),
  ]
}

fn t_sticky_lines(t: VXML, indent: Int, pre: Bool) -> List(StickyLine) {
  let assert T(_, lines) = t
  let indent = case pre {
    True -> 0
    False -> indent
  }
  let last_index = list.length(lines) - 1
  let sticky_lines =
    list.index_map(lines, fn(line, i) {
      let content = html_string_processor(line.content)
      StickyLine(
        blame: line.blame,
        indent: indent,
        content: content,
        sticky_start: i == 0 && { !string.starts_with(content, " ") || pre },
        sticky_end: i == last_index
          && { !string.ends_with(content, " ") || pre },
      )
    })
  // if not pre:
  // - while lines have at least 1 line:
  //   - any starting blanks of first content can be removed (start is automatically non-sticky in that case)
  //   - any ending blanks of last content can be removed (end is automatically non-sticky in that case)
  //   - if first content is empty and at least 2 lines, can remove first
  //   - if last content is empty and at least 2 lines, can remove last
  //   - if first == last content is empty, can make sticky_start = False, sticky_end = True to induce simple newline at that indent
  case pre {
    True -> sticky_lines
    False -> t_very_fancy_sticky_lines_post_processing(sticky_lines)
  }
}

fn t_very_fancy_sticky_lines_post_processing(
  lines: List(StickyLine),
) -> List(StickyLine) {
  // see 'if not pre' comment above for what this function
  // thinks it's doing

  let trim_start = fn(sticky: StickyLine) -> StickyLine {
    StickyLine(..sticky, content: string.trim_start(sticky.content))
  }

  let trim_end = fn(sticky: StickyLine) -> StickyLine {
    StickyLine(..sticky, content: string.trim_end(sticky.content))
  }

  let assert [first, ..rest] = lines

  case string.starts_with(first.content, " ") {
    True -> {
      // action 1: the start is not sticky anyway, so
      // trim starting spaces (this function is never called in 'pre' btw)
      assert first.sticky_start == False
      t_very_fancy_sticky_lines_post_processing([trim_start(first), ..rest])
    }
    False -> {
      case first.content == "" {
        True ->
          case list.is_empty(rest) {
            False -> {
              // action 2: the next line is not sticky anyway, so drop
              // this empty line and keep only the others
              let assert Ok(new_first) = list.first(rest)
              assert new_first.sticky_start == False
              t_very_fancy_sticky_lines_post_processing(rest)
            }
            True -> {
              // action 3: we have only 1 empty line, make it non-sticky
              // at start and sticky at end to simulate a plain newline
              [StickyLine(..first, sticky_start: False, sticky_end: True)]
            }
          }
        False -> {
          let assert [last, ..init] = lines |> list.reverse
          case string.ends_with(last.content, " ") {
            True -> {
              // action 4 mirroring action 1: the end is not sticky anyway,
              // so trim ending spaces of last line
              assert last.sticky_end == False
              t_very_fancy_sticky_lines_post_processing(
                [trim_end(last), ..init] |> list.reverse,
              )
            }
            False -> {
              case last.content == "" {
                True -> {
                  let assert [new_last, ..] = init
                  assert new_last.sticky_end == False
                  t_very_fancy_sticky_lines_post_processing(
                    init |> list.reverse,
                  )
                }
                False -> lines
                // (could not find anything to change)
              }
            }
          }
        }
      }
    }
  }
}

fn t_sticky_tree(t: VXML, indent: Int, pre: Bool) -> StickyTree {
  StickyTree(
    opening_lines: t_sticky_lines(t, indent, pre),
    children: [],
    closing_lines: [],
  )
}

fn v_sticky_tree(v: VXML, indent: Int, spaces: Int, pre: Bool) -> StickyTree {
  let assert V(_, tag, _, children) = v
  let pre = pre || tag |> string.lowercase == "pre"
  StickyTree(
    opening_lines: opening_tag_to_sticky_lines(v, indent, spaces, pre),
    children: children
      |> list.map(vxml_sticky_tree(_, indent + spaces, spaces, pre)),
    closing_lines: case list.contains(self_closing_tags, tag) {
      True -> []
      False -> closing_tag_to_sticky_lines(v, indent, pre)
    },
  )
}

fn vxml_sticky_tree(
  node: VXML,
  indent: Int,
  spaces: Int,
  pre: Bool,
) -> StickyTree {
  case node {
    T(_, _) -> t_sticky_tree(node, indent, pre)
    V(_, _, _, _) -> v_sticky_tree(node, indent, spaces, pre)
  }
}

fn vxml_to_html_output_lines_internal(
  node: VXML,
  indent: Int,
  spaces: Int,
) -> List(OutputLine) {
  vxml_sticky_tree(node, indent, spaces, False)
  |> sticky_tree_2_sticky_lines([], _)
  |> list.reverse
  |> concat_sticky_lines
  |> list.map(sticky_2_blamed)
}

fn vxmls_to_html_output_lines_internal(
  vxmls: List(VXML),
  indent: Int,
  spaces: Int,
) -> List(OutputLine) {
  vxmls
  |> list.map(vxml_to_html_output_lines_internal(_, indent, spaces))
  |> list.flatten
}

/// Serializes one VXML node to HTML output lines.
pub fn vxml_to_html_output_lines(
  vxml: VXML,
  starting_indent: Int,
  indentation: Int,
) -> List(OutputLine) {
  vxml_to_html_output_lines_internal(vxml, starting_indent, indentation)
}

/// Serializes VXML nodes to HTML output lines in the same order.
pub fn vxmls_to_html_output_lines(
  vxmls: List(VXML),
  starting_indent: Int,
  indentation: Int,
) -> List(OutputLine) {
  vxmls_to_html_output_lines_internal(vxmls, starting_indent, indentation)
}

/// Serializes one VXML node to an HTML string.
pub fn vxml_to_html(
  vxml: VXML,
  starting_indent: Int,
  indentation: Int,
) -> String {
  vxml
  |> vxml_to_html_output_lines(starting_indent, indentation)
  |> io_l.output_lines_to_string
}

/// Serializes VXML nodes to one HTML string in the same order.
pub fn vxmls_to_html(
  vxmls: List(VXML),
  starting_indent: Int,
  indentation: Int,
) -> String {
  vxmls
  |> vxmls_to_html_output_lines(starting_indent, indentation)
  |> io_l.output_lines_to_string
}

type XMLStreamingParserLogicalUnit {
  XMLStreamingParserText(List(Line))
  XMLStreamingParserOpeningTag(Blame, String, List(Attr))
  XMLStreamingParserSelfClosingTag(Blame, String, List(Attr))
  XMLStreamingParserXMLVersion(Blame, String, List(Attr))
  XMLStreamingParserDoctype(Blame)
  XMLStreamingParserClosingTag(Blame, String)
  XMLStreamingParserComment(List(Line))
}

type CharacterReferenceMode {
  XMLCharacterReferences
  PreserveCharacterReferences
}

type AttributeSyntax {
  XMLAttributes
  HTMLAttributes
}

fn decode_xml_named_character_reference(
  name: String,
) -> Result(String, String) {
  case name {
    "lt" -> Ok("<")
    "gt" -> Ok(">")
    "amp" -> Ok("&")
    "quot" -> Ok("\"")
    "apos" -> Ok("'")
    _ -> Error("unknown XML character reference '&" <> name <> ";'")
  }
}

fn decode_numeric_character_reference(
  spelling: String,
  base: Int,
) -> Result(String, String) {
  use codepoint <- on.error_ok(int.base_parse(spelling, base), fn(_) {
    Error("invalid numeric character reference")
  })
  use utf_codepoint <- on.error_ok(string.utf_codepoint(codepoint), fn(_) {
    Error("invalid numeric character reference")
  })
  Ok(string.from_utf_codepoints([utf_codepoint]))
}

fn decode_xml_character_reference(entity: String) -> Result(String, String) {
  case entity {
    "#x" <> rest -> decode_numeric_character_reference(rest, 16)
    "#X" <> rest -> decode_numeric_character_reference(rest, 16)
    "#" <> rest -> decode_numeric_character_reference(rest, 10)
    name -> decode_xml_named_character_reference(name)
  }
}

fn decode_xml_character_references_loop(
  graphemes: List(String),
  previous: List(String),
) -> Result(String, String) {
  case graphemes {
    [] -> previous |> list.reverse |> string.concat |> Ok
    ["&", ..rest] -> {
      let #(entity_graphemes, rest, closed) =
        take_until_semicolon(rest, [], False)
      use entity <- on.ok(case closed {
        True -> entity_graphemes |> list.reverse |> string.concat |> Ok
        False -> Error("unterminated XML character reference")
      })
      use decoded <- on.ok(decode_xml_character_reference(entity))
      decode_xml_character_references_loop(rest, [decoded, ..previous])
    }
    [first, ..rest] ->
      decode_xml_character_references_loop(rest, [first, ..previous])
  }
}

fn take_until_semicolon(
  remaining: List(String),
  previous: List(String),
  closed: Bool,
) -> #(List(String), List(String), Bool) {
  case remaining, closed {
    _, True -> #(previous, remaining, True)
    [], False -> #(previous, [], False)
    [";", ..rest], False -> #(previous, rest, True)
    [first, ..rest], False ->
      take_until_semicolon(rest, [first, ..previous], False)
  }
}

fn decode_character_references(
  content: String,
  mode: CharacterReferenceMode,
) -> Result(String, String) {
  case mode {
    XMLCharacterReferences ->
      content
      |> string.to_graphemes
      |> decode_xml_character_references_loop([])
    PreserveCharacterReferences -> Ok(content)
  }
}

fn decode_attr_val(
  attr: Attr,
  mode: CharacterReferenceMode,
) -> Result(Attr, #(Blame, String)) {
  use val <- on.error_ok(
    decode_character_references(attr.val, mode),
    fn(message) { Error(#(attr.blame, message)) },
  )
  Ok(Attr(..attr, val: val))
}

fn decode_line(
  line: Line,
  mode: CharacterReferenceMode,
) -> Result(Line, #(Blame, String)) {
  use content <- on.error_ok(
    decode_character_references(line.content, mode),
    fn(message) { Error(#(line.blame, message)) },
  )
  Ok(Line(..line, content: content))
}

fn take_while_text_or_newline_acc(
  previous: List(xs.Event),
  remaining: List(xs.Event),
) -> #(List(xs.Event), List(xs.Event)) {
  // returns reversed list on purpose!!!
  case remaining {
    [] -> #(previous, [])
    [first, ..rest] ->
      case first {
        xs.Text(_, _) | xs.TextWithUnrecognizedLessThan(_, _) | xs.Newline(_) ->
          take_while_text_or_newline_acc([first, ..previous], rest)
        _ -> #(previous, remaining)
      }
  }
}

fn take_while_text_or_newline(
  events: List(xs.Event),
) -> #(List(xs.Event), List(xs.Event)) {
  // returns reversed list on purpose!!!
  take_while_text_or_newline_acc([], events)
}

fn unrecognized_less_than_blame(events: List(xs.Event)) -> Option(Blame) {
  case events {
    [] -> None
    [xs.TextWithUnrecognizedLessThan(blame, _), ..] -> Some(blame)
    [_, ..rest] -> unrecognized_less_than_blame(rest)
  }
}

type Return(a, b) {
  Return(a)
  Continuation(b)
}

fn on_continuation(thing: Return(a, b), f: fn(b) -> a) -> a {
  case thing {
    Return(a) -> a
    Continuation(b) -> f(b)
  }
}

type TriWay {
  NoMoreEvents
  TagEnd(xs.Event, List(xs.Event))
  SomethingElse(xs.Event, List(xs.Event), Bool)
}

fn tri_way(events: List(xs.Event)) -> TriWay {
  case events {
    [] -> NoMoreEvents
    [first, ..rest] -> {
      case first {
        xs.TagEndOrdinary(_) -> TagEnd(first, rest)
        xs.TagEndSelfClosing(_) -> TagEnd(first, rest)
        xs.TagEndXMLVersion(_) -> TagEnd(first, rest)
        xs.InTagWhitespace(_, _) | xs.Newline(_) ->
          case tri_way(rest) {
            SomethingElse(first, rest, _) -> SomethingElse(first, rest, True)
            x -> x
          }
        _ -> SomethingElse(first, rest, False)
      }
    }
  }
}

fn get_attrs_and_tag_end(
  tag_start: xs.Event,
  rest: List(xs.Event),
  attribute_syntax: AttributeSyntax,
) -> Result(#(List(Attr), xs.Event, List(xs.Event)), #(Blame, String)) {
  let prepend_attr_if_ok = fn(
    result: Result(#(List(Attr), xs.Event, List(xs.Event)), #(Blame, String)),
    attr: Attr,
  ) {
    case result {
      Error(e) -> Error(e)
      Ok(#(attrs, end, rest)) -> Ok(#([attr, ..attrs], end, rest))
    }
  }

  use #(first, rest) <- on_continuation(case tri_way(rest) {
    TagEnd(tag_end, rest) -> Return(Ok(#([], tag_end, rest)))

    NoMoreEvents ->
      Return(
        Error(#(
          tag_start.blame,
          "ran out of events while waiting for end of tag",
        )),
      )

    SomethingElse(first, rest, _) -> Continuation(#(first, rest))
  })

  use #(key_blame, key_name) <- on.ok(case first {
    xs.Key(b, k) -> Ok(#(b, k))
    _ ->
      Error(#(
        first.blame,
        "expecting tag end or valid key after tag name; tag_start"
          <> xs.event_digest(tag_start)
          <> "; had "
          <> xs.event_digest(first)
          <> " instead",
      ))
  })

  let proto = Attr(key_blame, key_name, "")

  use #(second, rest) <- on_continuation(case tri_way(rest) {
    TagEnd(tag_end, rest) ->
      Return(case attribute_syntax {
        HTMLAttributes -> Ok(#([proto], tag_end, rest))
        XMLAttributes ->
          Error(#(
            key_blame,
            "attribute key without assigned value: " <> key_name,
          ))
      })

    NoMoreEvents ->
      Return(
        Error(#(
          tag_start.blame,
          "ran out of events while waiting for end of tag",
        )),
      )

    SomethingElse(second, rest, _) -> Continuation(#(second, rest))
  })

  use _ <- on_continuation(case second {
    xs.Assignment(_) -> Continuation(Nil)
    _ ->
      Return(case attribute_syntax {
        XMLAttributes ->
          Error(#(key_blame, "attribute key without assignment: " <> key_name))
        HTMLAttributes ->
          get_attrs_and_tag_end(tag_start, [second, ..rest], attribute_syntax)
          |> prepend_attr_if_ok(proto)
      })
  })

  // 'key=' or 'key  ='

  use #(third, rest, had_spaces) <- on_continuation(case tri_way(rest) {
    TagEnd(tag_end, rest) ->
      Return(case attribute_syntax {
        HTMLAttributes -> Ok(#([proto], tag_end, rest))
        XMLAttributes ->
          Error(#(key_blame, "attribute assignment without value: " <> key_name))
      })

    NoMoreEvents ->
      Return(
        Error(#(
          tag_start.blame,
          "ran out of events while waiting for end of tag",
        )),
      )

    SomethingElse(third, rest, had_spaces) ->
      Continuation(#(third, rest, had_spaces))
  })

  case third {
    xs.ValueDoubleQuoted(_, val) | xs.ValueSingleQuoted(_, val) -> {
      get_attrs_and_tag_end(tag_start, rest, attribute_syntax)
      |> prepend_attr_if_ok(Attr(..proto, val: val))
    }

    xs.ValueUnquoted(blame, val) ->
      case attribute_syntax {
        HTMLAttributes ->
          get_attrs_and_tag_end(tag_start, rest, attribute_syntax)
          |> prepend_attr_if_ok(Attr(..proto, val: val))
        XMLAttributes -> Error(#(blame, "unquoted attr val: " <> val))
      }

    xs.ValueMalformed(blame, val) ->
      Error(#(blame, "malformed attr val: " <> val))

    _ -> {
      case get_attrs_and_tag_end(tag_start, rest, attribute_syntax) {
        Error(e) -> Error(e)
        Ok(#(attrs, end, rest)) ->
          case had_spaces, attrs {
            False, [some, ..] ->
              Error(#(some.blame, "expecting attr val after '='"))
            _, _ -> Ok(#([proto, ..attrs], end, rest))
          }
      }
    }
  }
}

fn reach_end_of_comments(
  comment_start: xs.Event,
  rest: List(xs.Event),
) -> Result(#(List(xs.Event), List(xs.Event)), #(Blame, String)) {
  case rest {
    [xs.CommentEndSequence(_), ..rest] -> {
      Ok(#([], rest))
    }
    [xs.CommentContents(_, _) as first, ..rest] -> {
      use #(before, after) <- on.ok(reach_end_of_comments(comment_start, rest))
      Ok(#([first, ..before], after))
    }
    [xs.Newline(_), ..rest] -> {
      // just ignore it:
      reach_end_of_comments(comment_start, rest)
    }
    [] -> {
      Error(#(comment_start.blame, "unclosed comment"))
    }
    [some, ..] -> {
      let msg =
        "non-comment Event after comment start; start: "
        <> bl.blame_digest(comment_start.blame)
        <> "; Event: "
        <> xs.event_digest(some)
      panic as msg
    }
  }
}

fn reach_end_of_doctype(
  doctype_start: xs.Event,
  rest: List(xs.Event),
) -> Result(List(xs.Event), #(Blame, String)) {
  case rest {
    [xs.DoctypeEndSequence(_), ..rest] -> Ok(rest)
    [xs.DoctypeContents(_, _), ..rest] | [xs.Newline(_), ..rest] ->
      reach_end_of_doctype(doctype_start, rest)
    [] -> Error(#(doctype_start.blame, "unclosed doctype declaration"))
    [some, ..] -> {
      let msg =
        "non-doctype Event after doctype start; start: "
        <> bl.blame_digest(doctype_start.blame)
        <> "; Event: "
        <> xs.event_digest(some)
      panic as msg
    }
  }
}

fn decode_attrs(
  attrs: List(Attr),
  mode: CharacterReferenceMode,
  previous: List(Attr),
) -> Result(List(Attr), #(Blame, String)) {
  case attrs {
    [] -> previous |> list.reverse |> Ok
    [first, ..rest] -> {
      use attr <- on.ok(decode_attr_val(first, mode))
      decode_attrs(rest, mode, [attr, ..previous])
    }
  }
}

fn decode_lines(
  lines: List(Line),
  mode: CharacterReferenceMode,
  previous: List(Line),
) -> Result(List(Line), #(Blame, String)) {
  case lines {
    [] -> previous |> list.reverse |> Ok
    [first, ..rest] -> {
      use line <- on.ok(decode_line(first, mode))
      decode_lines(rest, mode, [line, ..previous])
    }
  }
}

fn xml_streaming_get_next_logical_unit(
  events: List(xs.Event),
  mode: CharacterReferenceMode,
  attribute_syntax: AttributeSyntax,
) -> Result(#(XMLStreamingParserLogicalUnit, List(xs.Event)), #(Blame, String)) {
  let assert [first, ..rest] = events

  case first {
    // XMLStreamingParserText
    xs.Text(_, _) | xs.TextWithUnrecognizedLessThan(_, _) | xs.Newline(_) -> {
      let #(guys, remaining) = take_while_text_or_newline(events)
      use _ <- on.ok(case attribute_syntax, unrecognized_less_than_blame(guys) {
        XMLAttributes, Some(blame) ->
          Error(#(blame, "unrecognized '<' in XML text"))
        _, _ -> Ok(Nil)
      })
      let assert [last, ..] = guys
      let guys = case last {
        xs.Newline(b) -> [xs.Text(b, ""), ..guys]
        _ -> guys
      }
      let guys = guys |> list.reverse
      let guys = case first {
        xs.Newline(b) -> [xs.Text(b, ""), ..guys]
        _ -> guys
      }
      let lines =
        list.map(guys, fn(x) {
          case x {
            xs.Newline(_) -> None
            xs.Text(b, c) | xs.TextWithUnrecognizedLessThan(b, c) ->
              Some(Line(b, c))
            _ -> panic
          }
        })
        |> option.values
      use lines <- on.ok(decode_lines(lines, mode, []))
      Ok(#(XMLStreamingParserText(lines), remaining))
    }

    // construction of: 
    //   - XMLStreamingParserOpeningTag
    //   - XMLStreamingParserSelfClosingTag
    xs.TagStartOrdinary(blame, tag) -> {
      use #(attrs, end, remaining) <- on.ok(get_attrs_and_tag_end(
        first,
        rest,
        attribute_syntax,
      ))
      use attrs <- on.ok(decode_attrs(attrs, mode, []))
      case end {
        xs.TagEndOrdinary(_) ->
          Ok(#(XMLStreamingParserOpeningTag(blame, tag, attrs), remaining))
        xs.TagEndSelfClosing(_) ->
          Ok(#(XMLStreamingParserSelfClosingTag(blame, tag, attrs), remaining))
        xs.TagEndXMLVersion(b) -> Error(#(b, "unexpected '?>' tag ending"))
        _ -> panic
      }
    }

    // construction of XMLStreamingParserXMLVersion
    xs.TagStartXMLVersion(blame, tag) -> {
      assert tag == "xml" || tag == "XML"
      use #(attrs, end, remaining) <- on.ok(get_attrs_and_tag_end(
        first,
        rest,
        attribute_syntax,
      ))
      use attrs <- on.ok(decode_attrs(attrs, mode, []))
      case end {
        xs.TagEndXMLVersion(_) ->
          Ok(#(XMLStreamingParserXMLVersion(blame, tag, attrs), remaining))
        xs.TagEndOrdinary(b) -> Error(#(b, "expecting '?>' tag ending"))
        xs.TagEndSelfClosing(b) -> Error(#(b, "expecting '?>' tag ending"))
        _ -> panic
      }
    }

    // construction of XMLStreamingParserDoctype
    xs.DoctypeStartSequence(blame) -> {
      use remaining <- on.ok(reach_end_of_doctype(first, rest))
      Ok(#(XMLStreamingParserDoctype(blame), remaining))
    }

    // construction of XMLStreamingParserClosingTag
    xs.TagStartClosing(blame, tag) -> {
      use #(attrs, end, remaining) <- on.ok(get_attrs_and_tag_end(
        first,
        rest,
        attribute_syntax,
      ))
      use <- on.nonempty_empty(attrs, fn(_, _) {
        Error(#(blame, "attrs in closing tag"))
      })
      case end {
        xs.TagEndOrdinary(_) ->
          Ok(#(XMLStreamingParserClosingTag(blame, tag), remaining))
        xs.TagEndSelfClosing(b) -> Error(#(b, "unexpected '/>' in closing tag"))
        xs.TagEndXMLVersion(b) -> Error(#(b, "unexpected '?>' in closing tag"))
        _ -> panic
      }
    }

    // construction of XMLS
    xs.CommentStartSequence(_) -> {
      use #(events, remaining) <- on.ok(reach_end_of_comments(first, rest))
      let lines =
        list.map(events, fn(e) {
          let assert xs.CommentContents(b, l) = e
          Line(b, l)
        })
      Ok(#(XMLStreamingParserComment(lines), remaining))
    }

    // ...this completes everything we can construct!
    // ...everything else is out of place!
    _ -> {
      let msg =
        "inner tag content (?) when ostensibly out-of-tag: " <> ins(first)
      panic as msg
    }
  }
}

fn xml_streaming_logical_units_acc(
  remaining: List(xs.Event),
  acc: List(XMLStreamingParserLogicalUnit),
  mode: CharacterReferenceMode,
  attribute_syntax: AttributeSyntax,
) -> Result(List(XMLStreamingParserLogicalUnit), #(Blame, String)) {
  case remaining {
    [] -> acc |> list.reverse |> Ok
    _ ->
      case
        xml_streaming_get_next_logical_unit(remaining, mode, attribute_syntax)
      {
        Error(error) -> Error(error)
        Ok(#(unit, remaining)) ->
          xml_streaming_logical_units_acc(
            remaining,
            [unit, ..acc],
            mode,
            attribute_syntax,
          )
      }
  }
}

fn xml_streaming_logical_units(
  events: List(xs.Event),
  mode: CharacterReferenceMode,
  attribute_syntax: AttributeSyntax,
) -> Result(List(XMLStreamingParserLogicalUnit), #(Blame, String)) {
  xml_streaming_logical_units_acc(events, [], mode, attribute_syntax)
}

fn list_to_stack_digest(l: List(a), d: fn(a) -> String) -> String {
  "[" <> { list.map(l, d) |> string.join(", ") } <> "]"
}

fn attr_to_stack_digest(attr: Attr) -> String {
  attr.key <> "=" <> attr.val
}

fn attrs_to_stack_digest(attrs: List(Attr)) -> String {
  list_to_stack_digest(attrs, attr_to_stack_digest)
}

fn vxml_to_stack_digest(node: VXML) -> String {
  let assert V(bl, tag, attrs, children) = node
  "V("
  <> { bl.blame_digest(bl) }
  <> ", "
  <> tag
  <> ", "
  <> attrs_to_stack_digest(attrs)
  <> ", "
  <> "["
  <> case children {
    [_] -> "1 child]"
    _ -> ins(list.length(children)) <> " children]"
  }
  <> ")"
}

fn vxmls_from_streaming_logical_units_acc(
  units: List(XMLStreamingParserLogicalUnit),
  stack: List(VXML),
  previously_completed: List(VXML),
  filter_out_doctype_nodes: Bool,
  filter_out_root_level_text: Bool,
) -> Result(List(VXML), #(Blame, String)) {
  case units {
    [] -> {
      case stack {
        [] -> Ok(previously_completed |> list.reverse)
        [last, ..] -> {
          let assert V(blame, tag, _, _) = last
          let ancestor_tag_sequence =
            stack
            |> list.map(vxml_to_stack_digest)
            |> string.join(" -> ")
          Error(#(
            blame,
            "unclosed '"
              <> tag
              <> "' at end of document; open ancestor sequence: "
              <> ancestor_tag_sequence,
          ))
        }
      }
    }

    [first, ..rest] -> {
      case first {
        XMLStreamingParserDoctype(b) -> {
          let v = V(b, "!DOCTYPE", [], [])
          case stack {
            [] ->
              vxmls_from_streaming_logical_units_acc(
                rest,
                [],
                case filter_out_doctype_nodes {
                  True -> previously_completed
                  False -> [v, ..previously_completed]
                },
                filter_out_doctype_nodes,
                filter_out_root_level_text,
              )
            _ -> Error(#(b, "found !DOCTYPE node at non-root level"))
          }
        }

        XMLStreamingParserXMLVersion(b, tag, attrs) -> {
          let v = V(b, tag, attrs, [])
          case stack {
            [] ->
              vxmls_from_streaming_logical_units_acc(
                rest,
                [],
                case filter_out_doctype_nodes {
                  True -> previously_completed
                  False -> [v, ..previously_completed]
                },
                filter_out_doctype_nodes,
                filter_out_root_level_text,
              )
            _ -> Error(#(b, "found XML version-node at non-root level"))
          }
        }

        XMLStreamingParserOpeningTag(b, tag, attrs) -> {
          let v = V(b, tag, attrs, [])
          vxmls_from_streaming_logical_units_acc(
            rest,
            [v, ..stack],
            previously_completed,
            filter_out_doctype_nodes,
            filter_out_root_level_text,
          )
        }

        XMLStreamingParserText(lines) -> {
          let assert [first_line, ..] = lines
          let t = T(first_line.blame, lines)
          let #(stack, previously_completed) = case stack {
            [last, ..others] -> {
              let assert V(_, _, _, _) = last
              let last = V(..last, children: [t, ..last.children])
              #([last, ..others], previously_completed)
            }
            _ ->
              case filter_out_root_level_text {
                True -> #(stack, previously_completed)
                False -> #(stack, [t, ..previously_completed])
              }
          }
          vxmls_from_streaming_logical_units_acc(
            rest,
            stack,
            previously_completed,
            filter_out_doctype_nodes,
            filter_out_root_level_text,
          )
        }

        XMLStreamingParserComment(_) -> {
          vxmls_from_streaming_logical_units_acc(
            rest,
            stack,
            previously_completed,
            filter_out_doctype_nodes,
            filter_out_root_level_text,
          )
        }

        XMLStreamingParserClosingTag(b, tag) -> {
          case stack {
            [] -> Error(#(b, "closing '</" <> tag <> ">' on empty stack"))
            [last, ..others] -> {
              let assert V(_, last_tag, _, _) = last
              case last_tag == tag {
                False ->
                  Error(#(
                    b,
                    "expected closing '"
                      <> last_tag
                      <> "' tag, found '"
                      <> tag
                      <> "' instead",
                  ))
                True -> {
                  let last = V(..last, children: last.children |> list.reverse)
                  case others {
                    [] ->
                      vxmls_from_streaming_logical_units_acc(
                        rest,
                        [],
                        [last, ..previously_completed],
                        filter_out_doctype_nodes,
                        filter_out_root_level_text,
                      )
                    [parent, ..older] -> {
                      let assert V(_, _, _, _) = parent
                      let parent =
                        V(..parent, children: [last, ..parent.children])
                      vxmls_from_streaming_logical_units_acc(
                        rest,
                        [parent, ..older],
                        [],
                        filter_out_doctype_nodes,
                        filter_out_root_level_text,
                      )
                    }
                  }
                }
              }
            }
          }
        }

        XMLStreamingParserSelfClosingTag(b, tag, attrs) -> {
          let v = V(b, tag, attrs, [])
          case stack {
            [last, ..others] -> {
              let assert V(_, _, _, _) = last
              let last = V(..last, children: [v, ..last.children])
              vxmls_from_streaming_logical_units_acc(
                rest,
                [last, ..others],
                previously_completed,
                filter_out_doctype_nodes,
                filter_out_root_level_text,
              )
            }
            [] -> {
              vxmls_from_streaming_logical_units_acc(
                rest,
                [],
                [v, ..previously_completed],
                filter_out_doctype_nodes,
                filter_out_root_level_text,
              )
            }
          }
        }
      }
    }
  }
}

fn vxmls_from_streaming_logical_units(
  units: List(XMLStreamingParserLogicalUnit),
  filter_out_doctype_nodes: Bool,
  filter_out_root_level_text: Bool,
) -> Result(List(VXML), #(Blame, String)) {
  vxmls_from_streaming_logical_units_acc(
    units,
    [],
    [],
    filter_out_doctype_nodes,
    filter_out_root_level_text,
  )
}

fn vxml_from_streaming_logical_units(
  units: List(XMLStreamingParserLogicalUnit),
) -> Result(VXML, #(Blame, String)) {
  use vxmls <- on.ok(vxmls_from_streaming_logical_units(units, True, True))
  case vxmls {
    [] -> Error(#(bl.no_blame, "empty document (?)"))
    [one] -> Ok(one)
    [_, second, ..] -> {
      Error(#(second.blame, "found >1 root-level nodes"))
    }
  }
}

/// Parses XML-like input lines into VXML.
///
/// Tag and attribute names must satisfy XML's `Name` grammar. Attribute values
/// must be quoted. Character references are decoded in text and values.
pub fn xml_input_lines_to_vxml(
  lines: List(InputLine),
) -> Result(VXML, XMLParseError) {
  xml_input_lines_to_vxml_with_syntax(
    lines,
    XMLCharacterReferences,
    XMLAttributes,
  )
}

fn xml_input_lines_to_vxml_with_syntax(
  lines: List(InputLine),
  mode: CharacterReferenceMode,
  attribute_syntax: AttributeSyntax,
) -> Result(VXML, XMLParseError) {
  lines
  |> xs.input_lines_streamer
  |> xml_streaming_logical_units(mode, attribute_syntax)
  |> on.ok(vxml_from_streaming_logical_units)
  |> result.map_error(fn(error) { XMLParseError(error.0, error.1) })
}

/// Parses an XML-like string into VXML.
///
/// `filename` is recorded in source blame. Tag and attribute names must satisfy
/// XML's `Name` grammar. Attribute values must be quoted, and character
/// references are decoded in text and values.
pub fn xml_to_vxml(
  content: String,
  filename: String,
) -> Result(VXML, XMLParseError) {
  content
  |> io_l.string_to_input_lines(filename, 0)
  |> xml_input_lines_to_vxml
}

/// Parses best-effort repaired HTML into VXML.
///
/// The parser applies `html_repair`, permits unquoted attribute values, and
/// preserves recognized HTML character-reference spellings in text and
/// attribute values.
pub fn html_to_vxml(
  content: String,
  filename: String,
) -> Result(VXML, XMLParseError) {
  content
  |> html_repair
  |> io_l.string_to_input_lines(filename, 0)
  |> xml_input_lines_to_vxml_with_syntax(
    PreserveCharacterReferences,
    HTMLAttributes,
  )
}

// ************************************************************
// HTML repair facade
// ************************************************************

/// Escapes ampersands that do not begin a known HTML entity.
pub fn html_repair_escape_non_entity_ampersands(content: String) -> String {
  html_repair.html_repair_escape_non_entity_ampersands(content)
}

/// Gives common bare HTML boolean attributes empty assigned values.
pub fn html_repair_expand_boolean_attrs(content: String) -> String {
  html_repair.html_repair_expand_boolean_attrs(content)
}

/// Converts common HTML void-element openings to self-closing XML syntax.
pub fn html_repair_close_void_tags(content: String) -> String {
  html_repair.html_repair_close_void_tags(content)
}

/// Removes attributes from malformed closing tags.
pub fn html_repair_remove_attrs_from_closing_tags(content: String) -> String {
  html_repair.html_repair_remove_attrs_from_closing_tags(content)
}

/// Applies the package's best-effort HTML repairs for XML-oriented parsing.
pub fn html_repair(content: String) -> String {
  html_repair.html_repair(content)
}
