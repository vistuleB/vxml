import blamedlines.{
  type Blame, type BlamedLine, Blame, BlamedLine, prepend_comment as pc,
} as bl
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp
import gleam/result
import gleam/string
import simplifile
import xmlm

const vxml_indent = 2

//****************
//* public types *
//****************

pub type BlamedContent {
  BlamedContent(blame: Blame, content: String)
}

pub type VXML {
  V(
    blame: Blame,
    tag: String,
    attributes: List(BlamedAttribute),
    children: List(VXML),
  )
  T(blame: Blame, contents: List(BlamedContent))
}

pub type BlamedAttribute {
  BlamedAttribute(blame: Blame, key: String, value: String)
}

pub type VXMLParseError {
  VXMLParseErrorEmptyTag(Blame)
  VXMLParseErrorIllegalTagCharacter(Blame, String, String)
  VXMLParseErrorIllegalAttributeKeyCharacter(Blame, String, String)
  VXMLParseErrorIndentationTooLarge(Blame, String)
  VXMLParseErrorIndentationNotMultipleOfFour(Blame, String)
  VXMLParseErrorTextMissing(Blame)
  VXMLParseErrorTextNoClosingQuote(Blame, String)
  VXMLParseErrorTextNoOpeningQuote(Blame, String)
  VXMLParseErrorTextOutOfPlace(Blame, String)
  VXMLParseErrorCaretExpected(Blame, String)
  VXMLParseErrorNonUniqueRoot(Int)
}

pub type VXMLParseFileError {
  IOError(simplifile.FileError)
  DocumentError(VXMLParseError)
}

//***************
//* local types *
//***************

type FileHead =
  List(BlamedLine)

type BadTagName {
  EmptyTag
  IllegalTagCharacter(String, String)
}

type TentativeTagName =
  Result(String, BadTagName)

type BadAttributeKey {
  IllegalAttributeKeyCharacter(String, String)
}

type TentativeAttributeKey =
  Result(String, BadAttributeKey)

type TentativeBlamedAttribute {
  TentativeBlamedAttribute(
    blame: Blame,
    key: TentativeAttributeKey,
    value: String,
  )
}

type NonemptySuffixDiagnostic {
  EmptyCaret
  TaggedCaret(tag: String)
  DoubleQuoted(with_double_quotes: String)
  Other(String)
}

type TentativeVXML {
  TentativeT(blame: Blame, contents: List(BlamedContent))
  TentativeV(
    blame: Blame,
    tag: TentativeTagName,
    attributes: List(TentativeBlamedAttribute),
    children: List(TentativeVXML),
  )
  TentativeErrorIndentationTooLarge(blame: Blame, message: String)
  TentativeErrorIndentationNotMultipleOfFour(blame: Blame, message: String)
  TentativeErrorTextMissing(blame: Blame)
  TentativeErrorTextNoClosingQuote(blamd: Blame, message: String)
  TentativeErrorTextNoOpeningQuote(blamd: Blame, message: String)
  TentativeErrorTextOutOfPlace(blame: Blame, message: String)
  TentativeErrorCaretExpected(blame: Blame, message: String)
}

//*************
//* constants *
//*************

const ins = string.inspect

const tag_illegal_characters = ["-", ".", " ", "\""]

const attribute_key_illegal_characters = [".", ";", "\""]

//************
//* FileHead *
//************

fn current_line(head: FileHead) -> Option(BlamedLine) {
  case head {
    [] -> None
    [first, ..] -> Some(first)
  }
}

fn move_forward(head: FileHead) -> FileHead {
  let assert [_, ..rest] = head
  rest
}

//*******************
//* parse_tentative *
//*******************

fn is_double_quoted_thing(suffix: String) -> Bool {
  let trimmed = string.trim(suffix)
  string.starts_with(trimmed, "\"")
  && string.ends_with(trimmed, "\"")
  && string.length(trimmed) >= 2
}

fn nonempty_suffix_diagnostic(suffix: String) -> NonemptySuffixDiagnostic {
  let assert False = suffix == ""
  let assert False = string.starts_with(suffix, " ")

  case string.starts_with(suffix, "<>") {
    True -> {
      let tag = string.trim(string.drop_start(suffix, 2))
      case tag == "" {
        True -> EmptyCaret
        False -> TaggedCaret(tag)
      }
    }

    False ->
      case is_double_quoted_thing(suffix) {
        True -> DoubleQuoted(string.trim(suffix))
        False -> Other(suffix)
      }
  }
}

fn fast_forward_past_lines_of_indent_at_least(
  indent: Int,
  head: FileHead,
) -> #(List(BlamedLine), FileHead) {
  case current_line(head) {
    None -> #([], head)

    Some(BlamedLine(_, suffix_indent, _) as first_line) ->
      case suffix_indent < indent {
        True -> #([], head)

        False -> {
          let #(further_lines, last_head) =
            fast_forward_past_lines_of_indent_at_least(
              indent,
              move_forward(head),
            )

          #([first_line, ..further_lines], last_head)
        }
      }
  }
}

fn tentative_blamed_attribute(
  blame: Blame,
  pair: #(String, String),
) -> TentativeBlamedAttribute {
  let #(key, value) = pair
  let assert False = string.contains(key, " ")
  let assert False = string.is_empty(key)
  let bad_character = contains_one_of(key, attribute_key_illegal_characters)

  case bad_character == "" {
    True -> TentativeBlamedAttribute(blame: blame, key: Ok(key), value: value)

    False ->
      TentativeBlamedAttribute(
        blame: blame,
        key: Error(IllegalAttributeKeyCharacter(key, bad_character)),
        value: value,
      )
  }
}

fn fast_forward_past_attribute_lines_at_indent(
  indent: Int,
  head: FileHead,
) -> #(List(TentativeBlamedAttribute), FileHead) {
  case current_line(head) {
    None -> #([], head)

    Some(BlamedLine(blame, suffix_indent, suffix)) -> {
      case suffix == "" {
        True -> #([], move_forward(head))

        False -> {
          case suffix_indent == indent {
            False -> #([], head)

            True ->
              case string.starts_with(suffix, "<>") {
                True -> #([], head)

                False -> {
                  let tentative_attribute_pair =
                    suffix
                    |> string.split_once("=")
                    |> result.unwrap(#(suffix, ""))
                    |> tentative_blamed_attribute(blame, _)

                  let #(more_attribute_pairs, head_after_attributes) =
                    fast_forward_past_attribute_lines_at_indent(
                      indent,
                      move_forward(head),
                    )

                  #(
                    [tentative_attribute_pair, ..more_attribute_pairs],
                    head_after_attributes,
                  )
                }
              }
          }
        }
      }
    }
  }
}

fn strip_quotes(s: String) -> String {
  let assert True = string.starts_with(s, "\"")
  let assert True = string.ends_with(s, "\"")
  let assert True = string.length(s) >= 2
  s |> string.drop_start(1) |> string.drop_end(1)
}

fn add_quotes(s: String) -> String {
  "\"" <> s <> "\""
}

fn fast_forward_past_double_quoted_lines_at_indent(
  indent: Int,
  head: FileHead,
) -> #(List(BlamedContent), FileHead) {
  case current_line(head) {
    None -> #([], head)

    Some(BlamedLine(blame, suffix_indent, suffix)) -> {
      case suffix == "" {
        True -> #([], head)

        False -> {
          case suffix_indent != indent {
            True -> #([], head)

            False ->
              case is_double_quoted_thing(suffix) {
                False -> #([], head)

                True -> {
                  let double_quoted =
                    BlamedContent(blame, strip_quotes(string.trim(suffix)))

                  let #(more_double_quoteds, head_after_double_quoteds) =
                    fast_forward_past_double_quoted_lines_at_indent(
                      indent,
                      move_forward(head),
                    )

                  #(
                    [double_quoted, ..more_double_quoteds],
                    head_after_double_quoteds,
                  )
                }
              }
          }
        }
      }
    }
  }
}

fn contains_one_of(thing: String, substrings: List(String)) -> String {
  case substrings {
    [] -> ""

    [first, ..rest] -> {
      case string.contains(thing, first) {
        True -> first
        False -> contains_one_of(thing, rest)
      }
    }
  }
}

fn check_good_tag_name(proposed_name) -> TentativeTagName {
  case string.is_empty(proposed_name) {
    True -> Error(EmptyTag)

    False -> {
      let something_illegal =
        contains_one_of(proposed_name, tag_illegal_characters)
      case string.is_empty(something_illegal) {
        True -> Ok(proposed_name)

        False -> Error(IllegalTagCharacter(proposed_name, something_illegal))
      }
    }
  }
}

fn assign_error_in_would_be_text_at_indent(
  indent: Int,
  line: BlamedLine,
) -> TentativeVXML {
  let BlamedLine(blame, suffix_indent, suffix) = line
  case suffix_indent == indent {
    False ->
      case suffix_indent % vxml_indent == 0 {
        True -> TentativeErrorIndentationTooLarge(blame, suffix)
        False -> TentativeErrorIndentationTooLarge(blame, suffix)
      }
    True -> {
      case string.starts_with(suffix, "\"") {
        False -> TentativeErrorTextNoOpeningQuote(blame, suffix)
        True -> {
          case is_double_quoted_thing(suffix) {
            False -> TentativeErrorTextNoClosingQuote(blame, suffix)
            True -> TentativeErrorTextOutOfPlace(blame, suffix)
          }
        }
      }
    }
  }
}

fn assign_errors_in_would_be_text_at_indent(
  indent: Int,
  lines: List(BlamedLine),
) -> List(TentativeVXML) {
  list.map(lines, assign_error_in_would_be_text_at_indent(indent, _))
}

fn continue_parsing(
  element: TentativeVXML,
  indent: Int,
  head: FileHead,
) -> #(List(TentativeVXML), FileHead) {
  let #(other_elements, head_after_indent) =
    tentative_parse_at_indent(indent, head)

  #([element, ..other_elements], head_after_indent)
}

fn continue_parsing_many(
  elements: List(TentativeVXML),
  indent: Int,
  head: FileHead,
) -> #(List(TentativeVXML), FileHead) {
  let #(other_elements, head_after_indent) =
    tentative_parse_at_indent(indent, head)

  #(list.flatten([elements, other_elements]), head_after_indent)
}

fn tentative_parse_at_indent(
  indent: Int,
  head: FileHead,
) -> #(List(TentativeVXML), FileHead) {
  case current_line(head) {
    None -> #([], head)

    Some(BlamedLine(blame, suffix_indent, suffix)) -> {
      case suffix == "" {
        True -> tentative_parse_at_indent(indent, move_forward(head))

        False -> {
          case suffix_indent < indent {
            True -> {
              case suffix_indent > indent - vxml_indent {
                True -> {
                  let error =
                    TentativeErrorIndentationNotMultipleOfFour(blame, suffix)

                  continue_parsing(error, indent, move_forward(head))
                }

                False -> #([], head)
              }
            }

            False ->
              case suffix_indent > indent {
                True -> {
                  let #(_, head_after_oversize_indent) =
                    fast_forward_past_lines_of_indent_at_least(
                      suffix_indent,
                      head,
                    )

                  let error = case suffix_indent % vxml_indent == 0 {
                    True -> {
                      let error_message =
                        "(" <> ins(suffix_indent) <> " > " <> ins(indent) <> ")"

                      TentativeErrorIndentationTooLarge(
                        blame,
                        error_message <> suffix,
                      )
                    }

                    False ->
                      TentativeErrorIndentationNotMultipleOfFour(blame, suffix)
                  }

                  continue_parsing(error, indent, head_after_oversize_indent)
                }

                False -> {
                  let assert True = suffix_indent == indent

                  case nonempty_suffix_diagnostic(suffix) {
                    TaggedCaret(annotation) -> {
                      let #(tentative_attributes, head_after_attributes) =
                        fast_forward_past_attribute_lines_at_indent(
                          indent + vxml_indent,
                          move_forward(head),
                        )

                      let #(children, remaining_after_children) =
                        tentative_parse_at_indent(
                          indent + vxml_indent,
                          head_after_attributes,
                        )

                      let tentative_tag =
                        TentativeV(
                          blame: blame,
                          tag: check_good_tag_name(string.trim(annotation)),
                          attributes: tentative_attributes,
                          children: children,
                        )

                      continue_parsing(
                        tentative_tag,
                        indent,
                        remaining_after_children,
                      )
                    }

                    EmptyCaret -> {
                      let #(indented_lines, remaining_after_indented_lines) =
                        fast_forward_past_lines_of_indent_at_least(
                          indent + vxml_indent,
                          move_forward(head),
                        )

                      let #(double_quoted_at_correct_indent, others) =
                        fast_forward_past_double_quoted_lines_at_indent(
                          indent + vxml_indent,
                          indented_lines,
                        )

                      let text_node_or_error = case
                        list.is_empty(double_quoted_at_correct_indent)
                      {
                        True -> TentativeErrorTextMissing(blame)
                        False ->
                          TentativeT(
                            blame: blame,
                            contents: double_quoted_at_correct_indent,
                          )
                      }

                      let error_siblings =
                        assign_errors_in_would_be_text_at_indent(
                          indent + vxml_indent,
                          others,
                        )

                      continue_parsing_many(
                        [text_node_or_error, ..error_siblings],
                        indent,
                        remaining_after_indented_lines,
                      )
                    }

                    DoubleQuoted(text) -> {
                      let error = TentativeErrorTextOutOfPlace(blame, text)
                      continue_parsing(error, indent, move_forward(head))
                    }

                    Other(something) -> {
                      let error = TentativeErrorCaretExpected(blame, something)
                      continue_parsing(error, indent, move_forward(head))
                    }
                  }
                }
              }
          }
        }
      }
    }
  }
}

//**************************
//* parsing from tentative *
//**************************

fn tentative_blamed_attribute_to_blamed_attribute(
  t: TentativeBlamedAttribute,
) -> Result(BlamedAttribute, VXMLParseError) {
  case t.key {
    Ok(key) -> Ok(BlamedAttribute(blame: t.blame, key: key, value: t.value))

    Error(IllegalAttributeKeyCharacter(original_would_be_key, bad_char)) ->
      Error(VXMLParseErrorIllegalAttributeKeyCharacter(
        t.blame,
        original_would_be_key,
        bad_char,
      ))
  }
}

fn tentative_blamed_attributes_to_blamed_attributes(
  attrs: List(TentativeBlamedAttribute),
) -> Result(List(BlamedAttribute), VXMLParseError) {
  case attrs {
    [] -> Ok([])
    [first, ..rest] ->
      case tentative_blamed_attribute_to_blamed_attribute(first) {
        Error(error) -> Error(error)
        Ok(blamed_attribute) ->
          case tentative_blamed_attributes_to_blamed_attributes(rest) {
            Ok(blamed_attributes) -> Ok([blamed_attribute, ..blamed_attributes])

            Error(error) -> Error(error)
          }
      }
  }
}

fn parse_from_tentatives(
  tentatives: List(TentativeVXML),
) -> Result(List(VXML), VXMLParseError) {
  case tentatives {
    [] -> Ok([])
    [first, ..rest] ->
      case parse_from_tentative(first) {
        Ok(parsed) ->
          case parse_from_tentatives(rest) {
            Ok(parseds) -> Ok([parsed, ..parseds])

            Error(error) -> Error(error)
          }

        Error(error) -> Error(error)
      }
  }
}

fn parse_from_tentative(
  tentative: TentativeVXML,
) -> Result(VXML, VXMLParseError) {
  case tentative {
    TentativeErrorIndentationTooLarge(blame, message) ->
      Error(VXMLParseErrorIndentationTooLarge(blame, message))

    TentativeErrorIndentationNotMultipleOfFour(blame, message) ->
      Error(VXMLParseErrorIndentationNotMultipleOfFour(blame, message))

    TentativeErrorTextMissing(blame) -> Error(VXMLParseErrorTextMissing(blame))

    TentativeErrorTextNoClosingQuote(blame, message) ->
      Error(VXMLParseErrorTextNoClosingQuote(blame, message))

    TentativeErrorTextNoOpeningQuote(blame, message) ->
      Error(VXMLParseErrorTextNoOpeningQuote(blame, message))

    TentativeErrorTextOutOfPlace(blame, message) ->
      Error(VXMLParseErrorTextOutOfPlace(blame, message))

    TentativeErrorCaretExpected(blame, message) ->
      Error(VXMLParseErrorCaretExpected(blame, message))

    TentativeT(blame, contents) -> Ok(T(blame, contents))

    TentativeV(blame, tentative_name, tentative_attributes, tentative_children) ->
      case tentative_name {
        Error(EmptyTag) -> Error(VXMLParseErrorEmptyTag(blame))

        Error(IllegalTagCharacter(original_bad_name, bad_char)) ->
          Error(VXMLParseErrorIllegalTagCharacter(
            blame,
            original_bad_name,
            bad_char,
          ))

        Ok(name) ->
          case
            tentative_blamed_attributes_to_blamed_attributes(
              tentative_attributes,
            )
          {
            Error(error) -> Error(error)

            Ok(attributes) ->
              case parse_from_tentatives(tentative_children) {
                Error(error) -> Error(error)

                Ok(children) ->
                  Ok(V(
                    blame: blame,
                    tag: name,
                    attributes: attributes,
                    children: children,
                  ))
              }
          }
      }
  }
}

//****************************************
//* tentative parsing api (blamed lines) *
//****************************************

fn tentative_parse_blamed_lines(
  head: FileHead,
  debug_messages: Bool,
) -> List(TentativeVXML) {
  let #(parsed, final_head) = tentative_parse_at_indent(0, head)
  let assert True = list.is_empty(final_head)

  case debug_messages {
    True -> debug_print_tentatives("(debug_print_tentatives)", parsed)
    False -> Nil
  }

  parsed
}

//****************************
//* debug printing Tentative *
//****************************

fn tentative_error_blame_and_type_and_message(
  t: TentativeVXML,
) -> #(Blame, String, String) {
  case t {
    TentativeT(_, _) -> panic as "not an error node"
    TentativeV(_, _, _, _) -> panic as "not an error node"
    TentativeErrorIndentationTooLarge(blame, message) -> #(
      blame,
      "IndentationTooLarge",
      message,
    )
    TentativeErrorIndentationNotMultipleOfFour(blame, message) -> #(
      blame,
      "IndentationNotMultipleOfFour",
      message,
    )
    TentativeErrorCaretExpected(blame, message) -> #(
      blame,
      "CareExpected",
      message,
    )
    TentativeErrorTextMissing(blame) -> #(blame, "TextMissing", "")
    TentativeErrorTextNoClosingQuote(blame, message) -> #(
      blame,
      "TextNoClosingQuote",
      message,
    )
    TentativeErrorTextNoOpeningQuote(blame, message) -> #(
      blame,
      "TextNoOpeningQuote",
      message,
    )
    TentativeErrorTextOutOfPlace(blame, message) -> #(
      blame,
      "TextOutOfPlace",
      message,
    )
  }
}

fn tentative_to_blamed_lines_internal(
  t: TentativeVXML,
  indentation: Int,
) -> List(BlamedLine) {
  case t {
    TentativeT(blame, blamed_contents) -> {
      [
        BlamedLine(blame, indentation, "<>"),
        ..list.map(blamed_contents, fn(blamed_content) -> BlamedLine {
          BlamedLine(
            blamed_content.blame,
            indentation + vxml_indent,
            blamed_content.content,
          )
        })
      ]
    }

    TentativeV(
      blame,
      Ok(_) as tag_result,
      tentative_blamed_attributes,
      children,
    ) -> {
      [
        BlamedLine(blame, indentation, "<> " <> ins(tag_result)),
        ..list.flatten([
          list.map(tentative_blamed_attributes, fn(tentative_blamed_attribute) {
            BlamedLine(
              tentative_blamed_attribute.blame,
              indentation + vxml_indent,
              ins(tentative_blamed_attribute.key)
                <> " "
                <> tentative_blamed_attribute.value,
            )
          }),
          tentatives_to_blamed_lines_internal(children, indentation + vxml_indent),
        ])
      ]
    }

    TentativeV(blame, Error(err), tentative_blamed_attributes, children) -> {
      [
        BlamedLine(
          blame |> pc("ERROR BadTagName"),
          indentation,
          "<> " <> ins(err),
        ),
        ..list.flatten([
          list.map(tentative_blamed_attributes, fn(tentative_blamed_attribute) {
            BlamedLine(
              tentative_blamed_attribute.blame,
              indentation + vxml_indent,
              ins(tentative_blamed_attribute.key)
                <> " "
                <> tentative_blamed_attribute.value,
            )
          }),
          tentatives_to_blamed_lines_internal(children, indentation + vxml_indent),
        ])
      ]
    }

    _ -> {
      let #(blame, error_type, message) =
        tentative_error_blame_and_type_and_message(t)
      [BlamedLine(blame |> pc("ERROR " <> error_type), indentation, message)]
    }
  }
}

fn tentatives_to_blamed_lines_internal(
  tentatives: List(TentativeVXML),
  indentation: Int,
) -> List(BlamedLine) {
  tentatives
  |> list.map(tentative_to_blamed_lines_internal(_, indentation))
  |> list.flatten
}

fn debug_print_tentatives(banner: String, tentatives: List(TentativeVXML)) {
  tentatives
  |> tentatives_to_blamed_lines_internal(0)
  |> bl.blamed_lines_to_table_vanilla_bob_and_jane_sue(banner, _)
  |> io.print
}

//*************************
//* debug annotating VXML *
//*************************

pub fn debug_annotate_vxml(vxml: VXML) -> VXML {
  case vxml {
    T(blame, blamed_contents) -> {
      T(
        blame |> pc("T"),
        list.index_map(blamed_contents, fn(blamed_content, i) {
          BlamedContent(
            blamed_content.blame
              |> pc("T > BlamedContent(" <> ins(i + 1) <> ")"),
            blamed_content.content,
          )
        }),
      )
    }
    V(blame, tag, attributes, children) -> {
      V(
        blame |> pc("V"),
        tag,
        list.index_map(attributes, fn(attribute, i) {
          BlamedAttribute(
            attribute.blame |> pc("Attribute(" <> ins(i + 1) <> ")"),
            attribute.key,
            attribute.value,
          )
        }),
        debug_annotate_vxmls(children),
      )
    }
  }
}

pub fn debug_annotate_vxmls(vxmls: List(VXML)) -> List(VXML) {
  list.map(vxmls, debug_annotate_vxml)
}

//****************************
//* VXML -> List(BlamedLine) *
//****************************

fn vxml_to_blamed_lines_internal(
  vxml: VXML,
  indentation: Int,
) -> List(BlamedLine) {
  case vxml {
    T(blame, blamed_contents) -> [
      BlamedLine(blame, indentation, "<>"),
      ..list.map(blamed_contents, fn(blamed_content) {
        BlamedLine(
          blamed_content.blame,
          indentation + vxml_indent,
          add_quotes(blamed_content.content),
        )
      })
    ]

    V(blame, tag, blamed_attributes, children) -> {
      [
        BlamedLine(blame, indentation, "<> " <> tag),
        ..list.append(
          list.map(blamed_attributes, fn(blamed_attribute) {
            BlamedLine(
              blamed_attribute.blame,
              indentation + vxml_indent,
              blamed_attribute.key <> "=" <> blamed_attribute.value,
            )
          }),
          vxmls_to_blamed_lines_internal(children, indentation + vxml_indent),
        )
      ]
    }
  }
}

fn vxmls_to_blamed_lines_internal(
  vxmls: List(VXML),
  indentation: Int,
) -> List(BlamedLine) {
  vxmls
  |> list.map(vxml_to_blamed_lines_internal(_, indentation))
  |> list.flatten
}

pub fn vxml_to_blamed_lines(vxml: VXML) -> List(BlamedLine) {
  vxml_to_blamed_lines_internal(vxml, 0)
}

pub fn vxmls_to_blamed_lines(vxmls: List(VXML)) -> List(BlamedLine) {
  vxmls_to_blamed_lines_internal(vxmls, 0)
}

//******************
//* VXML -> String *
//******************

pub fn vxmls_to_string(vxmls: List(VXML)) -> String {
  vxmls
  |> vxmls_to_blamed_lines
  |> bl.blamed_lines_to_string
}

pub fn vxml_to_string(vxml: VXML) -> String {
  vxml
  |> vxml_to_blamed_lines
  |> bl.blamed_lines_to_string
}

//************************
//* VXML -> debug String *
//************************

pub fn debug_vxmls_to_string(banner: String, vxmls: List(VXML)) -> String {
  vxmls
  |> debug_annotate_vxmls
  |> vxmls_to_blamed_lines
  |> bl.blamed_lines_to_table_vanilla_bob_and_jane_sue(banner, _)
}

pub fn debug_vxml_to_string(banner: String, vxml: VXML) -> String {
  vxml
  |> debug_annotate_vxml
  |> vxml_to_blamed_lines
  |> bl.blamed_lines_to_table_vanilla_bob_and_jane_sue(banner, _)
}

//***************
//* debug print *
//***************

pub fn debug_print_vxml(banner: String, vxml: VXML) {
  vxml
  |> debug_vxml_to_string(banner, _)
  |> io.print
}

pub fn debug_print_vxmls(banner: String, vxmls: List(VXML)) {
  vxmls
  |> debug_vxmls_to_string(banner, _)
  |> io.print
}

pub fn debug_print_vxmls_and(vxmls: List(VXML), banner: String) -> List(VXML) {
  vxmls
  |> debug_vxmls_to_string(banner, _)
  |> io.print
  vxmls
}

//***************
//* VXML -> jsx *
//***************

fn jsx_string_processor(content: String, ampersand_replacer: regexp.Regexp) -> String {
  content
  |> regexp.replace(ampersand_replacer, _, "&amp;")
  |> string.replace("{", "&#123;")
  |> string.replace("}", "&#125;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
}

fn jsx_attribute(b: BlamedAttribute, ampersand_replacer: regexp.Regexp) -> String {
  let value = string.trim(b.value) |> jsx_string_processor(ampersand_replacer)
  case value == "false" || value == "true" || result.is_ok(int.parse(value)) {
    True -> b.key <> "={" <> value <> "}"
    False -> b.key <> "=\"" <> value <> "\""
  }
}

fn tag_close_blamed_lines(
  blame: Blame,
  tag: String,
  indent: Int,
) -> List(BlamedLine) {
  [
    BlamedLine(blame: blame, indent: indent, suffix: "</" <> tag <> ">")
  ]
}

fn tag_open_blamed_lines(
  blame: Blame,
  tag: String,
  indent: Int,
  closing: String,
  attributes: List(BlamedAttribute),
  ampersand_replacer: regexp.Regexp,
) -> List(BlamedLine) {
  case attributes {
    [] -> [
      BlamedLine(blame: blame, indent: indent, suffix: "<" <> tag <> closing),
    ]
    [first] -> [
      BlamedLine(
        blame: blame,
        indent: indent,
        suffix: "<" <> tag <> " " <> jsx_attribute(first, ampersand_replacer) <> closing,
      ),
    ]
    _ -> {
      let tag_line =
        BlamedLine(blame: blame, indent: indent, suffix: "<" <> tag)
      let attribute_lines =
        attributes_to_blamed_lines(attributes, indent + 2, closing, ampersand_replacer)
      [tag_line, ..attribute_lines]
    }
  }
}

fn add_text_to_last_blamed_line(
  lines: List(BlamedLine),
  text: String,
) -> List(BlamedLine) {
  case lines |> list.reverse {
    [] -> []
    [last, ..rest] -> {
      [BlamedLine(..last, suffix: last.suffix <> text), ..rest]
      |> list.reverse
    }
  }
}

fn attributes_to_blamed_lines(
  attributes: List(BlamedAttribute),
  indent: Int,
  include_at_last: String,
  ampersand_replacer,
) -> List(BlamedLine) {
  attributes
  |> list.map(fn(t) {
    BlamedLine(blame: t.blame, indent: indent, suffix: jsx_attribute(t, ampersand_replacer))
  })
  |> add_text_to_last_blamed_line(include_at_last)
}

fn bool_2_jsx_space(b: Bool) -> String {
  case b {
    True -> "{\" \"}"
    False -> ""
  }
}

fn vxml_to_jsx_blamed_lines_internal(t: VXML, indent: Int, ampersand_replacer: regexp.Regexp) -> List(BlamedLine) {
  case t {
    T(_, blamed_contents) -> {
      let n = list.length(blamed_contents)
      blamed_contents
      |> list.index_map(fn(t, i) {
        BlamedLine(blame: t.blame, indent: indent, suffix: {
          let content = jsx_string_processor(t.content, ampersand_replacer)
          let start = {i == 0 && {string.starts_with(content, " ") || string.is_empty(content)}} |> bool_2_jsx_space
          let end = {i == n - 1 && {string.ends_with(content, " ") || string.is_empty(content)}} |> bool_2_jsx_space
          start <> content <> end
        })
      })
    }

    V(blame, tag, blamed_attributes, children) -> {
      case list.is_empty(children) {
        False ->
          [
            tag_open_blamed_lines(blame, tag, indent, ">", blamed_attributes, ampersand_replacer),
            vxmls_to_jsx_blamed_lines_internal(children, indent + 2, ampersand_replacer),
            tag_close_blamed_lines(blame, tag, indent),
          ]
          |> list.flatten

        True ->
          tag_open_blamed_lines(blame, tag, indent, " />", blamed_attributes, ampersand_replacer)
      }
    }
  }
}

fn vxmls_to_jsx_blamed_lines_internal(
  vxmls: List(VXML),
  indent: Int,
  ampersand_replacer: regexp.Regexp,
) -> List(BlamedLine) {
  vxmls
  |> list.map(vxml_to_jsx_blamed_lines_internal(_, indent, ampersand_replacer))
  |> list.flatten
}

pub fn vxml_to_jsx_blamed_lines(vxml: VXML, indent: Int) -> List(BlamedLine) {
  // a regex that matches ampersands that appear outside of html entities:
  let assert Ok(ampersand_replacer) = regexp.from_string("&(?!(?:[a-z]{2,6};|#\\d{2,4};))")
  vxml_to_jsx_blamed_lines_internal(vxml, indent, ampersand_replacer)
}

pub fn vxmls_to_jsx_blamed_lines(vxmls: List(VXML), indent: Int) -> List(BlamedLine) {
  // a regex that matches ampersands that appear outside of html entities:
  let assert Ok(ampersand_replacer) = regexp.from_string("&(?!(?:[a-z]{2,6};|#\\d{2,4};))")
  vxmls_to_jsx_blamed_lines_internal(vxmls, indent, ampersand_replacer)
}

pub fn vxml_to_jsx(vxml: VXML, indent: Int) -> String {
  vxml_to_jsx_blamed_lines(vxml, indent)
  |> bl.blamed_lines_to_string
}

pub fn debug_vxml_to_jsx(banner: String, vxml: VXML) -> String {
  vxml
  |> debug_annotate_vxml
  |> vxml_to_jsx_blamed_lines(0)
  |> bl.blamed_lines_to_table_vanilla_bob_and_jane_sue(banner, _)
}

// **********************
// * VXML -> html
// **********************

fn html_string_processor(content: String, ampersand_replacer: regexp.Regexp) -> String {
  content
  |> regexp.replace(ampersand_replacer, _, "&amp;")
  |> string.replace("<", "&lt;")
  |> string.replace(">", "&gt;")
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

fn sticky_2_blamed(stickie: StickyLine) -> BlamedLine {
  BlamedLine(stickie.blame, stickie.indent, stickie.content)
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

fn transvase(to: List(a), from: List(a)) -> List(a) {
  case from {
    [] -> to
    [first, ..rest] -> transvase([first, ..to], rest)
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
  let already_stuck = transvase(already_stuck, opening_lines)
  let already_stuck = sticky_trees_2_sticky_lines(already_stuck, children)
  transvase(already_stuck, closing_lines)
}

fn attributes_to_sticky_lines(
  attributes: List(BlamedAttribute),
  indent: Int,
  inline: Bool,
) -> List(StickyLine) {
  attributes
  |> list.map(fn(t) {
    StickyLine(
      blame: t.blame,
      indent: indent,
      content: t.key <> "=\"" <> t.value <> "\"",
      sticky_start: inline,
      sticky_end: inline,
    )
  })
}

const sticky_tags = [
  "NumberedTitle", "a", "span", "i", "b", "strong", "em", "code", "tt", "br"
]

const self_closing_tags = ["img", "br", "hr"]

fn opening_tag_to_sticky_lines(
  t: VXML,
  indent: Int,
  spaces: Int,
  pre: Bool,
) -> List(StickyLine) {
  let assert V(blame, tag, attributes, _) = t
  let indent = case pre {
    True -> 0
    False -> indent
  }
  let sticky_outside = list.contains(sticky_tags, tag)
  let sticky_inside = list.is_empty(attributes)
  list.flatten([
    [StickyLine(blame, indent, "<" <> tag, sticky_outside, sticky_inside)],
    attributes_to_sticky_lines(attributes, indent + spaces, sticky_inside),
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

pub fn first_rest(l: List(a)) -> Result(#(a, List(a)), Nil) {
  case l {
    [] -> Error(Nil)
    [first, ..rest] -> Ok(#(first, rest))
  }
}

pub fn head_last(l: List(a)) -> Result(#(List(a), a), Nil) {
  case l {
    [] -> Error(Nil)
    [last] -> Ok(#([], last))
    [first, ..rest] -> {
      let assert Ok(#(head, last)) = head_last(rest)
      Ok(#([first, ..head], last))
    }
  }
}

fn t_sticky_lines(t: VXML, indent: Int, pre: Bool, ampersand_replacer: regexp.Regexp) -> List(StickyLine) {
  let assert T(_, contents) = t
  let indent = case pre {
    True -> 0
    False -> indent
  }
  let last_index = list.length(contents) - 1
  let sticky_lines = list.index_map(
    contents,
    fn(b, i) {
      let content = html_string_processor(b.content, ampersand_replacer)
      StickyLine(
        blame: b.blame,
        indent: indent,
        content: content,
        sticky_start: i == 0 && !string.starts_with(content, " "),
        sticky_end: i == last_index && !string.ends_with(content, " "),
      )
    }
  )
  // if not pre:
  // - while contents have at least 1 content:
  //   - any starting blanks of first content can be removed (start is automatically non-sticky in that case)
  //   - any ending blanks of last content can be removed (end is automatically non-sticky in that case)
  //   - if first content is empty and at least 2 contents, can remove first
  //   - if last content is empty and at least 2 contents, can remove last
  //   - if first == last content is empty, can make sticky_start = False, sticky_end = True to induce simple newline at that indent
  case pre {
    True -> sticky_lines
    False -> t_very_fancy_contents_post_processing(sticky_lines)
  }
}

fn t_very_fancy_contents_post_processing(ls: List(StickyLine)) -> List(StickyLine) {
  // see 'if not pre' comment above for what this function
  // thinks it's doing

  let trim_start = fn(sticky: StickyLine) -> StickyLine {
    StickyLine(..sticky, content: string.trim_start(sticky.content))
  }

  let trim_last = fn(sticky: StickyLine) -> StickyLine {
    StickyLine(..sticky, content: string.trim_end(sticky.content))
  }

  let assert Ok(#(first, rest)) = first_rest(ls)

  case string.starts_with(first.content, " ") {
    True -> {
      // action 1: the start is not sticky anyway, so
      // trim starting spaces (this function is never called in 'pre' btw)
      let assert True = first.sticky_start == False
      t_very_fancy_contents_post_processing([trim_start(first), ..rest])
    }
    False -> {
      case first.content == "" {
        True -> case list.is_empty(rest) {
          False -> {
            // action 2: the next line is not sticky anyway, so drop
            // this empty line and keep only the others
            let assert Ok(new_first) = list.first(rest)
            let assert True = new_first.sticky_start == False
            t_very_fancy_contents_post_processing(rest)
          }
          True -> {
            // action 3: we have only 1 empty line, make it non-sticky
            // at start and sticky at end to simulate a plain newline
            [StickyLine(..first, sticky_start: False, sticky_end: True)]
          }
        }
        False -> {
          let assert Ok(#(head, last)) = head_last(ls)
          case string.ends_with(last.content, " ") {
            True -> {
              // action 4 mirroring action 1: the end is not sticky anyway,
              // so trim ending spaces of last line
              let assert True = last.sticky_end == False
              t_very_fancy_contents_post_processing(list.append(head, [trim_last(last)]))
            }
            False -> {
              case last.content == "" {
                True -> case list.is_empty(head) {
                  False -> {
                    // action 5 mirroring action 2: the previous line is not
                    // sticky anyway at end, so drop last empty line
                    let assert Ok(new_last) = list.last(head)
                    let assert True = new_last.sticky_end == False
                    t_very_fancy_contents_post_processing(head)
                  }
                  True -> panic as "don't think we should get here?"
                }
                False -> ls // (could not find anything to change)
              }
            }
          }
        }
      }
    }
  }
}

fn t_sticky_tree(t: VXML, indent: Int, pre: Bool, ampersand_replacer: regexp.Regexp) -> StickyTree {
  StickyTree(
    opening_lines: t_sticky_lines(t, indent, pre, ampersand_replacer),
    children: [],
    closing_lines: [],
  )
}

fn v_sticky_tree(v: VXML, indent: Int, spaces: Int, pre: Bool, ampersand_replacer: regexp.Regexp) -> StickyTree {
  let assert V(_, tag, _, children) = v
  let pre = pre || tag |> string.lowercase == "pre"
  StickyTree(
    opening_lines: opening_tag_to_sticky_lines(v, indent, spaces, pre),
    children: children |> list.map(vxml_sticky_tree(_, indent + spaces, spaces, pre, ampersand_replacer)),
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
  ampersand_replacer: regexp.Regexp,
) -> StickyTree {
  case node {
    T(_, _) -> t_sticky_tree(node, indent, pre, ampersand_replacer)
    V(_, _, _, _) -> v_sticky_tree(node, indent, spaces, pre, ampersand_replacer)
  }
}

pub fn vxml_to_html_blamed_lines_internal(
  node: VXML,
  indent: Int,
  spaces: Int,
  ampersand_replacer: regexp.Regexp,
) -> List(BlamedLine) {
  vxml_sticky_tree(node, indent, spaces, False, ampersand_replacer)
  |> sticky_tree_2_sticky_lines([], _)
  |> list.reverse
  |> concat_sticky_lines
  |> list.map(sticky_2_blamed)
}

pub fn vxmls_to_html_blamed_lines_internal(
  vxmls: List(VXML),
  indent: Int,
  spaces: Int,
  ampersand_replacer: regexp.Regexp,
) -> List(BlamedLine) {
  vxmls
  |> list.map(vxml_to_html_blamed_lines_internal(_, indent, spaces, ampersand_replacer))
  |> list.flatten
}

pub fn vxml_to_html_blamed_lines(
  node: VXML,
  indent: Int,
  spaces: Int,
) -> List(BlamedLine) {
  let assert Ok(ampersand_replacer) = regexp.from_string("&(?!(?:[a-z]{2,6};|#\\d{2,4};))")
  vxml_to_html_blamed_lines_internal(node, indent, spaces, ampersand_replacer)
}

pub fn vxmls_to_html_blamed_lines(
  vxmls: List(VXML),
  indent: Int,
  spaces: Int,
) -> List(BlamedLine) {
  let assert Ok(ampersand_replacer) = regexp.from_string("&(?!(?:[a-z]{2,6};|#\\d{2,4};))")
  vxmls_to_html_blamed_lines_internal(vxmls, indent, spaces, ampersand_replacer)
}

//**********************
//* parse_blamed_lines *
//**********************

pub fn parse_blamed_lines(
  lines,
  debug_messages: Bool,
) -> Result(List(VXML), VXMLParseError) {
  lines
  |> tentative_parse_blamed_lines(debug_messages)
  |> parse_from_tentatives
}

//****************
//* parse_string *
//****************

pub fn parse_string(
  source: String,
  shortname_for_blame: String,
  debug_messages: Bool,
  unique_root: Bool,
) -> Result(List(VXML), VXMLParseError) {
  bl.string_to_blamed_lines_easy_mode(source, shortname_for_blame)
  |> parse_blamed_lines(debug_messages)
  |> result.try(
    fn(vxmls) {
      case unique_root && list.length(vxmls) != 1 {
        True -> Error(VXMLParseErrorNonUniqueRoot(list.length(vxmls)))
        False -> Ok(vxmls)
      }
    }
  )
}

pub fn unique_root_parse_string(
  source: String,
  shortname_for_blame: String,
  debug_messages: Bool,
) -> Result(VXML, VXMLParseError) {
  use vxmls <- result.try(parse_string(source, shortname_for_blame, debug_messages, True))
  let assert [vxml] = vxmls
  Ok(vxml)
}

//**************
//* parse_file *
//**************

fn println_if(condition: Bool, message: String) -> Nil {
  case condition {
    True -> io.println(message)
    False -> Nil
  }
}

pub fn parse_file(
  path: String,
  shortname_for_blame: String,
  debug_messages: Bool,
  unique_root: Bool,
) -> Result(List(VXML), VXMLParseFileError) {
  case simplifile.read(path) {
    Error(io_error) -> {
      println_if(
        debug_messages,
        "\nerror reading " <> path <> ": " <> ins(io_error),
      )
      Error(IOError(io_error))
    }

    Ok(file) -> {
      case parse_string(file, shortname_for_blame, debug_messages, unique_root) {
        Ok(vxmls) -> {
          println_if(debug_messages, "\nsuccessfully parsed " <> path)
          Ok(vxmls)
        }

        Error(parse_error) -> {
          println_if(
            debug_messages,
            "encountered parse error while parsing " <> path,
          )
          Error(DocumentError(parse_error))
        }
      }
    }
  }
}

//********
//* main *
//********

pub type XMLMParseError {
  XMLMIOError(String)
  XMLMParseError(String)
}

fn xmlm_attribute_to_vxml_attributes(
  filename: String,
  line_no: Int,
  xmlm_attribute: xmlm.Attribute,
) -> BlamedAttribute {
  let blame = Blame(filename, line_no, 0, [])
  BlamedAttribute(blame, xmlm_attribute.name.local, xmlm_attribute.value)
}

pub fn on_error_on_ok(res: Result(a, b), f1: fn(b) -> c, f2: fn(a) -> c) -> c {
  case res {
    Error(e) -> f1(e)
    Ok(r) -> f2(r)
  }
}

pub fn xmlm_based_html_parser(
  content: String,
  filename: String,
) -> Result(VXML, XMLMParseError) {
  // some preliminary cleanup that avoids complaints
  // from the xmlm parser:
  let content = string.replace(content, "async ", "async=\"\"")
  let content = string.replace(content, "async\n", "async=\"\"\n")
  let content = string.replace(content, "\\,<", "\\,&lt;")
  let content = string.replace(content, " < ", " &lt; ")
  let content = string.replace(content, "\\rt{0.1}<", "\\rt{0.1}&lt;")
  let content = string.replace(content, "& ", "&amp;")
  let content = string.replace(content, "&\n", "&amp;\n")
  let content = string.replace(content, "\r\n", "\n")
  let content = string.replace(content, " &", "&amp;")
  let content = string.replace(content, "{", "&#123;")
  let content = string.replace(content, "}", "&#125;")
  let content = string.replace(content, "|", "&#124;")

  // close img tags
  let assert Ok(re) = regexp.from_string("(<img)(\\b(?![^>]*\\/\\s*>)[^>]*)(>)")
  let content =
    regexp.match_map(re, content, fn(match) {
      let regexp.Match(_, sub) = match
      let assert [_, Some(middle), _] = sub
      "<img" <> middle <> "/>"
    })

  // remove attributes in closing tags
  let assert Ok(re) = regexp.from_string("(<\\/)(\\w+)(\\s+[^>]*)(>)")
  let matches = regexp.scan(re, content)

  let content =
    list.fold(matches, content, fn(content_str, match) {
      let regexp.Match(_, sub) = match
      let assert [_, Some(tag), _, _] = sub
      regexp.replace(re, content_str, "</" <> tag <> ">")
    })

  // content
  // |> string.split("\n")
  // |> list.each(io.println)

  let input = xmlm.from_string(content)

  // **********
  // use this to debug if you get an input_error on a file, see
  // "input_error" case at end of function
  // **********
  // // case xmlm.signals(
  // //   input
  // // ) {
  // //   Ok(#(signals, _)) -> {
  // //     list.each(
  // //       signals,
  // //       fn(signal) {io.println(signal |> xmlm.signal_to_string)}
  // //     )
  // //   }
  // //   Error(input_error) -> {
  // //     io.println("got error:" <> ins(input_error))
  // //   }
  // // }

  case
    xmlm.document_tree(
      input,
      fn(xmlm_tag, children) {
        V(
          Blame(filename, 0, 0, []),
          xmlm_tag.name.local,
          xmlm_tag.attributes
            |> list.map(xmlm_attribute_to_vxml_attributes(filename, 0, _)),
          children,
        )
      },
      fn(content) {
        let blamed_contents =
          content
          |> string.split("\n")
          |> list.map(fn(content) {
            BlamedContent(Blame(filename, 0, 0, []), content)
          })
        T(Blame(filename, 0, 0, []), blamed_contents)
      },
    )
  {
    Ok(#(_, vxml, _)) -> {
      vxml |> Ok
    }
    Error(input_error) -> {
      input_error |> ins |> XMLMParseError |> Error
    }
  }
}

fn test_vxml_sample() {
  let path = "test/sample.vxml"
  io.println("")
  case parse_file(path, "sample", True, False) {
    Error(IOError(error)) -> io.println("there was an IOError: " <> ins(error))

    Error(DocumentError(error)) ->
      io.println("there was a parsing error: " <> ins(error))

    Ok(vxmls) -> {
      debug_print_vxmls("(debug_print_vxmls)", vxmls)

      io.println("")
      io.println(vxmls_to_string(vxmls))

      io.println("")
      vxmls_to_html_blamed_lines(vxmls, 0, 2)
      |> bl.blamed_lines_to_table_vanilla_bob_and_jane_sue("lines", _)
      |> io.print
    }
  }
}

fn test_html_sample() {
  let path = "test/sample.html"

  use content <- on_error_on_ok(simplifile.read(path), fn(_) {
    io.println("could not read file " <> path)
  })

  use vxml <- on_error_on_ok(xmlm_based_html_parser(content, path), fn(e) {
    io.println("xmlm_based_html_parser error: " <> ins(e))
  })

  debug_print_vxml("(debug_print_vxmls)", vxml)
  io.println("")
  io.println(vxml_to_string(vxml))
}

fn test_regex() {
  let assert Ok(a) = regexp.from_string("&(?!(?:[a-z]{2,6};|#\\d{2,4};))")
  echo regexp.replace(a, " &amp;&Gamma; ", "&amp;")
  echo regexp.replace(a, " &a#a; ", "&amp;")
}

pub fn make_linter_shut_up() {
  test_vxml_sample()
  test_html_sample()
  test_regex()
}

pub fn main() {
  test_vxml_sample()
  // test_html_sample()
  // test_regex()
}
