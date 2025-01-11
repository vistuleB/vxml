import blamedlines.{
  type Blame, type BlamedLine, Blame, BlamedLine, prepend_comment as pc,
}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile

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
                    |> string.split_once(" ")
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
      case suffix_indent % 4 == 0 {
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
              case suffix_indent > indent - 4 {
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

                  let error = case suffix_indent % 4 == 0 {
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
                          indent + 4,
                          move_forward(head),
                        )

                      let #(children, remaining_after_children) =
                        tentative_parse_at_indent(
                          indent + 4,
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
                          indent + 4,
                          move_forward(head),
                        )

                      let #(double_quoted_at_correct_indent, others) =
                        fast_forward_past_double_quoted_lines_at_indent(
                          indent + 4,
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
                          indent + 4,
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
            indentation + 4,
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
              indentation + 4,
              ins(tentative_blamed_attribute.key)
                <> " "
                <> tentative_blamed_attribute.value,
            )
          }),
          tentatives_to_blamed_lines_internal(children, indentation + 4),
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
              indentation + 4,
              ins(tentative_blamed_attribute.key)
                <> " "
                <> tentative_blamed_attribute.value,
            )
          }),
          tentatives_to_blamed_lines_internal(children, indentation + 4),
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
  |> blamedlines.blamed_lines_to_table_vanilla_bob_and_jane_sue(banner, _)
  |> io.print
}

//***********************
//* debug printing VXML *
//***********************

pub fn debug_annotate_blames(vxml: VXML) -> VXML {
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
        list.map(children, debug_annotate_blames),
      )
    }
  }
}

fn vxml_to_blamed_lines_internal(
  node: VXML,
  indentation: Int,
) -> List(BlamedLine) {
  case node {
    T(blame, blamed_contents) -> [
      BlamedLine(blame, indentation, "<>"),
      ..list.map(blamed_contents, fn(blamed_content) {
        BlamedLine(
          blamed_content.blame,
          indentation + 4,
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
              indentation + 4,
              blamed_attribute.key <> " " <> blamed_attribute.value,
            )
          }),
          vxmls_to_blamed_lines_internal(children, indentation + 4),
        )
      ]
    }
  }
}

fn vxmls_to_blamed_lines_internal(
  tentatives: List(VXML),
  indentation: Int,
) -> List(BlamedLine) {
  tentatives
  |> list.map(vxml_to_blamed_lines_internal(_, indentation))
  |> list.flatten
}

pub fn debug_vxmls_to_string(banner: String, vxmls: List(VXML)) -> String {
  vxmls
  |> list.map(debug_annotate_blames)
  |> vxmls_to_blamed_lines_internal(0)
  |> blamedlines.blamed_lines_to_table_vanilla_bob_and_jane_sue(banner, _)
}

pub fn debug_vxml_to_string(banner: String, vxml: VXML) -> String {
  [vxml]
  |> debug_vxmls_to_string(banner, _)
}

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

//******************
//* vxml -> string *
//******************

pub fn vxmls_to_string(vxmls: List(VXML)) -> String {
  vxmls
  |> vxmls_to_blamed_lines_internal(0)
  |> blamedlines.blamed_lines_to_string
}

pub fn vxml_to_string(vxml: VXML) -> String {
  vxml
  |> vxml_to_blamed_lines_internal(0)
  |> blamedlines.blamed_lines_to_string
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
) -> Result(List(VXML), VXMLParseError) {
  blamedlines.string_to_blamed_lines_easy_mode(source, shortname_for_blame)
  |> parse_blamed_lines(debug_messages)
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
      case parse_string(file, shortname_for_blame, debug_messages) {
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

fn test_sample() {
  let path = "test/sample.vxml"

  io.println("")

  case parse_file(path, "sample", True) {
    Error(IOError(error)) -> io.println("there was an IOError: " <> ins(error))

    Error(DocumentError(error)) ->
      io.println("there was a parsing error: " <> ins(error))

    Ok(vxmls) -> {
      debug_print_vxmls("(debug_print_vxmls)", vxmls)

      io.println("")

      io.println(vxmls_to_string(vxmls))
    }
  }
}

pub fn main() {
  test_sample()
}
