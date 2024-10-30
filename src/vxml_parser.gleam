import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/string
import simplifile

//****************
//* public types *
//****************

pub type Blame {
  Blame(filename: String, line_no: Int, comments: List(String))
}

pub type BlamedContent {
  BlamedContent(blame: Blame, content: String)
}

pub type BlamedAttribute {
  BlamedAttribute(blame: Blame, key: String, value: String)
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

pub type BlamedLine {
  BlamedLine(blame: Blame, indent: Int, suffix: String)
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
      let tag = string.trim(string.drop_left(suffix, 2))
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
  s |> string.drop_left(1) |> string.drop_right(1)
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

  #(list.concat([elements, other_elements]), head_after_indent)
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
                        "indent too large "
                        <> ins(suffix_indent)
                        <> " > "
                        <> ins(indent)

                      TentativeErrorIndentationTooLarge(blame, error_message)
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

//************************
//* blamed line building *
//************************

fn add_blames_map_fold(
  current_info: #(Int, String),
  // line_number, filename
  current_line: #(Int, String),
  // indent, suffix
) -> #(#(Int, String), BlamedLine) {
  let #(line_number, filename) = current_info
  let #(indent, suffix) = current_line
  #(
    #(line_number + 1, filename),
    BlamedLine(Blame(filename, line_number, []), indent, suffix),
  )
}

fn add_blames(
  pairs: List(#(Int, String)),
  proto_blame: #(Int, String),
) -> List(BlamedLine) {
  list.map_fold(pairs, proto_blame, add_blames_map_fold)
  |> pair.second
}

fn line_to_indent_suffix_pair(line: String, extra_indent: Int) -> #(Int, String) {
  let suffix = string.trim_left(line)
  let indent = string.length(line) - string.length(suffix)
  #(indent + extra_indent, suffix)
}

pub fn string_to_blamed_lines(
  source: String,
  filename: String,
) -> List(BlamedLine) {
  string.split(source, "\n")
  |> list.map(line_to_indent_suffix_pair(_, 0))
  |> add_blames(#(1, filename))
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

//************
//* printing *
//************

const pre_announce_pad_to = 60

const margin_announce_pad_to = 30

const debug_print_spaces = "    "

fn margin_assembler(
  pre_blame: String,
  blame: Blame,
  announce: String,
  margin: String,
) -> String {
  let up_to_line_number =
    pre_blame <> blame.filename <> ":" <> ins(blame.line_no)

  string.pad_right(up_to_line_number, pre_announce_pad_to, " ")
  <> string.pad_right(announce, margin_announce_pad_to, " ")
  <> "###"
  <> margin
}

fn margin_error_assembler(
  pre_blame: String,
  blame: Blame,
  error_message: String,
) -> String {
  let up_to_line_number =
    pre_blame <> blame.filename <> ":" <> ins(blame.line_no)

  string.pad_right(up_to_line_number, pre_announce_pad_to, " ") <> error_message
}

//**********************
//* printing Tentative *
//**********************

fn debug_print_tentative_internal(
  pre_blame: String,
  indentation: String,
  t: TentativeVXML,
) {
  case t {
    TentativeT(blame, blamed_contents) -> {
      { margin_assembler(pre_blame, blame, "text_node", indentation) <> "<>" }
      |> io.println

      list.map(blamed_contents, fn(blamed_content) {
        {
          margin_assembler(
            pre_blame,
            blamed_content.blame,
            "text_line",
            indentation,
          )
          <> debug_print_spaces
          <> add_quotes(blamed_content.content)
        }
        |> io.println
      })

      Nil
    }

    TentativeV(blame, tag, tentative_blamed_attributes, children) -> {
      io.println(
        margin_assembler(pre_blame, blame, "tag", indentation)
        <> "<>"
        <> " "
        <> ins(tag),
      )

      list.map(tentative_blamed_attributes, fn(t) {
        {
          margin_assembler(pre_blame, t.blame, "attribute", indentation)
          <> debug_print_spaces
          <> ins(t.key)
          <> " "
          <> t.value
        }
        |> io.println
      })

      debug_print_tentatives_internal(
        pre_blame,
        indentation <> debug_print_spaces,
        children,
      )
    }

    TentativeErrorIndentationTooLarge(blame, message) ->
      margin_error_assembler(
        pre_blame,
        blame,
        "ERROR LARGE INDENTATION: " <> message,
      )
      |> io.println

    TentativeErrorIndentationNotMultipleOfFour(blame, message) ->
      margin_error_assembler(
        pre_blame,
        blame,
        "ERROR INDENTATION (!MULT 4): " <> message,
      )
      |> io.println

    TentativeErrorCaretExpected(blame, message) ->
      margin_error_assembler(
        pre_blame,
        blame,
        "ERROR CARET EXPECTED: " <> message,
      )
      |> io.println

    TentativeErrorTextMissing(blame) ->
      margin_error_assembler(pre_blame, blame, "TEXT MISSING ERROR")
      |> io.println

    TentativeErrorTextNoClosingQuote(blame, message) ->
      margin_error_assembler(
        pre_blame,
        blame,
        "ERROR NO CLOSING QUOTE: " <> message,
      )
      |> io.println

    TentativeErrorTextNoOpeningQuote(blame, message) ->
      margin_error_assembler(
        pre_blame,
        blame,
        "ERROR NO OPENING QUOTE: " <> message,
      )
      |> io.println

    TentativeErrorTextOutOfPlace(blame, message) ->
      margin_error_assembler(
        pre_blame,
        blame,
        "ERROR TEXT OUT OF PLACE: " <> message,
      )
      |> io.println
  }
}

fn debug_print_tentatives_internal(
  pre_blame: String,
  indentation: String,
  tentatives: List(TentativeVXML),
) {
  case tentatives {
    [] -> Nil
    [first, ..rest] -> {
      debug_print_tentative_internal(pre_blame, indentation, first)
      debug_print_tentatives_internal(pre_blame, indentation, rest)
    }
  }
}

fn debug_print_tentatives(pre_blame: String, tentatives: List(TentativeVXML)) {
  debug_print_tentatives_internal(pre_blame, "", tentatives)
}

//*******************************************
//* debug printing VXML as leptos-style XML *
//*******************************************

fn map_with_special_last(
  z: List(a),
  fn1: fn(a) -> b,
  fn2: fn(a) -> b,
) -> List(b) {
  case z {
    [] -> []
    [last] -> [fn2(last)]
    [first, ..rest] ->
      fn1(first) |> list.prepend(map_with_special_last(rest, fn1, fn2), _)
  }
}

fn map_with_special_first_last(
  z: List(a),
  fn_first: fn(a) -> b,
  fn_middle: fn(a) -> b,
  fn_last: fn(a) -> b,
  fn_first_and_last: fn(a) -> b,
) -> List(b) {
  case z {
    [] -> []
    [first, ..rest] -> {
      case rest {
        [] -> list.prepend([], fn_first_and_last(first))

        [_, ..] -> {
          fn_first(first)
          |> list.prepend(map_with_special_last(rest, fn_middle, fn_last), _)
        }
      }
    }
  }
}

fn debug_print_vxml_as_leptos_xml_internal(
  pre_blame: String,
  indentation: String,
  t: VXML,
) {
  case t {
    T(_, blamed_contents) -> {
      map_with_special_first_last(
        blamed_contents,
        fn(first) {
          {
            margin_assembler(pre_blame, first.blame, "TEXT_FIRST", indentation)
            <> "r#\""
            <> first.content
          }
          |> io.println
        },
        fn(middle) {
          {
            margin_assembler(
              pre_blame,
              middle.blame,
              "TEXT_MIDDLE",
              indentation,
            )
            <> middle.content
          }
          |> io.println
        },
        fn(last) {
          {
            margin_assembler(pre_blame, last.blame, "TEXT_LAST", indentation)
            <> last.content
            <> "\"#"
          }
          |> io.println
        },
        fn(first_and_last) {
          {
            margin_assembler(
              pre_blame,
              first_and_last.blame,
              "TEXT_FIRST_AND_LAST",
              indentation,
            )
            <> "r#\""
            <> first_and_last.content
            <> "\"#"
          }
          |> io.println
        },
      )

      Nil
    }

    V(blame, tag, blamed_attributes, children) -> {
      case list.is_empty(children) {
        False -> {
          {
            margin_assembler(pre_blame, blame, "TAG OPEN", indentation)
            <> "<"
            <> tag
          }
          |> io.print

          list.map(blamed_attributes, fn(t) {
            { " " <> t.key <> "=\"" <> t.value <> "\"" }
            |> io.print
          })

          ">"
          |> io.println

          debug_print_vxmls_as_leptos_xml_internal(
            pre_blame,
            indentation <> debug_print_spaces,
            children,
          )

          {
            margin_assembler(pre_blame, blame, "TAG CLOSE", indentation)
            <> "</"
            <> tag
            <> ">"
          }
          |> io.println
        }

        True -> {
          {
            margin_assembler(pre_blame, blame, "TAG OPEN/CLOSE", indentation)
            <> "<"
            <> tag
          }
          |> io.print

          list.map(blamed_attributes, fn(t) {
            { " " <> t.key <> "=\"" <> t.value <> "\"" }
            |> io.print
          })

          { "></" <> tag <> ">" }
          |> io.println
        }
      }
    }
  }
}

fn debug_print_vxmls_as_leptos_xml_internal(
  pre_blame: String,
  indentation: String,
  vxmls: List(VXML),
) {
  case vxmls {
    [] -> Nil
    [first, ..rest] -> {
      debug_print_vxml_as_leptos_xml_internal(pre_blame, indentation, first)
      debug_print_vxmls_as_leptos_xml_internal(pre_blame, indentation, rest)
    }
  }
}

pub fn debug_print_vxmls_as_leptos_xml(pre_blame: String, vxmls: List(VXML)) {
  debug_print_vxmls_as_leptos_xml_internal(pre_blame, "", vxmls)
}

//*********************************
//* debug printing VXML as itself *
//*********************************

fn debug_print_vxml_internal(pre_blame: String, indentation: String, t: VXML) {
  case t {
    T(blame, blamed_contents) -> {
      { margin_assembler(pre_blame, blame, "TEXT_NODE", indentation) <> "<>" }
      |> io.println

      list.map(blamed_contents, fn(blamed_content) {
        {
          margin_assembler(
            pre_blame,
            blamed_content.blame,
            "TEXT_LINE",
            indentation,
          )
          <> debug_print_spaces
          <> add_quotes(blamed_content.content)
        }
        |> io.println
      })

      Nil
    }

    V(blame, tag, blamed_attributes, children) -> {
      {
        margin_assembler(pre_blame, blame, "TAG", indentation)
        <> "<>"
        <> " "
        <> tag
      }
      |> io.println

      list.map(blamed_attributes, fn(t) {
        {
          margin_assembler(pre_blame, t.blame, "ATTRIBUTE", indentation)
          <> debug_print_spaces
          <> t.key
          <> " "
          <> t.value
        }
        |> io.println
      })

      debug_print_vxmls_internal(
        pre_blame,
        indentation <> debug_print_spaces,
        children,
      )
    }
  }
}

fn debug_print_vxmls_internal(
  pre_blame: String,
  indentation: String,
  vxmls: List(VXML),
) {
  case vxmls {
    [] -> Nil
    [first, ..rest] -> {
      debug_print_vxml_internal(pre_blame, indentation, first)
      debug_print_vxmls_internal(pre_blame, indentation, rest)
    }
  }
}

pub fn debug_print_vxmls(pre_blame: String, vxmls: List(VXML)) {
  debug_print_vxmls_internal(pre_blame, "", vxmls)
}

//******************
//* vxml -> string *
//******************

fn emit_vxml_internal(indentation: String, vxml: VXML) -> List(String) {
  case vxml {
    T(_, blamed_contents) -> {
      let node = indentation <> "<>"

      let contents =
        list.map(blamed_contents, fn(blamed_content) {
          { indentation <> "    " <> add_quotes(blamed_content.content) }
        })

      [node, ..contents]
    }

    V(_, tag, blamed_attributes, children) -> {
      let node = indentation <> "<> " <> tag

      let attributes =
        list.map(blamed_attributes, fn(t) {
          indentation <> "    " <> t.key <> " " <> t.value
        })

      let children = emit_vxmls_internal(indentation <> "    ", children)

      [node, ..list.concat([attributes, children])]
    }
  }
}

fn emit_vxmls_internal(indentation: String, vxmls: List(VXML)) -> List(String) {
  list.concat(list.map(vxmls, emit_vxml_internal(indentation, _)))
}

pub fn emit_vxmls_as_list_string(vxmls: List(VXML)) -> List(String) {
  emit_vxmls_internal("", vxmls)
}

pub fn emit_vxmls(vxmls: List(VXML)) -> String {
  string.join(emit_vxmls_as_list_string(vxmls), "\n")
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
  string_to_blamed_lines(source, shortname_for_blame)
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

  case parse_file(path, "sample", False) {
    Error(IOError(error)) -> io.println("there was an IOError: " <> ins(error))

    Error(DocumentError(error)) ->
      io.println("there was a parsing error: " <> ins(error))

    Ok(vxmls) -> {
      debug_print_vxmls("(debug_print_vxmls)", vxmls)
      io.println("")
      debug_print_vxmls_as_leptos_xml(
        "(debug_print_vxmls_as_leptos_xml)",
        vxmls,
      )
      io.println("")
    }
  }
}

pub fn main() {
  test_sample()
}
