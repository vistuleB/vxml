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
  VXMLParseErrorTextOutOfPlace(Blame, String)
  VXMLParseErrorCaretExpected(Blame, String)
  VXMLParseErrorConsecutiveTextNodes(Blame)
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
) -> FileHead {
  case current_line(head) {
    None -> head

    Some(BlamedLine(_, suffix_indent, _)) ->
      case suffix_indent < indent {
        True -> head

        False ->
          fast_forward_past_lines_of_indent_at_least(indent, move_forward(head))
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
          case suffix_indent != indent {
            True -> #([], head)

            False ->
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
                    list.prepend(more_attribute_pairs, tentative_attribute_pair),
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
                    list.prepend(more_double_quoteds, double_quoted),
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

                  let #(siblings, head_after_indent) =
                    tentative_parse_at_indent(indent, move_forward(head))

                  #(list.prepend(siblings, error), head_after_indent)
                }

                False -> #([], head)
              }
            }

            False ->
              case suffix_indent > indent {
                True -> {
                  let head_after_oversize_indent =
                    fast_forward_past_lines_of_indent_at_least(
                      suffix_indent,
                      head,
                    )

                  let #(siblings, head_after_indent) =
                    tentative_parse_at_indent(
                      indent,
                      head_after_oversize_indent,
                    )

                  case suffix_indent % 4 == 0 {
                    True -> {
                      let error_message =
                        "indent too large "
                        <> ins(suffix_indent)
                        <> " > "
                        <> ins(indent)

                      let error =
                        TentativeErrorIndentationTooLarge(blame, error_message)

                      #(list.prepend(siblings, error), head_after_indent)
                    }

                    False -> #(
                      list.prepend(
                        siblings,
                        TentativeErrorIndentationNotMultipleOfFour(
                          blame,
                          suffix,
                        ),
                      ),
                      head_after_indent,
                    )
                  }
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

                      let #(siblings, remaining_after_indent) =
                        tentative_parse_at_indent(
                          indent,
                          remaining_after_children,
                        )

                      #(
                        list.prepend(siblings, tentative_tag),
                        remaining_after_indent,
                      )
                    }

                    EmptyCaret -> {
                      let #(double_quotes, remaining_after_double_quotes) =
                        fast_forward_past_double_quoted_lines_at_indent(
                          indent + 4,
                          move_forward(head),
                        )

                      case list.is_empty(double_quotes) {
                        True -> {
                          let error = TentativeErrorTextMissing(blame)

                          let #(siblings, remaining_after_indent) =
                            tentative_parse_at_indent(
                              indent,
                              remaining_after_double_quotes,
                            )

                          #(
                            list.prepend(siblings, error),
                            remaining_after_indent,
                          )
                        }

                        False -> {
                          let tentative_text =
                            TentativeT(blame: blame, contents: double_quotes)

                          let #(siblings, remaining_after_indent) =
                            tentative_parse_at_indent(
                              indent,
                              remaining_after_double_quotes,
                            )

                          #(
                            list.prepend(siblings, tentative_text),
                            remaining_after_indent,
                          )
                        }
                      }
                    }

                    DoubleQuoted(text) -> {
                      let error = TentativeErrorTextOutOfPlace(blame, text)

                      let #(siblings, remaining_after_indent) =
                        tentative_parse_at_indent(indent, move_forward(head))

                      #(list.prepend(siblings, error), remaining_after_indent)
                    }

                    Other(something) -> {
                      let error = TentativeErrorCaretExpected(blame, something)

                      let #(siblings, remaining_after_indent) =
                        tentative_parse_at_indent(indent, move_forward(head))

                      #(list.prepend(siblings, error), remaining_after_indent)
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
            Ok(blamed_attributes) ->
              Ok(list.prepend(blamed_attributes, blamed_attribute))

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
            Ok(parseds) -> Ok(list.prepend(parseds, parsed))

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

fn string_to_blamed_lines(
  extra_indent: Int,
  source: String,
  filename: String,
  starting_line_number: Int,
) -> List(BlamedLine) {
  string.split(source, "\n")
  |> list.map(line_to_indent_suffix_pair(_, extra_indent))
  |> add_blames(#(starting_line_number, filename))
}

//****************************************
//* tentative parsing api (blamed lines) *
//****************************************

fn tentative_parse_blamed_lines(head: FileHead) -> List(TentativeVXML) {
  let #(parsed, final_head) = tentative_parse_at_indent(0, head)
  let assert True = list.is_empty(final_head)
  parsed
}

fn tentative_parse_blamed_lines_with_debug_print(
  head: FileHead,
) -> List(TentativeVXML) {
  let #(parsed, final_head) = tentative_parse_at_indent(0, head)
  let assert True = list.is_empty(final_head)

  debug_print_tentatives("(debug_print_tentatives)", parsed)
  io.println("")

  parsed
}

//**********************************
//* pub parsing api (blamed lines) *
//**********************************

pub fn parse_blamed_lines(lines) -> Result(List(VXML), VXMLParseError) {
  lines
  |> tentative_parse_blamed_lines
  |> parse_from_tentatives
}

pub fn parse_blamed_lines_with_debug_print(
  lines,
) -> Result(List(VXML), VXMLParseError) {
  lines
  |> tentative_parse_blamed_lines_with_debug_print
  |> parse_from_tentatives
}

//****************************
//* pub parsing api (string) *
//****************************

pub fn parse_string(
  source: String,
  filename: String,
) -> Result(List(VXML), VXMLParseError) {
  string_to_blamed_lines(0, source, filename, 1)
  |> parse_blamed_lines
}

pub fn parse_string_with_debug_print(
  source: String,
  filename: String,
) -> Result(List(VXML), VXMLParseError) {
  string_to_blamed_lines(0, source, filename, 1)
  |> parse_blamed_lines_with_debug_print
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
      { margin_assembler(pre_blame, blame, "TEXT_CARET", indentation) <> "<>" }
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

    TentativeV(blame, tag, tentative_blamed_attributes, children) -> {
      io.println(
        margin_assembler(pre_blame, blame, "TAG", indentation)
        <> "<>"
        <> " "
        <> ins(tag),
      )

      list.map(tentative_blamed_attributes, fn(t) {
        {
          margin_assembler(pre_blame, t.blame, "ATTRIBUTE", indentation)
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
        "INDENTATION ERROR (LARGE): " <> message,
      )
      |> io.println

    TentativeErrorIndentationNotMultipleOfFour(blame, message) ->
      margin_error_assembler(
        pre_blame,
        blame,
        "INDENTATION ERROR (!MULT 4): " <> message,
      )
      |> io.println

    TentativeErrorCaretExpected(blame, message) ->
      margin_error_assembler(
        pre_blame,
        blame,
        "CARET EXPECTED ERROR: " <> message,
      )
      |> io.println

    TentativeErrorTextMissing(blame) ->
      margin_error_assembler(pre_blame, blame, "TEXT MISSING ERROR")
      |> io.println

    TentativeErrorTextOutOfPlace(blame, message) ->
      margin_error_assembler(
        pre_blame,
        blame,
        "TEXT WRONG PLACE ERROR: " <> message,
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

//********
//* main *
//********

fn test_sample() {
  let filename = "test/sample.vxml"

  case simplifile.read(filename) {
    Error(e) -> io.println("Error reading " <> filename <> ": " <> ins(e))

    Ok(file) -> {
      case parse_string_with_debug_print(file, filename) {
        Ok(vxmls) -> {
          debug_print_vxmls("(debug_print_vxmls)", vxmls)
          io.println("")

          debug_print_vxmls_as_leptos_xml(
            "(debug_print_vxmls_as_leptos_xml)",
            vxmls,
          )
          io.println("")
        }

        Error(error) -> {
          io.println("\nthere was a parsing error:")
          io.println(ins(error))
        }
      }

      Nil
    }
  }
}

pub fn main() {
  test_sample()
}
