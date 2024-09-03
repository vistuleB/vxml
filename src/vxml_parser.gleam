import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import simplifile

// ****************
// * public types *
// ****************

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
  VXMLParseErrorIndentationNotMultipleOfFour(Blame)
  VXMLParseErrorTextMissing(Blame)
  VXMLParseErrorTextOutOfPlace(Blame, String)
  VXMLParseErrorCaretExpected(Blame, String)
  VXMLParseErrorConsecutiveTextNodes(Blame)
}

// ***************
// * local types *
// ***************

type FileHead {
  FileHead(filename: String, line_no: Int, lines: List(String))
}

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
  TentativeErrorIndentationNotMultipleOfFour(blame: Blame)
  TentativeErrorTextMissing(blame: Blame)
  TentativeErrorTextOutOfPlace(blame: Blame, message: String)
  TentativeErrorCaretExpected(blame: Blame, message: String)
}

// *************
// * constants *
// *************

const ins = string.inspect

// ************
// * FileHead *
// ************

fn get_first_line(head: FileHead) -> Option(String) {
  case head.lines {
    [] -> None
    [first, ..] -> Some(first)
  }
}

fn get_blame(head: FileHead) -> Blame {
  Blame(filename: head.filename, line_no: head.line_no, comments: [])
}

fn get_next_file_head(head: FileHead) -> FileHead {
  let assert [_, ..rest] = head.lines
  FileHead(filename: head.filename, line_no: head.line_no + 1, lines: rest)
}

// ************************
// * parse_from_tentative *
// ************************

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
            Ok(parseds) -> {
              case parsed {
                T(..) ->
                  case parseds {
                    [] -> Ok(list.prepend(parseds, parsed))

                    [next, ..] ->
                      case next {
                        T(..) ->
                          Error(VXMLParseErrorConsecutiveTextNodes(next.blame))

                        _ -> Ok(list.prepend(parseds, parsed))
                      }
                  }

                _ -> Ok(list.prepend(parseds, parsed))
              }
            }

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

    TentativeErrorIndentationNotMultipleOfFour(blame) ->
      Error(VXMLParseErrorIndentationNotMultipleOfFour(blame))

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

// *******************
// * parse_tentative *
// *******************

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

fn line_indent(line: String) -> Int {
  string.length(line) - string.length(string.trim_left(line))
}

fn fast_forward_past_lines_of_indent_at_least(
  indent: Int,
  head: FileHead,
) -> FileHead {
  case get_first_line(head) {
    None -> head

    Some(first) ->
      case line_indent(first) < indent {
        True -> head

        False ->
          fast_forward_past_lines_of_indent_at_least(
            indent,
            get_next_file_head(head),
          )
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
  let bad_character = contains_one_of(key, [".", ";", "\""])

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
  case get_first_line(head) {
    None -> #([], head)
    Some(first) -> {
      let suffix = string.trim_left(first)
      case suffix == "" {
        True -> #([], get_next_file_head(head))
        False -> {
          let first_indent = string.length(first) - string.length(suffix)
          case first_indent != indent {
            True -> #([], head)
            False ->
              case string.starts_with(suffix, "<>") {
                True -> #([], head)
                False -> {
                  let tentative_attribute_pair =
                    suffix
                    |> string.split_once(" ")
                    |> result.unwrap(#(suffix, ""))
                    |> tentative_blamed_attribute(get_blame(head), _)

                  let #(more_attribute_pairs, remaining_after_attributes) =
                    fast_forward_past_attribute_lines_at_indent(
                      indent,
                      get_next_file_head(head),
                    )

                  #(
                    list.prepend(more_attribute_pairs, tentative_attribute_pair),
                    remaining_after_attributes,
                  )
                }
              }
          }
        }
      }
    }
  }
}

fn fast_forward_past_double_quoted_lines_at_indent(
  indent: Int,
  head: FileHead,
) -> #(List(BlamedContent), FileHead) {
  case get_first_line(head) {
    None -> #([], head)
    Some(first) -> {
      let suffix = string.trim_left(first)
      case suffix == "" {
        True -> #([], head)
        False -> {
          let first_indent = string.length(first) - string.length(suffix)
          case first_indent != indent {
            True -> #([], head)
            False ->
              case is_double_quoted_thing(suffix) {
                False -> #([], head)
                True -> {
                  let #(more_others, remaining_after_others) =
                    fast_forward_past_double_quoted_lines_at_indent(
                      indent,
                      get_next_file_head(head),
                    )

                  #(
                    list.prepend(
                      more_others,
                      BlamedContent(get_blame(head), string.trim(suffix)),
                    ),
                    remaining_after_others,
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
      let something_illegal = contains_one_of(proposed_name, ["-", ".", " "])
      case string.is_empty(something_illegal) {
        True -> Ok(proposed_name)
        False -> Error(IllegalTagCharacter(proposed_name, something_illegal))
      }
    }
  }
}

fn tentative_parse(
  indent: Int,
  head: FileHead,
) -> #(List(TentativeVXML), FileHead) {
  case get_first_line(head) {
    None -> #([], head)

    Some(first) -> {
      let suffix = string.trim_left(first)

      case suffix == "" {
        True -> tentative_parse(indent, get_next_file_head(head))

        False -> {
          let line_indent = string.length(first) - string.length(suffix)

          case line_indent < indent {
            True -> #([], head)

            False ->
              case line_indent > indent {
                True -> {
                  let remaining_after_oversize_indent =
                    fast_forward_past_lines_of_indent_at_least(
                      line_indent,
                      head,
                    )

                  let #(siblings, remaining_after_indent) =
                    tentative_parse(indent, remaining_after_oversize_indent)

                  case line_indent % 4 == 0 {
                    True -> {
                      let error_message =
                        "indent too large "
                        <> ins(line_indent)
                        <> " > "
                        <> ins(indent)

                      #(
                        list.prepend(
                          siblings,
                          TentativeErrorIndentationTooLarge(
                            get_blame(head),
                            error_message,
                          ),
                        ),
                        remaining_after_indent,
                      )
                    }

                    False -> #(
                      list.prepend(
                        siblings,
                        TentativeErrorIndentationNotMultipleOfFour(get_blame(
                          head,
                        )),
                      ),
                      remaining_after_indent,
                    )
                  }
                }

                False -> {
                  let assert True = line_indent == indent

                  case nonempty_suffix_diagnostic(suffix) {
                    TaggedCaret(annotation) -> {
                      let #(tentative_attributes, remaining_after_attributes) =
                        fast_forward_past_attribute_lines_at_indent(
                          indent + 4,
                          get_next_file_head(head),
                        )

                      let #(children, remaining_after_children) =
                        tentative_parse(indent + 4, remaining_after_attributes)

                      let tentative_tag =
                        TentativeV(
                          blame: get_blame(head),
                          tag: check_good_tag_name(string.trim(annotation)),
                          attributes: tentative_attributes,
                          children: children,
                        )

                      let #(siblings, remaining_after_indent) =
                        tentative_parse(indent, remaining_after_children)

                      #(
                        list.prepend(siblings, tentative_tag),
                        remaining_after_indent,
                      )
                    }

                    EmptyCaret -> {
                      let #(double_quotes, remaining_after_double_quotes) =
                        fast_forward_past_double_quoted_lines_at_indent(
                          indent + 4,
                          get_next_file_head(head),
                        )

                      case list.is_empty(double_quotes) {
                        True -> {
                          let error = TentativeErrorTextMissing(get_blame(head))

                          let #(siblings, remaining_after_indent) =
                            tentative_parse(
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
                            TentativeT(
                              blame: get_blame(head),
                              contents: double_quotes,
                            )

                          let #(siblings, remaining_after_indent) =
                            tentative_parse(
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
                      let error =
                        TentativeErrorTextOutOfPlace(get_blame(head), text)

                      let #(siblings, remaining_after_indent) =
                        tentative_parse(indent, get_next_file_head(head))

                      #(list.prepend(siblings, error), remaining_after_indent)
                    }

                    Other(something) -> {
                      let error =
                        TentativeErrorCaretExpected(get_blame(head), something)

                      let #(siblings, remaining_after_indent) =
                        tentative_parse(indent, get_next_file_head(head))

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

fn tentative_parse_at_indent_0(head: FileHead) -> List(TentativeVXML) {
  let #(parsed, remaining_beyond_parsed) = tentative_parse(0, head)
  let assert True = list.is_empty(remaining_beyond_parsed.lines)
  parsed
}

const margin_line_number_pad_to = 6

const margin_announce_pad_to = 30

fn margin_assembler(
  prefix: String,
  blame: Blame,
  announce: String,
  margin: String,
) -> String {
  prefix
  <> blame.filename
  <> ":"
  <> string.pad_right(ins(blame.line_no), margin_line_number_pad_to, " ")
  <> " "
  <> string.pad_right(announce, margin_announce_pad_to, " ")
  <> "."
  <> margin
}

fn margin_error_assembler(
  prefix: String,
  blame: Blame,
  error_message: String,
) -> String {
  prefix
  <> blame.filename
  <> ":"
  <> string.pad_right(ins(blame.line_no), margin_line_number_pad_to, " ")
  <> " "
  <> error_message
}

fn pretty_print_tentative(
  margin_prefix: String,
  margin: String,
  t: TentativeVXML,
) {
  let p = margin_prefix
  let m = margin

  case t {
    TentativeT(blame, blamed_contents) -> {
      io.println(margin_assembler(p, blame, "TEXT_CARET", m) <> "<>")
      list.map(blamed_contents, fn(blamed_content) {
        io.println(
          margin_assembler(p, blamed_content.blame, "TEXT_LINE", m)
          <> blamed_content.content,
        )
      })
      Nil
    }

    TentativeV(blame, tag, tentative_blamed_attributes, children) -> {
      io.println(
        margin_assembler(p, blame, "TAG", m) <> "<>" <> " " <> ins(tag),
      )

      list.map(tentative_blamed_attributes, fn(t) -> Nil {
        io.println(
          margin_assembler(p, t.blame, "ATTRIBUTE", m <> "  ")
          <> ins(t.key)
          <> " "
          <> t.value,
        )
      })

      pretty_print_tentatives(p, m <> "  ", children)
    }

    TentativeErrorIndentationTooLarge(blame, message) ->
      io.println(margin_error_assembler(
        p,
        blame,
        "INDENTATION ERROR (LARGE): " <> message,
      ))

    TentativeErrorIndentationNotMultipleOfFour(blame) ->
      io.println(margin_error_assembler(
        p,
        blame,
        "INDENTATION ERROR (!MULT 4): ",
      ))

    TentativeErrorCaretExpected(blame, message) ->
      io.println(margin_error_assembler(
        p,
        blame,
        "CARET EXPECTED ERROR: " <> message,
      ))

    TentativeErrorTextMissing(blame) ->
      io.println(margin_error_assembler(p, blame, "TEXT MISSING ERROR"))

    TentativeErrorTextOutOfPlace(blame, message) ->
      io.println(margin_error_assembler(
        p,
        blame,
        "TEXT WRONG PLACE ERROR: " <> message,
      ))
  }
}

fn pretty_print_tentatives(
  margin_prefix: String,
  margin: String,
  tentatives: List(TentativeVXML),
) {
  case tentatives {
    [] -> Nil
    [first, ..rest] -> {
      pretty_print_tentative(margin_prefix, margin, first)
      pretty_print_tentatives(margin_prefix, margin, rest)
    }
  }
}

fn pretty_print_vxml(margin_prefix: String, margin: String, t: VXML) {
  let p = margin_prefix
  let m = margin

  case t {
    T(blame, blamed_contents) -> {
      io.println(margin_assembler(p, blame, "TEXT_CARET", m) <> "<>")
      list.map(blamed_contents, fn(blamed_content) {
        io.println(
          margin_assembler(p, blamed_content.blame, "TEXT_LINE", m <> "  ")
          <> blamed_content.content,
        )
      })
      Nil
    }

    V(blame, tag, blamed_attributes, children) -> {
      io.println(margin_assembler(p, blame, "TAG", m) <> "<>" <> " " <> tag)

      list.map(blamed_attributes, fn(t) -> Nil {
        io.println(
          margin_assembler(p, t.blame, "ATTRIBUTE", m <> "  ")
          <> t.key
          <> " "
          <> t.value,
        )
      })

      pretty_print_vxmls(p, m <> "  ", children)
    }
  }
}

pub fn pretty_print_vxmls(
  margin_prefix: String,
  margin: String,
  vxmls: List(VXML),
) {
  case vxmls {
    [] -> Nil
    [first, ..rest] -> {
      pretty_print_vxml(margin_prefix, margin, first)
      pretty_print_vxmls(margin_prefix, margin, rest)
    }
  }
}

fn tentative_parse_string(
  source: String,
  source_name: String,
  starting_line_number: Int,
) -> List(TentativeVXML) {
  let lines = string.split(source, "\n")
  let head = FileHead(source_name, starting_line_number, lines)
  let parsed = tentative_parse_at_indent_0(head)
  // io.println("\n\n(tentative parse:)")
  // pretty_print_tentatives("(tentative)", "", parsed)
  // io.println("(tentative end)\n\n")
  parsed
}

pub fn parse_string(
  source: String,
  source_name: String,
  starting_line_number: Int,
) -> Result(List(VXML), VXMLParseError) {
  tentative_parse_string(source, source_name, starting_line_number)
  |> parse_from_tentatives
}

pub fn main() {
  let filename = "src/sample.vxml"

  case simplifile.read(filename) {
    Error(e) -> io.println("Error reading " <> filename <> ": " <> ins(e))

    Ok(file) -> {
      case parse_string(file, filename, 1) {
        Ok(vxmls) -> pretty_print_vxmls("(writerlys)", "", vxmls)

        Error(error) -> {
          io.println("\nthere was a parsing error:")
          io.println(ins(error))
        }
      }

      Nil
    }
  }
}
