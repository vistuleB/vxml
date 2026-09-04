// Best-effort HTML string repairs for XML-oriented parsing.
//
// These helpers perform deliberately narrow string rewrites. They are not
// HTML parsers and are not generally quote-aware or token-aware. Use them to
// prepare controlled, HTML-like input for an XML-oriented parser, not to
// normalize arbitrary HTML.

import gleam/list
import gleam/option.{Some}
import gleam/regexp
import gleam/string
import glentities/decoder as html_entities

// Best-effort string repair. This is not quote-aware or token-aware.
fn html_repair_close_void_tag(content: String, tag: String) -> String {
  let assert Ok(re) = regexp.from_string("(<" <> tag <> ")(\\b[^>]*)(>)")

  regexp.match_map(re, content, fn(match) {
    let regexp.Match(_, sub) = match
    let assert [_, maybe_middle, _] = sub
    let middle = maybe_middle |> option.unwrap("")
    case middle |> string.trim_end |> string.ends_with("/") {
      True -> "<" <> tag <> middle <> ">"
      False -> "<" <> tag <> middle <> "/>"
    }
  })
}

fn is_known_named_entity(name: String) -> Bool {
  html_entities.decode_named(name) != "&" <> name <> ";"
}

fn is_valid_numeric_entity(name: String) -> Bool {
  case name {
    "#x" <> rest | "#X" <> rest ->
      case html_entities.decode_hex(rest) == rest {
        True -> False
        False -> True
      }
    "#" <> rest ->
      case html_entities.decode_dec(rest) == rest {
        True -> False
        False -> True
      }
    _ -> False
  }
}

fn is_entity_name_character(character: String) -> Bool {
  let assert Ok(re) = regexp.from_string("^[A-Za-z0-9#xX]$")
  regexp.check(re, character)
}

fn take_entity_candidate(
  rest: List(String),
  previous: List(String),
) -> #(String, List(String), Bool) {
  case rest {
    [] -> #(previous |> list.reverse |> string.concat, [], False)
    [";", ..rest] -> #(previous |> list.reverse |> string.concat, rest, True)
    [first, ..rest] ->
      case is_entity_name_character(first) {
        True -> take_entity_candidate(rest, [first, ..previous])
        False -> #(
          previous |> list.reverse |> string.concat,
          [first, ..rest],
          False,
        )
      }
  }
}

fn html_repair_escape_non_entity_ampersands_loop(
  graphemes: List(String),
  previous: List(String),
) -> String {
  case graphemes {
    [] -> previous |> list.reverse |> string.concat
    ["&", ..rest] -> {
      let #(candidate, rest, closed) = take_entity_candidate(rest, [])
      case
        closed
        && {
          is_known_named_entity(candidate) || is_valid_numeric_entity(candidate)
        }
      {
        True ->
          html_repair_escape_non_entity_ampersands_loop(rest, [
            "&" <> candidate <> ";",
            ..previous
          ])
        False -> {
          let candidate = case closed {
            True -> candidate <> ";"
            False -> candidate
          }
          html_repair_escape_non_entity_ampersands_loop(rest, [
            "&amp;" <> candidate,
            ..previous
          ])
        }
      }
    }
    [first, ..rest] ->
      html_repair_escape_non_entity_ampersands_loop(rest, [first, ..previous])
  }
}

/// Escapes ampersands that do not begin a known HTML entity.
pub fn html_repair_escape_non_entity_ampersands(content: String) -> String {
  content
  |> string.to_graphemes
  |> html_repair_escape_non_entity_ampersands_loop([])
}

fn html_repair_expand_boolean_attr(content: String, attr: String) -> String {
  let assert Ok(re) = regexp.from_string("(\\s" <> attr <> ")(\\s|>|/>)")

  regexp.match_map(re, content, fn(match) {
    let regexp.Match(_, sub) = match
    let assert [Some(attr), Some(after)] = sub
    attr <> "=\"\"" <> after
  })
}

/// Gives common bare HTML boolean attributes empty assigned values.
pub fn html_repair_expand_boolean_attrs(content: String) -> String {
  [
    "allowfullscreen", "async", "autofocus", "autoplay", "checked", "controls",
    "default", "defer", "disabled", "formnovalidate", "hidden", "inert", "ismap",
    "loop", "multiple", "muted", "nomodule", "novalidate", "open", "playsinline",
    "readonly", "required", "reversed", "selected",
  ]
  |> list.fold(content, fn(content, attr) {
    html_repair_expand_boolean_attr(content, attr)
  })
}

/// Converts common HTML void-element openings to self-closing XML syntax.
pub fn html_repair_close_void_tags(content: String) -> String {
  [
    "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta",
    "source", "track", "wbr",
  ]
  |> list.fold(content, fn(content, tag) {
    html_repair_close_void_tag(content, tag)
  })
}

/// Removes attributes from malformed closing tags.
pub fn html_repair_remove_attrs_from_closing_tags(content: String) -> String {
  let assert Ok(re) =
    regexp.from_string("(<\\/)([a-zA-Z][a-zA-Z0-9._-]*)(\\s+[^>]*)(>)")

  regexp.match_map(re, content, fn(match) {
    let regexp.Match(_, sub) = match
    let assert [_, Some(tag), _, _] = sub
    "</" <> tag <> ">"
  })
}

/// Best-effort repair for common HTML syntax that blocks XML-oriented parsers.
pub fn html_repair(content: String) -> String {
  content
  |> html_repair_expand_boolean_attrs
  |> html_repair_escape_non_entity_ampersands
  |> html_repair_close_void_tags
  |> html_repair_remove_attrs_from_closing_tags
}
