//// Best-effort HTML string repairs for XML-oriented parsing.

import gleam/list
import gleam/option.{Some}
import gleam/regexp
import gleam/string

/// Matches ampersands that do not begin a syntactically plausible entity.
pub const non_entity_ampersand_pattern = "&(?!(?:[A-Za-z][A-Za-z0-9]*;|#[0-9]+;|#[xX][0-9A-Fa-f]+;))"

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

/// Escapes ampersands that do not begin a syntactically plausible entity.
pub fn html_repair_escape_non_entity_ampersands(content: String) -> String {
  let assert Ok(re) = regexp.from_string(non_entity_ampersand_pattern)

  regexp.replace(re, content, "&amp;")
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
