//// XML `Name` grammar support shared by parsing and VXML validation.

import gleam/list
import gleam/string

pub const grammar = "XML Name"

fn codepoint_is_between(codepoint: Int, first: Int, last: Int) -> Bool {
  codepoint >= first && codepoint <= last
}

fn is_name_start_char(codepoint: Int) -> Bool {
  codepoint == 0x3A
  || codepoint_is_between(codepoint, 0x41, 0x5A)
  || codepoint == 0x5F
  || codepoint_is_between(codepoint, 0x61, 0x7A)
  || codepoint_is_between(codepoint, 0xC0, 0xD6)
  || codepoint_is_between(codepoint, 0xD8, 0xF6)
  || codepoint_is_between(codepoint, 0xF8, 0x2FF)
  || codepoint_is_between(codepoint, 0x370, 0x37D)
  || codepoint_is_between(codepoint, 0x37F, 0x1FFF)
  || codepoint_is_between(codepoint, 0x200C, 0x200D)
  || codepoint_is_between(codepoint, 0x2070, 0x218F)
  || codepoint_is_between(codepoint, 0x2C00, 0x2FEF)
  || codepoint_is_between(codepoint, 0x3001, 0xD7FF)
  || codepoint_is_between(codepoint, 0xF900, 0xFDCF)
  || codepoint_is_between(codepoint, 0xFDF0, 0xFFFD)
  || codepoint_is_between(codepoint, 0x10000, 0xEFFFF)
}

fn is_name_char(codepoint: Int) -> Bool {
  is_name_start_char(codepoint)
  || codepoint == 0x2D
  || codepoint == 0x2E
  || codepoint_is_between(codepoint, 0x30, 0x39)
  || codepoint == 0xB7
  || codepoint_is_between(codepoint, 0x300, 0x36F)
  || codepoint_is_between(codepoint, 0x203F, 0x2040)
}

/// Returns whether a string satisfies the XML `Name` grammar.
pub fn is_name(name: String) -> Bool {
  case string.to_utf_codepoints(name) {
    [] -> False
    [first, ..rest] ->
      is_name_start_char(string.utf_codepoint_to_int(first))
      && list.all(rest, fn(codepoint) {
        is_name_char(string.utf_codepoint_to_int(codepoint))
      })
  }
}
