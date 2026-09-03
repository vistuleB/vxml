//// Line-based input and output helpers.
////
//// `InputLine` and `OutputLine` store leading indentation separately from the
//// remaining line suffix and attach `Blame` to both. They provide the bridge
//// between files or strings and VXML parsers or serializers.
////
//// String conversion normalizes CRLF and CR endings to LF, converts leading
//// ASCII spaces into an indentation count, preserves all other whitespace,
//// and represents a terminal newline as a final empty line.

import gleam/list
import gleam/result
import gleam/string
import simplifile.{type FileError}
import vxml/blame.{type Blame} as bl

@external(erlang, "vxml_io_lines_ffi", "split_leading_spaces")
@external(javascript, "./io_lines_ffi.mjs", "splitLeadingSpaces")
fn split_leading_spaces(source: String) -> #(Int, String)

/// A source line with its leading whitespace count, remaining suffix, and blame.
pub type InputLine {
  InputLine(blame: Blame, indent: Int, suffix: String)
}

/// An output line with its leading-space count, remaining suffix, and blame.
pub type OutputLine {
  OutputLine(blame: Blame, indent: Int, suffix: String)
}

// *************
// private utils
// *************

fn spaces(i: Int) -> String {
  string.repeat(" ", i)
}

/// Normalizes CRLF and CR line endings to LF.
pub fn normalize_line_endings(source: String) -> String {
  source
  |> string.replace("\r\n", "\n")
  |> string.replace("\r", "\n")
}

// ***************************************************
// String -> List(InputLine) & path -> List(InputLine)
// ***************************************************

/// Converts a string to input lines, preserving its path and trailing content.
///
/// Leading ASCII spaces are removed from each suffix and counted in `indent`.
/// Tabs and other whitespace remain in the suffix. `added_indentation` is added
/// to the space count. A terminal newline produces a final empty `InputLine`.
pub fn string_to_input_lines(
  source: String,
  path: String,
  added_indentation: Int,
) -> List(InputLine) {
  source
  |> normalize_line_endings
  |> string.split("\n")
  |> list.index_map(fn(s, i) {
    let #(indent, suffix) = split_leading_spaces(s)
    InputLine(
      blame: bl.Src(
        comments: [],
        path: path,
        line_no: i + 1,
        char_no: indent + 1,
        // ...to match VSCode numbering
        cursor: bl.Movable,
      ),
      indent: indent + added_indentation,
      suffix: suffix,
    )
  })
}

/// Reads a file path into input lines.
pub fn path_to_input_lines(
  path: String,
  added_indentation: Int,
) -> Result(List(InputLine), FileError) {
  simplifile.read(path)
  |> result.map(string_to_input_lines(_, path, added_indentation))
}

/// Alias for `path_to_input_lines`.
pub fn read(
  path: String,
  added_indentation: Int,
) -> Result(List(InputLine), FileError) {
  path_to_input_lines(path, added_indentation)
}

// **************************************************
// List(InputLine) -> List(OutputLine)
// **************************************************

/// Restores one input line's indentation and suffix.
pub fn input_line_to_string(line: InputLine) -> String {
  spaces(line.indent) <> line.suffix
}

/// Converts input lines to one newline-separated string.
pub fn input_lines_to_string(lines: List(InputLine)) -> String {
  lines
  |> list.map(input_line_to_string)
  |> string.join("\n")
}

/// Converts input lines to equivalent output lines, preserving blame.
pub fn input_lines_to_output_lines(lines: List(InputLine)) -> List(OutputLine) {
  lines
  |> list.map(fn(l) { OutputLine(l.blame, l.indent, l.suffix) })
}

// **************************************************
// OutputLine -> String & List(OutputLine) -> String
// **************************************************

/// Restores one output line's indentation and suffix.
pub fn output_line_to_string(line: OutputLine) -> String {
  spaces(line.indent) <> line.suffix
}

/// Converts output lines to a newline-separated string.
pub fn output_lines_to_string(lines: List(OutputLine)) -> String {
  lines
  |> list.map(output_line_to_string)
  |> string.join("\n")
}

// **************************************************
// List(InputLine) -> String table pretty-printer &
// List(OutputLine) -> String table pretty-printer
// **************************************************

const default_blame_digest_margin = bl.BlameTableMarginColumnsMinMax(48, 48)

const default_comments_margin = bl.BlameTableMarginColumnsMinMax(30, 30)

/// Renders input lines as a blame-annotated table using default margins.
pub fn input_lines_table(
  content: List(InputLine),
  banner: String,
  indent: Int,
) -> String {
  let margin = spaces(indent)
  content
  |> list.map(fn(c) { #(c.blame, spaces(c.indent) <> c.suffix) })
  |> bl.blamed_strings_annotated_table(
    banner,
    default_blame_digest_margin,
    default_comments_margin,
  )
  |> list.map(fn(s) { margin <> s })
  |> string.join("\n")
}

/// Renders output lines as table lines using explicit margin constraints.
pub fn output_lines_table_lines_with(
  content: List(OutputLine),
  banner: String,
  indent: Int,
  blame_digest_margin: bl.BlameTableMarginColumnsMinMax,
  comments_margin: bl.BlameTableMarginColumnsMinMax,
) -> List(String) {
  let margin = spaces(indent)
  content
  |> list.map(fn(c) { #(c.blame, spaces(c.indent) <> c.suffix) })
  |> bl.blamed_strings_annotated_table(
    banner,
    blame_digest_margin,
    comments_margin,
  )
  |> list.map(fn(s) { margin <> s })
}

/// Renders output lines as table lines using default margin constraints.
pub fn output_lines_table_lines(
  content: List(OutputLine),
  banner: String,
  indent: Int,
) -> List(String) {
  output_lines_table_lines_with(
    content,
    banner,
    indent,
    default_blame_digest_margin,
    default_comments_margin,
  )
}

/// Renders output lines as a table string using explicit margin constraints.
pub fn output_lines_table_with(
  content: List(OutputLine),
  banner: String,
  indent: Int,
  blame_digest_margin: bl.BlameTableMarginColumnsMinMax,
  comments_margin: bl.BlameTableMarginColumnsMinMax,
) -> String {
  output_lines_table_lines_with(
    content,
    banner,
    indent,
    blame_digest_margin,
    comments_margin,
  )
  |> string.join("\n")
}

/// Renders output lines as a table string using default margin constraints.
pub fn output_lines_table(
  content: List(OutputLine),
  banner: String,
  indent: Int,
) -> String {
  output_lines_table_lines(content, banner, indent)
  |> string.join("\n")
}
