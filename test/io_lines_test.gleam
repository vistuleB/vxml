import gleam/list
import gleeunit/should
import vxml/blame
import vxml/io_lines

pub fn normalize_line_endings_test() {
  "one\r\ntwo\rthree"
  |> io_lines.normalize_line_endings
  |> should.equal("one\ntwo\nthree")
}

pub fn string_to_input_lines_normalizes_line_endings_test() {
  "one\r\n  two\rthree"
  |> io_lines.string_to_input_lines("test", 0)
  |> list.map(fn(line) { line.suffix })
  |> should.equal(["one", "two", "three"])
}

pub fn string_to_input_lines_extracts_only_ascii_spaces_test() {
  let lines =
    "  spaces\n\ttab\n \tmixed"
    |> io_lines.string_to_input_lines("test", 0)

  lines
  |> list.map(fn(line) { #(line.indent, line.suffix) })
  |> should.equal([#(2, "spaces"), #(0, "\ttab"), #(1, "\tmixed")])

  lines
  |> io_lines.input_lines_to_string
  |> should.equal("  spaces\n\ttab\n \tmixed")
}

fn margin(columns: Int) {
  blame.BlameTableMarginColumnsMinMax(columns, columns)
}

fn annotated_output_line() {
  io_lines.OutputLine(
    blame.Src(["note"], "doc.wly", 3, 5, blame.Movable),
    2,
    "hello",
  )
}

fn line_at(lines: List(String), index: Int) {
  lines |> list.drop(index) |> list.first
}

pub fn output_lines_table_suppresses_zero_comment_margin_test() {
  io_lines.output_lines_table_lines_with(
    [annotated_output_line()],
    "",
    0,
    margin(18),
    margin(0),
  )
  |> line_at(3)
  |> should.equal(Ok("│ doc.wly:3:5     █  hello"))
}

pub fn output_lines_table_suppresses_zero_blame_margin_test() {
  io_lines.output_lines_table_lines_with(
    [annotated_output_line()],
    "",
    0,
    margin(0),
    margin(10),
  )
  |> line_at(3)
  |> should.equal(Ok("[note]    █  hello"))
}

pub fn output_lines_table_suppresses_both_zero_margins_test() {
  let lines =
    io_lines.output_lines_table_lines_with(
      [annotated_output_line()],
      "",
      0,
      margin(0),
      margin(0),
    )

  lines |> line_at(1) |> should.equal(Ok("│doc"))
  lines |> line_at(3) |> should.equal(Ok("│  hello"))
}
