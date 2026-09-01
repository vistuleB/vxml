# VXML

[![Package Version](https://img.shields.io/hexpm/v/vxml)](https://hex.pm/packages/vxml)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/vxml/)

This package is the reference implementation of VXML ("Vanilla XML"), a
datatype and document format representing a simplified subset of XML for
document processing and markup-language transpilation.

From XML, VXML keeps only recursive nodes, attributes, and text nodes.
Other XML features are not expressible in VXML.

VXML is intended to operate as an intermediate between different lightweight
markup formats. A parser can convert a source document into
VXML, a pipeline can transform the AST, and an emitter can serialize the result
to HTML, XML-like text, JSX, or any other target for which an emitter has been
written. VXML's simple shape forces simple encoding and decoding contracts.

VXML comes with its own indentation-based serialization format for human
inspection and for persisting documents required by test suites.

The in-program VXML datatype also conveys _blame_ from the source document or
an intervening transformation pipeline. Every node, attribute, and text line
carries a `Blame`. This provides a traceability mechanism for document
transpilation. Blames are not encoded in
VXML's default serialization but a specific emitter can choose to be blame-aware,
e.g., to provide "click to jump back to source"-type functionality.

VXML is semantics-agnostic: tags and attributes are names, not behaviors.

Add the package to a Gleam project with:

```sh
gleam add vxml
```

## Example

This code parses an XML file to VXML and serializes the result as pretty-printed
HTML with two spaces of indentation:

```gleam
import gleam/result
import gleam/string
import simplifile
import vxml
import vxml/blame
import vxml/io_lines

pub fn xml_file_to_html(path: String) -> Result(String, #(blame.Blame, String)) {
  simplifile.read(path)
  |> result.map_error(fn(e) { #(blame.no_blame, string.inspect(e)) })
  |> result.try(vxml.parse_xml(_, path))
  |> result.map(vxml.vxml_to_html_output_lines(_, 0, 2))
  |> result.map(io_lines.output_lines_to_string)
}
```

## Package Contents

This package includes:

- the `VXML` tree type with recursive element nodes and terminal text nodes
- `InputLine`/`OutputLine` datatypes that allow `Blame`-aware inspection of line
  sequences before parsing and after emitting
- `vxml_table` for pretty-printing "live" VXML documents in a
  blame-annotated table
- out-of-the-box parsers for XML-ish input and serialized VXML itself
- best-effort HTML repair helpers for making common damaged-HTML patterns
  palatable to XML-oriented parsers
- serializers for HTML-, XML-, and JSX-like output, as well as VXML itself

## Model

Low-level payloads are:

- `Blame`: a type for encoding provenance of data, detailed below
- `Line`: `Line(blame: Blame, content: String)` encodes single-line text payload
- `Attr`: `Attr(blame: Blame, key: String, val: String)` encodes an attribute key-value pair

The main type is:

```gleam
pub type VXML {
  V(blame: Blame, tag: String, attrs: List(Attr), children: List(VXML))
  T(blame: Blame, lines: List(Line))
}
```

`V` is an element node containing a tag, attributes, and child nodes. `T` is a
terminal text node containing one or more lines. Every `V`, `T`, `Attr`, and
`Line` carries one `Blame`.

As a possible mnemonic, `V` stands for "VXML", since `V` is the recursive
variant that contains `VXML` children.

## Serialized Format

VXML includes a compact text format used for persistence, tests, and debug
output.

The `<>` marker opens a node, attributes appear underneath the tag, and an
unadorned marker introduces a text node:

```vxml
<> Article
  id=intro
  <> Title
    <>
      'A dark and stormy night'
  <> Section
    <> SectionTitle
      <>
        'Darkness descends'
    <> Paragraphs
      <>
        'This is the third text node'
        'of the tree, but the first'
        'text node with >1 lines.'
      <>
        'For VXML, this is just a'
        'second text node. A "paragraph"'
        'is not one of VXML's abstractions.'
```

These rules apply to both the VXML datatype and its serialized form:

1. An element node begins with `<> ` followed by its tag. Its attributes precede
   its child nodes, with both indented two spaces relative to the element.
2. A tag must match `[A-Za-z_][A-Za-z0-9_.]*`.
3. An attribute is written as `key=value`. The key must be nonempty and must not
   contain `=`, space, tab, carriage return, or newline. The value may be empty
   but must not contain a carriage return or newline.
4. A text node begins with `<>` and contains one or more text lines, indented two
   spaces relative to the node. A text node with no lines is invalid.
5. A text line is enclosed in single quotes. Its content may be empty but must
   not contain a carriage return or newline.
6. The format has no escape syntax. Single quotes and backslashes within text
   content are literal; the first and last single quotes delimit the serialized
   line.

Blank physical lines are ignored when parsing serialized VXML. Attribute values
are trimmed at both ends by the parser. Blame is not represented in the
serialized form, and the format defines no comment syntax.

The serializer preserves leading and trailing whitespace in attribute values,
because that whitespace is valid VXML. The parser trims it. Consequently, the
serialized format does not provide a lossless round trip for attribute values
with boundary whitespace.

The VXML types are not opaque, so malformed values can be constructed directly.
Serialization rejects invalid tags, attribute keys, attribute values, and text
nodes. A serialization error includes the offending value's blame and the valid
output produced before the error.

Serialized VXML can be parsed and emitted directly:

```gleam
let assert Ok([tree]) =
  vxml.parse_string(source, "example.vxml", True)

let assert Ok(text) =
  vxml.vxml_to_string(tree)
```

## Validation

Because the VXML types are public, applications and transformation pipelines
can construct values that do not satisfy the serialized VXML rules. Validate a
complete tree with:

```gleam
case vxml.validate(tree) {
  Ok(Nil) -> // valid VXML
  Error(vxml.VXMLValidationError(blame, problem)) -> // invalid VXML
}
```

`validate` recursively checks:

- element tag names
- attribute keys and values
- text-line contents
- that every text node contains at least one line

The error identifies both the problem and the offending value's blame.
Leading or trailing whitespace in an attribute value is valid and is not
rejected.

## Ingress: Parsing XML and HTML

The default XML-like parser takes a source string and a filename-like token to
use for blame-generation:

```gleam
let path = "content/source.xml"
let short_pathname_to_use_in_blame = "source.xml"

simplifile.read(path)
|> result.map_error(fn(e) { #(blame.no_blame, string.inspect(e)) })
|> result.try(vxml.parse_xml(_, short_pathname_to_use_in_blame))
```

For iffy input that may come from a handwritten HTML source, `html_repair`
can repair a few common patterns before parsing:

```gleam
let path = "content/source.html"
let short_pathname_to_use_in_blame = "source.html"

simplifile.read(path)
|> result.map_error(fn(e) { #(blame.no_blame, string.inspect(e)) })
|> result.map(vxml.html_repair)
|> result.try(vxml.parse_xml(_, short_pathname_to_use_in_blame))
```

The `html_repair` step:

- expands common boolean attributes, such as `disabled`
- escapes ampersands that are not already HTML entities
- closes HTML void tags, such as `img`, `br`, and `meta`
- removes attributes from malformed closing tags

The individual repair helpers are public so callers can apply only the repair
steps they want. These helpers are deliberately narrow string repairs, not a
general HTML parser.

XML comments are tokenized by the lower-level streamer, but `parse_xml` does not
represent them in the returned VXML tree.

The XML parser accepts XML names that are not valid in serialized VXML. For
example, XML commonly permits names containing hyphens or namespace colons,
while the VXML tag grammar does not. Call `validate` when parsed XML will enter
a pipeline that requires serialized-VXML compliance.

Before parsing, source strings are converted to `List(InputLine)`. That
conversion can be performed directly with `io_lines.string_to_input_lines`, and
the result can be inspected with
[`io_lines.input_lines_table`](#blame-tables). For even lower-level inspection
one can use `xml_streamer.input_lines_streamer`, which turns those input lines
into XML token events rather than VXML.

## XML Output

The XML serializer emits the element-and-text subset represented by VXML:

```gleam
let lines = vxml.vxml_to_xml_output_lines(tree, 0, 2)
let source = vxml.vxml_to_xml(tree, 0, 2)
```

Element-only content is indented. Mixed content remains compact so formatting
does not introduce text whitespace. Consecutive `Line` values are separated by
newlines; adjacent text nodes receive no additional separator. Empty elements
use `<tag/>` syntax, and text and attribute values are XML-escaped.

The serializer does not add an XML declaration or doctype. VXML has no variants
for declarations, doctypes, comments, processing instructions, or CDATA.

## HTML and JSX Output

Use the HTML helpers when a VXML tree directly represents HTML elements. The
output stays line-based until it is converted to a string or written to disk:

```gleam
let lines = vxml.vxml_to_html_output_lines(tree, 0, 2)
```

The HTML serializer escapes non-entity ampersands in text. It treats common
inline tags as sticky when laying out output, so inline content is not forced
onto separate lines unless the tree requires it.

JSX-like output is available through:

```gleam
let lines = vxml.vxml_to_jsx_output_lines(tree, 0, 2)
let source = vxml.vxml_to_jsx(tree, 0, 2)
```

The VXML text serializer validates its input and returns a
`VXMLSerializationError`. The XML, HTML, and JSX serializers instead assume
that the supplied tree is suitable for their target format. Call `validate`
first when serialized-VXML compliance is required; target formats may impose
additional rules of their own.

## Blame

Every node, attribute, and line carries a `Blame` value. Blame records where a
piece of data came from, or which later transformation introduced it.

```gleam
pub type Blame {
  Src(comments, path, line_no, char_no, cursor)
  Des(comments, name, line_no) // maintained desugarer code
  Ext(comments, name)          // external/manual code attribution
  NoBlame(comments)
}
```

`SourceCursor` controls whether source positions can move when text is sliced:

- `Movable` source positions advance with text manipulation.
- `Anchored` source positions stay fixed.

This is useful for parser and transformation pipelines that need diagnostics or
source maps after several tree rewrites.

`Des` and `Ext` can be used for code-attributed blame, respectively from inside
a transformation pipeline and from outside it, such as an emitter step.

## Blame Tables

Use `vxml_table` to inspect serialized VXML together with its attached blames.
For direct control over the emitted lines, use `vxml_to_output_lines` together
with `io_lines.output_lines_table_with`. This allows the blame margin columns
to be sized explicitly:

```gleam
let assert Ok([tree]) =
  vxml.parse_string(source, "example.vxml", True)
let assert Ok(lines) = vxml.vxml_to_output_lines(tree)

lines
|> io_lines.output_lines_table_with(
  "",
  0,
  blame.BlameTableMarginColumnsMinMax(30, 30),
  blame.BlameTableMarginColumnsMinMax(0, 0),
)
|> io.println
```

The first `BlameTableMarginColumnsMinMax` controls the blame digest columns.
The second controls the blame comments columns. Passing `(0, 0)` for the
comments columns suppresses them entirely. For the serialized VXML example above,
this prints:

```gleam
┌────────────────────────────────────────────────────────────────────
│ Blame                       █doc
├────────────────────────────────────────────────────────────────────
│ example.vxml:1:1 ->         █<> Article
│ example.vxml:2:3            █  id=intro
│ example.vxml:3:3 ->         █  <> Title
│ example.vxml:4:5            █    <>
│ example.vxml:5:7            █      'A dark and stormy night'
│ example.vxml:6:3 ->         █  <> Section
│ example.vxml:7:5 ->         █    <> SectionTitle
│ example.vxml:8:7            █      <>
│ example.vxml:9:9            █        'Darkness descends'
│ example.vxml:10:5 ->        █    <> Paragraphs
│ example.vxml:11:7           █      <>
│ example.vxml:12:9           █        'This is the third text node'
│ example.vxml:13:9           █        'of the tree, but the first'
│ example.vxml:14:9           █        'text node with >1 lines.'
│ example.vxml:15:7           █      <>
│ example.vxml:16:9           █        'For VXML, this is just a'
│ example.vxml:17:9           █        'second text node. A "paragraph"'
│ example.vxml:18:9           █        'is not one of VXML's abstractions.'
└────────────────────────────────────────────────────────────────────
```

## Import Guide

- `vxml`: core tree types, validation, serialized VXML parsing,
  HTML/XML/JSX-like serialization, XML-like parsing, and HTML repair helpers
- `vxml/blame`: provenance data and formatting utilities
- `vxml/io_lines`: input/output line types and conversion helpers
- `xml_streamer`: advanced XML token stream helpers

Most users should start with `vxml`, `vxml/blame`, and `vxml/io_lines`. Use
`xml_streamer` when token-level XML processing is needed.

## Tests

Run the package tests from this directory:

```sh
gleam test
```
