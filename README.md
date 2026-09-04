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

A sister package, [VXML Pipeline](https://hex.pm/packages/vxml_pipeline),
offers a suite of VXML → VXML transformation utilities.

## Example

This code parses an XML file to VXML and serializes the result as pretty-printed
HTML with two spaces of indentation:

```gleam
import gleam/result
import gleam/string
import simplifile
import vxml
import vxml/blame

pub fn xml_file_to_html(path: String) -> Result(String, vxml.XMLParseError) {
  simplifile.read(path)
  |> result.map_error(fn(e) {
    vxml.XMLParseError(blame.no_blame, string.inspect(e))
  })
  |> result.try(vxml.xml_to_vxml(_, path))
  |> result.map(vxml.vxml_to_html(_, 0, 2))
}
```

## Package Contents

This package includes:

- the `VXML` tree type with recursive element nodes and terminal text nodes
- `InputLine`/`OutputLine` datatypes that allow `Blame`-aware inspection of line
  sequences before parsing and after emitting
- `vxml_table` for pretty-printing "live" VXML documents in a
  blame-annotated table
- out-of-the-box parsers for XML, HTML, and serialized VXML
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

1. Indentation is space-based. Each nesting level uses exactly two spaces; tabs
   must not be used for indentation. An element node begins with `<> ` followed
   by its tag. Its attributes precede its child nodes, with both nested one level
   beneath the element.
2. A tag must be an XML `Name`. Names may begin with a letter, `_`, `:`, or
   another XML name-start character. Later characters may additionally include
   digits, `-`, `.`, and the other characters admitted by XML's `Name` grammar.
3. An attribute is written as `key=value`. The key must be nonempty and must not
   contain `=`, space, tab, carriage return, or newline. The value may be empty
   but must not contain a carriage return or newline. Leading spaces and tabs
   are preserved; trailing spaces and tabs are invalid.
4. A text node begins with `<>` and contains one or more text lines, indented two
   spaces relative to the node. A text node with no lines is invalid.
5. A text line is enclosed in single quotes. Its content may be empty but must
   not contain a carriage return or newline.
6. The format has no escape syntax. Single quotes and backslashes within text
   content are literal; the first and last single quotes delimit the serialized
   line.

Empty lines and lines containing only spaces are ignored when parsing
serialized VXML. Attribute-value content begins immediately after the first
`=`. Leading spaces and tabs in a value are data and round-trip unchanged.
Trailing spaces and tabs in a value are rejected by both the parser and
serializer. Blame is not represented in the serialized form, and the format
defines no comment syntax.

The VXML types are not opaque, so malformed values can be constructed directly.
Serialization rejects invalid tags, attribute keys, attribute values, and text
nodes. A serialization error includes the offending value's blame and the valid
output produced before the error.

Serialized VXML can be parsed and emitted directly:

```gleam
let assert Ok(tree) =
  vxml.string_to_vxml(source, "example.vxml")

let assert Ok(text) =
  vxml.vxml_to_string(tree)
```

The parsing functions distinguish between input that must contain exactly one
root and input that may contain any number of roots:

| Input | Exactly one root | Zero or more roots |
|---|---|---|
| `List(InputLine)` | `input_lines_to_vxml` | `input_lines_to_vxmls` |
| `String` | `string_to_vxml` | `string_to_vxmls` |
| filesystem path | `path_to_vxml` | `path_to_vxmls` |

The singular functions return `VXML` and reject empty or multiple-root input.
The plural functions return `List(VXML)`. String parsing accepts a second
argument used as the source path in generated `Blame` values.

## Validation

Because the VXML types are public, applications and transformation pipelines
can construct values that do not satisfy the serialized VXML rules. Validate a
complete tree with:

```gleam
case vxml.validate(tree) {
  Ok(Nil) -> // valid VXML
  Error(vxml.VXMLValidationError(blame, reason)) -> // invalid VXML
}
```

`validate` recursively checks:

- element tag names
- attribute keys and values
- text-line contents
- that every text node contains at least one line

The error identifies both the reason and the offending value's blame.
Leading spaces and tabs in an attribute value are valid and preserved.
Trailing spaces and tabs are rejected.

## Parsing XML and HTML

Both parsers take a source string and a path used in generated `Blame` values:

```gleam
vxml.xml_to_vxml(source, "source.xml")
vxml.html_to_vxml(source, "source.html")
```

They return `Result(VXML, XMLParseError)`. Both discard comments, XML
declarations, and root-level doctypes because VXML cannot represent them.

### XML parsing

`xml_to_vxml` parses XML-like input directly. It decodes character references
in text and attribute values. The five predefined XML entities (`&amp;`,
`&lt;`, `&gt;`, `&quot;`, and `&apos;`) and numeric references are supported;
unknown named entities are errors. An unescaped `<` that does not begin a
recognized XML construct is also an error.

For example, to read an XML file while retaining a shorter source path in its
blames:

```gleam
let path = "content/source.xml"
let short_pathname_to_use_in_blame = "source.xml"

simplifile.read(path)
|> result.map_error(fn(e) {
  vxml.XMLParseError(blame.no_blame, string.inspect(e))
})
|> result.try(vxml.xml_to_vxml(_, short_pathname_to_use_in_blame))
```

### HTML parsing

`html_to_vxml` first applies a narrow repair pass, then parses the repaired
input with unquoted attribute values enabled. The repair pass:

- gives common bare boolean attributes an empty value
- escapes ampersands that do not begin a recognized named or numeric HTML
  character reference
- makes HTML void elements such as `img`, `br`, and `meta` self-closing
- removes attributes from malformed closing tags

The repair helpers are public for callers that need only selected steps. They
are string repairs, not a browser-compatible HTML parser.

Unlike XML parsing, HTML parsing preserves recognized character-reference
spellings in text and attribute values. For example, `&ensp;`, `&Gamma;`,
`&#160;`, and `&#xA0;` remain exactly those strings in VXML. Unknown
entity-like text is protected as literal text: `fish&chips;` becomes
`fish&amp;chips;` in VXML and therefore displays as `fish&chips;` when emitted
as HTML. A `<` that does not begin recognized markup is likewise preserved as
text.

An HTML file can otherwise be read in the same way as XML:

```gleam
let path = "content/source.html"
let short_pathname_to_use_in_blame = "source.html"

simplifile.read(path)
|> result.map_error(fn(e) {
  vxml.XMLParseError(blame.no_blame, string.inspect(e))
})
|> result.try(vxml.html_to_vxml(_, short_pathname_to_use_in_blame))
```

### Lower-level access and validation

The XML and HTML parsers recognize tag and attribute names using XML's `Name`
grammar, which is also the tag grammar of serialized VXML. A parsed tree can
nevertheless fail `validate`; for example, XML and HTML allow an attribute
value to end in whitespace, while serialized VXML does not. Such a tree may
remain suitable for XML or HTML output. Call `validate` when compatibility with
the serialized VXML format is required.

Before parsing, source strings are converted to `List(InputLine)`. That
conversion can be performed directly with `io_lines.string_to_input_lines`, and
the result can be inspected with
[`io_lines.input_lines_table`](#blame-tables). For even lower-level inspection
one can use `vxml/xml_streamer.input_lines_streamer`, which turns those input
lines into XML token events rather than VXML.

## XML Output

```gleam
let lines = vxml.vxml_to_xml_output_lines(tree, 0, 2)
let source = vxml.vxml_to_xml(tree, 0, 2)
```

XML output treats every VXML string as character data and escapes XML syntax
characters. Entity spellings in VXML are not treated as syntax: the literal
string `&ensp;` emits as `&amp;ensp;`.

Element-only content is indented, while mixed content remains compact so that
formatting does not introduce text whitespace. Consecutive `Line` values are
separated by newlines, adjacent text nodes receive no separator, and empty
elements use `<tag/>` syntax.

The serializer does not add an XML declaration or doctype. VXML has no variants
for declarations, doctypes, comments, processing instructions, or CDATA.

## HTML Output

```gleam
let lines = vxml.vxml_to_html_output_lines(tree, 0, 2)
let source = vxml.vxml_to_html(tree, 0, 2)
```

HTML output preserves recognized named and numeric HTML character references in
VXML text and attribute values. This applies whether the spelling came from
parsed HTML or was inserted by application or pipeline code. Raw ampersands and
unknown entity-like strings are escaped.

| VXML text | HTML output |
|---|---|
| `1.&ensp;&Gamma;` | `1.&ensp;&Gamma;` |
| `fish & chips` | `fish &amp; chips` |
| `fish&chips;` | `fish&amp;chips;` |

The serializer treats common inline tags as sticky when laying out output, so
inline content is not forced onto separate lines unless the tree requires it.

Entity spellings can be normalized explicitly before output:

```gleam
vxml.html_entities_to_unicode(tree, except: ["&ensp;"])
vxml.unicode_to_named_html_entities(tree, except: ["&ensp;"])
```

Both functions transform text lines and attribute values.
`html_entities_to_unicode` decodes recognized references except the exact
spellings in `except`. `unicode_to_named_html_entities` uses named references
except for the characters represented by `except`. Exception entries must be
recognized literal HTML entity strings such as `&ensp;`, `&#160;`, or `&#xA0;`.

Convenience exception lists are available for common policies:

```gleam
vxml.html_syntax_entities
vxml.html_spacing_entities
vxml.html_invisible_entities
vxml.html_layout_entities
```

## JSX Output

```gleam
let lines = vxml.vxml_to_jsx_output_lines(tree, 0, 2)
let source = vxml.vxml_to_jsx(tree, 0, 2)
```

The JSX serializer escapes `{`, `}`, `<`, `>`, and `&` in text without
preserving HTML entity spellings. VXML text containing `&ensp;` therefore
emits as `&amp;ensp;`. Quoted attribute values escape the same characters plus
`"`. Attribute values exactly equal to `true`, `false`, or a decimal integer
are emitted as JavaScript expression attributes, such as `enabled={true}` or
`count={3}`. Other attribute values are emitted as double-quoted strings.

## Output Validation

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
let assert Ok(tree) =
  vxml.string_to_vxml(source, "example.vxml")
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
  HTML/XML/JSX-like serialization, XML/HTML parsing, and HTML repair helpers
- `vxml/blame`: provenance data and formatting utilities
- `vxml/io_lines`: input/output line types and conversion helpers
- `vxml/xml_streamer`: advanced XML token stream helpers

Most users should start with `vxml`, `vxml/blame`, and `vxml/io_lines`. Use
`vxml/xml_streamer` when token-level XML processing is needed.

## Tests

Run the package tests from this directory:

```sh
gleam test
```
