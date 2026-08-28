import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import simplifile
import vxml.{type Attr, type VXML, Attr, Line, T, V}
import vxml/blame.{Anchored, Movable, Src}
import vxml/io_lines
import xmlm

fn xmlm_attr_to_vxml_attrs(
  filename: String,
  line_no: Int,
  xmlm_attr: xmlm.Attribute,
) -> Attr {
  let blame = Src([], filename, line_no, 0, Movable)
  Attr(blame, xmlm_attr.name.local, xmlm_attr.value)
}

fn xmlm_based_html_parser(
  content: String,
  filename: String,
) -> Result(VXML, xmlm.InputError) {
  let input = content |> vxml.html_repair |> xmlm.from_string

  case
    xmlm.document_tree(
      input,
      fn(xmlm_tag, children) {
        V(
          Src([], filename, 0, 0, Anchored),
          xmlm_tag.name.local,
          xmlm_tag.attributes
            |> list.map(xmlm_attr_to_vxml_attrs(filename, 0, _)),
          children,
        )
      },
      fn(content) {
        let lines =
          content
          |> string.split("\n")
          |> list.map(fn(content) {
            Line(Src([], filename, 0, 0, Movable), content)
          })
        T(Src([], filename, 0, 0, Movable), lines)
      },
    )
  {
    Ok(#(_, vxml, _)) -> Ok(vxml)
    Error(input_error) -> Error(input_error)
  }
}

pub fn main() {
  gleeunit.main()
}

pub fn parse_and_serialize_roundtrip_test() {
  let source = "<> Book\n  title=Example\n  <>\n    'hello'\n  <> Chapter"

  let assert Ok(parsed) = vxml.parse_string(source, "sample.vxml", True)

  parsed
  |> vxml.vxmls_to_string
  |> should.equal(Ok(source))
}

pub fn serialize_text_keeps_backslashes_literal_test() {
  T(blame.no_blame, [
    Line(
      blame.no_blame,
      "$A\\in \\mathcal{F}$ is called a $\\textbf{tail event}$",
    ),
  ])
  |> vxml.vxml_to_string
  |> should.equal(Ok(
    "<>\n  '$A\\in \\mathcal{F}$ is called a $\\textbf{tail event}$'",
  ))
}

pub fn serialize_returns_partial_output_for_bad_attribute_value_test() {
  V(
    blame.no_blame,
    "Book",
    [
      Attr(blame.no_blame, "title", "Example"),
      Attr(blame.no_blame, "subtitle", "Bad\nValue"),
    ],
    [],
  )
  |> vxml.vxml_to_output_lines
  |> should.equal(
    Error(vxml.VXMLSerializationError(
      partial: [
        io_lines.OutputLine(blame.no_blame, 0, "<> Book"),
        io_lines.OutputLine(blame.no_blame, 2, "title=Example"),
      ],
      blame: blame.no_blame,
      problem: vxml.BadAttributeValue(vxml.IllegalValueCharacter(
        "Bad\nValue",
        "\n",
      )),
    )),
  )
}

pub fn serialize_returns_partial_output_for_bad_text_test() {
  T(blame.no_blame, [
    Line(blame.no_blame, "First"),
    Line(blame.no_blame, "Bad\rText"),
  ])
  |> vxml.vxml_to_output_lines
  |> should.equal(
    Error(vxml.VXMLSerializationError(
      partial: [
        io_lines.OutputLine(blame.no_blame, 0, "<>"),
        io_lines.OutputLine(blame.no_blame, 2, "'First'"),
      ],
      blame: blame.no_blame,
      problem: vxml.BadText(vxml.IllegalTextCharacter("Bad\rText", "\r")),
    )),
  )
}

pub fn serialize_rejects_newline_in_text_test() {
  T(blame.no_blame, [Line(blame.no_blame, "Bad\nText")])
  |> vxml.vxml_to_output_lines
  |> should.equal(
    Error(vxml.VXMLSerializationError(
      partial: [io_lines.OutputLine(blame.no_blame, 0, "<>")],
      blame: blame.no_blame,
      problem: vxml.BadText(vxml.IllegalTextCharacter("Bad\nText", "\n")),
    )),
  )
}

pub fn serialize_rejects_line_free_text_node_test() {
  [
    V(blame.no_blame, "Book", [], []),
    T(blame.no_blame, []),
  ]
  |> vxml.vxmls_to_output_lines
  |> should.equal(
    Error(vxml.VXMLSerializationError(
      partial: [io_lines.OutputLine(blame.no_blame, 0, "<> Book")],
      blame: blame.no_blame,
      problem: vxml.BadText(vxml.EmptyText),
    )),
  )
}

pub fn serialize_returns_partial_output_for_bad_attribute_key_test() {
  V(
    blame.no_blame,
    "Book",
    [
      Attr(blame.no_blame, "bad=key", "value"),
    ],
    [],
  )
  |> vxml.vxml_to_output_lines
  |> should.equal(
    Error(vxml.VXMLSerializationError(
      partial: [io_lines.OutputLine(blame.no_blame, 0, "<> Book")],
      blame: blame.no_blame,
      problem: vxml.BadAttributeKey(vxml.IllegalKeyCharacter("bad=key", "=")),
    )),
  )
}

pub fn serialize_rejects_empty_attribute_key_test() {
  V(blame.no_blame, "Book", [Attr(blame.no_blame, "", "value")], [])
  |> vxml.vxml_to_output_lines
  |> should.equal(
    Error(vxml.VXMLSerializationError(
      partial: [io_lines.OutputLine(blame.no_blame, 0, "<> Book")],
      blame: blame.no_blame,
      problem: vxml.BadAttributeKey(vxml.EmptyKey),
    )),
  )
}

pub fn serialize_returns_prior_roots_for_bad_tag_test() {
  [
    V(blame.no_blame, "Book", [], []),
    V(blame.no_blame, "bad-tag", [], []),
  ]
  |> vxml.vxmls_to_output_lines
  |> should.equal(
    Error(vxml.VXMLSerializationError(
      partial: [io_lines.OutputLine(blame.no_blame, 0, "<> Book")],
      blame: blame.no_blame,
      problem: vxml.BadTag(vxml.MalformedTag("bad-tag", vxml.tag_pattern)),
    )),
  )
}

pub fn serialize_rejects_empty_tag_test() {
  V(blame.no_blame, "", [], [])
  |> vxml.vxml_to_output_lines
  |> should.equal(
    Error(vxml.VXMLSerializationError(
      partial: [],
      blame: blame.no_blame,
      problem: vxml.BadTag(vxml.EmptyTag),
    )),
  )
}

pub fn serialize_nested_failure_preserves_prefix_and_offending_blame_test() {
  let root_blame = Src([], "nested.vxml", 1, 1, Anchored)
  let child_blame = Src([], "nested.vxml", 2, 3, Anchored)
  let line_blame = Src([], "nested.vxml", 3, 5, Movable)
  let bad_attr_blame = Src([], "nested.vxml", 4, 5, Movable)

  V(root_blame, "Root", [], [
    V(child_blame, "Child", [], [
      T(child_blame, [Line(line_blame, "valid")]),
      V(
        child_blame,
        "Broken",
        [
          Attr(bad_attr_blame, "key", "bad\nvalue"),
        ],
        [],
      ),
    ]),
  ])
  |> vxml.vxml_to_output_lines
  |> should.equal(
    Error(vxml.VXMLSerializationError(
      partial: [
        io_lines.OutputLine(root_blame, 0, "<> Root"),
        io_lines.OutputLine(child_blame, 2, "<> Child"),
        io_lines.OutputLine(child_blame, 4, "<>"),
        io_lines.OutputLine(line_blame, 6, "'valid'"),
        io_lines.OutputLine(child_blame, 4, "<> Broken"),
      ],
      blame: bad_attr_blame,
      problem: vxml.BadAttributeValue(vxml.IllegalValueCharacter(
        "bad\nvalue",
        "\n",
      )),
    )),
  )
}

pub fn string_and_table_serializers_propagate_errors_test() {
  let tree = V(blame.no_blame, "", [], [])
  let expected =
    Error(vxml.VXMLSerializationError(
      partial: [],
      blame: blame.no_blame,
      problem: vxml.BadTag(vxml.EmptyTag),
    ))

  tree
  |> vxml.vxml_to_string
  |> should.equal(expected)

  tree
  |> vxml.vxml_table("invalid", 0)
  |> should.equal(expected)
}

pub fn serialize_text_node_with_one_empty_line_test() {
  T(blame.no_blame, [Line(blame.no_blame, "")])
  |> vxml.vxml_to_string
  |> should.equal(Ok("<>\n  ''"))
}

pub fn parse_string_accepts_underscore_start_tag_test() {
  let source = "<> _Internal"

  let assert Ok(parsed) = vxml.parse_string(source, "sample.vxml", True)

  parsed
  |> vxml.vxmls_to_string
  |> should.equal(Ok(source))
}

pub fn parse_string_rejects_multiple_roots_when_unique_root_test() {
  "<> One\n<> Two"
  |> vxml.parse_string("sample.vxml", True)
  |> should.equal(Error(vxml.VXMLParseErrorNonUniqueRoot(2)))
}

pub fn validate_tag_accepts_serialized_vxml_tag_names_test() {
  "Chapter_2.alpha"
  |> vxml.validate_tag
  |> should.equal(Ok("Chapter_2.alpha"))
}

pub fn validate_tag_accepts_underscore_start_test() {
  "_Chapter"
  |> vxml.validate_tag
  |> should.equal(Ok("_Chapter"))
}

pub fn validate_tag_rejects_hyphen_test() {
  "chapter-2"
  |> vxml.validate_tag
  |> should.equal(Error(vxml.MalformedTag("chapter-2", vxml.tag_pattern)))
}

pub fn validate_tag_rejects_digit_start_test() {
  "2Chapter"
  |> vxml.validate_tag
  |> should.equal(Error(vxml.MalformedTag("2Chapter", vxml.tag_pattern)))
}

pub fn validate_key_accepts_syntax_safe_punctuation_test() {
  ["chapter.title", "quoted\"key", "semi;colon"]
  |> list.map(vxml.validate_key)
  |> should.equal([
    Ok("chapter.title"),
    Ok("quoted\"key"),
    Ok("semi;colon"),
  ])
}

pub fn validate_key_rejects_assignment_separator_test() {
  "chapter=title"
  |> vxml.validate_key
  |> should.equal(Error(vxml.IllegalKeyCharacter("chapter=title", "=")))
}

pub fn validate_key_accepts_backslash_test() {
  "chapter\\title"
  |> vxml.validate_key
  |> should.equal(Ok("chapter\\title"))
}

pub fn validate_key_rejects_ascii_whitespace_test() {
  ["chapter title", "chapter\ttitle", "chapter\ntitle", "chapter\rtitle"]
  |> list.map(vxml.validate_key)
  |> should.equal([
    Error(vxml.IllegalKeyCharacter("chapter title", " ")),
    Error(vxml.IllegalKeyCharacter("chapter\ttitle", "\t")),
    Error(vxml.IllegalKeyCharacter("chapter\ntitle", "\n")),
    Error(vxml.IllegalKeyCharacter("chapter\rtitle", "\r")),
  ])
}

pub fn validate_value_accepts_line_safe_content_test() {
  ["", "some value", "a=b", "chapter\\title", "quoted\"value"]
  |> list.map(vxml.validate_value)
  |> should.equal([
    Ok(""),
    Ok("some value"),
    Ok("a=b"),
    Ok("chapter\\title"),
    Ok("quoted\"value"),
  ])
}

pub fn validate_value_rejects_newlines_test() {
  ["some\nvalue", "some\rvalue"]
  |> list.map(vxml.validate_value)
  |> should.equal([
    Error(vxml.IllegalValueCharacter("some\nvalue", "\n")),
    Error(vxml.IllegalValueCharacter("some\rvalue", "\r")),
  ])
}

pub fn html_parser_accepts_common_html_repairs_test() {
  "<html><body><img src=\"x\"><input disabled><p>fish & chips</p></body></html>"
  |> xmlm_based_html_parser("sample.html")
  |> should.be_ok
}

pub fn xml_parser_accepts_underscore_start_tag_test() {
  "<_Internal>Hi</_Internal>"
  |> vxml.parse_xml("sample.xml")
  |> should.be_ok
}

pub fn html_output_escapes_text_test() {
  let assert Ok([node]) =
    "<> p\n  <>\n    'fish & chips < ok >'"
    |> vxml.parse_string("sample.vxml", True)

  node
  |> vxml.vxml_to_html_output_lines(0, 2)
  |> io_lines.output_lines_to_string
  |> should.equal("<p>\n  fish &amp; chips &lt; ok &gt;\n</p>")
}

pub fn xml_output_indents_element_only_content_test() {
  V(blame.no_blame, "book", [], [
    V(
      blame.no_blame,
      "chapter",
      [
        Attr(blame.no_blame, "title", "A & \"B\""),
      ],
      [],
    ),
  ])
  |> vxml.vxml_to_xml(0, 2)
  |> should.equal(
    "<book>\n  <chapter title=\"A &amp; &quot;B&quot;\"/>\n</book>",
  )
}

pub fn xml_output_keeps_mixed_content_compact_test() {
  V(blame.no_blame, "p", [], [
    T(blame.no_blame, [Line(blame.no_blame, "Hello ")]),
    V(blame.no_blame, "em", [], [
      T(blame.no_blame, [Line(blame.no_blame, "XML & friends")]),
    ]),
    T(blame.no_blame, [Line(blame.no_blame, " <done>!")]),
  ])
  |> vxml.vxml_to_xml(0, 2)
  |> should.equal("<p>Hello <em>XML &amp; friends</em> &lt;done&gt;!</p>")
}

pub fn xml_output_adds_no_whitespace_between_text_nodes_test() {
  [
    T(blame.no_blame, [
      Line(blame.no_blame, "a"),
      Line(blame.no_blame, "b"),
    ]),
    T(blame.no_blame, [
      Line(blame.no_blame, "c"),
      Line(blame.no_blame, "d"),
    ]),
  ]
  |> vxml.vxmls_to_xml(0, 2)
  |> should.equal("a\nbc\nd")
}

pub fn xml_output_escapes_attribute_whitespace_test() {
  V(
    blame.no_blame,
    "x",
    [
      Attr(blame.no_blame, "value", "a\tb\nc\rd"),
    ],
    [],
  )
  |> vxml.vxml_to_xml(0, 2)
  |> should.equal("<x value=\"a&#9;b&#10;c&#13;d\"/>")
}

pub fn sample_vxml_file_parses_test() {
  let assert Ok(vxmls) = vxml.parse_file("samples/sample.vxml", False)

  vxmls
  |> list.length
  |> should.equal(2)
}

pub fn sample_html_file_parses_and_emits_test() {
  let assert Ok(content) = simplifile.read("samples/sample.html")
  let assert Ok(node) = xmlm_based_html_parser(content, "samples/sample.html")

  node
  |> vxml.vxml_to_html_output_lines(0, 2)
  |> list.length
  |> fn(length) { length > 0 }
  |> should.be_true
}

pub fn sample_html_streaming_parser_returns_one_root_test() {
  let assert Ok(content) = simplifile.read("samples/sample2.html")

  content
  |> vxml.html_repair
  |> vxml.parse_xml("samples/sample2.html")
  |> should.be_ok
}

pub fn html_repair_close_void_tags_test() {
  "<div><img src=\"x\"><br><input disabled></div>"
  |> vxml.html_repair_close_void_tags
  |> should.equal("<div><img src=\"x\"/><br/><input disabled/></div>")
}

pub fn html_repair_escape_non_entity_ampersands_test() {
  "fish & chips &amp; &CounterClockwiseContourIntegral; &#9; &#xA0; &#XA0;"
  |> vxml.html_repair_escape_non_entity_ampersands
  |> should.equal(
    "fish &amp; chips &amp; &CounterClockwiseContourIntegral; &#9; &#xA0; &#XA0;",
  )
}

pub fn html_and_jsx_output_preserve_long_and_numeric_entities_test() {
  let node =
    T(blame.no_blame, [
      Line(
        blame.no_blame,
        "&CounterClockwiseContourIntegral; &#9; &#xA0; &#XA0; & raw",
      ),
    ])
  let expected =
    "&CounterClockwiseContourIntegral; &#9; &#xA0; &#XA0; &amp; raw"

  node
  |> vxml.vxml_to_html_output_lines(0, 2)
  |> io_lines.output_lines_to_string
  |> should.equal(expected)

  node
  |> vxml.vxml_to_jsx(0, 2)
  |> should.equal(expected)
}

pub fn html_repair_expand_boolean_attrs_test() {
  "<script async src=\"x\"></script><input disabled/>"
  |> vxml.html_repair_expand_boolean_attrs
  |> should.equal(
    "<script async=\"\" src=\"x\"></script><input disabled=\"\"/>",
  )
}

pub fn html_repair_close_void_tags_leaves_already_closed_tags_test() {
  "<meta charset=\"utf-8\"/><hr/>"
  |> vxml.html_repair_close_void_tags
  |> should.equal("<meta charset=\"utf-8\"/><hr/>")
}

pub fn html_repair_remove_attrs_from_closing_tags_uses_each_tag_match_test() {
  "</span class=\"x\"></div id=\"main\"></a href=\"/somewhere\">"
  |> vxml.html_repair_remove_attrs_from_closing_tags
  |> should.equal("</span></div></a>")
}

pub fn html_repair_remove_attrs_from_closing_tags_supports_parser_tag_names_test() {
  "</x-tag data-old=\"1\"></x.name data-old=\"2\"></x_tag data-old=\"3\">"
  |> vxml.html_repair_remove_attrs_from_closing_tags
  |> should.equal("</x-tag></x.name></x_tag>")
}

pub fn html_repair_combines_html_repairs_test() {
  "<img src=\"x\"><span>body</span class=\"old\">"
  |> vxml.html_repair
  |> should.equal("<img src=\"x\"/><span>body</span>")
}
