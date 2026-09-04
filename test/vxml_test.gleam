import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import glentities/decoder as entity_decoder
import simplifile
import vxml.{type Attr, type VXML, Attr, Line, T, V}
import vxml/blame.{Anchored, Movable, Src}
import vxml/io_lines
import vxml/xml_streamer
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

  let assert Ok(parsed) = vxml.string_to_vxml(source, "sample.vxml")

  parsed
  |> vxml.vxml_to_string
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
      reason: vxml.AttributeValueIsBad(vxml.IllegalValueCharacter(
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
      reason: vxml.LinesAreBad(vxml.IllegalLineCharacter(
        2,
        4,
        "Bad\rText",
        "\r",
      )),
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
      reason: vxml.LinesAreBad(vxml.IllegalLineCharacter(
        1,
        4,
        "Bad\nText",
        "\n",
      )),
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
      reason: vxml.LinesAreBad(vxml.NoLines),
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
      reason: vxml.AttributeKeyIsBad(vxml.IllegalKeyCharacter("bad=key", "=")),
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
      reason: vxml.AttributeKeyIsBad(vxml.EmptyKey),
    )),
  )
}

pub fn serialize_returns_prior_roots_for_bad_tag_test() {
  let assert Error(vxml.VXMLSerializationError(
    partial: [io_lines.OutputLine(blame: _, indent: 0, suffix: "<> Book")],
    blame: _,
    reason: vxml.TagIsBad(vxml.MalformedTag("bad tag", _)),
  )) =
    [
      V(blame.no_blame, "Book", [], []),
      V(blame.no_blame, "bad tag", [], []),
    ]
    |> vxml.vxmls_to_output_lines
}

pub fn serialize_rejects_empty_tag_test() {
  V(blame.no_blame, "", [], [])
  |> vxml.vxml_to_output_lines
  |> should.equal(
    Error(vxml.VXMLSerializationError(
      partial: [],
      blame: blame.no_blame,
      reason: vxml.TagIsBad(vxml.EmptyTag),
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
      reason: vxml.AttributeValueIsBad(vxml.IllegalValueCharacter(
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
      reason: vxml.TagIsBad(vxml.EmptyTag),
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

pub fn string_to_vxml_accepts_underscore_start_tag_test() {
  let source = "<> _Internal"

  let assert Ok(parsed) = vxml.string_to_vxml(source, "sample.vxml")

  parsed
  |> vxml.vxml_to_string
  |> should.equal(Ok(source))
}

pub fn string_to_vxml_rejects_multiple_roots_test() {
  "<> One\n<> Two"
  |> vxml.string_to_vxml("sample.vxml")
  |> should.equal(Error(vxml.ExpectedOneRoot(2)))
}

pub fn serialized_vxml_parser_reports_indentation_test() {
  let assert Error(vxml.UnexpectedIndentation(_, 0, 2, "<> Book")) =
    vxml.string_to_vxml("  <> Book", "sample.vxml")
}

pub fn serialized_vxml_parser_rejects_tabs_in_indentation_test() {
  let assert Error(vxml.TabInIndentation(_, "\t<> Book")) =
    vxml.string_to_vxml("\t<> Book", "sample.vxml")

  let assert Error(vxml.TabInIndentation(_, "\t<> Chapter")) =
    vxml.string_to_vxml("<> Book\n \t<> Chapter", "sample.vxml")
}

pub fn serialized_vxml_parser_reports_text_line_quote_tests() {
  let assert Error(vxml.TextLineOpeningQuoteMissing(_, "hello'")) =
    vxml.string_to_vxml("<>\n  hello'", "sample.vxml")

  let assert Error(vxml.TextLineClosingQuoteMissing(_, "'hello")) =
    vxml.string_to_vxml("<>\n  'hello", "sample.vxml")
}

pub fn serialized_vxml_parser_reports_empty_text_node_test() {
  let assert Error(vxml.TextNodeLinesMissing(_)) =
    vxml.string_to_vxml("<>", "sample.vxml")
}

pub fn serialized_vxml_parser_reports_missing_node_marker_test() {
  let assert Error(vxml.NodeMarkerMissing(_, "Book")) =
    vxml.string_to_vxml("Book", "sample.vxml")
}

pub fn validate_tag_accepts_serialized_vxml_tag_names_test() {
  "chapter-2.alpha"
  |> vxml.validate_tag
  |> should.equal(Ok("chapter-2.alpha"))
}

pub fn validate_tag_accepts_xml_names_test() {
  ["svg:path", "élément", "章", "a·b", "a\u{0301}"]
  |> list.map(vxml.validate_tag)
  |> should.equal([
    Ok("svg:path"),
    Ok("élément"),
    Ok("章"),
    Ok("a·b"),
    Ok("a\u{0301}"),
  ])
}

pub fn xml_parser_accepts_xml_names_test() {
  let assert Ok(V(
    _,
    "svg:élément",
    [Attr(_, "xml:lang", "fr"), Attr(_, "_用途", "例")],
    [V(_, "章", [], [])],
  )) =
    "<svg:élément xml:lang=\"fr\" _用途=\"例\"><章/></svg:élément>"
    |> vxml.xml_to_vxml("names.xml")
}

pub fn validate_tag_accepts_underscore_start_test() {
  "_Chapter"
  |> vxml.validate_tag
  |> should.equal(Ok("_Chapter"))
}

pub fn hyphenated_tag_round_trips_through_serialized_vxml_test() {
  let tree = V(blame.no_blame, "my-widget", [], [])
  let assert Ok(serialized) = vxml.vxml_to_string(tree)
  let assert Ok(parsed) = vxml.string_to_vxml(serialized, "widget.vxml")

  parsed
  |> vxml.vxml_to_string
  |> should.equal(Ok(serialized))
}

pub fn validate_tag_rejects_digit_start_test() {
  let assert Error(vxml.MalformedTag("2Chapter", _)) =
    vxml.validate_tag("2Chapter")
}

pub fn validate_tag_rejects_non_xml_name_characters_test() {
  ["-chapter", "chapter name", "chapter/name", "chapter?"]
  |> list.each(fn(tag) {
    let assert Error(vxml.MalformedTag(_, "XML Name")) = vxml.validate_tag(tag)
  })
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
  ["", "some value", "  aligned", "\taligned", "a=b", "chapter\\title"]
  |> list.map(vxml.validate_value)
  |> should.equal([
    Ok(""),
    Ok("some value"),
    Ok("  aligned"),
    Ok("\taligned"),
    Ok("a=b"),
    Ok("chapter\\title"),
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

pub fn validate_value_rejects_trailing_spaces_and_tabs_test() {
  ["value ", "value\t", " \t"]
  |> list.map(vxml.validate_value)
  |> should.equal([
    Error(vxml.TrailingWhitespace("value ", " ")),
    Error(vxml.TrailingWhitespace("value\t", "\t")),
    Error(vxml.TrailingWhitespace(" \t", "\t")),
  ])
}

pub fn validate_accepts_a_well_formed_tree_test() {
  V(blame.no_blame, "Book", [Attr(blame.no_blame, "title", " Example")], [
    T(blame.no_blame, [
      Line(blame.no_blame, "First line"),
      Line(blame.no_blame, ""),
    ]),
  ])
  |> vxml.validate
  |> should.equal(Ok(Nil))
}

pub fn serialized_vxml_preserves_leading_attribute_whitespace_test() {
  let source = "<> Book\n  title=  Example\n  tab=\tvalue"
  let assert Ok(tree) = vxml.string_to_vxml(source, "sample.vxml")
  let assert V(_, "Book", [Attr(_, "title", title), Attr(_, "tab", tab)], []) =
    tree
  title |> should.equal("  Example")
  tab |> should.equal("\tvalue")
  tree |> vxml.vxml_to_string |> should.equal(Ok(source))
}

pub fn serialized_vxml_parser_rejects_trailing_attribute_whitespace_test() {
  let assert Error(vxml.BadAttributeValue(
    _,
    vxml.TrailingWhitespace("Example ", " "),
  )) = vxml.string_to_vxml("<> Book\n  title=Example ", "sample.vxml")

  let assert Error(vxml.BadAttributeValue(
    _,
    vxml.TrailingWhitespace("Example\t", "\t"),
  )) = vxml.string_to_vxml("<> Book\n  title=Example\t", "sample.vxml")
}

pub fn serialized_vxml_serializer_rejects_trailing_attribute_whitespace_test() {
  V(
    blame.no_blame,
    "Book",
    [
      Attr(blame.no_blame, "title", "Example "),
    ],
    [],
  )
  |> vxml.vxml_to_string
  |> should.equal(
    Error(vxml.VXMLSerializationError(
      partial: [io_lines.OutputLine(blame.no_blame, 0, "<> Book")],
      blame: blame.no_blame,
      reason: vxml.AttributeValueIsBad(vxml.TrailingWhitespace("Example ", " ")),
    )),
  )
}

pub fn validate_rejects_invalid_element_and_attribute_names_test() {
  let assert Error(vxml.VXMLValidationError(
    _,
    vxml.TagIsBad(vxml.MalformedTag("bad tag", _)),
  )) = vxml.validate(V(blame.no_blame, "bad tag", [], []))

  V(blame.no_blame, "Book", [Attr(blame.no_blame, "bad key", "value")], [])
  |> vxml.validate
  |> should.equal(
    Error(vxml.VXMLValidationError(
      blame.no_blame,
      vxml.AttributeKeyIsBad(vxml.IllegalKeyCharacter("bad key", " ")),
    )),
  )
}

pub fn validate_rejects_line_breaks_in_values_and_text_test() {
  V(blame.no_blame, "Book", [Attr(blame.no_blame, "title", "bad\nvalue")], [])
  |> vxml.validate
  |> should.equal(
    Error(vxml.VXMLValidationError(
      blame.no_blame,
      vxml.AttributeValueIsBad(vxml.IllegalValueCharacter("bad\nvalue", "\n")),
    )),
  )

  T(blame.no_blame, [Line(blame.no_blame, "bad\rtext")])
  |> vxml.validate
  |> should.equal(
    Error(vxml.VXMLValidationError(
      blame.no_blame,
      vxml.LinesAreBad(vxml.IllegalLineCharacter(1, 4, "bad\rtext", "\r")),
    )),
  )
}

pub fn validate_rejects_an_empty_nested_text_node_test() {
  let child_blame = Src([], "invalid.vxml", 3, 5, Movable)

  V(blame.no_blame, "Book", [], [T(child_blame, [])])
  |> vxml.validate
  |> should.equal(
    Error(vxml.VXMLValidationError(child_blame, vxml.LinesAreBad(vxml.NoLines))),
  )
}

pub fn html_parser_accepts_common_html_repairs_test() {
  "<html><body><img src=\"x\"><input disabled><p>fish & chips</p></body></html>"
  |> xmlm_based_html_parser("sample.html")
  |> should.be_ok
}

pub fn xml_parser_accepts_underscore_start_tag_test() {
  "<_Internal>Hi</_Internal>"
  |> vxml.xml_to_vxml("sample.xml")
  |> should.be_ok
}

pub fn xml_parser_returns_named_error_test() {
  let assert Error(vxml.XMLParseError(_, _)) =
    vxml.xml_to_vxml("<unclosed>", "sample.xml")
}

pub fn xml_parser_advances_past_unicode_text_test() {
  let assert Ok(V(_, "span", [], [T(_, [Line(_, "Übungsaufgabe")])])) =
    vxml.xml_to_vxml("<span>Übungsaufgabe</span>", "sample.xml")
}

pub fn xml_parser_handles_attributes_comments_and_multiline_text_test() {
  "<section title=\"Überblick\" state='ready'>\n<!-- note -->\n<p>α</p>\n</section>"
  |> vxml.xml_to_vxml("sample.xml")
  |> should.be_ok
}

pub fn xml_parser_allows_tag_delimiters_inside_quoted_values_test() {
  let source = "<div double=\"a > b /> c ?> d\" single='a > b /> c ?> d'/>"
  let assert Ok(V(
    _,
    "div",
    [Attr(_, "double", "a > b /> c ?> d"), Attr(_, "single", "a > b /> c ?> d")],
    [],
  )) = vxml.xml_to_vxml(source, "sample.xml")
}

pub fn xml_parser_rejects_unterminated_quoted_value_test() {
  let assert Error(vxml.XMLParseError(_, _)) =
    vxml.xml_to_vxml("<div title=\"unterminated>", "sample.xml")
}

pub fn xml_parser_rejects_unquoted_attribute_value_test() {
  let assert Error(vxml.XMLParseError(_, _)) =
    vxml.xml_to_vxml("<div title=unquoted/>", "sample.xml")
}

pub fn xml_parser_rejects_attributes_without_quoted_values_test() {
  ["<root disabled/>", "<root key=/>"]
  |> list.each(fn(source) {
    let assert Error(vxml.XMLParseError(_, _)) =
      vxml.xml_to_vxml(source, "sample.xml")
  })
}

pub fn xml_parser_decodes_character_references_test() {
  let assert Ok(V(
    _,
    "p",
    [Attr(_, "title", "A & B")],
    [T(_, [Line(_, "Γ < & > \" '")])],
  )) =
    "<p title=\"A &amp; B\">&#915; &lt; &amp; &gt; &quot; &apos;</p>"
    |> vxml.xml_to_vxml("sample.xml")
}

pub fn xml_parser_rejects_unknown_named_character_references_test() {
  let assert Error(vxml.XMLParseError(_, _)) =
    "<p>&Gamma;</p>"
    |> vxml.xml_to_vxml("sample.xml")
}

pub fn xml_parser_rejects_unrecognized_less_than_in_text_test() {
  ["<root>before < after</root>", "<2invalid/>"]
  |> list.each(fn(source) {
    let assert Error(vxml.XMLParseError(_, _)) =
      vxml.xml_to_vxml(source, "sample.xml")
  })
}

pub fn html_parser_preserves_unrecognized_less_than_as_text_test() {
  let assert Ok(V(_, "p", [], [T(_, [Line(_, "before <2invalid/> after")])])) =
    "<p>before <2invalid/> after</p>"
    |> vxml.html_to_vxml("sample.html")
}

pub fn html_parser_preserves_character_references_test() {
  let assert Ok(V(
    _,
    "p",
    [Attr(_, "title", "A &amp; B")],
    [T(_, [Line(_, "&Gamma; &amp; fish")])],
  )) =
    "<p title=\"A &amp; B\">&Gamma; &amp; fish</p>"
    |> vxml.html_to_vxml("sample.html")
}

pub fn html_parser_accepts_unquoted_attribute_values_test() {
  let assert Ok(V(_, "img", [Attr(_, "src", "fish&amp;chips.jpg")], [])) =
    "<img src=fish&amp;chips.jpg>"
    |> vxml.html_to_vxml("sample.html")
}

pub fn html_parser_accepts_attributes_without_values_test() {
  let assert Ok(V(_, "root", [Attr(_, "custom", ""), Attr(_, "key", "")], [])) =
    vxml.html_to_vxml("<root custom key=/>", "sample.html")
}

pub fn xml_parser_accepts_and_discards_html5_doctype_test() {
  let assert Ok(V(_, "html", [], [])) =
    "<!DOCTYPE html>\n<html/>"
    |> vxml.xml_to_vxml("sample.html")
}

pub fn xml_parser_accepts_and_discards_public_doctype_test() {
  let assert Ok(V(_, "html", [], [])) =
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n<html/>"
    |> vxml.xml_to_vxml("sample.html")
}

pub fn xml_parser_accepts_doctype_internal_subset_test() {
  let assert Ok(V(_, "root", [], [])) =
    "<!DOCTYPE root [\n<!ELEMENT root (#PCDATA)>\n<!ENTITY example \"a > b\">\n]>\n<root/>"
    |> vxml.xml_to_vxml("sample.xml")
}

pub fn xml_streamer_tokenizes_unquoted_attribute_values_test() {
  let events =
    "<div title=unquoted count=2/>"
    |> io_lines.string_to_input_lines("sample.html", 0)
    |> xml_streamer.input_lines_streamer

  events
  |> list.any(fn(event) {
    case event {
      xml_streamer.ValueUnquoted(_, "unquoted") -> True
      _ -> False
    }
  })
  |> should.be_true

  events
  |> list.any(fn(event) {
    case event {
      xml_streamer.ValueUnquoted(_, "2") -> True
      _ -> False
    }
  })
  |> should.be_true
}

pub fn glentities_decoder_decodes_named_and_numeric_entities_test() {
  "A &Gamma; &#916; &#x394; &copy; B"
  |> entity_decoder.decode
  |> should.equal("A Γ Δ Δ © B")
}

pub fn glentities_decoder_preserves_unknown_entities_test() {
  "fish&chips;"
  |> entity_decoder.decode
  |> should.equal("fish&chips;")
}

pub fn xml_parser_accepts_empty_comment_test() {
  "<root><!----><child/></root>"
  |> vxml.xml_to_vxml("sample.xml")
  |> should.be_ok
}

pub fn xml_parser_accepts_multiline_comment_test() {
  "<root><!-- first line\nsecond line --><child/></root>"
  |> vxml.xml_to_vxml("sample.xml")
  |> should.be_ok
}

pub fn xml_streamer_scans_many_non_tag_lt_characters_without_overflow_test() {
  let prefix = string.repeat("<!", 10_000)

  { prefix <> "<root/>" }
  |> io_lines.string_to_input_lines("sample.xml", 0)
  |> xml_streamer.input_lines_streamer
  |> list.is_empty
  |> should.be_false
}

pub fn html_output_escapes_text_test() {
  let assert Ok(node) =
    "<> p\n  <>\n    'fish & chips < ok >'"
    |> vxml.string_to_vxml("sample.vxml")

  node
  |> vxml.vxml_to_html(0, 2)
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
  let assert Ok(vxmls) = vxml.path_to_vxmls("samples/sample.vxml")

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
  |> vxml.html_to_vxml("samples/sample2.html")
  |> should.be_ok
}

pub fn html_repair_close_void_tags_test() {
  "<div><img src=\"x\"><br><input disabled></div>"
  |> vxml.html_repair_close_void_tags
  |> should.equal("<div><img src=\"x\"/><br/><input disabled/></div>")
}

pub fn html_repair_escape_non_entity_ampersands_test() {
  "fish & chips &chips; &amp; &CounterClockwiseContourIntegral; &#9; &#xA0; &#XA0; &#nope;"
  |> vxml.html_repair_escape_non_entity_ampersands
  |> should.equal(
    "fish &amp; chips &amp;chips; &amp; &CounterClockwiseContourIntegral; &#9; &#xA0; &#XA0; &amp;#nope;",
  )
}

pub fn html_parser_treats_unknown_entity_like_literal_text_test() {
  let assert Ok(V(_, "p", [], [T(_, [Line(_, "fish&amp;chips;")])])) =
    "<p>fish&chips;</p>"
    |> vxml.html_to_vxml("sample.html")
}

pub fn html_output_preserves_known_entities_test() {
  let node =
    V(
      blame.no_blame,
      "p",
      [
        Attr(blame.no_blame, "title", "&Gamma; &ensp; & raw &chips;"),
      ],
      [
        T(blame.no_blame, [
          Line(
            blame.no_blame,
            "&CounterClockwiseContourIntegral; &ensp; &#9; &#xA0; & raw &chips;",
          ),
        ]),
      ],
    )
  let expected =
    "<p title=\"&Gamma; &ensp; &amp; raw &amp;chips;\">\n  &CounterClockwiseContourIntegral; &ensp; &#9; &#xA0; &amp; raw &amp;chips;\n</p>"

  node
  |> vxml.vxml_to_html(0, 2)
  |> should.equal(expected)
}

pub fn html_entities_to_unicode_decodes_known_entities_test() {
  let node =
    V(
      blame.no_blame,
      "p",
      [Attr(blame.no_blame, "title", "&Gamma; &ensp; &chips;")],
      [
        T(blame.no_blame, [
          Line(blame.no_blame, "&Gamma; &ensp; &#xA0; &chips;"),
        ]),
      ],
    )

  let assert Ok(V(_, _, [Attr(_, _, title)], [T(_, [Line(_, content)])])) =
    node
    |> vxml.html_entities_to_unicode(except: [])

  title
  |> should.equal("Γ   &chips;")
  content
  |> should.equal("Γ     &chips;")
}

pub fn html_entities_to_unicode_preserves_exception_spellings_test() {
  let node =
    T(blame.no_blame, [
      Line(blame.no_blame, "&Gamma; &ensp; &#xA0;"),
    ])

  let assert Ok(T(_, [Line(_, content)])) =
    node
    |> vxml.html_entities_to_unicode(except: ["&ensp;"])

  content
  |> should.equal("Γ &ensp;  ")
}

pub fn unicode_to_named_html_entities_encodes_known_characters_test() {
  let node =
    V(blame.no_blame, "p", [Attr(blame.no_blame, "title", "Γ   &")], [
      T(blame.no_blame, [Line(blame.no_blame, "Γ   &")]),
    ])

  let assert Ok(V(_, _, [Attr(_, _, title)], [T(_, [Line(_, content)])])) =
    node
    |> vxml.unicode_to_named_html_entities(except: [])

  title
  |> should.equal("&Gamma; &ensp; &amp;")
  content
  |> should.equal("&Gamma; &ensp; &amp;")
}

pub fn unicode_to_named_html_entities_preserves_exception_characters_test() {
  let node =
    T(blame.no_blame, [
      Line(blame.no_blame, "Γ   &"),
    ])

  let assert Ok(T(_, [Line(_, content)])) =
    node
    |> vxml.unicode_to_named_html_entities(except: ["&ensp;", "&amp;"])

  content
  |> should.equal("&Gamma;   &")
}

pub fn html_entity_normalization_rejects_bad_exceptions_test() {
  T(blame.no_blame, [Line(blame.no_blame, "x")])
  |> vxml.html_entities_to_unicode(except: ["ensp"])
  |> should.equal(Error(vxml.MalformedHTMLEntityException("ensp")))

  T(blame.no_blame, [Line(blame.no_blame, "x")])
  |> vxml.html_entities_to_unicode(except: ["&chips;"])
  |> should.equal(Error(vxml.UnrecognizedHTMLEntityException("&chips;")))
}

pub fn jsx_output_escapes_text_without_preserving_entities_test() {
  let node =
    T(blame.no_blame, [
      Line(blame.no_blame, "&CounterClockwiseContourIntegral; {x} <y> & raw"),
    ])
  let expected =
    "&amp;CounterClockwiseContourIntegral; &#123;x&#125; &lt;y&gt; &amp; raw"

  node
  |> vxml.vxml_to_jsx(0, 2)
  |> should.equal(expected)
}

pub fn jsx_output_escapes_string_attribute_values_test() {
  V(
    blame.no_blame,
    "p",
    [
      Attr(blame.no_blame, "title", "A & \"quoted\" {word} <x>"),
    ],
    [],
  )
  |> vxml.vxml_to_jsx(0, 2)
  |> should.equal(
    "<p title=\"A &amp; &quot;quoted&quot; &#123;word&#125; &lt;x&gt;\" />",
  )
}

pub fn jsx_output_uses_exact_boolean_and_integer_attributes_test() {
  V(
    blame.no_blame,
    "Widget",
    [
      Attr(blame.no_blame, "active", "true"),
      Attr(blame.no_blame, "count", "-3"),
      Attr(blame.no_blame, "padded", " 3"),
    ],
    [],
  )
  |> vxml.vxml_to_jsx(0, 2)
  |> should.equal("<Widget\n  active={true}\n  count={-3}\n  padded=\" 3\"\n/>")
}

pub fn html_output_escapes_attribute_values_test() {
  V(blame.no_blame, "p", [Attr(blame.no_blame, "title", "A & \"B\"")], [])
  |> vxml.vxml_to_html(0, 2)
  |> should.equal("<p title=\"A &amp; &quot;B&quot;\">\n</p>")
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
