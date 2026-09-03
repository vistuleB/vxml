//// Token-level XML streaming utilities.
////
//// This module exposes the lower-level token stream used by VXML's streaming
//// XML parser. Most callers can use `vxml.parse_xml` instead. Use this module
//// when an application needs to inspect or transform XML events before they
//// become a VXML tree.

import gleam/list
import gleam/regexp
import gleam/string.{inspect as ins}
import splitter as sp
import vxml/blame.{type Blame} as bl
import vxml/io_lines.{type InputLine}

const bd = bl.blame_digest

/// Renders one token as a debug string.
pub fn event_digest(e: Event) -> String {
  case e {
    Newline(b) -> "Newline(" <> bd(b) <> ")"

    TagStartOrdinary(b, load) ->
      "TagStartOrdinary(" <> load <> ", " <> bd(b) <> ")"
    TagStartXMLVersion(b, load) ->
      "TagStartXMLVersion(" <> load <> ", " <> bd(b) <> ")"
    TagStartDoctype(b, load) ->
      "TagStartDoctype(" <> load <> ", " <> bd(b) <> ")"
    TagStartClosing(b, load) ->
      "TagStartClosing(" <> load <> ", " <> bd(b) <> ")"

    InTagWhitespace(b, load) ->
      "InTagWhitespace(" <> load <> ", " <> bd(b) <> ")"

    Key(b, load) -> "Key(" <> ins(load) <> ", " <> bd(b) <> ")"
    KeyMalformed(b, load) ->
      "KeyMalformed(" <> ins(load) <> ", " <> bd(b) <> ")"
    Assignment(b) -> "Assignment(" <> bd(b) <> ")"
    ValueDoubleQuoted(b, load) ->
      "ValueDoubleQuoted(" <> ins(load) <> ", " <> bd(b) <> ")"
    ValueSingleQuoted(b, load) ->
      "ValueSingleQuoted(" <> ins(load) <> ", " <> bd(b) <> ")"
    ValueMalformed(b, load) ->
      "ValueMalformed(" <> ins(load) <> ", " <> bd(b) <> ")"

    TagEndOrdinary(b) -> "TagEndOrdinary(" <> bd(b) <> ")"
    TagEndSelfClosing(b) -> "TagEndSelfClosing(" <> bd(b) <> ")"
    TagEndXMLVersion(b) -> "TagEndXMLVersion(" <> bd(b) <> ")"

    Text(b, load) -> "Text(" <> ins(load) <> ", " <> bd(b) <> ")"
    CommentContents(b, load) ->
      "CommentContents(" <> ins(load) <> ", " <> bd(b) <> ")"

    CommentStartSequence(b) -> "CommentStartSequence(" <> bd(b) <> ")"
    CommentEndSequence(b) -> "CommentEndSequence(" <> bd(b) <> ")"
  }
}

/// A token produced by the XML streamer.
pub type Event {
  /// A physical newline, whether inside or outside a tag.
  Newline(blame: Blame)

  /// The name following the `<` of an ordinary opening tag.
  TagStartOrdinary(blame: Blame, load: String)
  /// The name following `<?` at the beginning of an XML declaration.
  TagStartXMLVersion(blame: Blame, load: String)
  /// The declaration name following `<!` at the beginning of a doctype.
  TagStartDoctype(blame: Blame, load: String)
  /// The name following the `</` of a closing tag.
  TagStartClosing(blame: Blame, load: String)

  /// Whitespace occurring inside a tag.
  InTagWhitespace(blame: Blame, load: String)

  /// A syntactically valid attribute key.
  Key(blame: Blame, load: String)
  /// Text encountered where an attribute key was expected.
  KeyMalformed(blame: Blame, load: String)
  /// An attribute assignment marker, `=`.
  Assignment(blame: Blame)
  /// An attribute value delimited by double quotes.
  ValueDoubleQuoted(blame: Blame, load: String)
  /// An attribute value delimited by single quotes.
  ValueSingleQuoted(blame: Blame, load: String)
  /// Text encountered where a quoted attribute value was expected.
  ValueMalformed(blame: Blame, load: String)

  /// The end of an ordinary opening tag, `>`.
  TagEndOrdinary(blame: Blame)
  /// The end of a self-closing tag, `/>`.
  TagEndSelfClosing(blame: Blame)
  /// The end of an XML declaration, `?>`.
  TagEndXMLVersion(blame: Blame)

  /// Text outside a tag; outside-tag whitespace is included here.
  Text(blame: Blame, load: String)
  /// Text inside an XML comment.
  CommentContents(blame: Blame, load: String)

  /// The opening sequence of an XML comment, `<!--`.
  CommentStartSequence(blame: Blame)
  /// The closing sequence of an XML comment, `-->`.
  CommentEndSequence(blame: Blame)
}

type ContentLine {
  ContentLine(blame: Blame, content: String)
}

type FileHead =
  List(ContentLine)

type State {
  OutsideTag
  InsideOpeningTagExpectingNextKey
  InsideOpeningTagExpectingNextAssignment
  InsideOpeningTagExpectingNextValue
  InsideClosingTag
  InsideComment
}

type TagOrNot {
  XMLDoc(String)
  // String will be "?xml" or "?XML"
  Doctype(String)
  // String will be "!DOCTYPE" or "!Doctype" or "!doctype"
  Ordinary(String)
  OrdinaryClosing(String)
  NoTag
  CommentStart
}

fn advance_line(cl: ContentLine, by: Int) -> ContentLine {
  assert by > 0
  assert string.length(cl.content) >= by
  ContentLine(
    bl.advance(cl.blame, by),
    string.slice(cl.content, by, string.length(cl.content) - by),
  )
}

fn is_ordinary_tag(input: String) -> Bool {
  let pattern = "^[a-zA-Z_][a-zA-Z0-9._-]*$"
  let assert Ok(re) = regexp.from_string(pattern)
  regexp.check(re, input)
}

fn is_valid_key(input: String) -> Bool {
  let pattern = "^[a-zA-Z][:a-zA-Z0-9._-]*$"
  let assert Ok(re) = regexp.from_string(pattern)
  regexp.check(re, input)
}

fn check_for_tag_after_lt(after: String) -> TagOrNot {
  let s = sp.new([" ", ">", "/>", "?>"])
  let #(before, _, _) = sp.split(s, after)
  case before {
    "?xml" | "?XML" ->
      XMLDoc(string.slice(before, 1, string.length(before) - 1))
    "!DOCTYPE" | "!Doctype" | "!doctype" ->
      Doctype(string.slice(before, 1, string.length(before) - 1))
    _ ->
      case is_ordinary_tag(before) {
        True -> Ordinary(before)
        False -> NoTag
      }
  }
}

fn check_for_tag_after_lt_closing(after: String) -> TagOrNot {
  let s = sp.new([" ", ">", "/>", "?>"])
  let #(before, _, _) = sp.split(s, after)
  case is_ordinary_tag(before) {
    True -> OrdinaryClosing(before)
    False -> NoTag
  }
}

fn take_text_up_to_next_tag(text: String) -> #(String, TagOrNot) {
  take_text_up_to_next_tag_loop(text, [])
}

fn take_text_up_to_next_tag_loop(
  remaining: String,
  preceding_reversed: List(String),
) -> #(String, TagOrNot) {
  case string.split_once(remaining, "<") {
    Error(Nil) -> #(
      [remaining, ..preceding_reversed]
        |> list.reverse
        |> string.concat,
      NoTag,
    )
    Ok(#(before, after)) ->
      case string.starts_with(after, "/") {
        True -> {
          let after_slash = string.slice(after, 1, string.length(after) - 1)
          case check_for_tag_after_lt_closing(after_slash) {
            OrdinaryClosing(_) as tag -> #(
              [before, ..preceding_reversed]
                |> list.reverse
                |> string.concat,
              tag,
            )
            _ ->
              take_text_up_to_next_tag_loop(after_slash, [
                before <> "</",
                ..preceding_reversed
              ])
          }
        }
        False ->
          case string.starts_with(after, "!--") {
            True -> #(
              [before, ..preceding_reversed]
                |> list.reverse
                |> string.concat,
              CommentStart,
            )
            False ->
              case check_for_tag_after_lt(after) {
                NoTag ->
                  take_text_up_to_next_tag_loop(after, [
                    before <> "<",
                    ..preceding_reversed
                  ])
                tag -> #(
                  [before, ..preceding_reversed]
                    |> list.reverse
                    |> string.concat,
                  tag,
                )
              }
          }
      }
  }
}

fn event_stream_internal(
  previous: List(Event),
  state: State,
  remaining: FileHead,
) -> List(Event) {
  case remaining {
    [] -> list.reverse(previous)
    [first, ..rest] ->
      case first.content {
        "" ->
          case rest {
            [] -> list.reverse(previous)
            _ ->
              event_stream_internal(
                [Newline(first.blame), ..previous],
                state,
                rest,
              )
          }
        _ ->
          case state {
            OutsideTag -> stream_outside_tag(previous, first, rest)
            InsideComment -> stream_inside_comment(previous, first, rest)
            _ -> stream_inside_tag(previous, state, first, rest)
          }
      }
  }
}

fn stream_outside_tag(
  previous: List(Event),
  first: ContentLine,
  rest: FileHead,
) -> List(Event) {
  let #(text, tag_or_not) = take_text_up_to_next_tag(first.content)
  let previous = case text {
    "" -> previous
    _ -> [Text(first.blame, text), ..previous]
  }
  let end_of_text_blame = bl.advance(first.blame, string.length(text))

  case tag_or_not {
    NoTag -> {
      assert text == first.content
      event_stream_internal(
        [Newline(end_of_text_blame), ..previous],
        OutsideTag,
        rest,
      )
    }
    _ -> {
      let #(event, prefix, tag, state) =
        tag_start(tag_or_not, end_of_text_blame)
      let length = string.length(text <> prefix <> tag)
      assert string.length(first.content) >= length
      event_stream_internal([event, ..previous], state, [
        advance_line(first, length),
        ..rest
      ])
    }
  }
}

fn tag_start(tag: TagOrNot, blame: Blame) -> #(Event, String, String, State) {
  case tag {
    XMLDoc(name) -> #(
      TagStartXMLVersion(blame, name),
      "<?",
      name,
      InsideOpeningTagExpectingNextKey,
    )
    Doctype(name) -> #(
      TagStartDoctype(blame, name),
      "<!",
      name,
      InsideOpeningTagExpectingNextKey,
    )
    Ordinary(name) -> #(
      TagStartOrdinary(blame, name),
      "<",
      name,
      InsideOpeningTagExpectingNextKey,
    )
    OrdinaryClosing(name) -> #(
      TagStartClosing(blame, name),
      "</",
      name,
      InsideClosingTag,
    )
    CommentStart -> #(CommentStartSequence(blame), "<!--", "", InsideComment)
    NoTag -> panic as "NoTag has no tag-start event"
  }
}

fn stream_inside_comment(
  previous: List(Event),
  first: ContentLine,
  rest: FileHead,
) -> List(Event) {
  case string.split_once(first.content, "-->") {
    Error(Nil) ->
      event_stream_internal(
        [
          Newline(bl.advance(first.blame, string.length(first.content))),
          CommentContents(first.blame, first.content),
          ..previous
        ],
        InsideComment,
        rest,
      )
    Ok(#("", _)) ->
      event_stream_internal(
        [CommentEndSequence(first.blame), ..previous],
        OutsideTag,
        [advance_line(first, 3), ..rest],
      )
    Ok(#(before, _)) -> {
      let length = string.length(before)
      event_stream_internal(
        [
          CommentEndSequence(bl.advance(first.blame, length)),
          CommentContents(first.blame, before),
          ..previous
        ],
        OutsideTag,
        [advance_line(first, length + 3), ..rest],
      )
    }
  }
}

fn stream_inside_tag(
  previous: List(Event),
  state: State,
  first: ContentLine,
  rest: FileHead,
) -> List(Event) {
  let num_whitespace =
    string.length(first.content)
    - string.length(string.trim_start(first.content))

  case num_whitespace > 0 {
    True -> {
      let whitespace = string.slice(first.content, 0, num_whitespace)
      event_stream_internal(
        [InTagWhitespace(first.blame, whitespace), ..previous],
        state,
        [advance_line(first, num_whitespace), ..rest],
      )
    }
    False -> stream_inside_tag_without_whitespace(previous, first, rest)
  }
}

fn stream_inside_tag_without_whitespace(
  previous: List(Event),
  first: ContentLine,
  rest: FileHead,
) -> List(Event) {
  case string.starts_with(first.content, "=") {
    True ->
      event_stream_internal(
        [Assignment(first.blame), ..previous],
        InsideOpeningTagExpectingNextValue,
        [advance_line(first, 1), ..rest],
      )
    False ->
      case string.starts_with(first.content, "\"") {
        True ->
          stream_quoted_value(previous, first, rest, "\"", ValueDoubleQuoted)
        False ->
          case string.starts_with(first.content, "'") {
            True ->
              stream_quoted_value(previous, first, rest, "'", ValueSingleQuoted)
            False -> stream_key_or_tag_end(previous, first, rest)
          }
      }
  }
}

fn stream_quoted_value(
  previous: List(Event),
  first: ContentLine,
  rest: FileHead,
  quote: String,
  quoted_event: fn(Blame, String) -> Event,
) -> List(Event) {
  let splitter = sp.new([quote, "?>", "/>", ">"])
  let #(before, delimiter, _) =
    first.content
    |> string.slice(1, string.length(first.content) - 1)
    |> sp.split(splitter, _)
  let #(event, taken) = case delimiter == quote {
    True -> {
      let taken = quote <> before <> quote
      #(quoted_event(first.blame, before), taken)
    }
    False -> {
      let taken = quote <> before
      #(ValueMalformed(first.blame, taken), taken)
    }
  }
  event_stream_internal([event, ..previous], InsideOpeningTagExpectingNextKey, [
    advance_line(first, string.length(taken)),
    ..rest
  ])
}

fn stream_key_or_tag_end(
  previous: List(Event),
  first: ContentLine,
  rest: FileHead,
) -> List(Event) {
  let splitter = sp.new(["=", " ", "/>", "?>", ">"])
  let #(before, delimiter, _) = sp.split(splitter, first.content)
  case before {
    "" -> stream_tag_end(previous, first, rest, delimiter)
    _ -> {
      let event = case is_valid_key(before) {
        True -> Key(first.blame, before)
        False -> KeyMalformed(first.blame, before)
      }
      event_stream_internal(
        [event, ..previous],
        InsideOpeningTagExpectingNextAssignment,
        [advance_line(first, string.length(before)), ..rest],
      )
    }
  }
}

fn stream_tag_end(
  previous: List(Event),
  first: ContentLine,
  rest: FileHead,
  delimiter: String,
) -> List(Event) {
  let #(event, length) = case delimiter {
    "/>" -> #(TagEndSelfClosing(first.blame), 2)
    "?>" -> #(TagEndXMLVersion(first.blame), 2)
    ">" -> #(TagEndOrdinary(first.blame), 1)
    _ -> panic as "unexpected tag ending delimiter"
  }
  event_stream_internal([event, ..previous], OutsideTag, [
    advance_line(first, length),
    ..rest
  ])
}

fn input_lines_to_content_lines(lines: List(InputLine)) -> List(ContentLine) {
  list.map(lines, fn(line) {
    ContentLine(
      line.blame |> bl.advance(-line.indent),
      string.repeat(" ", line.indent) <> line.suffix,
    )
  })
}

/// Streams XML tokens from input lines.
///
/// All physical newlines are represented. Attribute values cannot span lines.
/// Comment delimiters in invalid positions do not change the streamer's state.
pub fn input_lines_streamer(lines: List(InputLine)) -> List(Event) {
  lines
  |> input_lines_to_content_lines
  |> event_stream_internal([], OutsideTag, _)
}
