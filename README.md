# vxml_parser

## Intro

VXML looks like this (text & tag nodes only; tag nodes can have attributes):

```
<> vxmlSample
    attr1 mom
    attr2 dad
    <>
        "this is a text child"
        "with two lines"
    <> html
        <> header
            charset utf-8
        <> body
            <> div
                <>
                    "some text"
                    "more text"
        <> goda
            <> child1
            <> child2
            <>
                "hello I am child3, but the first text child"
            <> child4
<> "yello"
```

## Development

```sh
gleam run   # Run the project
```

## What's there

Here is a dumbed-down version of the VXML type:

```
pub type VXML {
  V(
    tag: String,
    attributes: List(#(String, String)),
    children: List(VXML),
  )
  T(
    contents: List(String)
  )
}
```

This is gleam syntax for "a `VXML` is either a `V` or a `T`, where `V` has these fields and `T` has those fields".

In reality the type is a little more complex every parsed line carries a `Blame`:

```
pub type Blame {
  Blame(filename: String, line_no: Int, comments: List(String))
}
```

The parser doesn't really care about `Blame` but they are littered everywhere. (They will help us track source across multiple desugarings etc.)

So in fact, the true collection of types, with `Blame` everywhere, is like so:

```
pub type Blame {
  Blame(filename: String, line_no: Int, comments: List(String))
}

pub type BlamedContent {
  BlamedContent(blame: Blame, content: String)
}

pub type BlamedAttribute {
  BlamedAttribute(blame: Blame, key: String, value: String)
}

pub type VXML {
  V(
    blame: Blame,
    tag: String,
    attributes: List(BlamedAttribute),
    children: List(VXML),
  )
  T(blame: Blame, contents: List(BlamedContent))
}
```

We also have a parser error type:

```
pub type VXMLParseError {
  VXMLParseErrorEmptyTag(Blame)
  VXMLParseErrorIllegalTagCharacter(Blame, String, String)
  VXMLParseErrorIllegalAttributeKeyCharacter(Blame, String, String)
  VXMLParseErrorIndentationTooLarge(Blame, String)
  VXMLParseErrorIndentationNotMultipleOfFour(Blame, String)
  VXMLParseErrorTextMissing(Blame)
  VXMLParseErrorTextOutOfPlace(Blame, String)
  VXMLParseErrorCaretExpected(Blame, String)
  VXMLParseErrorConsecutiveTextNodes(Blame)
}
```
<!-- 
Parsing happens in two stages: a "flexible" (called "tentative" in code) stage that tries to recover from errors, but collects errors as a type of node in the tree, and a second "final" stage that will generate an error as soon as it sees an error in the output of the flexible stage.

The "flexible" stage generates a `List(TentativeVXML)`. The final stage generates a `VXML` object or an `VXMLParseError`. (Technically: a `Result(VXML, VXMLParseError)`.) -->

You can try `gleam run` and modifying `src/sample.vxml` to see what happens when errors are introduced to the file.