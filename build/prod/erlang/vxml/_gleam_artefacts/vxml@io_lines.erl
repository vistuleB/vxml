-module(vxml@io_lines).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/vxml/io_lines.gleam").
-export([normalize_line_endings/1, string_to_input_lines/3, read/2, input_line_to_string/1, input_lines_to_string/1, input_lines_to_output_lines/1, output_line_to_string/1, output_lines_to_string/1, input_lines_table/3, output_lines_table_lines_with/5, output_lines_table_lines/3, output_lines_table_with/5, output_lines_table/3]).
-export_type([input_line/0, output_line/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Line-based input and output helpers.\n"
    "\n"
    " `InputLine` and `OutputLine` pair indentation and text with `Blame`.\n"
    " They provide a small bridge between files/strings and VXML parsers or\n"
    " serializers.\n"
).

-type input_line() :: {input_line, vxml@blame:blame(), integer(), binary()}.

-type output_line() :: {output_line, vxml@blame:blame(), integer(), binary()}.

-file("src/vxml/io_lines.gleam", 27).
-spec spaces(integer()) -> binary().
spaces(I) ->
    gleam@string:repeat(<<" "/utf8>>, I).

-file("src/vxml/io_lines.gleam", 32).
?DOC(" Normalize CRLF and CR line endings to LF.\n").
-spec normalize_line_endings(binary()) -> binary().
normalize_line_endings(Source) ->
    _pipe = Source,
    _pipe@1 = gleam@string:replace(_pipe, <<"\r\n"/utf8>>, <<"\n"/utf8>>),
    gleam@string:replace(_pipe@1, <<"\r"/utf8>>, <<"\n"/utf8>>).

-file("src/vxml/io_lines.gleam", 43).
?DOC(" Convert a string to input lines, preserving source path and indentation.\n").
-spec string_to_input_lines(binary(), binary(), integer()) -> list(input_line()).
string_to_input_lines(Source, Path, Added_indentation) ->
    _pipe = Source,
    _pipe@1 = normalize_line_endings(_pipe),
    _pipe@2 = gleam@string:split(_pipe@1, <<"\n"/utf8>>),
    gleam@list:index_map(
        _pipe@2,
        fun(S, I) ->
            Suffix = gleam@string:trim_start(S),
            Indent = string:length(S) - string:length(Suffix),
            {input_line,
                {src, [], Path, I + 1, Indent + 1, movable},
                Indent + Added_indentation,
                Suffix}
        end
    ).

-file("src/vxml/io_lines.gleam", 70).
?DOC(" Read a file into input lines.\n").
-spec read(binary(), integer()) -> {ok, list(input_line())} |
    {error, simplifile:file_error()}.
read(Path, Added_indentation) ->
    _pipe = simplifile:read(Path),
    gleam@result:map(
        _pipe,
        fun(_capture) ->
            string_to_input_lines(_capture, Path, Added_indentation)
        end
    ).

-file("src/vxml/io_lines.gleam", 82).
-spec input_line_to_string(input_line()) -> binary().
input_line_to_string(Line) ->
    <<(spaces(erlang:element(3, Line)))/binary,
        (erlang:element(4, Line))/binary>>.

-file("src/vxml/io_lines.gleam", 86).
-spec input_lines_to_string(list(input_line())) -> binary().
input_lines_to_string(Lines) ->
    _pipe = Lines,
    _pipe@1 = gleam@list:map(_pipe, fun input_line_to_string/1),
    gleam@string:join(_pipe@1, <<"\n"/utf8>>).

-file("src/vxml/io_lines.gleam", 92).
-spec input_lines_to_output_lines(list(input_line())) -> list(output_line()).
input_lines_to_output_lines(Lines) ->
    _pipe = Lines,
    gleam@list:map(
        _pipe,
        fun(L) ->
            {output_line,
                erlang:element(2, L),
                erlang:element(3, L),
                erlang:element(4, L)}
        end
    ).

-file("src/vxml/io_lines.gleam", 101).
-spec output_line_to_string(output_line()) -> binary().
output_line_to_string(Line) ->
    <<(spaces(erlang:element(3, Line)))/binary,
        (erlang:element(4, Line))/binary>>.

-file("src/vxml/io_lines.gleam", 106).
?DOC(" Convert output lines to a newline-separated string.\n").
-spec output_lines_to_string(list(output_line())) -> binary().
output_lines_to_string(Lines) ->
    _pipe = Lines,
    _pipe@1 = gleam@list:map(_pipe, fun output_line_to_string/1),
    gleam@string:join(_pipe@1, <<"\n"/utf8>>).

-file("src/vxml/io_lines.gleam", 121).
-spec input_lines_table(list(input_line()), binary(), integer()) -> binary().
input_lines_table(Content, Banner, Indent) ->
    Margin = spaces(Indent),
    _pipe = Content,
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(C) ->
            {erlang:element(2, C),
                <<(spaces(erlang:element(3, C)))/binary,
                    (erlang:element(4, C))/binary>>}
        end
    ),
    _pipe@2 = vxml@blame:blamed_strings_annotated_table(
        _pipe@1,
        Banner,
        {blame_table_margin_columns_min_max, 48, 48},
        {blame_table_margin_columns_min_max, 30, 30}
    ),
    _pipe@3 = gleam@list:map(_pipe@2, fun(S) -> <<Margin/binary, S/binary>> end),
    gleam@string:join(_pipe@3, <<"\n"/utf8>>).

-file("src/vxml/io_lines.gleam", 138).
-spec output_lines_table_lines_with(
    list(output_line()),
    binary(),
    integer(),
    vxml@blame:blame_table_margin_columns_min_max(),
    vxml@blame:blame_table_margin_columns_min_max()
) -> list(binary()).
output_lines_table_lines_with(
    Content,
    Banner,
    Indent,
    Blame_digest_margin,
    Comments_margin
) ->
    Margin = spaces(Indent),
    _pipe = Content,
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(C) ->
            {erlang:element(2, C),
                <<(spaces(erlang:element(3, C)))/binary,
                    (erlang:element(4, C))/binary>>}
        end
    ),
    _pipe@2 = vxml@blame:blamed_strings_annotated_table(
        _pipe@1,
        Banner,
        Blame_digest_margin,
        Comments_margin
    ),
    gleam@list:map(_pipe@2, fun(S) -> <<Margin/binary, S/binary>> end).

-file("src/vxml/io_lines.gleam", 156).
-spec output_lines_table_lines(list(output_line()), binary(), integer()) -> list(binary()).
output_lines_table_lines(Content, Banner, Indent) ->
    output_lines_table_lines_with(
        Content,
        Banner,
        Indent,
        {blame_table_margin_columns_min_max, 48, 48},
        {blame_table_margin_columns_min_max, 30, 30}
    ).

-file("src/vxml/io_lines.gleam", 170).
-spec output_lines_table_with(
    list(output_line()),
    binary(),
    integer(),
    vxml@blame:blame_table_margin_columns_min_max(),
    vxml@blame:blame_table_margin_columns_min_max()
) -> binary().
output_lines_table_with(
    Content,
    Banner,
    Indent,
    Blame_digest_margin,
    Comments_margin
) ->
    _pipe = output_lines_table_lines_with(
        Content,
        Banner,
        Indent,
        Blame_digest_margin,
        Comments_margin
    ),
    gleam@string:join(_pipe, <<"\n"/utf8>>).

-file("src/vxml/io_lines.gleam", 187).
-spec output_lines_table(list(output_line()), binary(), integer()) -> binary().
output_lines_table(Content, Banner, Indent) ->
    _pipe = output_lines_table_lines(Content, Banner, Indent),
    gleam@string:join(_pipe, <<"\n"/utf8>>).
