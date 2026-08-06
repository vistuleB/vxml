-module(vxml).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/vxml.gleam").
-export([validate_key/1, validate_tag/1, annotate_blames/1, vxml_to_output_lines/1, vxmls_to_output_lines/1, vxml_to_string/1, vxmls_to_string/1, vxml_table/3, parse_input_lines/2, parse_string/3, parse_file/2, vxml_to_jsx_output_lines/3, vxmls_to_jsx_output_lines/3, vxml_to_jsx/3, vxmls_to_jsx/3, vxml_to_html_output_lines/3, vxmls_to_html_output_lines/3, parse_xml_input_lines/1, parse_xml/2, html_repair_escape_non_entity_ampersands/1, html_repair_expand_boolean_attrs/1, html_repair_close_void_tags/1, html_repair_remove_attrs_from_closing_tags/1, html_repair/1]).
-export_type([attr/0, line/0, v_x_m_l/0, bad_key/0, bad_tag/0, v_x_m_l_parse_error/0, v_x_m_l_parse_file_error/0, sticky_line/0, sticky_tree/0, x_m_l_streaming_parser_logical_unit/0, return/2, tri_way/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Core VXML tree types, parsers, validators, and serializers.\n"
    "\n"
    " VXML is a generic XML-like tree with two node kinds: element nodes (`V`)\n"
    " and text nodes (`T`). It can be serialized to a readable VXML text format,\n"
    " HTML, or JSX-like output. This module also includes XML/HTML parsing helpers.\n"
).

-type attr() :: {attr, vxml@blame:blame(), binary(), binary()}.

-type line() :: {line, vxml@blame:blame(), binary()}.

-type v_x_m_l() :: {v,
        vxml@blame:blame(),
        binary(),
        list(attr()),
        list(v_x_m_l())} |
    {t, vxml@blame:blame(), list(line())}.

-type bad_key() :: empty_key | {illegal_key_character, binary(), binary()}.

-type bad_tag() :: empty_tag | {malformed_tag, binary(), binary()}.

-type v_x_m_l_parse_error() :: {v_x_m_l_parse_error_attribute_assignment_missing,
        vxml@blame:blame(),
        binary()} |
    {v_x_m_l_parse_error_bad_tag, vxml@blame:blame(), bad_tag()} |
    {v_x_m_l_parse_error_bad_attribute_key, vxml@blame:blame(), bad_key()} |
    {v_x_m_l_parse_error_indentation_too_large, vxml@blame:blame(), binary()} |
    {v_x_m_l_parse_error_indentation_not_multiple_of_four,
        vxml@blame:blame(),
        binary()} |
    {v_x_m_l_parse_error_text_missing, vxml@blame:blame()} |
    {v_x_m_l_parse_error_text_no_closing_quote, vxml@blame:blame(), binary()} |
    {v_x_m_l_parse_error_text_no_opening_quote, vxml@blame:blame(), binary()} |
    {v_x_m_l_parse_error_text_out_of_place, vxml@blame:blame(), binary()} |
    {v_x_m_l_parse_error_caret_expected, vxml@blame:blame(), binary()} |
    {v_x_m_l_parse_error_non_unique_root, integer()}.

-type v_x_m_l_parse_file_error() :: {i_o_error, simplifile:file_error()} |
    {document_error, v_x_m_l_parse_error()}.

-type sticky_line() :: {sticky_line,
        vxml@blame:blame(),
        integer(),
        binary(),
        boolean(),
        boolean()}.

-type sticky_tree() :: {sticky_tree,
        list(sticky_line()),
        list(sticky_tree()),
        list(sticky_line())}.

-type x_m_l_streaming_parser_logical_unit() :: {x_m_l_streaming_parser_text,
        list(line())} |
    {x_m_l_streaming_parser_opening_tag,
        vxml@blame:blame(),
        binary(),
        list(attr())} |
    {x_m_l_streaming_parser_self_closing_tag,
        vxml@blame:blame(),
        binary(),
        list(attr())} |
    {x_m_l_streaming_parser_x_m_l_version,
        vxml@blame:blame(),
        binary(),
        list(attr())} |
    {x_m_l_streaming_parser_doctype,
        vxml@blame:blame(),
        binary(),
        list(attr()),
        boolean()} |
    {x_m_l_streaming_parser_closing_tag, vxml@blame:blame(), binary()} |
    {x_m_l_streaming_parser_comment, list(line())}.

-type return(HRO, HRP) :: {return, HRO} | {continuation, HRP}.

-type tri_way() :: no_more_events |
    {tag_end, xml_streamer:event(), list(xml_streamer:event())} |
    {something_else,
        xml_streamer:event(),
        list(xml_streamer:event()),
        boolean()}.

-file("src/vxml.gleam", 75).
-spec contains_chars(binary(), list(binary())) -> binary().
contains_chars(Thing, Substrings) ->
    case Substrings of
        [] ->
            <<""/utf8>>;

        [First | Rest] ->
            case gleam_stdlib:contains_string(Thing, First) of
                true ->
                    First;

                false ->
                    contains_chars(Thing, Rest)
            end
    end.

-file("src/vxml.gleam", 89).
?DOC(" Validate an attribute key for the VXML text format.\n").
-spec validate_key(binary()) -> {ok, binary()} | {error, bad_key()}.
validate_key(Key) ->
    case Key of
        <<""/utf8>> ->
            {error, empty_key};

        _ ->
            Bad_char = contains_chars(
                Key,
                [<<"."/utf8>>, <<" "/utf8>>, <<"\""/utf8>>, <<";"/utf8>>]
            ),
            case Bad_char =:= <<""/utf8>> of
                true ->
                    {ok, Key};

                false ->
                    {error, {illegal_key_character, Key, Bad_char}}
            end
    end.

-file("src/vxml.gleam", 103).
?DOC(" Validate an element tag for the VXML text format.\n").
-spec validate_tag(binary()) -> {ok, binary()} | {error, bad_tag()}.
validate_tag(Tag) ->
    case Tag =:= <<""/utf8>> of
        true ->
            {error, empty_tag};

        false ->
            Re@1 = case gleam@regexp:from_string(
                <<"^[A-Za-z_][A-Za-z0-9_.]*$"/utf8>>
            ) of
                {ok, Re} -> Re;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"vxml"/utf8>>,
                                function => <<"validate_tag"/utf8>>,
                                line => 107,
                                value => _assert_fail,
                                start => 2883,
                                'end' => 2934,
                                pattern_start => 2894,
                                pattern_end => 2900})
            end,
            case gleam@regexp:check(Re@1, Tag) of
                true ->
                    {ok, Tag};

                false ->
                    {error,
                        {malformed_tag,
                            Tag,
                            <<"^[A-Za-z_][A-Za-z0-9_.]*$"/utf8>>}}
            end
    end.

-file("src/vxml.gleam", 123).
-spec parse_text_lines_at_indent(integer(), list(vxml@io_lines:input_line())) -> {ok,
        {list(line()), list(vxml@io_lines:input_line())}} |
    {error, v_x_m_l_parse_error()}.
parse_text_lines_at_indent(Indent, Head) ->
    on:empty_nonempty(
        Head,
        fun() -> {ok, {[], Head}} end,
        fun(_use0, Rest) ->
            {input_line, Blame, Suffix_indent, Suffix} = _use0,
            on:true_false(
                Suffix =:= <<""/utf8>>,
                fun() -> parse_text_lines_at_indent(Indent, Rest) end,
                fun() ->
                    on:true_false(
                        Suffix_indent > Indent,
                        fun() ->
                            {error,
                                {v_x_m_l_parse_error_indentation_too_large,
                                    Blame,
                                    Suffix}}
                        end,
                        fun() ->
                            on:true_false(
                                Suffix_indent < Indent,
                                fun() -> {ok, {[], Head}} end,
                                fun() ->
                                    Suffix@1 = gleam@string:trim_end(Suffix),
                                    on:false_true(
                                        begin
                                            _pipe = Suffix@1,
                                            gleam_stdlib:string_starts_with(
                                                _pipe,
                                                <<"'"/utf8>>
                                            )
                                        end,
                                        fun() ->
                                            {error,
                                                {v_x_m_l_parse_error_text_no_opening_quote,
                                                    Blame,
                                                    Suffix@1}}
                                        end,
                                        fun() ->
                                            Content = begin
                                                _pipe@1 = Suffix@1,
                                                gleam@string:drop_start(
                                                    _pipe@1,
                                                    1
                                                )
                                            end,
                                            on:false_true(
                                                begin
                                                    _pipe@2 = Content,
                                                    gleam_stdlib:string_ends_with(
                                                        _pipe@2,
                                                        <<"'"/utf8>>
                                                    )
                                                end,
                                                fun() ->
                                                    {error,
                                                        {v_x_m_l_parse_error_text_no_closing_quote,
                                                            Blame,
                                                            Suffix@1}}
                                                end,
                                                fun() ->
                                                    Content@1 = begin
                                                        _pipe@3 = Content,
                                                        gleam@string:drop_end(
                                                            _pipe@3,
                                                            1
                                                        )
                                                    end,
                                                    Line = {line,
                                                        Blame,
                                                        Content@1},
                                                    on:ok(
                                                        parse_text_lines_at_indent(
                                                            Indent,
                                                            Rest
                                                        ),
                                                        fun(_use0@1) ->
                                                            {Lines, After} = _use0@1,
                                                            {ok,
                                                                {[Line | Lines],
                                                                    After}}
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/vxml.gleam", 166).
-spec parse_attributes_at_indent(integer(), list(vxml@io_lines:input_line())) -> {ok,
        {list(attr()), list(vxml@io_lines:input_line())}} |
    {error, v_x_m_l_parse_error()}.
parse_attributes_at_indent(Indent, Head) ->
    on:empty_nonempty(
        Head,
        fun() -> {ok, {[], Head}} end,
        fun(_use0, Rest) ->
            {input_line, Blame, Suffix_indent, Suffix} = _use0,
            on:true_false(
                Suffix =:= <<""/utf8>>,
                fun() -> parse_attributes_at_indent(Indent, Rest) end,
                fun() ->
                    on:true_false(
                        Suffix_indent > Indent,
                        fun() ->
                            {error,
                                {v_x_m_l_parse_error_indentation_too_large,
                                    Blame,
                                    Suffix}}
                        end,
                        fun() ->
                            on:true_false(
                                Suffix_indent < Indent,
                                fun() -> {ok, {[], Head}} end,
                                fun() ->
                                    on:true_false(
                                        begin
                                            _pipe = Suffix,
                                            gleam_stdlib:string_starts_with(
                                                _pipe,
                                                <<"<>"/utf8>>
                                            )
                                        end,
                                        fun() -> {ok, {[], Head}} end,
                                        fun() ->
                                            on:error_ok(
                                                begin
                                                    _pipe@1 = Suffix,
                                                    gleam@string:split_once(
                                                        _pipe@1,
                                                        <<"="/utf8>>
                                                    )
                                                end,
                                                fun(_) ->
                                                    {error,
                                                        {v_x_m_l_parse_error_attribute_assignment_missing,
                                                            Blame,
                                                            Suffix}}
                                                end,
                                                fun(_use0@1) ->
                                                    {Key, Val} = _use0@1,
                                                    on:error_ok(
                                                        validate_key(Key),
                                                        fun(E) ->
                                                            {error,
                                                                {v_x_m_l_parse_error_bad_attribute_key,
                                                                    Blame,
                                                                    E}}
                                                        end,
                                                        fun(_) ->
                                                            Val@1 = begin
                                                                _pipe@2 = Val,
                                                                gleam@string:trim(
                                                                    _pipe@2
                                                                )
                                                            end,
                                                            Attr = {attr,
                                                                Blame,
                                                                Key,
                                                                Val@1},
                                                            on:ok(
                                                                parse_attributes_at_indent(
                                                                    Indent,
                                                                    Rest
                                                                ),
                                                                fun(_use0@2) ->
                                                                    {Attrs,
                                                                        After} = _use0@2,
                                                                    Attrs@1 = [Attr |
                                                                        Attrs],
                                                                    {ok,
                                                                        {Attrs@1,
                                                                            After}}
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/vxml.gleam", 211).
-spec parse_nodes_at_indent(integer(), list(vxml@io_lines:input_line())) -> {ok,
        {list(v_x_m_l()), list(vxml@io_lines:input_line())}} |
    {error, v_x_m_l_parse_error()}.
parse_nodes_at_indent(Indent, Head) ->
    on:empty_nonempty(
        Head,
        fun() -> {ok, {[], Head}} end,
        fun(_use0, Rest) ->
            {input_line, Blame, Suffix_indent, Suffix} = _use0,
            on:true_false(
                Suffix =:= <<""/utf8>>,
                fun() -> parse_nodes_at_indent(Indent, Rest) end,
                fun() ->
                    on:true_false(
                        Suffix_indent > Indent,
                        fun() ->
                            {error,
                                {v_x_m_l_parse_error_indentation_too_large,
                                    Blame,
                                    Suffix}}
                        end,
                        fun() ->
                            on:true_false(
                                Suffix_indent < Indent,
                                fun() -> {ok, {[], Head}} end,
                                fun() ->
                                    on:false_true(
                                        begin
                                            _pipe = Suffix,
                                            gleam_stdlib:string_starts_with(
                                                _pipe,
                                                <<"<>"/utf8>>
                                            )
                                        end,
                                        fun() ->
                                            {error,
                                                {v_x_m_l_parse_error_caret_expected,
                                                    Blame,
                                                    Suffix}}
                                        end,
                                        fun() ->
                                            Tag = begin
                                                _pipe@1 = Suffix,
                                                _pipe@2 = gleam@string:drop_start(
                                                    _pipe@1,
                                                    2
                                                ),
                                                gleam@string:trim(_pipe@2)
                                            end,
                                            case Tag of
                                                <<""/utf8>> ->
                                                    on:ok(
                                                        parse_text_lines_at_indent(
                                                            Indent + 2,
                                                            Rest
                                                        ),
                                                        fun(_use0@1) ->
                                                            {Lines, After} = _use0@1,
                                                            case Lines of
                                                                [] ->
                                                                    {error,
                                                                        {v_x_m_l_parse_error_text_missing,
                                                                            Blame}};

                                                                _ ->
                                                                    Node = {t,
                                                                        Blame,
                                                                        Lines},
                                                                    on:ok(
                                                                        parse_nodes_at_indent(
                                                                            Indent,
                                                                            After
                                                                        ),
                                                                        fun(
                                                                            _use0@2
                                                                        ) ->
                                                                            {Nodes,
                                                                                After@1} = _use0@2,
                                                                            {ok,
                                                                                {[Node |
                                                                                        Nodes],
                                                                                    After@1}}
                                                                        end
                                                                    )
                                                            end
                                                        end
                                                    );

                                                _ ->
                                                    on:error_ok(
                                                        validate_tag(Tag),
                                                        fun(E) ->
                                                            {error,
                                                                {v_x_m_l_parse_error_bad_tag,
                                                                    Blame,
                                                                    E}}
                                                        end,
                                                        fun(_) ->
                                                            on:ok(
                                                                parse_attributes_at_indent(
                                                                    Indent + 2,
                                                                    Rest
                                                                ),
                                                                fun(_use0@3) ->
                                                                    {Attrs,
                                                                        After@2} = _use0@3,
                                                                    on:ok(
                                                                        parse_nodes_at_indent(
                                                                            Indent
                                                                            + 2,
                                                                            After@2
                                                                        ),
                                                                        fun(
                                                                            _use0@4
                                                                        ) ->
                                                                            {Children,
                                                                                After@3} = _use0@4,
                                                                            Node@1 = {v,
                                                                                begin
                                                                                    _pipe@3 = Blame,
                                                                                    vxml@blame:set_anchored(
                                                                                        _pipe@3
                                                                                    )
                                                                                end,
                                                                                Tag,
                                                                                Attrs,
                                                                                Children},
                                                                            on:ok(
                                                                                parse_nodes_at_indent(
                                                                                    Indent,
                                                                                    After@3
                                                                                ),
                                                                                fun(
                                                                                    _use0@5
                                                                                ) ->
                                                                                    {Nodes@1,
                                                                                        After@4} = _use0@5,
                                                                                    {ok,
                                                                                        {[Node@1 |
                                                                                                Nodes@1],
                                                                                            After@4}}
                                                                                end
                                                                            )
                                                                        end
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                            end
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/vxml.gleam", 281).
-spec annotate_blames(v_x_m_l()) -> v_x_m_l().
annotate_blames(Vxml) ->
    case Vxml of
        {t, Blame, Lines} ->
            {t,
                begin
                    _pipe = Blame,
                    vxml@blame:prepend_comment(_pipe, <<"T"/utf8>>)
                end,
                gleam@list:index_map(
                    Lines,
                    fun(Line, I) ->
                        {line,
                            begin
                                _pipe@1 = erlang:element(2, Line),
                                vxml@blame:prepend_comment(
                                    _pipe@1,
                                    <<<<"T > Line("/utf8,
                                            (gleam@string:inspect(I + 1))/binary>>/binary,
                                        ")"/utf8>>
                                )
                            end,
                            erlang:element(3, Line)}
                    end
                )};

        {v, Blame@1, Tag, Attrs, Children} ->
            {v,
                begin
                    _pipe@2 = Blame@1,
                    vxml@blame:prepend_comment(_pipe@2, <<"V"/utf8>>)
                end,
                Tag,
                gleam@list:index_map(
                    Attrs,
                    fun(Attr, I@1) ->
                        {attr,
                            begin
                                _pipe@3 = erlang:element(2, Attr),
                                vxml@blame:prepend_comment(
                                    _pipe@3,
                                    <<<<"Attr("/utf8,
                                            (gleam@string:inspect(I@1 + 1))/binary>>/binary,
                                        ")"/utf8>>
                                )
                            end,
                            erlang:element(3, Attr),
                            erlang:element(4, Attr)}
                    end
                ),
                gleam@list:map(Children, fun annotate_blames/1)}
    end.

-file("src/vxml.gleam", 312).
-spec delimit(binary()) -> binary().
delimit(S) ->
    <<<<"'"/utf8, S/binary>>/binary, "'"/utf8>>.

-file("src/vxml.gleam", 316).
-spec vxml_to_output_lines_internal(v_x_m_l(), integer()) -> list(vxml@io_lines:output_line()).
vxml_to_output_lines_internal(Vxml, Indentation) ->
    case Vxml of
        {t, Blame, Lines} ->
            [{output_line, Blame, Indentation, <<"<>"/utf8>>} |
                gleam@list:map(
                    Lines,
                    fun(Line) ->
                        {output_line,
                            erlang:element(2, Line),
                            Indentation + 2,
                            delimit(erlang:element(3, Line))}
                    end
                )];

        {v, Blame@1, Tag, Attrs, Children} ->
            [{output_line, Blame@1, Indentation, <<"<> "/utf8, Tag/binary>>} |
                lists:append(
                    gleam@list:map(
                        Attrs,
                        fun(Attr) ->
                            {output_line,
                                erlang:element(2, Attr),
                                Indentation + 2,
                                <<<<(erlang:element(3, Attr))/binary, "="/utf8>>/binary,
                                    (erlang:element(4, Attr))/binary>>}
                        end
                    ),
                    begin
                        _pipe = Children,
                        _pipe@1 = gleam@list:map(
                            _pipe,
                            fun(_capture) ->
                                vxml_to_output_lines_internal(
                                    _capture,
                                    Indentation + 2
                                )
                            end
                        ),
                        lists:append(_pipe@1)
                    end
                )]
    end.

-file("src/vxml.gleam", 353).
?DOC(" Serialize one VXML node to VXML text-format output lines.\n").
-spec vxml_to_output_lines(v_x_m_l()) -> list(vxml@io_lines:output_line()).
vxml_to_output_lines(Vxml) ->
    vxml_to_output_lines_internal(Vxml, 0).

-file("src/vxml.gleam", 357).
-spec vxmls_to_output_lines(list(v_x_m_l())) -> list(vxml@io_lines:output_line()).
vxmls_to_output_lines(Vxmls) ->
    _pipe = Vxmls,
    _pipe@1 = gleam@list:map(_pipe, fun vxml_to_output_lines/1),
    lists:append(_pipe@1).

-file("src/vxml.gleam", 368).
?DOC(" Serialize one VXML node to the VXML text format.\n").
-spec vxml_to_string(v_x_m_l()) -> binary().
vxml_to_string(Vxml) ->
    _pipe = Vxml,
    _pipe@1 = vxml_to_output_lines(_pipe),
    vxml@io_lines:output_lines_to_string(_pipe@1).

-file("src/vxml.gleam", 374).
-spec vxmls_to_string(list(v_x_m_l())) -> binary().
vxmls_to_string(Vxmls) ->
    _pipe = Vxmls,
    _pipe@1 = vxmls_to_output_lines(_pipe),
    vxml@io_lines:output_lines_to_string(_pipe@1).

-file("src/vxml.gleam", 384).
-spec vxml_table(v_x_m_l(), binary(), integer()) -> binary().
vxml_table(Vxml, Banner, Indent) ->
    _pipe = Vxml,
    _pipe@1 = vxml_to_output_lines(_pipe),
    vxml@io_lines:output_lines_table(_pipe@1, Banner, Indent).

-file("src/vxml.gleam", 390).
-spec parse_input_lines(list(vxml@io_lines:input_line()), boolean()) -> {ok,
        list(v_x_m_l())} |
    {error, v_x_m_l_parse_error()}.
parse_input_lines(Lines, Unique_root) ->
    on:ok(
        parse_nodes_at_indent(0, Lines),
        fun(_use0) ->
            {Vxmls, After} = _use0,
            _assert_subject = [],
            case After =:= _assert_subject of
                true -> nil;
                false -> erlang:error(#{gleam_error => assert,
                        message => <<"Assertion failed."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml"/utf8>>,
                        function => <<"parse_input_lines"/utf8>>,
                        line => 395,
                        kind => binary_operator,
                        operator => '==',
                        left => #{kind => expression,
                            value => After,
                            start => 10559,
                            'end' => 10564
                            },
                        right => #{kind => literal,
                            value => _assert_subject,
                            start => 10568,
                            'end' => 10570
                            },
                        start => 10552,
                        'end' => 10570,
                        expression_start => 10559})
            end,
            case Unique_root of
                false ->
                    {ok, Vxmls};

                true ->
                    case Vxmls of
                        [_] ->
                            {ok, Vxmls};

                        _ ->
                            {error,
                                {v_x_m_l_parse_error_non_unique_root,
                                    begin
                                        _pipe = Vxmls,
                                        erlang:length(_pipe)
                                    end}}
                    end
            end
        end
    ).

-file("src/vxml.gleam", 411).
?DOC(" Parse a string containing the VXML text format.\n").
-spec parse_string(binary(), binary(), boolean()) -> {ok, list(v_x_m_l())} |
    {error, v_x_m_l_parse_error()}.
parse_string(Source, Filename, Unique_root) ->
    _pipe = Source,
    _pipe@1 = vxml@io_lines:string_to_input_lines(_pipe, Filename, 0),
    parse_input_lines(_pipe@1, Unique_root).

-file("src/vxml.gleam", 426).
?DOC(" Parse a file containing the VXML text format.\n").
-spec parse_file(binary(), boolean()) -> {ok, list(v_x_m_l())} |
    {error, v_x_m_l_parse_file_error()}.
parse_file(Path, Unique_root) ->
    on:error_ok(
        simplifile:read(Path),
        fun(Io_error) -> {error, {i_o_error, Io_error}} end,
        fun(Contents) -> _pipe = parse_string(Contents, Path, Unique_root),
            gleam@result:map_error(_pipe, fun(E) -> {document_error, E} end) end
    ).

-file("src/vxml.gleam", 440).
-spec jsx_string_processor(binary(), gleam@regexp:regexp()) -> binary().
jsx_string_processor(Content, Ampersand_re) ->
    _pipe = Content,
    _pipe@1 = gleam_regexp_ffi:replace(Ampersand_re, _pipe, <<"&amp;"/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"{"/utf8>>, <<"&#123;"/utf8>>),
    _pipe@3 = gleam@string:replace(_pipe@2, <<"}"/utf8>>, <<"&#125;"/utf8>>),
    _pipe@4 = gleam@string:replace(_pipe@3, <<"<"/utf8>>, <<"&lt;"/utf8>>),
    gleam@string:replace(_pipe@4, <<">"/utf8>>, <<"&gt;"/utf8>>).

-file("src/vxml.gleam", 452).
-spec jsx_key_val(attr(), gleam@regexp:regexp()) -> binary().
jsx_key_val(Attr, Ampersand_re) ->
    Val = begin
        _pipe = gleam@string:trim(erlang:element(4, Attr)),
        jsx_string_processor(_pipe, Ampersand_re)
    end,
    case ((Val =:= <<"false"/utf8>>) orelse (Val =:= <<"true"/utf8>>)) orelse gleam@result:is_ok(
        gleam_stdlib:parse_int(Val)
    ) of
        true ->
            <<<<<<(erlang:element(3, Attr))/binary, "={"/utf8>>/binary,
                    Val/binary>>/binary,
                "}"/utf8>>;

        false ->
            <<<<<<(erlang:element(3, Attr))/binary, "=\""/utf8>>/binary,
                    Val/binary>>/binary,
                "\""/utf8>>
    end.

-file("src/vxml.gleam", 460).
-spec jsx_attr_output_line(attr(), integer(), gleam@regexp:regexp()) -> vxml@io_lines:output_line().
jsx_attr_output_line(Attr, Indent, Ampersand_re) ->
    {output_line,
        erlang:element(2, Attr),
        Indent,
        jsx_key_val(Attr, Ampersand_re)}.

-file("src/vxml.gleam", 472).
-spec jsx_tag_close_output_lines(vxml@blame:blame(), binary(), integer()) -> list(vxml@io_lines:output_line()).
jsx_tag_close_output_lines(Blame, Tag, Indent) ->
    [{output_line,
            Blame,
            Indent,
            <<<<"</"/utf8, Tag/binary>>/binary, ">"/utf8>>}].

-file("src/vxml.gleam", 480).
-spec jsx_tag_open_output_lines(
    vxml@blame:blame(),
    binary(),
    integer(),
    binary(),
    binary(),
    list(attr()),
    gleam@regexp:regexp(),
    integer()
) -> list(vxml@io_lines:output_line()).
jsx_tag_open_output_lines(
    Blame,
    Tag,
    Indent,
    Closing_same_line,
    Closing_different_line,
    Attrs,
    Ampersand_re,
    Indentation
) ->
    case Attrs of
        [] ->
            [{output_line,
                    Blame,
                    Indent,
                    <<<<"<"/utf8, Tag/binary>>/binary,
                        Closing_same_line/binary>>}];

        [First] ->
            [{output_line,
                    Blame,
                    Indent,
                    <<<<<<<<"<"/utf8, Tag/binary>>/binary, " "/utf8>>/binary,
                            (jsx_key_val(First, Ampersand_re))/binary>>/binary,
                        Closing_same_line/binary>>}];

        _ ->
            _pipe@1 = [[{output_line, Blame, Indent, <<"<"/utf8, Tag/binary>>}],
                begin
                    _pipe = Attrs,
                    gleam@list:map(
                        _pipe,
                        fun(_capture) ->
                            jsx_attr_output_line(
                                _capture,
                                Indent + Indentation,
                                Ampersand_re
                            )
                        end
                    )
                end,
                [{output_line, Blame, Indent, Closing_different_line}]],
            lists:append(_pipe@1)
    end.

-file("src/vxml.gleam", 531).
-spec bool_2_jsx_space(boolean()) -> binary().
bool_2_jsx_space(B) ->
    case B of
        true ->
            <<"{\" \"}"/utf8>>;

        false ->
            <<""/utf8>>
    end.

-file("src/vxml.gleam", 538).
-spec vxml_to_jsx_output_lines_internal(
    v_x_m_l(),
    integer(),
    gleam@regexp:regexp(),
    integer()
) -> list(vxml@io_lines:output_line()).
vxml_to_jsx_output_lines_internal(Vxml, Indent, Ampersand_re, Indentation) ->
    case Vxml of
        {t, _, Lines} ->
            N = erlang:length(Lines),
            _pipe = Lines,
            gleam@list:index_map(
                _pipe,
                fun(T, I) ->
                    {output_line,
                        erlang:element(2, T),
                        Indent,
                        begin
                            Content = jsx_string_processor(
                                erlang:element(3, T),
                                Ampersand_re
                            ),
                            Start = begin
                                _pipe@1 = ((I =:= 0) andalso (gleam_stdlib:string_starts_with(
                                    Content,
                                    <<" "/utf8>>
                                )
                                orelse gleam@string:is_empty(Content))),
                                bool_2_jsx_space(_pipe@1)
                            end,
                            End = begin
                                _pipe@2 = ((I =:= (N - 1)) andalso (gleam_stdlib:string_ends_with(
                                    Content,
                                    <<" "/utf8>>
                                )
                                orelse gleam@string:is_empty(Content))),
                                bool_2_jsx_space(_pipe@2)
                            end,
                            <<<<Start/binary, Content/binary>>/binary,
                                End/binary>>
                        end}
                end
            );

        {v, Blame, Tag, Attrs, Children} ->
            case gleam@list:is_empty(Children) of
                false ->
                    _pipe@5 = [jsx_tag_open_output_lines(
                            Blame,
                            Tag,
                            Indent,
                            <<">"/utf8>>,
                            <<">"/utf8>>,
                            Attrs,
                            Ampersand_re,
                            Indentation
                        ),
                        begin
                            _pipe@3 = Children,
                            _pipe@4 = gleam@list:map(
                                _pipe@3,
                                fun(_capture) ->
                                    vxml_to_jsx_output_lines_internal(
                                        _capture,
                                        Indent + Indentation,
                                        Ampersand_re,
                                        Indentation
                                    )
                                end
                            ),
                            lists:append(_pipe@4)
                        end,
                        jsx_tag_close_output_lines(Blame, Tag, Indent)],
                    lists:append(_pipe@5);

                true ->
                    jsx_tag_open_output_lines(
                        Blame,
                        Tag,
                        Indent,
                        <<" />"/utf8>>,
                        <<"/>"/utf8>>,
                        Attrs,
                        Ampersand_re,
                        Indentation
                    )
            end
    end.

-file("src/vxml.gleam", 616).
-spec vxml_to_jsx_output_lines(v_x_m_l(), integer(), integer()) -> list(vxml@io_lines:output_line()).
vxml_to_jsx_output_lines(Vxml, Starting_indent, Indentation) ->
    Ampersand_re@1 = case gleam@regexp:from_string(
        <<"&(?!(?:[a-zA-Z]{2,6};|#x[a-f\\d]{1,6};|#\\d{2,6};))"/utf8>>
    ) of
        {ok, Ampersand_re} -> Ampersand_re;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml"/utf8>>,
                        function => <<"vxml_to_jsx_output_lines"/utf8>>,
                        line => 621,
                        value => _assert_fail,
                        start => 15933,
                        'end' => 16004,
                        pattern_start => 15944,
                        pattern_end => 15960})
    end,
    vxml_to_jsx_output_lines_internal(
        Vxml,
        Starting_indent,
        Ampersand_re@1,
        Indentation
    ).

-file("src/vxml.gleam", 630).
-spec vxmls_to_jsx_output_lines(list(v_x_m_l()), integer(), integer()) -> list(vxml@io_lines:output_line()).
vxmls_to_jsx_output_lines(Vxmls, Starting_indent, Indentation) ->
    Ampersand_re@1 = case gleam@regexp:from_string(
        <<"&(?!(?:[a-zA-Z]{2,6};|#x[a-f\\d]{1,6};|#\\d{2,6};))"/utf8>>
    ) of
        {ok, Ampersand_re} -> Ampersand_re;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml"/utf8>>,
                        function => <<"vxmls_to_jsx_output_lines"/utf8>>,
                        line => 635,
                        value => _assert_fail,
                        start => 16240,
                        'end' => 16311,
                        pattern_start => 16251,
                        pattern_end => 16267})
    end,
    _pipe = Vxmls,
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(_capture) ->
            vxml_to_jsx_output_lines_internal(
                _capture,
                Starting_indent,
                Ampersand_re@1,
                Indentation
            )
        end
    ),
    lists:append(_pipe@1).

-file("src/vxml.gleam", 650).
-spec vxml_to_jsx(v_x_m_l(), integer(), integer()) -> binary().
vxml_to_jsx(Vxml, Starting_indent, Indentation) ->
    _pipe = Vxml,
    _pipe@1 = vxml_to_jsx_output_lines(_pipe, Starting_indent, Indentation),
    vxml@io_lines:output_lines_to_string(_pipe@1).

-file("src/vxml.gleam", 660).
-spec vxmls_to_jsx(list(v_x_m_l()), integer(), integer()) -> binary().
vxmls_to_jsx(Vxmls, Starting_indent, Indentation) ->
    _pipe = Vxmls,
    _pipe@1 = vxmls_to_jsx_output_lines(_pipe, Starting_indent, Indentation),
    vxml@io_lines:output_lines_to_string(_pipe@1).

-file("src/vxml.gleam", 670).
-spec html_string_processor(binary(), gleam@regexp:regexp()) -> binary().
html_string_processor(Content, Ampersand_re) ->
    _pipe = Content,
    _pipe@1 = gleam_regexp_ffi:replace(Ampersand_re, _pipe, <<"&amp;"/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"<"/utf8>>, <<"&lt;"/utf8>>),
    gleam@string:replace(_pipe@2, <<">"/utf8>>, <<"&gt;"/utf8>>).

-file("src/vxml.gleam", 698).
-spec sticky_2_blamed(sticky_line()) -> vxml@io_lines:output_line().
sticky_2_blamed(Stickie) ->
    {output_line,
        erlang:element(2, Stickie),
        erlang:element(3, Stickie),
        erlang:element(4, Stickie)}.

-file("src/vxml.gleam", 702).
-spec concat_sticky_lines_internal(
    list(sticky_line()),
    sticky_line(),
    list(sticky_line())
) -> list(sticky_line()).
concat_sticky_lines_internal(Already_stuck, Working_on, Upcoming) ->
    case Upcoming of
        [] ->
            _pipe = [Working_on | Already_stuck],
            lists:reverse(_pipe);

        [Next | Rest] ->
            case erlang:element(6, Working_on) andalso erlang:element(5, Next) of
                true ->
                    concat_sticky_lines_internal(
                        Already_stuck,
                        {sticky_line,
                            erlang:element(2, Working_on),
                            erlang:element(3, Working_on),
                            <<(erlang:element(4, Working_on))/binary,
                                (erlang:element(4, Next))/binary>>,
                            erlang:element(5, Working_on),
                            erlang:element(6, Next)},
                        Rest
                    );

                false ->
                    concat_sticky_lines_internal(
                        [Working_on | Already_stuck],
                        Next,
                        Rest
                    )
            end
    end.

-file("src/vxml.gleam", 734).
-spec concat_sticky_lines(list(sticky_line())) -> list(sticky_line()).
concat_sticky_lines(Lines) ->
    case Lines of
        [] ->
            [];

        [First | Rest] ->
            concat_sticky_lines_internal([], First, Rest)
    end.

-file("src/vxml.gleam", 741).
-spec pour(list(HTH), list(HTH)) -> list(HTH).
pour(To, From) ->
    case From of
        [] ->
            To;

        [First | Rest] ->
            pour([First | To], Rest)
    end.

-file("src/vxml.gleam", 762).
-spec sticky_tree_2_sticky_lines(list(sticky_line()), sticky_tree()) -> list(sticky_line()).
sticky_tree_2_sticky_lines(Already_stuck, Subtree) ->
    {sticky_tree, Opening_lines, Children, Closing_lines} = Subtree,
    Already_stuck@1 = pour(Already_stuck, Opening_lines),
    Already_stuck@2 = sticky_trees_2_sticky_lines(Already_stuck@1, Children),
    pour(Already_stuck@2, Closing_lines).

-file("src/vxml.gleam", 748).
-spec sticky_trees_2_sticky_lines(list(sticky_line()), list(sticky_tree())) -> list(sticky_line()).
sticky_trees_2_sticky_lines(Already_stuck, Subtrees) ->
    case Subtrees of
        [] ->
            Already_stuck;

        [First | Rest] ->
            sticky_trees_2_sticky_lines(
                sticky_tree_2_sticky_lines(Already_stuck, First),
                Rest
            )
    end.

-file("src/vxml.gleam", 772).
-spec attrs_to_sticky_lines(list(attr()), integer(), boolean()) -> list(sticky_line()).
attrs_to_sticky_lines(Attrs, Indent, Inline) ->
    Space = case Inline of
        true ->
            <<" "/utf8>>;

        false ->
            <<""/utf8>>
    end,
    _pipe = Attrs,
    gleam@list:map(
        _pipe,
        fun(T) ->
            {sticky_line,
                erlang:element(2, T),
                Indent,
                <<<<<<<<Space/binary, (erlang:element(3, T))/binary>>/binary,
                            "=\""/utf8>>/binary,
                        (erlang:element(4, T))/binary>>/binary,
                    "\""/utf8>>,
                Inline,
                Inline}
        end
    ).

-file("src/vxml.gleam", 799).
-spec opening_tag_to_sticky_lines(v_x_m_l(), integer(), integer(), boolean()) -> list(sticky_line()).
opening_tag_to_sticky_lines(T, Indent, Spaces, Pre) ->
    {Blame@1, Tag@1, Attrs@1} = case T of
        {v, Blame, Tag, Attrs, _} -> {Blame, Tag, Attrs};
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml"/utf8>>,
                        function => <<"opening_tag_to_sticky_lines"/utf8>>,
                        line => 805,
                        value => _assert_fail,
                        start => 19996,
                        'end' => 20034,
                        pattern_start => 20007,
                        pattern_end => 20030})
    end,
    Indent@1 = case Pre of
        true ->
            0;

        false ->
            Indent
    end,
    Sticky_outside = gleam@list:contains(
        [<<"a"/utf8>>,
            <<"span"/utf8>>,
            <<"i"/utf8>>,
            <<"b"/utf8>>,
            <<"strong"/utf8>>,
            <<"em"/utf8>>,
            <<"code"/utf8>>,
            <<"tt"/utf8>>,
            <<"br"/utf8>>,
            <<"img"/utf8>>],
        Tag@1
    ),
    Sticky_inside = erlang:length(Attrs@1) =< 1,
    lists:append(
        [[{sticky_line,
                    Blame@1,
                    Indent@1,
                    <<"<"/utf8, Tag@1/binary>>,
                    Sticky_outside,
                    Sticky_inside}],
            attrs_to_sticky_lines(Attrs@1, Indent@1 + Spaces, Sticky_inside),
            [{sticky_line,
                    Blame@1,
                    Indent@1,
                    <<">"/utf8>>,
                    Sticky_inside,
                    Sticky_outside}]]
    ).

-file("src/vxml.gleam", 819).
-spec closing_tag_to_sticky_lines(v_x_m_l(), integer(), boolean()) -> list(sticky_line()).
closing_tag_to_sticky_lines(T, Indent, Pre) ->
    {Blame@1, Tag@1} = case T of
        {v, Blame, Tag, _, _} -> {Blame, Tag};
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml"/utf8>>,
                        function => <<"closing_tag_to_sticky_lines"/utf8>>,
                        line => 824,
                        value => _assert_fail,
                        start => 20533,
                        'end' => 20567,
                        pattern_start => 20544,
                        pattern_end => 20563})
    end,
    Indent@1 = case Pre of
        true ->
            0;

        false ->
            Indent
    end,
    Sticky_outside = gleam@list:contains(
        [<<"a"/utf8>>,
            <<"span"/utf8>>,
            <<"i"/utf8>>,
            <<"b"/utf8>>,
            <<"strong"/utf8>>,
            <<"em"/utf8>>,
            <<"code"/utf8>>,
            <<"tt"/utf8>>,
            <<"br"/utf8>>,
            <<"img"/utf8>>],
        Tag@1
    ),
    [{sticky_line,
            Blame@1,
            Indent@1,
            <<<<"</"/utf8, Tag@1/binary>>/binary, ">"/utf8>>,
            Sticky_outside,
            Sticky_outside}].

-file("src/vxml.gleam", 878).
-spec t_very_fancy_sticky_lines_post_processing(list(sticky_line())) -> list(sticky_line()).
t_very_fancy_sticky_lines_post_processing(Lines) ->
    Trim_start = fun(Sticky) ->
        {sticky_line,
            erlang:element(2, Sticky),
            erlang:element(3, Sticky),
            gleam@string:trim_start(erlang:element(4, Sticky)),
            erlang:element(5, Sticky),
            erlang:element(6, Sticky)}
    end,
    Trim_end = fun(Sticky@1) ->
        {sticky_line,
            erlang:element(2, Sticky@1),
            erlang:element(3, Sticky@1),
            gleam@string:trim_end(erlang:element(4, Sticky@1)),
            erlang:element(5, Sticky@1),
            erlang:element(6, Sticky@1)}
    end,
    {First@1, Rest@1} = case Lines of
        [First | Rest] -> {First, Rest};
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml"/utf8>>,
                        function => <<"t_very_fancy_sticky_lines_post_processing"/utf8>>,
                        line => 892,
                        value => _assert_fail,
                        start => 22586,
                        'end' => 22620,
                        pattern_start => 22597,
                        pattern_end => 22612})
    end,
    case gleam_stdlib:string_starts_with(
        erlang:element(4, First@1),
        <<" "/utf8>>
    ) of
        true ->
            _assert_subject = erlang:element(5, First@1),
            case _assert_subject =:= false of
                true -> nil;
                false -> erlang:error(#{gleam_error => assert,
                        message => <<"Assertion failed."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml"/utf8>>,
                        function => <<"t_very_fancy_sticky_lines_post_processing"/utf8>>,
                        line => 898,
                        kind => binary_operator,
                        operator => '==',
                        left => #{kind => expression,
                            value => _assert_subject,
                            start => 22826,
                            'end' => 22844
                            },
                        right => #{kind => literal,
                            value => false,
                            start => 22848,
                            'end' => 22853
                            },
                        start => 22819,
                        'end' => 22853,
                        expression_start => 22826})
            end,
            t_very_fancy_sticky_lines_post_processing(
                [Trim_start(First@1) | Rest@1]
            );

        false ->
            case erlang:element(4, First@1) =:= <<""/utf8>> of
                true ->
                    case gleam@list:is_empty(Rest@1) of
                        false ->
                            New_first@1 = case gleam@list:first(Rest@1) of
                                {ok, New_first} -> New_first;
                                _assert_fail@1 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                file => <<?FILEPATH/utf8>>,
                                                module => <<"vxml"/utf8>>,
                                                function => <<"t_very_fancy_sticky_lines_post_processing"/utf8>>,
                                                line => 908,
                                                value => _assert_fail@1,
                                                start => 23204,
                                                'end' => 23247,
                                                pattern_start => 23215,
                                                pattern_end => 23228})
                            end,
                            _assert_subject@1 = erlang:element(5, New_first@1),
                            case _assert_subject@1 =:= false of
                                true -> nil;
                                false -> erlang:error(#{gleam_error => assert,
                                        message => <<"Assertion failed."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"vxml"/utf8>>,
                                        function => <<"t_very_fancy_sticky_lines_post_processing"/utf8>>,
                                        line => 909,
                                        kind => binary_operator,
                                        operator => '==',
                                        left => #{kind => expression,
                                            value => _assert_subject@1,
                                            start => 23269,
                                            'end' => 23291
                                            },
                                        right => #{kind => literal,
                                            value => false,
                                            start => 23295,
                                            'end' => 23300
                                            },
                                        start => 23262,
                                        'end' => 23300,
                                        expression_start => 23269})
                            end,
                            t_very_fancy_sticky_lines_post_processing(Rest@1);

                        true ->
                            [{sticky_line,
                                    erlang:element(2, First@1),
                                    erlang:element(3, First@1),
                                    erlang:element(4, First@1),
                                    false,
                                    true}]
                    end;

                false ->
                    {Last@1, Init@1} = case begin
                        _pipe = Lines,
                        lists:reverse(_pipe)
                    end of
                        [Last | Init] -> {Last, Init};
                        _assert_fail@2 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"vxml"/utf8>>,
                                        function => <<"t_very_fancy_sticky_lines_post_processing"/utf8>>,
                                        line => 919,
                                        value => _assert_fail@2,
                                        start => 23674,
                                        'end' => 23723,
                                        pattern_start => 23685,
                                        pattern_end => 23699})
                    end,
                    case gleam_stdlib:string_ends_with(
                        erlang:element(4, Last@1),
                        <<" "/utf8>>
                    ) of
                        true ->
                            _assert_subject@2 = erlang:element(6, Last@1),
                            case _assert_subject@2 =:= false of
                                true -> nil;
                                false -> erlang:error(#{gleam_error => assert,
                                        message => <<"Assertion failed."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"vxml"/utf8>>,
                                        function => <<"t_very_fancy_sticky_lines_post_processing"/utf8>>,
                                        line => 924,
                                        kind => binary_operator,
                                        operator => '==',
                                        left => #{kind => expression,
                                            value => _assert_subject@2,
                                            start => 23948,
                                            'end' => 23963
                                            },
                                        right => #{kind => literal,
                                            value => false,
                                            start => 23967,
                                            'end' => 23972
                                            },
                                        start => 23941,
                                        'end' => 23972,
                                        expression_start => 23948})
                            end,
                            t_very_fancy_sticky_lines_post_processing(
                                begin
                                    _pipe@1 = [Trim_end(Last@1) | Init@1],
                                    lists:reverse(_pipe@1)
                                end
                            );

                        false ->
                            case erlang:element(4, Last@1) =:= <<""/utf8>> of
                                true ->
                                    New_last@1 = case Init@1 of
                                        [New_last | _] -> New_last;
                                        _assert_fail@3 ->
                                            erlang:error(
                                                    #{gleam_error => let_assert,
                                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                        file => <<?FILEPATH/utf8>>,
                                                        module => <<"vxml"/utf8>>,
                                                        function => <<"t_very_fancy_sticky_lines_post_processing"/utf8>>,
                                                        line => 932,
                                                        value => _assert_fail@3,
                                                        start => 24225,
                                                        'end' => 24257,
                                                        pattern_start => 24236,
                                                        pattern_end => 24250}
                                                )
                                    end,
                                    _assert_subject@3 = erlang:element(
                                        6,
                                        New_last@1
                                    ),
                                    case _assert_subject@3 =:= false of
                                        true -> nil;
                                        false -> erlang:error(
                                            #{gleam_error => assert,
                                                message => <<"Assertion failed."/utf8>>,
                                                file => <<?FILEPATH/utf8>>,
                                                module => <<"vxml"/utf8>>,
                                                function => <<"t_very_fancy_sticky_lines_post_processing"/utf8>>,
                                                line => 933,
                                                kind => binary_operator,
                                                operator => '==',
                                                left => #{kind => expression,
                                                    value => _assert_subject@3,
                                                    start => 24283,
                                                    'end' => 24302
                                                    },
                                                right => #{kind => literal,
                                                    value => false,
                                                    start => 24306,
                                                    'end' => 24311
                                                    },
                                                start => 24276,
                                                'end' => 24311,
                                                expression_start => 24283}
                                        )
                                    end,
                                    t_very_fancy_sticky_lines_post_processing(
                                        begin
                                            _pipe@2 = Init@1,
                                            lists:reverse(_pipe@2)
                                        end
                                    );

                                false ->
                                    Lines
                            end
                    end
            end
    end.

-file("src/vxml.gleam", 841).
-spec t_sticky_lines(v_x_m_l(), integer(), boolean(), gleam@regexp:regexp()) -> list(sticky_line()).
t_sticky_lines(T, Indent, Pre, Ampersand_re) ->
    Lines@1 = case T of
        {t, _, Lines} -> Lines;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml"/utf8>>,
                        function => <<"t_sticky_lines"/utf8>>,
                        line => 847,
                        value => _assert_fail,
                        start => 20933,
                        'end' => 20959,
                        pattern_start => 20944,
                        pattern_end => 20955})
    end,
    Indent@1 = case Pre of
        true ->
            0;

        false ->
            Indent
    end,
    Last_index = erlang:length(Lines@1) - 1,
    Sticky_lines = gleam@list:index_map(
        Lines@1,
        fun(Line, I) ->
            Content = html_string_processor(
                erlang:element(3, Line),
                Ampersand_re
            ),
            {sticky_line,
                erlang:element(2, Line),
                Indent@1,
                Content,
                (I =:= 0) andalso (not gleam_stdlib:string_starts_with(
                    Content,
                    <<" "/utf8>>
                )
                orelse Pre),
                (I =:= Last_index) andalso (not gleam_stdlib:string_ends_with(
                    Content,
                    <<" "/utf8>>
                )
                orelse Pre)}
        end
    ),
    case Pre of
        true ->
            Sticky_lines;

        false ->
            t_very_fancy_sticky_lines_post_processing(Sticky_lines)
    end.

-file("src/vxml.gleam", 949).
-spec t_sticky_tree(v_x_m_l(), integer(), boolean(), gleam@regexp:regexp()) -> sticky_tree().
t_sticky_tree(T, Indent, Pre, Ampersand_re) ->
    {sticky_tree, t_sticky_lines(T, Indent, Pre, Ampersand_re), [], []}.

-file("src/vxml.gleam", 988).
-spec vxml_sticky_tree(
    v_x_m_l(),
    integer(),
    integer(),
    boolean(),
    gleam@regexp:regexp()
) -> sticky_tree().
vxml_sticky_tree(Node, Indent, Spaces, Pre, Ampersand_re) ->
    case Node of
        {t, _, _} ->
            t_sticky_tree(Node, Indent, Pre, Ampersand_re);

        {v, _, _, _, _} ->
            v_sticky_tree(Node, Indent, Spaces, Pre, Ampersand_re)
    end.

-file("src/vxml.gleam", 962).
-spec v_sticky_tree(
    v_x_m_l(),
    integer(),
    integer(),
    boolean(),
    gleam@regexp:regexp()
) -> sticky_tree().
v_sticky_tree(V, Indent, Spaces, Pre, Ampersand_re) ->
    {Tag@1, Children@1} = case V of
        {v, _, Tag, _, Children} -> {Tag, Children};
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml"/utf8>>,
                        function => <<"v_sticky_tree"/utf8>>,
                        line => 969,
                        value => _assert_fail,
                        start => 24968,
                        'end' => 25005,
                        pattern_start => 24979,
                        pattern_end => 25001})
    end,
    Pre@1 = Pre orelse (begin
        _pipe = Tag@1,
        string:lowercase(_pipe)
    end
    =:= <<"pre"/utf8>>),
    {sticky_tree,
        opening_tag_to_sticky_lines(V, Indent, Spaces, Pre@1),
        begin
            _pipe@1 = Children@1,
            gleam@list:map(
                _pipe@1,
                fun(_capture) ->
                    vxml_sticky_tree(
                        _capture,
                        Indent + Spaces,
                        Spaces,
                        Pre@1,
                        Ampersand_re
                    )
                end
            )
        end,
        case gleam@list:contains(
            [<<"img"/utf8>>, <<"br"/utf8>>, <<"hr"/utf8>>],
            Tag@1
        ) of
            true ->
                [];

            false ->
                closing_tag_to_sticky_lines(V, Indent, Pre@1)
        end}.

-file("src/vxml.gleam", 1001).
-spec vxml_to_html_output_lines_internal(
    v_x_m_l(),
    integer(),
    integer(),
    gleam@regexp:regexp()
) -> list(vxml@io_lines:output_line()).
vxml_to_html_output_lines_internal(Node, Indent, Spaces, Ampersand_re) ->
    _pipe = vxml_sticky_tree(Node, Indent, Spaces, false, Ampersand_re),
    _pipe@1 = sticky_tree_2_sticky_lines([], _pipe),
    _pipe@2 = lists:reverse(_pipe@1),
    _pipe@3 = concat_sticky_lines(_pipe@2),
    gleam@list:map(_pipe@3, fun sticky_2_blamed/1).

-file("src/vxml.gleam", 1014).
-spec vxmls_to_html_output_lines_internal(
    list(v_x_m_l()),
    integer(),
    integer(),
    gleam@regexp:regexp()
) -> list(vxml@io_lines:output_line()).
vxmls_to_html_output_lines_internal(Vxmls, Indent, Spaces, Ampersand_re) ->
    _pipe = Vxmls,
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(_capture) ->
            vxml_to_html_output_lines_internal(
                _capture,
                Indent,
                Spaces,
                Ampersand_re
            )
        end
    ),
    lists:append(_pipe@1).

-file("src/vxml.gleam", 1031).
?DOC(" Serialize one VXML node to HTML output lines.\n").
-spec vxml_to_html_output_lines(v_x_m_l(), integer(), integer()) -> list(vxml@io_lines:output_line()).
vxml_to_html_output_lines(Node, Indent, Spaces) ->
    Ampersand_re@1 = case gleam@regexp:from_string(
        <<"&(?!(?:[a-zA-Z]{2,6};|#x[a-f\\d]{1,6};|#\\d{2,6};))"/utf8>>
    ) of
        {ok, Ampersand_re} -> Ampersand_re;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml"/utf8>>,
                        function => <<"vxml_to_html_output_lines"/utf8>>,
                        line => 1036,
                        value => _assert_fail,
                        start => 26489,
                        'end' => 26560,
                        pattern_start => 26500,
                        pattern_end => 26516})
    end,
    vxml_to_html_output_lines_internal(Node, Indent, Spaces, Ampersand_re@1).

-file("src/vxml.gleam", 1040).
-spec vxmls_to_html_output_lines(list(v_x_m_l()), integer(), integer()) -> list(vxml@io_lines:output_line()).
vxmls_to_html_output_lines(Vxmls, Indent, Spaces) ->
    Ampersand_re@1 = case gleam@regexp:from_string(
        <<"&(?!(?:[a-zA-Z]{2,6};|#x[a-f\\d]{1,6};|#\\d{2,6};))"/utf8>>
    ) of
        {ok, Ampersand_re} -> Ampersand_re;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml"/utf8>>,
                        function => <<"vxmls_to_html_output_lines"/utf8>>,
                        line => 1045,
                        value => _assert_fail,
                        start => 26749,
                        'end' => 26820,
                        pattern_start => 26760,
                        pattern_end => 26776})
    end,
    vxmls_to_html_output_lines_internal(Vxmls, Indent, Spaces, Ampersand_re@1).

-file("src/vxml.gleam", 1059).
-spec take_while_text_or_newline_acc(
    list(xml_streamer:event()),
    list(xml_streamer:event())
) -> {list(xml_streamer:event()), list(xml_streamer:event())}.
take_while_text_or_newline_acc(Previous, Remaining) ->
    case Remaining of
        [] ->
            {Previous, []};

        [First | Rest] ->
            case First of
                {text, _, _} ->
                    take_while_text_or_newline_acc([First | Previous], Rest);

                {newline, _} ->
                    take_while_text_or_newline_acc([First | Previous], Rest);

                _ ->
                    {Previous, Remaining}
            end
    end.

-file("src/vxml.gleam", 1075).
-spec take_while_text_or_newline(list(xml_streamer:event())) -> {list(xml_streamer:event()),
    list(xml_streamer:event())}.
take_while_text_or_newline(Events) ->
    take_while_text_or_newline_acc([], Events).

-file("src/vxml.gleam", 1087).
-spec on_continuation(return(HUK, HUL), fun((HUL) -> HUK)) -> HUK.
on_continuation(Thing, F) ->
    case Thing of
        {return, A} ->
            A;

        {continuation, B} ->
            F(B)
    end.

-file("src/vxml.gleam", 1100).
-spec tri_way(list(xml_streamer:event())) -> tri_way().
tri_way(Events) ->
    case Events of
        [] ->
            no_more_events;

        [First | Rest] ->
            case First of
                {tag_end_ordinary, _} ->
                    {tag_end, First, Rest};

                {tag_end_self_closing, _} ->
                    {tag_end, First, Rest};

                {tag_end_x_m_l_version, _} ->
                    {tag_end, First, Rest};

                {in_tag_whitespace, _, _} ->
                    case tri_way(Rest) of
                        {something_else, First@1, Rest@1, _} ->
                            {something_else, First@1, Rest@1, true};

                        X ->
                            X
                    end;

                {newline, _} ->
                    case tri_way(Rest) of
                        {something_else, First@1, Rest@1, _} ->
                            {something_else, First@1, Rest@1, true};

                        X ->
                            X
                    end;

                _ ->
                    {something_else, First, Rest, false}
            end
    end.

-file("src/vxml.gleam", 1119).
-spec get_attrs_and_tag_end(xml_streamer:event(), list(xml_streamer:event())) -> {ok,
        {list(attr()), xml_streamer:event(), list(xml_streamer:event())}} |
    {error, {vxml@blame:blame(), binary()}}.
get_attrs_and_tag_end(Tag_start, Rest) ->
    Prepend_attr_if_ok = fun(Result, Attr) -> case Result of
            {error, E} ->
                {error, E};

            {ok, {Attrs, End, Rest@1}} ->
                {ok, {[Attr | Attrs], End, Rest@1}}
        end end,
    on_continuation(case tri_way(Rest) of
            {tag_end, Tag_end, Rest@2} ->
                {return, {ok, {[], Tag_end, Rest@2}}};

            no_more_events ->
                {return,
                    {error,
                        {erlang:element(2, Tag_start),
                            <<"ran out of events while waiting for end of tag"/utf8>>}}};

            {something_else, First, Rest@3, _} ->
                {continuation, {First, Rest@3}}
        end, fun(_use0) ->
            {First@1, Rest@4} = _use0,
            on:ok(case First@1 of
                    {key, B, K} ->
                        {ok, {B, K}};

                    _ ->
                        {error,
                            {erlang:element(2, First@1),
                                <<<<<<<<"expecting tag end or valid key after tag name; tag_start"/utf8,
                                                (xml_streamer:event_digest(
                                                    Tag_start
                                                ))/binary>>/binary,
                                            "; had "/utf8>>/binary,
                                        (xml_streamer:event_digest(First@1))/binary>>/binary,
                                    " instead"/utf8>>}}
                end, fun(_use0@1) ->
                    {Key_blame, Key_name} = _use0@1,
                    Proto = {attr, Key_blame, Key_name, <<""/utf8>>},
                    on_continuation(case tri_way(Rest@4) of
                            {tag_end, Tag_end@1, Rest@5} ->
                                {return, {ok, {[Proto], Tag_end@1, Rest@5}}};

                            no_more_events ->
                                {return,
                                    {error,
                                        {erlang:element(2, Tag_start),
                                            <<"ran out of events while waiting for end of tag"/utf8>>}}};

                            {something_else, Second, Rest@6, _} ->
                                {continuation, {Second, Rest@6}}
                        end, fun(_use0@2) ->
                            {Second@1, Rest@7} = _use0@2,
                            on_continuation(case Second@1 of
                                    {assignment, _} ->
                                        {continuation, nil};

                                    _ ->
                                        {return,
                                            begin
                                                _pipe = get_attrs_and_tag_end(
                                                    Tag_start,
                                                    [Second@1 | Rest@7]
                                                ),
                                                Prepend_attr_if_ok(_pipe, Proto)
                                            end}
                                end, fun(_) ->
                                    on_continuation(case tri_way(Rest@7) of
                                            {tag_end, Tag_end@2, Rest@8} ->
                                                {return,
                                                    {ok,
                                                        {[Proto],
                                                            Tag_end@2,
                                                            Rest@8}}};

                                            no_more_events ->
                                                {return,
                                                    {error,
                                                        {erlang:element(
                                                                2,
                                                                Tag_start
                                                            ),
                                                            <<"ran out of events while waiting for end of tag"/utf8>>}}};

                                            {something_else,
                                                Third,
                                                Rest@9,
                                                Had_spaces} ->
                                                {continuation,
                                                    {Third, Rest@9, Had_spaces}}
                                        end, fun(_use0@3) ->
                                            {Third@1, Rest@10, Had_spaces@1} = _use0@3,
                                            case Third@1 of
                                                {value_double_quoted, _, Val} ->
                                                    _pipe@1 = get_attrs_and_tag_end(
                                                        Tag_start,
                                                        Rest@10
                                                    ),
                                                    Prepend_attr_if_ok(
                                                        _pipe@1,
                                                        {attr,
                                                            erlang:element(
                                                                2,
                                                                Proto
                                                            ),
                                                            erlang:element(
                                                                3,
                                                                Proto
                                                            ),
                                                            Val}
                                                    );

                                                {value_single_quoted, _, Val} ->
                                                    _pipe@1 = get_attrs_and_tag_end(
                                                        Tag_start,
                                                        Rest@10
                                                    ),
                                                    Prepend_attr_if_ok(
                                                        _pipe@1,
                                                        {attr,
                                                            erlang:element(
                                                                2,
                                                                Proto
                                                            ),
                                                            erlang:element(
                                                                3,
                                                                Proto
                                                            ),
                                                            Val}
                                                    );

                                                {value_malformed, Blame, Val@1} ->
                                                    {error,
                                                        {Blame,
                                                            <<"malformed attr val: "/utf8,
                                                                Val@1/binary>>}};

                                                _ ->
                                                    case get_attrs_and_tag_end(
                                                        Tag_start,
                                                        Rest@10
                                                    ) of
                                                        {error, E@1} ->
                                                            {error, E@1};

                                                        {ok,
                                                            {Attrs@1,
                                                                End@1,
                                                                Rest@11}} ->
                                                            case {Had_spaces@1,
                                                                Attrs@1} of
                                                                {false,
                                                                    [Some | _]} ->
                                                                    {error,
                                                                        {erlang:element(
                                                                                2,
                                                                                Some
                                                                            ),
                                                                            <<"expecting attr val after '='"/utf8>>}};

                                                                {_, _} ->
                                                                    {ok,
                                                                        {[Proto |
                                                                                Attrs@1],
                                                                            End@1,
                                                                            Rest@11}}
                                                            end
                                                    end
                                            end
                                        end)
                                end)
                        end)
                end)
        end).

-file("src/vxml.gleam", 1236).
-spec reach_end_of_comments(xml_streamer:event(), list(xml_streamer:event())) -> {ok,
        {list(xml_streamer:event()), list(xml_streamer:event())}} |
    {error, {vxml@blame:blame(), binary()}}.
reach_end_of_comments(Comment_start, Rest) ->
    case Rest of
        [{comment_end_sequence, _} | Rest@1] ->
            {ok, {[], Rest@1}};

        [{comment_contents, _, _} = First | Rest@2] ->
            on:ok(
                reach_end_of_comments(Comment_start, Rest@2),
                fun(_use0) ->
                    {Before, After} = _use0,
                    {ok, {[First | Before], After}}
                end
            );

        [{newline, _} | Rest@3] ->
            reach_end_of_comments(Comment_start, Rest@3);

        [] ->
            {error,
                {erlang:element(2, Comment_start), <<"unclosed comment"/utf8>>}};

        [Some | _] ->
            Msg = <<<<<<"non-comment Event after comment start; start: "/utf8,
                        (vxml@blame:blame_digest(
                            erlang:element(2, Comment_start)
                        ))/binary>>/binary,
                    "; Event: "/utf8>>/binary,
                (xml_streamer:event_digest(Some))/binary>>,
            erlang:error(#{gleam_error => panic,
                    message => Msg,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"vxml"/utf8>>,
                    function => <<"reach_end_of_comments"/utf8>>,
                    line => 1261})
    end.

-file("src/vxml.gleam", 1266).
-spec xml_streaming_get_next_logical_unit(list(xml_streamer:event())) -> {ok,
        {x_m_l_streaming_parser_logical_unit(), list(xml_streamer:event())}} |
    {error, {vxml@blame:blame(), binary()}}.
xml_streaming_get_next_logical_unit(Events) ->
    {First@1, Rest@1} = case Events of
        [First | Rest] -> {First, Rest};
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml"/utf8>>,
                        function => <<"xml_streaming_get_next_logical_unit"/utf8>>,
                        line => 1269,
                        value => _assert_fail,
                        start => 33101,
                        'end' => 33136,
                        pattern_start => 33112,
                        pattern_end => 33127})
    end,
    case First@1 of
        {text, _, _} ->
            {Guys, Remaining} = take_while_text_or_newline(Events),
            Last@1 = case Guys of
                [Last | _] -> Last;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"vxml"/utf8>>,
                                function => <<"xml_streaming_get_next_logical_unit"/utf8>>,
                                line => 1275,
                                value => _assert_fail@1,
                                start => 33294,
                                'end' => 33322,
                                pattern_start => 33305,
                                pattern_end => 33315})
            end,
            Guys@1 = case Last@1 of
                {newline, B} ->
                    [{text, B, <<""/utf8>>} | Guys];

                _ ->
                    Guys
            end,
            Guys@2 = begin
                _pipe = Guys@1,
                lists:reverse(_pipe)
            end,
            Guys@3 = case First@1 of
                {newline, B@1} ->
                    [{text, B@1, <<""/utf8>>} | Guys@2];

                _ ->
                    Guys@2
            end,
            Lines = begin
                _pipe@1 = gleam@list:map(Guys@3, fun(X) -> case X of
                            {newline, _} ->
                                none;

                            {text, B@2, C} ->
                                {some, {line, B@2, C}};

                            _ ->
                                erlang:error(#{gleam_error => panic,
                                        message => <<"`panic` expression evaluated."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"vxml"/utf8>>,
                                        function => <<"xml_streaming_get_next_logical_unit"/utf8>>,
                                        line => 1290})
                        end end),
                gleam@option:values(_pipe@1)
            end,
            {ok, {{x_m_l_streaming_parser_text, Lines}, Remaining}};

        {newline, _} ->
            {Guys, Remaining} = take_while_text_or_newline(Events),
            Last@1 = case Guys of
                [Last | _] -> Last;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"vxml"/utf8>>,
                                function => <<"xml_streaming_get_next_logical_unit"/utf8>>,
                                line => 1275,
                                value => _assert_fail@1,
                                start => 33294,
                                'end' => 33322,
                                pattern_start => 33305,
                                pattern_end => 33315})
            end,
            Guys@1 = case Last@1 of
                {newline, B} ->
                    [{text, B, <<""/utf8>>} | Guys];

                _ ->
                    Guys
            end,
            Guys@2 = begin
                _pipe = Guys@1,
                lists:reverse(_pipe)
            end,
            Guys@3 = case First@1 of
                {newline, B@1} ->
                    [{text, B@1, <<""/utf8>>} | Guys@2];

                _ ->
                    Guys@2
            end,
            Lines = begin
                _pipe@1 = gleam@list:map(Guys@3, fun(X) -> case X of
                            {newline, _} ->
                                none;

                            {text, B@2, C} ->
                                {some, {line, B@2, C}};

                            _ ->
                                erlang:error(#{gleam_error => panic,
                                        message => <<"`panic` expression evaluated."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"vxml"/utf8>>,
                                        function => <<"xml_streaming_get_next_logical_unit"/utf8>>,
                                        line => 1290})
                        end end),
                gleam@option:values(_pipe@1)
            end,
            {ok, {{x_m_l_streaming_parser_text, Lines}, Remaining}};

        {tag_start_ordinary, Blame, Tag} ->
            on:ok(
                get_attrs_and_tag_end(First@1, Rest@1),
                fun(_use0) ->
                    {Attrs, End, Remaining@1} = _use0,
                    case End of
                        {tag_end_ordinary, _} ->
                            {ok,
                                {{x_m_l_streaming_parser_opening_tag,
                                        Blame,
                                        Tag,
                                        Attrs},
                                    Remaining@1}};

                        {tag_end_self_closing, _} ->
                            {ok,
                                {{x_m_l_streaming_parser_self_closing_tag,
                                        Blame,
                                        Tag,
                                        Attrs},
                                    Remaining@1}};

                        {tag_end_x_m_l_version, B@3} ->
                            {error,
                                {B@3, <<"unexpected '?>' tag ending"/utf8>>}};

                        _ ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"`panic` expression evaluated."/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"vxml"/utf8>>,
                                    function => <<"xml_streaming_get_next_logical_unit"/utf8>>,
                                    line => 1308})
                    end
                end
            );

        {tag_start_x_m_l_version, Blame@1, Tag@1} ->
            case Tag@1 =:= <<"xml"/utf8>> orelse Tag@1 =:= <<"XML"/utf8>> of
                true -> nil;
                false -> erlang:error(#{gleam_error => assert,
                        message => <<"Assertion failed."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml"/utf8>>,
                        function => <<"xml_streaming_get_next_logical_unit"/utf8>>,
                        line => 1314,
                        kind => binary_operator,
                        operator => '||',
                        left => #{kind => expression,
                            value => false,
                            start => 34540,
                            'end' => 34552
                            },
                        right => #{kind => expression,
                            value => false,
                            start => 34556,
                            'end' => 34568
                            },
                        start => 34533,
                        'end' => 34568,
                        expression_start => 34540})
            end,
            on:ok(
                get_attrs_and_tag_end(First@1, Rest@1),
                fun(_use0@1) ->
                    {Attrs@1, End@1, Remaining@2} = _use0@1,
                    case End@1 of
                        {tag_end_x_m_l_version, _} ->
                            {ok,
                                {{x_m_l_streaming_parser_x_m_l_version,
                                        Blame@1,
                                        Tag@1,
                                        Attrs@1},
                                    Remaining@2}};

                        {tag_end_ordinary, B@4} ->
                            {error, {B@4, <<"expecting '?>' tag ending"/utf8>>}};

                        {tag_end_self_closing, B@5} ->
                            {error, {B@5, <<"expecting '?>' tag ending"/utf8>>}};

                        _ ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"`panic` expression evaluated."/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"vxml"/utf8>>,
                                    function => <<"xml_streaming_get_next_logical_unit"/utf8>>,
                                    line => 1321})
                    end
                end
            );

        {tag_start_doctype, Blame@2, Tag@2} ->
            on:ok(
                get_attrs_and_tag_end(First@1, Rest@1),
                fun(_use0@2) ->
                    {Attrs@2, End@2, Remaining@3} = _use0@2,
                    case End@2 of
                        {tag_end_ordinary, _} ->
                            {ok,
                                {{x_m_l_streaming_parser_doctype,
                                        Blame@2,
                                        Tag@2,
                                        Attrs@2,
                                        false},
                                    Remaining@3}};

                        {tag_end_self_closing, _} ->
                            {ok,
                                {{x_m_l_streaming_parser_doctype,
                                        Blame@2,
                                        Tag@2,
                                        Attrs@2,
                                        true},
                                    Remaining@3}};

                        {tag_end_x_m_l_version, B@6} ->
                            {error,
                                {B@6, <<"unexpected '?>' tag ending"/utf8>>}};

                        _ ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"`panic` expression evaluated."/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"vxml"/utf8>>,
                                    function => <<"xml_streaming_get_next_logical_unit"/utf8>>,
                                    line => 1334})
                    end
                end
            );

        {tag_start_closing, Blame@3, Tag@3} ->
            on:ok(
                get_attrs_and_tag_end(First@1, Rest@1),
                fun(_use0@3) ->
                    {Attrs@3, End@3, Remaining@4} = _use0@3,
                    on:nonempty_empty(
                        Attrs@3,
                        fun(_, _) ->
                            {error, {Blame@3, <<"attrs in closing tag"/utf8>>}}
                        end,
                        fun() -> case End@3 of
                                {tag_end_ordinary, _} ->
                                    {ok,
                                        {{x_m_l_streaming_parser_closing_tag,
                                                Blame@3,
                                                Tag@3},
                                            Remaining@4}};

                                {tag_end_self_closing, B@7} ->
                                    {error,
                                        {B@7,
                                            <<"unexpected '/>' in closing tag"/utf8>>}};

                                {tag_end_x_m_l_version, B@8} ->
                                    {error,
                                        {B@8,
                                            <<"unexpected '?>' in closing tag"/utf8>>}};

                                _ ->
                                    erlang:error(#{gleam_error => panic,
                                            message => <<"`panic` expression evaluated."/utf8>>,
                                            file => <<?FILEPATH/utf8>>,
                                            module => <<"vxml"/utf8>>,
                                            function => <<"xml_streaming_get_next_logical_unit"/utf8>>,
                                            line => 1349})
                            end end
                    )
                end
            );

        {comment_start_sequence, _} ->
            on:ok(
                reach_end_of_comments(First@1, Rest@1),
                fun(_use0@4) ->
                    {Events@1, Remaining@5} = _use0@4,
                    Lines@1 = gleam@list:map(
                        Events@1,
                        fun(E) ->
                            {B@10, L@1} = case E of
                                {comment_contents, B@9, L} -> {B@9, L};
                                _assert_fail@2 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                file => <<?FILEPATH/utf8>>,
                                                module => <<"vxml"/utf8>>,
                                                function => <<"xml_streaming_get_next_logical_unit"/utf8>>,
                                                line => 1358,
                                                value => _assert_fail@2,
                                                start => 36273,
                                                'end' => 36312,
                                                pattern_start => 36284,
                                                pattern_end => 36308})
                            end,
                            {line, B@10, L@1}
                        end
                    ),
                    {ok,
                        {{x_m_l_streaming_parser_comment, Lines@1}, Remaining@5}}
                end
            );

        _ ->
            Msg = <<"inner tag content (?) when ostensibly out-of-tag: "/utf8,
                (gleam@string:inspect(First@1))/binary>>,
            erlang:error(#{gleam_error => panic,
                    message => Msg,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"vxml"/utf8>>,
                    function => <<"xml_streaming_get_next_logical_unit"/utf8>>,
                    line => 1369})
    end.

-file("src/vxml.gleam", 1374).
-spec xml_streaming_logical_units_acc(
    list(xml_streamer:event()),
    list(x_m_l_streaming_parser_logical_unit())
) -> {ok, list(x_m_l_streaming_parser_logical_unit())} |
    {error, {vxml@blame:blame(), binary()}}.
xml_streaming_logical_units_acc(Remaining, Acc) ->
    case Remaining of
        [] ->
            _pipe = Acc,
            _pipe@1 = lists:reverse(_pipe),
            {ok, _pipe@1};

        _ ->
            case xml_streaming_get_next_logical_unit(Remaining) of
                {error, Error} ->
                    {error, Error};

                {ok, {Unit, Remaining@1}} ->
                    xml_streaming_logical_units_acc(Remaining@1, [Unit | Acc])
            end
    end.

-file("src/vxml.gleam", 1389).
-spec xml_streaming_logical_units(list(xml_streamer:event())) -> {ok,
        list(x_m_l_streaming_parser_logical_unit())} |
    {error, {vxml@blame:blame(), binary()}}.
xml_streaming_logical_units(Events) ->
    xml_streaming_logical_units_acc(Events, []).

-file("src/vxml.gleam", 1395).
-spec list_to_stack_digest(list(HVM), fun((HVM) -> binary())) -> binary().
list_to_stack_digest(L, D) ->
    <<<<"["/utf8,
            (begin
                _pipe = gleam@list:map(L, D),
                gleam@string:join(_pipe, <<", "/utf8>>)
            end)/binary>>/binary,
        "]"/utf8>>.

-file("src/vxml.gleam", 1399).
-spec attr_to_stack_digest(attr()) -> binary().
attr_to_stack_digest(Attr) ->
    <<<<(erlang:element(3, Attr))/binary, "="/utf8>>/binary,
        (erlang:element(4, Attr))/binary>>.

-file("src/vxml.gleam", 1403).
-spec attrs_to_stack_digest(list(attr())) -> binary().
attrs_to_stack_digest(Attrs) ->
    list_to_stack_digest(Attrs, fun attr_to_stack_digest/1).

-file("src/vxml.gleam", 1407).
-spec vxml_to_stack_digest(v_x_m_l()) -> binary().
vxml_to_stack_digest(Node) ->
    {Bl@1, Tag@1, Attrs@1, Children@1} = case Node of
        {v, Bl, Tag, Attrs, Children} -> {Bl, Tag, Attrs, Children};
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml"/utf8>>,
                        function => <<"vxml_to_stack_digest"/utf8>>,
                        line => 1408,
                        value => _assert_fail,
                        start => 37641,
                        'end' => 37686,
                        pattern_start => 37652,
                        pattern_end => 37679})
    end,
    <<<<<<<<<<<<<<<<<<"V("/utf8, ((vxml@blame:blame_digest(Bl@1)))/binary>>/binary,
                                    ", "/utf8>>/binary,
                                Tag@1/binary>>/binary,
                            ", "/utf8>>/binary,
                        (attrs_to_stack_digest(Attrs@1))/binary>>/binary,
                    ", "/utf8>>/binary,
                "["/utf8>>/binary,
            (case Children@1 of
                [_] ->
                    <<"1 child]"/utf8>>;

                _ ->
                    <<(gleam@string:inspect(erlang:length(Children@1)))/binary,
                        " children]"/utf8>>
            end)/binary>>/binary,
        ")"/utf8>>.

-file("src/vxml.gleam", 1424).
-spec vxmls_from_streaming_logical_units_acc(
    list(x_m_l_streaming_parser_logical_unit()),
    list(v_x_m_l()),
    list(v_x_m_l()),
    boolean(),
    boolean()
) -> {ok, list(v_x_m_l())} | {error, {vxml@blame:blame(), binary()}}.
vxmls_from_streaming_logical_units_acc(
    Units,
    Stack,
    Previously_completed,
    Filter_out_doctype_nodes,
    Filter_out_root_level_text
) ->
    case Units of
        [] ->
            case Stack of
                [] ->
                    {ok,
                        begin
                            _pipe = Previously_completed,
                            lists:reverse(_pipe)
                        end};

                [Last | _] ->
                    {Blame@1, Tag@1} = case Last of
                        {v, Blame, Tag, _, _} -> {Blame, Tag};
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"vxml"/utf8>>,
                                        function => <<"vxmls_from_streaming_logical_units_acc"/utf8>>,
                                        line => 1436,
                                        value => _assert_fail,
                                        start => 38311,
                                        'end' => 38348,
                                        pattern_start => 38322,
                                        pattern_end => 38341})
                    end,
                    Ancestor_tag_sequence = begin
                        _pipe@1 = Stack,
                        _pipe@2 = gleam@list:map(
                            _pipe@1,
                            fun vxml_to_stack_digest/1
                        ),
                        gleam@string:join(_pipe@2, <<" -> "/utf8>>)
                    end,
                    {error,
                        {Blame@1,
                            <<<<<<"unclosed '"/utf8, Tag@1/binary>>/binary,
                                    "' at end of document; open ancestor sequence: "/utf8>>/binary,
                                Ancestor_tag_sequence/binary>>}}
            end;

        [First | Rest] ->
            case First of
                {x_m_l_streaming_parser_doctype, B, Tag@2, Attrs, _} ->
                    V = {v, B, Tag@2, Attrs, []},
                    case Stack of
                        [] ->
                            vxmls_from_streaming_logical_units_acc(
                                Rest,
                                [],
                                case Filter_out_doctype_nodes of
                                    true ->
                                        Previously_completed;

                                    false ->
                                        [V | Previously_completed]
                                end,
                                Filter_out_doctype_nodes,
                                Filter_out_root_level_text
                            );

                        _ ->
                            {error,
                                {B,
                                    <<"found !DOCTYPE node at non-root level"/utf8>>}}
                    end;

                {x_m_l_streaming_parser_x_m_l_version, B@1, Tag@3, Attrs@1} ->
                    V@1 = {v, B@1, Tag@3, Attrs@1, []},
                    case Stack of
                        [] ->
                            vxmls_from_streaming_logical_units_acc(
                                Rest,
                                [],
                                case Filter_out_doctype_nodes of
                                    true ->
                                        Previously_completed;

                                    false ->
                                        [V@1 | Previously_completed]
                                end,
                                Filter_out_doctype_nodes,
                                Filter_out_root_level_text
                            );

                        _ ->
                            {error,
                                {B@1,
                                    <<"found XML version-node at non-root level"/utf8>>}}
                    end;

                {x_m_l_streaming_parser_opening_tag, B@2, Tag@4, Attrs@2} ->
                    V@2 = {v, B@2, Tag@4, Attrs@2, []},
                    vxmls_from_streaming_logical_units_acc(
                        Rest,
                        [V@2 | Stack],
                        Previously_completed,
                        Filter_out_doctype_nodes,
                        Filter_out_root_level_text
                    );

                {x_m_l_streaming_parser_text, Lines} ->
                    First_line@1 = case Lines of
                        [First_line | _] -> First_line;
                        _assert_fail@1 ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"vxml"/utf8>>,
                                        function => <<"vxmls_from_streaming_logical_units_acc"/utf8>>,
                                        line => 1502,
                                        value => _assert_fail@1,
                                        start => 40333,
                                        'end' => 40368,
                                        pattern_start => 40344,
                                        pattern_end => 40360})
                    end,
                    T = {t, erlang:element(2, First_line@1), Lines},
                    {Stack@1, Previously_completed@1} = case Stack of
                        [Last@1 | Others] ->
                            case Last@1 of
                                {v, _, _, _, _} -> nil;
                                _assert_fail@2 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                file => <<?FILEPATH/utf8>>,
                                                module => <<"vxml"/utf8>>,
                                                function => <<"vxmls_from_streaming_logical_units_acc"/utf8>>,
                                                line => 1506,
                                                value => _assert_fail@2,
                                                start => 40522,
                                                'end' => 40553,
                                                pattern_start => 40533,
                                                pattern_end => 40546})
                            end,
                            Last@2 = {v,
                                erlang:element(2, Last@1),
                                erlang:element(3, Last@1),
                                erlang:element(4, Last@1),
                                [T | erlang:element(5, Last@1)]},
                            {[Last@2 | Others], Previously_completed};

                        _ ->
                            case Filter_out_root_level_text of
                                true ->
                                    {Stack, Previously_completed};

                                false ->
                                    {Stack, [T | Previously_completed]}
                            end
                    end,
                    vxmls_from_streaming_logical_units_acc(
                        Rest,
                        Stack@1,
                        Previously_completed@1,
                        Filter_out_doctype_nodes,
                        Filter_out_root_level_text
                    );

                {x_m_l_streaming_parser_comment, _} ->
                    vxmls_from_streaming_logical_units_acc(
                        Rest,
                        Stack,
                        Previously_completed,
                        Filter_out_doctype_nodes,
                        Filter_out_root_level_text
                    );

                {x_m_l_streaming_parser_closing_tag, B@3, Tag@5} ->
                    case Stack of
                        [] ->
                            {error,
                                {B@3,
                                    <<<<"closing '</"/utf8, Tag@5/binary>>/binary,
                                        ">' on empty stack"/utf8>>}};

                        [Last@3 | Others@1] ->
                            Last_tag@1 = case Last@3 of
                                {v, _, Last_tag, _, _} -> Last_tag;
                                _assert_fail@3 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                file => <<?FILEPATH/utf8>>,
                                                module => <<"vxml"/utf8>>,
                                                function => <<"vxmls_from_streaming_logical_units_acc"/utf8>>,
                                                line => 1539,
                                                value => _assert_fail@3,
                                                start => 41584,
                                                'end' => 41622,
                                                pattern_start => 41595,
                                                pattern_end => 41615})
                            end,
                            case Last_tag@1 =:= Tag@5 of
                                false ->
                                    {error,
                                        {B@3,
                                            <<<<<<<<"expected closing '"/utf8,
                                                            Last_tag@1/binary>>/binary,
                                                        "' tag, found '"/utf8>>/binary,
                                                    Tag@5/binary>>/binary,
                                                "' instead"/utf8>>}};

                                true ->
                                    Last@4 = {v,
                                        erlang:element(2, Last@3),
                                        erlang:element(3, Last@3),
                                        erlang:element(4, Last@3),
                                        begin
                                            _pipe@3 = erlang:element(5, Last@3),
                                            lists:reverse(_pipe@3)
                                        end},
                                    case Others@1 of
                                        [] ->
                                            vxmls_from_streaming_logical_units_acc(
                                                Rest,
                                                [],
                                                [Last@4 | Previously_completed],
                                                Filter_out_doctype_nodes,
                                                Filter_out_root_level_text
                                            );

                                        [Parent | Older] ->
                                            case Parent of
                                                {v, _, _, _, _} -> nil;
                                                _assert_fail@4 ->
                                                    erlang:error(
                                                            #{gleam_error => let_assert,
                                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                                file => <<?FILEPATH/utf8>>,
                                                                module => <<"vxml"/utf8>>,
                                                                function => <<"vxmls_from_streaming_logical_units_acc"/utf8>>,
                                                                line => 1562,
                                                                value => _assert_fail@4,
                                                                start => 42471,
                                                                'end' => 42504,
                                                                pattern_start => 42482,
                                                                pattern_end => 42495}
                                                        )
                                            end,
                                            Parent@1 = {v,
                                                erlang:element(2, Parent),
                                                erlang:element(3, Parent),
                                                erlang:element(4, Parent),
                                                [Last@4 |
                                                    erlang:element(5, Parent)]},
                                            vxmls_from_streaming_logical_units_acc(
                                                Rest,
                                                [Parent@1 | Older],
                                                [],
                                                Filter_out_doctype_nodes,
                                                Filter_out_root_level_text
                                            )
                                    end
                            end
                    end;

                {x_m_l_streaming_parser_self_closing_tag, B@4, Tag@6, Attrs@3} ->
                    V@3 = {v, B@4, Tag@6, Attrs@3, []},
                    case Stack of
                        [Last@5 | Others@2] ->
                            case Last@5 of
                                {v, _, _, _, _} -> nil;
                                _assert_fail@5 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                file => <<?FILEPATH/utf8>>,
                                                module => <<"vxml"/utf8>>,
                                                function => <<"vxmls_from_streaming_logical_units_acc"/utf8>>,
                                                line => 1584,
                                                value => _assert_fail@5,
                                                start => 43186,
                                                'end' => 43217,
                                                pattern_start => 43197,
                                                pattern_end => 43210})
                            end,
                            Last@6 = {v,
                                erlang:element(2, Last@5),
                                erlang:element(3, Last@5),
                                erlang:element(4, Last@5),
                                [V@3 | erlang:element(5, Last@5)]},
                            vxmls_from_streaming_logical_units_acc(
                                Rest,
                                [Last@6 | Others@2],
                                Previously_completed,
                                Filter_out_doctype_nodes,
                                Filter_out_root_level_text
                            );

                        [] ->
                            vxmls_from_streaming_logical_units_acc(
                                Rest,
                                [],
                                [V@3 | Previously_completed],
                                Filter_out_doctype_nodes,
                                Filter_out_root_level_text
                            )
                    end
            end
    end.

-file("src/vxml.gleam", 1610).
-spec vxmls_from_streaming_logical_units(
    list(x_m_l_streaming_parser_logical_unit()),
    boolean(),
    boolean()
) -> {ok, list(v_x_m_l())} | {error, {vxml@blame:blame(), binary()}}.
vxmls_from_streaming_logical_units(
    Units,
    Filter_out_doctype_nodes,
    Filter_out_root_level_text
) ->
    vxmls_from_streaming_logical_units_acc(
        Units,
        [],
        [],
        Filter_out_doctype_nodes,
        Filter_out_root_level_text
    ).

-file("src/vxml.gleam", 1624).
-spec vxml_from_streaming_logical_units(
    list(x_m_l_streaming_parser_logical_unit())
) -> {ok, v_x_m_l()} | {error, {vxml@blame:blame(), binary()}}.
vxml_from_streaming_logical_units(Units) ->
    on:ok(
        vxmls_from_streaming_logical_units(Units, true, true),
        fun(Vxmls) -> case Vxmls of
                [] ->
                    {error, {{no_blame, []}, <<"empty document (?)"/utf8>>}};

                [One] ->
                    {ok, One};

                [_, Second | _] ->
                    {error,
                        {erlang:element(2, Second),
                            <<"found >1 root-level nodes"/utf8>>}}
            end end
    ).

-file("src/vxml.gleam", 1638).
?DOC(" Parse XML-like input lines into VXML.\n").
-spec parse_xml_input_lines(list(vxml@io_lines:input_line())) -> {ok, v_x_m_l()} |
    {error, {vxml@blame:blame(), binary()}}.
parse_xml_input_lines(Lines) ->
    _pipe = Lines,
    _pipe@1 = xml_streamer:input_lines_streamer(_pipe),
    _pipe@2 = xml_streaming_logical_units(_pipe@1),
    on:ok(_pipe@2, fun vxml_from_streaming_logical_units/1).

-file("src/vxml.gleam", 1648).
?DOC(" Parse an XML-like string into VXML.\n").
-spec parse_xml(binary(), binary()) -> {ok, v_x_m_l()} |
    {error, {vxml@blame:blame(), binary()}}.
parse_xml(Content, Filename) ->
    _pipe = Content,
    _pipe@1 = vxml@io_lines:string_to_input_lines(_pipe, Filename, 0),
    parse_xml_input_lines(_pipe@1).

-file("src/vxml.gleam", 1661).
-spec html_repair_escape_non_entity_ampersands(binary()) -> binary().
html_repair_escape_non_entity_ampersands(Content) ->
    vxml_html_repair:html_repair_escape_non_entity_ampersands(Content).

-file("src/vxml.gleam", 1665).
-spec html_repair_expand_boolean_attrs(binary()) -> binary().
html_repair_expand_boolean_attrs(Content) ->
    vxml_html_repair:html_repair_expand_boolean_attrs(Content).

-file("src/vxml.gleam", 1669).
-spec html_repair_close_void_tags(binary()) -> binary().
html_repair_close_void_tags(Content) ->
    vxml_html_repair:html_repair_close_void_tags(Content).

-file("src/vxml.gleam", 1673).
-spec html_repair_remove_attrs_from_closing_tags(binary()) -> binary().
html_repair_remove_attrs_from_closing_tags(Content) ->
    vxml_html_repair:html_repair_remove_attrs_from_closing_tags(Content).

-file("src/vxml.gleam", 1677).
-spec html_repair(binary()) -> binary().
html_repair(Content) ->
    vxml_html_repair:html_repair(Content).
