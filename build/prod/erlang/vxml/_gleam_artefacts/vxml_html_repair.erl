-module(vxml_html_repair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/vxml_html_repair.gleam").
-export([html_repair_escape_non_entity_ampersands/1, html_repair_expand_boolean_attrs/1, html_repair_close_void_tags/1, html_repair_remove_attrs_from_closing_tags/1, html_repair/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(" Best-effort HTML string repairs for XML-oriented parsing.\n").

-file("src/vxml_html_repair.gleam", 11).
-spec html_repair_close_void_tag(binary(), binary()) -> binary().
html_repair_close_void_tag(Content, Tag) ->
    Re@1 = case gleam@regexp:from_string(
        <<<<"(<"/utf8, Tag/binary>>/binary, ")(\\b[^>]*)(>)"/utf8>>
    ) of
        {ok, Re} -> Re;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml_html_repair"/utf8>>,
                        function => <<"html_repair_close_void_tag"/utf8>>,
                        line => 12,
                        value => _assert_fail,
                        start => 379,
                        'end' => 450,
                        pattern_start => 390,
                        pattern_end => 396})
    end,
    gleam_regexp_ffi:match_map(
        Re@1,
        Content,
        fun(Match) ->
            {match, _, Sub} = Match,
            Maybe_middle@1 = case Sub of
                [_, Maybe_middle, _] -> Maybe_middle;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"vxml_html_repair"/utf8>>,
                                function => <<"html_repair_close_void_tag"/utf8>>,
                                line => 16,
                                value => _assert_fail@1,
                                start => 537,
                                'end' => 574,
                                pattern_start => 548,
                                pattern_end => 568})
            end,
            Middle = begin
                _pipe = Maybe_middle@1,
                gleam@option:unwrap(_pipe, <<""/utf8>>)
            end,
            case begin
                _pipe@1 = Middle,
                _pipe@2 = gleam@string:trim_end(_pipe@1),
                gleam_stdlib:string_ends_with(_pipe@2, <<"/"/utf8>>)
            end of
                true ->
                    <<<<<<"<"/utf8, Tag/binary>>/binary, Middle/binary>>/binary,
                        ">"/utf8>>;

                false ->
                    <<<<<<"<"/utf8, Tag/binary>>/binary, Middle/binary>>/binary,
                        "/>"/utf8>>
            end
        end
    ).

-file("src/vxml_html_repair.gleam", 25).
-spec html_repair_escape_non_entity_ampersands(binary()) -> binary().
html_repair_escape_non_entity_ampersands(Content) ->
    Re@1 = case gleam@regexp:from_string(
        <<"&(?!(?:[a-zA-Z]{2,6};|#x[a-f\\d]{1,6};|#\\d{2,6};))"/utf8>>
    ) of
        {ok, Re} -> Re;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml_html_repair"/utf8>>,
                        function => <<"html_repair_escape_non_entity_ampersands"/utf8>>,
                        line => 26,
                        value => _assert_fail,
                        start => 867,
                        'end' => 928,
                        pattern_start => 878,
                        pattern_end => 884})
    end,
    gleam_regexp_ffi:replace(Re@1, Content, <<"&amp;"/utf8>>).

-file("src/vxml_html_repair.gleam", 31).
-spec html_repair_expand_boolean_attr(binary(), binary()) -> binary().
html_repair_expand_boolean_attr(Content, Attr) ->
    Re@1 = case gleam@regexp:from_string(
        <<<<"(\\s"/utf8, Attr/binary>>/binary, ")(\\s|>|/>)"/utf8>>
    ) of
        {ok, Re} -> Re;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml_html_repair"/utf8>>,
                        function => <<"html_repair_expand_boolean_attr"/utf8>>,
                        line => 32,
                        value => _assert_fail,
                        start => 1052,
                        'end' => 1123,
                        pattern_start => 1063,
                        pattern_end => 1069})
    end,
    gleam_regexp_ffi:match_map(
        Re@1,
        Content,
        fun(Match) ->
            {match, _, Sub} = Match,
            {Attr@2, After@1} = case Sub of
                [{some, Attr@1}, {some, After}] -> {Attr@1, After};
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"vxml_html_repair"/utf8>>,
                                function => <<"html_repair_expand_boolean_attr"/utf8>>,
                                line => 36,
                                value => _assert_fail@1,
                                start => 1210,
                                'end' => 1252,
                                pattern_start => 1221,
                                pattern_end => 1246})
            end,
            <<<<Attr@2/binary, "=\"\""/utf8>>/binary, After@1/binary>>
        end
    ).

-file("src/vxml_html_repair.gleam", 41).
-spec html_repair_expand_boolean_attrs(binary()) -> binary().
html_repair_expand_boolean_attrs(Content) ->
    _pipe = [<<"allowfullscreen"/utf8>>,
        <<"async"/utf8>>,
        <<"autofocus"/utf8>>,
        <<"autoplay"/utf8>>,
        <<"checked"/utf8>>,
        <<"controls"/utf8>>,
        <<"default"/utf8>>,
        <<"defer"/utf8>>,
        <<"disabled"/utf8>>,
        <<"formnovalidate"/utf8>>,
        <<"hidden"/utf8>>,
        <<"inert"/utf8>>,
        <<"ismap"/utf8>>,
        <<"loop"/utf8>>,
        <<"multiple"/utf8>>,
        <<"muted"/utf8>>,
        <<"nomodule"/utf8>>,
        <<"novalidate"/utf8>>,
        <<"open"/utf8>>,
        <<"playsinline"/utf8>>,
        <<"readonly"/utf8>>,
        <<"required"/utf8>>,
        <<"reversed"/utf8>>,
        <<"selected"/utf8>>],
    gleam@list:fold(
        _pipe,
        Content,
        fun(Content@1, Attr) ->
            html_repair_expand_boolean_attr(Content@1, Attr)
        end
    ).

-file("src/vxml_html_repair.gleam", 53).
-spec html_repair_close_void_tags(binary()) -> binary().
html_repair_close_void_tags(Content) ->
    _pipe = [<<"area"/utf8>>,
        <<"base"/utf8>>,
        <<"br"/utf8>>,
        <<"col"/utf8>>,
        <<"embed"/utf8>>,
        <<"hr"/utf8>>,
        <<"img"/utf8>>,
        <<"input"/utf8>>,
        <<"link"/utf8>>,
        <<"meta"/utf8>>,
        <<"source"/utf8>>,
        <<"track"/utf8>>,
        <<"wbr"/utf8>>],
    gleam@list:fold(
        _pipe,
        Content,
        fun(Content@1, Tag) -> html_repair_close_void_tag(Content@1, Tag) end
    ).

-file("src/vxml_html_repair.gleam", 63).
-spec html_repair_remove_attrs_from_closing_tags(binary()) -> binary().
html_repair_remove_attrs_from_closing_tags(Content) ->
    Re@1 = case gleam@regexp:from_string(
        <<"(<\\/)([a-zA-Z][a-zA-Z0-9._-]*)(\\s+[^>]*)(>)"/utf8>>
    ) of
        {ok, Re} -> Re;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"vxml_html_repair"/utf8>>,
                        function => <<"html_repair_remove_attrs_from_closing_tags"/utf8>>,
                        line => 64,
                        value => _assert_fail,
                        start => 2125,
                        'end' => 2216,
                        pattern_start => 2136,
                        pattern_end => 2142})
    end,
    gleam_regexp_ffi:match_map(
        Re@1,
        Content,
        fun(Match) ->
            {match, _, Sub} = Match,
            Tag@1 = case Sub of
                [_, {some, Tag}, _, _] -> Tag;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"vxml_html_repair"/utf8>>,
                                function => <<"html_repair_remove_attrs_from_closing_tags"/utf8>>,
                                line => 69,
                                value => _assert_fail@1,
                                start => 2303,
                                'end' => 2340,
                                pattern_start => 2314,
                                pattern_end => 2334})
            end,
            <<<<"</"/utf8, Tag@1/binary>>/binary, ">"/utf8>>
        end
    ).

-file("src/vxml_html_repair.gleam", 75).
?DOC(" Best-effort repair for common HTML syntax that blocks XML-oriented parsers.\n").
-spec html_repair(binary()) -> binary().
html_repair(Content) ->
    _pipe = Content,
    _pipe@1 = html_repair_expand_boolean_attrs(_pipe),
    _pipe@2 = html_repair_escape_non_entity_ampersands(_pipe@1),
    _pipe@3 = html_repair_close_void_tags(_pipe@2),
    html_repair_remove_attrs_from_closing_tags(_pipe@3).
