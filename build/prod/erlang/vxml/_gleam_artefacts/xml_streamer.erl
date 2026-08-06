-module(xml_streamer).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/xml_streamer.gleam").
-export([event_digest/1, input_lines_streamer/1]).
-export_type([event/0, content_line/0, state/0, tag_or_not/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Token-level XML streaming utilities.\n"
    "\n"
    " This module exposes the lower-level token stream used by VXML's streaming\n"
    " XML parser. Most callers can use `vxml.parse_xml` instead. Use this module\n"
    " when an application needs to inspect or transform XML\n"
    " events before they become a VXML tree.\n"
).

-type event() :: {newline, vxml@blame:blame()} |
    {tag_start_ordinary, vxml@blame:blame(), binary()} |
    {tag_start_x_m_l_version, vxml@blame:blame(), binary()} |
    {tag_start_doctype, vxml@blame:blame(), binary()} |
    {tag_start_closing, vxml@blame:blame(), binary()} |
    {in_tag_whitespace, vxml@blame:blame(), binary()} |
    {key, vxml@blame:blame(), binary()} |
    {key_malformed, vxml@blame:blame(), binary()} |
    {assignment, vxml@blame:blame()} |
    {value_double_quoted, vxml@blame:blame(), binary()} |
    {value_single_quoted, vxml@blame:blame(), binary()} |
    {value_malformed, vxml@blame:blame(), binary()} |
    {tag_end_ordinary, vxml@blame:blame()} |
    {tag_end_self_closing, vxml@blame:blame()} |
    {tag_end_x_m_l_version, vxml@blame:blame()} |
    {text, vxml@blame:blame(), binary()} |
    {comment_contents, vxml@blame:blame(), binary()} |
    {comment_start_sequence, vxml@blame:blame()} |
    {comment_end_sequence, vxml@blame:blame()}.

-type content_line() :: {content_line, vxml@blame:blame(), binary()}.

-type state() :: outside_tag |
    inside_opening_tag_expecting_next_key |
    inside_opening_tag_expecting_next_assignment |
    inside_opening_tag_expecting_next_value |
    inside_closing_tag |
    inside_comment.

-type tag_or_not() :: {x_m_l_doc, binary()} |
    {doctype, binary()} |
    {ordinary, binary()} |
    {ordinary_closing, binary()} |
    no_tag |
    comment_start.

-file("src/xml_streamer.gleam", 19).
?DOC(" Render one token as a debug string.\n").
-spec event_digest(event()) -> binary().
event_digest(E) ->
    case E of
        {newline, B} ->
            <<<<"Newline("/utf8, (vxml@blame:blame_digest(B))/binary>>/binary,
                ")"/utf8>>;

        {tag_start_ordinary, B@1, Load} ->
            <<<<<<<<"TagStartOrdinary("/utf8, Load/binary>>/binary, ", "/utf8>>/binary,
                    (vxml@blame:blame_digest(B@1))/binary>>/binary,
                ")"/utf8>>;

        {tag_start_x_m_l_version, B@2, Load@1} ->
            <<<<<<<<"TagStartXMLVersion("/utf8, Load@1/binary>>/binary,
                        ", "/utf8>>/binary,
                    (vxml@blame:blame_digest(B@2))/binary>>/binary,
                ")"/utf8>>;

        {tag_start_doctype, B@3, Load@2} ->
            <<<<<<<<"TagStartDoctype("/utf8, Load@2/binary>>/binary, ", "/utf8>>/binary,
                    (vxml@blame:blame_digest(B@3))/binary>>/binary,
                ")"/utf8>>;

        {tag_start_closing, B@4, Load@3} ->
            <<<<<<<<"TagStartClosing("/utf8, Load@3/binary>>/binary, ", "/utf8>>/binary,
                    (vxml@blame:blame_digest(B@4))/binary>>/binary,
                ")"/utf8>>;

        {in_tag_whitespace, B@5, Load@4} ->
            <<<<<<<<"InTagWhitespace("/utf8, Load@4/binary>>/binary, ", "/utf8>>/binary,
                    (vxml@blame:blame_digest(B@5))/binary>>/binary,
                ")"/utf8>>;

        {key, B@6, Load@5} ->
            <<<<<<<<"Key("/utf8, (gleam@string:inspect(Load@5))/binary>>/binary,
                        ", "/utf8>>/binary,
                    (vxml@blame:blame_digest(B@6))/binary>>/binary,
                ")"/utf8>>;

        {key_malformed, B@7, Load@6} ->
            <<<<<<<<"KeyMalformed("/utf8,
                            (gleam@string:inspect(Load@6))/binary>>/binary,
                        ", "/utf8>>/binary,
                    (vxml@blame:blame_digest(B@7))/binary>>/binary,
                ")"/utf8>>;

        {assignment, B@8} ->
            <<<<"Assignment("/utf8, (vxml@blame:blame_digest(B@8))/binary>>/binary,
                ")"/utf8>>;

        {value_double_quoted, B@9, Load@7} ->
            <<<<<<<<"ValueDoubleQuoted("/utf8,
                            (gleam@string:inspect(Load@7))/binary>>/binary,
                        ", "/utf8>>/binary,
                    (vxml@blame:blame_digest(B@9))/binary>>/binary,
                ")"/utf8>>;

        {value_single_quoted, B@10, Load@8} ->
            <<<<<<<<"ValueSingleQuoted("/utf8,
                            (gleam@string:inspect(Load@8))/binary>>/binary,
                        ", "/utf8>>/binary,
                    (vxml@blame:blame_digest(B@10))/binary>>/binary,
                ")"/utf8>>;

        {value_malformed, B@11, Load@9} ->
            <<<<<<<<"ValueMalformed("/utf8,
                            (gleam@string:inspect(Load@9))/binary>>/binary,
                        ", "/utf8>>/binary,
                    (vxml@blame:blame_digest(B@11))/binary>>/binary,
                ")"/utf8>>;

        {tag_end_ordinary, B@12} ->
            <<<<"TagEndOrdinary("/utf8, (vxml@blame:blame_digest(B@12))/binary>>/binary,
                ")"/utf8>>;

        {tag_end_self_closing, B@13} ->
            <<<<"TagEndSelfClosing("/utf8,
                    (vxml@blame:blame_digest(B@13))/binary>>/binary,
                ")"/utf8>>;

        {tag_end_x_m_l_version, B@14} ->
            <<<<"TagEndXMLVersion("/utf8,
                    (vxml@blame:blame_digest(B@14))/binary>>/binary,
                ")"/utf8>>;

        {text, B@15, Load@10} ->
            <<<<<<<<"Text("/utf8, (gleam@string:inspect(Load@10))/binary>>/binary,
                        ", "/utf8>>/binary,
                    (vxml@blame:blame_digest(B@15))/binary>>/binary,
                ")"/utf8>>;

        {comment_contents, B@16, Load@11} ->
            <<<<<<<<"CommentContents("/utf8,
                            (gleam@string:inspect(Load@11))/binary>>/binary,
                        ", "/utf8>>/binary,
                    (vxml@blame:blame_digest(B@16))/binary>>/binary,
                ")"/utf8>>;

        {comment_start_sequence, B@17} ->
            <<<<"CommentStartSequence("/utf8,
                    (vxml@blame:blame_digest(B@17))/binary>>/binary,
                ")"/utf8>>;

        {comment_end_sequence, B@18} ->
            <<<<"CommentEndSequence("/utf8,
                    (vxml@blame:blame_digest(B@18))/binary>>/binary,
                ")"/utf8>>
    end.

-file("src/xml_streamer.gleam", 127).
-spec advance_line(content_line(), integer()) -> content_line().
advance_line(Cl, By) ->
    _assert_subject = 0,
    case By > _assert_subject of
        true -> nil;
        false -> erlang:error(#{gleam_error => assert,
                message => <<"Assertion failed."/utf8>>,
                file => <<?FILEPATH/utf8>>,
                module => <<"xml_streamer"/utf8>>,
                function => <<"advance_line"/utf8>>,
                line => 128,
                kind => binary_operator,
                operator => '>',
                left => #{kind => expression,
                    value => By,
                    start => 4105,
                    'end' => 4107
                    },
                right => #{kind => literal,
                    value => _assert_subject,
                    start => 4110,
                    'end' => 4111
                    },
                start => 4098,
                'end' => 4111,
                expression_start => 4105})
    end,
    _assert_subject@1 = string:length(erlang:element(3, Cl)),
    case _assert_subject@1 >= By of
        true -> nil;
        false -> erlang:error(#{gleam_error => assert,
                message => <<"Assertion failed."/utf8>>,
                file => <<?FILEPATH/utf8>>,
                module => <<"xml_streamer"/utf8>>,
                function => <<"advance_line"/utf8>>,
                line => 129,
                kind => binary_operator,
                operator => '>=',
                left => #{kind => expression,
                    value => _assert_subject@1,
                    start => 4121,
                    'end' => 4146
                    },
                right => #{kind => expression,
                    value => By,
                    start => 4150,
                    'end' => 4152
                    },
                start => 4114,
                'end' => 4152,
                expression_start => 4121})
    end,
    {content_line,
        vxml@blame:advance(erlang:element(2, Cl), By),
        gleam@string:drop_start(erlang:element(3, Cl), By)}.

-file("src/xml_streamer.gleam", 133).
-spec is_ordinary_tag(binary()) -> boolean().
is_ordinary_tag(Input) ->
    Pattern = <<"^[a-zA-Z_][a-zA-Z0-9._-]*$"/utf8>>,
    Re@1 = case gleam@regexp:from_string(Pattern) of
        {ok, Re} -> Re;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"xml_streamer"/utf8>>,
                        function => <<"is_ordinary_tag"/utf8>>,
                        line => 135,
                        value => _assert_fail,
                        start => 4322,
                        'end' => 4369,
                        pattern_start => 4333,
                        pattern_end => 4339})
    end,
    gleam@regexp:check(Re@1, Input).

-file("src/xml_streamer.gleam", 139).
-spec is_valid_key(binary()) -> boolean().
is_valid_key(Input) ->
    Pattern = <<"^[a-zA-Z][:a-zA-Z0-9._-]*$"/utf8>>,
    Re@1 = case gleam@regexp:from_string(Pattern) of
        {ok, Re} -> Re;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"xml_streamer"/utf8>>,
                        function => <<"is_valid_key"/utf8>>,
                        line => 141,
                        value => _assert_fail,
                        start => 4487,
                        'end' => 4534,
                        pattern_start => 4498,
                        pattern_end => 4504})
    end,
    gleam@regexp:check(Re@1, Input).

-file("src/xml_streamer.gleam", 145).
-spec check_for_tag_after_lt(binary()) -> tag_or_not().
check_for_tag_after_lt(After) ->
    S = splitter:new([<<" "/utf8>>, <<">"/utf8>>, <<"/>"/utf8>>, <<"?>"/utf8>>]),
    {Before, _, _} = splitter_ffi:split(S, After),
    on:true_false(
        (Before =:= <<"?xml"/utf8>>) orelse (Before =:= <<"?XML"/utf8>>),
        fun() ->
            {x_m_l_doc,
                begin
                    _pipe = Before,
                    gleam@string:drop_start(_pipe, 1)
                end}
        end,
        fun() ->
            on:true_false(
                ((Before =:= <<"!DOCTYPE"/utf8>>) orelse (Before =:= <<"!Doctype"/utf8>>))
                orelse (Before =:= <<"!doctype"/utf8>>),
                fun() ->
                    {doctype,
                        begin
                            _pipe@1 = Before,
                            gleam@string:drop_start(_pipe@1, 1)
                        end}
                end,
                fun() ->
                    on:true_false(
                        is_ordinary_tag(Before),
                        fun() -> {ordinary, Before} end,
                        fun() -> no_tag end
                    )
                end
            )
        end
    ).

-file("src/xml_streamer.gleam", 159).
-spec check_for_tag_after_lt_closing(binary()) -> tag_or_not().
check_for_tag_after_lt_closing(After) ->
    S = splitter:new([<<" "/utf8>>, <<">"/utf8>>, <<"/>"/utf8>>, <<"?>"/utf8>>]),
    {Before, _, _} = splitter_ffi:split(S, After),
    on:true_false(
        is_ordinary_tag(Before),
        fun() -> {ordinary_closing, Before} end,
        fun() -> no_tag end
    ).

-file("src/xml_streamer.gleam", 166).
-spec take_text_up_to_next_tag(binary()) -> {binary(), tag_or_not()}.
take_text_up_to_next_tag(Text) ->
    on:error_ok(
        gleam@string:split_once(Text, <<"<"/utf8>>),
        fun(_) -> {Text, no_tag} end,
        fun(_use0) ->
            {Text@1, After} = _use0,
            on:true_false(
                gleam_stdlib:string_starts_with(After, <<"/"/utf8>>),
                fun() ->
                    After@1 = gleam@string:drop_start(After, 1),
                    case check_for_tag_after_lt_closing(After@1) of
                        no_tag ->
                            {After_text, After_tag_or_not} = take_text_up_to_next_tag(
                                After@1
                            ),
                            {<<<<Text@1/binary, "</"/utf8>>/binary,
                                    After_text/binary>>,
                                After_tag_or_not};

                        Some_tag ->
                            case Some_tag of
                                {ordinary_closing, _} -> nil;
                                _assert_fail ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                                file => <<?FILEPATH/utf8>>,
                                                module => <<"xml_streamer"/utf8>>,
                                                function => <<"take_text_up_to_next_tag"/utf8>>,
                                                line => 179,
                                                value => _assert_fail,
                                                start => 5809,
                                                'end' => 5849,
                                                pattern_start => 5820,
                                                pattern_end => 5838})
                            end,
                            {Text@1, Some_tag}
                    end
                end,
                fun() ->
                    on:true_false(
                        gleam_stdlib:string_starts_with(After, <<"!--"/utf8>>),
                        fun() -> {Text@1, comment_start} end,
                        fun() -> case check_for_tag_after_lt(After) of
                                no_tag ->
                                    {After_text@1, After_tag_or_not@1} = take_text_up_to_next_tag(
                                        After
                                    ),
                                    {<<<<Text@1/binary, "<"/utf8>>/binary,
                                            After_text@1/binary>>,
                                        After_tag_or_not@1};

                                Some_tag@1 ->
                                    {Text@1, Some_tag@1}
                            end end
                    )
                end
            )
        end
    ).

-file("src/xml_streamer.gleam", 200).
-spec event_stream_internal(list(event()), state(), list(content_line())) -> list(event()).
event_stream_internal(Previous, State, Remaining) ->
    on:empty_nonempty(Remaining, fun() -> _pipe = Previous,
            lists:reverse(_pipe) end, fun(First, Rest) ->
            on:true_false(
                erlang:element(3, First) =:= <<""/utf8>>,
                fun() -> case Rest of
                        [] ->
                            _pipe@1 = Previous,
                            lists:reverse(_pipe@1);

                        _ ->
                            event_stream_internal(
                                [{newline, erlang:element(2, First)} | Previous],
                                State,
                                Rest
                            )
                    end end,
                fun() ->
                    on:true_false(
                        State =:= outside_tag,
                        fun() ->
                            {Text, Tag_or_not} = take_text_up_to_next_tag(
                                erlang:element(3, First)
                            ),
                            Previous@1 = case Text /= <<""/utf8>> of
                                true ->
                                    [{text, erlang:element(2, First), Text} |
                                        Previous];

                                false ->
                                    Previous
                            end,
                            End_of_text_blame = vxml@blame:advance(
                                erlang:element(2, First),
                                begin
                                    _pipe@2 = Text,
                                    string:length(_pipe@2)
                                end
                            ),
                            on:true_false(
                                Tag_or_not =:= no_tag,
                                fun() ->
                                    _assert_subject = erlang:element(3, First),
                                    case Text =:= _assert_subject of
                                        true -> nil;
                                        false -> erlang:error(
                                            #{gleam_error => assert,
                                                message => <<"Assertion failed."/utf8>>,
                                                file => <<?FILEPATH/utf8>>,
                                                module => <<"xml_streamer"/utf8>>,
                                                function => <<"event_stream_internal"/utf8>>,
                                                line => 228,
                                                kind => binary_operator,
                                                operator => '==',
                                                left => #{kind => expression,
                                                    value => Text,
                                                    start => 7088,
                                                    'end' => 7092
                                                    },
                                                right => #{kind => expression,
                                                    value => _assert_subject,
                                                    start => 7096,
                                                    'end' => 7109
                                                    },
                                                start => 7081,
                                                'end' => 7109,
                                                expression_start => 7088}
                                        )
                                    end,
                                    event_stream_internal(
                                        [{newline, End_of_text_blame} |
                                            Previous@1],
                                        outside_tag,
                                        Rest
                                    )
                                end,
                                fun() ->
                                    {Tag_event, Z, Tag@4, New_state} = case Tag_or_not of
                                        {x_m_l_doc, Tag} ->
                                            {{tag_start_x_m_l_version,
                                                    End_of_text_blame,
                                                    Tag},
                                                <<"<?"/utf8>>,
                                                Tag,
                                                inside_opening_tag_expecting_next_key};

                                        {doctype, Tag@1} ->
                                            {{tag_start_doctype,
                                                    End_of_text_blame,
                                                    Tag@1},
                                                <<"<!"/utf8>>,
                                                Tag@1,
                                                inside_opening_tag_expecting_next_key};

                                        {ordinary, Tag@2} ->
                                            {{tag_start_ordinary,
                                                    End_of_text_blame,
                                                    Tag@2},
                                                <<"<"/utf8>>,
                                                Tag@2,
                                                inside_opening_tag_expecting_next_key};

                                        {ordinary_closing, Tag@3} ->
                                            {{tag_start_closing,
                                                    End_of_text_blame,
                                                    Tag@3},
                                                <<"</"/utf8>>,
                                                Tag@3,
                                                inside_closing_tag};

                                        comment_start ->
                                            {{comment_start_sequence,
                                                    End_of_text_blame},
                                                <<"<!--"/utf8>>,
                                                <<""/utf8>>,
                                                inside_comment};

                                        _ ->
                                            erlang:error(#{gleam_error => panic,
                                                    message => <<"should have escaped NoTag earlier"/utf8>>,
                                                    file => <<?FILEPATH/utf8>>,
                                                    module => <<"xml_streamer"/utf8>>,
                                                    function => <<"event_stream_internal"/utf8>>,
                                                    line => 266})
                                    end,
                                    Length = string:length(
                                        <<<<Text/binary, Z/binary>>/binary,
                                            Tag@4/binary>>
                                    ),
                                    case string:length(erlang:element(3, First))
                                    < Length of
                                        true ->
                                            Msg = <<<<<<<<<<<<<<"tag split invariant failed: content="/utf8,
                                                                        (gleam@string:inspect(
                                                                            erlang:element(
                                                                                3,
                                                                                First
                                                                            )
                                                                        ))/binary>>/binary,
                                                                    ", text="/utf8>>/binary,
                                                                (gleam@string:inspect(
                                                                    Text
                                                                ))/binary>>/binary,
                                                            ", prefix="/utf8>>/binary,
                                                        (gleam@string:inspect(Z))/binary>>/binary,
                                                    ", tag="/utf8>>/binary,
                                                (gleam@string:inspect(Tag@4))/binary>>,
                                            erlang:error(#{gleam_error => panic,
                                                    message => Msg,
                                                    file => <<?FILEPATH/utf8>>,
                                                    module => <<"xml_streamer"/utf8>>,
                                                    function => <<"event_stream_internal"/utf8>>,
                                                    line => 280});

                                        false ->
                                            nil
                                    end,
                                    event_stream_internal(
                                        [Tag_event | Previous@1],
                                        New_state,
                                        [advance_line(First, Length) | Rest]
                                    )
                                end
                            )
                        end,
                        fun() ->
                            on:true_false(
                                State =:= inside_comment,
                                fun() ->
                                    case gleam@string:split_once(
                                        erlang:element(3, First),
                                        <<"-->"/utf8>>
                                    ) of
                                        {error, nil} ->
                                            event_stream_internal(
                                                [{comment_contents,
                                                        erlang:element(2, First),
                                                        erlang:element(3, First)} |
                                                    Previous],
                                                inside_comment,
                                                Rest
                                            );

                                        {ok, {Before, _}} ->
                                            case Before =:= <<""/utf8>> of
                                                true ->
                                                    event_stream_internal(
                                                        [{comment_contents,
                                                                erlang:element(
                                                                    2,
                                                                    First
                                                                ),
                                                                Before} |
                                                            Previous],
                                                        outside_tag,
                                                        [advance_line(First, 3) |
                                                            Rest]
                                                    );

                                                false ->
                                                    Length@1 = string:length(
                                                        Before
                                                    ),
                                                    event_stream_internal(
                                                        [{comment_end_sequence,
                                                                vxml@blame:advance(
                                                                    erlang:element(
                                                                        2,
                                                                        First
                                                                    ),
                                                                    Length@1
                                                                )},
                                                            {comment_contents,
                                                                erlang:element(
                                                                    2,
                                                                    First
                                                                ),
                                                                Before} |
                                                            Previous],
                                                        outside_tag,
                                                        [advance_line(
                                                                First,
                                                                Length@1 + 3
                                                            ) |
                                                            Rest]
                                                    )
                                            end
                                    end
                                end,
                                fun() ->
                                    Num_whitespace = string:length(
                                        erlang:element(3, First)
                                    )
                                    - string:length(
                                        gleam@string:trim_start(
                                            erlang:element(3, First)
                                        )
                                    ),
                                    on:true_false(
                                        Num_whitespace > 0,
                                        fun() ->
                                            Whitespace = gleam@string:slice(
                                                erlang:element(3, First),
                                                0,
                                                Num_whitespace
                                            ),
                                            event_stream_internal(
                                                [{in_tag_whitespace,
                                                        erlang:element(2, First),
                                                        Whitespace} |
                                                    Previous],
                                                State,
                                                [advance_line(
                                                        First,
                                                        Num_whitespace
                                                    ) |
                                                    Rest]
                                            )
                                        end,
                                        fun() ->
                                            on:true_false(
                                                gleam_stdlib:string_starts_with(
                                                    erlang:element(3, First),
                                                    <<"="/utf8>>
                                                ),
                                                fun() ->
                                                    event_stream_internal(
                                                        [{assignment,
                                                                erlang:element(
                                                                    2,
                                                                    First
                                                                )} |
                                                            Previous],
                                                        inside_opening_tag_expecting_next_value,
                                                        [advance_line(First, 1) |
                                                            Rest]
                                                    )
                                                end,
                                                fun() ->
                                                    on:true_false(
                                                        gleam_stdlib:string_starts_with(
                                                            erlang:element(
                                                                3,
                                                                First
                                                            ),
                                                            <<"\""/utf8>>
                                                        ),
                                                        fun() ->
                                                            S = splitter:new(
                                                                [<<"\""/utf8>>,
                                                                    <<"?>"/utf8>>,
                                                                    <<"/>"/utf8>>,
                                                                    <<">"/utf8>>]
                                                            ),
                                                            {Before@1, Thing, _} = splitter_ffi:split(
                                                                S,
                                                                begin
                                                                    _pipe@3 = erlang:element(
                                                                        3,
                                                                        First
                                                                    ),
                                                                    gleam@string:drop_start(
                                                                        _pipe@3,
                                                                        1
                                                                    )
                                                                end
                                                            ),
                                                            {Event, Taken@2} = case Thing
                                                            =:= <<"\""/utf8>> of
                                                                true ->
                                                                    Taken = <<<<"\""/utf8,
                                                                            Before@1/binary>>/binary,
                                                                        "\""/utf8>>,
                                                                    {{value_double_quoted,
                                                                            erlang:element(
                                                                                2,
                                                                                First
                                                                            ),
                                                                            Before@1},
                                                                        Taken};

                                                                false ->
                                                                    Taken@1 = <<"\""/utf8,
                                                                        Before@1/binary>>,
                                                                    {{value_malformed,
                                                                            erlang:element(
                                                                                2,
                                                                                First
                                                                            ),
                                                                            Taken@1},
                                                                        Taken@1}
                                                            end,
                                                            event_stream_internal(
                                                                [Event |
                                                                    Previous],
                                                                inside_opening_tag_expecting_next_key,
                                                                [advance_line(
                                                                        First,
                                                                        begin
                                                                            _pipe@4 = Taken@2,
                                                                            string:length(
                                                                                _pipe@4
                                                                            )
                                                                        end
                                                                    ) |
                                                                    Rest]
                                                            )
                                                        end,
                                                        fun() ->
                                                            on:true_false(
                                                                gleam_stdlib:string_starts_with(
                                                                    erlang:element(
                                                                        3,
                                                                        First
                                                                    ),
                                                                    <<"'"/utf8>>
                                                                ),
                                                                fun() ->
                                                                    S@1 = splitter:new(
                                                                        [<<"'"/utf8>>,
                                                                            <<"?>"/utf8>>,
                                                                            <<"/>"/utf8>>,
                                                                            <<">"/utf8>>]
                                                                    ),
                                                                    {Before@2,
                                                                        Thing@1,
                                                                        _} = splitter_ffi:split(
                                                                        S@1,
                                                                        begin
                                                                            _pipe@5 = erlang:element(
                                                                                3,
                                                                                First
                                                                            ),
                                                                            gleam@string:drop_start(
                                                                                _pipe@5,
                                                                                1
                                                                            )
                                                                        end
                                                                    ),
                                                                    {Event@1,
                                                                        Taken@5} = case Thing@1
                                                                    =:= <<"'"/utf8>> of
                                                                        true ->
                                                                            Taken@3 = <<<<"'"/utf8,
                                                                                    Before@2/binary>>/binary,
                                                                                "'"/utf8>>,
                                                                            {{value_single_quoted,
                                                                                    erlang:element(
                                                                                        2,
                                                                                        First
                                                                                    ),
                                                                                    Before@2},
                                                                                Taken@3};

                                                                        false ->
                                                                            Taken@4 = <<"'"/utf8,
                                                                                Before@2/binary>>,
                                                                            {{value_malformed,
                                                                                    erlang:element(
                                                                                        2,
                                                                                        First
                                                                                    ),
                                                                                    Taken@4},
                                                                                Taken@4}
                                                                    end,
                                                                    event_stream_internal(
                                                                        [Event@1 |
                                                                            Previous],
                                                                        inside_opening_tag_expecting_next_key,
                                                                        [advance_line(
                                                                                First,
                                                                                begin
                                                                                    _pipe@6 = Taken@5,
                                                                                    string:length(
                                                                                        _pipe@6
                                                                                    )
                                                                                end
                                                                            ) |
                                                                            Rest]
                                                                    )
                                                                end,
                                                                fun() ->
                                                                    S@2 = splitter:new(
                                                                        [<<"="/utf8>>,
                                                                            <<" "/utf8>>,
                                                                            <<"/>"/utf8>>,
                                                                            <<"?>"/utf8>>,
                                                                            <<">"/utf8>>]
                                                                    ),
                                                                    {Before@3,
                                                                        Thing@2,
                                                                        _} = splitter_ffi:split(
                                                                        S@2,
                                                                        erlang:element(
                                                                            3,
                                                                            First
                                                                        )
                                                                    ),
                                                                    on:true_false(
                                                                        Before@3
                                                                        /= <<""/utf8>>,
                                                                        fun() ->
                                                                            Event@2 = case is_valid_key(
                                                                                Before@3
                                                                            ) of
                                                                                true ->
                                                                                    {key,
                                                                                        erlang:element(
                                                                                            2,
                                                                                            First
                                                                                        ),
                                                                                        Before@3};

                                                                                false ->
                                                                                    {key_malformed,
                                                                                        erlang:element(
                                                                                            2,
                                                                                            First
                                                                                        ),
                                                                                        Before@3}
                                                                            end,
                                                                            event_stream_internal(
                                                                                [Event@2 |
                                                                                    Previous],
                                                                                inside_opening_tag_expecting_next_assignment,
                                                                                [advance_line(
                                                                                        First,
                                                                                        begin
                                                                                            _pipe@7 = Before@3,
                                                                                            string:length(
                                                                                                _pipe@7
                                                                                            )
                                                                                        end
                                                                                    ) |
                                                                                    Rest]
                                                                            )
                                                                        end,
                                                                        fun() ->
                                                                            _assert_subject@1 = <<""/utf8>>,
                                                                            case Before@3
                                                                            =:= _assert_subject@1 of
                                                                                true -> nil;
                                                                                false -> erlang:error(
                                                                                    #{gleam_error => assert,
                                                                                        message => <<"Assertion failed."/utf8>>,
                                                                                        file => <<?FILEPATH/utf8>>,
                                                                                        module => <<"xml_streamer"/utf8>>,
                                                                                        function => <<"event_stream_internal"/utf8>>,
                                                                                        line => 408,
                                                                                        kind => binary_operator,
                                                                                        operator => '==',
                                                                                        left => #{kind => expression,
                                                                                            value => Before@3,
                                                                                            start => 12211,
                                                                                            'end' => 12217
                                                                                            },
                                                                                        right => #{kind => literal,
                                                                                            value => _assert_subject@1,
                                                                                            start => 12221,
                                                                                            'end' => 12223
                                                                                            },
                                                                                        start => 12204,
                                                                                        'end' => 12223,
                                                                                        expression_start => 12211}
                                                                                )
                                                                            end,
                                                                            case (Thing@2
                                                                                =:= <<"/>"/utf8>>)
                                                                                orelse (Thing@2
                                                                                =:= <<"?>"/utf8>>) orelse Thing@2
                                                                                =:= <<">"/utf8>> of
                                                                                true -> nil;
                                                                                false -> erlang:error(
                                                                                    #{gleam_error => assert,
                                                                                        message => <<"Assertion failed."/utf8>>,
                                                                                        file => <<?FILEPATH/utf8>>,
                                                                                        module => <<"xml_streamer"/utf8>>,
                                                                                        function => <<"event_stream_internal"/utf8>>,
                                                                                        line => 409,
                                                                                        kind => binary_operator,
                                                                                        operator => '||',
                                                                                        left => #{kind => expression,
                                                                                            value => false,
                                                                                            start => 12233,
                                                                                            'end' => 12263
                                                                                            },
                                                                                        right => #{kind => expression,
                                                                                            value => false,
                                                                                            start => 12267,
                                                                                            'end' => 12279
                                                                                            },
                                                                                        start => 12226,
                                                                                        'end' => 12279,
                                                                                        expression_start => 12233}
                                                                                )
                                                                            end,
                                                                            case Thing@2 of
                                                                                <<"/>"/utf8>> ->
                                                                                    event_stream_internal(
                                                                                        [{tag_end_self_closing,
                                                                                                erlang:element(
                                                                                                    2,
                                                                                                    First
                                                                                                )} |
                                                                                            Previous],
                                                                                        outside_tag,
                                                                                        [advance_line(
                                                                                                First,
                                                                                                2
                                                                                            ) |
                                                                                            Rest]
                                                                                    );

                                                                                <<"?>"/utf8>> ->
                                                                                    event_stream_internal(
                                                                                        [{tag_end_x_m_l_version,
                                                                                                erlang:element(
                                                                                                    2,
                                                                                                    First
                                                                                                )} |
                                                                                            Previous],
                                                                                        outside_tag,
                                                                                        [advance_line(
                                                                                                First,
                                                                                                2
                                                                                            ) |
                                                                                            Rest]
                                                                                    );

                                                                                <<">"/utf8>> ->
                                                                                    event_stream_internal(
                                                                                        [{tag_end_ordinary,
                                                                                                erlang:element(
                                                                                                    2,
                                                                                                    First
                                                                                                )} |
                                                                                            Previous],
                                                                                        outside_tag,
                                                                                        [advance_line(
                                                                                                First,
                                                                                                1
                                                                                            ) |
                                                                                            Rest]
                                                                                    );

                                                                                _ ->
                                                                                    erlang:error(
                                                                                        #{gleam_error => panic,
                                                                                            message => <<"unexpected tag ending delimiter"/utf8>>,
                                                                                            file => <<?FILEPATH/utf8>>,
                                                                                            module => <<"xml_streamer"/utf8>>,
                                                                                            function => <<"event_stream_internal"/utf8>>,
                                                                                            line => 430}
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
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end).

-file("src/xml_streamer.gleam", 434).
-spec input_lines_to_content_lines(list(vxml@io_lines:input_line())) -> list(content_line()).
input_lines_to_content_lines(Lines) ->
    gleam@list:map(
        Lines,
        fun(Line) ->
            {content_line,
                begin
                    _pipe = erlang:element(2, Line),
                    vxml@blame:advance(_pipe, - erlang:element(3, Line))
                end,
                <<(gleam@string:repeat(<<" "/utf8>>, erlang:element(3, Line)))/binary,
                    (erlang:element(4, Line))/binary>>}
        end
    ).

-file("src/xml_streamer.gleam", 444).
?DOC(" Stream XML tokens from input lines.\n").
-spec input_lines_streamer(list(vxml@io_lines:input_line())) -> list(event()).
input_lines_streamer(Lines) ->
    _pipe = Lines,
    _pipe@1 = input_lines_to_content_lines(_pipe),
    event_stream_internal([], outside_tag, _pipe@1).
