-module(xmlm).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/xmlm.gleam").
-export([uchar_utf8/1, name_to_string/1, attribute_to_string/1, attributes_to_string/1, tag_to_string/1, signal_to_string/1, signals_to_string/1, input_error_to_string/1, input_stream_from_bit_array/1, input_to_string/1, from_bit_array/1, from_string/1, with_encoding/2, with_stripping/2, with_namespace_callback/2, with_entity_callback/2, eoi/1, peek/1, signal/1, signals/1, fold_signals/3, tree/3, document_tree/3]).
-export_type([unicode_lexer_error/0, encoding/0, name/0, attribute/0, tag/0, signal/0, position/0, internal_input_error/0, input_error/0, limit/0, input/0, loop_done/0, stop_or_go/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " For usage examples and strategies for working with this library, check out \n"
    " the programs located in the `test/examples` directory.\n"
    " \n"
    " ## Notes\n"
    " \n"
    " - Don't forget to work with the `Input` that is returned by any \n"
    "   \"inputting\" function rather than the original.\n"
    " - If something is marked as being \"unspecified\", do not depend on it.  \n"
    "   It may change at any time without a major version bump.  This mainly \n"
    "   applies to the various `*_to_string` functions.\n"
    "\n"
).

-type unicode_lexer_error() :: unicode_lexer_eoi | unicode_lexer_malformed.

-type encoding() :: utf8 |
    utf16 |
    utf16_be |
    utf16_le |
    iso8859x1 |
    iso8859x15 |
    us_ascii.

-type name() :: {name, binary(), binary()}.

-type attribute() :: {attribute, name(), binary()}.

-type tag() :: {tag, name(), list(attribute())}.

-type signal() :: {dtd, gleam@option:option(binary())} |
    {element_start, tag()} |
    element_end |
    {data, binary()}.

-type position() :: {position, integer(), integer()}.

-type internal_input_error() :: {expected_char_seqs, list(binary()), binary()} |
    expected_root_element |
    {illegal_char_ref, binary()} |
    {illegal_char_seq, binary()} |
    malformed_char_stream |
    max_buffer_size |
    unexpected_eoi |
    {unknown_encoding, binary()} |
    {unknown_entity_ref, binary()} |
    {unknown_ns_prefix, binary()} |
    unicode_lexer_error_eoi |
    unicode_lexer_error_malformed |
    {invalid_argument, binary()}.

-opaque input_error() :: {input_error, position(), internal_input_error()}.

-type limit() :: {limit_start_tag, name()} |
    {limit_end_tag, name()} |
    {limit_pi, name()} |
    limit_comment |
    limit_c_data |
    limit_dtd |
    limit_text |
    limit_eoi.

-opaque input() :: {input,
        gleam@option:option(encoding()),
        boolean(),
        fun((binary()) -> gleam@option:option(binary())),
        fun((binary()) -> gleam@option:option(binary())),
        fun((input()) -> {ok, {integer(), input()}} | {error, input_error()}),
        list(integer()),
        integer(),
        boolean(),
        integer(),
        integer(),
        limit(),
        signal(),
        boolean(),
        boolean(),
        list({name(), list(binary()), boolean()}),
        gleam@dict:dict(binary(), binary()),
        list(integer()),
        list(integer())}.

-type loop_done() :: {loop_done_exited, {integer(), input()}} |
    {loop_done_by_condition, {integer(), input()}}.

-type stop_or_go() :: go | stop.

-file("src/xmlm.gleam", 695).
-spec internal_input_error_from_unicode_lexer_error(unicode_lexer_error()) -> internal_input_error().
internal_input_error_from_unicode_lexer_error(Unicode_lexer_error) ->
    case Unicode_lexer_error of
        unicode_lexer_eoi ->
            unicode_lexer_error_eoi;

        unicode_lexer_malformed ->
            unicode_lexer_error_malformed
    end.

-file("src/xmlm.gleam", 743).
-spec input_error_new(input(), internal_input_error()) -> input_error().
input_error_new(Input, Input_error) ->
    {input_error,
        {position, erlang:element(10, Input), erlang:element(11, Input)},
        Input_error}.

-file("src/xmlm.gleam", 154).
-spec make_input_uchar(
    fun((list(integer())) -> {ok, {integer(), list(integer())}} |
        {error, unicode_lexer_error()})
) -> fun((input()) -> {ok, {integer(), input()}} | {error, input_error()}).
make_input_uchar(Uchar_lexer) ->
    fun(Input) -> case Uchar_lexer(erlang:element(7, Input)) of
            {error, E} ->
                {error,
                    input_error_new(
                        Input,
                        internal_input_error_from_unicode_lexer_error(E)
                    )};

            {ok, {Uchar, Stream}} ->
                Input@1 = {input,
                    erlang:element(2, Input),
                    erlang:element(3, Input),
                    erlang:element(4, Input),
                    erlang:element(5, Input),
                    erlang:element(6, Input),
                    Stream,
                    erlang:element(8, Input),
                    erlang:element(9, Input),
                    erlang:element(10, Input),
                    erlang:element(11, Input),
                    erlang:element(12, Input),
                    erlang:element(13, Input),
                    erlang:element(14, Input),
                    erlang:element(15, Input),
                    erlang:element(16, Input),
                    erlang:element(17, Input),
                    erlang:element(18, Input),
                    erlang:element(19, Input)},
                {ok, {Uchar, Input@1}}
        end end.

-file("src/xmlm.gleam", 177).
-spec uchar_byte(list(integer())) -> {ok, {integer(), list(integer())}} |
    {error, unicode_lexer_error()}.
uchar_byte(Stream) ->
    case Stream of
        [Byte | Rest] ->
            {ok, {Byte, Rest}};

        [] ->
            {error, unicode_lexer_eoi}
    end.

-file("src/xmlm.gleam", 173).
?DOC(" Gets the next uchar byte advancing any needed internal state.\n").
-spec input_uchar_byte() -> fun((input()) -> {ok, {integer(), input()}} |
    {error, input_error()}).
input_uchar_byte() ->
    make_input_uchar(fun uchar_byte/1).

-file("src/xmlm.gleam", 190).
-spec uchar_iso_8859_1(list(integer())) -> {ok, {integer(), list(integer())}} |
    {error, unicode_lexer_error()}.
uchar_iso_8859_1(Stream) ->
    case Stream of
        [Byte | Rest] ->
            {ok, {Byte, Rest}};

        [] ->
            {error, unicode_lexer_eoi}
    end.

-file("src/xmlm.gleam", 186).
-spec input_uchar_iso_8859_1() -> fun((input()) -> {ok, {integer(), input()}} |
    {error, input_error()}).
input_uchar_iso_8859_1() ->
    make_input_uchar(fun uchar_iso_8859_1/1).

-file("src/xmlm.gleam", 204).
-spec uchar_iso_8859_15(list(integer())) -> {ok, {integer(), list(integer())}} |
    {error, unicode_lexer_error()}.
uchar_iso_8859_15(Stream) ->
    case Stream of
        [16#00A4 | Rest] ->
            {ok, {16#20AC, Rest}};

        [16#00A6 | Rest@1] ->
            {ok, {16#0160, Rest@1}};

        [16#00A8 | Rest@2] ->
            {ok, {16#0161, Rest@2}};

        [16#00B4 | Rest@3] ->
            {ok, {16#017D, Rest@3}};

        [16#00B8 | Rest@4] ->
            {ok, {16#017E, Rest@4}};

        [16#00BC | Rest@5] ->
            {ok, {16#0152, Rest@5}};

        [16#00BD | Rest@6] ->
            {ok, {16#0153, Rest@6}};

        [16#00BE | Rest@7] ->
            {ok, {16#0178, Rest@7}};

        [Char | Rest@8] ->
            {ok, {Char, Rest@8}};

        [] ->
            {error, unicode_lexer_eoi}
    end.

-file("src/xmlm.gleam", 199).
-spec input_uchar_iso_8859_15() -> fun((input()) -> {ok, {integer(), input()}} |
    {error, input_error()}).
input_uchar_iso_8859_15() ->
    make_input_uchar(fun uchar_iso_8859_15/1).

-file("src/xmlm.gleam", 379).
?DOC(" Are the most significant bits of an 8 bit int NOT `10`?\n").
-spec most_significant_bytes_are_not_10(integer()) -> boolean().
most_significant_bytes_are_not_10(N) ->
    erlang:'band'(N, 2#11000000) /= 2#10000000.

-file("src/xmlm.gleam", 322).
-spec do_uchar_utf8_len4(list(integer()), integer()) -> {ok,
        {integer(), list(integer())}} |
    {error, unicode_lexer_error()}.
do_uchar_utf8_len4(Stream, Byte0) ->
    case Stream of
        [Byte1, Byte2, Byte3 | Stream@1] ->
            case most_significant_bytes_are_not_10(Byte3) orelse most_significant_bytes_are_not_10(
                Byte2
            ) of
                true ->
                    {error, unicode_lexer_malformed};

                false ->
                    case (Byte0 =:= 16#F0) andalso ((Byte1 < 16#90) orelse (16#BF
                    < Byte1)) of
                        true ->
                            {error, unicode_lexer_malformed};

                        false ->
                            case (Byte0 =:= 16#F4) andalso ((Byte1 < 16#80)
                            orelse (16#8F < Byte1)) of
                                true ->
                                    {error, unicode_lexer_malformed};

                                false ->
                                    case begin
                                        _pipe = Byte1,
                                        most_significant_bytes_are_not_10(_pipe)
                                    end of
                                        true ->
                                            {error, unicode_lexer_malformed};

                                        false ->
                                            B0 = begin
                                                _pipe@1 = Byte0,
                                                _pipe@2 = erlang:'band'(
                                                    _pipe@1,
                                                    16#07
                                                ),
                                                erlang:'bsl'(_pipe@2, 18)
                                            end,
                                            B1 = begin
                                                _pipe@3 = Byte1,
                                                _pipe@4 = erlang:'band'(
                                                    _pipe@3,
                                                    16#3F
                                                ),
                                                erlang:'bsl'(_pipe@4, 12)
                                            end,
                                            B2 = begin
                                                _pipe@5 = Byte2,
                                                _pipe@6 = erlang:'band'(
                                                    _pipe@5,
                                                    16#3F
                                                ),
                                                erlang:'bsl'(_pipe@6, 6)
                                            end,
                                            B3 = begin
                                                _pipe@7 = Byte3,
                                                erlang:'band'(_pipe@7, 16#3F)
                                            end,
                                            Result = begin
                                                _pipe@8 = B0,
                                                _pipe@9 = erlang:'bor'(
                                                    _pipe@8,
                                                    B1
                                                ),
                                                _pipe@10 = erlang:'bor'(
                                                    _pipe@9,
                                                    B2
                                                ),
                                                erlang:'bor'(_pipe@10, B3)
                                            end,
                                            {ok, {Result, Stream@1}}
                                    end
                            end
                    end
            end;

        [] ->
            {error, unicode_lexer_eoi};

        _ ->
            {error, unicode_lexer_malformed}
    end.

-file("src/xmlm.gleam", 276).
-spec do_uchar_utf8_len3(list(integer()), integer()) -> {ok,
        {integer(), list(integer())}} |
    {error, unicode_lexer_error()}.
do_uchar_utf8_len3(Stream, Byte0) ->
    case Stream of
        [Byte1, Byte2 | Stream@1] ->
            case begin
                _pipe = Byte2,
                most_significant_bytes_are_not_10(_pipe)
            end of
                true ->
                    {error, unicode_lexer_malformed};

                false ->
                    case (Byte0 =:= 16#E0) andalso ((Byte1 < 16#A0) orelse (16#BF
                    < Byte1)) of
                        true ->
                            {error, unicode_lexer_malformed};

                        false ->
                            case (Byte0 =:= 16#ED) andalso ((Byte1 < 16#80)
                            orelse (16#9F < Byte1)) of
                                true ->
                                    {error, unicode_lexer_malformed};

                                false ->
                                    case begin
                                        _pipe@1 = Byte1,
                                        most_significant_bytes_are_not_10(
                                            _pipe@1
                                        )
                                    end of
                                        true ->
                                            {error, unicode_lexer_malformed};

                                        false ->
                                            B0 = begin
                                                _pipe@2 = Byte0,
                                                _pipe@3 = erlang:'band'(
                                                    _pipe@2,
                                                    16#0F
                                                ),
                                                erlang:'bsl'(_pipe@3, 12)
                                            end,
                                            B1 = begin
                                                _pipe@4 = Byte1,
                                                _pipe@5 = erlang:'band'(
                                                    _pipe@4,
                                                    16#3F
                                                ),
                                                erlang:'bsl'(_pipe@5, 6)
                                            end,
                                            B2 = begin
                                                _pipe@6 = Byte2,
                                                erlang:'band'(_pipe@6, 16#3F)
                                            end,
                                            Result = begin
                                                _pipe@7 = B0,
                                                _pipe@8 = erlang:'bor'(
                                                    _pipe@7,
                                                    B1
                                                ),
                                                erlang:'bor'(_pipe@8, B2)
                                            end,
                                            {ok, {Result, Stream@1}}
                                    end
                            end
                    end
            end;

        [] ->
            {error, unicode_lexer_eoi};

        _ ->
            {error, unicode_lexer_malformed}
    end.

-file("src/xmlm.gleam", 254).
-spec do_uchar_utf8_len2(list(integer()), integer()) -> {ok,
        {integer(), list(integer())}} |
    {error, unicode_lexer_error()}.
do_uchar_utf8_len2(Stream, Byte0) ->
    case Stream of
        [Byte1 | Stream@1] ->
            case begin
                _pipe = Byte1,
                most_significant_bytes_are_not_10(_pipe)
            end of
                true ->
                    {error, unicode_lexer_malformed};

                false ->
                    Result = begin
                        _pipe@1 = erlang:'band'(Byte0, 16#1F),
                        _pipe@2 = erlang:'bsl'(_pipe@1, 6),
                        erlang:'bor'(_pipe@2, erlang:'band'(Byte1, 16#3F))
                    end,
                    {ok, {Result, Stream@1}}
            end;

        [] ->
            {error, unicode_lexer_eoi}
    end.

-file("src/xmlm.gleam", 236).
?DOC(false).
-spec uchar_utf8(list(integer())) -> {ok, {integer(), list(integer())}} |
    {error, unicode_lexer_error()}.
uchar_utf8(Stream) ->
    case Stream of
        [Byte0 | Bytes] ->
            case Byte0 of
                N when (0 =< N) andalso (N =< 127) ->
                    {ok, {Byte0, Bytes}};

                N@1 when (128 =< N@1) andalso (N@1 =< 193) ->
                    {error, unicode_lexer_malformed};

                N@2 when (194 =< N@2) andalso (N@2 =< 223) ->
                    do_uchar_utf8_len2(Bytes, Byte0);

                N@3 when (224 =< N@3) andalso (N@3 =< 239) ->
                    do_uchar_utf8_len3(Bytes, Byte0);

                N@4 when (240 =< N@4) andalso (N@4 =< 244) ->
                    do_uchar_utf8_len4(Bytes, Byte0);

                _ ->
                    {error, unicode_lexer_malformed}
            end;

        [] ->
            {error, unicode_lexer_eoi}
    end.

-file("src/xmlm.gleam", 231).
-spec input_uchar_utf8() -> fun((input()) -> {ok, {integer(), input()}} |
    {error, input_error()}).
input_uchar_utf8() ->
    make_input_uchar(fun uchar_utf8/1).

-file("src/xmlm.gleam", 389).
-spec int16_be(list(integer())) -> {ok, {integer(), list(integer())}} |
    {error, unicode_lexer_error()}.
int16_be(Stream) ->
    case Stream of
        [Byte0, Byte1 | Stream@1] ->
            Char = begin
                _pipe = Byte0,
                _pipe@1 = erlang:'bsl'(_pipe, 8),
                erlang:'bor'(_pipe@1, Byte1)
            end,
            {ok, {Char, Stream@1}};

        [] ->
            {error, unicode_lexer_eoi};

        _ ->
            {error, unicode_lexer_malformed}
    end.

-file("src/xmlm.gleam", 402).
-spec int16_le(list(integer())) -> {ok, {integer(), list(integer())}} |
    {error, unicode_lexer_error()}.
int16_le(Stream) ->
    case Stream of
        [Byte0, Byte1 | Stream@1] ->
            Char = begin
                _pipe = Byte1,
                _pipe@1 = erlang:'bsl'(_pipe, 8),
                erlang:'bor'(_pipe@1, Byte0)
            end,
            {ok, {Char, Stream@1}};

        [] ->
            {error, unicode_lexer_eoi};

        _ ->
            {error, unicode_lexer_malformed}
    end.

-file("src/xmlm.gleam", 415).
-spec uchar_utf16(
    fun((list(integer())) -> {ok, {integer(), list(integer())}} |
        {error, unicode_lexer_error()})
) -> fun((list(integer())) -> {ok, {integer(), list(integer())}} |
    {error, unicode_lexer_error()}).
uchar_utf16(Int16) ->
    fun(Stream) -> case Int16(Stream) of
            {error, E} ->
                {error, E};

            {ok, {Char0, Stream@1}} ->
                case Char0 of
                    Char0@1 when (Char0@1 < 16#D800) orelse (Char0@1 > 16#DFFF) ->
                        {ok, {Char0@1, Stream@1}};

                    Char0@2 when Char0@2 > 16#DBFF ->
                        {error, unicode_lexer_malformed};

                    Char0@3 ->
                        case Int16(Stream@1) of
                            {error, E@1} ->
                                {error, E@1};

                            {ok, {Char1, Stream@2}} ->
                                Char = erlang:'bor'(
                                    erlang:'bsl'(
                                        erlang:'band'(Char0@3, 16#3FF),
                                        10
                                    ),
                                    erlang:'band'(Char1, 16#3FF)
                                )
                                + 16#10000,
                                {ok, {Char, Stream@2}}
                        end
                end
        end end.

-file("src/xmlm.gleam", 446).
-spec input_uchar_utf16be() -> fun((input()) -> {ok, {integer(), input()}} |
    {error, input_error()}).
input_uchar_utf16be() ->
    make_input_uchar(uchar_utf16(fun int16_be/1)).

-file("src/xmlm.gleam", 450).
-spec input_uchar_utf16le() -> fun((input()) -> {ok, {integer(), input()}} |
    {error, input_error()}).
input_uchar_utf16le() ->
    make_input_uchar(uchar_utf16(fun int16_le/1)).

-file("src/xmlm.gleam", 458).
-spec uchar_ascii(list(integer())) -> {ok, {integer(), list(integer())}} |
    {error, unicode_lexer_error()}.
uchar_ascii(Stream) ->
    case Stream of
        [Byte | Rest] when Byte =< 127 ->
            {ok, {Byte, Rest}};

        [] ->
            {error, unicode_lexer_eoi};

        _ ->
            {error, unicode_lexer_malformed}
    end.

-file("src/xmlm.gleam", 454).
-spec input_uchar_ascii() -> fun((input()) -> {ok, {integer(), input()}} |
    {error, input_error()}).
input_uchar_ascii() ->
    make_input_uchar(fun uchar_ascii/1).

-file("src/xmlm.gleam", 539).
?DOC(" Convert `name` into an unspecified string representation.\n").
-spec name_to_string(name()) -> binary().
name_to_string(Name) ->
    {name, Uri, Local} = Name,
    case Uri of
        <<""/utf8>> ->
            gleam@string:inspect(Local);

        Uri@1 ->
            gleam@string:inspect(
                <<<<Uri@1/binary, ":"/utf8>>/binary, Local/binary>>
            )
    end.

-file("src/xmlm.gleam", 569).
?DOC(" Convert `attribute` into an unspecified string representation.\n").
-spec attribute_to_string(attribute()) -> binary().
attribute_to_string(Attribute) ->
    <<<<<<<<"Attribute(name: "/utf8,
                    (name_to_string(erlang:element(2, Attribute)))/binary>>/binary,
                ", value: "/utf8>>/binary,
            (gleam@string:inspect(erlang:element(3, Attribute)))/binary>>/binary,
        ")"/utf8>>.

-file("src/xmlm.gleam", 579).
?DOC(" Convert `attributes` into an unspecified string representation.\n").
-spec attributes_to_string(list(attribute())) -> binary().
attributes_to_string(Attributes) ->
    case Attributes of
        [] ->
            <<"[]"/utf8>>;

        Attributes@1 ->
            <<<<"["/utf8,
                    (gleam@string:join(
                        gleam@list:map(Attributes@1, fun attribute_to_string/1),
                        <<", "/utf8>>
                    ))/binary>>/binary,
                "]"/utf8>>
    end.

-file("src/xmlm.gleam", 602).
?DOC(" Convert `tag` into an unspecified string representation.\n").
-spec tag_to_string(tag()) -> binary().
tag_to_string(Tag) ->
    <<<<<<<<"Tag(name: "/utf8, (name_to_string(erlang:element(2, Tag)))/binary>>/binary,
                ", attributes: "/utf8>>/binary,
            (attributes_to_string(erlang:element(3, Tag)))/binary>>/binary,
        ")"/utf8>>.

-file("src/xmlm.gleam", 637).
?DOC(" Convert `signal` into an unspecified string representation.\n").
-spec signal_to_string(signal()) -> binary().
signal_to_string(Signal) ->
    case Signal of
        {dtd, {some, Data}} ->
            <<<<"Dtd("/utf8, Data/binary>>/binary, ")"/utf8>>;

        {dtd, none} ->
            <<"Dtd(None)"/utf8>>;

        {element_start, Tag} ->
            <<<<"ElementStart("/utf8, (tag_to_string(Tag))/binary>>/binary,
                ")"/utf8>>;

        element_end ->
            <<"ElementEnd"/utf8>>;

        {data, Data@1} ->
            <<<<"Data("/utf8, (gleam@string:inspect(Data@1))/binary>>/binary,
                ")"/utf8>>
    end.

-file("src/xmlm.gleam", 649).
?DOC(" Convert `signals` into an unspecified string representation.\n").
-spec signals_to_string(list(signal())) -> binary().
signals_to_string(Signals) ->
    gleam@string:join(
        gleam@list:map(Signals, fun signal_to_string/1),
        <<"\n"/utf8>>
    ).

-file("src/xmlm.gleam", 653).
-spec signal_start_stream() -> signal().
signal_start_stream() ->
    {data, <<""/utf8>>}.

-file("src/xmlm.gleam", 669).
?DOC(" Convert `position` into an unspecified string representation.\n").
-spec position_to_string(position()) -> binary().
position_to_string(Position) ->
    <<<<<<<<"Position(line: "/utf8,
                    (erlang:integer_to_binary(erlang:element(2, Position)))/binary>>/binary,
                ", column: "/utf8>>/binary,
            (erlang:integer_to_binary(erlang:element(3, Position)))/binary>>/binary,
        ")"/utf8>>.

-file("src/xmlm.gleam", 704).
-spec internal_error_message(internal_input_error()) -> binary().
internal_error_message(Input_error) ->
    Bracket = fun(L, V, R) -> <<<<L/binary, V/binary>>/binary, R/binary>> end,
    case Input_error of
        {expected_char_seqs, Expected, Actual} ->
            Expected@1 = gleam@list:fold(
                Expected,
                <<""/utf8>>,
                fun(Acc, V@1) ->
                    <<Acc/binary,
                        (Bracket(<<"\""/utf8>>, V@1, <<"\", "/utf8>>))/binary>>
                end
            ),
            <<<<<<<<"expected one of these character sequence: "/utf8,
                            Expected@1/binary>>/binary,
                        "found \""/utf8>>/binary,
                    Actual/binary>>/binary,
                "\""/utf8>>;

        expected_root_element ->
            <<"expected root element"/utf8>>;

        {illegal_char_ref, Msg} ->
            Bracket(
                <<"illegal character reference (#"/utf8>>,
                Msg,
                <<")"/utf8>>
            );

        {illegal_char_seq, Msg@1} ->
            Bracket(
                <<"character sequence illegal here (\""/utf8>>,
                Msg@1,
                <<"\")"/utf8>>
            );

        malformed_char_stream ->
            <<"malformed character stream"/utf8>>;

        max_buffer_size ->
            <<"maximal buffer size exceeded"/utf8>>;

        unexpected_eoi ->
            <<"unexpected end of input"/utf8>>;

        {unknown_encoding, Msg@2} ->
            Bracket(<<"unknown encoding ("/utf8>>, Msg@2, <<")"/utf8>>);

        {unknown_entity_ref, Msg@3} ->
            Bracket(<<"unknown entity reference ("/utf8>>, Msg@3, <<")"/utf8>>);

        {unknown_ns_prefix, Msg@4} ->
            Bracket(<<"unknown namespace prefix ("/utf8>>, Msg@4, <<")"/utf8>>);

        unicode_lexer_error_eoi ->
            <<"unicode lexer error eoi"/utf8>>;

        unicode_lexer_error_malformed ->
            <<"unicode lexer error malformed"/utf8>>;

        {invalid_argument, Msg@5} ->
            Bracket(<<"invalid argument ("/utf8>>, Msg@5, <<")"/utf8>>)
    end.

-file("src/xmlm.gleam", 749).
?DOC(" Converts the `input_error` into a non-specified human readable format.\n").
-spec input_error_to_string(input_error()) -> binary().
input_error_to_string(Input_error) ->
    {input_error, Position, Input_error@1} = Input_error,
    <<<<<<"ERROR "/utf8, (position_to_string(Position))/binary>>/binary,
            " "/utf8>>/binary,
        (internal_error_message(Input_error@1))/binary>>.

-file("src/xmlm.gleam", 784).
?DOC(false).
-spec input_stream_from_bit_array(bitstring()) -> list(integer()).
input_stream_from_bit_array(Bit_array) ->
    xmlm_ffi:bit_array_to_list(Bit_array).

-file("src/xmlm.gleam", 831).
?DOC(" Convert `input` into an unspecified string representation.\n").
-spec input_to_string(input()) -> binary().
input_to_string(Input) ->
    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"Input(\n\tencoding: "/utf8,
                                                                                                                            (gleam@string:inspect(
                                                                                                                                erlang:element(
                                                                                                                                    2,
                                                                                                                                    Input
                                                                                                                                )
                                                                                                                            ))/binary>>/binary,
                                                                                                                        "\n\tstrip: "/utf8>>/binary,
                                                                                                                    (gleam@string:inspect(
                                                                                                                        erlang:element(
                                                                                                                            3,
                                                                                                                            Input
                                                                                                                        )
                                                                                                                    ))/binary>>/binary,
                                                                                                                "\n\tstream: "/utf8>>/binary,
                                                                                                            (gleam@string:inspect(
                                                                                                                erlang:element(
                                                                                                                    7,
                                                                                                                    Input
                                                                                                                )
                                                                                                            ))/binary>>/binary,
                                                                                                        "\n\tchar: "/utf8>>/binary,
                                                                                                    (gleam@string:inspect(
                                                                                                        erlang:element(
                                                                                                            8,
                                                                                                            Input
                                                                                                        )
                                                                                                    ))/binary>>/binary,
                                                                                                "\n\tcr: "/utf8>>/binary,
                                                                                            (gleam@string:inspect(
                                                                                                erlang:element(
                                                                                                    9,
                                                                                                    Input
                                                                                                )
                                                                                            ))/binary>>/binary,
                                                                                        "\n\tline: "/utf8>>/binary,
                                                                                    (gleam@string:inspect(
                                                                                        erlang:element(
                                                                                            10,
                                                                                            Input
                                                                                        )
                                                                                    ))/binary>>/binary,
                                                                                "\n\tcolumn: "/utf8>>/binary,
                                                                            (gleam@string:inspect(
                                                                                erlang:element(
                                                                                    11,
                                                                                    Input
                                                                                )
                                                                            ))/binary>>/binary,
                                                                        "\n\tlimit: "/utf8>>/binary,
                                                                    (gleam@string:inspect(
                                                                        erlang:element(
                                                                            12,
                                                                            Input
                                                                        )
                                                                    ))/binary>>/binary,
                                                                "\n\tpeek: "/utf8>>/binary,
                                                            (gleam@string:inspect(
                                                                erlang:element(
                                                                    13,
                                                                    Input
                                                                )
                                                            ))/binary>>/binary,
                                                        "\n\tstripping: "/utf8>>/binary,
                                                    (gleam@string:inspect(
                                                        erlang:element(
                                                            14,
                                                            Input
                                                        )
                                                    ))/binary>>/binary,
                                                "\n\tlast_whitespace: "/utf8>>/binary,
                                            (gleam@string:inspect(
                                                erlang:element(15, Input)
                                            ))/binary>>/binary,
                                        "\n\tscopes: "/utf8>>/binary,
                                    (gleam@string:inspect(
                                        erlang:element(16, Input)
                                    ))/binary>>/binary,
                                "\n\tns: "/utf8>>/binary,
                            (gleam@string:inspect(erlang:element(17, Input)))/binary>>/binary,
                        "\n\tidentifier: "/utf8>>/binary,
                    (gleam@string:inspect(erlang:element(18, Input)))/binary>>/binary,
                "\n\tdata: "/utf8>>/binary,
            (gleam@string:inspect(erlang:element(19, Input)))/binary>>/binary,
        "\n)"/utf8>>.

-file("src/xmlm.gleam", 865).
-spec error(input(), internal_input_error()) -> {ok, any()} |
    {error, input_error()}.
error(Input, Err) ->
    {error,
        {input_error,
            {position, erlang:element(10, Input), erlang:element(11, Input)},
            Err}}.

-file("src/xmlm.gleam", 3213).
-spec buffer_to_string(list(integer())) -> binary().
buffer_to_string(Buffer) ->
    _pipe = lists:reverse(Buffer),
    xmlm_ffi:int_list_to_string(_pipe).

-file("src/xmlm.gleam", 3146).
?DOC(
    " UTF-8 encode a uchar in the buffer. Assumes that `uchar` is a valid\n"
    " codepoint.\n"
).
-spec buffer_add_uchar(list(integer()), integer()) -> list(integer()).
buffer_add_uchar(Buffer, Uchar) ->
    case Uchar =< 16#007F of
        true ->
            [Uchar | Buffer];

        false ->
            case Uchar =< 16#07FF of
                true ->
                    [erlang:'bor'(16#80, erlang:'band'(Uchar, 16#3F)),
                        erlang:'bor'(16#C0, erlang:'bsr'(Uchar, 6)) |
                        Buffer];

                false ->
                    case Uchar =< 16#FFFF of
                        true ->
                            [erlang:'bor'(16#80, erlang:'band'(Uchar, 16#3F)),
                                erlang:'bor'(
                                    16#80,
                                    erlang:'band'(erlang:'bsr'(Uchar, 6), 16#3F)
                                ),
                                erlang:'bor'(16#E0, erlang:'bsr'(Uchar, 12)) |
                                Buffer];

                        false ->
                            [erlang:'bor'(16#80, erlang:'band'(Uchar, 16#3F)),
                                erlang:'bor'(
                                    16#80,
                                    erlang:'band'(erlang:'bsr'(Uchar, 6), 16#3F)
                                ),
                                erlang:'bor'(
                                    16#80,
                                    erlang:'band'(
                                        erlang:'bsr'(Uchar, 12),
                                        16#3F
                                    )
                                ),
                                erlang:'bor'(16#F0, erlang:'bsr'(Uchar, 18)) |
                                Buffer]
                    end
            end
    end.

-file("src/xmlm.gleam", 3139).
-spec buffer_new() -> list(integer()).
buffer_new() ->
    [].

-file("src/xmlm.gleam", 3221).
-spec string_from_char(integer()) -> binary().
string_from_char(Char) ->
    _pipe = buffer_new(),
    _pipe@1 = buffer_add_uchar(_pipe, Char),
    buffer_to_string(_pipe@1).

-file("src/xmlm.gleam", 869).
-spec error_illegal_char(input(), integer()) -> {ok, any()} |
    {error, input_error()}.
error_illegal_char(Input, Uchar) ->
    error(Input, {illegal_char_seq, string_from_char(Uchar)}).

-file("src/xmlm.gleam", 873).
-spec error_expected_seqs(input(), list(binary()), binary()) -> {ok, any()} |
    {error, input_error()}.
error_expected_seqs(Input, Expected, Actual) ->
    error(Input, {expected_char_seqs, Expected, Actual}).

-file("src/xmlm.gleam", 881).
-spec error_expected_chars(input(), list(integer())) -> {ok, any()} |
    {error, input_error()}.
error_expected_chars(Input, Expected) ->
    Expected@1 = gleam@list:map(Expected, fun string_from_char/1),
    error(
        Input,
        {expected_char_seqs,
            Expected@1,
            string_from_char(erlang:element(8, Input))}
    ).

-file("src/xmlm.gleam", 899).
?DOC(
    " `xmlm.from_bit_array(source)` returns a new `Input` abstraction from the \n"
    " given `source`.\n"
).
-spec from_bit_array(bitstring()) -> input().
from_bit_array(Source) ->
    Bindings = begin
        _pipe = maps:new(),
        _pipe@1 = gleam@dict:insert(_pipe, <<""/utf8>>, <<""/utf8>>),
        _pipe@2 = gleam@dict:insert(
            _pipe@1,
            <<"xml"/utf8>>,
            <<"http://www.w3.org/XML/1998/namespace"/utf8>>
        ),
        gleam@dict:insert(
            _pipe@2,
            <<"xmlns"/utf8>>,
            <<"http://www.w3.org/2000/xmlns/"/utf8>>
        )
    end,
    {input,
        none,
        false,
        fun(_) -> none end,
        fun(_) -> none end,
        input_uchar_byte(),
        xmlm_ffi:bit_array_to_list(Source),
        9007199254740990,
        false,
        1,
        0,
        limit_text,
        signal_start_stream(),
        false,
        true,
        [],
        Bindings,
        [],
        []}.

-file("src/xmlm.gleam", 892).
?DOC(
    " `xmlm.from_string(source)` returns a new `Input` abstraction from the given\n"
    " `source`.\n"
).
-spec from_string(binary()) -> input().
from_string(Source) ->
    from_bit_array(gleam_stdlib:identity(Source)).

-file("src/xmlm.gleam", 930).
?DOC(" `xmlm.with_encoding(input)` sets the `input` to use the given `encoding`.\n").
-spec with_encoding(input(), encoding()) -> input().
with_encoding(Input, Encoding) ->
    {input,
        {some, Encoding},
        erlang:element(3, Input),
        erlang:element(4, Input),
        erlang:element(5, Input),
        erlang:element(6, Input),
        erlang:element(7, Input),
        erlang:element(8, Input),
        erlang:element(9, Input),
        erlang:element(10, Input),
        erlang:element(11, Input),
        erlang:element(12, Input),
        erlang:element(13, Input),
        erlang:element(14, Input),
        erlang:element(15, Input),
        erlang:element(16, Input),
        erlang:element(17, Input),
        erlang:element(18, Input),
        erlang:element(19, Input)}.

-file("src/xmlm.gleam", 937).
?DOC(
    " `xmlm.with_stripping(input, stripping)` sets the `input` to use the given\n"
    " `stripping`.\n"
).
-spec with_stripping(input(), boolean()) -> input().
with_stripping(Input, Stripping) ->
    {input,
        erlang:element(2, Input),
        erlang:element(3, Input),
        erlang:element(4, Input),
        erlang:element(5, Input),
        erlang:element(6, Input),
        erlang:element(7, Input),
        erlang:element(8, Input),
        erlang:element(9, Input),
        erlang:element(10, Input),
        erlang:element(11, Input),
        erlang:element(12, Input),
        erlang:element(13, Input),
        Stripping,
        erlang:element(15, Input),
        erlang:element(16, Input),
        erlang:element(17, Input),
        erlang:element(18, Input),
        erlang:element(19, Input)}.

-file("src/xmlm.gleam", 980).
?DOC(
    " `xmlm.with_namespace_callback(input, namespace_callback)` sets the `input` \n"
    " to use the given `namespace_callback` to bind undeclared namespace prefixes.\n"
    " \n"
    " ## Example\n"
    " \n"
    " Imagine an XML document something like this, that specifies a namespace.\n"
    " \n"
    " ```xml\n"
    " <a xmlns:snazzy=\"https://www.example.com/snazzy\">\n"
    "   <snazzy:b />\n"
    " </a>\n"
    " ```\n"
    " \n"
    " This will parse Ok because the namespace is properly declared.  \n"
    " \n"
    " However, the following XML document would give an error, telling you about \n"
    " the unknown namespace prefix `snazzy`.\n"
    " \n"
    " ```xml\n"
    " <a>\n"
    "   <snazzy:b />\n"
    " </a>\n"
    " ```\n"
    " \n"
    " To address this, you may provide a function to bind undeclared namespace \n"
    " prefixes.\n"
    " \n"
    " ```gleam\n"
    " xmlm.from_string(xml_data)\n"
    " |> xmlm.with_namespace_callback(fn(prefix) {\n"
    "   case prefix {\n"
    "     \"snazzy\" -> Some(\"https://www.example.com/snazzy\")\n"
    "     _ -> None\n"
    "   }\n"
    " })\n"
    " ```\n"
    " \n"
    " In this way, the `snazzy` prefix will be bound and no error will occur.\n"
).
-spec with_namespace_callback(
    input(),
    fun((binary()) -> gleam@option:option(binary()))
) -> input().
with_namespace_callback(Input, Namespace_callback) ->
    {input,
        erlang:element(2, Input),
        erlang:element(3, Input),
        Namespace_callback,
        erlang:element(5, Input),
        erlang:element(6, Input),
        erlang:element(7, Input),
        erlang:element(8, Input),
        erlang:element(9, Input),
        erlang:element(10, Input),
        erlang:element(11, Input),
        erlang:element(12, Input),
        erlang:element(13, Input),
        erlang:element(14, Input),
        erlang:element(15, Input),
        erlang:element(16, Input),
        erlang:element(17, Input),
        erlang:element(18, Input),
        erlang:element(19, Input)}.

-file("src/xmlm.gleam", 1020).
?DOC(
    " `xmlm.with_entity_callback(input, namespace_callback)` sets the `input` to \n"
    " use the given `entity_callback` to resolve non-predefined entity references.\n"
    " \n"
    " ## Example\n"
    " \n"
    " Imagine an XML document that looks something like this:\n"
    " \n"
    " ```xml\n"
    " <p> &apple; &pie; </p>\n"
    " ```\n"
    " \n"
    " It has non-predifined entity references, and so when parsing, it will give \n"
    " an error.  To address this, we could use an entity callback function to \n"
    " resolve these references.\n"
    " \n"
    " ```gleam\n"
    " xmlm.from_string(xml_data)\n"
    " |> xmlm.with_entity_callback(fn(entity_reference) {\n"
    "   case entity_reference {\n"
    "     \"apple\" -> Some(\"APPLE!\")\n"
    "     \"pie\" -> Some(\"PIE!\")\n"
    "     _ -> None\n"
    "   }\n"
    " })\n"
    " ```\n"
    " \n"
    " With that entity callback, the parsed `Data` signal would look something \n"
    " like this:\n"
    " \n"
    " ```gleam\n"
    " Data(\"APPLE! PIE!\")\n"
    " ```\n"
).
-spec with_entity_callback(
    input(),
    fun((binary()) -> gleam@option:option(binary()))
) -> input().
with_entity_callback(Input, Entity_callback) ->
    {input,
        erlang:element(2, Input),
        erlang:element(3, Input),
        erlang:element(4, Input),
        Entity_callback,
        erlang:element(6, Input),
        erlang:element(7, Input),
        erlang:element(8, Input),
        erlang:element(9, Input),
        erlang:element(10, Input),
        erlang:element(11, Input),
        erlang:element(12, Input),
        erlang:element(13, Input),
        erlang:element(14, Input),
        erlang:element(15, Input),
        erlang:element(16, Input),
        erlang:element(17, Input),
        erlang:element(18, Input),
        erlang:element(19, Input)}.

-file("src/xmlm.gleam", 1027).
-spec input_identifier_to_string(input()) -> binary().
input_identifier_to_string(Input) ->
    _pipe = erlang:element(18, Input),
    buffer_to_string(_pipe).

-file("src/xmlm.gleam", 1031).
-spec input_data_to_string(input()) -> binary().
input_data_to_string(Input) ->
    _pipe = erlang:element(19, Input),
    buffer_to_string(_pipe).

-file("src/xmlm.gleam", 1036).
?DOC(" Checks if `uchar` is in the inclusive range `[from, to]`.\n").
-spec is_in_range(integer(), integer(), integer()) -> boolean().
is_in_range(Uchar, Low, High) ->
    (Low =< Uchar) andalso (Uchar =< High).

-file("src/xmlm.gleam", 1040).
-spec is_whitespace(integer()) -> boolean().
is_whitespace(Int) ->
    case Int of
        16#0020 ->
            true;

        16#0009 ->
            true;

        16#000D ->
            true;

        16#000A ->
            true;

        _ ->
            false
    end.

-file("src/xmlm.gleam", 1048).
?DOC(" XML 1.0 non-terminal: {Char}\n").
-spec is_char(integer()) -> boolean().
is_char(Uchar) ->
    case Uchar of
        Uchar@1 when (16#0020 =< Uchar@1) andalso (Uchar@1 =< 16#D7FF) ->
            true;

        16#0009 ->
            true;

        16#000A ->
            true;

        16#000D ->
            true;

        Uchar@2 when (16#E000 =< Uchar@2) andalso (Uchar@2 =< 16#FFFD) ->
            true;

        Uchar@3 when (16#10000 =< Uchar@3) andalso (Uchar@3 =< 16#10FFFF) ->
            true;

        _ ->
            false
    end.

-file("src/xmlm.gleam", 1058).
-spec is_digit(integer()) -> boolean().
is_digit(Uchar) ->
    _pipe = Uchar,
    is_in_range(_pipe, 16#0030, 16#0039).

-file("src/xmlm.gleam", 1062).
-spec is_hex_digit(integer()) -> boolean().
is_hex_digit(Uchar) ->
    (is_in_range(Uchar, 16#0030, 16#0039) orelse is_in_range(
        Uchar,
        16#0041,
        16#0046
    ))
    orelse is_in_range(Uchar, 16#0061, 16#0066).

-file("src/xmlm.gleam", 1069).
-spec is_common_range(integer()) -> boolean().
is_common_range(Uchar) ->
    ((((((((((is_in_range(Uchar, 16#00C0, 16#00D6) orelse is_in_range(
        Uchar,
        16#00D8,
        16#00F6
    ))
    orelse is_in_range(Uchar, 16#00F8, 16#02FF))
    orelse is_in_range(Uchar, 16#0370, 16#037D))
    orelse is_in_range(Uchar, 16#037F, 16#1FFF))
    orelse is_in_range(Uchar, 16#200C, 16#200D))
    orelse is_in_range(Uchar, 16#2070, 16#218F))
    orelse is_in_range(Uchar, 16#2C00, 16#2FEF))
    orelse is_in_range(Uchar, 16#3001, 16#D7FF))
    orelse is_in_range(Uchar, 16#F900, 16#FDCF))
    orelse is_in_range(Uchar, 16#FDF0, 16#FFFD))
    orelse is_in_range(Uchar, 16#10000, 16#EFFFF).

-file("src/xmlm.gleam", 1085).
?DOC(" XML 1.1 non-terminal: {NameStartChar} - ':'\n").
-spec is_name_start_char(integer()) -> boolean().
is_name_start_char(Uchar) ->
    not is_whitespace(Uchar) andalso (((is_in_range(Uchar, 16#0061, 16#007A)
    orelse is_in_range(Uchar, 16#0041, 16#005A))
    orelse (Uchar =:= 16#005F))
    orelse is_common_range(Uchar)).

-file("src/xmlm.gleam", 1101).
?DOC(" XML 1.1 non-terminal: {NameChar} - ':'\n").
-spec is_name_char(integer()) -> boolean().
is_name_char(Uchar) ->
    not is_whitespace(Uchar) andalso (((((((((is_in_range(
        Uchar,
        16#0061,
        16#007A
    )
    orelse is_in_range(Uchar, 16#0041, 16#005A))
    orelse is_in_range(Uchar, 16#0030, 16#0039))
    orelse (Uchar =:= 16#005F))
    orelse (Uchar =:= 16#002D))
    orelse (Uchar =:= 16#002E))
    orelse (Uchar =:= 16#00B7))
    orelse is_common_range(Uchar))
    orelse is_in_range(Uchar, 16#0300, 16#036F))
    orelse is_in_range(Uchar, 16#203F, 16#2040)).

-file("src/xmlm.gleam", 1125).
-spec next_char(input()) -> {ok, input()} | {error, input_error()}.
next_char(Input) ->
    case erlang:element(8, Input) =:= 9007199254740991 of
        true ->
            error(Input, unexpected_eoi);

        false ->
            Input@1 = case erlang:element(8, Input) =:= 16#000A of
                true ->
                    {input,
                        erlang:element(2, Input),
                        erlang:element(3, Input),
                        erlang:element(4, Input),
                        erlang:element(5, Input),
                        erlang:element(6, Input),
                        erlang:element(7, Input),
                        erlang:element(8, Input),
                        erlang:element(9, Input),
                        erlang:element(10, Input) + 1,
                        1,
                        erlang:element(12, Input),
                        erlang:element(13, Input),
                        erlang:element(14, Input),
                        erlang:element(15, Input),
                        erlang:element(16, Input),
                        erlang:element(17, Input),
                        erlang:element(18, Input),
                        erlang:element(19, Input)};

                false ->
                    {input,
                        erlang:element(2, Input),
                        erlang:element(3, Input),
                        erlang:element(4, Input),
                        erlang:element(5, Input),
                        erlang:element(6, Input),
                        erlang:element(7, Input),
                        erlang:element(8, Input),
                        erlang:element(9, Input),
                        erlang:element(10, Input),
                        erlang:element(11, Input) + 1,
                        erlang:element(12, Input),
                        erlang:element(13, Input),
                        erlang:element(14, Input),
                        erlang:element(15, Input),
                        erlang:element(16, Input),
                        erlang:element(17, Input),
                        erlang:element(18, Input),
                        erlang:element(19, Input)}
            end,
            case (erlang:element(6, Input@1))(Input@1) of
                {error, E} ->
                    {error, E};

                {ok, {Char, Input@2}} ->
                    Input@3 = {input,
                        erlang:element(2, Input@2),
                        erlang:element(3, Input@2),
                        erlang:element(4, Input@2),
                        erlang:element(5, Input@2),
                        erlang:element(6, Input@2),
                        erlang:element(7, Input@2),
                        Char,
                        erlang:element(9, Input@2),
                        erlang:element(10, Input@2),
                        erlang:element(11, Input@2),
                        erlang:element(12, Input@2),
                        erlang:element(13, Input@2),
                        erlang:element(14, Input@2),
                        erlang:element(15, Input@2),
                        erlang:element(16, Input@2),
                        erlang:element(17, Input@2),
                        erlang:element(18, Input@2),
                        erlang:element(19, Input@2)},
                    case not is_char(erlang:element(8, Input@3)) of
                        true ->
                            error(Input@3, malformed_char_stream);

                        false ->
                            Input@5 = case erlang:element(9, Input@3) andalso (erlang:element(
                                8,
                                Input@3
                            )
                            =:= 16#000A) of
                                false ->
                                    {ok, Input@3};

                                true ->
                                    case (erlang:element(6, Input@3))(Input@3) of
                                        {error, E@1} ->
                                            {error, E@1};

                                        {ok, {Char@1, Input@4}} ->
                                            {ok,
                                                {input,
                                                    erlang:element(2, Input@4),
                                                    erlang:element(3, Input@4),
                                                    erlang:element(4, Input@4),
                                                    erlang:element(5, Input@4),
                                                    erlang:element(6, Input@4),
                                                    erlang:element(7, Input@4),
                                                    Char@1,
                                                    erlang:element(9, Input@4),
                                                    erlang:element(10, Input@4),
                                                    erlang:element(11, Input@4),
                                                    erlang:element(12, Input@4),
                                                    erlang:element(13, Input@4),
                                                    erlang:element(14, Input@4),
                                                    erlang:element(15, Input@4),
                                                    erlang:element(16, Input@4),
                                                    erlang:element(17, Input@4),
                                                    erlang:element(18, Input@4),
                                                    erlang:element(19, Input@4)}}
                                    end
                            end,
                            case Input@5 of
                                {error, E@2} ->
                                    {error, E@2};

                                {ok, Input@6} ->
                                    Input@7 = case erlang:element(8, Input@6)
                                    =:= 16#000D of
                                        true ->
                                            {input,
                                                erlang:element(2, Input@6),
                                                erlang:element(3, Input@6),
                                                erlang:element(4, Input@6),
                                                erlang:element(5, Input@6),
                                                erlang:element(6, Input@6),
                                                erlang:element(7, Input@6),
                                                16#000A,
                                                true,
                                                erlang:element(10, Input@6),
                                                erlang:element(11, Input@6),
                                                erlang:element(12, Input@6),
                                                erlang:element(13, Input@6),
                                                erlang:element(14, Input@6),
                                                erlang:element(15, Input@6),
                                                erlang:element(16, Input@6),
                                                erlang:element(17, Input@6),
                                                erlang:element(18, Input@6),
                                                erlang:element(19, Input@6)};

                                        false ->
                                            {input,
                                                erlang:element(2, Input@6),
                                                erlang:element(3, Input@6),
                                                erlang:element(4, Input@6),
                                                erlang:element(5, Input@6),
                                                erlang:element(6, Input@6),
                                                erlang:element(7, Input@6),
                                                erlang:element(8, Input@6),
                                                false,
                                                erlang:element(10, Input@6),
                                                erlang:element(11, Input@6),
                                                erlang:element(12, Input@6),
                                                erlang:element(13, Input@6),
                                                erlang:element(14, Input@6),
                                                erlang:element(15, Input@6),
                                                erlang:element(16, Input@6),
                                                erlang:element(17, Input@6),
                                                erlang:element(18, Input@6),
                                                erlang:element(19, Input@6)}
                                    end,
                                    {ok, Input@7}
                            end
                    end
            end
    end.

-file("src/xmlm.gleam", 1170).
-spec next_char_eof(input()) -> {ok, input()} | {error, input_error()}.
next_char_eof(Input) ->
    case next_char(Input) of
        {error, {input_error, _, unicode_lexer_error_eoi}} ->
            {ok,
                {input,
                    erlang:element(2, Input),
                    erlang:element(3, Input),
                    erlang:element(4, Input),
                    erlang:element(5, Input),
                    erlang:element(6, Input),
                    erlang:element(7, Input),
                    9007199254740991,
                    erlang:element(9, Input),
                    erlang:element(10, Input),
                    erlang:element(11, Input),
                    erlang:element(12, Input),
                    erlang:element(13, Input),
                    erlang:element(14, Input),
                    erlang:element(15, Input),
                    erlang:element(16, Input),
                    erlang:element(17, Input),
                    erlang:element(18, Input),
                    erlang:element(19, Input)}};

        {error, _} = E ->
            E;

        {ok, Input@1} ->
            {ok, Input@1}
    end.

-file("src/xmlm.gleam", 1181).
-spec skip_whitespace(input()) -> {ok, input()} | {error, input_error()}.
skip_whitespace(Input) ->
    case not is_whitespace(erlang:element(8, Input)) of
        true ->
            {ok, Input};

        false ->
            case next_char(Input) of
                {error, E} ->
                    {error, E};

                {ok, Input@1} ->
                    skip_whitespace(Input@1)
            end
    end.

-file("src/xmlm.gleam", 1193).
-spec skip_whitespace_eof(input()) -> {ok, input()} | {error, input_error()}.
skip_whitespace_eof(Input) ->
    case is_whitespace(erlang:element(8, Input)) of
        true ->
            case next_char_eof(Input) of
                {error, E} ->
                    {error, E};

                {ok, Input@1} ->
                    skip_whitespace_eof(Input@1)
            end;

        false ->
            {ok, Input}
    end.

-file("src/xmlm.gleam", 1205).
-spec accept(input(), integer()) -> {ok, input()} | {error, input_error()}.
accept(Input, Char) ->
    case erlang:element(8, Input) =:= Char of
        true ->
            next_char(Input);

        false ->
            error_expected_chars(Input, [Char])
    end.

-file("src/xmlm.gleam", 3209).
-spec buffer_clear(list(integer())) -> list(integer()).
buffer_clear(_) ->
    [].

-file("src/xmlm.gleam", 1212).
-spec clear_identifier(input()) -> input().
clear_identifier(Input) ->
    {input,
        erlang:element(2, Input),
        erlang:element(3, Input),
        erlang:element(4, Input),
        erlang:element(5, Input),
        erlang:element(6, Input),
        erlang:element(7, Input),
        erlang:element(8, Input),
        erlang:element(9, Input),
        erlang:element(10, Input),
        erlang:element(11, Input),
        erlang:element(12, Input),
        erlang:element(13, Input),
        erlang:element(14, Input),
        erlang:element(15, Input),
        erlang:element(16, Input),
        erlang:element(17, Input),
        buffer_clear(erlang:element(18, Input)),
        erlang:element(19, Input)}.

-file("src/xmlm.gleam", 1216).
-spec clear_data(input()) -> input().
clear_data(Input) ->
    {input,
        erlang:element(2, Input),
        erlang:element(3, Input),
        erlang:element(4, Input),
        erlang:element(5, Input),
        erlang:element(6, Input),
        erlang:element(7, Input),
        erlang:element(8, Input),
        erlang:element(9, Input),
        erlang:element(10, Input),
        erlang:element(11, Input),
        erlang:element(12, Input),
        erlang:element(13, Input),
        erlang:element(14, Input),
        erlang:element(15, Input),
        erlang:element(16, Input),
        erlang:element(17, Input),
        erlang:element(18, Input),
        buffer_clear(erlang:element(19, Input))}.

-file("src/xmlm.gleam", 1220).
-spec add_char_to_identifier(input(), integer()) -> input().
add_char_to_identifier(Input, Char) ->
    {input,
        erlang:element(2, Input),
        erlang:element(3, Input),
        erlang:element(4, Input),
        erlang:element(5, Input),
        erlang:element(6, Input),
        erlang:element(7, Input),
        erlang:element(8, Input),
        erlang:element(9, Input),
        erlang:element(10, Input),
        erlang:element(11, Input),
        erlang:element(12, Input),
        erlang:element(13, Input),
        erlang:element(14, Input),
        erlang:element(15, Input),
        erlang:element(16, Input),
        erlang:element(17, Input),
        buffer_add_uchar(erlang:element(18, Input), Char),
        erlang:element(19, Input)}.

-file("src/xmlm.gleam", 1224).
-spec add_char_to_data(input(), integer()) -> input().
add_char_to_data(Input, Char) ->
    {input,
        erlang:element(2, Input),
        erlang:element(3, Input),
        erlang:element(4, Input),
        erlang:element(5, Input),
        erlang:element(6, Input),
        erlang:element(7, Input),
        erlang:element(8, Input),
        erlang:element(9, Input),
        erlang:element(10, Input),
        erlang:element(11, Input),
        erlang:element(12, Input),
        erlang:element(13, Input),
        erlang:element(14, Input),
        erlang:element(15, Input),
        erlang:element(16, Input),
        erlang:element(17, Input),
        erlang:element(18, Input),
        buffer_add_uchar(erlang:element(19, Input), Char)}.

-file("src/xmlm.gleam", 1228).
-spec add_char_to_data_strip(input(), integer()) -> input().
add_char_to_data_strip(Input, Char) ->
    case is_whitespace(Char) of
        true ->
            {input,
                erlang:element(2, Input),
                erlang:element(3, Input),
                erlang:element(4, Input),
                erlang:element(5, Input),
                erlang:element(6, Input),
                erlang:element(7, Input),
                erlang:element(8, Input),
                erlang:element(9, Input),
                erlang:element(10, Input),
                erlang:element(11, Input),
                erlang:element(12, Input),
                erlang:element(13, Input),
                erlang:element(14, Input),
                true,
                erlang:element(16, Input),
                erlang:element(17, Input),
                erlang:element(18, Input),
                erlang:element(19, Input)};

        false ->
            Input@1 = case {erlang:element(15, Input),
                erlang:element(19, Input)} of
                {_, []} ->
                    Input;

                {false, _} ->
                    Input;

                {true, _} ->
                    add_char_to_data(Input, 16#0020)
            end,
            Input@2 = {input,
                erlang:element(2, Input@1),
                erlang:element(3, Input@1),
                erlang:element(4, Input@1),
                erlang:element(5, Input@1),
                erlang:element(6, Input@1),
                erlang:element(7, Input@1),
                erlang:element(8, Input@1),
                erlang:element(9, Input@1),
                erlang:element(10, Input@1),
                erlang:element(11, Input@1),
                erlang:element(12, Input@1),
                erlang:element(13, Input@1),
                erlang:element(14, Input@1),
                false,
                erlang:element(16, Input@1),
                erlang:element(17, Input@1),
                erlang:element(18, Input@1),
                erlang:element(19, Input@1)},
            add_char_to_data(Input@2, Char)
    end.

-file("src/xmlm.gleam", 1244).
-spec expand_name(input(), name()) -> {ok, name()} | {error, input_error()}.
expand_name(Input, Name) ->
    {name, Prefix, Local} = Name,
    External_ = fun(Prefix@1) -> case (erlang:element(4, Input))(Prefix@1) of
            none ->
                error(Input, {unknown_ns_prefix, Prefix@1});

            {some, Uri} ->
                {ok, Uri}
        end end,
    case gleam_stdlib:map_get(erlang:element(17, Input), Prefix) of
        {ok, Uri@1} ->
            case not gleam@string:is_empty(Uri@1) of
                true ->
                    {ok, {name, Uri@1, Local}};

                false ->
                    case gleam@string:is_empty(Prefix) of
                        true ->
                            {ok, {name, <<""/utf8>>, Local}};

                        false ->
                            case External_(Prefix) of
                                {ok, Uri@2} ->
                                    {ok, {name, Uri@2, Local}};

                                {error, Msg} ->
                                    {error, Msg}
                            end
                    end
            end;

        {error, nil} ->
            case External_(Prefix) of
                {ok, Uri@3} ->
                    {ok, {name, Uri@3, Local}};

                {error, Msg@1} ->
                    {error, Msg@1}
            end
    end.

-file("src/xmlm.gleam", 1307).
-spec parse_ncname__loop(input()) -> {ok, input()} | {error, input_error()}.
parse_ncname__loop(Input) ->
    case is_name_char(erlang:element(8, Input)) of
        false ->
            {ok, Input};

        true ->
            Input@1 = add_char_to_identifier(Input, erlang:element(8, Input)),
            case next_char(Input@1) of
                {error, E} ->
                    {error, E};

                {ok, Input@2} ->
                    parse_ncname__loop(Input@2)
            end
    end.

-file("src/xmlm.gleam", 1284).
?DOC(
    " An XML Name, minus the colon (`:`)\n"
    " \n"
    " XML Namespace 1.1 non-terminal: {NCName}\n"
    " https://www.w3.org/TR/2006/REC-xml-names11-20060816/#NT-NCName\n"
).
-spec parse_ncname(input()) -> {ok, {binary(), input()}} |
    {error, input_error()}.
parse_ncname(Input) ->
    Input@1 = clear_identifier(Input),
    case not is_name_start_char(erlang:element(8, Input@1)) of
        true ->
            error_illegal_char(Input@1, erlang:element(8, Input@1));

        false ->
            Input@2 = add_char_to_identifier(
                Input@1,
                erlang:element(8, Input@1)
            ),
            case next_char(Input@2) of
                {error, E} ->
                    {error, E};

                {ok, Input@3} ->
                    case parse_ncname__loop(Input@3) of
                        {error, E@1} ->
                            {error, E@1};

                        {ok, Input@4} ->
                            Name = input_identifier_to_string(Input@4),
                            {ok, {Name, Input@4}}
                    end
            end
    end.

-file("src/xmlm.gleam", 1324).
?DOC(
    " Qualified name\n"
    " XML Namespace 1.1 non-terminal: {QName}\n"
    " https://www.w3.org/TR/2006/REC-xml-names11-20060816/#NT-QName\n"
).
-spec parse_qname(input()) -> {ok, {name(), input()}} | {error, input_error()}.
parse_qname(Input) ->
    case parse_ncname(Input) of
        {error, E} ->
            {error, E};

        {ok, {Name, Input@1}} ->
            case erlang:element(8, Input@1) /= 16#003A of
                true ->
                    {ok, {{name, <<""/utf8>>, Name}, Input@1}};

                false ->
                    case next_char(Input@1) of
                        {error, E@1} ->
                            {error, E@1};

                        {ok, Input@2} ->
                            case parse_ncname(Input@2) of
                                {error, E@2} ->
                                    {error, E@2};

                                {ok, {Local_name, Input@3}} ->
                                    {ok, {{name, Name, Local_name}, Input@3}}
                            end
                    end
            end
    end.

-file("src/xmlm.gleam", 1501).
-spec predefined_entities() -> gleam@dict:dict(binary(), binary()).
predefined_entities() ->
    _pipe = maps:new(),
    _pipe@1 = gleam@dict:insert(_pipe, <<"lt"/utf8>>, <<"<"/utf8>>),
    _pipe@2 = gleam@dict:insert(_pipe@1, <<"gt"/utf8>>, <<">"/utf8>>),
    _pipe@3 = gleam@dict:insert(_pipe@2, <<"amp"/utf8>>, <<"&"/utf8>>),
    _pipe@4 = gleam@dict:insert(_pipe@3, <<"apos"/utf8>>, <<"'"/utf8>>),
    gleam@dict:insert(_pipe@4, <<"quot"/utf8>>, <<"\""/utf8>>).

-file("src/xmlm.gleam", 1511).
?DOC(" XML 1.0 non-terminal: {EntityRef}, '&' was eaten.\n").
-spec parse_entity_reference(input(), gleam@dict:dict(binary(), binary())) -> {ok,
        {binary(), input()}} |
    {error, input_error()}.
parse_entity_reference(Input, Predefined_entities) ->
    case parse_ncname(Input) of
        {error, E} ->
            {error, E};

        {ok, {Entity, Input@1}} ->
            case accept(Input@1, 16#003B) of
                {error, E@1} ->
                    {error, E@1};

                {ok, Input@2} ->
                    case gleam_stdlib:map_get(Predefined_entities, Entity) of
                        {ok, Replacement} ->
                            {ok, {Replacement, Input@2}};

                        {error, nil} ->
                            case (erlang:element(5, Input@2))(Entity) of
                                {some, Replacement@1} ->
                                    {ok, {Replacement@1, Input@2}};

                                none ->
                                    error(Input@2, {unknown_entity_ref, Entity})
                            end
                    end
            end
    end.

-file("src/xmlm.gleam", 1488).
-spec parse_char_reference__loop3(input()) -> {ok, input()} |
    {error, input_error()}.
parse_char_reference__loop3(Input) ->
    case erlang:element(8, Input) =:= 16#003B of
        true ->
            {ok, Input};

        false ->
            Input@1 = add_char_to_identifier(Input, erlang:element(8, Input)),
            case next_char(Input@1) of
                {error, E} ->
                    {error, E};

                {ok, Input@2} ->
                    parse_char_reference__loop3(Input@2)
            end
    end.

-file("src/xmlm.gleam", 1431).
-spec parse_char_reference__loop1(input(), integer()) -> {ok, loop_done()} |
    {error, input_error()}.
parse_char_reference__loop1(Input, C) ->
    case erlang:element(8, Input) =:= 16#003B of
        true ->
            {ok, {loop_done_by_condition, {C, Input}}};

        false ->
            Input@1 = add_char_to_identifier(Input, erlang:element(8, Input)),
            case not is_hex_digit(erlang:element(8, Input@1)) of
                true ->
                    {ok, {loop_done_exited, {C, Input@1}}};

                false ->
                    C@1 = ((C * 16) + (case erlang:element(8, Input@1) =< 16#0039 of
                        true ->
                            erlang:element(8, Input@1) - 48;

                        false ->
                            case erlang:element(8, Input@1) =< 16#0046 of
                                true ->
                                    erlang:element(8, Input@1) - 55;

                                false ->
                                    erlang:element(8, Input@1) - 87
                            end
                    end)),
                    case next_char(Input@1) of
                        {error, E} ->
                            {error, E};

                        {ok, Input@2} ->
                            parse_char_reference__loop1(Input@2, C@1)
                    end
            end
    end.

-file("src/xmlm.gleam", 1466).
-spec parse_char_reference__loop2(input(), integer()) -> {ok, loop_done()} |
    {error, input_error()}.
parse_char_reference__loop2(Input, C) ->
    case erlang:element(8, Input) =:= 16#003B of
        true ->
            {ok, {loop_done_by_condition, {C, Input}}};

        false ->
            Input@1 = add_char_to_identifier(Input, erlang:element(8, Input)),
            case not is_digit(erlang:element(8, Input@1)) of
                true ->
                    {ok, {loop_done_exited, {C, Input@1}}};

                false ->
                    C@1 = (C * 10) + (erlang:element(8, Input@1) - 48),
                    case next_char(Input@1) of
                        {error, E} ->
                            {error, E};

                        {ok, Input@2} ->
                            parse_char_reference__loop2(Input@2, C@1)
                    end
            end
    end.

-file("src/xmlm.gleam", 1365).
?DOC(" XML 1.0 non-terminal: {CharRef}, '&' was eaten.\n").
-spec parse_char_reference(input()) -> {ok, {binary(), input()}} |
    {error, input_error()}.
parse_char_reference(Input) ->
    Input@1 = clear_identifier(Input),
    case next_char(Input@1) of
        {error, E} ->
            {error, E};

        {ok, Input@2} ->
            case erlang:element(8, Input@2) =:= 16#003B of
                true ->
                    error(Input@2, {illegal_char_ref, <<""/utf8>>});

                false ->
                    Result = case erlang:element(8, Input@2) =:= 16#0078 of
                        false ->
                            parse_char_reference__loop2(Input@2, 0);

                        true ->
                            Input@3 = add_char_to_identifier(
                                Input@2,
                                erlang:element(8, Input@2)
                            ),
                            case next_char(Input@3) of
                                {error, E@1} ->
                                    {error, E@1};

                                {ok, Input@4} ->
                                    parse_char_reference__loop1(Input@4, 0)
                            end
                    end,
                    case Result of
                        {error, E@2} ->
                            {error, E@2};

                        {ok, Intermediate_result} ->
                            Tup = (case Intermediate_result of
                                {loop_done_by_condition, {C, Input@5}} ->
                                    {ok, {C, Input@5}};

                                {loop_done_exited, {_, Input@6}} ->
                                    case parse_char_reference__loop3(Input@6) of
                                        {error, E@3} ->
                                            {error, E@3};

                                        {ok, Input@7} ->
                                            {ok, {-1, Input@7}}
                                    end
                            end),
                            case Tup of
                                {error, E@4} ->
                                    {error, E@4};

                                {ok, {C@1, Input@8}} ->
                                    case next_char(Input@8) of
                                        {error, E@5} ->
                                            {error, E@5};

                                        {ok, Input@9} ->
                                            case is_char(C@1) of
                                                true ->
                                                    Input@10 = clear_identifier(
                                                        Input@9
                                                    ),
                                                    Input@11 = add_char_to_identifier(
                                                        Input@10,
                                                        C@1
                                                    ),
                                                    {ok,
                                                        {buffer_to_string(
                                                                erlang:element(
                                                                    18,
                                                                    Input@11
                                                                )
                                                            ),
                                                            Input@11}};

                                                false ->
                                                    error(
                                                        Input@9,
                                                        {illegal_char_ref,
                                                            buffer_to_string(
                                                                erlang:element(
                                                                    18,
                                                                    Input@9
                                                                )
                                                            )}
                                                    )
                                            end
                                    end
                            end
                    end
            end
    end.

-file("src/xmlm.gleam", 1347).
?DOC(" XML 1.0 non-terminal: {Reference}\n").
-spec parse_reference(input()) -> {ok, {binary(), input()}} |
    {error, input_error()}.
parse_reference(Input) ->
    case next_char(Input) of
        {error, E} ->
            {error, E};

        {ok, Input@1} ->
            case erlang:element(8, Input@1) =:= 16#0023 of
                true ->
                    parse_char_reference(Input@1);

                false ->
                    parse_entity_reference(Input@1, predefined_entities())
            end
    end.

-file("src/xmlm.gleam", 1563).
-spec parse_attribute_value__loop(input(), integer()) -> {ok, input()} |
    {error, input_error()}.
parse_attribute_value__loop(Input, Delim) ->
    case erlang:element(8, Input) of
        Char when Char =:= Delim ->
            {ok, Input};

        Char@1 when Char@1 =:= 16#003C ->
            error_illegal_char(Input, 16#003C);

        Char@2 when Char@2 =:= 16#0026 ->
            case parse_reference(Input) of
                {error, E} ->
                    {error, E};

                {ok, {Reference, Input@1}} ->
                    Input@3 = begin
                        _pipe = gleam@string:to_utf_codepoints(Reference),
                        gleam@list:fold(
                            _pipe,
                            Input@1,
                            fun(Input@2, Char@3) ->
                                add_char_to_data_strip(
                                    Input@2,
                                    gleam_stdlib:identity(Char@3)
                                )
                            end
                        )
                    end,
                    parse_attribute_value__loop(Input@3, Delim)
            end;

        _ ->
            Input@4 = add_char_to_data_strip(Input, erlang:element(8, Input)),
            case next_char(Input@4) of
                {error, E@1} ->
                    {error, E@1};

                {ok, Input@5} ->
                    parse_attribute_value__loop(Input@5, Delim)
            end
    end.

-file("src/xmlm.gleam", 1536).
?DOC(" XML 1.0 non-terminals: {S}* {AttValue}\n").
-spec parse_attribute_value(input()) -> {ok, {binary(), input()}} |
    {error, input_error()}.
parse_attribute_value(Input) ->
    gleam@result:'try'(
        skip_whitespace(Input),
        fun(Input@1) ->
            gleam@bool:lazy_guard(
                not ((erlang:element(8, Input@1) =:= 16#0022) orelse (erlang:element(
                    8,
                    Input@1
                )
                =:= 16#0027)),
                fun() -> error_expected_chars(Input@1, [16#0022, 16#0027]) end,
                fun() ->
                    Delim = erlang:element(8, Input@1),
                    gleam@result:'try'(
                        next_char(Input@1),
                        fun(Input@2) ->
                            gleam@result:'try'(
                                skip_whitespace(Input@2),
                                fun(Input@3) ->
                                    Input@4 = clear_data(Input@3),
                                    Input@5 = {input,
                                        erlang:element(2, Input@4),
                                        erlang:element(3, Input@4),
                                        erlang:element(4, Input@4),
                                        erlang:element(5, Input@4),
                                        erlang:element(6, Input@4),
                                        erlang:element(7, Input@4),
                                        erlang:element(8, Input@4),
                                        erlang:element(9, Input@4),
                                        erlang:element(10, Input@4),
                                        erlang:element(11, Input@4),
                                        erlang:element(12, Input@4),
                                        erlang:element(13, Input@4),
                                        erlang:element(14, Input@4),
                                        true,
                                        erlang:element(16, Input@4),
                                        erlang:element(17, Input@4),
                                        erlang:element(18, Input@4),
                                        erlang:element(19, Input@4)},
                                    gleam@result:'try'(
                                        parse_attribute_value__loop(
                                            Input@5,
                                            Delim
                                        ),
                                        fun(Input@6) ->
                                            gleam@result:'try'(
                                                next_char(Input@6),
                                                fun(Input@7) ->
                                                    Data = input_data_to_string(
                                                        Input@7
                                                    ),
                                                    {ok, {Data, Input@7}}
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

-file("src/xmlm.gleam", 1702).
-spec maybe_update_stripping(input(), binary(), binary(), binary()) -> input().
maybe_update_stripping(Input, Attribute_value, Prefix, Local) ->
    case (Prefix =:= <<"xml"/utf8>>) andalso (Local =:= <<"space"/utf8>>) of
        true ->
            case Attribute_value of
                Attr_val when Attr_val =:= <<"preserve"/utf8>> ->
                    {input,
                        erlang:element(2, Input),
                        erlang:element(3, Input),
                        erlang:element(4, Input),
                        erlang:element(5, Input),
                        erlang:element(6, Input),
                        erlang:element(7, Input),
                        erlang:element(8, Input),
                        erlang:element(9, Input),
                        erlang:element(10, Input),
                        erlang:element(11, Input),
                        erlang:element(12, Input),
                        erlang:element(13, Input),
                        false,
                        erlang:element(15, Input),
                        erlang:element(16, Input),
                        erlang:element(17, Input),
                        erlang:element(18, Input),
                        erlang:element(19, Input)};

                Attr_val@1 when Attr_val@1 =:= <<"default"/utf8>> ->
                    {input,
                        erlang:element(2, Input),
                        erlang:element(3, Input),
                        erlang:element(4, Input),
                        erlang:element(5, Input),
                        erlang:element(6, Input),
                        erlang:element(7, Input),
                        erlang:element(8, Input),
                        erlang:element(9, Input),
                        erlang:element(10, Input),
                        erlang:element(11, Input),
                        erlang:element(12, Input),
                        erlang:element(13, Input),
                        erlang:element(3, Input),
                        erlang:element(15, Input),
                        erlang:element(16, Input),
                        erlang:element(17, Input),
                        erlang:element(18, Input),
                        erlang:element(19, Input)};

                _ ->
                    Input
            end;

        false ->
            Input
    end.

-file("src/xmlm.gleam", 1635).
-spec parse_attributes__loop__handle_qname_and_value(
    input(),
    list(binary()),
    list(attribute())
) -> {ok, {list(binary()), list(attribute()), input()}} | {error, input_error()}.
parse_attributes__loop__handle_qname_and_value(Input, Pre_acc, Acc) ->
    case parse_qname(Input) of
        {error, E} ->
            {error, E};

        {ok, {{name, Prefix, Local} = Name, Input@1}} ->
            case skip_whitespace(Input@1) of
                {error, E@1} ->
                    {error, E@1};

                {ok, Input@2} ->
                    case accept(Input@2, 16#003D) of
                        {error, E@2} ->
                            {error, E@2};

                        {ok, Input@3} ->
                            case parse_attribute_value(Input@3) of
                                {error, E@3} ->
                                    {error, E@3};

                                {ok, {Attribute_value, Input@4}} ->
                                    Attribute = {attribute,
                                        Name,
                                        Attribute_value},
                                    case gleam@string:is_empty(Prefix) andalso (Local
                                    =:= <<"xmlns"/utf8>>) of
                                        true ->
                                            Ns = gleam@dict:insert(
                                                erlang:element(17, Input@4),
                                                <<""/utf8>>,
                                                Attribute_value
                                            ),
                                            Input@5 = {input,
                                                erlang:element(2, Input@4),
                                                erlang:element(3, Input@4),
                                                erlang:element(4, Input@4),
                                                erlang:element(5, Input@4),
                                                erlang:element(6, Input@4),
                                                erlang:element(7, Input@4),
                                                erlang:element(8, Input@4),
                                                erlang:element(9, Input@4),
                                                erlang:element(10, Input@4),
                                                erlang:element(11, Input@4),
                                                erlang:element(12, Input@4),
                                                erlang:element(13, Input@4),
                                                erlang:element(14, Input@4),
                                                erlang:element(15, Input@4),
                                                erlang:element(16, Input@4),
                                                Ns,
                                                erlang:element(18, Input@4),
                                                erlang:element(19, Input@4)},
                                            parse_attributes__loop(
                                                Input@5,
                                                [<<""/utf8>> | Pre_acc],
                                                [Attribute | Acc]
                                            );

                                        false ->
                                            case Prefix =:= <<"xmlns"/utf8>> of
                                                true ->
                                                    Ns@1 = gleam@dict:insert(
                                                        erlang:element(
                                                            17,
                                                            Input@4
                                                        ),
                                                        Local,
                                                        Attribute_value
                                                    ),
                                                    Input@6 = {input,
                                                        erlang:element(
                                                            2,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            3,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            4,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            5,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            6,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            7,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            8,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            9,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            10,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            11,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            12,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            13,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            14,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            15,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            16,
                                                            Input@4
                                                        ),
                                                        Ns@1,
                                                        erlang:element(
                                                            18,
                                                            Input@4
                                                        ),
                                                        erlang:element(
                                                            19,
                                                            Input@4
                                                        )},
                                                    parse_attributes__loop(
                                                        Input@6,
                                                        [Local | Pre_acc],
                                                        [Attribute | Acc]
                                                    );

                                                false ->
                                                    Input@7 = maybe_update_stripping(
                                                        Input@4,
                                                        Attribute_value,
                                                        Prefix,
                                                        Local
                                                    ),
                                                    parse_attributes__loop(
                                                        Input@7,
                                                        Pre_acc,
                                                        [Attribute | Acc]
                                                    )
                                            end
                                    end
                            end
                    end
            end
    end.

-file("src/xmlm.gleam", 1607).
-spec parse_attributes__loop(input(), list(binary()), list(attribute())) -> {ok,
        {list(binary()), list(attribute()), input()}} |
    {error, input_error()}.
parse_attributes__loop(Input, Pre_acc, Acc) ->
    case is_whitespace(erlang:element(8, Input)) of
        false ->
            {ok, {Pre_acc, Acc, Input}};

        true ->
            case skip_whitespace(Input) of
                {error, E} ->
                    {error, E};

                {ok, Input@1} ->
                    case erlang:element(8, Input@1) of
                        Char when (Char =:= 16#002F) orelse (erlang:element(
                            8,
                            Input@1
                        ) =:= 16#003E) ->
                            {ok, {Pre_acc, Acc, Input@1}};

                        _ ->
                            parse_attributes__loop__handle_qname_and_value(
                                Input@1,
                                Pre_acc,
                                Acc
                            )
                    end
            end
    end.

-file("src/xmlm.gleam", 1601).
?DOC(
    " Returns a list of bound prefixes and attributes\n"
    "\n"
    " XML 1.0 non-terminals:  ({S} {Attribute})* {S}?\n"
    " \n"
    " TODO: currently returns the attributes in reverse\n"
).
-spec parse_attributes(input()) -> {ok,
        {list(binary()), list(attribute()), input()}} |
    {error, input_error()}.
parse_attributes(Input) ->
    parse_attributes__loop(Input, [], []).

-file("src/xmlm.gleam", 1850).
-spec parse_limit__start_tag(input()) -> {ok, {limit(), input()}} |
    {error, input_error()}.
parse_limit__start_tag(Input) ->
    case parse_qname(Input) of
        {error, E} ->
            {error, E};

        {ok, {Name, Input@1}} ->
            {ok, {{limit_start_tag, Name}, Input@1}}
    end.

-file("src/xmlm.gleam", 1835).
-spec eat_cdata_lbrack(input()) -> {ok, input()} | {error, input_error()}.
eat_cdata_lbrack(Input) ->
    Input@1 = add_char_to_identifier(Input, erlang:element(8, Input)),
    gleam@result:'try'(
        next_char(Input@1),
        fun(Input@2) ->
            Input@3 = add_char_to_identifier(
                Input@2,
                erlang:element(8, Input@2)
            ),
            gleam@result:'try'(
                next_char(Input@3),
                fun(Input@4) ->
                    Input@5 = add_char_to_identifier(
                        Input@4,
                        erlang:element(8, Input@4)
                    ),
                    gleam@result:'try'(
                        next_char(Input@5),
                        fun(Input@6) ->
                            Input@7 = add_char_to_identifier(
                                Input@6,
                                erlang:element(8, Input@6)
                            ),
                            gleam@result:'try'(
                                next_char(Input@7),
                                fun(Input@8) ->
                                    Input@9 = add_char_to_identifier(
                                        Input@8,
                                        erlang:element(8, Input@8)
                                    ),
                                    gleam@result:'try'(
                                        next_char(Input@9),
                                        fun(Input@10) ->
                                            Input@11 = add_char_to_identifier(
                                                Input@10,
                                                erlang:element(8, Input@10)
                                            ),
                                            next_char(Input@11)
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

-file("src/xmlm.gleam", 1813).
-spec parse_limit__cdata(input()) -> {ok, {limit(), input()}} |
    {error, input_error()}.
parse_limit__cdata(Input) ->
    case next_char(Input) of
        {error, E} ->
            {error, E};

        {ok, Input@1} ->
            Input@2 = clear_identifier(Input@1),
            case eat_cdata_lbrack(Input@2) of
                {error, E@1} ->
                    {error, E@1};

                {ok, Input@3} ->
                    Cdata = input_identifier_to_string(Input@3),
                    case Cdata =:= <<"CDATA["/utf8>> of
                        true ->
                            {ok, {limit_c_data, Input@3}};

                        false ->
                            error_expected_seqs(
                                Input@3,
                                [<<"CDATA["/utf8>>],
                                Cdata
                            )
                    end
            end
    end.

-file("src/xmlm.gleam", 1799).
-spec parse_limit__comment(input()) -> {ok, {limit(), input()}} |
    {error, input_error()}.
parse_limit__comment(Input) ->
    case next_char(Input) of
        {error, E} ->
            {error, E};

        {ok, Input@1} ->
            case accept(Input@1, 16#002D) of
                {error, E@1} ->
                    {error, E@1};

                {ok, Input@2} ->
                    {ok, {limit_comment, Input@2}}
            end
    end.

-file("src/xmlm.gleam", 1782).
-spec parse_limit__end_tag(input()) -> {ok, {limit(), input()}} |
    {error, input_error()}.
parse_limit__end_tag(Input) ->
    case next_char(Input) of
        {error, E} ->
            {error, E};

        {ok, Input@1} ->
            case parse_qname(Input@1) of
                {error, E@1} ->
                    {error, E@1};

                {ok, {Name, Input@2}} ->
                    case skip_whitespace(Input@2) of
                        {error, E@2} ->
                            {error, E@2};

                        {ok, Input@3} ->
                            {ok, {{limit_end_tag, Name}, Input@3}}
                    end
            end
    end.

-file("src/xmlm.gleam", 1770).
-spec parse_limit__pi(input()) -> {ok, {limit(), input()}} |
    {error, input_error()}.
parse_limit__pi(Input) ->
    case next_char(Input) of
        {error, E} ->
            {error, E};

        {ok, Input@1} ->
            case parse_qname(Input@1) of
                {error, E@1} ->
                    {error, E@1};

                {ok, {Name, Input@2}} ->
                    {ok, {{limit_pi, Name}, Input@2}}
            end
    end.

-file("src/xmlm.gleam", 1724).
?DOC(" Parses a markup limit.\n").
-spec parse_limit(input()) -> {ok, input()} | {error, input_error()}.
parse_limit(Input) ->
    Result = case erlang:element(8, Input) =:= 9007199254740991 of
        true ->
            {ok, {limit_eoi, Input}};

        false ->
            case erlang:element(8, Input) /= 16#003C of
                true ->
                    {ok, {limit_text, Input}};

                false ->
                    case next_char(Input) of
                        {error, E} ->
                            {error, E};

                        {ok, Input@1} ->
                            case erlang:element(8, Input@1) of
                                Char when Char =:= 16#003F ->
                                    parse_limit__pi(Input@1);

                                Char@1 when Char@1 =:= 16#002F ->
                                    parse_limit__end_tag(Input@1);

                                Char@2 when Char@2 =:= 16#0021 ->
                                    case next_char(Input@1) of
                                        {error, E@1} ->
                                            {error, E@1};

                                        {ok, Input@2} ->
                                            _ = case erlang:element(8, Input@2) of
                                                Char@3 when Char@3 =:= 16#002D ->
                                                    parse_limit__comment(
                                                        Input@2
                                                    );

                                                Char@4 when Char@4 =:= 16#0044 ->
                                                    {ok, {limit_dtd, Input@2}};

                                                Char@5 when Char@5 =:= 16#005B ->
                                                    parse_limit__cdata(Input@2);

                                                _ ->
                                                    error(
                                                        Input@2,
                                                        {illegal_char_seq,
                                                            <<"<!"/utf8,
                                                                (string_from_char(
                                                                    erlang:element(
                                                                        8,
                                                                        Input@2
                                                                    )
                                                                ))/binary>>}
                                                    )
                                            end
                                    end;

                                _ ->
                                    parse_limit__start_tag(Input@1)
                            end
                    end
            end
    end,
    case Result of
        {error, E@2} ->
            {error, E@2};

        {ok, {Limit, Input@3}} ->
            {ok,
                {input,
                    erlang:element(2, Input@3),
                    erlang:element(3, Input@3),
                    erlang:element(4, Input@3),
                    erlang:element(5, Input@3),
                    erlang:element(6, Input@3),
                    erlang:element(7, Input@3),
                    erlang:element(8, Input@3),
                    erlang:element(9, Input@3),
                    erlang:element(10, Input@3),
                    erlang:element(11, Input@3),
                    Limit,
                    erlang:element(13, Input@3),
                    erlang:element(14, Input@3),
                    erlang:element(15, Input@3),
                    erlang:element(16, Input@3),
                    erlang:element(17, Input@3),
                    erlang:element(18, Input@3),
                    erlang:element(19, Input@3)}}
    end.

-file("src/xmlm.gleam", 1885).
-spec skip_comment__loop(input()) -> {ok, input()} | {error, input_error()}.
skip_comment__loop(Input) ->
    case erlang:element(8, Input) /= 16#002D of
        true ->
            case next_char(Input) of
                {error, E} ->
                    {error, E};

                {ok, Input@1} ->
                    skip_comment__loop(Input@1)
            end;

        false ->
            {ok, Input}
    end.

-file("src/xmlm.gleam", 1858).
?DOC(" XML 1.0 non-terminal: {Comment}, '<!--' was eaten\n").
-spec skip_comment(input()) -> {ok, input()} | {error, input_error()}.
skip_comment(Input) ->
    case skip_comment__loop(Input) of
        {error, E} ->
            {error, E};

        {ok, Input@1} ->
            case next_char(Input@1) of
                {error, E@1} ->
                    {error, E@1};

                {ok, Input@2} ->
                    case erlang:element(8, Input@2) /= 16#002D of
                        true ->
                            skip_comment(Input@2);

                        false ->
                            case next_char(Input@2) of
                                {error, E@2} ->
                                    {error, E@2};

                                {ok, Input@3} ->
                                    case erlang:element(8, Input@3) /= 16#003E of
                                        true ->
                                            error_expected_chars(
                                                Input@3,
                                                [16#003E]
                                            );

                                        false ->
                                            next_char_eof(Input@3)
                                    end
                            end
                    end
            end
    end.

-file("src/xmlm.gleam", 1914).
-spec skip_pi__loop(input()) -> {ok, input()} | {error, input_error()}.
skip_pi__loop(Input) ->
    case erlang:element(8, Input) /= 16#003F of
        true ->
            case next_char(Input) of
                {ok, Input@1} ->
                    skip_pi__loop(Input@1);

                {error, _} = E ->
                    E
            end;

        false ->
            {ok, Input}
    end.

-file("src/xmlm.gleam", 1897).
?DOC(" XML 1.0 non-terminal: {PI}, '<?' qname was eaten\n").
-spec skip_pi(input()) -> {ok, input()} | {error, input_error()}.
skip_pi(Input) ->
    case skip_pi__loop(Input) of
        {error, E} ->
            {error, E};

        {ok, Input@1} ->
            case next_char(Input@1) of
                {error, E@1} ->
                    {error, E@1};

                {ok, Input@2} ->
                    case erlang:element(8, Input@2) /= 16#003E of
                        true ->
                            skip_pi(Input@2);

                        false ->
                            next_char_eof(Input@2)
                    end
            end
    end.

-file("src/xmlm.gleam", 3257).
-spec skip_whitespace_eof_then_parse_limit(input()) -> {ok, input()} |
    {error, input_error()}.
skip_whitespace_eof_then_parse_limit(Input) ->
    case skip_whitespace_eof(Input) of
        {error, E} ->
            {error, E};

        {ok, Input@1} ->
            parse_limit(Input@1)
    end.

-file("src/xmlm.gleam", 3243).
-spec skip_comment_then_parse_limit(input()) -> {ok, input()} |
    {error, input_error()}.
skip_comment_then_parse_limit(Input) ->
    case skip_comment(Input) of
        {error, E} ->
            {error, E};

        {ok, Input@1} ->
            parse_limit(Input@1)
    end.

-file("src/xmlm.gleam", 3236).
-spec skip_pi_then_parse_limit(input()) -> {ok, input()} |
    {error, input_error()}.
skip_pi_then_parse_limit(Input) ->
    case skip_pi(Input) of
        {error, E} ->
            {error, E};

        {ok, Input@1} ->
            parse_limit(Input@1)
    end.

-file("src/xmlm.gleam", 1926).
?DOC(" XML 1.0 non-terminal: {Misc}\n").
-spec skip_misc(input(), boolean()) -> {ok, input()} | {error, input_error()}.
skip_misc(Input, Allow_xmlpi) ->
    case erlang:element(12, Input) of
        {limit_pi, {name, Prefix, Local}} ->
            case gleam@string:is_empty(Prefix) andalso (<<"xml"/utf8>> =:= string:lowercase(
                Local
            )) of
                true ->
                    case Allow_xmlpi of
                        true ->
                            {ok, Input};

                        false ->
                            error(Input, {illegal_char_seq, Local})
                    end;

                false ->
                    case skip_pi_then_parse_limit(Input) of
                        {error, E} ->
                            {error, E};

                        {ok, Input@1} ->
                            skip_misc(Input@1, Allow_xmlpi)
                    end
            end;

        limit_comment ->
            case skip_comment_then_parse_limit(Input) of
                {error, E@1} ->
                    {error, E@1};

                {ok, Input@2} ->
                    skip_misc(Input@2, Allow_xmlpi)
            end;

        limit_text ->
            case is_whitespace(erlang:element(8, Input)) of
                true ->
                    case skip_whitespace_eof_then_parse_limit(Input) of
                        {error, E@2} ->
                            {error, E@2};

                        {ok, Input@3} ->
                            skip_misc(Input@3, Allow_xmlpi)
                    end;

                false ->
                    {ok, Input}
            end;

        limit_c_data ->
            {ok, Input};

        limit_dtd ->
            {ok, Input};

        {limit_end_tag, _} ->
            {ok, Input};

        limit_eoi ->
            {ok, Input};

        {limit_start_tag, _} ->
            {ok, Input}
    end.

-file("src/xmlm.gleam", 2068).
-spec parse_chardata__loop(input(), fun((input(), integer()) -> input())) -> {ok,
        input()} |
    {error, input_error()}.
parse_chardata__loop(Input, Add_char) ->
    case erlang:element(8, Input) =:= 16#005D of
        false ->
            {ok, Input};

        true ->
            Input@1 = Add_char(Input, erlang:element(8, Input)),
            case next_char(Input@1) of
                {error, E} ->
                    {error, E};

                {ok, Input@2} ->
                    parse_chardata__loop(Input@2, Add_char)
            end
    end.

-file("src/xmlm.gleam", 2025).
-spec parse_chardata__handle_rbrack(
    input(),
    fun((input(), integer()) -> input())
) -> {ok, input()} | {error, input_error()}.
parse_chardata__handle_rbrack(Input, Add_char) ->
    Input@1 = Add_char(Input, erlang:element(8, Input)),
    case next_char(Input@1) of
        {error, E} ->
            {error, E};

        {ok, Input@2} ->
            case erlang:element(8, Input@2) /= 16#005D of
                true ->
                    {ok, Input@2};

                false ->
                    Input@3 = Add_char(Input@2, erlang:element(8, Input@2)),
                    case next_char(Input@3) of
                        {error, E@1} ->
                            {error, E@1};

                        {ok, Input@4} ->
                            case parse_chardata__loop(Input@4, Add_char) of
                                {error, E@2} ->
                                    {error, E@2};

                                {ok, Input@5} ->
                                    case erlang:element(8, Input@5) =:= 16#003E of
                                        true ->
                                            error(
                                                Input@5,
                                                {illegal_char_seq,
                                                    <<"]]>"/utf8>>}
                                            );

                                        false ->
                                            {ok, Input@5}
                                    end
                            end
                    end
            end
    end.

-file("src/xmlm.gleam", 2011).
-spec parse_chardata__handle_non_rbrack(
    input(),
    fun((input(), integer()) -> input())
) -> {ok, input()} | {error, input_error()}.
parse_chardata__handle_non_rbrack(Input, Add_char) ->
    Input@1 = Add_char(Input, erlang:element(8, Input)),
    case next_char(Input@1) of
        {error, E} ->
            {error, E};

        {ok, Input@2} ->
            parse_chardata(Input@2, Add_char)
    end.

-file("src/xmlm.gleam", 1989).
-spec parse_chardata__handle_reference(
    input(),
    fun((input(), integer()) -> input())
) -> {ok, input()} | {error, input_error()}.
parse_chardata__handle_reference(Input, Add_char) ->
    case parse_reference(Input) of
        {error, E} ->
            {error, E};

        {ok, {Reference, Input@1}} ->
            Input@3 = begin
                _pipe = gleam@string:to_utf_codepoints(Reference),
                gleam@list:fold(
                    _pipe,
                    Input@1,
                    fun(Input@2, Char) ->
                        Add_char(Input@2, gleam_stdlib:identity(Char))
                    end
                )
            end,
            parse_chardata(Input@3, Add_char)
    end.

-file("src/xmlm.gleam", 1968).
?DOC(" XML 1.0 non-terminals: {CharData}* ({Reference}{Chardata})*\n").
-spec parse_chardata(input(), fun((input(), integer()) -> input())) -> {ok,
        input()} |
    {error, input_error()}.
parse_chardata(Input, Add_char) ->
    case erlang:element(8, Input) =:= 16#003C of
        true ->
            {ok, Input};

        false ->
            case erlang:element(8, Input) =:= 16#0026 of
                true ->
                    parse_chardata__handle_reference(Input, Add_char);

                false ->
                    case erlang:element(8, Input) =:= 16#005D of
                        true ->
                            parse_chardata__handle_rbrack(Input, Add_char);

                        false ->
                            parse_chardata__handle_non_rbrack(Input, Add_char)
                    end
            end
    end.

-file("src/xmlm.gleam", 2140).
-spec parse_cdata__loop__eat_rbrackets(
    input(),
    fun((input(), integer()) -> input())
) -> {ok, {input(), stop_or_go()}} | {error, input_error()}.
parse_cdata__loop__eat_rbrackets(Input, Add_char) ->
    case erlang:element(8, Input) /= 16#005D of
        true ->
            {ok, {Input, go}};

        false ->
            case next_char(Input) of
                {error, E} ->
                    {error, E};

                {ok, Input@1} ->
                    case erlang:element(8, Input@1) =:= 16#003E of
                        true ->
                            case next_char(Input@1) of
                                {error, E@1} ->
                                    {error, E@1};

                                {ok, Input@2} ->
                                    {ok, {Input@2, stop}}
                            end;

                        false ->
                            Input@3 = Add_char(Input@1, 16#005D),
                            parse_cdata__loop__eat_rbrackets(Input@3, Add_char)
                    end
            end
    end.

-file("src/xmlm.gleam", 2094).
-spec parse_cdata__loop(
    input(),
    fun((input(), integer()) -> input()),
    stop_or_go()
) -> {ok, input()} | {error, input_error()}.
parse_cdata__loop(Input, Add_char, Stop_or_go) ->
    case Stop_or_go =:= stop of
        true ->
            {ok, Input};

        false ->
            case erlang:element(8, Input) /= 16#005D of
                true ->
                    Input@1 = Add_char(Input, erlang:element(8, Input)),
                    case next_char(Input@1) of
                        {error, E} ->
                            {error, E};

                        {ok, Input@2} ->
                            parse_cdata__loop(Input@2, Add_char, Stop_or_go)
                    end;

                false ->
                    case next_char(Input) of
                        {error, E@1} ->
                            {error, E@1};

                        {ok, Input@3} ->
                            case parse_cdata__loop__eat_rbrackets(
                                Input@3,
                                Add_char
                            ) of
                                {error, E@2} ->
                                    {error, E@2};

                                {ok, {Input@4, Go}} ->
                                    case Go =:= stop of
                                        true ->
                                            {ok, Input@4};

                                        false ->
                                            Input@5 = Add_char(Input@4, 16#005D),
                                            parse_cdata__loop(
                                                Input@5,
                                                Add_char,
                                                go
                                            )
                                    end
                            end
                    end
            end
    end.

-file("src/xmlm.gleam", 2082).
?DOC(" XML 1.0 non-terminals: {CData} {CDEnd}\n").
-spec parse_cdata(input(), fun((input(), integer()) -> input())) -> {ok,
        input()} |
    {error, input_error()}.
parse_cdata(Input, Add_char) ->
    parse_cdata__loop(Input, Add_char, go).

-file("src/xmlm.gleam", 2307).
-spec parse_dtd_signal__loop__loop(input(), integer()) -> {ok, input()} |
    {error, input_error()}.
parse_dtd_signal__loop__loop(Input, Quot_or_apos) ->
    case erlang:element(8, Input) =:= Quot_or_apos of
        true ->
            {ok, Input};

        false ->
            Input@1 = add_char_to_data(Input, erlang:element(8, Input)),
            case next_char(Input@1) of
                {error, E} ->
                    {error, E};

                {ok, Input@2} ->
                    parse_dtd_signal__loop__loop(Input@2, Quot_or_apos)
            end
    end.

-file("src/xmlm.gleam", 3229).
-spec next_char_then_skip_comment(input()) -> {ok, input()} |
    {error, input_error()}.
next_char_then_skip_comment(Input) ->
    case next_char(Input) of
        {error, E} ->
            {error, E};

        {ok, Input@1} ->
            skip_comment(Input@1)
    end.

-file("src/xmlm.gleam", 2205).
-spec parse_dtd_signal__loop(input(), integer()) -> {ok, input()} |
    {error, input_error()}.
parse_dtd_signal__loop(Input, Nest) ->
    case Nest =< 0 of
        true ->
            {ok, Input};

        false ->
            case erlang:element(8, Input) =:= 16#003C of
                true ->
                    case next_char(Input) of
                        {error, E} ->
                            {error, E};

                        {ok, Input@1} ->
                            case erlang:element(8, Input@1) /= 16#0021 of
                                true ->
                                    Input@2 = add_char_to_data(Input@1, 16#003C),
                                    parse_dtd_signal__loop(Input@2, Nest + 1);

                                false ->
                                    case next_char(Input@1) of
                                        {error, E@1} ->
                                            {error, E@1};

                                        {ok, Input@3} ->
                                            case erlang:element(8, Input@3) /= 16#002D of
                                                true ->
                                                    Input@4 = add_char_to_data(
                                                        Input@3,
                                                        16#003C
                                                    ),
                                                    Input@5 = add_char_to_data(
                                                        Input@4,
                                                        16#0021
                                                    ),
                                                    parse_dtd_signal__loop(
                                                        Input@5,
                                                        Nest + 1
                                                    );

                                                false ->
                                                    case next_char(Input@3) of
                                                        {error, E@2} ->
                                                            {error, E@2};

                                                        {ok, Input@6} ->
                                                            case erlang:element(
                                                                8,
                                                                Input@6
                                                            )
                                                            /= 16#002D of
                                                                true ->
                                                                    Input@7 = add_char_to_data(
                                                                        Input@6,
                                                                        16#003C
                                                                    ),
                                                                    Input@8 = add_char_to_data(
                                                                        Input@7,
                                                                        16#0021
                                                                    ),
                                                                    Input@9 = add_char_to_data(
                                                                        Input@8,
                                                                        16#002D
                                                                    ),
                                                                    parse_dtd_signal__loop(
                                                                        Input@9,
                                                                        Nest + 1
                                                                    );

                                                                false ->
                                                                    case next_char_then_skip_comment(
                                                                        Input@6
                                                                    ) of
                                                                        {error,
                                                                            E@3} ->
                                                                            {error,
                                                                                E@3};

                                                                        {ok,
                                                                            Input@10} ->
                                                                            parse_dtd_signal__loop(
                                                                                Input@10,
                                                                                Nest
                                                                            )
                                                                    end
                                                            end
                                                    end
                                            end
                                    end
                            end
                    end;

                false ->
                    case (erlang:element(8, Input) =:= 16#0022) orelse (erlang:element(
                        8,
                        Input
                    )
                    =:= 16#0027) of
                        true ->
                            Quot_or_apos = erlang:element(8, Input),
                            Input@11 = add_char_to_data(Input, Quot_or_apos),
                            case next_char(Input@11) of
                                {error, E@4} ->
                                    {error, E@4};

                                {ok, Input@12} ->
                                    case parse_dtd_signal__loop__loop(
                                        Input@12,
                                        Quot_or_apos
                                    ) of
                                        {error, E@5} ->
                                            {error, E@5};

                                        {ok, Input@13} ->
                                            Input@14 = add_char_to_data(
                                                Input@13,
                                                Quot_or_apos
                                            ),
                                            case next_char(Input@14) of
                                                {error, E@6} ->
                                                    {error, E@6};

                                                {ok, Input@15} ->
                                                    parse_dtd_signal__loop(
                                                        Input@15,
                                                        Nest
                                                    )
                                            end
                                    end
                            end;

                        false ->
                            Nest@1 = case erlang:element(8, Input) =:= 16#003E of
                                true ->
                                    Nest - 1;

                                false ->
                                    Nest
                            end,
                            Input@16 = add_char_to_data(
                                Input,
                                erlang:element(8, Input)
                            ),
                            case next_char(Input@16) of
                                {error, E@7} ->
                                    {error, E@7};

                                {ok, Input@17} ->
                                    parse_dtd_signal__loop(Input@17, Nest@1)
                            end
                    end
            end
    end.

-file("src/xmlm.gleam", 2169).
?DOC(" XML 1.0 non-terminals: `{Misc}* {doctypedecl} {Misc}*`\n").
-spec parse_dtd_signal(input()) -> {ok, {signal(), input()}} |
    {error, input_error()}.
parse_dtd_signal(Input) ->
    case skip_misc(Input, false) of
        {error, E} ->
            {error, E};

        {ok, Input@1} ->
            case erlang:element(12, Input@1) /= limit_dtd of
                true ->
                    {ok, {{dtd, none}, Input@1}};

                false ->
                    Input@2 = clear_data(Input@1),
                    Input@3 = add_char_to_data(Input@2, 16#003C),
                    Input@4 = add_char_to_data(Input@3, 16#0021),
                    case parse_dtd_signal__loop(Input@4, 1) of
                        {error, E@1} ->
                            {error, E@1};

                        {ok, Input@5} ->
                            Dtd = buffer_to_string(erlang:element(19, Input@5)),
                            case parse_limit(Input@5) of
                                {error, E@2} ->
                                    {error, E@2};

                                {ok, Input@6} ->
                                    case skip_misc(Input@6, false) of
                                        {error, E@3} ->
                                            {error, E@3};

                                        {ok, Input@7} ->
                                            {ok, {{dtd, {some, Dtd}}, Input@7}}
                                    end
                            end
                    end
            end
    end.

-file("src/xmlm.gleam", 2338).
-spec parse_data__bufferize(input(), fun((input(), integer()) -> input())) -> {ok,
        input()} |
    {error, input_error()}.
parse_data__bufferize(Input, Add_char) ->
    case erlang:element(12, Input) of
        limit_text ->
            case parse_chardata(Input, Add_char) of
                {error, E} ->
                    {error, E};

                {ok, Input@1} ->
                    case parse_limit(Input@1) of
                        {error, E@1} ->
                            {error, E@1};

                        {ok, Input@2} ->
                            parse_data__bufferize(Input@2, Add_char)
                    end
            end;

        limit_c_data ->
            case parse_cdata(Input, Add_char) of
                {error, E@2} ->
                    {error, E@2};

                {ok, Input@3} ->
                    case parse_limit(Input@3) of
                        {error, E@3} ->
                            {error, E@3};

                        {ok, Input@4} ->
                            parse_data__bufferize(Input@4, Add_char)
                    end
            end;

        {limit_start_tag, _} ->
            {ok, Input};

        {limit_end_tag, _} ->
            {ok, Input};

        {limit_pi, _} ->
            case skip_pi(Input) of
                {error, E@4} ->
                    {error, E@4};

                {ok, Input@5} ->
                    case parse_limit(Input@5) of
                        {error, E@5} ->
                            {error, E@5};

                        {ok, Input@6} ->
                            parse_data__bufferize(Input@6, Add_char)
                    end
            end;

        limit_comment ->
            case skip_comment(Input) of
                {error, E@6} ->
                    {error, E@6};

                {ok, Input@7} ->
                    case parse_limit(Input@7) of
                        {error, E@7} ->
                            {error, E@7};

                        {ok, Input@8} ->
                            parse_data__bufferize(Input@8, Add_char)
                    end
            end;

        limit_dtd ->
            error(Input, {illegal_char_seq, <<"<!D"/utf8>>});

        limit_eoi ->
            error(Input, unexpected_eoi)
    end.

-file("src/xmlm.gleam", 2320).
-spec parse_data(input()) -> {ok, {binary(), input()}} | {error, input_error()}.
parse_data(Input) ->
    Input@1 = clear_data(Input),
    Input@2 = {input,
        erlang:element(2, Input@1),
        erlang:element(3, Input@1),
        erlang:element(4, Input@1),
        erlang:element(5, Input@1),
        erlang:element(6, Input@1),
        erlang:element(7, Input@1),
        erlang:element(8, Input@1),
        erlang:element(9, Input@1),
        erlang:element(10, Input@1),
        erlang:element(11, Input@1),
        erlang:element(12, Input@1),
        erlang:element(13, Input@1),
        erlang:element(14, Input@1),
        true,
        erlang:element(16, Input@1),
        erlang:element(17, Input@1),
        erlang:element(18, Input@1),
        erlang:element(19, Input@1)},
    Add_char = case erlang:element(14, Input@2) of
        true ->
            fun add_char_to_data_strip/2;

        false ->
            fun add_char_to_data/2
    end,
    case parse_data__bufferize(Input@2, Add_char) of
        {error, E} ->
            {error, E};

        {ok, Input@3} ->
            Data = buffer_to_string(erlang:element(19, Input@3)),
            {ok, {Data, Input@3}}
    end.

-file("src/xmlm.gleam", 2439).
-spec expand_attribute(input(), attribute()) -> {ok, {attribute(), input()}} |
    {error, input_error()}.
expand_attribute(Input, Attribute) ->
    {name, Prefix, Local} = erlang:element(2, Attribute),
    case Prefix of
        <<""/utf8>> ->
            case Local =:= <<"xmlns"/utf8>> of
                true ->
                    {ok,
                        {{attribute,
                                {name,
                                    <<"http://www.w3.org/2000/xmlns/"/utf8>>,
                                    <<"xmlns"/utf8>>},
                                erlang:element(3, Attribute)},
                            Input}};

                false ->
                    {ok, {Attribute, Input}}
            end;

        _ ->
            case expand_name(Input, erlang:element(2, Attribute)) of
                {error, E} ->
                    {error, E};

                {ok, Name} ->
                    {ok,
                        {{attribute, Name, erlang:element(3, Attribute)}, Input}}
            end
    end.

-file("src/xmlm.gleam", 2393).
-spec parse_element_start_signal(input(), name()) -> {ok, {signal(), input()}} |
    {error, input_error()}.
parse_element_start_signal(Input, Name) ->
    Strip = erlang:element(14, Input),
    case parse_attributes(Input) of
        {error, E} ->
            {error, E};

        {ok, {Prefixes, Attributes, Input@1}} ->
            Input@2 = {input,
                erlang:element(2, Input@1),
                erlang:element(3, Input@1),
                erlang:element(4, Input@1),
                erlang:element(5, Input@1),
                erlang:element(6, Input@1),
                erlang:element(7, Input@1),
                erlang:element(8, Input@1),
                erlang:element(9, Input@1),
                erlang:element(10, Input@1),
                erlang:element(11, Input@1),
                erlang:element(12, Input@1),
                erlang:element(13, Input@1),
                erlang:element(14, Input@1),
                erlang:element(15, Input@1),
                [{Name, Prefixes, Strip} | erlang:element(16, Input@1)],
                erlang:element(17, Input@1),
                erlang:element(18, Input@1),
                erlang:element(19, Input@1)},
            Attributes@1 = lists:reverse(Attributes),
            Result = gleam@list:fold(
                Attributes@1,
                {ok, {[], Input@2}},
                fun(Acc, Attribute) -> case Acc of
                        {error, E@1} ->
                            {error, E@1};

                        {ok, {Attributes@2, Input@3}} ->
                            case expand_attribute(Input@3, Attribute) of
                                {error, E@2} ->
                                    {error, E@2};

                                {ok, {Expanded_attribute, Input@4}} ->
                                    {ok,
                                        {[Expanded_attribute | Attributes@2],
                                            Input@4}}
                            end
                    end end
            ),
            case Result of
                {error, E@3} ->
                    {error, E@3};

                {ok, {Expanded_attributes, Input@5}} ->
                    case expand_name(Input@5, Name) of
                        {error, E@4} ->
                            {error, E@4};

                        {ok, Name@1} ->
                            Signal = {element_start,
                                {tag, Name@1, Expanded_attributes}},
                            {ok, {Signal, Input@5}}
                    end
            end
    end.

-file("src/xmlm.gleam", 2465).
-spec parse_element_end_signal(input(), name()) -> {ok, {signal(), input()}} |
    {error, input_error()}.
parse_element_end_signal(Input, Name) ->
    case erlang:element(16, Input) of
        [{Name_, Prefixes, Strip} | Scopes] ->
            case erlang:element(8, Input) /= 16#003E of
                true ->
                    error_expected_chars(Input, [16#003E]);

                false ->
                    case Name /= Name_ of
                        true ->
                            error_expected_seqs(
                                Input,
                                [name_to_string(Name_)],
                                name_to_string(Name)
                            );

                        false ->
                            Input@1 = {input,
                                erlang:element(2, Input),
                                erlang:element(3, Input),
                                erlang:element(4, Input),
                                erlang:element(5, Input),
                                erlang:element(6, Input),
                                erlang:element(7, Input),
                                erlang:element(8, Input),
                                erlang:element(9, Input),
                                erlang:element(10, Input),
                                erlang:element(11, Input),
                                erlang:element(12, Input),
                                erlang:element(13, Input),
                                Strip,
                                erlang:element(15, Input),
                                Scopes,
                                gleam@list:fold(
                                    Prefixes,
                                    erlang:element(17, Input),
                                    fun(Dict, Prefix) ->
                                        gleam@dict:delete(Dict, Prefix)
                                    end
                                ),
                                erlang:element(18, Input),
                                erlang:element(19, Input)},
                            Input@3 = case Scopes of
                                [] ->
                                    {ok,
                                        {input,
                                            erlang:element(2, Input@1),
                                            erlang:element(3, Input@1),
                                            erlang:element(4, Input@1),
                                            erlang:element(5, Input@1),
                                            erlang:element(6, Input@1),
                                            erlang:element(7, Input@1),
                                            9007199254740989,
                                            erlang:element(9, Input@1),
                                            erlang:element(10, Input@1),
                                            erlang:element(11, Input@1),
                                            erlang:element(12, Input@1),
                                            erlang:element(13, Input@1),
                                            erlang:element(14, Input@1),
                                            erlang:element(15, Input@1),
                                            erlang:element(16, Input@1),
                                            erlang:element(17, Input@1),
                                            erlang:element(18, Input@1),
                                            erlang:element(19, Input@1)}};

                                _ ->
                                    case next_char(Input@1) of
                                        {error, E} ->
                                            {error, E};

                                        {ok, Input@2} ->
                                            parse_limit(Input@2)
                                    end
                            end,
                            case Input@3 of
                                {error, E@1} ->
                                    {error, E@1};

                                {ok, Input@4} ->
                                    {ok, {element_end, Input@4}}
                            end
                    end
            end;

        _ ->
            erlang:error(#{gleam_error => panic,
                    message => <<"impossible"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"xmlm"/utf8>>,
                    function => <<"parse_element_end_signal"/utf8>>,
                    line => 2512})
    end.

-file("src/xmlm.gleam", 2578).
-spec parse_signal__find(input()) -> {ok, {signal(), input()}} |
    {error, input_error()}.
parse_signal__find(Input) ->
    case erlang:element(12, Input) of
        {limit_start_tag, Name} ->
            parse_element_start_signal(Input, Name);

        {limit_end_tag, Name@1} ->
            parse_element_end_signal(Input, Name@1);

        limit_text ->
            case parse_data(Input) of
                {error, E} ->
                    {error, E};

                {ok, {Data, Input@1}} ->
                    case gleam@string:is_empty(Data) of
                        true ->
                            parse_signal__find(Input@1);

                        false ->
                            {ok, {{data, Data}, Input@1}}
                    end
            end;

        limit_c_data ->
            case parse_data(Input) of
                {error, E} ->
                    {error, E};

                {ok, {Data, Input@1}} ->
                    case gleam@string:is_empty(Data) of
                        true ->
                            parse_signal__find(Input@1);

                        false ->
                            {ok, {{data, Data}, Input@1}}
                    end
            end;

        {limit_pi, _} ->
            case skip_pi_then_parse_limit(Input) of
                {error, E@1} ->
                    {error, E@1};

                {ok, Input@2} ->
                    parse_signal__find(Input@2)
            end;

        limit_comment ->
            case skip_comment_then_parse_limit(Input) of
                {error, E@2} ->
                    {error, E@2};

                {ok, Input@3} ->
                    parse_signal__find(Input@3)
            end;

        limit_dtd ->
            error(Input, {illegal_char_seq, <<"<!D"/utf8>>});

        limit_eoi ->
            error(Input, unexpected_eoi)
    end.

-file("src/xmlm.gleam", 3250).
-spec accept_then_parse_limit(input(), integer()) -> {ok, input()} |
    {error, input_error()}.
accept_then_parse_limit(Input, Char) ->
    case accept(Input, Char) of
        {error, E} ->
            {error, E};

        {ok, Input@1} ->
            parse_limit(Input@1)
    end.

-file("src/xmlm.gleam", 2534).
-spec parse_signal__non_empty_scope(input()) -> {ok, {signal(), input()}} |
    {error, input_error()}.
parse_signal__non_empty_scope(Input) ->
    Result = case erlang:element(13, Input) of
        {element_start, _} ->
            case skip_whitespace(Input) of
                {error, E} ->
                    {error, E};

                {ok, Input@1} ->
                    case erlang:element(8, Input@1) =:= 16#003E of
                        true ->
                            accept_then_parse_limit(Input@1, 16#003E);

                        false ->
                            case erlang:element(8, Input@1) =:= 16#002F of
                                true ->
                                    Tag@1 = case erlang:element(16, Input@1) of
                                        [{Tag, _, _} | _] ->
                                            Tag;

                                        _ ->
                                            erlang:error(#{gleam_error => panic,
                                                    message => <<"impossible"/utf8>>,
                                                    file => <<?FILEPATH/utf8>>,
                                                    module => <<"xmlm"/utf8>>,
                                                    function => <<"parse_signal__non_empty_scope"/utf8>>,
                                                    line => 2550})
                                    end,
                                    case next_char(Input@1) of
                                        {error, E@1} ->
                                            {error, E@1};

                                        {ok, Input@2} ->
                                            {ok,
                                                {input,
                                                    erlang:element(2, Input@2),
                                                    erlang:element(3, Input@2),
                                                    erlang:element(4, Input@2),
                                                    erlang:element(5, Input@2),
                                                    erlang:element(6, Input@2),
                                                    erlang:element(7, Input@2),
                                                    erlang:element(8, Input@2),
                                                    erlang:element(9, Input@2),
                                                    erlang:element(10, Input@2),
                                                    erlang:element(11, Input@2),
                                                    {limit_end_tag, Tag@1},
                                                    erlang:element(13, Input@2),
                                                    erlang:element(14, Input@2),
                                                    erlang:element(15, Input@2),
                                                    erlang:element(16, Input@2),
                                                    erlang:element(17, Input@2),
                                                    erlang:element(18, Input@2),
                                                    erlang:element(19, Input@2)}}
                                    end;

                                false ->
                                    error_expected_chars(
                                        Input@1,
                                        [16#002F, 16#003E]
                                    )
                            end
                    end
            end;

        _ ->
            {ok, Input}
    end,
    case Result of
        {error, E@2} ->
            {error, E@2};

        {ok, Input@3} ->
            parse_signal__find(Input@3)
    end.

-file("src/xmlm.gleam", 2523).
-spec parse_signal__empty_scope(input()) -> {ok, {signal(), input()}} |
    {error, input_error()}.
parse_signal__empty_scope(Input) ->
    case erlang:element(12, Input) of
        {limit_start_tag, Name} ->
            parse_element_start_signal(Input, Name);

        _ ->
            error(Input, expected_root_element)
    end.

-file("src/xmlm.gleam", 2516).
-spec parse_signal(input()) -> {ok, {signal(), input()}} |
    {error, input_error()}.
parse_signal(Input) ->
    case erlang:element(16, Input) of
        [] ->
            parse_signal__empty_scope(Input);

        _ ->
            parse_signal__non_empty_scope(Input)
    end.

-file("src/xmlm.gleam", 2757).
?DOC(" XML 1.0 non-terminal: {XMLDecl}?\n").
-spec parse_xml_declaration(input(), boolean(), boolean()) -> {ok, input()} |
    {error, input_error()}.
parse_xml_declaration(Input, Ignore_encoding, Ignore_utf16) ->
    Yes_no = [<<"yes"/utf8>>, <<"no"/utf8>>],
    Parse_val = fun(Input@1) ->
        gleam@result:'try'(
            skip_whitespace(Input@1),
            fun(Input@2) ->
                gleam@result:'try'(
                    accept(Input@2, 16#003D),
                    fun(Input@3) ->
                        gleam@result:'try'(
                            skip_whitespace(Input@3),
                            fun(Input@4) -> parse_attribute_value(Input@4) end
                        )
                    end
                )
            end
        )
    end,
    Parse_val_expected = fun(Input@5, Expected) ->
        gleam@result:'try'(
            Parse_val(Input@5),
            fun(_use0) ->
                {Val, Input@6} = _use0,
                case gleam@list:find(
                    Expected,
                    fun(Expected@1) -> Val =:= Expected@1 end
                ) of
                    {error, nil} ->
                        error_expected_seqs(Input@6, Expected, Val);

                    {ok, _} ->
                        {ok, Input@6}
                end
            end
        )
    end,
    case erlang:element(12, Input) of
        {limit_pi, {name, Uri, Local}} ->
            gleam@bool:guard(
                not (gleam@string:is_empty(Uri) andalso (Local =:= <<"xml"/utf8>>)),
                {ok, Input},
                fun() ->
                    gleam@result:'try'(
                        skip_whitespace(Input),
                        fun(Input@7) ->
                            gleam@result:'try'(
                                parse_ncname(Input@7),
                                fun(_use0@1) ->
                                    {V, Input@8} = _use0@1,
                                    gleam@bool:lazy_guard(
                                        V /= <<"version"/utf8>>,
                                        fun() ->
                                            error_expected_seqs(
                                                Input@8,
                                                [<<"version"/utf8>>],
                                                V
                                            )
                                        end,
                                        fun() ->
                                            gleam@result:'try'(
                                                Parse_val_expected(
                                                    Input@8,
                                                    [<<"1.0"/utf8>>,
                                                        <<"1.1"/utf8>>]
                                                ),
                                                fun(Input@9) ->
                                                    gleam@result:'try'(
                                                        skip_whitespace(Input@9),
                                                        fun(Input@10) ->
                                                            gleam@result:'try'(
                                                                (case erlang:element(
                                                                    8,
                                                                    Input@10
                                                                )
                                                                /= 16#003F of
                                                                    true ->
                                                                        gleam@result:'try'(
                                                                            parse_ncname(
                                                                                Input@10
                                                                            ),
                                                                            fun(
                                                                                _use0@2
                                                                            ) ->
                                                                                {Name,
                                                                                    Input@11} = _use0@2,
                                                                                gleam@result:'try'(
                                                                                    (case Name
                                                                                    =:= <<"encoding"/utf8>> of
                                                                                        true ->
                                                                                            gleam@result:'try'(
                                                                                                Parse_val(
                                                                                                    Input@11
                                                                                                ),
                                                                                                fun(
                                                                                                    _use0@3
                                                                                                ) ->
                                                                                                    {Encoding,
                                                                                                        Input@12} = _use0@3,
                                                                                                    Encoding@1 = string:lowercase(
                                                                                                        Encoding
                                                                                                    ),
                                                                                                    gleam@result:'try'(
                                                                                                        begin
                                                                                                            gleam@bool:guard(
                                                                                                                Ignore_encoding,
                                                                                                                {ok,
                                                                                                                    Input@12},
                                                                                                                fun(
                                                                                                                    
                                                                                                                ) ->
                                                                                                                    gleam@bool:guard(
                                                                                                                        Encoding@1
                                                                                                                        =:= <<"utf-8"/utf8>>,
                                                                                                                        {ok,
                                                                                                                            {input,
                                                                                                                                erlang:element(
                                                                                                                                    2,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    3,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    4,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    5,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                input_uchar_utf8(
                                                                                                                                    
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    7,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    8,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    9,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    10,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    11,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    12,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    13,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    14,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    15,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    16,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    17,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    18,
                                                                                                                                    Input@12
                                                                                                                                ),
                                                                                                                                erlang:element(
                                                                                                                                    19,
                                                                                                                                    Input@12
                                                                                                                                )}},
                                                                                                                        fun(
                                                                                                                            
                                                                                                                        ) ->
                                                                                                                            gleam@bool:guard(
                                                                                                                                Encoding@1
                                                                                                                                =:= <<"utf-16be"/utf8>>,
                                                                                                                                {ok,
                                                                                                                                    {input,
                                                                                                                                        erlang:element(
                                                                                                                                            2,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            3,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            4,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            5,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        input_uchar_utf16be(
                                                                                                                                            
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            7,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            8,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            9,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            10,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            11,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            12,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            13,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            14,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            15,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            16,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            17,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            18,
                                                                                                                                            Input@12
                                                                                                                                        ),
                                                                                                                                        erlang:element(
                                                                                                                                            19,
                                                                                                                                            Input@12
                                                                                                                                        )}},
                                                                                                                                fun(
                                                                                                                                    
                                                                                                                                ) ->
                                                                                                                                    gleam@bool:guard(
                                                                                                                                        Encoding@1
                                                                                                                                        =:= <<"utf-16le"/utf8>>,
                                                                                                                                        {ok,
                                                                                                                                            {input,
                                                                                                                                                erlang:element(
                                                                                                                                                    2,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    3,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    4,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    5,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                input_uchar_utf16le(
                                                                                                                                                    
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    7,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    8,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    9,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    10,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    11,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    12,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    13,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    14,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    15,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    16,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    17,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    18,
                                                                                                                                                    Input@12
                                                                                                                                                ),
                                                                                                                                                erlang:element(
                                                                                                                                                    19,
                                                                                                                                                    Input@12
                                                                                                                                                )}},
                                                                                                                                        fun(
                                                                                                                                            
                                                                                                                                        ) ->
                                                                                                                                            gleam@bool:guard(
                                                                                                                                                Encoding@1
                                                                                                                                                =:= <<"iso-8859-1"/utf8>>,
                                                                                                                                                {ok,
                                                                                                                                                    {input,
                                                                                                                                                        erlang:element(
                                                                                                                                                            2,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            3,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            4,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            5,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        input_uchar_iso_8859_1(
                                                                                                                                                            
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            7,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            8,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            9,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            10,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            11,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            12,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            13,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            14,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            15,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            16,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            17,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            18,
                                                                                                                                                            Input@12
                                                                                                                                                        ),
                                                                                                                                                        erlang:element(
                                                                                                                                                            19,
                                                                                                                                                            Input@12
                                                                                                                                                        )}},
                                                                                                                                                fun(
                                                                                                                                                    
                                                                                                                                                ) ->
                                                                                                                                                    gleam@bool:guard(
                                                                                                                                                        Encoding@1
                                                                                                                                                        =:= <<"iso-8859-15"/utf8>>,
                                                                                                                                                        {ok,
                                                                                                                                                            {input,
                                                                                                                                                                erlang:element(
                                                                                                                                                                    2,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    3,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    4,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    5,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                input_uchar_iso_8859_15(
                                                                                                                                                                    
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    7,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    8,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    9,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    10,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    11,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    12,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    13,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    14,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    15,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    16,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    17,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    18,
                                                                                                                                                                    Input@12
                                                                                                                                                                ),
                                                                                                                                                                erlang:element(
                                                                                                                                                                    19,
                                                                                                                                                                    Input@12
                                                                                                                                                                )}},
                                                                                                                                                        fun(
                                                                                                                                                            
                                                                                                                                                        ) ->
                                                                                                                                                            gleam@bool:guard(
                                                                                                                                                                Encoding@1
                                                                                                                                                                =:= <<"us-ascii"/utf8>>,
                                                                                                                                                                {ok,
                                                                                                                                                                    {input,
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            2,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            3,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            4,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            5,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        input_uchar_ascii(
                                                                                                                                                                            
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            7,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            8,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            9,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            10,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            11,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            12,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            13,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            14,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            15,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            16,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            17,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            18,
                                                                                                                                                                            Input@12
                                                                                                                                                                        ),
                                                                                                                                                                        erlang:element(
                                                                                                                                                                            19,
                                                                                                                                                                            Input@12
                                                                                                                                                                        )}},
                                                                                                                                                                fun(
                                                                                                                                                                    
                                                                                                                                                                ) ->
                                                                                                                                                                    gleam@bool:guard(
                                                                                                                                                                        Encoding@1
                                                                                                                                                                        =:= <<"ascii"/utf8>>,
                                                                                                                                                                        {ok,
                                                                                                                                                                            {input,
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    2,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    3,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    4,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    5,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                input_uchar_ascii(
                                                                                                                                                                                    
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    7,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    8,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    9,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    10,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    11,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    12,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    13,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    14,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    15,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    16,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    17,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    18,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                ),
                                                                                                                                                                                erlang:element(
                                                                                                                                                                                    19,
                                                                                                                                                                                    Input@12
                                                                                                                                                                                )}},
                                                                                                                                                                        fun(
                                                                                                                                                                            
                                                                                                                                                                        ) ->
                                                                                                                                                                            gleam@bool:lazy_guard(
                                                                                                                                                                                Encoding@1
                                                                                                                                                                                =:= <<"utf-16"/utf8>>,
                                                                                                                                                                                fun(
                                                                                                                                                                                    
                                                                                                                                                                                ) ->
                                                                                                                                                                                    case Ignore_utf16 of
                                                                                                                                                                                        true ->
                                                                                                                                                                                            {ok,
                                                                                                                                                                                                Input@12};

                                                                                                                                                                                        false ->
                                                                                                                                                                                            error(
                                                                                                                                                                                                Input@12,
                                                                                                                                                                                                malformed_char_stream
                                                                                                                                                                                            )
                                                                                                                                                                                    end
                                                                                                                                                                                end,
                                                                                                                                                                                fun(
                                                                                                                                                                                    
                                                                                                                                                                                ) ->
                                                                                                                                                                                    error(
                                                                                                                                                                                        Input@12,
                                                                                                                                                                                        {unknown_encoding,
                                                                                                                                                                                            Encoding@1}
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
                                                                                                                        end
                                                                                                                    )
                                                                                                                end
                                                                                                            )
                                                                                                        end,
                                                                                                        fun(
                                                                                                            Input@13
                                                                                                        ) ->
                                                                                                            gleam@result:'try'(
                                                                                                                skip_whitespace(
                                                                                                                    Input@13
                                                                                                                ),
                                                                                                                fun(
                                                                                                                    Input@14
                                                                                                                ) ->
                                                                                                                    gleam@bool:guard(
                                                                                                                        erlang:element(
                                                                                                                            8,
                                                                                                                            Input@14
                                                                                                                        )
                                                                                                                        =:= 16#003F,
                                                                                                                        {ok,
                                                                                                                            Input@14},
                                                                                                                        fun(
                                                                                                                            
                                                                                                                        ) ->
                                                                                                                            gleam@result:'try'(
                                                                                                                                parse_ncname(
                                                                                                                                    Input@14
                                                                                                                                ),
                                                                                                                                fun(
                                                                                                                                    _use0@4
                                                                                                                                ) ->
                                                                                                                                    {Name@1,
                                                                                                                                        Input@15} = _use0@4,
                                                                                                                                    case Name@1
                                                                                                                                    =:= <<"standalone"/utf8>> of
                                                                                                                                        true ->
                                                                                                                                            Parse_val_expected(
                                                                                                                                                Input@15,
                                                                                                                                                Yes_no
                                                                                                                                            );

                                                                                                                                        false ->
                                                                                                                                            error_expected_seqs(
                                                                                                                                                Input@15,
                                                                                                                                                [<<"standalone"/utf8>>,
                                                                                                                                                    <<"?>"/utf8>>],
                                                                                                                                                Name@1
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
                                                                                            );

                                                                                        false ->
                                                                                            case Name
                                                                                            =:= <<"standalone"/utf8>> of
                                                                                                true ->
                                                                                                    Parse_val_expected(
                                                                                                        Input@11,
                                                                                                        Yes_no
                                                                                                    );

                                                                                                false ->
                                                                                                    error_expected_seqs(
                                                                                                        Input@11,
                                                                                                        [<<"encoding"/utf8>>,
                                                                                                            <<"standalone"/utf8>>,
                                                                                                            <<"?>"/utf8>>],
                                                                                                        Name
                                                                                                    )
                                                                                            end
                                                                                    end),
                                                                                    fun(
                                                                                        Input@16
                                                                                    ) ->
                                                                                        {ok,
                                                                                            Input@16}
                                                                                    end
                                                                                )
                                                                            end
                                                                        );

                                                                    false ->
                                                                        {ok,
                                                                            Input@10}
                                                                end),
                                                                fun(Input@17) ->
                                                                    gleam@result:'try'(
                                                                        skip_whitespace(
                                                                            Input@17
                                                                        ),
                                                                        fun(
                                                                            Input@18
                                                                        ) ->
                                                                            gleam@result:'try'(
                                                                                accept(
                                                                                    Input@18,
                                                                                    16#003F
                                                                                ),
                                                                                fun(
                                                                                    Input@19
                                                                                ) ->
                                                                                    gleam@result:'try'(
                                                                                        accept(
                                                                                            Input@19,
                                                                                            16#003E
                                                                                        ),
                                                                                        fun(
                                                                                            Input@20
                                                                                        ) ->
                                                                                            parse_limit(
                                                                                                Input@20
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
                                end
                            )
                        end
                    )
                end
            );

        _ ->
            {ok, Input}
    end.

-file("src/xmlm.gleam", 2652).
-spec find_encoding(input()) -> {ok, {boolean(), input()}} |
    {error, input_error()}.
find_encoding(Input) ->
    Reset = fun(Input@1, Uchar) ->
        Input@2 = {input,
            erlang:element(2, Input@1),
            erlang:element(3, Input@1),
            erlang:element(4, Input@1),
            erlang:element(5, Input@1),
            Uchar,
            erlang:element(7, Input@1),
            erlang:element(8, Input@1),
            erlang:element(9, Input@1),
            erlang:element(10, Input@1),
            0,
            erlang:element(12, Input@1),
            erlang:element(13, Input@1),
            erlang:element(14, Input@1),
            erlang:element(15, Input@1),
            erlang:element(16, Input@1),
            erlang:element(17, Input@1),
            erlang:element(18, Input@1),
            erlang:element(19, Input@1)},
        next_char(Input@2)
    end,
    case erlang:element(2, Input) of
        none ->
            gleam@result:'try'(
                next_char(Input),
                fun(Input@3) -> case erlang:element(8, Input@3) of
                        16#FE ->
                            gleam@result:'try'(
                                next_char(Input@3),
                                fun(Input@4) ->
                                    gleam@bool:lazy_guard(
                                        erlang:element(8, Input@4) /= 16#FF,
                                        fun() ->
                                            error(
                                                Input@4,
                                                malformed_char_stream
                                            )
                                        end,
                                        fun() ->
                                            gleam@result:'try'(
                                                Reset(
                                                    Input@4,
                                                    input_uchar_utf16be()
                                                ),
                                                fun(Input@5) ->
                                                    {ok, {true, Input@5}}
                                                end
                                            )
                                        end
                                    )
                                end
                            );

                        16#FF ->
                            gleam@result:'try'(
                                next_char(Input@3),
                                fun(Input@6) ->
                                    gleam@bool:lazy_guard(
                                        erlang:element(8, Input@6) /= 16#FE,
                                        fun() ->
                                            error(
                                                Input@6,
                                                malformed_char_stream
                                            )
                                        end,
                                        fun() ->
                                            gleam@result:'try'(
                                                Reset(
                                                    Input@6,
                                                    input_uchar_utf16le()
                                                ),
                                                fun(Input@7) ->
                                                    {ok, {true, Input@7}}
                                                end
                                            )
                                        end
                                    )
                                end
                            );

                        16#EF ->
                            gleam@result:'try'(
                                next_char(Input@3),
                                fun(Input@8) ->
                                    gleam@bool:lazy_guard(
                                        erlang:element(8, Input@8) /= 16#BB,
                                        fun() ->
                                            error(
                                                Input@8,
                                                malformed_char_stream
                                            )
                                        end,
                                        fun() ->
                                            gleam@result:'try'(
                                                next_char(Input@8),
                                                fun(Input@9) ->
                                                    gleam@bool:lazy_guard(
                                                        erlang:element(
                                                            8,
                                                            Input@9
                                                        )
                                                        /= 16#BF,
                                                        fun() ->
                                                            error(
                                                                Input@9,
                                                                malformed_char_stream
                                                            )
                                                        end,
                                                        fun() ->
                                                            gleam@result:'try'(
                                                                Reset(
                                                                    Input@9,
                                                                    input_uchar_utf8(
                                                                        
                                                                    )
                                                                ),
                                                                fun(Input@10) ->
                                                                    {ok,
                                                                        {true,
                                                                            Input@10}}
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            );

                        16#3C ->
                            {ok,
                                {false,
                                    {input,
                                        erlang:element(2, Input@3),
                                        erlang:element(3, Input@3),
                                        erlang:element(4, Input@3),
                                        erlang:element(5, Input@3),
                                        input_uchar_utf8(),
                                        erlang:element(7, Input@3),
                                        erlang:element(8, Input@3),
                                        erlang:element(9, Input@3),
                                        erlang:element(10, Input@3),
                                        erlang:element(11, Input@3),
                                        erlang:element(12, Input@3),
                                        erlang:element(13, Input@3),
                                        erlang:element(14, Input@3),
                                        erlang:element(15, Input@3),
                                        erlang:element(16, Input@3),
                                        erlang:element(17, Input@3),
                                        erlang:element(18, Input@3),
                                        erlang:element(19, Input@3)}}};

                        _ ->
                            {ok,
                                {false,
                                    {input,
                                        erlang:element(2, Input@3),
                                        erlang:element(3, Input@3),
                                        erlang:element(4, Input@3),
                                        erlang:element(5, Input@3),
                                        input_uchar_utf8(),
                                        erlang:element(7, Input@3),
                                        erlang:element(8, Input@3),
                                        erlang:element(9, Input@3),
                                        erlang:element(10, Input@3),
                                        erlang:element(11, Input@3),
                                        erlang:element(12, Input@3),
                                        erlang:element(13, Input@3),
                                        erlang:element(14, Input@3),
                                        erlang:element(15, Input@3),
                                        erlang:element(16, Input@3),
                                        erlang:element(17, Input@3),
                                        erlang:element(18, Input@3),
                                        erlang:element(19, Input@3)}}}
                    end end
            );

        {some, Encoding} ->
            gleam@result:'try'(case Encoding of
                    us_ascii ->
                        Reset(Input, input_uchar_ascii());

                    iso8859x1 ->
                        Reset(Input, input_uchar_iso_8859_1());

                    iso8859x15 ->
                        Reset(Input, input_uchar_iso_8859_15());

                    utf8 ->
                        gleam@result:'try'(
                            Reset(Input, input_uchar_utf8()),
                            fun(Input@11) ->
                                gleam@bool:lazy_guard(
                                    erlang:element(8, Input@11) =:= 16#FEFF,
                                    fun() ->
                                        Input@12 = {input,
                                            erlang:element(2, Input@11),
                                            erlang:element(3, Input@11),
                                            erlang:element(4, Input@11),
                                            erlang:element(5, Input@11),
                                            erlang:element(6, Input@11),
                                            erlang:element(7, Input@11),
                                            erlang:element(8, Input@11),
                                            erlang:element(9, Input@11),
                                            erlang:element(10, Input@11),
                                            0,
                                            erlang:element(12, Input@11),
                                            erlang:element(13, Input@11),
                                            erlang:element(14, Input@11),
                                            erlang:element(15, Input@11),
                                            erlang:element(16, Input@11),
                                            erlang:element(17, Input@11),
                                            erlang:element(18, Input@11),
                                            erlang:element(19, Input@11)},
                                        gleam@result:'try'(
                                            next_char(Input@12),
                                            fun(Input@13) -> {ok, Input@13} end
                                        )
                                    end,
                                    fun() -> {ok, Input@11} end
                                )
                            end
                        );

                    utf16 ->
                        gleam@result:'try'(
                            next_char(Input),
                            fun(Input@14) ->
                                Byte0 = erlang:element(8, Input@14),
                                gleam@result:'try'(
                                    next_char(Input@14),
                                    fun(Input@15) ->
                                        Byte1 = erlang:element(8, Input@15),
                                        case {Byte0, Byte1} of
                                            {16#FE, 16#FF} ->
                                                Reset(
                                                    Input@15,
                                                    input_uchar_utf16be()
                                                );

                                            {16#FF, 16#FE} ->
                                                Reset(
                                                    Input@15,
                                                    input_uchar_utf16le()
                                                );

                                            {_, _} ->
                                                error(
                                                    Input@15,
                                                    malformed_char_stream
                                                )
                                        end
                                    end
                                )
                            end
                        );

                    utf16_be ->
                        gleam@result:'try'(
                            Reset(Input, input_uchar_utf16be()),
                            fun(Input@16) ->
                                gleam@bool:lazy_guard(
                                    erlang:element(8, Input@16) =:= 16#FEFF,
                                    fun() ->
                                        Input@17 = {input,
                                            erlang:element(2, Input@16),
                                            erlang:element(3, Input@16),
                                            erlang:element(4, Input@16),
                                            erlang:element(5, Input@16),
                                            erlang:element(6, Input@16),
                                            erlang:element(7, Input@16),
                                            erlang:element(8, Input@16),
                                            erlang:element(9, Input@16),
                                            erlang:element(10, Input@16),
                                            0,
                                            erlang:element(12, Input@16),
                                            erlang:element(13, Input@16),
                                            erlang:element(14, Input@16),
                                            erlang:element(15, Input@16),
                                            erlang:element(16, Input@16),
                                            erlang:element(17, Input@16),
                                            erlang:element(18, Input@16),
                                            erlang:element(19, Input@16)},
                                        gleam@result:'try'(
                                            next_char(Input@17),
                                            fun(Input@18) -> {ok, Input@18} end
                                        )
                                    end,
                                    fun() -> {ok, Input@16} end
                                )
                            end
                        );

                    utf16_le ->
                        gleam@result:'try'(
                            Reset(Input, input_uchar_utf16le()),
                            fun(Input@19) ->
                                gleam@bool:lazy_guard(
                                    erlang:element(8, Input@19) =:= 16#FEFF,
                                    fun() ->
                                        Input@20 = {input,
                                            erlang:element(2, Input@19),
                                            erlang:element(3, Input@19),
                                            erlang:element(4, Input@19),
                                            erlang:element(5, Input@19),
                                            erlang:element(6, Input@19),
                                            erlang:element(7, Input@19),
                                            erlang:element(8, Input@19),
                                            erlang:element(9, Input@19),
                                            erlang:element(10, Input@19),
                                            0,
                                            erlang:element(12, Input@19),
                                            erlang:element(13, Input@19),
                                            erlang:element(14, Input@19),
                                            erlang:element(15, Input@19),
                                            erlang:element(16, Input@19),
                                            erlang:element(17, Input@19),
                                            erlang:element(18, Input@19),
                                            erlang:element(19, Input@19)},
                                        gleam@result:'try'(
                                            next_char(Input@20),
                                            fun(Input@21) -> {ok, Input@21} end
                                        )
                                    end,
                                    fun() -> {ok, Input@19} end
                                )
                            end
                        )
                end, fun(Input@22) -> {ok, {true, Input@22}} end)
    end.

-file("src/xmlm.gleam", 2616).
?DOC(" `eoi(input)` tells if the end of input is reached.\n").
-spec eoi(input()) -> {ok, {boolean(), input()}} | {error, input_error()}.
eoi(Input) ->
    gleam@bool:guard(
        erlang:element(8, Input) =:= 9007199254740991,
        {ok, {true, Input}},
        fun() ->
            gleam@bool:guard(
                erlang:element(8, Input) /= 9007199254740990,
                {ok, {false, Input}},
                fun() ->
                    gleam@bool:lazy_guard(
                        erlang:element(13, Input) /= element_end,
                        fun() ->
                            gleam@result:'try'(
                                find_encoding(Input),
                                fun(_use0) ->
                                    {Ignore_incoding, Input@1} = _use0,
                                    gleam@result:'try'(
                                        parse_limit(Input@1),
                                        fun(Input@2) ->
                                            gleam@result:'try'(
                                                parse_xml_declaration(
                                                    Input@2,
                                                    Ignore_incoding,
                                                    false
                                                ),
                                                fun(Input@3) ->
                                                    gleam@result:'try'(
                                                        parse_dtd_signal(
                                                            Input@3
                                                        ),
                                                        fun(_use0@1) ->
                                                            {Signal, Input@4} = _use0@1,
                                                            Input@5 = {input,
                                                                erlang:element(
                                                                    2,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    3,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    4,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    5,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    6,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    7,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    8,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    9,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    10,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    11,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    12,
                                                                    Input@4
                                                                ),
                                                                Signal,
                                                                erlang:element(
                                                                    14,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    15,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    16,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    17,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    18,
                                                                    Input@4
                                                                ),
                                                                erlang:element(
                                                                    19,
                                                                    Input@4
                                                                )},
                                                            {ok,
                                                                {false, Input@5}}
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end,
                        fun() ->
                            gleam@result:'try'(
                                next_char_eof(Input),
                                fun(Input@6) ->
                                    gleam@result:'try'(
                                        parse_limit(Input@6),
                                        fun(Input@7) ->
                                            gleam@bool:guard(
                                                erlang:element(8, Input@7) =:= 9007199254740991,
                                                {ok, {true, Input@7}},
                                                fun() ->
                                                    gleam@result:'try'(
                                                        skip_misc(Input@7, true),
                                                        fun(Input@8) ->
                                                            gleam@bool:guard(
                                                                erlang:element(
                                                                    8,
                                                                    Input@8
                                                                )
                                                                =:= 9007199254740991,
                                                                {ok,
                                                                    {true,
                                                                        Input@8}},
                                                                fun() ->
                                                                    gleam@result:'try'(
                                                                        parse_xml_declaration(
                                                                            Input@8,
                                                                            false,
                                                                            true
                                                                        ),
                                                                        fun(
                                                                            Input@9
                                                                        ) ->
                                                                            gleam@result:'try'(
                                                                                parse_dtd_signal(
                                                                                    Input@9
                                                                                ),
                                                                                fun(
                                                                                    _use0@2
                                                                                ) ->
                                                                                    {Signal@1,
                                                                                        Input@10} = _use0@2,
                                                                                    Input@11 = {input,
                                                                                        erlang:element(
                                                                                            2,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            3,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            4,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            5,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            6,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            7,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            8,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            9,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            10,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            11,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            12,
                                                                                            Input@10
                                                                                        ),
                                                                                        Signal@1,
                                                                                        erlang:element(
                                                                                            14,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            15,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            16,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            17,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            18,
                                                                                            Input@10
                                                                                        ),
                                                                                        erlang:element(
                                                                                            19,
                                                                                            Input@10
                                                                                        )},
                                                                                    {ok,
                                                                                        {false,
                                                                                            Input@11}}
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
                end
            )
        end
    ).

-file("src/xmlm.gleam", 2904).
?DOC(
    " `xmlm.peek(input)` is the same as `xmlm.signal(input)` except that\n"
    " the signal is not removed from the sequence.\n"
).
-spec peek(input()) -> {ok, {signal(), input()}} | {error, input_error()}.
peek(Input) ->
    case eoi(Input) of
        {error, E} ->
            {error, E};

        {ok, {true, Input@1}} ->
            error(Input@1, unexpected_eoi);

        {ok, {false, Input@2}} ->
            {ok, {erlang:element(13, Input@2), Input@2}}
    end.

-file("src/xmlm.gleam", 2924).
?DOC(
    " `xmlm.signal(input)` inputs a `Signal`.  \n"
    " \n"
    " Repeatedly invoking the function with the same input abstraction will\n"
    " either generate a well-formed sequence apple shton of signals or raise an\n"
    " error. Additionally, no two consecutive Data signals can appear in the\n"
    " sequence, and their strings will always be non-empty.\n"
    " \n"
    " Note: Currently, after a well-formed sequence has been input, another \n"
    " sequence can be input.  However, this behavior is **deprecated**. (It is \n"
    " inherited from the OCaml library on which this one is based, and will chage \n"
    " at some point in the future.)\n"
).
-spec signal(input()) -> {ok, {signal(), input()}} | {error, input_error()}.
signal(Input) ->
    case erlang:element(8, Input) =:= 9007199254740989 of
        true ->
            Input@1 = {input,
                erlang:element(2, Input),
                erlang:element(3, Input),
                erlang:element(4, Input),
                erlang:element(5, Input),
                erlang:element(6, Input),
                erlang:element(7, Input),
                9007199254740990,
                erlang:element(9, Input),
                erlang:element(10, Input),
                erlang:element(11, Input),
                erlang:element(12, Input),
                erlang:element(13, Input),
                erlang:element(14, Input),
                erlang:element(15, Input),
                erlang:element(16, Input),
                erlang:element(17, Input),
                erlang:element(18, Input),
                erlang:element(19, Input)},
            {ok, {erlang:element(13, Input@1), Input@1}};

        false ->
            case peek(Input) of
                {error, E} ->
                    {error, E};

                {ok, {Signal, Input@2}} ->
                    case parse_signal(Input@2) of
                        {error, E@1} ->
                            {error, E@1};

                        {ok, {Peeked_signal, Input@3}} ->
                            Input@4 = {input,
                                erlang:element(2, Input@3),
                                erlang:element(3, Input@3),
                                erlang:element(4, Input@3),
                                erlang:element(5, Input@3),
                                erlang:element(6, Input@3),
                                erlang:element(7, Input@3),
                                erlang:element(8, Input@3),
                                erlang:element(9, Input@3),
                                erlang:element(10, Input@3),
                                erlang:element(11, Input@3),
                                erlang:element(12, Input@3),
                                Peeked_signal,
                                erlang:element(14, Input@3),
                                erlang:element(15, Input@3),
                                erlang:element(16, Input@3),
                                erlang:element(17, Input@3),
                                erlang:element(18, Input@3),
                                erlang:element(19, Input@3)},
                            {ok, {Signal, Input@4}}
                    end
            end
    end.

-file("src/xmlm.gleam", 2955).
-spec do_input_signals(input(), list(signal())) -> {ok,
        {list(signal()), input()}} |
    {error, input_error()}.
do_input_signals(Input, Acc) ->
    case eoi(Input) of
        {error, E} ->
            {error, E};

        {ok, {true, Input@1}} ->
            {ok, {lists:reverse(Acc), Input@1}};

        {ok, {false, Input@2}} ->
            case signal(Input@2) of
                {ok, {Signal, Input@3}} ->
                    do_input_signals(Input@3, [Signal | Acc]);

                {error, {input_error, Position, unicode_lexer_error_eoi}} ->
                    {error, {input_error, Position, unexpected_eoi}};

                {error, E@1} ->
                    {error, E@1}
            end
    end.

-file("src/xmlm.gleam", 2951).
?DOC(" Return a list of all `Signals` in the given `input`.\n").
-spec signals(input()) -> {ok, {list(signal()), input()}} |
    {error, input_error()}.
signals(Input) ->
    do_input_signals(Input, []).

-file("src/xmlm.gleam", 2980).
?DOC(
    " `xmlm.fold_signals(over: input, from: acc, with: f)` reduces the `Signals` \n"
    " of the `input` to a single value starting with `acc` by calling the given \n"
    " function `f` on each `Signal` in the `input`.\n"
).
-spec fold_signals(input(), EOY, fun((EOY, signal()) -> EOY)) -> {ok,
        {EOY, input()}} |
    {error, input_error()}.
fold_signals(Input, Acc, F) ->
    case eoi(Input) of
        {error, E} ->
            {error, E};

        {ok, {true, Input@1}} ->
            {ok, {Acc, Input@1}};

        {ok, {false, Input@2}} ->
            case signal(Input@2) of
                {ok, {Signal, Input@3}} ->
                    fold_signals(Input@3, F(Acc, Signal), F);

                {error, {input_error, Position, unicode_lexer_error_eoi}} ->
                    {error, {input_error, Position, unexpected_eoi}};

                {error, E@1} ->
                    {error, E@1}
            end
    end.

-file("src/xmlm.gleam", 3047).
-spec tree__loop(
    input(),
    list(tag()),
    list(list(EPG)),
    fun((tag(), list(EPG)) -> EPG),
    fun((binary()) -> EPG)
) -> {ok, {EPG, input()}} | {error, input_error()}.
tree__loop(Input, Tags, Context, Element_callback, Data_callback) ->
    case signal(Input) of
        {error, E} ->
            {error, E};

        {ok, {Signal, Input@1}} ->
            case Signal of
                {element_start, Tag} ->
                    tree__loop(
                        Input@1,
                        [Tag | Tags],
                        [[] | Context],
                        Element_callback,
                        Data_callback
                    );

                element_end ->
                    case {Tags, Context} of
                        {[Tag@1 | Tags_], [Children | Context_]} ->
                            Element = Element_callback(
                                Tag@1,
                                lists:reverse(Children)
                            ),
                            case Context_ of
                                [Parent | Context__] ->
                                    tree__loop(
                                        Input@1,
                                        Tags_,
                                        [[Element | Parent] | Context__],
                                        Element_callback,
                                        Data_callback
                                    );

                                [] ->
                                    {ok, {Element, Input@1}}
                            end;

                        {_, _} ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"impossible"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"xmlm"/utf8>>,
                                    function => <<"tree__loop"/utf8>>,
                                    line => 3082})
                    end;

                {data, Data} ->
                    case Context of
                        [Children@1 | Context_@1] ->
                            More_context = [Data_callback(Data) | Children@1],
                            tree__loop(
                                Input@1,
                                Tags,
                                [More_context | Context_@1],
                                Element_callback,
                                Data_callback
                            );

                        [] ->
                            erlang:error(#{gleam_error => panic,
                                    message => <<"impossible"/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"xmlm"/utf8>>,
                                    function => <<"tree__loop"/utf8>>,
                                    line => 3096})
                    end;

                {dtd, _} ->
                    erlang:error(#{gleam_error => panic,
                            message => <<"impossible"/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"xmlm"/utf8>>,
                            function => <<"tree__loop"/utf8>>,
                            line => 3098})
            end
    end.

-file("src/xmlm.gleam", 3022).
?DOC(
    " `xmlm.tree(input, element_callback, data_callback)` inputs signals in \n"
    " different ways depending on the next signal.\n"
    " \n"
    " If the next signal is a...\n"
    " \n"
    " - `Data` signal, `tree` inputs the signal and invokes `data_callback` with \n"
    "   the character data of the signal.\n"
    " - `ElementStart` signal, `tree` inputs the sequence of signals until its \n"
    "   matching `ElementEnd` and envokes `element_callback` and `data_callback` \n"
    "   as follows:\n"
    "   - `element_callback` is called on each `ElementEnd` signal with the \n"
    "     corresponding `ElementStart` tag and the result of the callback \n"
    "     invocation for the element's children.\n"
    "   - `data_callback` is called on each `Data` signal with the character data.  \n"
    "     This function won't be called twice consecutively or with the empty \n"
    "     string.\n"
    " - Other signals, returns an error.\n"
    " \n"
    " See [document_tree](#document_tree) for getting the entire document as a \n"
    " tree.\n"
).
-spec tree(input(), fun((tag(), list(EPB)) -> EPB), fun((binary()) -> EPB)) -> {ok,
        {EPB, input()}} |
    {error, input_error()}.
tree(Input, Element_callback, Data_callback) ->
    case signal(Input) of
        {error, E} ->
            {error, E};

        {ok, {Signal, Input@1}} ->
            case Signal of
                {data, Data} ->
                    {ok, {Data_callback(Data), Input@1}};

                {element_start, Tag} ->
                    tree__loop(
                        Input@1,
                        [Tag | []],
                        [[] | []],
                        Element_callback,
                        Data_callback
                    );

                {dtd, _} ->
                    error(
                        Input@1,
                        {invalid_argument,
                            <<"input signal not ElementStart or Data"/utf8>>}
                    );

                element_end ->
                    error(
                        Input@1,
                        {invalid_argument,
                            <<"input signal not ElementStart or Data"/utf8>>}
                    )
            end
    end.

-file("src/xmlm.gleam", 3109).
?DOC(
    " `xmlm.document_tree(input, element_callback, data_callback)` reads a \n"
    " complete, well-formed sequence of signals.\n"
    " \n"
    " See [tree](#tree) for getting a tree produced by a single `Signal`.\n"
).
-spec document_tree(
    input(),
    fun((tag(), list(EPM)) -> EPM),
    fun((binary()) -> EPM)
) -> {ok, {gleam@option:option(binary()), EPM, input()}} |
    {error, input_error()}.
document_tree(Input, Element_callback, Data_callback) ->
    case signal(Input) of
        {error, E} ->
            {error, E};

        {ok, {Signal, Input@1}} ->
            case Signal of
                {dtd, Data} ->
                    case tree(Input@1, Element_callback, Data_callback) of
                        {error, E@1} ->
                            {error, E@1};

                        {ok, {A, Input@2}} ->
                            {ok, {Data, A, Input@2}}
                    end;

                _ ->
                    error(
                        Input@1,
                        {invalid_argument, <<"input signal not Dtd"/utf8>>}
                    )
            end
    end.
