-module(vxml@blame).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/vxml/blame.gleam").
-export([clear_comments/1, prepend_comment/2, append_comment/2, advance/2, set_anchored/1, blame_digest/1, comments_digest/1, path_contains/2, blamed_strings_annotated_table/4]).
-export_type([source_cursor/0, blame/0, blame_table_margin_columns_min_max/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Source provenance for VXML trees.\n"
    "\n"
    " `Blame` values attach origin information to VXML nodes, attributes, and\n"
    " lines. Source blame can be movable or anchored, which lets parsers and\n"
    " transformation pipelines preserve useful locations while slicing or moving\n"
    " text.\n"
).

-type source_cursor() :: movable | anchored.

-type blame() :: {src,
        list(binary()),
        binary(),
        integer(),
        integer(),
        source_cursor()} |
    {des, list(binary()), binary(), integer()} |
    {ext, list(binary()), binary()} |
    {no_blame, list(binary())}.

-type blame_table_margin_columns_min_max() :: {blame_table_margin_columns_min_max,
        integer(),
        integer()}.

-file("src/vxml/blame.gleam", 39).
-spec spaces(integer()) -> binary().
spaces(I) ->
    gleam@string:repeat(<<" "/utf8>>, I).

-file("src/vxml/blame.gleam", 50).
?DOC(" Remove comments while preserving the main blame identity.\n").
-spec clear_comments(blame()) -> blame().
clear_comments(Blame) ->
    case Blame of
        {src, _, _, _, _, _} ->
            {src,
                [],
                erlang:element(3, Blame),
                erlang:element(4, Blame),
                erlang:element(5, Blame),
                erlang:element(6, Blame)};

        {des, _, _, _} ->
            {des, [], erlang:element(3, Blame), erlang:element(4, Blame)};

        {ext, _, _} ->
            {ext, [], erlang:element(3, Blame)};

        {no_blame, _} ->
            {no_blame, []}
    end.

-file("src/vxml/blame.gleam", 60).
?DOC(" Add a comment before existing blame comments.\n").
-spec prepend_comment(blame(), binary()) -> blame().
prepend_comment(Blame, Comment) ->
    case Blame of
        {src, _, _, _, _, _} ->
            {src,
                [Comment | erlang:element(2, Blame)],
                erlang:element(3, Blame),
                erlang:element(4, Blame),
                erlang:element(5, Blame),
                erlang:element(6, Blame)};

        {des, _, _, _} ->
            {des,
                [Comment | erlang:element(2, Blame)],
                erlang:element(3, Blame),
                erlang:element(4, Blame)};

        {ext, _, _} ->
            {ext,
                [Comment | erlang:element(2, Blame)],
                erlang:element(3, Blame)};

        {no_blame, _} ->
            {no_blame, [Comment | erlang:element(2, Blame)]}
    end.

-file("src/vxml/blame.gleam", 69).
-spec append_comment(blame(), binary()) -> blame().
append_comment(Blame, Comment) ->
    case Blame of
        {src, _, _, _, _, _} ->
            {src,
                lists:append(erlang:element(2, Blame), [Comment]),
                erlang:element(3, Blame),
                erlang:element(4, Blame),
                erlang:element(5, Blame),
                erlang:element(6, Blame)};

        {des, _, _, _} ->
            {des,
                lists:append(erlang:element(2, Blame), [Comment]),
                erlang:element(3, Blame),
                erlang:element(4, Blame)};

        {ext, _, _} ->
            {ext,
                lists:append(erlang:element(2, Blame), [Comment]),
                erlang:element(3, Blame)};

        {no_blame, _} ->
            {no_blame, lists:append(erlang:element(2, Blame), [Comment])}
    end.

-file("src/vxml/blame.gleam", 79).
?DOC(" Advance movable source blame by a character offset.\n").
-spec advance(blame(), integer()) -> blame().
advance(Blame, By) ->
    case Blame of
        {src, _, _, _, _, movable} ->
            {src,
                erlang:element(2, Blame),
                erlang:element(3, Blame),
                erlang:element(4, Blame),
                erlang:element(5, Blame) + By,
                erlang:element(6, Blame)};

        _ ->
            Blame
    end.

-file("src/vxml/blame.gleam", 87).
?DOC(" Mark source blame as anchored.\n").
-spec set_anchored(blame()) -> blame().
set_anchored(Blame) ->
    case Blame of
        {src, _, _, _, _, _} ->
            {src,
                erlang:element(2, Blame),
                erlang:element(3, Blame),
                erlang:element(4, Blame),
                erlang:element(5, Blame),
                anchored};

        _ ->
            Blame
    end.

-file("src/vxml/blame.gleam", 95).
?DOC(" Render a short human-readable blame label.\n").
-spec blame_digest(blame()) -> binary().
blame_digest(Blame) ->
    case Blame of
        {src, _, Path, Line_no, Char_no, Cursor} ->
            case Cursor of
                movable ->
                    <<<<<<<<Path/binary, ":"/utf8>>/binary,
                                (gleam@string:inspect(Line_no))/binary>>/binary,
                            ":"/utf8>>/binary,
                        (gleam@string:inspect(Char_no))/binary>>;

                anchored ->
                    <<<<<<<<<<Path/binary, ":"/utf8>>/binary,
                                    (gleam@string:inspect(Line_no))/binary>>/binary,
                                ":"/utf8>>/binary,
                            (gleam@string:inspect(Char_no))/binary>>/binary,
                        " ->"/utf8>>
            end;

        {des, _, Name, Line_no@1} ->
            <<<<Name/binary, "~"/utf8>>/binary,
                (gleam@string:inspect(Line_no@1))/binary>>;

        {ext, _, Name@1} ->
            <<"e:"/utf8, Name@1/binary>>;

        {no_blame, _} ->
            <<""/utf8>>
    end.

-file("src/vxml/blame.gleam", 109).
-spec comments_digest(blame()) -> binary().
comments_digest(Blame) ->
    <<(gleam@list:index_fold(
            erlang:element(2, Blame),
            <<"["/utf8>>,
            fun(Acc, Comment, I) -> <<<<Acc/binary, (case I > 0 of
                            true ->
                                <<", "/utf8>>;

                            false ->
                                <<""/utf8>>
                        end)/binary>>/binary, Comment/binary>> end
        ))/binary,
        "]"/utf8>>.

-file("src/vxml/blame.gleam", 121).
-spec path_contains(blame(), binary()) -> boolean().
path_contains(Blame, S) ->
    case Blame of
        {src, _, _, _, _, _} ->
            gleam_stdlib:contains_string(erlang:element(3, Blame), S);

        _ ->
            false
    end.

-file("src/vxml/blame.gleam", 136).
-spec normalized_margin(blame_table_margin_columns_min_max()) -> blame_table_margin_columns_min_max().
normalized_margin(Constraints) ->
    case erlang:element(3, Constraints) < erlang:element(2, Constraints) of
        true ->
            {blame_table_margin_columns_min_max,
                erlang:element(3, Constraints),
                erlang:element(3, Constraints)};

        false ->
            Constraints
    end.

-file("src/vxml/blame.gleam", 145).
-spec constrained_width(integer(), blame_table_margin_columns_min_max()) -> integer().
constrained_width(Content_width, Constraints) ->
    Constraints@1 = normalized_margin(Constraints),
    gleam@int:max(
        gleam@int:min(Content_width, erlang:element(3, Constraints@1)),
        erlang:element(2, Constraints@1)
    ).

-file("src/vxml/blame.gleam", 153).
-spec should_render_margin_column(blame_table_margin_columns_min_max()) -> boolean().
should_render_margin_column(Constraints) ->
    Constraints@1 = normalized_margin(Constraints),
    erlang:element(3, Constraints@1) > 0.

-file("src/vxml/blame.gleam", 160).
-spec truncate_with_suffix_or_pad(binary(), integer(), binary()) -> binary().
truncate_with_suffix_or_pad(Content, Desired_length, Truncation_suffix) ->
    L = string:length(Content),
    case L > Desired_length of
        true ->
            <<(gleam@string:drop_end(
                    Content,
                    L - (Desired_length - string:length(Truncation_suffix))
                ))/binary,
                Truncation_suffix/binary>>;

        false ->
            <<Content/binary, (spaces(Desired_length - L))/binary>>
    end.

-file("src/vxml/blame.gleam", 177).
-spec mid_truncation_or_pad(binary(), integer(), binary()) -> binary().
mid_truncation_or_pad(Content, Desired_length, Mid_truncation_dots) ->
    L = string:length(Content),
    case (L + 1) >= Desired_length of
        true ->
            Amt_to_drop = (1 + L) - (Desired_length - string:length(
                Mid_truncation_dots
            )),
            Inner_content = gleam@string:drop_start(Content, 2),
            Slice_start = ((string:length(Inner_content) div 2) - (Amt_to_drop
            div 2))
            - 10,
            Start = gleam@string:slice(Inner_content, 0, Slice_start),
            End = gleam@string:slice(
                Inner_content,
                Slice_start + Amt_to_drop,
                1000
            ),
            <<<<<<<<"│ "/utf8, Start/binary>>/binary,
                        Mid_truncation_dots/binary>>/binary,
                    End/binary>>/binary,
                " "/utf8>>;

        false ->
            <<Content/binary, (spaces(Desired_length - L))/binary>>
    end.

-file("src/vxml/blame.gleam", 198).
-spec glue_columns_3(
    list({binary(), binary(), binary()}),
    blame_table_margin_columns_min_max(),
    blame_table_margin_columns_min_max(),
    binary(),
    binary()
) -> {{integer(), integer()}, list(binary())}.
glue_columns_3(
    Table_lines,
    Blame_digest_margin,
    Comments_margin,
    Mid_truncation_dots,
    Truncation_suffix_col2
) ->
    Render_comments = should_render_margin_column(Comments_margin),
    {Col1_max, Col2_max} = gleam@list:fold(
        Table_lines,
        {0, 0},
        fun(Acc, Tuple) ->
            {gleam@int:max(
                    erlang:element(1, Acc),
                    begin
                        _pipe = erlang:element(1, Tuple),
                        string:length(_pipe)
                    end
                ),
                case Render_comments of
                    true ->
                        gleam@int:max(
                            erlang:element(2, Acc),
                            begin
                                _pipe@1 = erlang:element(2, Tuple),
                                string:length(_pipe@1)
                            end
                        );

                    false ->
                        0
                end}
        end
    ),
    Col1_size = constrained_width(Col1_max, Blame_digest_margin),
    Col2_size = case Render_comments of
        true ->
            constrained_width(Col2_max, Comments_margin);

        false ->
            0
    end,
    Table_lines@1 = gleam@list:map(
        Table_lines,
        fun(Tuple@1) ->
            <<<<(mid_truncation_or_pad(
                        erlang:element(1, Tuple@1),
                        Col1_size,
                        Mid_truncation_dots
                    ))/binary,
                    (case Render_comments of
                        true ->
                            truncate_with_suffix_or_pad(
                                erlang:element(2, Tuple@1),
                                Col2_size,
                                Truncation_suffix_col2
                            );

                        false ->
                            <<""/utf8>>
                    end)/binary>>/binary,
                (erlang:element(3, Tuple@1))/binary>>
        end
    ),
    {{Col1_size, Col2_size}, Table_lines@1}.

-file("src/vxml/blame.gleam", 239).
-spec blamed_strings_annotated_table_header_lines(integer(), integer()) -> list(binary()).
blamed_strings_annotated_table_header_lines(
    Margin_total_width,
    Extra_dashes_for_content
) ->
    [<<"┌"/utf8,
            (gleam@string:repeat(
                <<"─"/utf8>>,
                Margin_total_width + Extra_dashes_for_content
            ))/binary>>,
        <<<<"│ Blame"/utf8,
                (gleam@string:repeat(<<" "/utf8>>, Margin_total_width - 7))/binary>>/binary,
            "█doc"/utf8>>,
        <<"├"/utf8,
            (gleam@string:repeat(
                <<"─"/utf8>>,
                Margin_total_width + Extra_dashes_for_content
            ))/binary>>].

-file("src/vxml/blame.gleam", 250).
-spec blamed_strings_annotated_table_body_lines(
    list({blame(), binary()}),
    binary(),
    blame_table_margin_columns_min_max(),
    blame_table_margin_columns_min_max()
) -> {{integer(), integer()}, list(binary())}.
blamed_strings_annotated_table_body_lines(
    Contents,
    Banner,
    Blame_digest_margin,
    Comments_margin
) ->
    Banner@1 = case Banner =:= <<""/utf8>> of
        true ->
            <<""/utf8>>;

        false ->
            <<<<"("/utf8, Banner/binary>>/binary, ")"/utf8>>
    end,
    {{Cols1, Cols2}, Table_lines} = begin
        _pipe = gleam@list:map(
            Contents,
            fun(C) ->
                {<<<<"│ "/utf8, Banner@1/binary>>/binary,
                        (blame_digest(erlang:element(1, C)))/binary>>,
                    comments_digest(erlang:element(1, C)),
                    <<"█"/utf8, (erlang:element(2, C))/binary>>}
            end
        ),
        glue_columns_3(
            _pipe,
            Blame_digest_margin,
            Comments_margin,
            <<"..."/utf8>>,
            <<"...]"/utf8>>
        )
    end,
    {{Cols1, Cols2}, Table_lines}.

-file("src/vxml/blame.gleam", 270).
-spec blamed_strings_annotated_table_footer_lines(integer(), integer()) -> list(binary()).
blamed_strings_annotated_table_footer_lines(
    Margin_total_width,
    Extra_dashes_for_content
) ->
    [<<"└"/utf8,
            (gleam@string:repeat(
                <<"─"/utf8>>,
                Margin_total_width + Extra_dashes_for_content
            ))/binary>>].

-file("src/vxml/blame.gleam", 279).
-spec blamed_strings_annotated_table(
    list({blame(), binary()}),
    binary(),
    blame_table_margin_columns_min_max(),
    blame_table_margin_columns_min_max()
) -> list(binary()).
blamed_strings_annotated_table(
    Lines,
    Banner,
    Blame_digest_margin,
    Comments_margin
) ->
    {{Cols1, Cols2}, Body_lines} = blamed_strings_annotated_table_body_lines(
        Lines,
        Banner,
        Blame_digest_margin,
        Comments_margin
    ),
    _pipe = [blamed_strings_annotated_table_header_lines(Cols1 + Cols2, 38),
        Body_lines,
        blamed_strings_annotated_table_footer_lines(Cols1 + Cols2, 38)],
    lists:append(_pipe).
