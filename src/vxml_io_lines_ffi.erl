-module(vxml_io_lines_ffi).
-export([split_leading_spaces/1]).

split_leading_spaces(Source) ->
    split_leading_spaces(Source, 0).

split_leading_spaces(<<$\s, Rest/binary>>, Count) ->
    split_leading_spaces(Rest, Count + 1);
split_leading_spaces(Rest, Count) ->
    {Count, Rest}.
