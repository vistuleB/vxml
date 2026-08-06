-record(input, {
    encoding :: gleam@option:option(xmlm:encoding()),
    strip :: boolean(),
    namespace_callback :: fun((binary()) -> gleam@option:option(binary())),
    entity_callback :: fun((binary()) -> gleam@option:option(binary())),
    uchar :: fun((xmlm:input()) -> {ok, {integer(), xmlm:input()}} |
        {error, xmlm:input_error()}),
    stream :: list(integer()),
    char :: integer(),
    cr :: boolean(),
    line :: integer(),
    column :: integer(),
    limit :: xmlm:limit(),
    peek :: xmlm:signal(),
    stripping :: boolean(),
    last_whitespace :: boolean(),
    scopes :: list({xmlm:name(), list(binary()), boolean()}),
    ns :: gleam@dict:dict(binary(), binary()),
    identifier :: list(integer()),
    data :: list(integer())
}).
