-record(src, {
    comments :: list(binary()),
    path :: binary(),
    line_no :: integer(),
    char_no :: integer(),
    cursor :: vxml@blame:source_cursor()
}).
