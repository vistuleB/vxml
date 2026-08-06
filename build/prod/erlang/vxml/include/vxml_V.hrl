-record(v, {
    blame :: vxml@blame:blame(),
    tag :: binary(),
    attrs :: list(vxml:attr()),
    children :: list(vxml:v_x_m_l())
}).
