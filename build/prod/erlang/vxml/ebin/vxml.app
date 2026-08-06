{application, vxml, [
    {vsn, "1.0.0"},
    {applications, [gleam_regexp,
                    gleam_stdlib,
                    on,
                    simplifile,
                    splitter,
                    xmlm]},
    {description, "A small XML-like tree datatype and serialization format for document processing."},
    {modules, [vxml,
               vxml@@main,
               vxml@blame,
               vxml@io_lines,
               vxml_html_repair,
               xml_streamer]},
    {registered, []}
]}.
