-module(on).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/on.gleam").
-export([ok/2, error/2, eager_error_ok/3, error_ok/3, eager_ok_error/3, ok_error/3, true/2, false/2, eager_false_true/3, false_true/3, eager_true_false/3, true_false/3, some/2, none/2, eager_none_some/3, none_some/3, eager_some_none/3, some_none/3, nonempty/2, empty/2, eager_empty_nonempty/3, empty_nonempty/3, eager_nonempty_empty/3, nonempty_empty/3, eager_empty_eager_singleton_gt1/4, eager_empty_singleton_gt1/4, empty_eager_singleton_gt1/4, empty_singleton_gt1/4, eager_empty_eager_gt1_singleton/4, eager_empty_gt1_singleton/4, empty_eager_gt1_singleton/4, empty_gt1_singleton/4, eager_singleton_eager_gt1_empty/4, eager_singleton_gt1_empty/4, singleton_eager_gt1_empty/4, singleton_gt1_empty/4, stay/2]).
-export_type([return/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type return(DKT, DKU) :: {return, DKT} | {stay, DKU}.

-file("src/on.gleam", 28).
?DOC(
    " Given a Result(a, b) and a callback f(a) -> Result(c, b) applies\n"
    " the callback if the Result has type Ok else maps the Error variant\n"
    " of type Result(a, b) to the Error variant of type Result(c, b).\n"
    " \n"
    " Equivalent to result.try and to error_ok(_, fn(e) -> {Error(e)}, _).\n"
    "\n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use ok_payload <- on.ok(Ok(3))\n"
    " // -> execution proceeds, ok_payload == 3; the current scope must\n"
    " // return a Result(c, b) for some c, b\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    "\n"
    " ```gleam\n"
    " use ok_payload <- on.ok(Error(\"Joe\"))\n"
    " // -> execution discontinues, scope returns Error(\"Joe\")\n"
    " ```\n"
).
-spec ok({ok, DKV} | {error, DKW}, fun((DKV) -> {ok, DKZ} | {error, DKW})) -> {ok,
        DKZ} |
    {error, DKW}.
ok(Result, F2) ->
    case Result of
        {error, E} ->
            {error, E};

        {ok, A} ->
            F2(A)
    end.

-file("src/on.gleam", 59).
?DOC(
    " Given a Result(a, b) and a callback f(b) -> Result(a, c) applies\n"
    " the callback if the Result has type Error else maps the Ok variant\n"
    " of type Result(a, b) to the Ok variant of type Result(c, b).\n"
    "\n"
    " Equivalent to on.ok_error(_, fn(o) -> {Ok(o)}, _).\n"
    "\n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use error_payload <- on.error(Ok(3))\n"
    " // -> execution discontinues, scope returns Ok(3)\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    "\n"
    " ```gleam\n"
    " use error_payload <- on.error(Error(\"Joe\"))\n"
    " // -> execution proceeds, error_payload == \"Joe\";\n"
    " // the scope must return a Result(a, c)\n"
    " ```\n"
).
-spec error({ok, DLE} | {error, DLF}, fun((DLF) -> {ok, DLE} | {error, DLI})) -> {ok,
        DLE} |
    {error, DLI}.
error(Result, F2) ->
    case Result of
        {ok, A} ->
            {ok, A};

        {error, E} ->
            F2(E)
    end.

-file("src/on.gleam", 98).
?DOC(
    " Given a Result(a, b), a value of type c and a callback f(a) -> c,\n"
    " returns the value of type c if the Result is an Error(_), else the\n"
    " evaluation of the second callback at a1 if the Result is Ok(a1).\n"
    "\n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use ok_payload <- on.eager_error_ok(\n"
    "   Ok(3),\n"
    "   on_error: option.None\n"
    " )\n"
    " // -> execution proceeds, ok_payload == 3; the scope must return\n"
    " // an Option(a) value to match the on_error return value\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use ok_payload <- on.eager_error_ok(\n"
    "   Error(\"Joe\"),\n"
    "   on_error: option.None\n"
    " )\n"
    " // -> execution discontinues, scope returns option.None\n"
    " ```\n"
).
-spec eager_error_ok({ok, DLN} | {error, any()}, DLR, fun((DLN) -> DLR)) -> DLR.
eager_error_ok(Result, C, F2) ->
    case Result of
        {error, _} ->
            C;

        {ok, A} ->
            F2(A)
    end.

-file("src/on.gleam", 135).
?DOC(
    " Given a Result(a, b) and callbacks f(b) -> c, f(a) -> c \n"
    " returns the evaluation of the first callback at b1 if the\n"
    " Result is Error(b1) and the evaluation of the second callback\n"
    " at a1 if the Result is Ok(a1).\n"
    "\n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use ok_payload <- on.error_ok(\n"
    "   Ok(3),\n"
    "   on_error: fn(e) { \"hi \" <> e <> \"!\" },\n"
    " )\n"
    " // -> execution proceeds, ok_payload == 3; the scope must return\n"
    " // a String to match the return value of the on_error callback\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use ok_payload <- on.error_ok(\n"
    "   Error(\"Joe\"),\n"
    "   on_error: fn(e) { \"hi \" <> e <> \"!\" },\n"
    " )\n"
    " // -> execution discontinues, scope returns \"hi Joe!\"\n"
    " ```\n"
).
-spec error_ok({ok, DLS} | {error, DLT}, fun((DLT) -> DLW), fun((DLS) -> DLW)) -> DLW.
error_ok(Result, F1, F2) ->
    case Result of
        {error, B} ->
            F1(B);

        {ok, A} ->
            F2(A)
    end.

-file("src/on.gleam", 171).
?DOC(
    " Given a Result(a, b), a value of type c and a callback f(b) -> c,\n"
    " returns the value of type c if the Result is an Ok(_), else the\n"
    " evaluation of the second callback at b1 if the Result is Error(b1).\n"
    "\n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use error_payload <- on.eager_ok_error(\n"
    "   Ok(3),\n"
    "   on_ok: 7,\n"
    " )\n"
    " // -> execution discontinues, scope returns 7\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use error_payload <- on.eager_ok_error(\n"
    "   Error(\"Joe\"),\n"
    "   on_ok: 7,\n"
    " )\n"
    " // -> execution proceeds, error_payload == \"Joe\"; the scope\n"
    " // must return an Int to match the on_ok return value\n"
    " ```\n"
).
-spec eager_ok_error({ok, any()} | {error, DLY}, DMB, fun((DLY) -> DMB)) -> DMB.
eager_ok_error(Result, C, F2) ->
    case Result of
        {ok, _} ->
            C;

        {error, B} ->
            F2(B)
    end.

-file("src/on.gleam", 209).
?DOC(
    " Given a Result(a, b) and callbacks f(b) -> c, f(a) -> c \n"
    " returns the evaluation of the first callback at a1 if the\n"
    " Result is Ok(a1) and the evaluation of the second callback\n"
    " at b1 if the Result is Error(b1).\n"
    "\n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use error_payload <- on.ok_error(\n"
    "   Ok(3),\n"
    "   on_ok: fn(x) { x + 1 },\n"
    " )\n"
    " // -> execution discontinues, scope returns 4\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use error_payload <- on.ok_error(\n"
    "   Error(\"Joe\"),\n"
    "   on_ok: fn(x) { x + 1 },\n"
    " )\n"
    " // -> execution proceeds, error_payload == \"Joe\"; the scope\n"
    " // must return an Int to match the return value of the on_ok\n"
    " // callback\n"
    " ```\n"
).
-spec ok_error({ok, DMC} | {error, DMD}, fun((DMC) -> DMG), fun((DMD) -> DMG)) -> DMG.
ok_error(Result, F1, F2) ->
    case Result of
        {ok, A} ->
            F1(A);

        {error, B} ->
            F2(B)
    end.

-file("src/on.gleam", 243).
?DOC(
    " Given a Bool returns False if the bool is False, else\n"
    " returns a lazily evaluated callback.\n"
    "\n"
    " Equivalent to on.eager_false_true(_, False, _).\n"
    "\n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use <- on.true(True)\n"
    " // -> execution proceeds, scope must return a Bool\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    "\n"
    " ```gleam\n"
    " use <- on.true(False)\n"
    " // -> execution discontinues, scope returns False\n"
    " ```\n"
).
-spec true(boolean(), fun(() -> boolean())) -> boolean().
true(Bool, F2) ->
    case Bool of
        false ->
            false;

        true ->
            F2()
    end.

-file("src/on.gleam", 270).
?DOC(
    " Given a Bool returns True if the bool is True, else\n"
    " returns a lazily evaluated callback.\n"
    "\n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use <- on.false(True)\n"
    " // -> execution discontinues, scope returns True\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    "\n"
    " ```gleam\n"
    " use <- on.false(False)\n"
    " // -> execution proceeds, scope must return a Bool\n"
    " ```\n"
).
-spec false(boolean(), fun(() -> boolean())) -> boolean().
false(Bool, F2) ->
    case Bool of
        true ->
            true;

        false ->
            F2()
    end.

-file("src/on.gleam", 309).
?DOC(
    " Given a Bool, a value of type c and a callback f() -> c,\n"
    " returns the value of type c if the bool is False, else the\n"
    " evaluation of the callback.\n"
    "\n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use <- on.eager_false_true(\n"
    "   True,\n"
    "   on_false: \"Joe\",\n"
    " )\n"
    " // -> execution proceeds, the scope must return a String to \n"
    " // match the return value of the on_false argument\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    "\n"
    " ```gleam\n"
    " use <- on.eager_false_true(\n"
    "   False,\n"
    "   on_false: \"Joe\",\n"
    " )\n"
    " // -> execution discontinues, scope returns \"Joe\"\n"
    " ```\n"
).
-spec eager_false_true(boolean(), DMH, fun(() -> DMH)) -> DMH.
eager_false_true(Bool, C, F2) ->
    case Bool of
        false ->
            C;

        true ->
            F2()
    end.

-file("src/on.gleam", 345).
?DOC(
    " Given a Bool and two callbacks f() -> c returns the\n"
    " evaluation of the first callback if the bool is False,\n"
    " else the second callback.\n"
    "\n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use <- on.false_true(\n"
    "   True,\n"
    "   on_false: fn() { \"Joe\" },\n"
    " )\n"
    " // -> execution proceeds, the scope must return a String \n"
    " // to match the return value of the on_false argument\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    "\n"
    " ```gleam\n"
    " use <- on.false_true(\n"
    "   False,\n"
    "   on_false: fn() { \"Joe\" },\n"
    " )\n"
    " // -> execution discontinues, scope returns \"Joe\"\n"
    " ```\n"
).
-spec false_true(boolean(), fun(() -> DMI), fun(() -> DMI)) -> DMI.
false_true(Bool, F1, F2) ->
    case Bool of
        false ->
            F1();

        true ->
            F2()
    end.

-file("src/on.gleam", 381).
?DOC(
    " Given a Bool, a value of type c and a callback f() -> c, \n"
    " returns the value of type c if the bool is True, else the\n"
    " evaluation of the callback.\n"
    "\n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use <- on.eager_true_false(\n"
    "   True,\n"
    "   on_true: \"Joe\",\n"
    " )\n"
    " // -> execution discontinues, scope returns \"Joe\"\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use <- on.eager_true_false(\n"
    "   False,\n"
    "   on_true: \"Joe\",\n"
    " )\n"
    " // -> execution proceeds, the scope must return a String to \n"
    " // match the return value of the on_true argument\n"
    " ```\n"
).
-spec eager_true_false(boolean(), DMJ, fun(() -> DMJ)) -> DMJ.
eager_true_false(Bool, C, F2) ->
    case Bool of
        true ->
            C;

        false ->
            F2()
    end.

-file("src/on.gleam", 418).
?DOC(
    " Given a Bool and two callbacks f() -> c returns the\n"
    " evaluation of the first callback if the bool is True,\n"
    " else the second callback.\n"
    "\n"
    "\n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use <- on.true_false(\n"
    "   True,\n"
    "   on_true: fn() { \"Joe\" },\n"
    " )\n"
    " // -> execution discontinues, scope returns \"Joe\"\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    "\n"
    " ```gleam\n"
    " use <- on.true_false(\n"
    "   False,\n"
    "   on_true: fn() { \"Joe\" },\n"
    " )\n"
    " // -> execution proceeds, the scope must return a String to \n"
    " // match the return value of the on_false argument\n"
    " ```\n"
).
-spec true_false(boolean(), fun(() -> DMK), fun(() -> DMK)) -> DMK.
true_false(Bool, F1, F2) ->
    case Bool of
        true ->
            F1();

        false ->
            F2()
    end.

-file("src/on.gleam", 456).
?DOC(
    " Given an Option(a) and a callback f(a) -> Option(c)\n"
    " applies the callback to the payload a if the option is Some(a),\n"
    " else returns None.\n"
    "\n"
    " Equivalent to:\n"
    " - on.eager_none_some(_, None, _).\n"
    " - option.then\n"
    " \n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use payload <- on.some(Some(3))\n"
    " // -> execution proceeds, payload == 3; the scope must return\n"
    " // an Option(c)\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use payload <- on.some(None)\n"
    " // -> execution discontinues, scope returns None\n"
    " ```\n"
).
-spec some(gleam@option:option(DML), fun((DML) -> gleam@option:option(DMN))) -> gleam@option:option(DMN).
some(Option, F2) ->
    case Option of
        none ->
            none;

        {some, A} ->
            F2(A)
    end.

-file("src/on.gleam", 484).
?DOC(
    " Given an Option(a) and a callback f() -> Option(a), applies\n"
    " the callback if the option is None, else returns the option\n"
    " unchanged.\n"
    "\n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use <- on.none(Some(3))\n"
    " // -> execution discontinues, scope returns Some(3)\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    "\n"
    " ```gleam\n"
    " use <- on.none(None)\n"
    " // -> execution proceeds, the scope must return an Option(a)\n"
    " ```\n"
).
-spec none(gleam@option:option(DMQ), fun(() -> gleam@option:option(DMQ))) -> gleam@option:option(DMQ).
none(Option, F2) ->
    case Option of
        {some, A} ->
            {some, A};

        none ->
            F2()
    end.

-file("src/on.gleam", 521).
?DOC(
    " Given an Option(a), a value of type c and a callback f(a) -> c, \n"
    " returns the value of type c if the option is None, else\n"
    " applies the callback to the option's payload.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " use payload <- on.eager_none_some(\n"
    "   Some(3),\n"
    "   on_none: \"Bob\",\n"
    " )\n"
    " // -> execution proceeds, payload == 3; the scope must return a\n"
    " // String to match the return value of the on_none return value\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " use payload <- on.eager_none_some(\n"
    "   None,\n"
    "   on_none: \"Bob\",\n"
    " )\n"
    " // -> execution discontinues, scope returns \"Bob\"\n"
    " ```\n"
).
-spec eager_none_some(gleam@option:option(DMU), DMW, fun((DMU) -> DMW)) -> DMW.
eager_none_some(Option, C, F2) ->
    case Option of
        none ->
            C;

        {some, A} ->
            F2(A)
    end.

-file("src/on.gleam", 559).
?DOC(
    " Given an Option(a), a callback f() -> c and a callback\n"
    " f(a) -> c evaluates the first callback if the option is\n"
    " None, else evaluates the second callback on the option's\n"
    " payload.\n"
    "\n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use payload <- on.none_some(\n"
    "   Some(3),\n"
    "   on_none: fn() { \"Bob\" },\n"
    " )\n"
    " // -> execution proceeds, payload == 3; the scope must return\n"
    " // a String to match the return value of the on_none return\n"
    " // value\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use payload <- on.none_some(\n"
    "   None,\n"
    "   on_none: fn() { \"Bob\" },\n"
    " )\n"
    " // -> execution discontinues, scope returns \"Bob\"\n"
    " ```\n"
).
-spec none_some(gleam@option:option(DMX), fun(() -> DMZ), fun((DMX) -> DMZ)) -> DMZ.
none_some(Option, F1, F2) ->
    case Option of
        none ->
            F1();

        {some, A} ->
            F2(A)
    end.

-file("src/on.gleam", 596).
?DOC(
    " Given an Option(a), a value of type c and a callback\n"
    " f() -> c, returns the value if the Option is Some(x),\n"
    " else evaluates the second callback if the option is\n"
    " None.\n"
    "\n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use <- on.eager_some_none(\n"
    "   Some(3),\n"
    "   on_some: \"q\",\n"
    " )\n"
    " // -> execution discontinues, scope returns \"q\"\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    "\n"
    " ```gleam\n"
    " use <- on.eager_some_none(\n"
    "   None,\n"
    "   on_some: \"q\",\n"
    " )\n"
    " // -> execution proceeds, the scope must return a String to match\n"
    " // the on_some return value\n"
    " ```\n"
).
-spec eager_some_none(gleam@option:option(any()), DNC, fun(() -> DNC)) -> DNC.
eager_some_none(Option, C, F2) ->
    case Option of
        {some, _} ->
            C;

        none ->
            F2()
    end.

-file("src/on.gleam", 633).
?DOC(
    " Given an Option(a), a callback f(a) -> c and a callback\n"
    " f() -> c, evaluates the first callback on the option's\n"
    " payload, else evaluates the second callback if the option\n"
    " is None.\n"
    "\n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " use <- on.some_none(\n"
    "   Some(3),\n"
    "   on_some: fn(x) { x + 1 },\n"
    " )\n"
    " // -> execution discontinues, scope returns 4\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    "\n"
    " ```gleam\n"
    " use <- on.some_none(\n"
    "   None,\n"
    "   on_some: fn(x) { x + 1 },\n"
    " )\n"
    " // -> execution proceeds, the scope must return an Int to match\n"
    " // the return value of the on_some return value\n"
    " ```\n"
).
-spec some_none(gleam@option:option(DND), fun((DND) -> DNF), fun(() -> DNF)) -> DNF.
some_none(Option, F1, F2) ->
    case Option of
        {some, A} ->
            F1(A);

        none ->
            F2()
    end.

-file("src/on.gleam", 669).
?DOC(
    " Given a List(a) and a callback\n"
    " f(a, List(a)) -> List(c), returns the empty list if the\n"
    " list is empty, else evaluates the callback on (first, rest)\n"
    " where 'first' is the first element and 'tail' is the tail of \n"
    " the list.\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use first, rest <- on.nonempty([1, 4, 7])\n"
    " // -> execution proceeds, first == 1, rest == [1, 7];\n"
    " // scope must return a List(c)\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use first, rest <- on.nonempty([])\n"
    " // -> execution discontinues, scope returns []\n"
    " ```\n"
).
-spec nonempty(list(DNG), fun((DNG, list(DNG)) -> list(DNJ))) -> list(DNJ).
nonempty(List, F2) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            F2(First, Rest)
    end.

-file("src/on.gleam", 697).
?DOC(
    " Given a List(a) and a callback f() -> List(a)\n"
    " returns the list if the is nonempty, else evaluates\n"
    " the callback.\n"
    "\n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use <- on.empty([1, 4, 7])\n"
    " // -> execution discontinues, scope returns [1, 4, 7]\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use <- on.empty([])\n"
    " // -> execution proceeds, the scope must return a List(c)\n"
    " ```\n"
).
-spec empty(list(DNM), fun(() -> list(DNM))) -> list(DNM).
empty(List, F2) ->
    case List of
        [_ | _] ->
            List;

        [] ->
            F2()
    end.

-file("src/on.gleam", 738).
?DOC(
    " Given a List(a), a value of type c, and callback\n"
    " f(a, List(a)) -> c, returns either the value of type c\n"
    " if the list is empty or else applies the callback to \n"
    " the head: a and tail: List(a) of the list.\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use first, rest <- on.eager_empty_nonempty(\n"
    "   [1, 4, 7],\n"
    "   on_empty: \"Joe\",\n"
    " )\n"
    " // -> execution proceeds, first == 1, rest == [4, 7];\n"
    " // scope must return a String to match the empty list\n"
    " // return value\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use first, rest <- on.eager_empty_nonempty(\n"
    "   [],\n"
    "   on_empty: \"Joe\",\n"
    " )\n"
    " // -> execution discontinues, scope returns \"Joe\"\n"
    " ```\n"
).
-spec eager_empty_nonempty(list(DNQ), DNS, fun((DNQ, list(DNQ)) -> DNS)) -> DNS.
eager_empty_nonempty(List, C, F2) ->
    case List of
        [] ->
            C;

        [First | Rest] ->
            F2(First, Rest)
    end.

-file("src/on.gleam", 776).
?DOC(
    " Given a List(a), a callback fn() -> c and a callback\n"
    " f(a, List(a)) -> c, evaluates the first callback if the\n"
    " list is empty else applies the second callback to the\n"
    " head: a and tail: List(a) of the list.\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use first, rest <- on.empty_nonempty(\n"
    "   [1, 4, 7],\n"
    "   on_empty: fn() { \"Joe\" },\n"
    " )\n"
    " // -> execution proceeds, first == 1, rest == [4, 7];\n"
    " // scope must return a String to match the empty list\n"
    " // callback return value\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use first, rest <- on.empty_nonempty(\n"
    "   [],\n"
    "   on_empty: fn() { \"Joe\" },\n"
    " )\n"
    " // -> execution discontinues, scope returns \"Joe\"\n"
    " ```\n"
).
-spec empty_nonempty(list(DNU), fun(() -> DNW), fun((DNU, list(DNU)) -> DNW)) -> DNW.
empty_nonempty(List, F1, F2) ->
    case List of
        [] ->
            F1();

        [First | Rest] ->
            F2(First, Rest)
    end.

-file("src/on.gleam", 813).
?DOC(
    " Given a List(a), a value of type c, and\n"
    " a callback f() -> c, returns either the value of type\n"
    " c if the list is nonempty, and otherwise evaluates the\n"
    " second callback, if the list is empty. \n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use <- on.eager_nonempty_empty(\n"
    "   [1, 4, 7],\n"
    "   \"z\",\n"
    " )\n"
    " // -> execution discontinues, scope returns \"z\"\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use <- on.eager_nonempty_empty(\n"
    "   [],\n"
    "   \"z\",\n"
    " )\n"
    " // -> execution proceeds, the scope must return a\n"
    " // String to match the eager nonempty return value\n"
    " ```\n"
).
-spec eager_nonempty_empty(list(any()), DOA, fun(() -> DOA)) -> DOA.
eager_nonempty_empty(List, C, F2) ->
    case List of
        [_ | _] ->
            C;

        [] ->
            F2()
    end.

-file("src/on.gleam", 852).
?DOC(
    " Given a List(a), a callback  f(a, List(a)) -> c, and\n"
    " a callback f() -> c, returns either the first callback\n"
    " evaluated on the first element and tail of the list if\n"
    " the list is nonempty, and otherwise evaluates the \n"
    " second callback, if the list is empty. \n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use <- on.nonempty_empty(\n"
    "   [1, 4, 7],\n"
    "   fn(first, _rest) { first + 1 },\n"
    " )\n"
    " // -> execution discontinues, scope returns 2\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use <- on.nonempty_empty(\n"
    "   [],\n"
    "   fn(first, _rest) { first + 1 },\n"
    " )\n"
    " // -> execution proceeds, the scope must return an\n"
    " // integer to match the return value of the first\n"
    " // callback\n"
    " ```\n"
).
-spec nonempty_empty(list(DOB), fun((DOB, list(DOB)) -> DOE), fun(() -> DOE)) -> DOE.
nonempty_empty(List, F1, F2) ->
    case List of
        [First | Rest] ->
            F1(First, Rest);

        [] ->
            F2()
    end.

-file("src/on.gleam", 897).
?DOC(
    " Given a List(a), two values c1 and c2of type c, and a callback\n"
    " f(a, a, List(a)) -> c, returns:\n"
    "\n"
    " - the c1 if the list is empty\n"
    " - the c2 if the list has one element\n"
    " - the callback evaluated with arguments a1, a2, rest if the list has the form [a1, a2, ..rest]\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use first, second, ..rest <- on.eager_empty_eager_singleton_gt1(\n"
    "   [1, 4, 7],\n"
    "   on_empty: 0,\n"
    "   on_singleton: 300,\n"
    " )\n"
    " // -> execution proceeds, first == 1, second == 4, rest == [7];\n"
    " // scope must return an Int to match the on_empty, on_singleton callbacks\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use first, second, ..rest <- on.eager_empty_eager_singleton_gt1(\n"
    "   [4],\n"
    "   on_empty: 0,\n"
    "   on_singleton: 300,\n"
    " )\n"
    " // -> execution discontinues, scope resturns 300\n"
    " ```\n"
).
-spec eager_empty_eager_singleton_gt1(
    list(DOF),
    DOH,
    DOH,
    fun((DOF, DOF, list(DOF)) -> DOH)
) -> DOH.
eager_empty_eager_singleton_gt1(List, C1, C2, F3) ->
    case List of
        [] ->
            C1;

        [_] ->
            C2;

        [First, Second | Rest] ->
            F3(First, Second, Rest)
    end.

-file("src/on.gleam", 940).
?DOC(
    " Given a List(a), a value of type c, and callbacks\n"
    " f(a) -> c, f(a, a, List(a)) -> c, returns:\n"
    "\n"
    " - the value of type c if the list is empty\n"
    " - the first callback evaluated with argument a1 if the list as the form [a1]\n"
    " - the second callback evaluated with arguments a1, a2, rest if the list has the form [a1, a2, ..rest]\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use first, second, ..rest <- on.eager_empty_singleton_gt1(\n"
    "   [1, 4, 7],\n"
    "   on_empty: 0,\n"
    "   on_singleton: fn(first) { first },\n"
    " )\n"
    " // -> execution proceeds, first == 1, second == 4, rest == [7];\n"
    " // scope must return an Int to match the on_empty, on_singleton callbacks\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use first, second, ..rest <- on.eager_empty_singleton_gt1(\n"
    "   [4],\n"
    "   on_empty: 0,\n"
    "   on_singleton: fn(first) { first },\n"
    " )\n"
    " // -> execution discontinues, scope resturns 4\n"
    " ```\n"
).
-spec eager_empty_singleton_gt1(
    list(DOJ),
    DOL,
    fun((DOJ) -> DOL),
    fun((DOJ, DOJ, list(DOJ)) -> DOL)
) -> DOL.
eager_empty_singleton_gt1(List, C, F2, F3) ->
    case List of
        [] ->
            C;

        [First] ->
            F2(First);

        [First@1, Second | Rest] ->
            F3(First@1, Second, Rest)
    end.

-file("src/on.gleam", 983).
?DOC(
    " Given a List(a), a value c1 of type c and a callback\n"
    " f(a, a, List(a)) -> c, returns:\n"
    "\n"
    " - the evaluation of the first callback if the list is empty\n"
    " - c1 if the list has one element\n"
    " - the third callback evaluated with arguments a1, a2, rest if the list has the form [a1, a2, ..rest]\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use first, second, ..rest <- on.empty_eager_singleton_gt1(\n"
    "   [1, 4, 7],\n"
    "   on_empty: fn() { 200 },\n"
    "   on_singleton: 300,\n"
    " )\n"
    " // -> execution proceeds, first == 1, second == 4, rest == [7];\n"
    " // scope must return an Int to match the on_empty, on_singleton callbacks\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use first, second, ..rest <- on.empty_singleton_gt1(\n"
    "   [4],\n"
    "   on_empty: fn() { 200 },\n"
    "   on_singleton: 300,\n"
    " )\n"
    " // -> execution discontinues, scope resturns 300\n"
    " ```\n"
).
-spec empty_eager_singleton_gt1(
    list(DON),
    fun(() -> DOP),
    DOP,
    fun((DON, DON, list(DON)) -> DOP)
) -> DOP.
empty_eager_singleton_gt1(List, F1, C, F3) ->
    case List of
        [] ->
            F1();

        [_] ->
            C;

        [First, Second | Rest] ->
            F3(First, Second, Rest)
    end.

-file("src/on.gleam", 1026).
?DOC(
    " Given a List(a), and callbacks f() -> c, f(a) -> c,\n"
    " f(a, a, List(a)) -> c, returns:\n"
    "\n"
    " - the evaluation of the first callback if the list is empty\n"
    " - the second callback evaluated with argument a1 if the list as the form [a1]\n"
    " - the third callback evaluated with arguments a1, a2, rest if the list has the form [a1, a2, ..rest]\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use first, second, ..rest <- on.empty_singleton_gt1(\n"
    "   [1, 4, 7],\n"
    "   on_empty: 0,\n"
    "   on_singleton: fn(first) { first },\n"
    " )\n"
    " // -> execution proceeds, first == 1, second == 4, rest == [7];\n"
    " // scope must return an Int to match the on_empty, on_singleton callbacks\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use first, second, ..rest <- on.empty_singleton_gt1(\n"
    "   [4],\n"
    "   on_empty: 0,\n"
    "   on_singleton: fn(first) { first },\n"
    " )\n"
    " // -> execution discontinues, scope resturns 4\n"
    " ```\n"
).
-spec empty_singleton_gt1(
    list(DOR),
    fun(() -> DOT),
    fun((DOR) -> DOT),
    fun((DOR, DOR, list(DOR)) -> DOT)
) -> DOT.
empty_singleton_gt1(List, F1, F2, F3) ->
    case List of
        [] ->
            F1();

        [First] ->
            F2(First);

        [First@1, Second | Rest] ->
            F3(First@1, Second, Rest)
    end.

-file("src/on.gleam", 1070).
?DOC(
    " Given a List(a), values c1 and c2 of type c, and a callback\n"
    " f(a) -> c, returns:\n"
    "\n"
    " - c1 if the list is empty\n"
    " - c2 if the list as more than one element\n"
    " - the second callback evaluated with argument a1 if the list as the form [a1]\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use first <- on.eager_empty_eager_gt1_singleton(\n"
    "   [1, 4, 7],\n"
    "   on_empty: Error(\"empty list\"),\n"
    "   on_gt1: Error(\"> 1 element in list\"),\n"
    " )\n"
    " // -> execution discontinues, scope returns Error(\"> 1 element in list\")\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use first <- on.eager_empty_eager_gt1_singleton(\n"
    "   [4],\n"
    "   on_empty: Error(\"empty list\"),\n"
    "   on_gt1: Error(\"> 1 element in list\"),\n"
    " )\n"
    " // -> execution proceeds, first == 4;\n"
    " // scope must return a Result(c, String) to match the on_empty \n"
    " // and on_gt1 values\n"
    " ```\n"
).
-spec eager_empty_eager_gt1_singleton(list(DOV), DOX, DOX, fun((DOV) -> DOX)) -> DOX.
eager_empty_eager_gt1_singleton(List, C1, C2, F3) ->
    case List of
        [] ->
            C1;

        [_, _ | _] ->
            C2;

        [First] ->
            F3(First)
    end.

-file("src/on.gleam", 1113).
?DOC(
    " Given a List(a), a value of type c, and callbacks\n"
    " f(a, a, List(a)) -> c, f(a) -> c, returns:\n"
    "\n"
    " - the value of type c if the list is empty\n"
    " - the first callback evaluated with arguments a1, a2, rest if the list has the form [a1, a2, ..rest]\n"
    " - the second callback evaluated with argument a1 if the list as the form [a1]\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use first <- on.eager_empty_gt1_singleton(\n"
    "   [1, 4, 7],\n"
    "   on_empty: Error(\"empty list\"),\n"
    "   on_gt1: fn(_, _, rest) { Error(string.inspect(2 + list.length(rest) <> \" > 1 elements in list\") },\n"
    " )\n"
    " // -> execution discontinues, scope returns Error(\"3 > 1 elements in list\")\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use first <- on.eager_empty_gt1_singleton(\n"
    "   [4],\n"
    "   on_empty: Error(\"empty list\"),\n"
    "   on_gt1: fn(_, _, rest) { Error(string.inspect(2 + list.length(rest) <> \" > 1 elements in list\") },\n"
    " )\n"
    " // -> execution discontinues, first == 4\n"
    " // scope must return a Result(c, String)\n"
    " ```\n"
).
-spec eager_empty_gt1_singleton(
    list(DOY),
    DPA,
    fun((DOY, DOY, list(DOY)) -> DPA),
    fun((DOY) -> DPA)
) -> DPA.
eager_empty_gt1_singleton(List, C, F2, F3) ->
    case List of
        [] ->
            C;

        [First, Second | Rest] ->
            F2(First, Second, Rest);

        [First@1] ->
            F3(First@1)
    end.

-file("src/on.gleam", 1156).
?DOC(
    " Given a List(a), a callback f() -> c, a value c1 of type c\n"
    " and a second callback f(a, a, List(a)) -> c, f(a) -> c, returns:\n"
    "\n"
    " - the evaluation of the first callback if the list is empty\n"
    " - c1 if the list has more than one element\n"
    " - the third callback evaluated with argument a1 if the list as the form [a1]\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use first <- on.empty_eager_gt1_singleton(\n"
    "   [1, 4, 7],\n"
    "   on_empty: fn() { Error(\"empty list\") },\n"
    "   on_gt1: Error(\"> 1 elements in list\"),\n"
    " )\n"
    " // -> execution discontinues, scope returns Error(\"> 1 elements in list\")\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use first <- on.empty_gt1_singleton(\n"
    "   [4],\n"
    "   on_empty: fn() { Error(\"empty list\") },\n"
    "   on_gt1: Error(\"> 1 elements in list\"),\n"
    " )\n"
    " // -> execution proceeds, first == 4;\n"
    " // scope must return a Result(c, String)\n"
    " ```\n"
).
-spec empty_eager_gt1_singleton(
    list(DPC),
    fun(() -> DPE),
    DPE,
    fun((DPC) -> DPE)
) -> DPE.
empty_eager_gt1_singleton(List, F1, C, F3) ->
    case List of
        [] ->
            F1();

        [_, _ | _] ->
            C;

        [First] ->
            F3(First)
    end.

-file("src/on.gleam", 1200).
?DOC(
    " Given a List(a) and callbacks f() -> c,\n"
    " f(a, a, List(a)) -> c, f(a) -> c, returns:\n"
    "\n"
    " - the evaluation of the first callback if the list is empty\n"
    " - the second callback evaluated with arguments a1, a2, rest if the list has the form [a1, a2, ..rest]\n"
    " - the third callback evaluated with argument a1 if the list as the form [a1]\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use first <- on.empty_gt1_singleton(\n"
    "   [1, 4, 7],\n"
    "   on_empty: fn() { Error(\"empty list\") },\n"
    "   on_gt1: fn(_, _, rest) {Error(string.inspect(2 + list.length(rest)) <> \" > 1 elements in list\")},\n"
    " )\n"
    " // -> execution discontinues, scope returns Error(\"3 > 1 elements in list\")\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use first <- on.empty_gt1_singleton(\n"
    "   [4],\n"
    "   on_empty: fn() { Error(\"empty list\") },\n"
    "   on_gt1: fn(_, _, rest) {Error(string.inspect(2 + list.length(rest)) <> \" > 1 elements in list\")},\n"
    " )\n"
    " // -> execution proceeds, first == 4;\n"
    " // scope must return a Result(c, String) to match the on_empty \n"
    " // and on_gt1 callbacks\n"
    " ```\n"
).
-spec empty_gt1_singleton(
    list(DPF),
    fun(() -> DPH),
    fun((DPF, DPF, list(DPF)) -> DPH),
    fun((DPF) -> DPH)
) -> DPH.
empty_gt1_singleton(List, F1, F2, F3) ->
    case List of
        [] ->
            F1();

        [First, Second | Rest] ->
            F2(First, Second, Rest);

        [First@1] ->
            F3(First@1)
    end.

-file("src/on.gleam", 1244).
?DOC(
    " Given a List(a), values c1, c2 of type c and a callback\n"
    " f() -> c, returns:\n"
    "\n"
    " - c1 if the list has one element\n"
    " - c2 if the list has more than one element\n"
    " - the evaluation of the third callback if the list is empty\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use <- on.eager_singleton_eager_gt1_empty(\n"
    "   [1, 4, 7],\n"
    "   on_singleton: 55,\n"
    "   on_gt1: 66,\n"
    " )\n"
    " // -> execution discontinues, scope returns 66\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use <- on.eager_singleton_eager_gt1_empty(\n"
    "   [],\n"
    "   on_singleton: 55,\n"
    "   on_gt1: 66,\n"
    " )\n"
    " // -> execution proceeds, scope must return an Int to \n"
    " // match the on_singleton, on_gt1 callbacks\n"
    " \n"
    " ```\n"
).
-spec eager_singleton_eager_gt1_empty(list(any()), DPL, DPL, fun(() -> DPL)) -> DPL.
eager_singleton_eager_gt1_empty(List, C1, C2, F3) ->
    case List of
        [_] ->
            C1;

        [_, _ | _] ->
            C2;

        [] ->
            F3()
    end.

-file("src/on.gleam", 1287).
?DOC(
    " Given a List(a), a value of type c and callbacks fn(a, a, List(a)) -> c,\n"
    " fn() -> c, returns:\n"
    "\n"
    " - the value of type c if the list has one elemnt\n"
    " - the second callback evaluated with arguments a1, a2, rest if the list has the form [a1, a2, ..rest]\n"
    " - the evaluation of the third callback if the list is empty\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use <- on.eager_singleton_gt1_empty(\n"
    "   [4],\n"
    "   on_singleton: 23,\n"
    "   on_gt1: fn(first, second, ..rest) { first + second},\n"
    " )\n"
    " // -> execution discontinues, scope returns 23\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use <- on.eager_singleton_gt1_empty(\n"
    "   [],\n"
    "   on_singleton: 23,\n"
    "   on_gt1: fn(first, second, ..rest) { first + second},\n"
    " )\n"
    " // -> execution proceeds, scope must return an Int\n"
    " \n"
    " ```\n"
).
-spec eager_singleton_gt1_empty(
    list(DPM),
    DPO,
    fun((DPM, DPM, list(DPM)) -> DPO),
    fun(() -> DPO)
) -> DPO.
eager_singleton_gt1_empty(List, C, F2, F3) ->
    case List of
        [_] ->
            C;

        [First, Second | Rest] ->
            F2(First, Second, Rest);

        [] ->
            F3()
    end.

-file("src/on.gleam", 1330).
?DOC(
    " Given a List(a) and a callback f(a) -> c,\n"
    " f(a, a, List(a)) -> c, f() -> c, returns:\n"
    "\n"
    " - the first callback evaluated with argument a1 if the list as the form [a1]\n"
    " - the second callback evaluated with arguments a1, a2, rest if the list has the form [a1, a2, ..rest]\n"
    " - the evaluation of the third callback if the list is empty\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use <- on.singleton_eager_gt1_empty(\n"
    "   [2, 4, 7],\n"
    "   on_singleton: fn(x) { x + 1 },\n"
    "   on_gt1: 66,\n"
    " )\n"
    " // -> execution discontinues, scope returns 66\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use <- on.singleton_eager_gt1_empty(\n"
    "   [],\n"
    "   on_singleton: fn(x) { x + 1 },\n"
    "   on_gt1: 66,\n"
    " )\n"
    " // -> execution proceeds, scope must return an Int\n"
    " \n"
    " ```\n"
).
-spec singleton_eager_gt1_empty(
    list(DPQ),
    fun((DPQ) -> DPS),
    DPS,
    fun(() -> DPS)
) -> DPS.
singleton_eager_gt1_empty(List, F1, C, F3) ->
    case List of
        [First] ->
            F1(First);

        [_, _ | _] ->
            C;

        [] ->
            F3()
    end.

-file("src/on.gleam", 1374).
?DOC(
    " Given a List(a) and callbacks f(a) -> c,\n"
    " f(a, a, List(a)) -> c, f() -> c, returns:\n"
    "\n"
    " - the first callback evaluated with argument a1 if the list as the form [a1]\n"
    " - the second callback evaluated with arguments a1, a2, rest if the list has the form [a1, a2, ..rest]\n"
    " - the evaluation of the third callback if the list is empty\n"
    " \n"
    " ### Example 1\n"
    " \n"
    " ```gleam\n"
    " use <- on.singleton_gt1_empty(\n"
    "   [2, 4, 7],\n"
    "   on_singleton: fn(x) { x + 1 },\n"
    "   on_gt1: fn(first, second, ..rest) { first + second},\n"
    " )\n"
    " // -> execution discontinues, scope returns 6 (= 2 + 4)\n"
    " ```\n"
    " \n"
    " ### Example 2\n"
    " \n"
    " ```gleam\n"
    " use <- on.singleton_gt1_empty(\n"
    "   [],\n"
    "   on_singleton: fn(x) { x + 1 },\n"
    "   on_gt1: fn(first, second, ..rest) { first + second},\n"
    " )\n"
    " // -> execution proceeds, scope must return an Int to \n"
    " // match the on_singleton, on_gt1 callbacks\n"
    " \n"
    " ```\n"
).
-spec singleton_gt1_empty(
    list(DPT),
    fun((DPT) -> DPV),
    fun((DPT, DPT, list(DPT)) -> DPV),
    fun(() -> DPV)
) -> DPV.
singleton_gt1_empty(List, F1, F2, F3) ->
    case List of
        [First] ->
            F1(First);

        [First@1, Second | Rest] ->
            F2(First@1, Second, Rest);

        [] ->
            F3()
    end.

-file("src/on.gleam", 1424).
?DOC(
    " Given a value of type Return(a, b) and a callback f(b) -> a, returns\n"
    " f(b1) if the value has the form Stay(b1) and returns a1\n"
    " if the value has the form Return(a1).\n"
    " \n"
    " ### Example 1\n"
    "\n"
    " ```gleam\n"
    " let #(string1, string2) = #(\"bob\", \"\")\n"
    " use _ <- on.stay(case string2 {\n"
    "   \"\" -> on.Return(#(string1, string1))\n"
    "   _ -> on.Stay(Nil)\n"
    " })\n"
    " // -> execution discontinues, scope returns #(\"bob\", \"bob\")\n"
    " ```\n"
    "\n"
    " ### Example 2\n"
    "\n"
    " ```gleam\n"
    " let #(string1, string2) = #(\"bob\", \"alice\")\n"
    " use _ <- on.stay(case string2 {\n"
    "   \"\" -> on.Return(#(string1, string1))\n"
    "   _ -> on.Stay(Nil)\n"
    " })\n"
    " // -> execution proceeds; the current scope must return a #(String, String)\n"
    " ```\n"
).
-spec stay(return(DPX, DPY), fun((DPY) -> DPX)) -> DPX.
stay(R, F) ->
    case R of
        {return, A} ->
            A;

        {stay, B} ->
            F(B)
    end.
