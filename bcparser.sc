using import String
using import UTF-8

import .utils
using import .program

inline string-slice (self start end)
    """"Generator that sequentially iterates on a string from `start` to `end` (optional), returning
        the index and character as loop variables.
    Generator
        inline () (start as usize)
        inline (i)
            static-if (not (none? end))
                i < end
            else
                i < self._count
        inline (i)
            _ i (self @ i)
        inline (i) (i + 1:usize)

inline whitespace? (c)
    switch c
    pass (char " ")
    pass (char "\t")
    pass (char "\n")
    do
        true
    default
        false

inline letter? (c)
    or
        and
            c >= (char "A")
            c <= (char "Z")
        and
            c >= (char "a")
            c <= (char "z")

inline digit? (c)
    and
        c >= (char "0")
        c <= (char "9")

inline alphanum? (c)
    or
        letter? c
        digit? c

inline err-malformed ()
    hide-traceback;
    error "malformed input file"

fn consume-leading-whitespace (stream initpos)
    initpos as:= usize
    for idx c in (string-slice stream initpos)
        if (c == (char "\n"))
            error "expected something before endl"
        if (not (whitespace? c))
            return idx
    error "expected something before EOF"

fn consume-trailing-whitespace (stream initpos)
    initpos as:= usize
    for idx c in (string-slice stream initpos)
        if (c == (char "\n"))
            return (idx + 1)
        if (not (whitespace? c))
            err-malformed;
    countof stream

fn consume-whitespace (stream initpos)
    initpos as:= usize
    loop (idx = initpos)
        c := stream @ idx
        if (whitespace? c)
            repeat (idx + 1)
        # we necessarily stop at the zero terminator at the most, in that case
        # we want to return its position instead of the next (invalid) index.
        break (min idx (countof stream))

fn parse-arg-int (stream initpos)
    let initpos = (consume-leading-whitespace stream initpos)
    if (not (digit? (stream @ initpos)))
        err-malformed;
    _
        fold (value = 0) for idx c in (string-slice stream initpos)
            if (not (digit? c))
                return value idx
            (value * 10) + (c - (char "0"))
        # if we reach the end of the string
        countof stream

spice append-instruction-with-args (program name stream argstart)
    let sw = (sc_switch_new `(hash name))

    va-map
        inline (ft)
            let makeop-call = (sc_call_new `ft)
            let next-pos =
                va-lfold none
                    inline (__ arg prev)
                        let result =
                            static-if (none? prev)
                                `(parse-arg-int stream argstart)
                            else
                                `(parse-arg-int stream prev)
                        sc_call_append_argument makeop-call `(typeinit (va@ 0 result))
                        `(va@ 1 result)
                    va-range 0 (countof ft.Type)
            sc_switch_append_case sw `[(hash (tostring ft.Name))]
                spice-quote
                    'append program.code
                        makeop-call
                    next-pos or argstart

        OpCode.__fields__
    sc_switch_append_default sw
        spice-quote
            error
                .. "unknown instruction: '" (tostring name) "' at position "
                    (tostring (argstart - (countof name)))
    sw
run-stage;

fn parse (filename)
    let source = (utils.read-file filename)
    let slen = (countof source)
    local program : Program

    inline advance (next)
        repeat (next as i32) (next as usize)

    for idx c in (enumerate source)
        let skip = (consume-whitespace source idx)
        if (skip > idx)
            advance skip

        # skip comment line
        if (c == (char "#"))
            advance
                loop (idx = (idx + 1))
                    if (((source @ idx) == (char "\n")) or (idx >= (countof source)))
                        break idx
                    idx + 1

        # get instruction
        local instruction : String
        :: instruction-name
        for idx c in (string-slice source idx)
            # capital letter?
            if ((c < 65:i8) or (c > 90:i8))
                merge instruction-name idx
            'append instruction c
        countof source
        instruction-name (idx) ::

        if ((countof instruction) == 0)
            err-malformed;

        if ((hash instruction) == (bitcast 'CONSTANTS hash))
            let idx = (consume-whitespace source idx)

            c := source @ idx
            if (c != (char "{"))
                err-malformed;

            :: parse-constants
            let table-start = (consume-whitespace source (idx + 1))
            for idx c in (string-slice source table-start)
                # we use this variant to consume empty lines as well. Because of this,
                # after we parse a constant we consume the trailing whitespace to avoid
                # having more than one constant on the same line.
                let skip = (consume-whitespace source idx)
                if (skip > idx)
                    repeat skip

                if (c == (char "}"))
                    merge parse-constants (idx + 1)
                # we expect either a string literal or a number.
                if (c == (char "\""))
                    local stringlit : String

                    :: parse-str
                    for idx c in (string-slice source (idx + 1))
                        if (c == (char "\n"))
                            err-malformed;
                        if (c == (char "\""))
                            'append program.constant-table (LangValue.String stringlit)
                            merge parse-str (idx + 1)
                        # TODO: refactor this out, because we also need to convert escapes like \n
                        'append stringlit c
                    # string ended
                    err-malformed;
                    parse-str (stringlit-end) ::

                    repeat (consume-trailing-whitespace source stringlit-end)
                if (c == (char "-"))
                    if (digit? (source @ (idx + 1)))
                        let val next = (parse-arg-int source (idx + 1))
                        'append program.constant-table (LangValue.Number (f64 -val))
                        repeat (consume-trailing-whitespace source next)
                    else
                        err-malformed;
                if (digit? c)
                    let val next = (parse-arg-int source idx)
                    'append program.constant-table (LangValue.Number (f64 val))
                    repeat (consume-trailing-whitespace source next)

            # reached EOF without closing constants table
            err-malformed;
            parse-constants (idx) ::
            advance idx

        let next-pos = (append-instruction-with-args program instruction source idx)
        if true (advance next-pos)

    program

do
    let parse
    locals;
