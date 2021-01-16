import .utils
using import String
using import UTF-8

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

fn consume-whitespace (stream initpos)
    imply initpos usize
    loop (idx = initpos)
        c := stream @ idx
        if (whitespace? c)
            repeat (idx + 1)
        # we necessarily stop at the zero terminator at the most, in that case
        # we want to return its position instead of the next (invalid) index.
        break (min idx (countof stream))

# like consume-whitespace, but stops after a line break.
fn consume-whitespace-endl (stream initpos)
    imply initpos usize
    loop (idx = initpos)
        c := stream @ idx
        if (c == (char "\n"))
            break (idx + 1)
        if (whitespace? c)
            repeat (idx + 1)
        # we necessarily stop at the zero terminator at the most, in that case
        # we want to return its position instead of the next (invalid) index.
        break (min idx (countof stream))

inline err-malformed ()
    hide-traceback;
    error "malformed input file"

fn parse (filename)
    let source = (utils.read-file filename)
    let slen = (countof source)
    loop (idx = 0:usize)
        let idx = (consume-whitespace source idx)
        if (idx >= slen)
            break;

        let c = (source @ idx)

        # skip comment line
        if (c == (char "#"))
            repeat
                loop (idx = (idx + 1))
                    if (((source @ idx) == (char "\n")) or (idx >= (countof source)))
                        break idx
                    idx + 1

        # get instruction
        local next-pos = idx
        local instruction : String
        loop ()
            let c = (source @ next-pos)
            # capital letter?
            if ((c < 65:i8) or (c > 90:i8))
                break;
            'append instruction c
            next-pos += 1

        if ((countof instruction) == 0)
            err-malformed;

        switch (bitcast (hash instruction) Symbol)
        case 'CONSTANTS
            # while (whitespace? (source @ next-pos))
            #     next-pos += 1
            next-pos = (consume-whitespace source (deref next-pos))

            c := source @ next-pos
            if (c != (char "{"))
                err-malformed;

            # parse constant list
            next-pos += 1
            loop (idx = (deref next-pos))
                let idx = (consume-whitespace source idx)
                if (idx >= slen)
                    err-malformed;

                c := source @ idx
                if (c == (char "}"))
                    next-pos = idx + 1
                    break;
                # we expect either a string literal or a number.
                if (c == (char "\""))
                    local stringlit : String
                    let stringlit-end =
                        loop (idx = (idx + 1))
                            if (idx >= slen)
                                err-malformed;
                            c := source @ idx
                            if (c == (char "\n"))
                                err-malformed;
                            if (c == (char "\""))
                                print stringlit
                                break (idx + 1)
                            # TODO: refactor this out, because we also need to convert escapes like \n
                            'append stringlit c
                            idx + 1

                    repeat (consume-whitespace-endl source stringlit-end)
                if (digit? c)
                    # TODO: this
                idx + 1
        case 'CALL
        case 'RETURN
        case 'CCALL
        case 'PUSH
        case 'PUSHI
        case 'POP
        case 'DISCARD
        default
            ;

        deref next-pos

do
    let parse
    locals;
