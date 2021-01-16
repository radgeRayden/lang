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

inline err-malformed ()
    hide-traceback;
    error "malformed input file"

fn parse (filename)
    let source = (utils.read-file filename)
    let slen = (countof source)
    loop (idx = 0:usize)
        if (idx >= slen)
            break;

        let c = (source @ idx)

        # consume leading whitespace
        if (whitespace? c)
            repeat (idx + 1)

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
            print (c as string)
            # capital letter?
            if ((c < 65:i8) or (c > 90:i8))
                break;
            'append instruction c
            next-pos += 1

        if ((countof instruction) == 0)
            err-malformed;

        switch (bitcast (hash instruction) Symbol)
        case 'CONSTANTS
            print "constants!!"
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
