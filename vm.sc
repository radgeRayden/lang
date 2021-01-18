using import Array
using import Map
using import String
using import .program
import .stdlib

inline tag== (ev eT)
    ('literal ev) == eT.Literal

fn execute (program)
    local stack : (Array LangValue)

    stdlib.register program

    print "--- EXECUTION STARTS HERE ---"
    for idx op in (enumerate program.code)
        inline jump (idx)
            repeat idx idx

        dispatch op
        case CALL (argc)

            let f = ('pop stack)
            # ...
        case CCALL ()
            let name = ('pop stack)
            assert (tag== name LangValue.String)
            let name = ('unsafe-extract-payload name String)

            try
                ('get program.functions name) stack
            else
                print name
                error "unknown C function"
            ;
        case PUSH (address)
            'append stack (copy (program.constant-table @ address))
        case PUSHI (index)
            'append stack (copy (stack @ index))
        case DISCARD (argc)
            for i in (range argc)
                'pop stack
            ;
        default
            error "unsupported opcode"

do
    let execute
    locals;
