using import Array
using import Map
using import String
using import .program
import .stdlib

inline tag== (ev eT)
    ('literal ev) == eT.Literal

fn execute (program)
    local stack : (Array LangValue)
    local acc : f64

    inline calc-index (i)
        (countof stack) - 1 - i

    inline gen-bin-op (op a b)
        let a b =
            stack @ (calc-index a)
            stack @ (calc-index b)
        op
            'unsafe-extract-payload a f64
            'unsafe-extract-payload b f64

    stdlib.register program

    print "--- EXECUTION STARTS HERE ---"
    for idx op in (enumerate program.code)
        inline jump (idx)
            repeat idx idx

        dispatch op
        case CALL (argc)
            let f = ('pop stack)
            # ...
        case CCALL (argc)
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
        case ADD (A B)
            acc = (gen-bin-op fadd A B)
        case SUB (A B)
            acc = (gen-bin-op fsub A B)
        case MUL (A B)
            acc = (gen-bin-op fmul A B)
        case DIV (A B)
            acc = (gen-bin-op fdiv A B)
        case STORE (index)
            stack @ (calc-index index) = (LangValue.Number acc)
        default
            error "unsupported opcode"

do
    let execute
    locals;
