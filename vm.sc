using import Array
using import Map
using import String
using import .program
import .utils
import .stdlib


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
            utils.extract-as-tag a 'Number
            utils.extract-as-tag b 'Number

    stdlib.register program

    print "--- EXECUTION STARTS HERE ---"
    for idx op in (enumerate program.code)
        inline jump (idx)
            let idx = (copy idx)
            repeat (idx as i32) (idx as usize)

        dispatch op
        case JUMP (address)
            jump address
        case CALL (argc)
            let f = ('pop stack)
            # ...
        case CCALL (argc)
            let name = (utils.extract-as-tag ('pop stack) 'String)
            try
                ('get program.functions name) stack
            else
                print name
                error "unknown C function"
            ;
        case PUSH (address)
            'append stack (copy (program.constant-table @ address))
        case PUSHI (index)
            'append stack (copy (stack @ (calc-index index)))
        case DISCARD (argc)
            for i in (range argc)
                'pop stack
            ;
        case ALLOCA (argc)
            for i in (range argc)
                'append stack (LangValue.Nil)
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
