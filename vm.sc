using import Array
using import Map
using import String
using import struct
using import .program
import .utils
import .stdlib

struct VM
    stack : (Array LangValue)
    program : Program
    acc : f64

    inline __typecall (cls program)
        local program = program
        stdlib.register program
        super-type.__typecall cls
            program = program

fn execute (vm)
    let stack acc program =
        vm.stack
        vm.acc
        vm.program

    inline calc-index (i)
        (countof stack) - 1 - i

    inline numeric-bin-op (op a b)
        let a b =
            stack @ (calc-index a)
            stack @ (calc-index b)
        op
            utils.extract-as-tag a 'Number
            utils.extract-as-tag b 'Number

    inline logic-bin-op (op a b)
        let a b =
            stack @ (calc-index a)
            stack @ (calc-index b)
        op
            imply a bool
            imply b bool

    print "--- EXECUTION STARTS HERE ---"
    for idx op in (enumerate program.code)
        inline jump (idx)
            let idx = (copy idx)
            repeat (idx as i32) (idx as usize)

        dispatch op
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

        # arithmetic
        case ADD (A B)
            acc = (numeric-bin-op fadd A B)
        case SUB (A B)
            acc = (numeric-bin-op fsub A B)
        case MUL (A B)
            acc = (numeric-bin-op fmul A B)
        case DIV (A B)
            acc = (numeric-bin-op fdiv A B)
        case STORE (index)
            stack @ (calc-index index) = (LangValue.Number acc)

        # control flow
        case JUMP (address)
            jump address
        case JUMP_T (address)
            let tval = ('pop stack)
            if tval
                jump address
        case JUMP_F (address)
            let tval = ('pop stack)
            if (not tval)
                jump address

        # comparisons
        case TEST_EQ (A B)
            # for equality, we let the Enum == metamethod do the work.
            let A B =
                stack @ (calc-index A)
                stack @ (calc-index B)
            'append stack (LangValue.Boolean (A == B))
        case TEST_NEQ (A B)
            # see TEST_EQ
            let A B =
                stack @ (calc-index A)
                stack @ (calc-index B)
            'append stack (LangValue.Boolean (A != B))
        case TEST_GT (A B)
            'append stack
                LangValue.Boolean
                    numeric-bin-op fcmp>o A B
        case TEST_LT (A B)
            'append stack
                LangValue.Boolean
                    numeric-bin-op fcmp<o A B

        # logical operators
        case NOT (arg)
            'append stack
                LangValue.Boolean
                    not (stack @ (calc-index arg))
        case AND (A B)
            'append stack
                LangValue.Boolean
                    logic-bin-op band A B
        case OR (A B)
            'append stack
                LangValue.Boolean
                    logic-bin-op bor A B

        default
            error "unsupported opcode"

do
    let execute VM
    locals;
