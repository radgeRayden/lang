using import struct
using import enum
using import Array
using import String
using import Map

enum LangValue
    String : String
    Number : f64
    Boolean : bool
    Nil

    inline true? (self)
        dispatch self
        case Nil ()
            false
        case Boolean (val)
            deref val
        default
            true

    inline __repr (self)
        'apply self
            inline (T val)
                ..
                    static-if (T.Index == String)
                        repr (tostring val)
                    else
                        tostring val
                    default-styler 'style-operator ":"
                    default-styler 'style-type (tostring T.Name)

let CWrapper =
    typeof
        static-typify
            fn (stack)
                ;
            (mutable (& (Array LangValue)))

let StackIndex = u32
enum OpCode
    # call function at the top of stack
    CALL : (argc = u8)
    # jump back to address after call
    RETURN
    # call registered C function
    CCALL : (argc = u8)
    # push value from constant value table onto the stack
    PUSH : (address = usize)
    # push value stored at stack position `index` onto the stack
    PUSHI : (index = StackIndex)
    # pop value from stack and store at target
    POP : (target = StackIndex)
    # copy stack entry A onto stack entry B
    COPY : (A = StackIndex) (B = StackIndex)
    # pop `argc` values from the stack and discard them
    DISCARD : (argc = u8)
    # make space for local variables
    ALLOCA : u32

    # arithmetic instructions
    ADD : (A = StackIndex) (B = StackIndex)
    SUB : (A = StackIndex) (B = StackIndex)
    MUL : (A = StackIndex) (B = StackIndex)
    DIV : (A = StackIndex) (B = StackIndex)
    NEG : (in = StackIndex)

    # store acc register at index
    STORE : (index = StackIndex)

    # control flow
    # unconditionally jump to address
    JUMP : (address = usize)
    # push (A == B)
    TEST_EQ : (A = StackIndex) (B = StackIndex)
    # push (A != B)
    TEST_NEQ : (A = StackIndex) (B = StackIndex)
    # push (A > B)
    TEST_GT : (A = StackIndex) (B = StackIndex)
    # push (A < B)
    TEST_LT : (A = StackIndex) (B = StackIndex)
    # jump if stack top is true
    JUMP_T : (address = usize)
    # jump if stack top is false
    JUMP_F : (address = usize)

    # logical operators
    NOT : (arg = StackIndex)
    AND : (A = StackIndex) (B = StackIndex)
    OR : (A = StackIndex) (B = StackIndex)

    inline __repr (self)
        'apply self
            inline (T args...)
                va-lfold (tostring T.Name)
                    inline (__ next computed)
                        .. computed " " (tostring next)
                    args...

struct Program
    constant-table : (Array LangValue)
    code : (Array OpCode)
    functions : (Map String CWrapper)

do
    let
        LangValue
        OpCode
        Program
        CWrapper
    locals;
