using import struct
using import enum
using import Array
using import String
using import Map

enum LangValue
    String : String
    Number : f64
    Nil

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
    # unconditionally jump to address
    JUMP : (address = usize)
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
