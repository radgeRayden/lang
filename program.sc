using import struct
using import enum
using import Array
using import String
using import Map

enum LangValue
    String : String
    Integer : i64
    Real : f64

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


enum OpCode
    # call function at the top of stack with `argc` arguments starting at index 1
    CALL : (argc = u8)
    # jump back to address after call
    RETURN
    # call registered C function
    CCALL : (argc = u8)
    # push value from constant value table onto the stack
    PUSH : (address = usize)
    # push value stored at stack position `index` onto the stack
    PUSHI : (index = u8)
    # pop value from stack and store at target
    POP : (target = u8)
    # pop `argc` values from the stack and discard them
    DISCARD : (argc = u8)

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
