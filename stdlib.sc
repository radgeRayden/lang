using import String
using import Array
using import .program

spice register-scope (scope program)
    scope as:= Scope
    let expr = (sc_expression_new)
    for k f in scope
        let name = (k as Symbol as string)
        sc_expression_append expr
            spice-quote
                'set program.functions (String [name])
                    imply f CWrapper
    expr

run-stage;

inline tag== (ev eT)
    ('literal ev) == eT.Literal

let C = (import .radlib.libc)

vvv bind global-scope
do
    fn print (args)
        assert ((countof args) == 1)
        let msg = ('pop args)
        move args
        assert (tag== msg LangValue.String)
        C.stdio.printf "%s\n" (('unsafe-extract-payload msg String) as rawstring)
        ((Array LangValue))
    locals;

fn register (program)
    register-scope global-scope program

do
    let CWrapper register
    locals;
