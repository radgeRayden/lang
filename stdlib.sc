using import String
using import Array
using import .common
import .utils

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

let C = (import .radlib.libc)

vvv bind global-scope
do
    fn print (stack)
        let msg = ('pop stack)
        C.stdio.printf "%s\n" ((utils.extract-as-tag msg 'String) as rawstring)
        ;

    fn tostring (stack)
        let arg = ('pop stack)
        'append stack
            LangValue.String
                'apply arg
                    inline (T val)
                        String (tostring val)
        ;
    locals;

fn register (program)
    register-scope global-scope program

do
    let CWrapper register
    locals;
