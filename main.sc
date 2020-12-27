# FIXME: because \ isn't pre processed, we can't have literals like \n and \t
spice prefix:ch (str)
    str as:= string
    let count = (countof str)
    if (count != 1)
        hide-traceback;
        error "character literals should have a size of one"

    c := str @ 0
    `c

inline tag== (ev eT)
    ('literal ev) == eT.Literal

run-stage;

using import enum
using import String
using import Array
using import struct
using import Rc

let C = (import .radlib.libc)

enum TokenKind
    Identifier : String
    StringLiteral : String
    OpenParens
    CloseParens
    StatementEnd

    inline __repr (self)
        'apply self
            inline (T value)
                static-if (not (none? value))
                    .. (repr T.Name) ": " (tostring value)
                else
                    repr T.Name

struct LangAnchor
    source : (Rc String)
    line : i32
    column : i32
    offset : i32

    inline __repr (self)
        using import .radlib.stringtools
        tostring (format "%s:%d:%d" (imply self.source String) self.line self.column)

    inline __copy (self)
        this-type
            copy self.source
            copy self.line
            copy self.column
            copy self.offset

struct Token
    kind : TokenKind
    anchor : LangAnchor

    inline __repr (self)
        .. (repr self.anchor) "\t" (repr self.kind)

inline letter? (c)
    or
        and
            c >= ch"A"
            c <= ch"Z"
        and
            c >= ch"a"
            c <= ch"z"

inline alphanum? (c)
    or
        letter? c
        and
            c >= ch"0"
            c <= ch"9"

inline whitespace? (c)
    switch c
    pass ch" "
    pass 9:i8 # \t
    pass 10:i8 # \n
    do
        true
    default
        false

fn tokenize (source filename)
    local tokens : (Array Token)
    local anchor : LangAnchor (Rc.wrap (copy filename))
    local line-start = 0

    loop (idx = 0)
        if (idx >= (countof source))
            break (deref tokens)

        c := source @ idx
        anchor.column = (idx - line-start)
        anchor.offset = idx

        if (letter? c)
            local ident : String
            'append ident c
            repeat
                loop (idx = (idx + 1))
                    c := source @ idx
                    if (alphanum? c)
                        'append ident c
                        idx + 1
                    else
                        'append tokens (Token (TokenKind.Identifier (deref ident)) (copy anchor))
                        break idx

        switch c
        case ch"("
            'append tokens (Token (TokenKind.OpenParens) (copy anchor))
        case ch")"
            'append tokens (Token (TokenKind.CloseParens) (copy anchor))
        case ch"\""
            local stringlit : String
            repeat
                loop (idx = (idx + 1))
                    if (idx >= (countof source))
                        break idx
                    c := source @ idx
                    if (not (c == ch"\""))
                        'append stringlit c
                        idx + 1
                    else
                        'append tokens (Token (TokenKind.StringLiteral (deref stringlit)) (copy anchor))
                        # skip ending quote
                        break (idx + 1)
        case ch";"
            'append tokens (Token (TokenKind.StatementEnd) (copy anchor))
        case 10:i8 # \n
            'append tokens (Token (TokenKind.StatementEnd) (copy anchor))
            # consume all whitespace
            repeat
                loop (idx = idx)
                    if (idx >= (countof source))
                        break idx
                    c := source @ idx

                    if (c == 10:i8)
                        anchor.line += 1
                        line-start = idx + 1

                    if (not (whitespace? c))
                        break idx
                    idx + 1
        default
            ;

        idx + 1

enum LangValue
    Function
    String
    Number

struct ASTNode
    value : LangValue

enum ParserState plain
    StatementEnd # doubles as initial state
    SeenIdentifier

run-stage;

inline LangAnchor->Anchor (anc)
    sc_anchor_new
        Symbol (tostring (imply anc.source String))
        anc.line
        anc.column
        anc.offset

vvv bind ParsingError
do
    inline UnexpectedToken (anc)
        hide-traceback;
        error@
            LangAnchor->Anchor anc
            "while parsing script"
            "unexpected token"
    locals;

fn parse (tokens)
    local root : (Array ASTNode)

    fold (pstate = ParserState.StatementEnd) for tk in tokens
        using ParserState
        switch pstate
        case StatementEnd
            # expects: identifier
            if (not (tag== tk.kind TokenKind.Identifier))
                ParsingError.UnexpectedToken tk.anchor
            ParserState.SeenIdentifier
        case SeenIdentifier
            # expects: open parens, operator
            dispatch tk.kind
            case OpenParens ()
                pstate
            default
                ParsingError.UnexpectedToken tk.anchor
        default
            if (tag== tk.kind TokenKind.StatementEnd)
                ParserState.StatementEnd
            else
                pstate

fn encode (AST)
    ;

fn execute (bytecode)
    ;

fn read-source (filename)
    local buf : String
    do
        using C.stdio
        let fhandle = (fopen filename "rb")
        fseek fhandle 0 SEEK_END
        let flen = (ftell fhandle)
        fseek fhandle 0 SEEK_SET

        'resize buf (flen as usize)
        fread buf._items 1 (flen as u64) fhandle
        fclose fhandle
    buf

do
    let module argc argv = (script-launch-args)
    assert (argc > 0) "expected source file"
    let input = (String (argv @ 0) (C.string.strlen (argv @ 0)))
    let source = (read-source input)
    let tokens = (tokenize source input)
    let AST = (parse tokens)
    let bytecode = (encode AST)
    execute bytecode
