using import String

let C = (import .radlib.libc)

fn read-file (filename)
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

inline extract-as-tag (ev tag)
    """"Force extraction of enum value as one of its tags.
        Expected arguments:
            ev = Enum
            tag = Symbol
        Will abort if unwrapping a value whose tag doesn't match what was requested.
        Example usage:
        ```
        enum A
            a : i32 i32
        let ev = (A.a)
        let a0 a1 = (extract-as-tag ev 'a)
        ```
    let eT = (typeof ev)
    let ft = (getattr eT tag)
    let lit = ft.Literal

    # safety check
    assert (('literal ev) == lit)
    'unsafe-extract-payload ev ft.Type

do
    let
        read-file
        extract-as-tag
    locals;
