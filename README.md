# blixt
A work in progress toy programming language written in Rust.
Blixt is a statically typed interpreted language.

An example of the syntax:

    fn main() {
       name := "Jonas"
       age := 28
       phrase := input("Enter your catchphrase: ")

       present(name, age, phrase)
       a := 12
       b := 4
       print("% is a multiple of %? %\n", a, b, is_multiple(a, b))
    }

    fn present(name: string, age: int, phrase: string) {
       print("% is % years old and usually says \"%\"\n", name, age, phrase)
    }

    fn is_multiple(a: int, b: int) -> bool {
       return a % b == 0
    }

    main()
