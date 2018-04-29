# r7rs-format
A string formatting library for r7rs scheme.

## description
Traditionally string formatting in Scheme is done through `srfi-48`. I consider the
`srfi-48` syntax outdated, so here's a string formatting library that follows more
modern formatting specification strings similar to those in Python, C# and Rust.

```scheme
(format "{}, {}!" "Hello" "World")
=> "Hello, World!"
```

```scheme
(format "{0} {2} {1}" 1 2 3)
=> "1 3 2"
```

```scheme
(format "{:s}" "s-expression")
=> "\"s-expression\""
```

Convenience routines `print`, `println` exist that will print directly to stdout.
Support for fixed-with and/or numeric formatting and alignment is under way.
Named (key-word) arguments are not a standard feature of scheme, so this feature
also lacks in this implementation.

## tests
Some unit-tests are present:

```bash
$ ./scripts/run-tests.sh
```

runs tests using `chibi-scheme`.

## TODO
- Overload `print` and `println` to accept a `port` as first argument.
- Implement the rest of the format spec.
