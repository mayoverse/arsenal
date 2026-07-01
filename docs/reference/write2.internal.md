# Helper functions for `write2`

Helper functions for
[`write2`](https://mayoverse.github.io/arsenal/reference/write2.md).

## Usage

``` r
verbatim(...)

code.chunk(..., chunk.opts = "r")
```

## Arguments

- ...:

  For `verbatim`, objects to print out monospaced (as if in the
  terminal). For `code.chunk`, either expressions or single character
  strings to paste into the code chunk.

- chunk.opts:

  A single character string giving the code chunk options. Make sure to
  specify the engine!

## Details

The `"verbatim"` class is to tell
[`write2`](https://mayoverse.github.io/arsenal/reference/write2.md) to
print the object inside a section surrounded by three back ticks. The
results will look like it would in the terminal (monospaced).

`code.chunk()` is to write explicit code chunks in the `.Rmd` file; it
captures the call and writes it to the file, to execute upon knitting.
