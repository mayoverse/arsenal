# Include a YAML header in `write2`

Include a YAML header in `write2`

## Usage

``` r
yaml(...)

# S3 method for class 'yaml'
print(x, ...)

# S3 method for class 'yaml'
c(..., recursive = FALSE)

is.yaml(x)
```

## Arguments

- ...:

  For `yaml()`, arguments to be bundled into a list and passed to
  [`as.yaml`](https://yaml.r-lib.org/reference/as.yaml.html). For
  `print.yaml()`, extra arguments. For `c.yaml()`, "yaml" objects to be
  concatenated.

- x:

  An object of class `"yaml"`.

- recursive:

  Not in use at this time.

## Value

A text string of class `"yaml"`.

## See also

[`as.yaml`](https://yaml.r-lib.org/reference/as.yaml.html),
[`write2`](https://mayoverse.github.io/arsenal/reference/write2.md)

## Author

Ethan Heinzen, adapted from an idea by Brendan Broderick

## Examples

``` r
x <- yaml(title = "My cool title", author = "Ethan P Heinzen")
x
#> ---
#> title: My cool title
#> author: Ethan P Heinzen
#> ---
y <- yaml("header-includes" = list("\\usepackage[labelformat=empty]{caption}"))
y
#> ---
#> header-includes:
#> - \usepackage[labelformat=empty]{caption}
#> ---
c(x, y)
#> ---
#> title: My cool title
#> author: Ethan P Heinzen
#> header-includes:
#> - \usepackage[labelformat=empty]{caption}
#> ---
```
