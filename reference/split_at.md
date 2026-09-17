# split_at

Split vector

## Usage

``` r
split_at(x, pos)
```

## Arguments

- x:

  vector with positions to split.

## Value

list

## Details

Split vector at position(s). Returns a `list` with all elements before
and after the split position.

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(42)
x <- sample(x = 1:10, size = 5)
split_at(x = x, pos = 3)
} # }
```
