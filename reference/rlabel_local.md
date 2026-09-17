# rlabel_local

Local random labelling of marked point pattern

## Usage

``` r
rlabel_local(X, distance, nsim = 19, drop = TRUE)
```

## Arguments

- X:

  ppp

- distance:

  Mark of points that do not change.

- nsim:

  Number of patterns to simulate.

- drop:

  If nsim = 1 and drop = TRUE , the result will be a point pattern,
  rather than a list containing a point pattern.

## Value

list

## Details

Local random labelling function, i.e. marks will be shuffeld only across
points within the specified local distance. Technically, this is achived
by sampling the mark of a neighbouring point j within the distance d for
the focal point i. Thus, the distance d must be selected in a way that
each point has at least one neighbour within d.

Returns a `list` with `ppp` objects.

## References

Velázquez, E., Martínez, I., Getzin, S., Moloney, K.A., Wiegand, T.,
2016. An evaluation of the state of spatial point pattern analysis in
ecology. Ecography 39, 1–14. \<https://doi.org/10.1111/ecog.01579\>

Wiegand, T., Moloney, K.A., 2014. Handbook of spatial point-pattern
analysis in ecology. Chapman and Hall/CRC Press, Boca Raton, USA.
\<isbn:978-1-4200-8254-8\>

## See also

[`rlabel`](https://rdrr.io/pkg/spatstat.random/man/rlabel.html)

## Examples

``` r
set.seed(42)
pattern <- spatstat.random::runifpoint(n = 250, win = spatstat.geom::owin(c(0, 100), c(0, 100)))
spatstat.geom::marks(pattern) <- runif(n = 250, min = 10, max = 120)

rlabel_local(X = pattern, distance = 25, nsim = 19)
#> Warning: drop = TRUE only possible for nsim = 1.
#> [[1]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[2]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[3]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[4]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[5]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[6]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[7]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[8]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[9]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[10]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[11]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[12]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[13]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[14]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[15]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[16]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[17]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[18]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
#> [[19]]
#> Marked planar point pattern: 250 points
#> marks are numeric, of storage type  ‘double’
#> window: rectangle = [0, 100] x [0, 100] units
#> 
```
