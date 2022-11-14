#' estimate_o_ring
#'
#' @description O-ring function
#'
#' @param x ppp
#' @param ... Arguments passed to \code{spatstat.explore::pcf.ppp()}
#'
#' @details
#' Estimates the O-ring function proposed by Wiegand and Moloney (2004). The
#' O-ring statistic is defined as:
#'
#' \deqn{O(r) = \lambda * g(r)}
#'
#' Generally speaking, O(r) scales the pair correlation g(r) function with help
#' of the intensity \eqn{\lambda}. One advantage of the O-ring statistic is that
#' it can be interpreted as a neighborhood density because it is a probability density
#' function (Wiegand & Moloney 2004, 2014).
#'
#' Returns an 'Function value object' of the \code{spatstat} package.
#'
#' @return fv.object
#'
#' @seealso
#' \code{\link{density.ppp}} \cr
#' \code{\link{pcf}}
#'
#' @examples
#' input_pattern <- spatstat.random::runifpoint(n = 100)
#' estimate_o_ring(input_pattern)
#'
#' @references
#' Wiegand, T., Moloney, K.A., 2004. Rings, circles, and null models for point pattern
#' analysis in ecology. Oikos 104, 209–229. <https://doi.org/10.1111/j.0030-1299.2004.12497.x>
#'
#' Wiegand, T., Moloney, K.A., 2014. Handbook of spatial point-pattern analysis in
#' ecology. Chapman and Hall/CRC Press, Boca Raton, USA. <isbn:978-1-4200-8254-8>
#'
#' @aliases estimate_o_ring
#' @rdname estimate_o_ring
#'
#' @export
estimate_o_ring <- function(x, ...) {

  if (!spatstat.geom::is.ppp(x)) {
    stop("Please provide ppp.")
  }

  p_fct <- spatstat.explore::pcf.ppp(x, ...)

  lambda <- spatstat.geom::intensity.ppp(spatstat.geom::unmark(x))

  o_ring <- spatstat.explore::eval.fv(p_fct * lambda)

  return(o_ring)
}
