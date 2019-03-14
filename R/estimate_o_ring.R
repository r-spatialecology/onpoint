#' estimate_o_ring
#'
#' @description O-ring function
#'
#' @param x ppp
#' @param ... Arguments passed to \code{spatstat::pcf.ppp()}
#'
#' @details
#' Estimate the O-ring function.
#'
#' @seealso
#' \code{\link{envelope}}
#' \code{\link{density.ppp}}
#'
#' @examples
#' input_pattern <- spatstat::runifpoint(n = 100)
#' estimate_o_ring(input_pattern)
#'
#' @references
#' Wiegand, T. and Moloney, K. A. 2014. Handbook of spatial point-pattern analysis in ecology. - Chapman and Hall/CRC Press.
#'
#' Wiegand, T. and Moloney, K. A. 2004. Rings, circles, and null models for point pattern analysis in ecology. - Oikos 104: 209–229.
#'
#' @aliases estimate_o_ring
#' @rdname estimate_o_ring

#' @export
estimate_o_ring <- function(x, ...) {

  if(!spatstat::is.ppp(x)) {
    stop("Please provide ppp.")
  }

  p_fct <- spatstat::pcf.ppp(x, ...)

  lambda <- spatstat::intensity.ppp(spatstat::unmark(x))

  o_ring <- spatstat::eval.fv(p_fct * lambda)

  return(o_ring)
}
