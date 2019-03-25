#' center_l_function
#'
#' @description Centered L-function
#'
#' @param x ppp
#' @param ... Arguments passed to \code{spatstat::Lest()}
#'
#' @details
#' Centers Besag's L-function to zero by calculating L(r) -r. Centering
#' the L-function allows an easier interpretation and plotting of the results (Haase 1995).
#'
#' @seealso
#' \code{\link{Lest}}
#'
#' @examples
#' input_pattern <- spatstat::runifpoint(n = 100)
#' center_l_function(input_pattern, correction = "Ripley")
#'
#' lest <- spatstat::Lest(input_pattern)
#' center_l_function(lest)
#'
#' @references
#' Besag, J. E. 1977. Discussion on Dr. Ripley's paper. - J. R. Stat. Soc. Ser. B 39: 193-195.
#'
#' Ripley, B. D. 1977. Modelling spatial patterns. - J. R. Stat. Soc. Ser. B 39: 172-192.
#'
#' Haase, P. 1995. Spatial pattern analysis in ecology based on Ripley's K-function:
#' Introduction and methods of edge correction. - J. Veg. Sci. 6: 575-582.
#'
#' @aliases center_l_function
#' @rdname center_l_function

#' @export
center_l_function <- function(x, ...) {

  if(!spatstat::is.ppp(x) && !spatstat::is.fv(x)) {
    stop("Please provide either ppp or fv object.")
  }

  if(spatstat::is.ppp(x)) {
    x <- spatstat::Lest(x, ...)
  }

  r <- x$r

  l_centered <- spatstat::eval.fv(x - r)

  return(l_centered)
}
