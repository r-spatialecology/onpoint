#' center_l_function
#'
#' @description Centered L-function
#'
#' @param x ppp
#' @param ... Arguments passed to \code{spatstat::Lest()}
#'
#' @details
#' Center Besag's L-function to zero.
#'
#' @seealso
#' \code{\link{envelope}}
#' \code{\link{density.ppp}}
#'
#' @examples
#' input_pattern <- spatstat::runifpoint(n = 100)
#' center_l_function(input_pattern, correction = "Ripley")
#'
#' lest <- spatstat::Lest(input_pattern)
#' center_l_function(lest)
#'
#' @references
#' Besag, J. (1977) Discussion of Dr Ripley's paper. Journal of the Royal Statistical Society, Series B, 39, 193–195.
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
