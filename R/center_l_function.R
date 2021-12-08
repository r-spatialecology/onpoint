#' center_l_function
#'
#' @description Centered L-function
#'
#' @param x ppp
#' @param ... Arguments passed to \code{spatstat.core::Lest()}
#'
#' @details
#' Centers Besag's L-function to zero by calculating L(r) -r. Centering the L-function
#' allows an easier interpretation and plotting of the results (Haase 1995).
#'
#' Returns an 'Function value object' of the \code{spatstat} package.
#'
#' @return fv.object
#'
#' @seealso
#' \code{\link{Lest}}
#'
#' @examples
#' input_pattern <- spatstat.core::runifpoint(n = 100)
#' center_l_function(input_pattern, correction = "Ripley")
#'
#' lest <- spatstat.core::Lest(input_pattern)
#' center_l_function(lest)
#'
#' @references
#' Besag, J.E., 1977. Discussion on Dr. Ripley’s paper. Journal of the Royal Statistical
#' Society. Series B (Methodological) 39, 193–195.
#' <https://doi.org/10.1111/j.2517-6161.1977.tb01616.x>
#'
#' Ripley, B.D., 1977. Modelling spatial patterns. Journal of the Royal Statistical Society.
#' Series B (Methodological) 39, 172–192. <https://doi.org/10.1111/j.2517-6161.1977.tb01615.x>
#'
#' Haase, P., 1995. Spatial pattern analysis in ecology based on Ripley’s K-function:
#' Introduction and methods of edge correction. Journal of Vegetation Science 6, 575–582.
#' <https://doi.org/10.2307/3236356>
#'
#' @aliases center_l_function
#' @rdname center_l_function
#'
#' @export
center_l_function <- function(x, ...) {

  if (!spatstat.geom::is.ppp(x) && !spatstat.geom::is.fv(x)) {
    stop("Please provide either ppp or fv object.")
  }

  if (spatstat.geom::is.ppp(x)) {
    x <- spatstat.core::Lest(x, ...)
  }

  r <- x$r

  l_centered <- spatstat.core::eval.fv(x - r)

  return(l_centered)
}
