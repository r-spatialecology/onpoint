#' center_Lest
#'
#' @description Centered L-function
#'
#' @param x ppp
#' @param ... Arguments passed to \code{spatstat.explore::Lest()}
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
#' \code{\link[spatstat.explore]{Lest}}
#'
#' @examples
#' input_pattern <- spatstat.random::runifpoint(n = 100)
#' center_Lest(input_pattern, correction = "Ripley")
#'
#' lest <- spatstat.explore::Lest(input_pattern)
#' center_Lest(lest)
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
#' @export
center_Lest <- function(x, ...) {

  if (!spatstat.geom::is.ppp(x) && !spatstat.geom::is.fv(x)) {
    stop("Please provide either ppp or fv object.")
  }

  if (spatstat.geom::is.ppp(x)) {
    x <- spatstat.explore::Lest(x, ...)
  }

  r <- x$r

  l_centered <- spatstat.explore::eval.fv(x - r)

  return(l_centered)
}
