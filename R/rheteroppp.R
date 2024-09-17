#' rheteroppp
#'
#' @description Simulate heterogeneous pattern
#'
#' @param x ppp
#' @param nsim Number of patterns to simulate.
#' @param fix_n Logical if true the null model patterns have exactly the same number of points ais input.
#' @param ... Arguments passed to \code{spatstat.explore::density.ppp()}.
#'
#' @details
#' Simulate heterogeneous point patterns as null model data for \code{spatstat.explore::envelope()}.
#' A heterogeneous Poisson process is used, meaning that there are no interaction between points,
#' however, the simulated coordinates depend on the intensity \eqn{\lambda} of the input pattern.
#'
#' Returns a \code{list} with \code{ppp} objects.
#'
#' @return list
#'
#' @seealso
#' \code{\link[spatstat.explore]{envelope}} \cr
#' \code{\link[spatstat.explore]{density.ppp}}
#'
#' @examples
#' set.seed(42)
#' input_pattern <- spatstat.random::rpoispp(lambda = function(x , y) {100 * exp(-3 * x)}, nsim = 1)
#' null_model <- rheteroppp(input_pattern, nsim = 19)
#' spatstat.explore::envelope(Y = input_pattern, fun = spatstat.explore::pcf, nsim = 19,
#' simulate = null_model)
#'
#' @references
#' Baddeley, A., Rubak, E., Turner, R., 2015. Spatial point patterns: Methodology
#' and applications with R. Chapman and Hall/CRC Press, London, UK. <isbn:978-1-4822-1020-0>
#'
#' Wiegand, T., Moloney, K.A., 2014. Handbook of spatial point-pattern analysis in
#' ecology. Chapman and Hall/CRC Press, Boca Raton, USA. <isbn:978-1-4200-8254-8>
#'
#' @export
rheteroppp <- function(x, nsim, fix_n = FALSE, ...) {

  if (!inherits(x = x, what = "ppp")) {
    stop("Please provide ppp object.", call. = FALSE)
  }

  lambda_xy <- spatstat.explore::density.ppp(x, ...)

  # check if exactly same number of points
  if (fix_n) {

    simulated_pattern <- spatstat.random::rpoint(n = x$n ,
                                          f = lambda_xy,
                                          nsim = nsim)
  }

  else {

    simulated_pattern <- spatstat.random::rpoispp(lambda = lambda_xy,
                                           nsim = nsim)
  }

  for (i in seq_len(nsim)) {
    simulated_pattern[[i]]$window <- x$window
  }

  return(simulated_pattern)
}
