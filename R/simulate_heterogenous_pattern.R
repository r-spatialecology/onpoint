#' simulate_heterogenous_pattern
#'
#' @description Simulate heterogenous pattern
#'
#' @param x ppp
#' @param nsim Number of patterns to simulate.
#' @param fix_n Logical if true the null model patterns have exactly the same number of points ais input.
#' @param ... Arguments passed to \code{spatstat.core::density.ppp()}.
#'
#' @details
#' Simulate heterogenous point patterns as null model data for \code{spatstat.core::envelope()}.
#' A heterogenous Poisson process is used, meaning that there are no interaction between points,
#' however, the simulated coordinates depend on the intensity \eqn{\lambda} of the input pattern.
#'
#' @seealso
#' \code{\link{envelope}} \cr
#' \code{\link{density.ppp}}
#'
#' @examples
#' set.seed(42)
#' input_pattern <- spatstat.core::rpoispp(lambda = function(x , y) {100 * exp(-3 * x)}, nsim = 1)
#' null_model <- simulate_heterogenous_pattern(input_pattern, nsim = 19)
#' spatstat.core::envelope(Y = input_pattern, fun = spatstat.core::pcf, nsim = 19,
#' simulate = null_model)
#'
#' @references
#' Baddeley, A. et al. 2015. Spatial Point Patterns: Methodology and Applications with R. - Chapman and Hall/CRC Press.
#'
#' Wiegand, T. and Moloney, K. A. 2014. Handbook of spatial point-pattern analysis in ecology. - Chapman and Hall/CRC Press.
#'
#'
#' @aliases simulate_heterogenous_pattern
#' @rdname simulate_heterogenous_pattern

#' @export
simulate_heterogenous_pattern <- function(x, nsim, fix_n = FALSE, ...) {

  if (class(x) != "ppp") {
    stop("Please provide ppp object.", call. = FALSE)
  }

  lambda_xy <- spatstat.core::density.ppp(x, ...)

  # check if exactly same number of points
  if (fix_n) {

    simulated_pattern <- spatstat.core::rpoint(n = x$n ,
                                          f = lambda_xy,
                                          nsim = nsim)
  }

  else {

    simulated_pattern <- spatstat.core::rpoispp(lambda = lambda_xy,
                                           nsim = nsim)
  }

  for (i in seq_len(nsim)) {
    simulated_pattern[[i]]$window <- x$window
  }

  return(simulated_pattern)
}
