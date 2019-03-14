#' simulate_heterogenous_pattern
#'
#' @description Simulate heterogenous pattern
#'
#' @param x ppp
#' @param nsim Number of patterns to simulate.
#' @param ... Arguments passed to \code{spatstat::density.ppp()}
#'
#' @details
#' Simulate heterogenous point patterns as null model data for \code{spatstat::envelope()}.
#'
#' @seealso
#' \code{\link{envelope}}
#' \code{\link{density.ppp}}
#'
#' @examples
#' input_pattern <- spatstat::rpoispp(lambda = function(x , y) {100 * exp(-3 * x)}, nsim = 1)
#' null_model <- simulate_heterogenous_pattern(input_pattern, nsim = 19)
#' spatstat::envelope(Y = input_pattern, fun = spatstat::pcf, nsim = 19, simulate = null_model)
#'
#' @aliases simulate_heterogenous_pattern
#' @rdname simulate_heterogenous_pattern

#' @export
simulate_heterogenous_pattern <- function(x, nsim, ...) {

  if(class(x) != "ppp") {
    stop("Please provide ppp object.", call. = FALSE)
  }

  lambda_xy <- spatstat::density.ppp(x, ...)

  simulated_pattern <- spatstat::rpoispp(lambda = lambda_xy,
                                         nsim = nsim)

  for(i in seq_len(nsim)) {
    simulated_pattern[[i]]$window <- x$window
  }

  return(simulated_pattern)
}
