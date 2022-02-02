#' estimate_pcf_fast
#'
#' @description Fast estimation of the pair correlation function
#'
#' @param pattern Point pattern.
#' @param ... Arguments passed down to `Kest` or `pcf.fv`.
#'
#' @details
#' The functions estimates the pair correlation functions based on an estimation
#' of Ripley's K-function. This makes it computationally faster than estimating the
#' pair correlation function directly.
#'
#' It is a wrapper around \code{Kest} and \code{pcf.fv} and returns a 'Function value
#' object' of the \code{spatstat} package.
#'
#' @seealso
#' \code{\link{Kest}} \cr
#' \code{\link{pcf.fv}}
#'
#' @return fv.object
#'
#' @examples
#' set.seed(42)
#' pattern <- spatstat.random::runifpoint(n = 100)
#' pcf_fast <- estimate_pcf_fast(pattern)
#'
#' @references
#' Ripley, B.D., 1977. Modelling spatial patterns. Journal of the Royal Statistical Society.
#' Series B (Methodological) 39, 172–192. <https://doi.org/10.1111/j.2517-6161.1977.tb01615.x>
#'
#' Stoyan, D., Stoyan, H., 1994. Fractals, random shapes and point fields.
#' John Wiley & Sons, Chichester, UK. <isbn:978-0-471-93757-9>
#'
#' @aliases estimate_pcf_fast
#' @rdname estimate_pcf_fast
#'
#' @export
estimate_pcf_fast <- function(pattern, ...){

  k_fun <- suppressMessages(spatstat.core::Kest(X = pattern, ...)) # estimate K-fct

  result <- spatstat.core::pcf.fv(X = k_fun, ...) # estimate pcf from K-fct

  return(result)
}
