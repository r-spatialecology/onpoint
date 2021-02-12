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
#' pair correlation function directly. It is a wrapper around `Kest` and `pcf.fv`.
#'
#' @seealso
#' \code{\link{Kest}} \cr
#' \code{\link{pcf.fv}}
#'
#' @return fv.object
#'
#' @examples
#' set.seed(42)
#' pattern <- spatstat.core::runifpoint(n = 100)
#' pcf_fast <- estimate_pcf_fast(pattern)
#'
#' @aliases estimate_pcf_fast
#' @rdname estimate_pcf_fast
#'
#' @references
#' Ripley, B. D. 1977. Modelling spatial patterns. - J. R. Stat. Soc. Ser. B 39: 172-192.
#'
#' Stoyan, D. and Stoyan, H. 1994. Fractals, random shapes and point fields. - John Wiley & Sons.

#' @export
estimate_pcf_fast <- function(pattern, ...){

  k_fun <- suppressMessages(spatstat.core::Kest(X = pattern, ...)) # estimate K-fct

  result <- spatstat.core::pcf.fv(X = k_fun, ...) # estimate pcf from K-fct

  return(result)
}
