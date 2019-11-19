#' summarise_envelope
#'
#' @description Summarise a simulation envelope
#'
#' @param x fv
#'
#' @details
#' Summarise a simulation envelope
#'
#' @seealso
#' \code{\link{envelope}}
#'
#' @examples
#' set.seed(42)
#' input_pattern <- spatstat::rThomas(kappa = 15, scale = 0.05, mu = 5)
#'
#' cluster_env <- spatstat::envelope(input_pattern, fun = "pcf", nsim = 39,
#' funargs = list(divisor = "d", correction = "Ripley", stoyan = 0.25))
#'
#' summarise_envelope(cluster_env)
#'
#' @aliases summarise_envelope
#' @rdname summarise_envelope

#' @export
summarise_envelope <- function(x) {

  # check if class is envelope
  if (!any(class(x) == "envelope")) {
    stop("Please provide an envelope object.")
  }

  # which obs values are below envelope
  below_envelope <- which(x$obs < x$lo)

  # which obs values are above envelope
  above_envelope <- which(x$obs > x$hi)

  # obs completely within envelope
  if (length(below_envelope) == 0 && length(above_envelope) == 0) {

    result <- 0
  }

  # obs outside envelope
  else {

    # no obs below envelope
    if (length(below_envelope) == 0) {

      diff_below <- 0
    }

    # some obs below envelope
    else {

      # calculate relative difference
      diff_below <- (x$obs[below_envelope] - x$lo[below_envelope]) /
        x$lo[below_envelope]
    }

    # no obs above envelope
    if (length(above_envelope) == 0) {

      diff_above <- 0
    }

    # some obs above envelope
    else {

      # calculate relative difference
      diff_above <- (x$obs[above_envelope] - x$hi[above_envelope]) /
        x$hi[above_envelope]
    }

    # calculate total difference
    # diff_total <- sum(diff_above) + sum(diff_below)
    result <- mean(diff_above) + mean(diff_below)
  }

  return(result)
}
