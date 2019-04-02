#' balance_points
#'
#' @description Balance number of points
#'
#' @param pattern ppp object.
#' @param n Either an integer or a ppp object.
#' @param verbose Print messages.
#'
#' @details
#' The function balances out the number of points in the input pattern to either
#' the provided number of points as integer or the same number of points if a ppp
#' object is provided.
#'
#' @return ppp
#'
#' @examples
#' set.seed(42)
#' input <- spatstat::rpoispp(lambda = 100)
#' input_b <- spatstat::rpoispp(lambda = 100)
#'
#' balance_points(pattern = input, n = 110)
#' balance_points(pattern = input, n = input_b)

#'
#' @aliases balance_points
#' @rdname balance_points

#' @export
balance_points <- function(pattern, n, verbose = TRUE) {

  # check if n is valid
  if(!spatstat::is.ppp(n)) {
    if (n %% 1 != 0) {
    stop("n must be either integer or ppp.", call. = FALSE)
    }
  }

  # if n is pattern - get number of points
  if(spatstat::is.ppp(n)) {
    n <- n$n
  }

  if(verbose) {

    difference_rel <- abs((pattern$n - n)) / pattern$n

    message("> Relative difference between pattern and n: ", round(difference_rel, 2))

    if(difference_rel > 0.33) {
      warning("Differences between pattern and n more than 0.33.", call. = FALSE)
    }
  }

  abs((pattern$n - n))

  # remove points because more points in simulated
  if(pattern$n > n) {

    # difference between patterns
    difference <- pattern$n - n

    # id of points to remove
    remove_points <- sample(x = seq_len(pattern$n), size = difference)

    # remove points
    pattern <- pattern[-remove_points]
  }

  # add points because less points in simulated
  else if(pattern$n < n) {

    # difference between patterns
    difference <- n - pattern$n

    # create missing points
    missing_points <- spatstat::runifpoint(n = difference,
                                           win = pattern$window,
                                           nsim = 1, drop = TRUE)

    # add missing points to simulated
    pattern <- spatstat::superimpose(pattern, missing_points,
                                     W = pattern$window)
  }

  return(pattern)
}
