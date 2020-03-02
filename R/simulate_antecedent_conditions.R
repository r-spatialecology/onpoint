#' simulate_antecedent_conditions
#'
#' @description Simulate heterogenous pattern
#'
#' @param x ppp
#' @param i Mark of points that are randomized.
#' @param j Mark of points that do not change.
#' @param nsim Number of patterns to simulate.
#' @param heterogenous If TRUE, points with the mark i are randomized using a heterogeneous
#' Poisson process.
#' @param ... Arguments passed to \code{spatstat::density.ppp()}.
#'
#' @details
#' Simulate point patterns as null model data for \code{spatstat::envelope()} using
#' antecendet conditions as null model. \code{x} must be marked point pattern.
#' Antecedent conditions are suitable as a null model if points of type j may influence
#' points of type i, but not the other way around (Velazquez et al 2016). One example are
#' the positions of seedlings that may be influenced by the position of mature trees.
#'
#' @seealso
#' \code{\link{envelope}}
#'
#' @references
#' Velazquez, E. et al. 2016. An evaluation of the state of spatial point pattern analysis in ecology. - Ecography (Cop.). 39: 1-14.
#'
#' Wiegand, T. and Moloney, K. A. 2004. Rings, circles, and null models for point pattern analysis in ecology. - Oikos 104: 209-229.
#'
#' @examples
#' set.seed(42)
#' pattern_a <- spatstat::runifpoint(n = 20)
#' spatstat::marks(pattern_a) <- "a"
#' pattern_b <- spatstat::runifpoint(n = 100)
#' spatstat::marks(pattern_b) <- "b"
#' pattern <- spatstat::superimpose(pattern_a, pattern_b)
#'
#' null_model <- simulate_antecedent_conditions(x = pattern, i = "b", j = "a", nsim = 19)
#' spatstat::envelope(Y = pattern, fun = spatstat::pcf, nsim = 19, simulate = null_model)
#'
#' @aliases simulate_antecedent_conditions
#' @rdname simulate_antecedent_conditions

#' @export
simulate_antecedent_conditions <- function(x, i, j, nsim, heterogenous = FALSE, ...) {


  # check if pattern ist marked
  if (!spatstat::is.marked(x)) {
    stop("Please provide marked point pattern.", call. = FALSE)
  }

  # check if more than 2 types are present
  if (length(unique(spatstat::marks(x))) > 2) {
    stop("Currently only bivariate point patterns are supported.", call. = FALSE)
  }

  # check if i and j are valid marks
  if (!all(unique(spatstat::marks(x)) %in% c(i, j))) {
    stop("i and j must be marks of x.", call. = FALSE)
  }

  # only points with mark j
  pattern_j <- spatstat::subset.ppp(x, marks == j, drop = TRUE)

  # only points with mark i
  pattern_i <- spatstat::subset.ppp(x, marks == i, drop = TRUE)

  if (heterogenous) {

    lambda_xy <- spatstat::density.ppp(pattern_i, ...)
  }

  # create nsim patterns
  result <- lapply(seq_len(length.out = nsim), function(current_nsim) {

    if (!heterogenous) {

      # random pattern i
      random_i <- spatstat::rpoint(n = pattern_i$n,
                                   win = pattern_i$window)
    }

    else {

      # random pattern i
      random_i <- spatstat::rpoint(n = pattern_i$n,
                                   f = lambda_xy,
                                   win = x$window)
    }

    # assign same marks again
    spatstat::marks(random_i) <- spatstat::marks(pattern_i)

    # combine random i with original j
    pattern_combined <- spatstat::superimpose(j = pattern_j,
                                              i = random_i,
                                              W = pattern_j$window)

    # select only original marks
    spatstat::subset.ppp(pattern_combined, select = "origMarks")

  })

  return(result)
}
