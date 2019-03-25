#' simulate_antecedent_conditions
#'
#' @description Simulate heterogenous pattern
#'
#' @param x ppp
#' @param i mark of points that are randomized.
#' @param j mark of points that do not change.
#' @param nsim Number of patterns to simulate.
#'
#' @details
#' Simulate point patterns as null model data for \code{spatstat::envelope()} using
#' antecendet conditions as null model. \code{x} must be marked point pattern.
#'
#' @seealso
#' \code{\link{envelope}}
#'
#' @examples
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
simulate_antecedent_conditions <- function(x, i, j, nsim) {


  # check if pattern ist marked
  if(!spatstat::is.marked(x)) {
    stop("Please provide marked point pattern.", call. = FALSE)
  }

  # only points with mark j
  pattern_j <- spatstat::subset.ppp(x, marks == j, drop = TRUE)

  # only points with mark i
  pattern_i <- spatstat::subset.ppp(x, marks == i, drop = TRUE)

  # create nsim patterns
  result <- lapply(seq_len(length.out = nsim), function(current_nsim) {

    # random pattern i
    random_i <- spatstat::rpoint(n = pattern_i$n,
                                 win = pattern_i$window)

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
