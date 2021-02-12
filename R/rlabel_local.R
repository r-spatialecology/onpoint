#' rlabel_local
#'
#' @description Local random labelling of marked point pattern
#'
#' @param X ppp
#' @param distance Mark of points that do not change.
#' @param nsim Number of patterns to simulate.
#' @param drop If nsim = 1 and drop = TRUE , the result will be a point pattern,
#' rather than a list containing a point pattern.
#' @param verbose If TRUE warning messages are printed.
#'
#' @details
#' Local random labelling function, i.e. marks will be shuffeld only across points
#' within the specified local distance. Technically, this is achived by sampling the
#' mark of a neighbouring point j within the distance d for the focal point i. Thus,
#' the distance d must be selected in a way that each point has at least one neighbour
#' within d.
#'
#' @seealso
#' \code{\link{rlabel}}
#'
#' @references
#' Velazquez, E. et al. 2016. An evaluation of the state of spatial point pattern analysis in ecology. - Ecography (Cop.). 39: 1-14.
#'
#' Wiegand, T., Moloney, K.A., 2014. Handbook of spatial point-pattern analysis in
#' ecology. Chapman and Hall/CRC Press, Boca Raton.
#'
#' @examples
#' set.seed(42)
#' pattern <- spatstat.core::runifpoint(n = 250, win = spatstat.geom::owin(c(0, 100), c(0, 100)))
#' spatstat.geom::marks(pattern) <- runif(n = 250, min = 10, max = 120)
#'
#' rlabel_local(X = pattern, distance = 25, nsim = 19)
#'
#' @aliases rlabel_local
#' @rdname rlabel_local

#' @export
rlabel_local <- function(X, distance, nsim = 19,
                         drop = TRUE, verbose = TRUE) {

  # check if pattern is marked
  if (!spatstat.geom::is.marked(X) |
      !inherits(spatstat.geom::marks(X), what = "numeric")) {

    stop("Please provide pattern with numeric marks.", call. = FALSE)
  }

  # get number of points
  n_points <- X$n

  # get marks
  original_marks <- spatstat.geom::marks(X)

  # create list for nsim
  result <- vector(mode = "list", length = nsim)

  # get distances
  pair_distances <- spatstat.geom::pairdist(X)

  # get all points with no neighbour below distance
  below_distance <- apply(X = pair_distances, MARGIN = 2,
                          FUN = function(x) sum(x < distance & x != 0))

  # some points don't have neighbour at r < distance
  if (any(below_distance == 0)) {
    stop("Not all points have at least one neighbour within the specified distance.",
         call. = FALSE)
  }

  for (i in 1:nsim) {

    # save pattern to exchanges marks without changing original pattern
    X_rlabel <- X

    # vector for sampled marks
    sample_marks <- vector(mode = "numeric", length = n_points)

    # new mark for each point
    for (j in 1:n_points) {

      # all points within distance
      # might be faster not to check for != 0 but remove j (diagonal)
      within_distance <- which(pair_distances[, j] < distance &
                                 pair_distances[, j] != 0, arr.ind = TRUE)

      # all marks within distance
      # Can happen that same same mark is used several times
      sample_marks[j] <- sample(original_marks[within_distance], size = 1)
    }

    # add new marks
    spatstat.geom::marks(X_rlabel) <- sample_marks

    # return list
    result[[i]] <- X_rlabel
  }

  # return only ppp if drop = TRUE
  if (drop) {

    # still return list if nsim > 1 but throw warning
    if (nsim != 1) {

      if (verbose) {
        warning("drop = TRUE only possible for nsim = 1.", call. = FALSE)
      }
    }

    # return only ppp
    else {
      result <- result[[1]]
    }
  }

  return(result)
}
