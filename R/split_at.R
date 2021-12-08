#' split_at
#'
#' @description Split vector
#'
#' @param x vector.
#' @param x vector with positions to split.
#'
#' @details
#' Split vector at position(s). Returns a \code{list} with all elements before and
#' after the split position.
#'
#' @return list
#'
#' @examples
#' set.seed(42)
#' x <- sample(x = 1:10, size = 5)
#' split_at(x = x, pos = 3)
#'
#' @aliases split_at
#' @rdname split_at
#'
#' @keywords internal
split_at <- function(x, pos) {

  unname(split(x, cumsum(seq_along(x) %in% pos)))

}
