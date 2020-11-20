#' split_at
#'
#' @description Split vector
#'
#' @param x vector.
#' @param x vector with positions to split.
#'
#' @details
#' Split vector at position(s).
#'
#' @aliases split_at
#' @rdname split_at
#'
#' @keywords internal
#'
#' @export
split_at <- function(x, pos) {

  unname(split(x, cumsum(seq_along(x) %in% pos)))
}
