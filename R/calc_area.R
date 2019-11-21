#' calc_area
#'
#' @description Calculate area of polygon
#'
#' @param x matrix
#'
#' @details
#' Calculate area of polygon
#'
#' @aliases calc_area
#' @rdname calc_area
#'
#' @keywords internal
#'
#' @export
calc_area <- function(x){

  # close the polygon
  x <- rbind(x, x[1, ])

  # get number of rows
  nrow_x <- nrow(x)

  # get coordinates
  x_coord <- x[ ,1]
  y_coord <- x[, 2]

  area <- abs(sum((x_coord[2:nrow_x] - x_coord[1:nrow_x - 1]) *
                (y_coord[2:nrow_x] + y_coord[1:nrow_x - 1])) / 2)

  return(area)
}
