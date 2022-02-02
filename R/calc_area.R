#' calc_area
#'
#' @description Calculate area of polygon
#'
#' @param x matrix with x,y coordinates.
#'
#' @details
#' Calculate area of polygon in input units. If the polygon is not closed, the first
#' coordinate is used as last coordinate to close it.
#'
#' @return numeric
#'
#' @examples
#' \dontrun{
#' dat <- matrix(data = c(0, 0, 0, 10, 10, 10, 10, 0), ncol = 2, byrow = TRUE)
#' calc_area(x = dat)
#' }
#'
#' @aliases calc_area
#' @rdname calc_area
#'
#' @keywords internal
calc_area <- function(x){

  # check if polygon is closed
  if (x[1, 1] != x[nrow(x), 1] || x[1, 2] != x[nrow(x), 2]) {
    # close the polygon
    x <- rbind(x, x[1, ])
  }

  # get number of rows
  nrow_x <- nrow(x)

  # get coordinates
  x_coord <- x[ ,1]
  y_coord <- x[, 2]

  area <- abs(sum((x_coord[2:nrow_x] - x_coord[1:nrow_x - 1]) *
                (y_coord[2:nrow_x] + y_coord[1:nrow_x - 1])) / 2)

  return(area)
}
