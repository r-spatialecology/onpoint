#' summarise_envelope
#'
#' @description Summarise simulation envelope
#'
#' @param x fv
#' @param seperated If TRUE one value for the relative area above and one for
#' the relative area below the envelope are returned.
#' @param plot_result A plot is drawn.
#'
#' @details
#' The area above and below the null model envelope is divided by the total area
#' under the curve. If \code{seperated = TRUE}, the first returning value is the
#' relative area above, the second value the relative value below the envelope.
#' If \code{seperated = FALSE} the value is the absolute sum of both ratio. If the
#' value is positive, the area above the envelope is larger than the value below
#' the envelope. If the value is negative, the area under the envelope is larger than
#' the value above the envelope.
#'
#' @return vector
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
summarise_envelope <- function(x, seperated = FALSE, plot_result = FALSE) {

  # check if class is envelope
  if (!any(class(x) == "envelope")) {
    stop("Please provide an envelope object.")
  }

  # base plot including envelope and obs
  if (plot_result) {

    ggplot_result <- ggplot2::ggplot(data = x) +
      ggplot2::geom_ribbon(ggplot2::aes(x = r, ymin = lo, ymax = hi),
                           fill = "grey85") +
      ggplot2::geom_line(ggplot2::aes(x = r, y = obs, linetype = "Observed")) +
      ggplot2::geom_line(ggplot2::aes(x = r, y = theo, linetype = "Theoretical")) +
      ggplot2::scale_linetype_manual(name = "", values = c(1, 2)) +
      ggplot2::labs(x = "r [unit]", y = expression(italic(f(r)))) +
      ggplot2::theme_classic()
  }

  # area above envelope #
  # which obs values are above envelope
  above_envelope <- which(x$obs > x$hi)

  # no values above area
  if (length(above_envelope) == 0) {

    above_area <- 0
  }

  else {

    # vector for position at which new polygon is needed
    above_next_polygon <- vector(mode = "logical",
                                 length = length(above_envelope))

    # check at which position jump to next polygon
    for (i in 1:(length(above_envelope) - 1)) {

      # next value
      j <- i + 1

      # if difference between current value and next value is larger than 1 a new
      # polygon is needed
      if (above_envelope[j] != (above_envelope[i] + 1)) {

        above_next_polygon[j] <- TRUE
      }
    }

    # split position into seperated polygons
    above_area_pos <- split_at(x = above_envelope,
                               pos = which(above_next_polygon == TRUE))

    # init vector for area
    above_area <- vector(mode = "numeric", length = length(above_area_pos))

    # loop through number of polygons
    for (i in 1:length(above_area)) {

      # matrix for coords area above
      above_area_temp <- matrix(data = NA, nrow = length(above_area_pos[[i]]),
                                ncol = 2)

      # coordinates of polygon above
      for (j in 1:nrow(above_area_temp)) {

        above_area_temp[j, 1] <- min(x$obs[above_area_pos[[i]]][j],
                                     x$hi[above_area_pos[[i]]][j])

        above_area_temp[j, 2] <- max(x$obs[above_area_pos[[i]]][j],
                                     x$hi[above_area_pos[[i]]][j])
      }

      # convert to matrix with xy coords
      above_area_poly_temp <- matrix(data = c(x$r[above_area_pos[[i]]], rev(x$r[above_area_pos[[i]]]),
                                              above_area_temp[, 1], rev(above_area_temp[, 2])),
                                     ncol = 2)

      # get area
      above_area[i] <- calc_area(above_area_poly_temp)

      # add polygon to plot
      if (plot_result) {
        ggplot_result <- ggplot_result +
          ggplot2::geom_polygon(data = as.data.frame(above_area_poly_temp),
                                ggplot2::aes(x = V1,
                                             y = V2),
                                fill = "#0D0887FF",alpha = 0.5)
      }
    }
  }

  # area below envelope #
  # which obs values are below envelope
  below_envelope <- which(x$obs < x$lo)

  # no area below
  if (length(below_envelope) == 0) {

    below_area <- 0
  }

  else {

    # vector for position at which new polygon is needed
    below_next_polygon <- vector(mode = "logical",
                                 length = length(below_envelope))

    # check at which position jump to next polygon
    for (i in 1:(length(below_envelope) - 1)) {

      # next value
      j <- i + 1

      # if difference between current value and next value is larger than 1 a new
      # polygon is needed
      if (below_envelope[j] != (below_envelope[i] + 1)) {

        below_next_polygon[j] <- TRUE
      }
    }

    # split position into seperated polygons
    below_area_pos <- split_at(x = below_envelope,
                               pos = which(below_next_polygon == TRUE))

    # init vector for area
    below_area <- vector(mode = "numeric", length = length(below_area_pos))

    # loop through number of polygons
    for (i in 1:length(below_area)) {

      # matrix for coords area below
      below_area_temp <- matrix(data = NA, nrow = length(below_area_pos[[i]]),
                                ncol = 2)

      # coordinates of polygon below
      for (j in 1:nrow(below_area_temp)) {

        below_area_temp[j, 1] <- min(x$obs[below_area_pos[[i]]][j],
                                     x$lo[below_area_pos[[i]]][j])

        below_area_temp[j, 2] <- max(x$obs[below_area_pos[[i]]][j],
                                     x$lo[below_area_pos[[i]]][j])
      }

      # convert to matrix with xy coords
      below_area_poly_temp <- matrix(data = c(x$r[below_area_pos[[i]]], rev(x$r[below_area_pos[[i]]]),
                                              below_area_temp[, 1], rev(below_area_temp[, 2])),
                                     ncol = 2)

      # get area
      below_area[i] <- calc_area(below_area_poly_temp)

      # add polygon to plot
      if (plot_result) {
        ggplot_result <- ggplot_result +
          ggplot2::geom_polygon(data = as.data.frame(below_area_poly_temp),
                                ggplot2::aes(x = V1,
                                             y = V2),
                                fill =  "#CC4678FF",alpha = 0.5)
      }
    }
  }

  # total area #
  # get polygon of total area under curve
  area_total_poly <- matrix(data = c(x$r, rev(x$r),
                                     rep(0, times = length(x$r)),
                                     rev(x$obs)), ncol = 2)

  # calc area
  area_total <- calc_area(area_total_poly)

  # result #
  if (seperated) {

    # seperated by above/below
    result <- c(sum(above_area) / sum(area_total) * 100,
                -sum(below_area) / sum(area_total) * 100)
  }

  # one value
  else {

    # get ratio of area outside to total area
    result <- (sum(above_area) + sum(below_area)) / sum(area_total) * 100

    if (sum(below_area) >  sum(above_area)) {
      result <- -result
    }
  }

  # plot result
  if (plot_result) {

    print(ggplot_result)
  }

    return(result)
}
