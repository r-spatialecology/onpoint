#' summarise_envelope
#'
#' @description Summarise a simulation envelope
#'
#' @param x fv
#' @param seperated If TRUE one value for the relative area above and one for
#' the relative area below the envelope are returned.
#' @param plot_result A plot is drawn.
#'
#' @details
#' Summarise a simulation envelope. The area above and below the null model
#' envelope is divided by the area where the observed value is within the envelope.
#' If \code{seperated = TRUE}, the first returning value is the relative area above,
#' the second value the relative value below the envelope.
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
#' summarise_envelope(cluster_env, seperated = FALSE, plot = TRUE)
#'
#' @aliases summarise_envelope
#' @rdname summarise_envelope

#' @export
summarise_envelope <- function(x, seperated = TRUE, plot_result = FALSE) {

  # check if class is envelope
  if (!any(class(x) == "envelope")) {
    stop("Please provide an envelope object.")
  }

  # base plot including envelope and obs
  if (plot_result) {

    ggplot_result <- ggplot2::ggplot(data = x) +
      ggplot2::geom_ribbon(ggplot2::aes(x = r, ymin = lo, ymax = hi),
                           fill = "grey") +
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

    # matrix for coords of area above
    above_area <- matrix(data = NA, nrow = length(above_envelope), ncol = 2)

    # coordinates of polygon above
    for (i in 1:nrow(above_area)) {

      above_area[i, 1] <- min(x$obs[above_envelope][i], x$hi[above_envelope][i])

      above_area[i, 2] <- max(x$obs[above_envelope][i], x$hi[above_envelope][i])
    }

    # convert to matrix with xy coords
    above_area_poly <- matrix(data = c(x$r[above_envelope], rev(x$r[above_envelope]),
                                       above_area[, 1], rev(above_area[, 2])),
                              ncol = 2)

    # get area
    above_area <- calc_area(above_area_poly)

    # add polygon to plot
    if (plot_result) {
      ggplot_result <- ggplot_result +
        ggplot2::geom_polygon(data = as.data.frame(above_area_poly),
                              ggplot2::aes(x = V1,
                                           y = V2),
                              fill = "#0D0887FF", alpha = 0.25)
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

    # matrix for coords area above
    below_area <- matrix(data = NA, nrow = length(below_envelope), ncol = 2)

    # coordinates of polygon above
    for (i in 1:nrow(below_area)) {

      below_area[i, 1] <- min(x$obs[below_envelope][i], x$lo[below_envelope][i])

      below_area[i, 2] <- max(x$obs[below_envelope][i], x$lo[below_envelope][i])
    }

    # convert to matrix with xy coords
    below_area_poly <- matrix(data = c(x$r[below_envelope], rev(x$r[below_envelope]),
                                       below_area[, 1], rev(below_area[, 2])),
                         ncol = 2)

    # get area
    below_area <- calc_area(below_area_poly)

    # add polygon below
    if (plot_result) {
      ggplot_result <- ggplot_result +
        ggplot2::geom_polygon(data = as.data.frame(below_area_poly),
                              ggplot2::aes(x = V1,
                                           y = V2),
                              fill = "#CC4678FF", alpha = 0.25)
    }
  }

  # area between envelope #
  # which obs values are above envelope
  between_envelope <- which(x$obs <= x$hi & x$obs >= x$lo)

  # no area between envelope
  if (length(between_envelope) == 0) {

    between_area <- 0
  }

  else {

    # vector for position at which new polygon is needed
    next_polygon <- vector(mode = "logical", length = length(between_envelope))

    # check at which position jump to next polygon
    for (i in 1:(length(between_envelope) - 1)) {

      # next value
      j <- i + 1

      # if difference between current value and next value is larger than 1 a new
      # polygon is needed
      if (between_envelope[j] != (between_envelope[i] + 1)) {

        next_polygon[j] <- TRUE
      }
    }

    # split position into seperated polygons
    between_area_pos <- split_at(x = between_envelope,
                                 pos = which(next_polygon == TRUE))

    # init vector for area
    between_area <- vector(mode = "numeric", length = length(between_area_pos))

    # loop through number of polygons
    for (i in 1:length(between_area)) {

      # matrix for coords area above
      between_area_temp <- matrix(data = NA, nrow = length(between_area_pos[[i]]),
                                  ncol = 2)

      # coordinates of polygon between
      for (j in 1:nrow(between_area_temp)) {

        between_area_temp[j, 1] <- min(x$obs[between_area_pos[[i]]][j],
                                       x$hi[between_area_pos[[i]]][j],
                                       x$lo[between_area_pos[[i]]][j])

        between_area_temp[j, 2] <- max(x$obs[between_area_pos[[i]]][j],
                                       x$hi[between_area_pos[[i]]][j],
                                       x$lo[between_area_pos[[i]]][j])
      }

      # convert to matrix with xy coords
      between_area_poly_temp <- matrix(data = c(x$r[between_area_pos[[i]]], rev(x$r[between_area_pos[[i]]]),
                                           between_area_temp[, 1], rev(between_area_temp[, 2])),
                           ncol = 2)

      # get area
      between_area[i] <- calc_area(between_area_poly_temp)

      # add polygon to plot
      if (plot_result) {
        ggplot_result <- ggplot_result +
          ggplot2::geom_polygon(data = as.data.frame(between_area_poly_temp),
                                ggplot2::aes(x = V1,
                                             y = V2),
                                fill = "#F0F921FF", alpha = 0.25)
      }
    }
  }

  # # total area #
  # # matrix for area above
  # area_total <- vector(mode = "numeric", length = length(x$r))
  #
  # # coordinates of polygon above
  # for (i in 1:length(area_total)) {
  #
  #   area_total[i] <- max(x$obs[i], x$lo[i], x$hi[i])
  # }
  #
  # area_total <- matrix(data = c(x$r, rev(x$r),
  #                               rep(0, times = length(x$r)),
  #                               rev(area_total)), ncol = 2)
  #
  # area_total <- calc_area(area_total)

  # total result #
  # seperated by above/below
  if (seperated) {

    result <- c(above_area / sum(between_area) * 100,
                -below_area / sum(between_area) * 100)
  }

  # one value
  else {

    # get ratio of area outside to total area
    result <- (above_area + below_area) / sum(between_area) * 100
  }

  # plot result
  if (plot_result) {

    print(ggplot_result)
  }

  return(result)
}
