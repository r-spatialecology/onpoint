#' summarize_envelope
#'
#' @description Summarize simulation envelope
#'
#' @param x fv
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
#' The returned \code{env_summarized} object includes information about the area
#' under the curve where the summary function observed pattern is above or below
#' the null model envelopes.
#'
#' @return env_summarized
#'
#' @seealso
#' \code{\link{envelope}}
#'
#' @examples
#' set.seed(42)
#' input_pattern <- spatstat.random::rThomas(kappa = 15, scale = 0.05, mu = 5)
#'
#' cluster_env <- spatstat.explore::envelope(input_pattern, fun = "pcf", nsim = 39,
#' funargs = list(divisor = "d", correction = "Ripley", stoyan = 0.25))
#'
#' summarize_envelope(cluster_env)
#'
#' @export
summarize_envelope <- function(x, plot_result = FALSE) {

  if (inherits(x = x, what = "envelope")) {

    x <- spatstat.explore::as.data.frame.fv(x)
  }

  else if (!inherits(x = x, what = "data.frame")) {

    stop("Please provide envelope object or data frame.", call. = FALSE)
  }

  if (!all(names(x) %in% c("r", "obs", "theo", "lo", "hi"))) {

    stop("Data frame must have columns: 'r', 'obs', 'theo', 'lo', 'hi'.", call. = FALSE)
  }

  # get rid of inf values
  x <- x[stats::complete.cases(x) ,]

  # area above envelope #
  # which obs values are above envelope
  above_envelope <- which(x$obs > x$hi)

  # no values above envelope
  if (length(above_envelope) == 0) {

    area_above <- 0
  }

  # only one valure r above envelope
  else if (length(above_envelope) == 1) {

    # matrix for coords area above
    area_above_poly_temp <- matrix(data = NA, nrow = 3, ncol = 2)

    # first or last value r above envelope
    if (above_envelope == 1 | above_envelope == length(x$r)) {

      # first value r above envelope
      if (above_envelope == 1) {

        pos <- 1
      }

      # last value r above envelope
      else {

        pos <- -1
      }

      # create polygon using three coordinates
      area_above_poly_temp[1, 1] <- x$r[above_envelope]
      area_above_poly_temp[1, 2] <- min(x$obs[above_envelope],
                                        x$hi[above_envelope])

      area_above_poly_temp[3, 1] <- x$r[above_envelope + pos]
      area_above_poly_temp[3, 2] <- max(x$obs[above_envelope + pos],
                                        x$hi[above_envelope + pos])

      area_above_poly_temp[2, 1] <- x$r[above_envelope]
      area_above_poly_temp[2, 2] <- max(x$obs[above_envelope],
                                        x$hi[above_envelope])
    }

    # any value r above envelope
    else {

      # create polygon using three coordinates
      area_above_poly_temp[1, 1] <- x$r[above_envelope - 1]
      area_above_poly_temp[1, 2] <- max(x$obs[above_envelope - 1],
                                        x$hi[above_envelope - 1])

      area_above_poly_temp[2, 1] <- x$r[above_envelope]
      area_above_poly_temp[2, 2] <- max(x$obs[above_envelope],
                                        x$hi[above_envelope])

      area_above_poly_temp[3, 1] <- x$r[above_envelope + 1]
      area_above_poly_temp[3, 2] <- max(x$obs[above_envelope + 1],
                                        x$hi[above_envelope + 1])
    }

    # get area
    area_above <- calc_area(area_above_poly_temp)

    # get polygon coords
    poly_above <- data.frame(x = area_above_poly_temp[, 1],
                             y = area_above_poly_temp[, 2],
                             id = 1,
                             type = "Area above")
  }

  # more than one scale above envelope
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
    area_above_pos <- split_at(x = above_envelope,
                               pos = which(above_next_polygon == TRUE))

    # init vector for area
    area_above <- vector(mode = "numeric", length = length(area_above_pos))

    # init vector for area
    poly_above <- vector(mode = "list", length = length(area_above_pos))

    # loop through number of polygons
    for (i in 1:length(area_above)) {

      if (length(area_above_pos[[i]]) > 1) {

        # matrix for coords area above
        area_above_temp <- matrix(data = NA, nrow = length(area_above_pos[[i]]),
                                  ncol = 2)

        # coordinates of polygon above
        for (j in 1:nrow(area_above_temp)) {

          area_above_temp[j, 1] <- min(x$obs[area_above_pos[[i]]][j],
                                       x$hi[area_above_pos[[i]]][j])

          area_above_temp[j, 2] <- max(x$obs[area_above_pos[[i]]][j],
                                       x$hi[area_above_pos[[i]]][j])
        }

        # convert to matrix with xy coords
        area_above_poly_temp <- matrix(data = c(x$r[area_above_pos[[i]]], rev(x$r[area_above_pos[[i]]]),
                                                area_above_temp[, 1], rev(area_above_temp[, 2])),
                                       ncol = 2)

        # get area
        area_above[i] <- calc_area(area_above_poly_temp)

        # get polygon coords
        poly_above[[i]] <- data.frame(x = area_above_poly_temp[,1],
                                      y = area_above_poly_temp[,2],
                                      id = i,
                                      type = "Area above")
      }

      else {

        # matrix for coords area above
        area_above_poly_temp <- matrix(data = NA, nrow = 3, ncol = 2)

        # first or last value r above envelope
        if (area_above_pos[[i]] == 1 | area_above_pos[[i]] == length(x$r)) {

          # first value r above envelope
          if (area_above_pos[[i]] == 1) {

            pos <- 1
          }

          # last value r above envelope
          else {

            pos <- -1
          }

          # create polygon using three coordinates
          area_above_poly_temp[1, 1] <- x$r[area_above_pos[[i]]]
          area_above_poly_temp[1, 2] <- min(x$obs[area_above_pos[[i]]],
                                            x$hi[area_above_pos[[i]]])

          area_above_poly_temp[3, 1] <- x$r[area_above_pos[[i]] + pos]
          area_above_poly_temp[3, 2] <- max(x$obs[area_above_pos[[i]] + pos],
                                            x$hi[area_above_pos[[i]] + pos])

          area_above_poly_temp[2, 1] <- x$r[area_above_pos[[i]]]
          area_above_poly_temp[2, 2] <- max(x$obs[area_above_pos[[i]]],
                                            x$hi[area_above_pos[[i]]])

        }

        # any value r above envelope
        else {

          # create polygon using three coordinates
          area_above_poly_temp[1, 1] <- x$r[area_above_pos[[i]] - 1]
          area_above_poly_temp[1, 2] <- max(x$obs[area_above_pos[[i]] - 1],
                                            x$hi[area_above_pos[[i]] - 1])

          area_above_poly_temp[2, 1] <- x$r[area_above_pos[[i]]]
          area_above_poly_temp[2, 2] <- max(x$obs[area_above_pos[[i]]],
                                            x$hi[area_above_pos[[i]]])

          area_above_poly_temp[3, 1] <- x$r[area_above_pos[[i]] + 1]
          area_above_poly_temp[3, 2] <- max(x$obs[area_above_pos[[i]] + 1],
                                            x$hi[area_above_pos[[i]] + 1])
        }

        # get area
        area_above[i] <- calc_area(area_above_poly_temp)

        poly_above[[i]] <- data.frame(x = area_above_poly_temp[, 1],
                                      y = area_above_poly_temp[, 2],
                                      id = i,
                                      type = "Area above")
      }
    }
  }

  # area below envelope #
  # which obs values are below envelope
  below_envelope <- which(x$obs < x$lo)

  # no area below
  if (length(below_envelope) == 0) {

    area_below <- 0
  }

  # only one scale below envelope
  else if (length(below_envelope) == 1) {

    # matrix for coords area below
    area_below_poly_temp <- matrix(data = NA, nrow = 3, ncol = 2)

    # first or last value r below envelope
    if (below_envelope == 1 | below_envelope == length(x$r)) {

      # first value r below envelope
      if (below_envelope == 1) {

        pos <- 1
      }

      # last value r below envelope
      else {

        pos <- -1
      }

      # create polygon using three coordinates
      area_below_poly_temp[1, 1] <- x$r[below_envelope]
      area_below_poly_temp[1, 2] <- min(x$obs[below_envelope],
                                        x$lo[below_envelope])

      area_below_poly_temp[3, 1] <- x$r[below_envelope + pos]
      area_below_poly_temp[3, 2] <- min(x$obs[below_envelope + pos],
                                        x$lo[below_envelope + pos])

      area_below_poly_temp[2, 1] <- x$r[below_envelope]
      area_below_poly_temp[2, 2] <- max(x$obs[below_envelope],
                                        x$lo[below_envelope])
    }

    # any value r below envelope
    else {

      # create polygon using three coordinates
      area_below_poly_temp[1, 1] <- x$r[below_envelope - 1]
      area_below_poly_temp[1, 2] <- min(x$obs[below_envelope - 1],
                                        x$lo[below_envelope - 1])

      area_below_poly_temp[2, 1] <- x$r[below_envelope]
      area_below_poly_temp[2, 2] <- min(x$obs[below_envelope],
                                        x$lo[below_envelope])

      area_below_poly_temp[3, 1] <- x$r[below_envelope + 1]
      area_below_poly_temp[3, 2] <- min(x$obs[below_envelope + 1],
                                        x$lo[below_envelope + 1])
    }

    # get area
    area_below <- calc_area(area_below_poly_temp)

    # get polygon coords
    poly_below <- data.frame(x = area_below_poly_temp[,1],
                             y = area_below_poly_temp[,2],
                             id = 1,
                             type = "Area below")
  }

  # more than one value r below envelope
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
    area_below_pos <- split_at(x = below_envelope,
                               pos = which(below_next_polygon == TRUE))

    # init vector for area
    area_below <- vector(mode = "numeric", length = length(area_below_pos))

    # init list for poly coords
    poly_below <- vector(mode = "list", length = length(area_below_pos))

    # loop through number of polygons
    for (i in 1:length(area_below)) {

      if (length(area_below_pos[[i]]) > 1) {

        # matrix for coords area below
        area_below_temp <- matrix(data = NA, nrow = length(area_below_pos[[i]]),
                                  ncol = 2)

        # coordinates of polygon below
        for (j in 1:nrow(area_below_temp)) {

          area_below_temp[j, 1] <- min(x$obs[area_below_pos[[i]]][j],
                                       x$lo[area_below_pos[[i]]][j])

          area_below_temp[j, 2] <- max(x$obs[area_below_pos[[i]]][j],
                                       x$lo[area_below_pos[[i]]][j])
        }

        # convert to matrix with xy coords
        area_below_poly_temp <- matrix(data = c(x$r[area_below_pos[[i]]], rev(x$r[area_below_pos[[i]]]),
                                                area_below_temp[, 1], rev(area_below_temp[, 2])),
                                       ncol = 2)

        # get area
        area_below[i] <- calc_area(area_below_poly_temp)

        # get polygon coords
        poly_below[[i]] <- data.frame(x = area_below_poly_temp[,1],
                                      y = area_below_poly_temp[,2],
                                      id = i,
                                      type = "Area below")
      }

      else {
        # matrix for coords area below
        area_below_poly_temp <- matrix(data = NA, nrow = 3, ncol = 2)

        # first or last value r below envelope
        if (area_below_pos[[i]] == 1 | area_below_pos[[i]] == length(x$r)) {

          # first value r below envelope
          if (area_below_pos[[i]] == 1) {

            pos <- 1
          }

          # last value r below envelope
          else {

            pos <- -1
          }

          # create polygon using three coordinates
          area_below_poly_temp[1, 1] <- x$r[area_below_pos[[i]]]
          area_below_poly_temp[1, 2] <- min(x$obs[area_below_pos[[i]]],
                                            x$hi[area_below_pos[[i]]])

          area_below_poly_temp[3, 1] <- x$r[area_below_pos[[i]] + pos]
          area_below_poly_temp[3, 2] <- max(x$obs[area_below_pos[[i]] + pos],
                                            x$hi[area_below_pos[[i]] + pos])

          area_below_poly_temp[2, 1] <- x$r[area_below_pos[[i]]]
          area_below_poly_temp[2, 2] <- max(x$obs[area_below_pos[[i]]],
                                            x$hi[area_below_pos[[i]]])

        }

        # any value r below envelope
        else {

          # create polygon using three coordinates
          area_below_poly_temp[1, 1] <- x$r[area_below_pos[[i]] - 1]
          area_below_poly_temp[1, 2] <- max(x$obs[area_below_pos[[i]] - 1],
                                            x$hi[area_below_pos[[i]] - 1])

          area_below_poly_temp[2, 1] <- x$r[area_below_pos[[i]]]
          area_below_poly_temp[2, 2] <- max(x$obs[area_below_pos[[i]]],
                                            x$hi[area_below_pos[[i]]])

          area_below_poly_temp[3, 1] <- x$r[area_below_pos[[i]] + 1]
          area_below_poly_temp[3, 2] <- max(x$obs[area_below_pos[[i]] + 1],
                                            x$hi[area_below_pos[[i]] + 1])
        }

        # get area
        area_below_pos[i] <- calc_area(area_below_poly_temp)

        # get polygon coords
        poly_below[[i]] <- data.frame(x = area_below_poly_temp[, 1],
                                      y = area_below_poly_temp[, 2],
                                      id = i,
                                      type = "Area below")
      }
    }
  }

  # total area #
  # get polygon of total area under curve
  area_total_poly <- matrix(data = c(x$r, rev(x$r),
                                     rep(0, times = length(x$r)),
                                     rev(x$obs)), ncol = 2)

  # calc area
  area_total <- calc_area(x = area_total_poly)

  # result #
  # seperated by above/below
  result_seperated <- c(sum(area_above) / sum(area_total) * 100,
                        -sum(area_below) / sum(area_total) * 100)

  # get ratio of area outside to total area
  result_total <- (sum(area_above) + sum(area_below)) / sum(area_total) * 100

  if (sum(area_below) > sum(area_above)) {
    result_total <- -result_total
  }

  # check if area above poly is present
  if (!exists("poly_above")) {
    poly_above <- data.frame(x = NA, y = NA, id = NA, type = "Area above")
  }

  # check if area below poly is present
  if (!exists("poly_below")) {
    poly_below <- data.frame(x = NA, y = NA, id = NA, type = "Area below")
  }

  # bind list to dataframe
  if (inherits(x = poly_above, what = "list")) {
    poly_above <- do.call(rbind, poly_above)
  }

  # bind list to dataframe
  if (inherits(x = poly_below, what = "list")) {
    poly_below <- do.call(rbind, poly_below)
  }

  # combine to one dataframe
  area_poly <- rbind(poly_above, poly_below)

  result <- list(area_above = area_above,
                 area_below = area_below,
                 area_total = area_total,
                 result_above = result_seperated[1],
                 result_below = result_seperated[2],
                 result_total = result_total,
                 internal = list(envelope = x,
                                 area_poly = area_poly))

  class(result) <- "env_summarized"

  # plot result
  if (plot_result) {

    print(plot.env_summarized(result))
  }

  return(result)
}
