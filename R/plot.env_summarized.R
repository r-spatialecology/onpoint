#' plot.env_summarized
#'
#' @description Plotting method for \code{env_summarized} object
#'
#' @param x Random patterns.
#' @param col Colors for areas above and below envelope.
#' @param x_lab,y_lab Labels of x- and y-axis.
#' @param base_size Base size of plot
#' @param label If TRUE the ratios of the area above and below are added to the plot.
#' @param ... To be generic for plotting function.
#'
#' @details
#' Plotting method for summarized envelope created with \code{\link{summarize_envelope}}.
#'
#' Returns a \code{ggplot} object.
#'
#' @return ggplot
#'
#' @seealso
#' \code{\link{summarize_envelope}}
#'
#' @examples
#' set.seed(42)
#' input_pattern <- spatstat.random::rThomas(kappa = 15, scale = 0.05, mu = 5)
#'
#' cluster_env <- spatstat.explore::envelope(input_pattern, fun = "pcf", nsim = 39,
#' funargs = list(divisor = "d", correction = "Ripley", stoyan = 0.25))
#'
#' x <- summarize_envelope(cluster_env)
#' plot(x)
#'
#' @aliases plot.env_summarized
#' @rdname plot.env_summarized
#'
#' @export
plot.env_summarized <- function(x, col = c("#97CBDE", "#E1B0B5"),
                                x_lab = NULL, y_lab = NULL, base_size = 10,
                                label = TRUE, ...) {

  # check if color for polygons is correct
  if (length(col) != 2) {
    warning("Please provide two colours for the polygons. Setting to default.",
            call. = FALSE)

    col <- c("#97CBDE", "#E1B0B5")
  }

  # check if lab labels are present
  if (is.null(x_lab)) {
    x_lab <- "r [unit]"
  }

  if (is.null(y_lab)) {
    y_lab <- expression(italic(f(r)))
  }

  # get summary function and areas of polygons
  summary_function <- x$internal$envelope
  area_poly <- x$internal$area_poly

  # get ratios above and below
  ratio_above <- round(x$result_above, digits = 2)
  ratio_below <- round(x$result_below, digits = 2)

  # get position of labels
  label_x <- max(summary_function$r) * 0.9
  label_y <- max(summary_function$obs, summary_function$hi) * 0.9

  # get area above and below
  area_poly_above <- area_poly[area_poly$type == "Area above", ]
  area_poly_below <- area_poly[area_poly$type == "Area below", ]

  # create base plot
  ggplot_result <- ggplot2::ggplot(data = summary_function) +
    ggplot2::geom_ribbon(ggplot2::aes(x = r, ymin = lo, ymax = hi,
                                      fill = "Simulation envelope")) +
    ggplot2::geom_polygon(data = data.frame(x = 0, y = 0),
                          ggplot2::aes(x = x, y = y, fill = "Area above")) +
    ggplot2::geom_polygon(data = data.frame(x = 0, y = 0),
                          ggplot2::aes(x = x, y = y, fill = "Area below")) +
    ggplot2::geom_line(ggplot2::aes(x = r, y = obs, linetype = "Observed function")) +
    ggplot2::geom_line(ggplot2::aes(x = r, y = theo, linetype = "Theoretical function")) +
    ggplot2::scale_linetype_manual(name = "", values = c(1, 2)) +
    ggplot2::scale_fill_manual(name = "", values = c("Area above" = col[1],
                                                     "Area below" = col[2],
                                                     "Simulation envelope" = "grey85")) +
    ggplot2::labs(x = x_lab, y = y_lab) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom")

  # if present, add polygons above
  if (!all(is.na(area_poly_above[, c("x", "y", "id")]))) {

    for (i in unique(area_poly_above$id)) {
      ggplot_result <- ggplot_result +
        ggplot2::geom_polygon(data = area_poly_above[area_poly_above$id == i &
                                                       !is.na(area_poly_above$id), ],
                              ggplot2::aes(x = x, y = y, fill = "Area above"))
    }
  }

  # if present, add polygons below
  if (!all(is.na(area_poly_below[, c("x", "y", "id")]))) {

    for (i in unique(area_poly_below$id)) {
      ggplot_result <- ggplot_result +
        ggplot2::geom_polygon(data = area_poly_below[area_poly_below$id == i &
                                                       !is.na(area_poly_below$id), ],
                              ggplot2::aes(x = x, y = y, fill = "Area below"))
    }
  }

  # add label with ratios
  if (label) {
    ggplot_result <- ggplot_result +
      ggplot2::annotate(geom = "label", x = label_x, y = label_y,
                          label = paste0("Area above: ", ratio_above, "%\n",
                                         "Area below: ",ratio_below, "%"))
  }

  # return ggplot object
  return(ggplot_result)
}
