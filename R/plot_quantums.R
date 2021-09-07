#' plot_quantums
#'
#' @description Plot simulation envelopes
#'
#' @param input envelope.
#' @param labels Name of the labels. See details for more information.
#' @param color_scale Colors used with labels.
#' @param legend_position The position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param quantum_position Position of the quantum relative to the simulation envelopes.
#' @param title Plot title.
#' @param xlab,ylab axis labels.
#' @param quantum_size Size of the colour bar.
#' @param line_size Size of the lines.
#' @param base_size Base font size.
#' @param full_fun If true observed value and envelope is plotted.
#' @param quantum If true quantums bars are plotted.
#' @param standarized If true observed value is standardized. See details for more details.
#'
#' @details
#' This functions provides a plotting style for envelope objects of the spatstat
#' package (for more information please see \code{spatstat.core::envelope}). The location of the
#' observed value in relation to the simulation envelope of the null model input is
#' indicated by an additional colour bar at the bottom of the plot. If \code{standarized = TRUE},
#' all values are standarized by subtracting the theoretical value for CSR
#'
#' Labels must be a vector including labels for the following three cases. The color
#' scale vector is used in the same order.
#' \cr 1 = observed > high
#' \cr 2 = low < observed < high
#' \cr 3 = observed < low
#'
#' To adjust the position of the quantum bar, use \code{quantum_position}. Larger values increase
#' the distance from the lower part of the envelope. \code{quantum_position = 0} puts the quantum
#' bar on the minium value of the simulation envelope, negative value shift above that value.
#'
#' Returns a \code{ggplot} object.
#'
#' @return ggplot
#'
#' @seealso
#' \code{\link{envelope}}
#'
#' @references
#' Esser, D.S., Leveau, J.H.J., Meyer, K.M., Wiegand, K., 2015. Spatial scales of
#' interactions among bacteria and between bacteria and the leaf surface.
#' FEMS Microbiology Ecology 91, 1–13. https://doi.org/10.1093/femsec/fiu034
#'
#' @examples
#' set.seed(42)
#' pattern <- spatstat.core::rThomas(kappa = 50, scale = 0.025, mu = 5)
#' csr_envelope <- spatstat.core::envelope(pattern, fun = spatstat.core::pcf, nsim = 19)
#' plot_quantums(csr_envelope, ylab = "g(r)")
#'
#' @aliases plot_quantums
#' @rdname plot_quantums

#' @export
plot_quantums <- function(input,
                          labels = NULL, color_scale = NULL,
                          legend_position = "bottom", quantum_position = 0.05,
                          title = NULL, xlab = NULL, ylab = NULL,
                          quantum_size = 0.1, line_size = 0.5, base_size = 15,
                          full_fun = TRUE, quantum = TRUE, standarized = FALSE) {

  if (!is(input, "envelope") && !is(input, "data.frame")) {

    stop("Please provide envelope or data frame.", call. = FALSE)
  }

  if (is.null(labels)) {
    labels <- c("obs > hi", "lo < obs < hi", "obs < lo")
  }

  if (length(labels) !=  3) {
    labels <- c("obs > hi", "lo < obs < hi", "obs < lo")
    warning("Not enough labels provided - using 'obs > hi', 'lo < obs < hi' and 'ob < lo'.",
            call. = FALSE)
  }

  if (is.null(xlab)) {xlab <- "r"}

  if (is.null(ylab)) {ylab <- "f(r)"}

  if (is.null(color_scale)) {
    color_scale <- c("#440154FF",
                     "#238A8DFF",
                     "#FDE725FF")
  }

  names(color_scale) <- labels

  if (is(input, "envelope")) {

    input <- spatstat.core::as.data.frame.fv(input)
  }

  names(input) <- c("r", "observed", "theoretical", "low", "high")

  if (standarized == TRUE) {
    input$observed <- input$observed - theoretical
    input$low <- input$low - theoretical
    input$high <- input$high - theoretical
    input$theoretical <- input$theoretical - theoretical
  }

  # get rid of stats
  input <- input[stats::complete.cases(input), ]

  input$type <- labels[[2]]
  input$type[input$observed > input$high] <- labels[[1]]
  input$type[input$observed < input$low] <- labels[[3]]

  # really needed
  input <- input[!is.na(input$type), ]

  if (full_fun == TRUE) {

    if (quantum == TRUE) {

      y_quantum <- min(c(input$low, input$observed)) - quantum_size * 1.5

      gg_plot <- ggplot2::ggplot(input) +
        ggplot2::geom_ribbon(ggplot2::aes(x = r, ymin = low, ymax = high),
                             fill = "grey") +
        ggplot2::geom_line(ggplot2::aes(x = r, y = observed, linetype = "Observed"),
                           size = line_size) +
        ggplot2::geom_line(ggplot2::aes(x = r, y = theoretical, linetype = "Theoretical"),
                           size = line_size) +
        ggplot2::geom_linerange(ggplot2::aes(x = r, colour = type,
                                             ymin = y_quantum - quantum_size,
                                             ymax = y_quantum + quantum_size)) +
        ggplot2::scale_color_manual(name = "", values = color_scale) +
        ggplot2::scale_linetype_manual(name = "", values = c(1, 2)) +
        ggplot2::labs(x = xlab, y = ylab, title = title) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(legend.position = legend_position)
    }

    else {
      gg_plot <- ggplot2::ggplot(input) +
        ggplot2::geom_ribbon(ggplot2::aes(x = r, ymin = low, ymax = high), fill = "grey") +
        ggplot2::geom_line(ggplot2::aes(x = r, y = observed, linetype = "Observed"), size = line_size) +
        ggplot2::geom_line(ggplot2::aes(x = r, y = theoretical, linetype = "Theoretical"), size = line_size) +
        ggplot2::scale_linetype_manual(name = "", values = c(1, 2)) +
        ggplot2::labs(x = xlab, y = ylab, title = title) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(legend.position = legend_position)
    }
  }

  else{
    gg_plot <- ggplot2::ggplot(input) +
      ggplot2::geom_linerange(ggplot2::aes(x = r, colour = type,
                                           ymin = 0 - quantum_size,
                                           ymax = 0 + quantum_size)) +
      ggplot2::coord_cartesian(ylim = c(0 - quantum_size * 1.25,
                                        0 + quantum_size * 1.25)) +
      ggplot2::scale_color_manual(name = "", values = color_scale) +
      ggplot2::labs(x = xlab, y = "", title = title) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(legend.position = legend_position,
                     strip.background = ggplot2::element_blank(),
                     axis.line.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  }

  return(gg_plot)
}
