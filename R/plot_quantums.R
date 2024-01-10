#' plot_quantums
#'
#' @description Plot simulation envelopes
#'
#' @param input envelope.
#' @param labels Name of the labels. See details for more information.
#' @param color_scale Colors used with labels.
#' @param legend_position The position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param quantum_position Vector with minimum and maximum y value of the quantum bar.
#' @param title Plot title.
#' @param xlab,ylab axis labels.
#' @param line_size Size of the lines.
#' @param base_size Base font size.
#' @param full_fun If true observed value and envelope is plotted.
#' @param quantum If true quantums bars are plotted.
#' @param standarized If true observed value is standardized. See details for more details.
#'
#' @details
#' This functions provides a plotting style for envelope objects of the spatstat
#' package (for more information please see \code{spatstat.explore::envelope}). The location of the
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
#' To adjust the position of the quantum bar, use \code{quantum_position}.
#'
#' Returns a \code{ggplot} object.
#'
#' @return ggplot
#'
#' @seealso
#' \code{\link{envelope}}
#'
#' @examples
#' set.seed(42)
#' pattern <- spatstat.random::rThomas(kappa = 50, scale = 0.025, mu = 5)
#' csr_envelope <- spatstat.explore::envelope(pattern, fun = spatstat.explore::pcf, nsim = 19)
#' plot_quantums(csr_envelope, ylab = "g(r)")
#'
#' @references
#' Esser, D.S., Leveau, J.H.J., Meyer, K.M., Wiegand, K., 2015. Spatial scales of
#' interactions among bacteria and between bacteria and the leaf surface.
#' FEMS Microbiology Ecology 91, 1–13. <https://doi.org/10.1093/femsec/fiu034>
#'
#' @export
plot_quantums <- function(input,
                          labels = NULL, color_scale = NULL,
                          legend_position = "bottom", quantum_position = NULL,
                          title = NULL, xlab = NULL, ylab = NULL,
                          line_size = 0.5, base_size = 15,
                          full_fun = TRUE, quantum = TRUE, standarized = FALSE) {

  if (!is(input, "envelope") && !is(input, "data.frame")) {

    stop("Please provide envelope or data frame.", call. = FALSE)
  }

  if (is.null(labels)) {
    labels <- c("obs < lo", "lo < obs < hi", "obs > hi")
  }

  if (length(labels) !=  3) {
    labels <- c("obs < lo", "lo < obs < hi", "obs > hi")
    warning("Not enough labels provided - using 'obs > hi', 'lo < obs < hi' and 'ob < lo'.",
            call. = FALSE)
  }

  if (is.null(xlab)) {xlab <- "r"}

  if (is.null(ylab)) {ylab <- "f(r)"}

  if (is.null(color_scale)) {
    color_scale <- c("#440154FF", "#238A8DFF", "#FDE725FF")
  }

  names(color_scale) <- labels

  if (is(input, "envelope")) {

    input <- spatstat.explore::as.data.frame.fv(input)

  }

  names(input) <- c("r", "observed", "theoretical", "low", "high")

  if (standarized == TRUE) {
    input$observed <- input$observed - input$theoretical
    input$low <- input$low - input$theoretical
    input$high <- input$high - input$theoretical
    input$theoretical <- input$theoretical - input$theoretical
  }

  # get rid of stats
  input <- input[stats::complete.cases(input), ]

  input$type <- labels[[2]]
  input$type[input$observed < input$low] <- labels[[1]]
  input$type[input$observed > input$high] <- labels[[3]]


  # really needed
  input <- input[!is.na(input$type), ]

  # get id when type changes for rectangles
  change_pos <- rle(input$type)

  # create id col
  input$id <- rep(x = seq(from = 1, to = length(change_pos$lengths)),
                  times = change_pos$lengths)

  # get min/max r value for each rectangle
  input_quantum <- do.call(data.frame, stats::aggregate(x = input$r, by = list(id = input$id),
                                                        FUN = function(x) c(min = min(x), max = max(x))))

  # class classification type
  input_quantum$type <- factor(change_pos$values, levels = labels)

  if (is.null(quantum_position)) {

    ttl_range <- max(c(input$high, input$observed)) - min(c(input$low, input$observed))

    quantum_position <- c(low = min(c(input$low, input$observed)) - (ttl_range * 0.05),
                          high = min(c(input$low, input$observed)))

  }

  if (full_fun == TRUE) {

    if (quantum == TRUE) {

      gg_plot <- ggplot2::ggplot(data = input) +
        ggplot2::geom_rect(data = input_quantum,
                           ggplot2::aes(xmin = x.min, xmax = x.max, ymin = quantum_position[1], ymax = quantum_position[2],
                                        fill = type, color = type)) +
        ggplot2::geom_ribbon(ggplot2::aes(x = r, ymin = low, ymax = high), fill = "grey") +
        ggplot2::geom_line(ggplot2::aes(x = r, y = observed, linetype = "Observed"), linewidth = line_size) +
        ggplot2::geom_line(ggplot2::aes(x = r, y = theoretical, linetype = "Theoretical"), linewidth = line_size) +
        ggplot2::scale_color_manual(name = "", values = color_scale) +
        ggplot2::scale_fill_manual(name = "", values = color_scale) +
        ggplot2::scale_linetype_manual(name = "", values = c(1, 2)) +
        ggplot2::labs(x = xlab, y = ylab, title = title) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(legend.position = legend_position)
    }

    else {
      gg_plot <- ggplot2::ggplot(input) +
        ggplot2::geom_ribbon(ggplot2::aes(x = r, ymin = low, ymax = high), fill = "grey") +
        ggplot2::geom_line(ggplot2::aes(x = r, y = observed, linetype = "Observed"), linewidth = line_size) +
        ggplot2::geom_line(ggplot2::aes(x = r, y = theoretical, linetype = "Theoretical"), linewidth = line_size) +
        ggplot2::scale_linetype_manual(name = "", values = c(1, 2)) +
        ggplot2::labs(x = xlab, y = ylab, title = title) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(legend.position = legend_position)
    }
  }

  else{
    gg_plot <- ggplot2::ggplot(data = input_quantum) +
      ggplot2::geom_rect(ggplot2::aes(fill = type, color = type, xmin = x.min, xmax = x.max,
                                      ymin = quantum_position[1], ymax = quantum_position[2])) +
      ggplot2::coord_cartesian(ylim = c(quantum_position[1] * 0.75, quantum_position[2] * 1.25)) +
      ggplot2::scale_fill_manual(name = "", values = color_scale) +
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
