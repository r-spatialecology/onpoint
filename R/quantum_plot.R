#' quantum_plot
#'
#' @description Plot simulation envelopes
#'
#' @param input envelope.
#' @param labels Name of the labels. See details for more information.
#' @param title Plot title.
#' @param xlab,ylab axis labels.
#' @param size Size of the colour bar.
#' @param full_fun If true observed value and envelope is plotted.
#' @param standarized If true observed value is standardized. See details for more details.
#'
#' @details
#' This functions provides a plotting style for envelope objects of the spatstat
#' package (for more information please see `?spatstat::envelope`). The location of the
#' observed value in relation to the simulation envelope of the null model input is
#' indicated by an additional colour bar at the bottom of the plot.
#'
#' A named vector including labels for the following three cases:
#' \cr 1 = observed > high
#' \cr 2 = low < observed < high
#' \cr 3 = observed < low
#'
#' All values are standarized by subtracting the theoretical value for CSR
#'
#' @seealso
#' \code{\link{envelope}}
#'
#' @references
#' Esser D.S., Leveau J.H.J., Meyer K.M., Wiegand K. (2015): Spatial scales of
#' interactions among bacteria and between bacteria and the leaf surface. FEMS Microbiology Ecology, in press.
#'
#' @examples
#' set.seed(42)
#' pattern <- spatstat::rThomas(kappa = 50, scale = 0.025, mu = 5)
#' csr_envelope <- spatstat::envelope(pattern, fun = spatstat::pcf, nsim = 39)
#' quantum_plot(csr_envelope, ylab = "g(r)")
#'
#' @aliases quantum_plot
#' @rdname quantum_plot

#' @export
quantum_plot <- function(input,
                         labels = c("clustering", "randomness", "segregation"),
                         title = NULL, xlab = NULL, ylab = NULL, size = 5,
                         full_fun = T, standarized = F){

  if(!is(input, "envelope") && !is(input, "input.frame")) {

    stop("Please provide envelope or data frame.", call. = FALSE)
  }

  if(length(labels) !=  3){
    labels <- c("clustering", "randomness", "segregation")
    warning("Not enough labels provided - using 'clustering', 'randomness' and 'segregation'.",
            call. = FALSE)
  }

  if(is.null(xlab)){xlab <- "r"}

  if(is.null(ylab)){ylab <- "f(r)"}

  input <- spatstat::as.data.frame.fv(input)

  names(input) <- c("r", "observed", "theoretical", "low", "high")

  if(standarized == TRUE){
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

  # allow for user color scale
  color_scale <- c("#440154FF",
                   "#238A8DFF",
                   "#FDE725FF")

  names(color_scale) <- labels

  if(full_fun == TRUE){

    gg_plot <- ggplot2::ggplot(input) +
      ggplot2::geom_ribbon(ggplot2::aes(x = r, ymin = low, ymax = high), fill = "grey") +
      ggplot2::geom_line(ggplot2::aes(x = r, y = observed, linetype = "Observed"), size = 0.5) +
      ggplot2::geom_line(ggplot2::aes(x = r, y = theoretical, linetype = "Theoretical"), size = 0.5) +
      ggplot2::geom_line(ggplot2::aes(x = r, y = min(c(low, observed)), colour = type, group = "x"), size = size) +
      ggplot2::scale_color_manual(name = "", values = color_scale) +
      ggplot2::scale_linetype_manual(name = "", values = c(1,2)) +
      ggplot2::labs(x = xlab, y = ylab, title = title) +
      ggplot2::theme_bw(base_size = 15) +
      ggplot2::theme(legend.position = "bottom")
  }

  else{
    gg_plot <- ggplot2::ggplot(input) +
      ggplot2::geom_line(ggplot2::aes(x = r, y = 0, colour = type, group = "x"), size = 5) +
      ggplot2::coord_cartesian(ylim = c(0, 0.1)) +
      ggplot2::scale_color_manual(name = "", values = color_scale) +
      ggplot2::labs(x = xlab, y = "", title = title) +
      ggplot2::theme_classic(base_size = 15) +
      ggplot2::theme(legend.position = "bottom",
                     strip.background = ggplot2::element_blank(),
                     axis.line.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  }
  return(gg_plot)
}
