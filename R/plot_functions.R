#' Produce scatterplot with sales data and a single continuous variable
#' @param data data frame with variables necessary for scatterplot.
#' @param xvar variable (string format) to be used on x-axis.
#' @param yvar variable (string format) to be used on y-axis.
#' @param facetvar optional variable (string format) to use for facetted version of plot.
#' @param label whether or not to label brushed points.
#' @return \code{ggplot2} object for the scatterplot.
#' @export
scatter_isox <- function(dataset,
                          xvar,
                          yvar,
                          facetvar = NULL,
                          label = FALSE,
                          point_colors = c("darkblue", "#66D65C")) {

  x <- rlang::sym(xvar)
  y <- rlang::sym(yvar)

  p <- ggplot(dataset, aes(x = !!x, y = !!y))

  p <- p +
    geom_point(aes(color = selected_), alpha=1/2) +
    scale_color_manual(values = point_colors, guide = "none") +
    scale_x_continuous(labels = function(l) plot_labeller(l, varname = xvar)) +
    scale_y_continuous(labels = function(l) plot_labeller(l, varname = yvar)) +
    theme(axis.title = element_text(size = rel(1.2)),
          axis.text = element_text(size = rel(1.1)))

  if (!is.null(facetvar)) {
    fvar <- rlang::sym(facetvar)

    p <- p +
      facet_grid(fvar, scales="free")
  }

  if (label && any(dataset$selected_)) {
    p <- p + geom_label_repel(
      data = filter(dataset, selected_),
      aes(label = scan.no)
    )
  }

  p
}



plot_labeller <- function(l, varname) {
  if (varname == "scan.no") {
    comma(l) #could use for permil
  } else {
    comma(l)
  }
}


# Density plot ------------------------------------------------------------

density_isox <- function(dataset,
                         xvar,
                         facetvar = NULL,
                         label = FALSE#,
                         #point_colors = c("darkblue", "#66D65C")
                         ) {

  x <- rlang::sym(xvar)


  p <- ggplot(dataset, aes(x = !!x))

  p <- p +
    geom_density() +
    scale_x_continuous(labels = function(l) plot_labeller(l, varname = xvar)) +
    theme(axis.title = element_text(size = rel(1.2)),
          axis.text = element_text(size = rel(1.1)))



  if (!is.null(facetvar)) {
    fvar <- rlang::sym(facetvar)

    p <- p + facet_grid(fvar, scales="free")
  }
  p
}
