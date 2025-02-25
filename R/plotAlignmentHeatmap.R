#' MiCoAl Heatmap
#'
#' @param object An object of class MiCoAl.
#' @param frac Minimum relative weight of maximum weight to plot.
#'
#' @return A ggplot heatmap
#' @export
plotAlignmentHeatmap <- function(object, frac) {
  # Filter the adjacency matrix for desired levels for visualization.
  levels_mat <- object@Alignment$levels_mat
  max_weight <- length(object@Communities)
  min_weight <- max_weight * frac

  levels_mat[levels_mat < min_weight] <- 0

  if (min_weight > max(levels_mat)) {
    return("No Alignment at this level.")
  }

  gg <- levels_mat %>%
    tibble::as_tibble(rownames = "RowName") %>%
    tidyr::pivot_longer(
      cols = -"RowName",
      names_to = "ColName",
      values_to = "level"
    ) %>%
    dplyr::rename(
      met = "RowName",
      met2 = "ColName"
    ) %>%
    dplyr::filter(.data[["level"]] >= min_weight) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$met2, y = .data$met, fill = .data$level)
    ) +
    ggplot2::geom_tile() +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_gradient(
      low = "yellow",
      high = "red",
      breaks = scales::breaks_pretty()
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
      axis.text.y = ggplot2::element_text(vjust = 0.4),
      axis.title = ggplot2::element_blank(),
      # panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    )

  gg
}
