#' Compare Alignments
#'
#' Visual comparison of multiple alignments by plotting the number of aligned
#' reactions against the fraction of aligned communities
#'
#' @param ... MiCoAl objects
#' @param names Character vector supplying names to be used as labels in plot
#' @param smooth Boolean to toggle a smooth line
#' @param se Boolean to toggle std. error bands
#' @param min_frac Numerical value specifying minimum fraction in alignment
#' @param max_frac Numerical value specifying maximum fraction in alignment
#'
#' @return A ggplot for the comparison of multiple MiCoAl objects.
#' @export
#'
compareAlignments <- function(
    ...,
    names,
    smooth = FALSE,
    se = FALSE,
    min_frac = NULL,
    max_frac = NULL) {
  if (is.null(min_frac)) min_frac <- 0
  if (is.null(max_frac)) max_frac <- 1

  alig_list <- list(...)

  rxns_per_level <- purrr::map2_df(alig_list, names, .rxnsPerLevel)

  tb <- tibble::tibble(
    alignment = names,
    n_comms = unlist(lapply(alig_list, function(x) length(x@Communities)))
  ) %>%
    dplyr::left_join(rxns_per_level, by = "alignment") %>%
    dplyr::mutate(
      frac = .data$level / .data$n_comms
    ) %>%
    dplyr::filter(
      .data$level > 1 &
        .data$frac > {{ min_frac }} &
        .data$frac < {{ max_frac }}
    )

  gg <- tb %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$frac,
      y = .data$rxns,
      color = .data$alignment,
      fill = .data$alignment
    )) +
    ggplot2::scale_x_continuous(
      limits = c(min_frac, max_frac), expand = c(0, 0.02)
    ) +
    ggplot2::labs(
      x = "Fraction Aligned Communities",
      y = "Number of Reactions"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = c(0.85, 0.85),
      legend.background = ggplot2::element_rect(),
      legend.frame = ggplot2::element_rect(),
      legend.title = ggplot2::element_blank()
    )

  if (smooth) {
    gg +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::geom_smooth(method = "loess", formula = " y ~ x", se = se)
  } else {
    gg + ggplot2::geom_point()
  }
}


#' Get Count of Reactions Per Level of Alignment
#'
#' @param object MiCoAl object
#' @param name Name of the object
#'
#' @return A tibble with levels and count of reaction
.rxnsPerLevel <- function(object, name) {
  levels <- object@Alignment$levels
  mat <- object@Alignment$levels_mat
  rxns <- purrr::map_dbl(levels, ~ sum(mat >= .x))

  tibble::tibble(
    level = levels,
    rxns = rxns,
    alignment = name
  )
}
