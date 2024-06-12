### Needs to generate an MiCo object
### add argument to set seed within Namespace


#' Generate Random Synthetic Communities
#'
#' @param n_mo Number of micro organisms
#' @param max_met Maximum number of metabolites in the communities
#' @param n_co Number of communities
#' @param scale_fac Scaling factor
#' @param seed Seed for reproducibility
#' @param dead_ends Logical value to toggle dead ends in data
#'
#' @return List with `n_co` number of communities.
#' @export
#'
#' @examples
#' #
rand_mo_data <-
  function(
    n_mo, max_met, n_co = 2, scale_fac = 2, seed = 123,
    dead_ends = FALSE) {

  r_names <- function(n = 5000) {
    a <- do.call(paste0, replicate(3, sample(LETTERS, n, TRUE), FALSE))
    paste0(a, sprintf("%03d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  }

  mo_names <- r_names(n_mo*scale_fac)
  met_vec <- paste0("met", 1:max_met)

  out_list <- list()
  # Iterate over all comms
  for (co in 1:n_co) {
    set.seed(co)
    co_col <- vector()
    mo_col <- vector()
    met_col <- vector()
    val_col <- vector()

    # Make the community name
    co_name <- paste0("Co", co)
    # Get sample of species for the community
    co_mo_names <- sample(mo_names, size = n_mo, replace = FALSE)

    for (i in seq_along(co_mo_names)) {
      t_met <- sample(2:max_met, 1)
      mets <- sample(met_vec, t_met)
      mo_col <- c(mo_col, rep(co_mo_names[i], t_met))
      # Make temp val vector
      t_vals <- vector()
      # Iterate over all metabolites
      for (m in mets) {
        # mo_col <- c(mo_col, paste0("Sp", i))
        met_col <- c(met_col, m)
        t_vals <- c(t_vals, stats::rnorm(1, mean = 0, sd = 3))
      }
      if (!dead_ends & (all(t_vals < 0) | all(t_vals > 0))) {
        to_change <- sample(1:length(t_vals), 1)
        t_vals[to_change] <- t_vals[to_change] * -1
      }

      # Append to val_col
      val_col <- c(val_col, t_vals)
    }
    out_list[[co_name]] <-
      tibble::as_tibble(list(MO = mo_col, met = met_col, val = val_col))
  }
  return(out_list)
}


