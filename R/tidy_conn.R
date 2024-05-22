#' Prepare a tibble for plotting credible intervals
#'
#' @param conn_mat Matrix of data
#' @param statistic Character string specifying statistic of interest
#' @param brnode_names Character vector of breeding node names
#' @param nbnode_names Character vector of nonbreeding node names
#' @return Returns a long tibble
tidy_conn <- function(conn_mat, statistic,
                      brnode_names, nbnode_names){
  initial.df <- as.data.frame(conn_mat)
  colnames(initial.df) <- nbnode_names
  tib.long <- initial.df %>%
    round(5) %>%
    tibble::add_column("Breeding" = brnode_names, .before = 1) %>%
    tidyr::pivot_longer(cols = 2:(length(nbnode_names)+1),
                        names_to = "Nonbreeding",
                        values_to = statistic)
  return(tib.long)
}
