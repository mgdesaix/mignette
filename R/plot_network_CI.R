#' Plot 95% credible intervals of connectivity
#'
#' @param network_model Network model output from mignette::run_network_model()
#' @param stage Character string "Breeding" or "Nonbreeding" for determining axes
#' @param stage_colors Optional character vector manually specifying the colors of the nodes in `stage`
#' @param overlap Logical, indicates whether to label connectivity values that have 95% CIs overlapping with 0
#' @return Returns a ggplot object of the 95% credible intervals of the connectivity estimates
#' @export
#'
plot_network_CI <- function(network_model, stage = "Breeding", stage_colors = NULL, overlap = TRUE){
  stopifnot("Not a valid stage choice - must be Breeding or Nonbreeding" = stage %in% c("Breeding", "Nonbreeding"))
  stopifnot("Not a valid overlap choice - must be TRUE or FALSE" = is.logical(overlap))

  brnode_names <- network_model$brnode_names
  nbnode_names <- network_model$nbnode_names

  q2.5.long <- tidy_conn(conn_mat = network_model$jags_out$q2.5$conn, statistic = "Q2.5",
                         brnode_names = brnode_names, nbnode_names = nbnode_names)
  mean.long <- tidy_conn(conn_mat = network_model$jags_out$mean$conn, statistic = "Mean",
                         brnode_names = brnode_names, nbnode_names = nbnode_names)
  q97.5.long <- tidy_conn(conn_mat = network_model$jags_out$q97.5$conn, statistic = "Q97.5",
                          brnode_names = brnode_names, nbnode_names = nbnode_names)
  overlap.long <- tidy_conn(conn_mat = network_model$jags_out$overlap$conn, statistic = "Overlap",
                            brnode_names = brnode_names, nbnode_names = nbnode_names)
  full.long <- q2.5.long %>%
    dplyr::left_join(mean.long, by = c("Breeding", "Nonbreeding")) %>%
    dplyr::left_join(q97.5.long, by = c("Breeding", "Nonbreeding")) %>%
    dplyr::left_join(overlap.long, by = c("Breeding", "Nonbreeding")) %>%
    dplyr::mutate(Breeding = factor(Breeding, levels = brnode_names),
                  Nonbreeding = factor(Nonbreeding, levels = nbnode_names),
                  Overlap = as.logical(Overlap))

  if(stage == "Breeding"){
    p.ci <- full.long %>%
      ggplot2::ggplot(aes(x = Nonbreeding, group = Breeding)) +
      ggplot2::geom_point(aes(y = Mean,
                              color = Breeding,
                              shape = Overlap),
                          position = ggplot2::position_dodge(width = 0.65),
                          size = 3)
  } else {
    p.ci <- full.long %>%
      ggplot2::ggplot(aes(x = Breeding, group = Nonbreeding)) +
      ggplot2::geom_point(aes(y = Mean,
                              color = Nonbreeding,
                              shape = Overlap),
                          position = ggplot2::position_dodge(width = 0.65),
                          size = 3)
  }

  p.ci <- p.ci +
    ggplot2::geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5),
                           width = 0.2,
                           position = ggplot2::position_dodge(width = 0.65)) +
    ylab("Connectivity") +
    ggplot2::theme_bw() +
    guides(color = guide_legend(order = 1),
           shape = guide_legend(order = 2))

  if(!is.null(stage_colors)){
    p.ci <- p.ci + scale_color_manual(values = stage_colors)
  }

  if(overlap == FALSE){
    p.ci <- p.ci + scale_shape_manual(values = c(16, 16)) + guides(shape = "none")
  }

  return(p.ci)
}
