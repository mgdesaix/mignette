#' Evaluate goodness of fit (Freeman-Tukey statistic)
#'
#' @param network_model Network model output from mignette::run_network_model()
#' @return Returns a ggplot of the observed vs. simulated Freeman-Tukey distributions, and Bayesian pvalue
#' @export
#'
get_FT_fit <- function(network_model){
  jags_out <- network_model[["jags_out"]]
  FT.obs.c <- c()
  FT.rep.c <- c()
  mc.reps <- dim(jags_out$sims.list$FT.obs)[1]

  for(i in 1:mc.reps){
    FT.obs.c[i] <- sum(jags_out$sims.list$FT.obs[i, , ])
    FT.rep.c[i] <- sum(jags_out$sims.list$FT.rep[i, , ])
  }

  FT.df <- tibble::tibble("FT_obs" = FT.obs.c,
                          "FT_rep" = FT.rep.c)

  bpval <- round(jags_out$mean$bpval, 3)

  FT.df %>%
    tidyr::pivot_longer(cols = FT_obs:FT_rep,
                        names_to = "Data",
                        values_to = "Statistic") %>%
    ggplot2::ggplot()+
    ggplot2::geom_density(aes(x = Statistic,
                              group = Data,
                              fill = Data),
                          alpha = 0.5,
                          position = "identity") +
    ggplot2::labs(title = "Posterior Predictive Check with Freeman-Tukey statistic",
                  subtitle = paste("pvalue = ", bpval)) +
    ggplot2::xlab("Freeman-Tukey statistic")
}
