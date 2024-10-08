% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_network_CI.R
\name{plot_network_CI}
\alias{plot_network_CI}
\title{Plot 95\% credible intervals of connectivity}
\usage{
plot_network_CI(
  network_model,
  stage = "Breeding",
  stage_colors = NULL,
  zero = TRUE
)
}
\arguments{
\item{network_model}{Network model output from mignette::run_network_model()}

\item{stage}{Character string "Breeding" or "Nonbreeding" for determining axes}

\item{stage_colors}{Optional character vector manually specifying the colors of the nodes in \code{stage}}

\item{zero}{Logical, indicates whether to label connectivity values that have 95\% CIs with posterior mass at 0}
}
\value{
Returns a ggplot object of the 95\% credible intervals of the connectivity estimates
}
\description{
Plot 95\% credible intervals of connectivity
}
