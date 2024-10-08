% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/net_create.R
\name{net_create}
\alias{net_create}
\title{Create a "net" object from the JAGS model for visualization}
\usage{
net_create(
  network_model,
  margin = 0.05,
  connected_tol = 0.001,
  replace_overlap = FALSE
)
}
\arguments{
\item{network_model}{Network model output from mignette::run_network_model()}

\item{margin}{Float value}

\item{connected_tol}{Float value to set threshold of connectivity estimates to set to 0}

\item{replace_overlap}{Logical value for changing connectivity estimates to 0 that have 95\% credible intervals bounded by 0}
}
\value{
Returns a net object to be used in \code{net_draw} function
}
\description{
Create a "net" object from the JAGS model for visualization
}
