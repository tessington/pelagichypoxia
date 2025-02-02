% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_dothresh_posterior.R
\name{calc_dothresh_posterior}
\alias{calc_dothresh_posterior}
\title{calc_dothresh_posterior}
\usage{
calc_dothresh_posterior(species, data.2.use, model.2.use)
}
\arguments{
\item{species}{what species to use.  either "zoop", "fish", "Pacific Hake", or "Herring"}

\item{data.2.use}{what response variable to use either "Sa", "fullness", or "dietcomp"}

\item{model.2.use}{which of the alternative models should be used to generate the posterior}
}
\value{
c(lower, upper, lower density, upper density;
 c(prior lower, prior upper, lower density, prior density)
}
\description{
calculate the X% credibility interval from MCMC output
}
\examples{

}
