#' calc_dothresh_posterior
#'@description
#'calculate the X% credibility interval from MCMC output
#'
#' @param species what species to use.  either "zoop", "fish", "Pacific Hake", or "Herring"
#' @param data.2.use what response variable to use either "Sa", "fullness", or "dietcomp"
#' @param model.2.use which of the alternative models should be used to generate the posterior
#'
#' @return
#'  c(lower, upper, lower density, upper density;
#'  c(prior lower, prior upper, lower density, prior density)
#'
#' @export
#'
#' @examples
#'


calc_dothresh_posterior <- function(species, data.2.use, model.2.use){

filename <- paste("outputs/modelselection/",species, data.2.use,"/","model.Rdata", sep = "")
load(filename)
require(loo)
require(KernSmooth)
require("sn")


output <-  model.outputs[[model.2.use]]


smoothed.thresh <- bkde(x = output$do_thresh,
                        kernel = "normal",
                        range.x = as.vector(c(2.0, 6)),
                        truncate = T
)
ci.thresh <- get.ci(smoothed.thresh$x, smoothed.thresh$y, 0.8)
median <- get.percentile(smoothed.thresh$x, smoothed.thresh$y, 0.5)

# what is 80% density inner density of prior?
x <- seq(from = 2.0, to = 8, length.out = 100)
y <- dsn(x, xi = 5, omega = 10, alpha = -10)
ci.prior <- get.ci(x, y, 0.8)
prior.median <- get.percentile(x,y, value = 0.5)
output <- rbind(c(ci.thresh[1:2], median),c(ci.prior[1:2], prior.median))
rownames(output) <- c("model", "prior")
colnames(output) <- c("lower", "upper", "median")
return(output)
}
