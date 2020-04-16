#' generate_posterior_prediction
#'@description
#' uses the posterior of zooplankton and herring backscatter
#' and herring feeding, to show posterior probability of predation intensity
#'
#'
#' @return
#'  a plot in the active plot window, with median listed and 80% credibility interval shown
#'
#' @export
#'
#' @examples
#'
generate_posterior_predation <-function() {
require(KernSmooth)


filename.list <- c("zoopSa", "Pacific Herringfullness", "Pacific HerringSa")

models.2.use<- c(4,6,3)





# get posteriors
  load(file = paste("outputs/modelselection/", filename.list[1],"/model.Rdata",sep= ""))
  euph.output <- model.outputs[[models.2.use[1]]]
  load(file = paste("outputs/modelselection/", filename.list[2],"/model.Rdata",sep= ""))
  full.output <- model.outputs[[models.2.use[2]]]
  load(file = paste("outputs/modelselection/", filename.list[3],"/model.Rdata",sep= ""))
  herr.output <- model.outputs[[models.2.use[3]]]



  pred.output <- herr.output$do_effect + full.output$do_effect - euph.output$do_effect


  smoothed <- bkde(x = pred.output,
                   kernel = "normal",
                   range.x = as.vector(c(-4.5, 4.5)),
                   truncate = T
  )
  ylim = c(0, 0.75)
  plot(smoothed$x, smoothed$y,
       type = "l",
       lwd = 2,
       ylim = ylim,
       yaxs = "i",
       xlab = "effect size",
       ylabe = "probability density")
  ci <- get.ci( smoothed$x, smoothed$y, 0.8)

  # need to plot a polygon starting at lower ci, up to upper ci.
  x.top <- c(ci[1], smoothed$x[smoothed$x >ci[1] & smoothed$x <ci[2]], ci[2])
  y.top <- c(ci[3], smoothed$y[smoothed$x > ci[1] & smoothed$x < ci[2]], ci[4])
  x.bottom <- rev(x.top)
  y.bottom <- rep(0, length(x.bottom))
  polygon(c(x.top, x.bottom), c(y.top, y.bottom), col = "gray", border = NA, lty = 0)
  lines(smoothed$x, smoothed$y, type = "l", lwd = 2)


  # find median
  med <- get.percentile(smoothed$x, smoothed$y, 0.5)
  text(x = -4, y = 0.6, labels = paste("median =", round(med,2), sep = ""))
}
