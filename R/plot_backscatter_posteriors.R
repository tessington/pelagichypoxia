#' plot_backscatter_posteriors
#'@description
#' creates a plot showing posterior probability distributions for backscatter metrics
#'
#'
#' @return
#' stores a pdf file "acoustic_posterior" in graphics folder
#'
#'
#'
#' @export
#'
#' @examples
#'

plot_backscatter_posteriors <- function() {
require(KernSmooth)


dir.list <- c("outputs/modelselection/ZoopSa",
                   "outputs/modelselection/fishSa",
                   "outputs/modelselection/Pacific HerringSa",
                   "outputs/modelselection/Pacific HakeSa")

plotfilename <- 'graphics/acoustic_posterior.pdf'
pdf(file = plotfilename, height = 8, width = 10)
model.list<-c(3,3,3,3)


xlim <- c(-2.5, 2.5)

par(mfrow = c(2,2), las = 1, mar = c(5,5,1,1))

ylim = c(0, 2.25)
plot.labels <- c(
  "A. Zooplankton",
  "B. Combined fish",
  "C. Pacific Herring",
  "D. Pacific Hake"
)

for (i in 1:length(dir.list)) {
filename <- paste(dir.list[i],"/","model.Rdata", sep = "")

  load(filename)
  output <- model.outputs[[model.list[i]]]

  smoothed <- bkde(x = output$do_effect,
                   kernel = "normal",
                   range.x = as.vector(c(-2.5, 2.5)),
                   truncate = T
  )

  plot(smoothed$x, smoothed$y,
        type = "l",
        lwd = 2,
       ylim = ylim,
       xlim = xlim,
       yaxs = "i",
       xlab = expression(paste("Effect size at 2 mg l" ^"-1")),
       ylab = "Posterior density",
       xpd = F,
       cex.axis = 1.25,
       cex.lab = 1.5
  )

  ci <- get.ci( smoothed$x, smoothed$y, 0.8)

  # need to plot a polygon starting at lower ci, up to upper ci.
  x.top <- c(ci[1], smoothed$x[smoothed$x >ci[1] & smoothed$x <ci[2]], ci[2])
  y.top <- c(ci[3], smoothed$y[smoothed$x > ci[1] & smoothed$x < ci[2]], ci[4])
  x.bottom <- rev(x.top)
  y.bottom <- rep(0, length(x.bottom))
  polygon(c(x.top, x.bottom), c(y.top, y.bottom), col = "gray", border = NA, lty = 0)
  lines(smoothed$x, smoothed$y, type = "l", lwd = 2)

  text(x = -2.5,
       y = 2.05,
       labels = plot.labels[i],
       pos = 4,
       cex = 1.5
        )
}
dev.off()
}
