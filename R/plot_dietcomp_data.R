#' plot_dietcomp_data
#'@description
#' creates a plot showing the diet fraction of herring and hake, by month, site, and time period
#' for each year
#'
#'
#' @return
#' stores a pdf file "dietcomp.pdf" in graphics folder
#'
#'
#'
#' @export
#'
#' @examples
#'

plot_dietcomp_data <- function() {
require(viridis)
col <- plasma(n=16)[c(1,6,11,16)]

# extract  diet fraction
data.2.use <- "dietcomp"
thehakedata <- load_dietdata("Pacific Hake", data.2.use)
theherringdata <- load_dietdata("Pacific Herring", data.2.use)


# plot with month on the x axis, logit ci on the y axis, and a point for each site

ylim <- c(0,1)
sites <- c("A","B","C","D")
site.labels <- c("Union",
                 "Hoodsport",
                 "Duckabush",
                 "Dabob Bay")
months <- c("Jun","Jul","Aug","Sep","Oct")

years = c(2012, 2013)

plot.labels <- c("2012 Herring", "2013 Herring", "2012 Hake", "2013 Hake")
plotfilename <- "graphics/dietcomp.pdf"
pdf(file = plotfilename, height = 7.5, width = 8)
layout.mat <- matrix(c(1,1,2,2,1,1,2,2,3,3,4,4,3,3,4,4), byrow = T, ncol = 4, nrow = 4)
layout(mat = layout.mat)
species.list <- c("herring","hake")
par(oma = c(0.5,.5, 0.5, 0.5), mar = c(5,5,0,0))
for (sp in 1:2){

  eval( parse(text = paste("plotdata <- the",species.list[sp],"data", sep = "")))

for (j in 1:2) {

# cycle through each site an plot (one year at a time)

plot(c(),c(),
     type = "n",
     las = 1,
     xlim = c(1,5),
     ylim = ylim,
     xlab = "",
     ylab = "",
     axes = F)
box()
if (sp ==2) axis(side = 1, at = 1:5, labels = months, cex.axis = 1.5)
axis(side = 2, at=c(0, 0.5, 1.0), las = 1, cex.axis = 1.5)
### now plot sz

  for (i in 1:length(sites)) {
    plot.data <- plotdata %>%
      filter(Year == years[j], Site == sites[i], Diel == "day")

    points(plot.data$Month, inv.logit(plot.data$y), pch = 21, bg = col[i], cex = 2.5)

    plot.data <- plotdata %>%
      filter(Year == years[j], Site == sites[i], Diel == "night")

    points(plot.data$Month, inv.logit(plot.data$y), pch = 23, bg = col[i], cex = 2.5)

  }
if (j == 1) mtext("Euphausiid diet fraction", side = 2, line =3, las = 0)
if (sp ==1) text(plot.labels[j], x= 1 , y = 1, pos = 4, cex = 1.5)
if (sp == 2) text(plot.labels[j+2], x= 1 , y = 1, pos = 4, cex = 1.5)
if (sp ==2 & j ==1) legend(x = 1.5, y = 0.4 , legend = site.labels, pch = 21, pt.bg = col, cex = 2.0)
}
}
dev.off()
}
