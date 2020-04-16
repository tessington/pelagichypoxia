#' plot_zoop_backscatter_data
#'@description
#' creates a plot showing the estimated backscatter for zooplankton
#' By month, sample site, and year.
#'
#'
#' @return
#' stores a pdf file "zoop_by_month" in graphics folder
#'
#'
#'
#' @export
#'
#' @examples
#'

plot_zoop_backscatter_data <- function(){
require(viridis)
col <- plasma(n=16)[c(1,6,11,16)]

cur.dir <- dir()
if (!"graphics" %in% cur.dir) dir.create("graphics")

thezoopdata <- load_zoop_data()


#  plot with month on the x axis, log(NASC) on the y axis, and a point for each site

ylim <- with(thezoopdata, c(min(y), max(y)))
sites <- c("A","B","C","D")
site.labels <- c("Union",
                 "Hoodsport",
                 "Duckabush",
                 "Dabob Bay")
months <- c("Jun","Jul","Aug","Sep","Oct")

years = c(2012, 2013)
plotfilename <- "graphics/zoop_by_month.pdf"
pdf(file = plotfilename, height = 6, width = 12)

par(mfrow = c(1,2), mar = c(3,3,3,1), xpd = NA , oma = c(1,1,1,10))

for (j in 1:2) {

# cycle through each site an plot (one year at a time)

plot(c(),c(),
     type = "n",
     las = 1,
     xlim = c(1,5),
     ylim = ylim,
     xlab = "",
     ylab = "",
     axes = F,
     main = years[j])
box()
axis(side = 1, at = 1:5, labels = months, cex.axis = 1.5)
axis(side = 2, las = 1, cex.axis = 1.5)
### now plot backscatter

  for (i in 1:length(sites)) {

    plot.data <- thezoopdata %>%
      filter(Year == years[j], Site == sites[i], Diel == "day")
    points(plot.data$Month, plot.data$y, pch = 21, bg = col[i], cex = 2.5)
    plot.data <- thezoopdata %>%
      filter(Year == years[j], Site == sites[i], Diel == "night")

    points(plot.data$Month, plot.data$y, pch = 23, bg = col[i], cex = 2.5)

  }
if (j==1) mtext("log zooplankton NASC", side = 2, line =2.75, las = 0, cex = 1.5)
if (j==2) legend(x=5.2, y=6, legend = site.labels, pch = 21, pt.bg = col, pt.cex = 2.5, cex = 1.5)
}
dev.off()
}
