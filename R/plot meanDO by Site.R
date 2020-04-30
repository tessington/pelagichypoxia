#' plot_meanDO_by_site
#'@description
#'  creates 2 x 2 panel plot of mean DO  by month for each site
#'
#'
#' @return
#' stores a pdf file "meanDObysite" in graphics folder
#'
#'
#'
#' @export
#'
#' @examples
#'
#
#
plot_meanDO_by_site <- function(){

# function to add error bars to points
add.error <-function(meanDO, SE){
lower <- meanDO - SE
upper <- meanDO + SE

for (i in 1:length(meanDO)){
  arrows(x0=i, y0 = lower[i], x1 = i, y1 = upper[i], angle = 90, length = 0.075, code = 3)
}
}
require(dplyr)
require(KernSmooth)
require(R.matlab)

# Set run specifications
theDOdata <- formatDOdataFN()
ylims2 <- c(0,2)
yat <- seq(0,2, by=1)
direction <- "below"

#############GET DATA #########################
# get real data

pch.list <- c(21,22,23,24)

# Relevel factors so they are in order


year.list <- 2012:2013
sites <- c("A","B","C","D")
months <- c("Jun","Jul","Aug","Sep","Oct")

Diel.2.use <- "night"
ylims1 <- c(1.75, 6.5)
plotfilename <- "graphics/meanDObysite.pdf"
pdf(file = plotfilename, height = 5, width = 8)

color <- c("white","white")

par(
  las = 1,
  mai = c(0.75, 0.75, 0.0, 0.00),
  omi = c(0.75,0.5,0.75,0.5),
  mfrow = c(1,2)
)

for (year in 1:2) {

  # first create placeholder
    plot(
      c(),
      c(),
      type = "l",
    lwd = 3,
    axes = F,
    xlab = "",
    ylab = "",
    col = color[1],
    ylim = ylims1,
    xlim = c(0.5,5.5),
    yaxs = "i"
  )
  box()
  axis(1, at = 1:5, labels =c("Jun","Jul",'Aug',"Sep",'Oct'), cex.axis = 1.15)
  axis(2, at = c(2,4,6,8),cex.axis = 1.25)
  axistext = "mean DO (mg / l)"
  axistext = expression(paste("Dissolved Oxygen (mg l"^"-1",")"))
    if (year ==1) mtext(side = 2, line = 2,text = axistext,las = 0, cex = 1.25)
  # label each plot with year
  mtext(side = 3, line =0, text =year.list[year], cex = 1.25)
  # make separate plot outputs for each year
   for (site in 1:4) {
     site.2.use <- sites[site]
    site.data <- filter(theDOdata, Site == site.2.use,
                        Year == year.list[year],
                        Diel == Diel.2.use)

    # plot meanDO
    with(
      site.data,
      lines(
        as.numeric(Month),
        meanDO,
        type = "l",
        lwd = 3,
        col = "black"
      )
    )
    with(site.data,
         add.error(meanDO, SE)
         )
    with(
      site.data,
      points(
        as.numeric(Month),
        meanDO,
        pch = pch.list[site],
        bg = color[year],
        cex = 1.5

      )
    )

  }

}

legend("topright", legend = c("Union", "Hoodsport", "Duckabush", "Dabob Bay"),
       lwd = 2,
       pch = pch.list,
       pt.bg = "white",
       cex = 1.1,
       bty = "n")
dev.off()
}


