#' plot_temp_profiles
#'@description
#' creates a plot showing temperature profiles (figure S4)
#'
#'
#' @return
#' stores a pdf file "tprofiles.pdf" in graphics folder
#'
#'
#'
#' @export
#'
#' @examples
#'

plot_temp_profiles <- function(){
require(viridis)

# do not use indices (where sensor was bad)
no.use <- c(1:4, 42)

plotfilename <- "graphics/tprofiles.pdf"
pdf(file = plotfilename, height = 8, width = 12)
col <- plasma(n=16)[c(2,6,10,16)]

pelagichypoxia::ctd_data

DO <- ctd_data$hc[[16]]
depth <- ctd_data$hc[[1]]
Temp <- ctd_data$hc[[2]]

samples <- unlist(ctd_data$hc[14])#make corresponding list of where/when measurements were taken
Year=substr(samples,1,4)
Month=substr(samples,5,6)
Day = substr(samples,7,8)
Month[Month=="06"] <- "Jun"
Month[Month=="07"] <- "Jul"
Month[Month=="08"] <- "Aug"
Month[Month=="09"&Day=="30"] <- "Oct"
Month[Month=="09"] <- "Sep"
Month[Month=="10"] <- "Oct"

Site=substr(samples,10,11)
Site[Site=="Un"] <- "A"
Site[Site=="Hp"] <- "B"
Site[Site=="Du"] <- "C"
Site[Site=="Da"] <- "D"


Diel=substr(samples,13,13)
Diel[Diel=='d'] <- "day"
Diel[Diel=='n'] <- "night"

Rep=substr(samples,14,14)

DO.data <-data.frame(Year,Month,Site,Diel,Rep)
DO.data$index <- 1:nrow(DO.data)
DO.data <- DO.data %>%
  filter(Site %in% c("A", "B", "C", "D"))
DO.data$Site <- droplevels(DO.data$Site, exclude = c("P2", "Tw"))


predict.X <- as.data.frame(
  cbind(
    c(rep(2012,20),rep(2013,20)),
    rep(c("Jun","Jul","Aug","Sep","Oct"),each = 4, times =2),
    rep(c("A","B","C","D"), each = 1, times = 10)))
names(predict.X) <- c("Year","Month","Site")
predict.X$Month <-
  with(predict.X, factor(Month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct")))
predict.X$Index <- NA

for (i in 1:nrow(predict.X)) {
  indices <- DO.data$index[with(DO.data, which(Year == predict.X$Year[i] & Month == predict.X$Month[i] &
                    Site == predict.X$Site[i]))]
  good.indices <- setdiff(indices, no.use)
  predict.X$Index[i] <- list(good.indices)
}

month.labels <- c("June", "July", "August", "September", "October")

Year <- 2012
Month <- "Jun"
year.list <- c(2012, 2013)
month.list <- c("Jun","Jul","Aug","Sep","Oct")
site.list <- c("A", "B", "C", "D")

par(mfrow = c(2,5), omi = c(0.1, 0.5, 0.7, 0.1), mar = c(5, 1, 1, 5))


  for (yr in 1:2) {
    for (mo in 1:5) {
    plot(
      c(),
      c(),
      ylim = c(-120, 0),
      xlim = c(8, 16),
      axes = F,
      ylab = "",
      xlab = ""
    )
    box()
    if (mo == 1) {
    axis(
      side = 2,
      at = seq(-120, 0, by = 20),
      labels = rev(seq(0, 120, by = 20)),
      las = 1,
      cex.axis = 1.25
    )
    } else {
      axis(
      side = 2,
      at = seq(-120, 0, by = 20),
      labels = F,
      las = 1
      )
    }


    axis(side = 3, at = seq(8, 14, by = 2), cex.axis = 1.25)
    if (mo ==1) mtext(side = 2, "Depth (m)", line = 3)
    if (yr == 1) mtext( side = 3, expression(paste('Temperature (',degree~C,')')), line = 2)
    if (yr == 1) mtext( side = 3, month.labels[mo], line = 4)
    for (site in 1:4) {
      plot.indices <- predict.X %>%
        filter (Year == year.list[yr] , Month == month.list[mo] , Site == site.list[site]) %>%
        select(Index)

      if (length(unlist(plot.indices)) == 1)
        plot.t.data <- Temp[,unlist(plot.indices)]
      if (length(unlist(plot.indices)) > 1)
        plot.t.data <- rowMeans(Temp[, unlist(plot.indices)], na.rm = T)



      nrows <- length(plot.t.data)
      lines(
        x = plot.t.data,
        y = -1:-nrows,
        lwd = 2,
        col = col[site]
      )
    }
  }
}
legend("bottomright",
       legend = c("Union","Hoodsport","Duckabush", "Dabob"),
       lty = "solid",
       lwd = 2,
       col = col,
       cex = 1.25,
       bg = "white")

dev.off()
}
