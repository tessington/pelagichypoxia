#' load_sa_fish_data
#'@description
#' extracts either the acoustic backscatter from specified species / groups
#' @param species what species to use.  either  "fish", "Pacific Hake", or "Pacific Herring"
#'
#'
#' @return
#' A dataframe containing year, month, site, diel period, and column y for the response
#' variable (backscatter), and mean dissolved oxygen.  Column se is set to 0
#'
#'
#'
#' @export
#'
#' @examples
#'

load_sa_fish_data <- function(species) {
require(reshape2)

pelagichypoxia::plot_fish_backscatter_data

Sadata <- fish_backscatter[[which(names(fish_backscatter)==species)]]


if (species == "fish") {
  Sadata<-melt(Sadata, id = c("Year","Month","Diel"), variable.name = "Site", value.name = "Sa")
  # reorder to put in similar order as other dataframes
  Sadata <- Sadata[with(Sadata, order(Year, Month, Diel, Site)),]
  Sadata$Diel <- tolower(Sadata$Diel)
  predict.X <- as.data.frame( cbind(
    c(rep(2012,40),rep(2013,40)),
    rep(c("Day","Night"),times = 40),
    rep(c("June","July","Aug","Sept","Oct"),each = 8, times =1),
    rep(c("A","B","C","D"), each = 2, times = 10)))
names(predict.X) <- c("Year","Diel","Month","Site")
predict.X$Month <-
  with(predict.X, factor(Month, levels = c("June", "July", "Aug", "Sep", "Oct")))

theDOdata <- formatDOdataFN()

Sadata <-merge(Sadata, theDOdata, by = c("Year", "Month", "Site", "Diel"), all.x = T)


for (i in 1:nrow(Sadata)) {
  Sadata$meanDO[i] <- mean(filter(theDOdata,
                                  Year == Sadata$Year[i],
                                  Month == Sadata$Month[i],
                                  Diel == Sadata$Diel[i],
                                  Site == Sadata$Site[i])$meanDO)
}



}

if (species %in% c("Pacific Herring", "Pacific Hake")){
  Sadata<-melt(Sadata, id = c("Year","Month"), variable.name = "Site", value.name = "Sa")
  # reorder to put in similar order as other dataframes
  Sadata <- Sadata[with(Sadata, order(Year, Month, Site)),]

  predict.X <- as.data.frame( cbind(
    c(rep(2012,40),rep(2013,40)),
    rep(c("Day","Night"),times = 40),
    rep(c("June","July","Aug","Sept","Oct"),each = 8, times =1),
    rep(c("A","B","C","D"), each = 2, times = 10)))
  names(predict.X) <- c("Year","Diel","Month","Site")
  predict.X$Month <-
    with(predict.X, factor(Month, levels = c("June", "July", "Aug", "Sep", "Oct")))

  theDOdata <- formatDOdataFN()
  # take the average of the two diel samples
  for (i in 1:nrow(Sadata)) {
    Sadata$meanDO[i] <- mean(filter(theDOdata,
                                    Year == Sadata$Year[i],
                                    Month == Sadata$Month[i],
                                    Site == Sadata$Site[i])$meanDO)
  }

}

thedata <- Sadata
thedata$y <- with (thedata, log(Sa))
thedata$se <- rep(0, nrow(thedata))
return(thedata)
}
