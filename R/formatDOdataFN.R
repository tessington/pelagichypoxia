#' formatDOdataFN
#'@description
#' retrieve dissolved oxygen data from matlab file HCmat, and format it for use in modeling
#'
#'
#'
#' @return
#'  a dataframe containing year, diel period, site, month, and mean DO (<20 m) for each
#'
#'
#' @export
#'
#' @examples
#'


formatDOdataFN <- function() {
#######

  require(dplyr)
  pelagichypoxia::ctd_data
  #View(O2$hc[[2]])
  DO <- ctd_data$hc[[16]]
  depth <- ctd_data$hc[[1]]
  #View(O2$hc[14])
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

  DO.data <-data.frame(Year,Month,Site,Diel,Rep,meanDO = NA, SE = NA)


  ####Calculate mean DO

  for(i in 1:ncol(DO)){
    DO.2.use <- DO[21:nrow(DO),i] # only use data from depth bins 20 m and deeper
    DO.data$meanDO[i]<- mean(DO.2.use, na.rm = T)
    # remove NaN
    x <- DO.2.use[which(is.nan(DO.2.use)==0)]
    if (length(x)>5) {
    # get SE
      t.max <- sqrt(length(x))
    DO.data$SE[i] <- calc.se(x,t.max)
    } else {
      DO.data$SE[i] <- NA
    }


  }


sitedata <- DO.data
extract.DO <- function(sampledata, sitedata) {
  site.info <- sitedata %>%
    filter(
      Year == as.character(sampledata$Year),
      Month == as.character(sampledata$Month),
      Diel == as.character(sampledata$Diel),
      Site == as.character(sampledata$Site),
      meanDO !="NaN")
  # check to see how many rows there are.
  n.rows <- nrow(site.info)
  if (n.rows == 1)  return(unlist(site.info[6:7]))
  if (n.rows == 0) return(c(NA, NA))
  if (n.rows > 1) {
   tmp.mean.se <- fixed.effect(site.info$meanDO, site.info$SE)
   return(unlist(tmp.mean.se))
  }
}

predict.X <- as.data.frame(
  cbind(
    c(rep(2012,40),rep(2013,40)),
    rep(c("day","night"),times = 40),
    rep(c("Jun","Jul","Aug","Sep","Oct"),each = 8, times =1),
    rep(c("A","B","C","D"), each = 2, times = 10)))
names(predict.X) <- c("Year","Diel","Month","Site")
predict.X$Month <-
  with(predict.X, factor(Month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct")))

# remove rep 1 from 2012 Hoodsport and Uniont samples because sensor was bad

rem.index <- with(DO.data, which(Year == 2012 & Month == "Jun" & Site %in% c("A", "B") & Rep ==1))
DO.data <- DO.data[-rem.index,]

# Have to loop because I can't get sapply or lapply to work on function
theDOdata <- predict.X
for (i in 1:nrow(predict.X)) {
  tmp.do <- extract.DO(predict.X[i,], DO.data)
  theDOdata$meanDO[i] = tmp.do[1]
  theDOdata$SE[i] = tmp.do[2]
}
# for some reason columns are coming through as lists so undo this
# Calculate effective sample size, variance, and SE


theDOdata$meanDO <- unlist(theDOdata$meanDO)


# replace 2012 June A  and B Night with 2012 June  A and B Day
theDOdata[with(theDOdata,
               which(Year == 2012 &
                       Month == "Jun" &
                       Diel == "night" &
                       Site %in% c("A", "B"))), 5:6] <-
  theDOdata[with(theDOdata,
                 which(Year == 2012 &
                         Month == "Jun" &
                         Diel == "day" & Site %in% c("A", "B"))), 5:6]


# replaces 2013 July C day with July C night
theDOdata[with(theDOdata,
               which(Year == 2013 &
                       Month == "Jul" &
                       Diel == "day" &
                       Site == "C")), 5:6] <-
  theDOdata[with(theDOdata,
                 which(Year == 2013 &
                         Month == "Jul" &
                         Diel == "night" & Site == "C")), 5:6]

##### End of formatting DO data######
return(theDOdata)
}
