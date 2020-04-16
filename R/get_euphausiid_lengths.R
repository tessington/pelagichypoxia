
#' get_euphausiid_lengths
#'@description
#' calculates the fraction of euphausiids that are greater than 10 mm, for each sampling event
#'
#'
#' @return
#'  a data frame listing year, month, site, diel and the proportion euphausiids that are large
#'
#'
#' @export
#'
#' @examples
#'

get_euphausiid_lengths <- function() {
require(dplyr)
require(reshape2)
require(KernSmooth)

# Create output matrix
output <- data.frame(matrix(NA, nrow = 79, ncol = 7))
names(output) <- c("Year", "Month","Day", "Site", "Diel", "Filename","Plarge")


euphausiid_lengths <- pelagichypoxia::euphausiid_lengths
list.names <- names(euphausiid_lengths)

for (i in 1:length(list.names)) {
    thedata <- euphausiid_lengths[[i]]
    # remove the zeros

   # only try this if there are 10 or more length samples
     if (nrow(thedata) > 10) {
       # code to calculate the proportion, by number, if euphausiids > 10 mm
       smoothed <- bkde(thedata$Length, kernel = "normal", range.x = c(0, 25))
       large.index <- which(smoothed$x >= 10)
       p.large <- sum(smoothed$y[large.index]) / sum(smoothed$y)

    } else {p.large <- NA}

    # get ready to output
    year <- substr(list.names[i], start = 4, stop =7)
    mo <- substr(list.names[i], start = 8, stop =9)
    day <- substr(list.names[i], start = 10, stop =11)
    site <- substr(list.names[i], start = 12, stop =13)

    test.diel <- gregexpr(pattern ="day", text = list.names[i])

    if(test.diel[[1]][1]==-1) {
      diel <- "night"
      tmp.filename <- sub(x = list.names[i], pattern ="night", replacement = "xlsx")
    } else {
      diel <- "day"
      tmp.filename <- sub(x = list.names[i],pattern ="day", replacement = "xlsx")
    }


    output$Year[i] <- year
    output$Month[i] <- mo
    output$Day[i] <- day
    output$Site[i] <- site
    output$Diel[i] <- diel
    output$Filename[i] <- tmp.filename
    output$Plarge[i] <- max(0.01, p.large) #constrain this to that there is always at least 1% of krill biomass that are above 10 mm
  }

# now assign to sample month
output$Day <- as.numeric(output$Day)
output$Month <- as.numeric(output$Month)

month.list <- 6:10
month.names <- c("Jun", "Jul", "Aug", "Sep", "Oct")

output$MonthName <- rep(NA, nrow(output))
output$MonthName[output$Month == 6] <- month.names[1]
output$MonthName[output$Month == 7] <- month.names[2]
output$MonthName[output$Month == 10] <- month.names[5]

# now deal with tricky august and September

output$MonthName[output$Month == 8 & output$Day < 10] <- month.names[3]
output$MonthName[output$Month == 8 & output$Day > 20] <- month.names[4]


output$MonthName[output$Month == 9 & output$Day < 10] <- month.names[4]
output$MonthName[output$Month == 9 & output$Day > 20] <- month.names[5]

output$MonthName <- with(output, factor(MonthName, levels = c("Jun", "Jul", "Aug", "Sep", "Oct")))

# now adjust sites to match rest of data (use A, B, C D notation)

output$SiteName <- NA
output$SiteName[output$Site == "UN"] <- "A"
output$SiteName[output$Site == "HP"] <- "B"
output$SiteName[output$Site == "DU"] <- "C"
output$SiteName[output$Site == "DA"] <- "D"

output$SiteName <- with(output, factor(SiteName, levels = c("A", "B", "C", "D")))

output$Diel <- as.factor(output$Diel)


### Create a master design matrix
new.output <- as.data.frame(
  cbind(
    c(rep(2012,40),rep(2013,40)),
    rep(c("day","night"),times = 40),
    rep(c("Jun","Jul","Aug","Sep","Oct"),each = 8, times =1),
    rep(c("A","B","C","D"), each = 2, times = 10)))
names(new.output) <- c("Year","Diel","Month","Site")
new.output$Month <-
  with(new.output, factor(Month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct")))
new.output$y <- NA

# Final steps, cycle through predict.X, the master design matrix, and look up estimates in output
# load net depths
netdepths <- pelagichypoxia::netdepths


for (i in 1:nrow(new.output)) {
  # extract out entries in output that match this
  tmp.data <- output %>%
    filter(Year == new.output$Year[i],
           SiteName == new.output$Site[i],
           MonthName == new.output$Month[i],
           Diel == new.output$Diel[i]
           )
  n.tmp.data <- nrow(tmp.data)

  if(n.tmp.data == 1) new.output$y[i] <- tmp.data$Plarge

  # if there are multiples
  if(n.tmp.data > 1) {
    # first check to see how many of these sample layers have euphausiid counts
    tmp.data <- tmp.data %>%
      filter(Plarge != "NA")
    # if only 1, it is ready
    if (nrow(tmp.data) == 1) new.output$y[i] <- tmp.data$Plarge
    # if more than one, weight by depth range
    if (nrow(tmp.data) >1) {
      #look up depth range
      for (j in 1:nrow(tmp.data)) {
        tmp.net.depth <- filter(netdepths, Net == tmp.data$Filename[j])
        tmp.data$DepthRange[j] <- with(tmp.net.depth, Lower  - Upper)
      }
      # get weighted average
      new.output$y[i] <- with(tmp.data, sum(Plarge * DepthRange) / sum(DepthRange))
  }

  }
}

# Final Step - interpolate to get proportions for missing data.
new.output<- as.data.frame(new.output)
# logit transform
new.output$LogitP <- with(new.output, log(y / (1- y)))
fit.model <- lm(LogitP ~ Month * Site , data = new.output) # fit a month x site interaction model, no year or diel effect

new.output$Phat <- inv.logit(predict(fit.model, newdata = new.output))

new.output$pLarge <- new.output$y
na.index <- which(is.na(new.output$p))
new.output$pLarge[na.index] <- new.output$Phat[na.index]
size <- new.output %>%
  select(Year, Month, Site, Diel, pLarge)

return(size)
}
