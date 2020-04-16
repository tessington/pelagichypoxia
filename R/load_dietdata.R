#' load_dietdata
#'@description
#' extracts either stomach fullness and diet composition (fraction of diets that were euphausiids)
#' for each sampling event, returns a data frame
#' @param species what species to use.  either "Pacific Hake", or "Pacific Herring"
#' @param data.2.use what metric to extract, either "dietcomp" or "fullness"
#'
#' @return
#'
#'
#'
#' @export
#'
#' @examples
#'
load_dietdata<-function(species, data.2.use) {

# get Diet data
pelagichypoxia::feeding_data


dietdata <- feeding_data[[which(names(feeding_data)==species)]]


# Create a Year, Month, Site, Diel data frame
predict.X <- as.data.frame(
  cbind(
    c(rep(2012,40),rep(2013,40)),
    rep(c("day","night"),times = 40),
    rep(c("Jun","Jul","Aug","Sep","Oct"),each = 8, times =1),
    rep(c("A","B","C","D"), each = 2, times = 10)))
names(predict.X) <- c("Year","Diel","Month","Site")
predict.X$Month <-
  with(predict.X, factor(Month, levels = c("Jun", "Jul", "Aug", "Sep", "Oct")))


theDOdata <- formatDOdataFN()

# attach DO to diet data
dietdata <- merge(dietdata, theDOdata, by = c("Year", "Month", "Site", "Diel"), all.x = T)

dietdata <- dietdata %>%
  select(
    Year,
    Month,
    Diel,
    Site,
    ci,
    ci_SE,
    logfullness,
    selogfullness,
    N,
    meanDO
  )


dietdata$Year <- as.factor(dietdata$Year)
dietdata$meanDO <- as.numeric(dietdata$meanDO)


# Delta method to get approximate SE of logit (ci)
dietdata$logit.ci <- logit(dietdata$ci)
dietdata$logit.ci.se <- logit.mod.se(dietdata$ci) * dietdata$ci_SE

if (data.2.use == "dietcomp") {
thedata <- dietdata %>%
  filter(!is.na(logit.ci), N > 3)
thedata$y <- thedata$logit.ci # makes it easier to call to response variable 'y'
thedata$se <- thedata$logit.ci.se

}
if (data.2.use == "fullness") {
  thedata <- dietdata %>%
    filter(!is.na(logfullness), N > 3)
  thedata$y <- thedata$logfullness
  thedata$se <- thedata$selogfullness
}

size.data <- get_euphausiid_lengths()

thedata <- merge(thedata, size.data)
thedata <- thedata %>%
  select(Year, Month, Diel, Site, meanDO, y, se, pLarge)

return(thedata)
}
