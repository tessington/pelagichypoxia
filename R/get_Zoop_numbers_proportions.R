
#' get_Zoop_numbers_proportions
#'@description
#' calculate numerical proportions of zooplankton sampled in multi-nets for multiple taxa
#'
#'@param species  which zooplankton species or species group should be used.  Must match one of the column
#'    names in data:zoopcomp
#'
#' @return
#'  a vector containing estimated numerical proportions for each taxa
#'
#'
#' @export
#'
#' @examples
#'
get_Zoop_numbers_proportions <- function(species) {
  require(dplyr)
  require(KernSmooth)

# load data
pelagichypoxia::zoopcomp
output <- zoopcomp # this is just to match old code variable names
# turn all counts into proportions
ntotal <- rowSums(zoopcomp[,7:37])
col.index <- which(colnames(output)==species)

output$Pspecies <- zoopcomp[,col.index] / ntotal

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
    new.output$pnum <- NA


pelagichypoxia::netdepths

    for (i in 1:nrow(new.output)) {
      # extract out entries in output that match this
      tmp.data <- output %>%
        filter(Year == new.output$Year[i],
               SiteName == new.output$Site[i],
               MonthName == new.output$Month[i],
               Diel == new.output$Diel[i]
        )
      n.tmp.data <- nrow(tmp.data)

      if(n.tmp.data == 1) new.output$pnum[i] <- tmp.data$Pspecies

      # if there are multiples
      if(n.tmp.data > 1) {
        # first check to see how many of these sample layers have euphausiid counts
        tmp.data <- tmp.data %>%
          filter(Pspecies != "NA")
        # if only 1, it is ready
        if (nrow(tmp.data) == 1) new.output$pnum[i] <- tmp.data$Pspecies
        # if more than one, weight by depth range
        if (nrow(tmp.data) >1) {
          #look up depth range
          for (j in 1:nrow(tmp.data)) {
            tmp.net.depth <- filter(netdepths, Net == tmp.data$Filename[j])
            tmp.data$DepthRange[j] <- with(tmp.net.depth, Lower  - Upper)
          }
          # get weighted average
          new.output$pnum[i] <- with(tmp.data, sum(Pspecies* DepthRange) / sum(DepthRange))
        }

      }
    }

  return(new.output)
}

