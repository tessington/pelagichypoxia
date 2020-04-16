#' load_sa_fish_data
#'@description
#' extracts the acoustic backscatter for zooplankton
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

load_zoop_data <- function() {
  Sazdata <- pelagichypoxia::zoop_backscatter

  # calculate proportion of total NASC that is euphasuiid
  new.output <- get_euphausiid_numbers()
  # merge together p.nasc in object new.output) into Saz data
  Sazdata <- merge(Sazdata, new.output, by = c("Year", "Month", "Site", "Diel") )
  # now load DO data
  theDOdata <- formatDOdataFN()

  thedata <-
    merge(
      Sazdata,
      theDOdata,
      by = c("Year", "Month", "Site", "Diel"),
      all.x = T
    )
  thedata$y <- log(thedata$Sa)

  thedata$se <- 0

  size.data <- read.csv(file  = "Data/plargeEuph.csv", header = T)
  thedata$plarge <- size.data$pLarge



  return(thedata)
}
