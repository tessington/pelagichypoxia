#' get_euphausiid_numbers
#'@description
#' use the multi-net tow data to estimate the fraction of NASC likely attributable to euphausiids.
#'
#'
#' @return
#'  a data matrix given the euphausiid numerical proportion for each sampling event, and the proportion of NASC attributable to euphausiids
#'
#'
#' @export
#'
#' @examples
#'
#'
#'

get_euphausiid_numbers <- function() {

new.output <- get_Zoop_numbers_proportions("euphausiid")

wgt.factor <- 116 # ratio of krill to copepod backscatter
new.output$pnasc <- new.output$pnum * wgt.factor / ( new.output$pnum * wgt.factor + (1 - new.output$pnum))

return(new.output)
}
