#' euphausiid_lengths
#' @format a list, with names elements corresponding to individual multinet tows.  Each sample is an measured euphausiid length
#'
#'
"euphausiid_lengths"

#' netdepths
#' @format a dataframe containing lower and upper bound for each multinet tow
#' \describe{
#'  \item{Net}{alphanumeric code for tow file}
#'  \item{lower}{lower depth}
#'  \item{upper}{upper depth}
#' }
#'
#'
"netdepths"

#' zoopcomp
#' @format a dataframe containing counts of zooplankton taxa for each multinet tow
#' \describe{
#'  \item{Year}{year of sample}
#'  \item{Month}{numeric version of month}
#'  \item{Day}{day of sample}
#'  \item{Site}{site, listed in 2 letter abbreviations, UN = Union, HP = hoodsport, DU = duckabush, DB = Dabob bay}
#'  \item{Diel}{day or night sample}
#'  \item{Filename}{original filename, designates alpha-numeric multinet tow ID}
#'  \item{species names}{several columns indicating individual taxa}
#'  \item{Monthname}{three character month abbreviations, matches what is used in other datasets}
#'  \item{Sitename}{turn Site into A, B, C, D to match other datasets}
#' }
#'
#'
"zoopcomp"

#' ctd_data
#' @format a large list that takes a matlab structure object giving ctd data in 1m depth bins
#' \describe{
#'  \item{hc}{the element of the list that matters: contains depth (m) first element, temperature in 2nd element , dissolved oxygen in the 16th element, and 14th has all of the sample information}
#' }
#'
#'
"ctd_data"

#' feeding_data
#' @format a two element list that has all of the measured feeding metrics for each species
#' \describe{
#'  \item{ci}{the fraction of diet that is euphauiids}
#'  \item{ci_SE}{estimated standard error of diet fraction}
#'  \item{logfullness}{ratio of stomach contents to fish length^3}
#'  \item{selogfullness}{standard error of ratio of stomach contents to fish length^3}
#'  \item{N}{samplesize}
#'  \item{logit.ci}{logit of diet fraction}
#'  \item{logit.ci.se}{standard error of logit of diet fraction}
#' }
#'
#'
"feeding_data"

#' fish_backscatter
#' @format a three element list that has all of the measured feeding metrics for combined fish, Pacific Herring and Pacific Hake
#' \describe{
#'  \item{Year}{Year}
#'  \item{Month}{Month}
#'  \item{Diel}{day or night sample?}
#'  \item{A}{site A, Union}
#'  \item{B}{site B, Hoodsport}
#'  \item{C}{site C, Duckabush}
#'  \item{D}{site D, Dabob Bay}

#' }
#'
#'
"fish_backscatter"

#' zoop_backscatter
#' @format a data frame containing acoustic backscatter classified as zooplankton, for each sampling event
#'
#'
"zoop_backscatter"
