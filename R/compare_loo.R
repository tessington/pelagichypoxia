#' compare_loo
#'@description
#' use leave-one-out cross validation approximation for a collection of n models
#' and calculate the difference in expected log point density (and se), and also
#' calculate model weights.  Stan output must already exist
#'
#' @param species what species to use.  either "zoop", "fish", "Pacific Hake", or "Pacific Herring"
#' @param data.2.use what response variable to use either "Sa", "fullness", or "dietcomp"
#'
#' @return
#'  list(comp, stacking_wts)
#'
#'
#' @export
#'
#' @examples
#'
#'
#'
compare_loo <-function(species, data.2.use) {


filename <- paste("outputs/modelselection/",species, data.2.use,"/","modelploos.Rdata", sep = "")
load(filename)

if (data.2.use =="Sa") {

  loo1 <-loo.outputs[[1]]
  loo2 <- loo.outputs[[2]]
  loo3 <- loo.outputs[[3]]
  loo4 <- loo.outputs[[4]]


  comp <- loo_compare(loo1,
                      loo2,
                      loo3,
                      loo4)


  lpd_point <- cbind(
    loo1$pointwise[,"elpd_loo"],
    loo2$pointwise[,"elpd_loo"],
    loo3$pointwise[,"elpd_loo"],
    loo4$pointwise[,"elpd_loo"]

  )

  stacking_wts <- stacking_weights(lpd_point)

} else {
loo1 <-loo.outputs[[1]]
loo2 <- loo.outputs[[2]]
loo3 <- loo.outputs[[3]]
loo4 <- loo.outputs[[4]]
loo5 <- loo.outputs[[5]]
loo6 <- loo.outputs[[6]]
loo7 <- loo.outputs[[7]]
loo8 <- loo.outputs[[8]]


comp <- loo_compare(loo1,
                    loo2,
                    loo3,
                    loo4,
                    loo5,
                    loo6,
                    loo7,
                    loo8)

index <- sort(rownames(comp), index.return = T)


lpd_point <- cbind(
  loo1$pointwise[,"elpd_loo"],
  loo2$pointwise[,"elpd_loo"],
  loo3$pointwise[,"elpd_loo"],
  loo4$pointwise[,"elpd_loo"],
  loo5$pointwise[,"elpd_loo"],
  loo6$pointwise[,"elpd_loo"],
  loo7$pointwise[,"elpd_loo"],
  loo8$pointwise[,"elpd_loo"]

)

stacking_wts <- stacking_weights(lpd_point)
}
return(list(comp = comp, stacking_wts = stacking_wts))
}
