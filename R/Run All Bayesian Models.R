#' run_all_bayesian_models
#'@description
#' fits all candidate bayesian models, saves the MCMC output and a namedd list with class c("psisi_loo")
#'
#' @param species what species to use.  either  "zoop", "fish", "Pacific Hake", or "Pacific Herring"
#' @param data.2.use what response metric to use.  Either "Sa", "dietcomp", or "fullness". The latter
#' two can only be used if species is Pacific Hake or Pacific Herring
#'
#' @return
#' stores .Rdata objects containing lists of outputs for each model, stored in outputs/modelselection/ folders
#'
#'
#'
#' @export
#'
#' @examples
#'

run_all_bayesian_models <- function(species, data.2.use) {
  require(lme4)
  require(rstan)
  require(loo)

  # see if need to make new subdirectorys, and if so, make them
  cur.dir <- dir()
  if (!"outputs" %in% cur.dir) dir.create("outputs")

  cur.dir <- dir("outputs/")
  if (!"modelselection" %in% cur.dir) dir.create("outputs/modelselection")

  cur.dir <- dir("outputs/modelselection")
  test.folder.name <- paste(species, data.2.use, sep = "")
  if (!test.folder.name %in% cur.dir) dir.create(paste("outputs/modelselection/", test.folder.name, sep = ""))


  outdir <-
    paste("outputs/modelselection/", species, data.2.use, sep = "")



  model.funs <-
    c(
      'Year + Diel',
      'Year + Diel + pLarge',
      'Year + Diel + Month',
      'Year + Diel + pLarge  + Month',
      'Year + Diel + meanDO',
      'Year + Diel + pLarge + meanDO',
      'Year + Diel + Month + meanDO',
      'Year + Diel + pLarge + Month + meanDO'
    )
  if (species %in% c("Pacific Herring", "Pacific Hake") &
      data.2.use == "Sa")
    model.funs <-
    model.funs <-
    c('Year ',
      'Year  + Month',
      'Year  + meanDO',
      'Year + Month + meanDO')

  if (species %in% c("zoop", "fish") &
      data.2.use == "Sa")
    model.funs <-
    model.funs <-
    c('Year + Diel',
      'Year  + Diel + Month',
      'Year  + Diel + meanDO',
      'Year + Diel + Month + meanDO')

  if (species %in% c("zoop", "fish") &
      data.2.use %in% c("dietcomp", "fullness"))  {
    print ("Error, dietcomp or fullness can only be specified for Pacific Hake or Pacific Herring")
  } else {
    model.name <- "Stan/fullmodelv1.1.stan"

    if (data.2.use %in% c("fullness", "dietcomp"))
      thedata <- load_dietdata(species, data.2.use)
    if (data.2.use == "Sa" &
        species %in% c("Pacific Herring", "Pacific Hake", "fish"))
      thedata <- load_sa_fish_data(species)
    if (data.2.use == "Sa" &
        species %in% c("euphausiid", "amphipod", "zoop"))
      thedata <- load_zoop_data()

    loo.outputs <- model.outputs  <- list()

    # loop through alternative models, then through alternative k-fold data sets
    for (j in 1:length(model.funs)) {
      model.fun <- model.funs[j]

      X <-
        as.matrix(model.matrix(object = eval(parse(
          text = paste("y~", model.fun, sep = "")
        )), data = thedata)[,-1])
      Z <-
        as.matrix(model.matrix(object = y ~ -1 + Site, data = thedata))
      Z <- matrix(Z, nrow = nrow(Z), ncol = ncol(Z))


      # Assign response variables for fitting data and out of sample test data
      y <-
        thedata$y - mean(thedata$y) # standardize so that mean = 1



      n.data <- nrow(thedata)
      Kx <- max(ncol(X), 1) # needed again for the final model
      Kz <- max(ncol(Z), 1)
      docol <- which(colnames(X) == "meanDO")
      if (length(docol) == 0)
        docol <- 0


      seobs <- thedata$se


      ############## Initialize and Run Stan #######################

      data <- list(
        N = n.data,
        Kx = Kx,
        Kz = Kz,
        y = y,
        X = X,
        Z = Z,
        seobs = seobs,
        usd_prior = 15,
        do_col = docol
      )
      initf2 <- function(chain_id = 1, Kx, Kx2, Kz) {
        # cat("chain_id =", chain_id, "\n")
        list(
          beta = array(rcauchy(Kx, 0, 0.5)),
          uraw = rnorm(Kz, 0, 1),
          ubar = rnorm(1, -3, 1.5),
          usd = runif(1, 0, 2),
          sigma = runif(1, 0, 10),
          do_thresh = runif(1, 2.5, 5)
        )
      }

      params = c(
        "beta",
        "do_thresh",
        "u",
        "sigma",
        "usd",
        "ubar",
        "y_pred_check",
        "usd",
        "do_effect",
        "log_lik"
      )

      #setwd("./src")
      n_chains <- 3
      iters <- 5000
      init_ll <- lapply(1:n_chains, function(id)
        initf2(
          chain_id = id,
          Kx = Kx,
          Kx2 = Kx2,
          Kz = Kz
        ))

      rstan_options(auto_write = TRUE)
      options(mc.cores = parallel::detectCores())

      model.stan = stan(
        file = model.name,
        data = data,
        iter = iters,
        par = params,
        warmup = floor(iters / 2),
        chains = 3,
        thin = 10,
        algorithm = 'NUTS',
        init = init_ll,
        verbose = F,
        control = list(adapt_engaged = TRUE, adapt_delta = 0.99)
      )

      # calculate LOO statistics and extract MCMC output, place in lists
      log_lik1 <- extract_log_lik(model.stan, merge_chains = FALSE)
      rel_n_eff <- relative_eff(exp(log_lik1))
      x <- loo(log_lik1,
               r_eff = rel_n_eff,
               cores = 2,
               K = 10)
      loo.outputs[[j]] <- x
      model.outputs[[j]] <- extract(model.stan)


    }
    # save into output director
    save.filename <- paste(outdir, "/", "modelploos.Rdata", sep = "")
    save(file = save.filename, loo.outputs)
    save.filename <- paste(outdir, "/", "model.Rdata", sep = "")
    save(file = save.filename, model.outputs)
  }
}
