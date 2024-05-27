library(meta)

###########################################################
# Incidence Analysis
###########################################################
#* Run incidence analysis on the given input data frame
#* @serializer unboxedJSON
#* @param data The dataframe to be analyzed
#* @param cfg The configuration to be used in the analysis
#* @post /INCD
incd <- function(data, cfg){
  rst <- metaprop(
    Et,
    Nt,
    data = data,
    studlab = study,
    sm = cfg$sm,
    method = cfg$pooling_method,
    method.tau = cfg$tau_estimation_method,
    hakn = cfg$hakn_adjustment,
    adhoc.hakn = cfg$adhoc_hakn,
    backtransf = TRUE
  )

  # run backtransf here
  rst <- backtransf_pwma(rst)

  # a temp result
  j <- list(
    incdma = rst,
    primma = c(),
    cumuma = c()
  )

  # convert to the format expected by the client
  ret <- list(
    submission_id = format(Sys.time(), "%Y%m%d%H%M%OS"),
    params = cfg,
    success = TRUE,
    data = list(
      incdma = meta_trans_metaprop(j, cfg)
    )
  )

  return(ret)
}


###########################################################
# Pairwise meta-analysis
###########################################################

#* Run pairwise meta-analysis on the given input data frame
#* @serializer unboxedJSON
#* @param data The dataframe to be analyzed
#* @param cfg The configuration to be used in the analysis
#* @post /PWMA
pwma <- function(data, cfg) {

}

###########################################################
# Network meta-analysis
###########################################################



###########################################################
# Example services
###########################################################

#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
function(msg = ""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot out data from the iris dataset
#* @param spec If provided, filter the data to only this species (e.g. 'setosa')
#* @get /plot
#* @serializer png
function(spec){
  data <- iris
  title <- "All Species"

  # Filter if the species was specified
  if (!missing(spec)) {
    title <- paste0("Only the '", spec, "' Species")
    data <- subset(iris, Species == spec)
  }

  plot(
    data$Sepal.Length, data$Petal.Length,
    main = title,
    xlab = "Sepal Length",
    ylab = "Petal Length"
  )
}


###########################################################
# Helper services
###########################################################

#' Backtransform
#' 
#' @description Backtransform the results of a pairwise meta-analysis
#' @param rst The result of the pairwise meta-analysis
#' @return The result of the pairwise meta-analysis with backtransformed values
backtransf_pwma <- function(rst) {
  rst$is.metabind <- inherits(rst, "metabind")
  if (rst$sm == "IRFT") {
    if (rst$is.metabind) {
      harmonic.mean <- rst$t.harmonic.mean.ma
    } else {
      harmonic.mean <- 1 / mean(1 / rst$time)
    }
  } else {
    if (rst$is.metabind) {
      harmonic.mean <- rst$n.harmonic.mean.ma
    } else {
      harmonic.mean <- 1 / mean(1 / rst$n)
    }
  }

  # all values needs to be backtransformed
  # for each study, just use its own n
  rst$bt.TE <- backtransf(rst$TE, rst$sm, rst$n, rst$n)
  rst$bt.lower <- backtransf(rst$lower, rst$sm, rst$n, rst$n)
  rst$bt.upper <- backtransf(rst$upper, rst$sm, rst$n, rst$n)

  # random
  rst$bt.TE.random <- backtransf(rst$TE.random, rst$sm, harmonic.mean, harmonic.mean)
  rst$bt.lower.random <- backtransf(rst$lower.random, rst$sm, harmonic.mean, harmonic.mean)
  rst$bt.upper.random <- backtransf(rst$upper.random, rst$sm, harmonic.mean, harmonic.mean)

  # fixed
  rst$bt.TE.fixed <- backtransf(rst$TE.fixed, rst$sm, harmonic.mean, harmonic.mean)
  rst$bt.lower.fixed <- backtransf(rst$lower.fixed, rst$sm, harmonic.mean, harmonic.mean)
  rst$bt.upper.fixed <- backtransf(rst$upper.fixed, rst$sm, harmonic.mean, harmonic.mean)

  # common
  rst$bt.TE.common <- backtransf(rst$TE.common, rst$sm, harmonic.mean, harmonic.mean)
  rst$bt.lower.common <- backtransf(rst$lower.common, rst$sm, harmonic.mean, harmonic.mean)
  rst$bt.upper.common <- backtransf(rst$upper.common, rst$sm, harmonic.mean, harmonic.mean)

  return(rst)
}


#' Reformat the results of a pairwise meta-analysis
#' @description Reformat the results of a pairwise meta-analysis
#' @param ret The result of the pairwise meta-analysis
#' @return The reformatted result of the pairwise meta-analysis
meta_trans_metaprop <- function(rst, cfg){
  data <- rst$incdma
  ret <- list(
    model = list(
      random = list(
        name = "Random effects model",
        E = sum(data$event)[1],
        N = sum(data$n)[1],
        TE = data$TE.random[1],
        seTE = data$seTE.random[1],
        lower = data$lower.random[1],
        upper = data$upper.random[1],

        bt_TE = data$bt.TE.random[1],
        bt_lower = data$bt.lower.random[1],
        bt_upper = data$bt.upper.random[1],
        w = 1
      ),
      fixed = list(
        name = "Fixed effects model",
        E = sum(data$event)[1],
        N = sum(data$n)[1],
        TE = data$TE.fixed[1],
        seTE = data$seTE.fixed[1],
        lower = data$lower.fixed[1],
        upper = data$upper.fixed[1],

        bt_TE = data$bt.TE.fixed[1],
        bt_lower = data$bt.lower.fixed[1],
        bt_upper = data$bt.upper.fixed[1],
        w = 1
      )
    ),
    heterogeneity = list(
      i2 = data$I2[1],
      tau2 = data$tau2[1],
      p = data$pval.Q[1]
    )
  )

  # add all records
  ret$stus <- lapply(seq_along(data$studlab), function(i) {
    list(
      name = data$studlab[i],
      E = data$event[i],
      N = data$n[i],
      TE = data$TE[i],
      seTE = data$seTE[i],
      lower = data$lower[i],
      upper = data$upper[i],

      bt_TE = data$bt.TE[i],
      bt_lower = data$bt.lower[i],
      bt_upper = data$bt.upper[i],

      w_random = data$w.random[i] / sum(data$w.random),
      w_fixed = data$w.fixed[i] / sum(data$w.fixed)
    )
  })

  return(ret)
}