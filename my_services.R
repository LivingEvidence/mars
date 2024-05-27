library(meta)
library(jsonlite)

###########################################################
# Incidence Analysis
###########################################################
#* Run incidence analysis on the given input data frame
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

  ret_str <- toJSON(list(
    incdma = rst,
    primma = c(),
    cumuma = c(),
    version = list(
      jsonlite = packageVersion("jsonlite"),
      meta = packageVersion("meta")
    )
  ), force = TRUE)

  fromJSON(ret_str)
}


###########################################################
# Pairwise meta-analysis
###########################################################


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