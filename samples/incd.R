library(meta)
library(jsonlite)

# make data
data <- list(
  study = c("a", "b"),
  Et = c(2, 0),
  Nt = c(9, 4)
)

sm <- "PFT"

# meta
rst <- metaprop(
  Et,
  Nt,
  data = data,
  studlab = study,
  sm = sm,
  method = "Inverse",
  method.tau = "DL",
  hakn = TRUE,
  adhoc.hakn = "se",
  backtransf = TRUE
)

rst$is.metabind <- inherits(rst, "metabind")
if (sm == "IRFT") {
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
rst$bt.TE <- backtransf(rst$TE, sm, harmonic.mean, harmonic.mean)
rst$bt.lower <- backtransf(rst$lower, sm, harmonic.mean, harmonic.mean)
rst$bt.upper <- backtransf(rst$upper, sm, harmonic.mean, harmonic.mean)

# random
rst$bt.TE.random <- backtransf(rst$TE.random, sm, harmonic.mean, harmonic.mean)
rst$bt.lower.random <- backtransf(rst$lower.random, sm, harmonic.mean, harmonic.mean)
rst$bt.upper.random <- backtransf(rst$upper.random, sm, harmonic.mean, harmonic.mean)

# fixed
rst$bt.TE.fixed <- backtransf(rst$TE.fixed, sm, harmonic.mean, harmonic.mean)
rst$bt.lower.fixed <- backtransf(rst$lower.fixed, sm, harmonic.mean, harmonic.mean)
rst$bt.upper.fixed <- backtransf(rst$upper.fixed, sm, harmonic.mean, harmonic.mean)

# common
rst$bt.TE.common <- backtransf(rst$TE.common, sm, harmonic.mean, harmonic.mean)
rst$bt.lower.common <- backtransf(rst$lower.common, sm, harmonic.mean, harmonic.mean)
rst$bt.upper.common <- backtransf(rst$upper.common, sm, harmonic.mean, harmonic.mean)
