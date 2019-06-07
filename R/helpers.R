#' @keywords internal
automode <- function(.mode, .varname) {
  if (.mode == "auto") {
    if (.varname %in% c("tavg", "tmean", "tmp", "tg",
                        "tmin", "tmn", "tn",
                        "tmax", "tmx", "tx",
                        "temp")) {
      "temp"
    } else {
      if (.varname %in% c("prec", "pre", "rr", "precip")) {
        "prec"
      } else {
        stop("Variable name not recognised. Please provide 'temp' or 'prec' for `mode`.")
      }
    }
  } else {
    .mode
  }
}

#' @keywords internal
get_anomaly_fun <- function(mode) {
  switch(mode,
         "temp" = `-`,
         "prec" = `/`)
}

#' @keywords internal
get_anomaly_rev_fun <- function(mode) {
  switch(mode,
         "temp" = `+`,
         "prec" = `*`)
}

#' @keywords internal
get_mfun <- function(parallel) {
  if (parallel & requireNamespace("parallel", quietly = TRUE)) {
    parallel::mcmapply
  } else {
    mapply
  }
}

#' @keywords internal
get_lfun <- function(parallel) {
  if (parallel & requireNamespace("parallel", quietly = TRUE)) {
    parallel::mclapply
  } else {
    lapply
  }
}

