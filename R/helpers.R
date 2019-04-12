#' @keywords internal
automode <- function(.mode, .varname) {
  if (.mode == "auto") {
    if (.varname %in% c("tavg", "tmin", "tmax", "tmean", "tmp", "temp", "tg")) {
      "temp"
    } else {
      "prec"
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
