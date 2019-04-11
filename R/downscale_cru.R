#' Downscale CRU TS data to 30 arcseconds using Worldclim data
#'
#' @param x a data.frame with at least id, lon, and lat
#' @param cru_path absolute path to the NetCDF file with CRU climate data
#' @param wc_dir absolute path to the WorldClim v2.0 TIF files for the respective climate parameter
#' @param varname the name of the variable in the CRU TS NetCDF file
#' @param mode either "auto", or one of "temp" or "prec"
#' @param parallel if TRUE (default), some parts of the code are run in parallel for speed gain
#'
#' @return a tidy data.frame with id, year, month, and the extracted climate variable
#' @import ncdf4
#' @importFrom dplyr select bind_rows arrange
#' @importFrom raster brick extract subset getZ mean
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom sp coordinates proj4string
#' @export
downscale_cru <- function(x, cru_path, wc_dir, varname, mode = "auto", parallel = TRUE) {

  if (mode == "auto") {
    if (varname %in% c("tavg", "tmin", "tmax", "tmean", "tmp", "temp")) {
      mode <- "temp"
    } else {
      mode <- "prec"
    }
  }

  anomaly_fun <- switch(mode,
                        "temp" = `-`,
                        "prec" = `/`)

  anomaly_rev_fun <- switch(mode,
                            "temp" = `+`,
                            "prec" = `*`)

  if (parallel & require("parallel")) {
    mfun <- parallel::mcmapply
    lfun <- parallel::mclapply
  } else {
    mfun <- mapply
    lfun <- lapply
  }

  crubrick <- raster::brick(cru_path, varname = varname)

  # month/date information
  dates <- raster::getZ(crubrick)
  years <- as.numeric(unique(substr(dates, 1, 4)))
  all_months <- as.numeric(substr(dates, 6, 7))
  month_seq_all <- lapply(1:12, function(x) which(all_months == x))
  climatology_start <- 1970 # for WorldClim 2.0
  climatology_end <- 2000   # for WorldClim 2.0
  clim_start_index <- which(years == climatology_start)
  clim_end_index <- which(years == climatology_end)
  month_seq_clim <- lapply(month_seq_all, function(x) x[clim_start_index:clim_end_index])

  # compute climatologies
  month_subsets_clim <- lfun(month_seq_clim, function(x) raster::subset(crubrick, x))
  climatology <- lfun(month_subsets_clim, function(x) raster::mean(x, na.rm = TRUE))

  # compute coarse anomalies
  month_subsets_all <- lfun(month_seq_all, function(x) raster::subset(crubrick, x))
  anomalies <- mfun(anomaly_fun, month_subsets_all, climatology)

  # interpolate anomalies to points of interest
  PROJ <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  sp::coordinates(x) <- ~ lon + lat
  sp::proj4string(x) <- PROJ
  e_anom <- lfun(anomalies, function(y) raster::extract(y, x, method = "bilinear"))

  # replay Worldclim anomalies
  wc <- lapply(list.files(wc_dir, full.names = TRUE, pattern = "[0-9]{2}\\.tif$"),
               raster::brick)
  e_wc <- lfun(wc, function(y) raster::extract(y, x, method = "bilinear"))
  down <- mfun(anomaly_rev_fun, e_anom, e_wc, SIMPLIFY = FALSE)
  for (i in seq_along(down)) {
    down[[i]] <- round(down[[i]], 2)
    down[[i]] <- as.data.frame(down[[i]])
    colnames(down[[i]]) <- years
    down[[i]]$id <- x$id
    down[[i]] <- tidyr::gather(down[[i]], year, !!varname, -id)
    down[[i]]$month <- i
  }

  # prepare nice output
  down <- dplyr::bind_rows(down) %>%
    dplyr::select(id, year, month, !!varname) %>%
    dplyr::arrange(id, year, month)

  down
}
