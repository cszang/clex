#' Downscale monthly climate data to 30 arcseconds using Worldclim data
#'
#' Currently, CRU TS 4.01 is supported out of the box; for E-OBS > 18e, NetCDF
#' files with monthly resolution have to be produced first, e.g. using CDO
#' commands.
#' @param x a data.frame with at least id, lon, and lat
#' @param nc_path absolute path to the NetCDF file with monthly climate data
#' @param wc_dir absolute path to the WorldClim v2.0 TIF files for the
#'   respective climate parameter
#' @param varname the name of the climate variable in the NetCDF file
#' @param mode either "auto", or one of "temp" or "prec"
#' @param id character string indicating name of column in x that holds unique
#'   location ids
#' @param parallel if TRUE (default), some parts of the code are run in parallel
#'   for speed gain
#' @param use_chelsa if TRUE, use CHELSA data instead of WorldClim for
#'   downscaling
#' @param wc_bilinear if TRUE, bilinearly interpolate worldclim data as well
#' @return a tidy data.frame with id, year, month, and the extracted climate
#'   variable
#' @references Mosier, T. M., Hill, D. F., & Sharp, K. V. (2014). 30-Arcsecond
#'   monthly climate surfaces with global land coverage. International Journal
#'   of Climatology, 34(7), 2175–2188. https://doi.org/10.1002/joc.3829
#' @keywords manip
#' @importFrom rlang !!
#' @export
downscale <- function(x, nc_path, wc_dir, varname, mode = "auto",
                      id = "id", parallel = TRUE, use_chelsa = FALSE,
                      wc_bilinear = FALSE) {

  mode <- automode(mode, varname)
  anomaly_fun <- get_anomaly_fun(mode)
  anomaly_rev_fun <- get_anomaly_rev_fun(mode)
  mfun <- get_mfun(parallel)
  lfun <- get_lfun(parallel)

  ncbrick <- raster::brick(nc_path, varname = varname)

  # month/date information
  dates <- raster::getZ(ncbrick)
  years <- as.numeric(unique(substr(dates, 1, 4)))
  all_months <- as.numeric(substr(dates, 6, 7))
  # this is needed for data that does not cover entire years, like e-obs18e
  all_years <- as.numeric(substr(dates, 1, 4))
  myears <- lapply(1:12, function(x) all_years[which(all_months == x)])
  month_seq_all <- lapply(1:12, function(x) which(all_months == x))
  climatology_start <- 1970 # for WorldClim 2.0
  climatology_end <- 2000   # for WorldClim 2.0
  clim_start_index <- which(years == climatology_start)
  clim_end_index <- which(years == climatology_end)
  month_seq_clim <- lapply(month_seq_all, function(x) x[clim_start_index:clim_end_index])

  # compute climatologies
  month_subsets_clim <- lfun(month_seq_clim, function(x) raster::subset(ncbrick, x))
  climatology <- lfun(month_subsets_clim, function(x) raster::mean(x, na.rm = TRUE))

  # compute coarse anomalies
  month_subsets_all <- lfun(month_seq_all, function(x) raster::subset(ncbrick, x))
  anomalies <- mfun(anomaly_fun, month_subsets_all, climatology)

  # interpolate anomalies to points of interest
  PROJ <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  sp::coordinates(x) <- ~ lon + lat
  sp::proj4string(x) <- PROJ
  e_anom <- lfun(anomalies, function(y) raster::extract(y, x, method = "bilinear"))

  # replay Worldclim anomalies
  if (use_chelsa) {
    wc_pattern <- "land\\.tif$"
  } else {
    wc_pattern <- "[0-9]{2}\\.tif$"
  }

  wc <- lapply(list.files(wc_dir, full.names = TRUE, pattern = wc_pattern),
               raster::brick)
  if (wc_bilinear) {
    e_wc <- lfun(wc, function(y) raster::extract(y, x, method = "bilinear")[, 1])
  } else {
    e_wc <- lfun(wc, function(y) raster::extract(y, x)[, 1])
  }
  down <- mfun(anomaly_rev_fun, e_anom, e_wc, SIMPLIFY = FALSE)
  for (i in seq_along(down)) {
    down[[i]] <- round(down[[i]], 2)
    down[[i]] <- as.data.frame(down[[i]])
    colnames(down[[i]]) <- myears[[i]]
    down[[i]][[id]] <- x[[id]]
    down[[i]] <- tidyr::gather(down[[i]], year, !!varname, -id)
    down[[i]]$year <- as.numeric(down[[i]]$year)
    down[[i]]$month <- i
  }

  # prepare nice output
  down <- Reduce(rbind, down)
  down <- down[c(id, "year", "month", varname)]
  down <- down[order(down[[id]], down$year, down$month), ]
  rownames(down) <- NULL
  down
}
