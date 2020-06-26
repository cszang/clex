#' Interpolate (or just extract) monthly climate data
#'
#' @param x a data.frame with at least id, lon, and lat
#' @param nc_path absolute path to the NetCDF file with monthly climate data
#' @param varname the name of the climate variable in the NetCDF file
#' @param id character string indicating name of column in x that holds unique
#'   location ids
#' @param ... additional arguments passed to \code{raster::extract()}
#' @return a tidy data.frame with id, year, month, and the extracted climate
#'   variable
#' @keywords manip
#' @importFrom rlang !!
#' @export
interpolate <- function(x, nc_path, varname, id = "id", ...) {

  ids <- x$siteID
  PROJ <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  sp::coordinates(x) <- ~ lon + lat
  sp::proj4string(x) <- PROJ

  ncbrick <- raster::brick(nc_path, varname = varname)
  dates <- raster::getZ(ncbrick)
  y <- raster::extract(ncbrick, x, ...)
  y <- as.data.frame(y)
  colnames(y) <- as.character(dates)
  y$siteID <- ids
  y <- y %>%
    gather(date, !!varname, -siteID) %>%
    arrange(siteID, date) %>%
    mutate(year = as.numeric(substr(date, 1, 4)),
           month = as.numeric(substr(date, 6, 7))) %>%
    select(siteID, year, month, !!varname, -date)
  y
}