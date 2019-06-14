#' Add drought indices to a data.frame of climate variables
#'
#' Currently add DMI, and SPEI and SPI for different scales. PET for SPEI is
#' computed according to Hargreaves.
#' @param x a data.frame with climate data, that has at least id for location
#'   id, lat for latitude, year, month, tmean, tmin, and tmax for mean, minimum,
#'   and maximum temperature, and prec for precipitation.
#' @param scales for which temporal scales should spei and spi be computed
#' @param id character string for location id variable
#' @param lat character string for lat variable
#' @param tmean character string for tmean variable
#' @param tmin character string for tmin variable
#' @param tmax character string for tmax variable
#' @param prec character string for prec variable
#' @param drop_lat should latitude be dropped from result?
#' @return a data.frame consisting of the original climate data with additional
#'   variables for drought indices
#' @export
add_indices <- function(x, scales = c(1, 2, 3, 6, 12), id = "id", lat = "lat", tmean = "tmean", tmin = "tmin", tmax = "tmax", prec = "prec", drop_lat = TRUE) {
  ids <- unique(x[[id]])
  n <- length(ids)
  n_scales <- length(scales)
  out <- list()
  prog <- dplyr::progress_estimated(n)
  for (i in 1:n) {
    prog$tick()$print()
    .x <- x[x[[id]] == ids[i], ]
    if (!any(is.na(.x$prec)) & !any(is.na(.x$tmean)) &
          !any(is.na(.x$tmin)) & !any(is.na(.x$tmax))) {
      .x$dmi <- round((.x[[prec]]/10)/(.x[[tmean]] + 10), 2)
      .lat <- .x$lat[1]
      ts_tmin <- stats::ts(.x[[tmin]], start = c(.x$year[1], .x$month[1]), frequency = 12)
      ts_tmax <- stats::ts(.x[[tmax]], start = c(.x$year[1], .x$month[1]), frequency = 12)
      ts_prec <- stats::ts(.x[[prec]], start = c(.x$year[1], .x$month[1]), frequency = 12)
      ts_pet <- SPEI::hargreaves(ts_tmin, ts_tmax, Ra = NA, lat = .lat, Pre = ts_prec)
      ts_cwb <- ts_prec - ts_pet
      spei <- lapply(scales, function(x) round(SPEI::spei(ts_cwb, scale = x)$fitted, 2))
      spi <- lapply(scales, function(x) round(SPEI::spi(ts_prec, scale = x)$fitted, 2))
      for (j in 1:n_scales) {
        spei_name <- paste0("spei", scales[j])
        .x[[spei_name]] <- as.vector(spei[[j]])
      }
      for (j in 1:n_scales) {
        spi_name <- paste0("spi", scales[j])
        .x[[spi_name]] <- as.vector(spi[[j]])
      }
    } else {
      ## write NA dummy output
      .x$dmi <- NA_real_
      for (j in 1:n_scales) {
        spei_name <- paste0("spei", scales[j])
        .x[[spei_name]] <- NA_real_
      }
      for (j in 1:n_scales) {
        spi_name <- paste0("spi", scales[j])
        .x[[spi_name]] <- NA_real_
      }
    }
    out[[i]] <- .x
  }
  
  out <- Reduce(rbind, out)
  if (drop_lat) {
    out[[lat]] <- NULL
  }
  out
}
