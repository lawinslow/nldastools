#' @title Get primary forcing data from local cache
#'
#'
#'
#'
#' @import ncdf4
#' @importFrom data.table rbindlist
#'
#' @export
get_primary_forcing_local = function(lon, lat, start=as.POSIXct('1979-01-02', tz='UTC'), stop=as.POSIXct('2017-01-01', tz='UTC')){
  nldas_path = file.path(get_local_storage(), 'driver_ncdf4_NLDAS')

  nldas_files = Sys.glob(paste0(nldas_path, '/*.nc4'))
  vardata = list()

  for(i in 1:length(nldas_files)){
    drivernc = nc_open(nldas_files[i])
    driverlat = ncvar_get(drivernc, 'lat_110')
    driverlon = ncvar_get(drivernc, 'lon_110')
    drivertime = ncvar_get(drivernc, 'time')

    xi = which.min(abs(lon - driverlon))
    yi = which.min(abs(lat - driverlat))

    if(abs(lon - driverlon[xi]) > median(diff(driverlon)) ||
       abs(lat - driverlat[yi]) > median(diff(driverlat))){
      stop('requested lat/lon is too far outside of data range')
    }
    
    onevar = ncvar_get(drivernc, drivernc$var[[1]]$name, start = c(xi, yi, 1), count = c(1, 1, -1))

    vardata[[i]] = data.frame(drivertime, onevar)
    names(vardata[[i]]) = c('datetime', drivernc$var[[1]]$name)
  }
  mergedvars = Reduce(function(...) merge(..., by='datetime'), vardata)
  mergedvars$datetime = as.POSIXct(mergedvars$datetime, origin='1970-01-01', tz='UTC')

  return(mergedvars)
}

