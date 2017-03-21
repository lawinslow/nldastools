#' @title Fast multi-lat/lon driver lookup
#' 
#' @param lons Vector of longitudes
#' @param lats Vector of latitudes
#' @param ids Site ids for mapping of data to lats/lons
#' 
#' 
#' 
#' 
#' @export
get_primary_forcing_local_multitool = function(lons, lats, ids){
  
  nldas_path = file.path(get_local_storage(), 'driver_ncdf4_NLDAS')
  
  nldas_files = Sys.glob(paste0(nldas_path, '/*.nc4'))
  vardata = list()

    
  drivernc = nc_open(nldas_files[1])
  driverlat = ncvar_get(drivernc, 'lat_110')
  driverlon = ncvar_get(drivernc, 'lon_110')
  nc_close(drivernc)
  xi = rep(NA, length(lons))
  yi = rep(NA, length(lons))
  
  for(i in 1:length(lons)){
    xi[i] = which.min(abs(lons[i] - driverlon))
    yi[i] = which.min(abs(lats[i] - driverlat))
    
    if(abs(lons[i] - driverlon[xi[i]]) > median(diff(driverlon)) ||
       abs(lats[i] - driverlat[yi[i]]) > median(diff(driverlat))){
      
      xi[i] = NA
      yi[i] = NA
    }
  }
  
  idxy = data.frame(ids, xi, yi)
  idxy$index = NA
  uxy  = unique(idxy[, c('xi', 'yi')])
  
  out = list()
  for(i in 1:nrow(uxy)){
    
    if(is.na(uxy$xi[i])){
      out[[i]] = data.frame()
      next
    }
    
    matches = which(idxy$xi == uxy$xi[i] & idxy$yi == uxy$yi[i])
    idxy[matches, 'index'] = i
    
    out[[i]] = .get_primary_forcing_local(uxy$xi[i], uxy$yi[i])
  }
  
  return(list(out, idxy))
}


.get_primary_forcing_local = function(xi, yi){
  
  nldas_path = file.path(get_local_storage(), 'driver_ncdf4_NLDAS')
  
  nldas_files = Sys.glob(paste0(nldas_path, '/*.nc4'))
  
  vardata = list()
  
  for(i in 1:length(nldas_files)){
    
    drivernc = nc_open(nldas_files[i])
    drivertime = ncvar_get(drivernc, 'time')
    
    onevar = ncvar_get(drivernc, drivernc$var[[1]]$name, start = c(xi, yi, 1), count = c(1, 1, -1))
    
    nc_close(drivernc)
    
    vardata[[i]] = data.frame(drivertime, onevar)
    names(vardata[[i]]) = c('datetime', drivernc$var[[1]]$name)
    
  }
  mergedvars = Reduce(function(...) merge(..., by='datetime'), vardata)
  mergedvars$datetime = as.POSIXct(mergedvars$datetime, origin='1970-01-01', tz='UTC')
  
  return(mergedvars)
}