#' @title Get CORDEX driver data
#' 
#' @description 
#' Returns full time series of CORDEX data based on requested model and lat/lon.
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' @importFrom plyr join_all
#' @import PCICt
#' 
#' 
#' 
#' @export
get_cordex_local = function(lon, lat, scenario=c('historical', 'rcp26', 'rcp45', 'rcp85'), model='NAM-44_CCCma-CanESM2'){
  
  models = c('NAM-44_CCCma-CanESM2', 'NAM-44_ICHEC-EC-EARTH', 'NAM-44_MPI-M-MPI-ESM-LR')
  
  Sys.glob(file.path('b:/big_data/esgf', paste0('*_', model, '_', scenario, '_*')))
  
  vars = c('huss', 'pr', 'prsn', 'ps', 'rlds', 'rsds', 'tas', 'uas', 'vas')
  fullvars = list()
  
  for(i in seq_along(vars)){
    
    files = Sys.glob(file.path('b:/big_data/esgf', paste0(vars[i], '_', model, '_', scenario, '_*')))
    vardata = list()
    
    for(j in seq_along(files)){
      
      drivernc = nc_open(files[j])
      
      driverlat = ncvar_get(drivernc, 'rlat') + ncatt_get(drivernc, 'rotated_pole', 'grid_north_pole_latitude')$value
      driverlon = ncvar_get(drivernc, 'rlon') + ncatt_get(drivernc, 'rotated_pole', 'grid_north_pole_longitude')$value
      if(min(driverlon) < -180){
        driverlon = driverlon + 180
      }
      
      
      #hmm = ncvar_get(drivernc, vars[i])
      #image(driverlon+180, driverlat, hmm[,,8])
      
      #drivertime = as.Date(ncvar_get(drivernc, 'time'), origin='1949-12-01')
      drivertime = as.PCICt.numeric(ncvar_get(drivernc, 'time')*60*24*60, cal='365', origin='1949-12-01')
      
      xi = which.min(abs(lon - driverlon))
      yi = which.min(abs(lat - driverlat))
      
      if(abs(lon - driverlon[xi]) > median(diff(driverlon)) ||
         abs(lat - driverlat[yi]) > median(diff(driverlat))){
        stop('requested lat/lon is too far outside of data range')
      }
      
      onevar = ncvar_get(drivernc, vars[i], start = c(xi, yi, 1), count = c(1, 1, -1))
      nc_close(drivernc)
      
      vardata[[j]] = data.frame(as.POSIXct(drivertime), onevar)
      names(vardata[[j]]) = c('datetime', vars[i])
      
    }
    
    fullvars[[i]] = dplyr::bind_rows(vardata)
  }
  
  dout = plyr::join_all(fullvars, by='datetime')
  
  return(dout)
}
