#' @title Extract ISIMIP drivers from local netcdf cache
#' 
#' @inheritParams get_primary_forcing_local
#' 
#' 
#' 
#' 
#' 
#' @export
get_isimip_local = function(lon, lat, scenario=c('rcp60', 'rcp26'), model=c('GFDL-ESM2M')){
  
  varnames = c("hurs", "huss", "pr",   "prsn", "ps",   "psl",  "rlds", "rsds", "sfcWind", "tas", "tasmax","tasmin")
  nc_cache = 'B:/big_data/ISIMIP'
  
  
  date_from_filename = function(filename){
    as.Date(lubridate::ymd(stringr::str_match(basename(filename), '([0-9]{8})-')[,2]))
  }
  
  
  fullvars = list()
  
  for(i in seq_along(varnames)){
    
    files = c(Sys.glob(file.path(nc_cache, 'historical', modname, paste0(varnames[i], '_*'))),
                 Sys.glob(file.path(nc_cache, scenario, modname, paste0(varnames[i], '_*'))))
    
    times = as.Date(sapply(files, date_from_filename, USE.NAMES = FALSE), origin='1970-01-01')
    files = files[order(times)]
    
    vardata = list()
    
    for(j in seq_along(files)){
      cat(sprintf('On File#: %i of Var: %s\n ', j, varnames[i]))
      drivernc = nc_open(files[j])
      
      driverlat = ncvar_get(drivernc, 'lat') #+ ncatt_get(drivernc, 'rotated_pole', 'grid_north_pole_latitude')$value
      driverlon = ncvar_get(drivernc, 'lon') #+ ncatt_get(drivernc, 'rotated_pole', 'grid_north_pole_longitude')$value
      if(min(driverlon) < -180){
        driverlon = driverlon + 180
      }
      
      
      #hmm = ncvar_get(drivernc, vars[i])
      #image(driverlon+180, driverlat, hmm[,,8])
      
      #drivertime = as.Date(ncvar_get(drivernc, 'time'), origin='1949-12-01')
      #drivertime = as.PCICt.numeric(ncvar_get(drivernc, 'time')*60*24*60, cal='365', origin='1949-12-01')
      
      if(ncatt_get(drivernc, 'time', 'units')$value == 'days since 2006-1-1 00:00:00'){
        drivertime = as.POSIXct(ncvar_get(drivernc, 'time')*60*60*24, origin='2006-01-01')
      }else{
        drivertime = as.POSIXct(ncvar_get(drivernc, 'time')*60*60*24, origin='1861-01-01')
      }
      
      
      xi = which.min(abs(lon - driverlon))
      yi = which.min(abs(lat - driverlat))
      
      if(abs(lon - driverlon[xi]) > abs(median(diff(driverlon))) ||
         abs(lat - driverlat[yi]) > abs(median(diff(driverlat)))){
        stop('requested lat/lon is too far outside of data range')
      }
      
      onevar = ncvar_get(drivernc, varnames[i], start = c(xi, yi, 1), count = c(1, 1, -1))
      nc_close(drivernc)
      
      vardata[[j]] = data.frame(as.POSIXct(drivertime), onevar)
      names(vardata[[j]]) = c('datetime', varnames[i])
      
    }
    
    fullvars[[i]] = dplyr::bind_rows(vardata)
  }
  
  dout = plyr::join_all(fullvars, by='datetime')
  return(dout)
  
}