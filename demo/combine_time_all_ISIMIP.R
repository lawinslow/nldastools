#combine all time ISIMIP
library(ncdf4)
library(sp)
library(dplyr)
library(abind)

varnames = c("hurs", "huss", "pr",   "prsn", "ps",   "psl",  "rlds", "rsds", "sfcWind", "tas", "tasmax","tasmin")
modnames = c('GFDL-ESM2M')

Sys.setenv(TZ='UTC')

nc_cache  = 'B:/big_data/ISIMIP/'
#out_cache = 'B:/big_data/ISIMIP/collapsed/'
out_cache = 'c:/temp/'

collapse_isimip_data = function(varname, modname, rcp){
  cat(rep('##########', 8), '\n')
  cat('Collapsing the ISIMIP Model:', modname, ' Variable:', varname, '\n')
  cat(rep('##########', 8), '\n')
  
  newfile = paste0(out_cache, '/', modname, '_', varname, '.nc4')
  
  srcfiles = c(Sys.glob(file.path(nc_cache, 'historical', modname, paste0(varname, '_*'))),
               Sys.glob(file.path(nc_cache, 'rcp60', modname, paste0(varname, '_*'))))
  
  
  date_from_filename = function(filename){
    as.Date(lubridate::ymd(stringr::str_match(basename(filename), '([0-9]{8})-')[,2]))
  }
  
  
  times = as.Date(sapply(srcfiles, date_from_filename, USE.NAMES = FALSE), origin='1970-01-01')
  #class(times) = class(Sys.time())
  
  #make sure they are in chronological order
  srcfiles = srcfiles[order(times)]
  
  
  #open to get the variable attributes and info we need
  firstfile = nc_open(srcfiles[1])
  
  x <- ncdim_def( 'lon_110', units="degrees_north", ncvar_get(firstfile, 'lon'), unlim = FALSE, longname = 'longitude')
  y <- ncdim_def( 'lat_110', units="degrees_east", ncvar_get(firstfile, 'lat'), unlim = FALSE, longname = 'latitude')
  t <- ncdim_def( "time", "days since 1861-1-1 00:00:00", 1:10, unlim=TRUE, calendar='proleptic_gregorian')
  
  var = ncvar_def(varname, 
                  units = ncatt_get(firstfile, varname, 'units')$value, 
                  dim = list(x, y, t), 
                  missval = ncatt_get(firstfile, varname, '_FillValue')$value, 
                  compression = 5, 
                  chunksizes = c(1,1,1000)) #Time only, I only use it this way ATM
  
  nc_close(firstfile)
  
  
  newnc = nc_create(newfile, force_v4=TRUE, vars=var)
  vardata = NULL
  timedata = NULL
  
  varidx = 1
  stride = 1e3
  
  for(i in seq_along(srcfiles)){
    tmpfile = nc_open(srcfiles[i])
    
    gc()
    vardata = ncvar_get(tmpfile, varname)
    timedata = ncvar_get(tmpfile, 'time')
    
    nc_close(tmpfile)
    
    while(length(timedata) > 0){
      ldata = min(stride, dim(vardata)[3])
      
      ncvar_put(newnc, varid=t, count = ldata, start = varidx, vals=timedata[1:ldata])
      ncvar_put(newnc, varid=var, count = c(-1,-1,ldata), start = c(1,1,varidx), vals=vardata[,,1:ldata])
      
      timedata = timedata[-1*(1:ldata)]
      vardata  = vardata[,,-1*(1:ldata)]
      varidx = varidx + ldata
      cat('timedata length:', length(timedata), '\n')
    }
      
    cat(sprintf('%0.2f %% done\n', 100*i/length(srcfiles)))
    
    #flush rest of data to file. 
    # if(i == length(srcfiles)){
    #   cat('flushing the rest of data to file\n')
    #   ndata = length(timedata)
    #   ncvar_put(newnc, varid=timedata, count = ndata, start = varidx, vals=timedata)
    #   ncvar_put(newnc, varid=var, count = c(-1,-1,ndata), start = c(1,1,varidx), vals=vardata)
    # }
    
  }
  
  nc_close(newnc)
  
  return(newfile)
}


for(i in seq_along(varnames)){
  for(j in seq_along(modnames)){
    collapse_isimip_data(varnames[i], modnames[j])
  }
}

