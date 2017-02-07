library(ncdf4)
library(sp)
library(dplyr)
library(abind)
Sys.setenv(TZ='UTC')

nc_cache = 'Z:/big_datasets/NLDAS'
out_cache = 'Z:/big_datasets/NLDAS/driver_ncdf4_NLDAS/bigchunk'
all_dfiles = Sys.glob(file.path(nc_cache, '*', '*', 'NLDAS_FORA0125_H.A*.002.nc'))
varnames = c('DLWRFsfc_110_SFC', 'DSWRFsfc_110_SFC', 'APCPsfc_110_SFC_acc1h', 'SPFH2m_110_HTGL', 
            'VGRD10m_110_HTGL', 'UGRD10m_110_HTGL', 'TMP2m_110_HTGL', 'PRESsfc_110_SFC')


collapse_nldas_var = function(varname){
  cat(rep('##########', 8), '\n')
  cat('Collapsing the NLDAS Variable:', varname, '\n')
  cat(rep('##########', 8), '\n')
  
  newfile = paste0(out_cache, '/', varname, '.nc4')
  
  date_from_filename = function(filename){
    as.POSIXct(strptime(substr(basename(filename), 19,29), format = '%Y%m%d.%H', tz='UTC'))
  }
  
  times = sapply(all_dfiles, date_from_filename)
  #class(times) = class(Sys.time())
  
  #open to get the variable attributes and info we need
  firstfile = nc_open(all_dfiles[1])
  
  x <- ncdim_def( 'lon_110', units="degrees_north", ncvar_get(firstfile, 'lon_110'), unlim = FALSE, longname = 'longitude')
  y <- ncdim_def( 'lat_110', units="degrees_east", ncvar_get(firstfile, 'lat_110'), unlim = FALSE, longname = 'latitude')
  t <- ncdim_def( "time", "seconds since 1970-01-01", times, unlim=TRUE, calendar='standard')
  
  var = ncvar_def(varname, 
                  units = ncatt_get(firstfile, varname, 'units')$value, 
                  dim = list(x, y, t), 
                  missval = ncatt_get(firstfile, varname, '_FillValue')$value, 
                  compression = 5, 
                  chunksizes = c(6,6,1000)) #force chunking to weigh time heavily 
  
  nc_close(firstfile)
  
  
  newnc = nc_create(newfile, force_v4=TRUE, vars=var)
  vardata = list()
  varidx = c()
  stride = 1000
  for(i in 1:length(all_dfiles)){
    tmpfile = nc_open(all_dfiles[i])
    
    vardata[[length(vardata)+1]] = ncvar_get(tmpfile, varname)
    varidx[length(vardata)]    = i
    
    nc_close(tmpfile)
    
    if(i%%stride == 0 || i == length(all_dfiles)){
      ncvar_put(newnc, varid=var, count = c(-1,-1,length(varidx)), start = c(1,1,min(varidx)), vals=abind(vardata, along=3))
      vardata = list()
      varidx = c()
      cat('Flushing data to nc file...\n')
    }
    
    if(i%%1000 == 0){
      cat(sprintf('%0.2f %% done\n', 100*i/length(all_dfiles)))
    }
  }
  
  nc_close(newnc)
  
  return(newfile)
}

#run and collapse all needed variables
newfiles = sapply(varnames, collapse_nldas_var)

