library(lubridate)
library(parallel)
library(httr)

dest_frmt = 'Z:/big_datasets/NLDAS/%i/%03i/NLDAS_FORA0125_H.A%s.%02i00.002.nc'
timesteps = seq(as.POSIXct('1979-01-02 00:00:00'), as.POSIXct('2016-12-31 00:00:00'), by='hour')
timesteps = seq(as.POSIXct('2007-12-23 00:00:00'), as.POSIXct('2016-12-31 00:00:00'), by='hour')


#url_frmt = 'ftp://hydro1.sci.gsfc.nasa.gov/data/s4pa/NLDAS/NLDAS_FORA0125_H.002/%i/%03i/NLDAS_FORA0125_H.A%s.%02i00.002.grb'
url_frmt = 'http://hydro1.gesdisc.eosdis.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=/data/NLDAS/NLDAS_FORA0125_H.002/%i/%03i/NLDAS_FORA0125_H.A%s.%02i00.002.grb&LABEL=NLDAS_FORA0125_H.nc&SHORTNAME=NLDAS_FORA0125_H&SERVICE=NCL_TO_NetCDF&VERSION=1.02'
pass = .rs.api.askForPassword('give me your pass')
user = 'lawinslow'

# for(i in 1:length(timesteps)){
#   url  = sprintf(url_frmt, year(timesteps[i]), yday(timesteps[i]), format(timesteps[i], '%Y%m%d'), hour(timesteps[i]))
#   dest = sprintf(dest_frmt, year(timesteps[i]), yday(timesteps[i]), format(timesteps[i], '%Y%m%d'), hour(timesteps[i]))
#   dir.create(dirname(dest), recursive = TRUE)
#   download.file(url, dest)
# }


dl_nldas = function(timestep){
  url  = sprintf(url_frmt, year(timestep), yday(timestep), format(timestep, '%Y%m%d'), hour(timestep))
  dest = sprintf(dest_frmt, year(timestep), yday(timestep), format(timestep, '%Y%m%d'), hour(timestep))
  if(!file.exists(dest)){
    dir.create(dirname(dest), recursive = TRUE)
    httr::GET(url, write_disk(dest), authenticate(user, pass))
  }
}

c1 = parallel::makePSOCKcluster(rep('localhost', 8))

parallel::clusterExport(c1, varlist = c('url_frmt', 'dest_frmt', 'user', 'pass'))
parallel::clusterCall(c1, function(){library(httr);library(lubridate)})
dl_res = parallel::clusterApplyLB(c1, timesteps, dl_nldas)


## check and re-download if needed
pr = winProgressBar(title='Checking and fixing NLDAS files', max=length(timesteps))
for(i in 1:length(timesteps)){
  timestep = timesteps[i]
  dest = sprintf(dest_frmt, year(timestep), yday(timestep), format(timestep, '%Y%m%d'), hour(timestep))
  destinfo = file.info(dest)
  if(is.na(destinfo$size) | destinfo$size < 4e6){
    #delete and re-download
    cat('Fixing: ', basename(dest), '\n')
    unlink(dest)
    dl_nldas(timestep)
  }
  setWinProgressBar(pr, i)
}
close(pr)




