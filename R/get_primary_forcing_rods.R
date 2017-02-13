#' @title Get NLDAS primary forcing data
#'
#' @param lon Longitude of data location request
#' @param lat Latitude of data location request
#' @param start Start date as POSIXct
#' @param stop End date as POSIXct
#'
#' @description
#' Interfaces with NASA Data Rods (Time series) services managed by Goddard Earth Sciences Data and
#' Information Services Center (GES DISC). More info about the interface available
#' \href{https://disc.sci.gsfc.nasa.gov/hydrology/data-rods-time-series-data}{here}.
#'
#' @import httr
#' @export
nldas2_primary_forcing_rods <- function(lon, lat, start=as.POSIXct('1979-01-02', tz='UTC'), stop=as.POSIXct('2017-01-01', tz='UTC')){

  if(length(lon) != 1 || length(lat) != 1){
    stop('Lat and Lon must both be length of 1')
  }

  if(start < as.POSIXct('1979-01-02', tz='UTC') || start > as.POSIXct(Sys.Date())){
    stop('Start date must be between 1979-01-02 and now')
  }

  if(stop < as.POSIXct('1979-01-02', tz='UTC') || stop > as.POSIXct(Sys.Date())){
    stop('Stop date must be between 1979-01-02 and now')
  }

  pf_vars = c('APCPsfc', 'DLWRFsfc', 'DSWRFsfc', 'PEVAPsfc', 'SPFH2m', 'TMP2m', 'UGRD10m', 'VGRD10m')


  url_pattern = paste0('https://hydro1.gesdisc.eosdis.nasa.gov/daac-bin/access/timeseries.cgi?',
                       'variable=NLDAS:NLDAS_FORA0125_H.002:%s&location=GEOM:POINT(%g,%%20%g)&startDate=%s&endDate=%s&type=asc2')


  dl_parse = function(var){
    durl = sprintf(url_pattern, var, lon, lat, format(start, '%Y-%m-%dT%H'), format(stop, '%Y-%m-%dT%H'))
    r = RETRY('GET', durl)
    d = read.table(text=content(r, 'text'), skip=40, header=TRUE, fill=TRUE)
    names(d) = c('datetime', 'hour', var)
    d$datetime = as.POSIXct(strptime(paste0(d$datetime, 'T', d$hour), '%Y-%m-%dT%H', tz='UTC'))
    d$hour = NULL
    return(d)
  }

  d_list = lapply(pf_vars, dl_parse)

  pf_data = Reduce(function(...){merge(..., by='datetime')}, d_list)
  return(pf_data)
}
