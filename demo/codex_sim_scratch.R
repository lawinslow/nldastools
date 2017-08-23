library(ncdf4)
library(stringr)

sims = Sys.glob('b:/big_data/esgf/*')
sims = basename(sims)
unique(stringr::str_extract(sims, 'NAM-44_.*?_day'))



nc = nc_open('b:/big_data/esgf/vas_NAM-44_ICHEC-EC-EARTH_rcp85_r12i1p1_SMHI-RCA4_v1_day_20060101-20101231.nc')
hmm = ncvar_get(nc, 'vas')


lat = ncvar_get(nc, 'rlat') + ncatt_get(nc, 'rotated_pole', 'grid_north_pole_latitude')$value
lon = ncvar_get(nc, 'rlon') + ncatt_get(nc, 'rotated_pole', 'grid_north_pole_longitude')$value


image(lon, lat, hmm[,,8])


rcp85 = get_cordex_local(-87,44, scenario='rcp85')

hmm = dplyr::rename(rcp85, spfh2m=huss, dswrfsfc=rsds, dlwrfsfc=rlds, tmp2m=tas, vgrd10m=vas, ugrd10m=uas, pressfc=ps)
dvr = lakemodeltools::nldas_to_glm_drivers(hmm)


#test = mda.lakes::get_driver_path('nhd_120052892')


