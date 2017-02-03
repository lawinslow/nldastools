#' @title Set local storage path
#' 
#' 
#' @param path Path to NLDAS cache directory
#'
#' @description NLDAS data can be stored locally. The
#' package uses an internally-cached path, which can 
#' be accessed and defined using get_* and set_local_storage.
#'
#'
#'
#'
#' @export
set_local_storage = function(path){
  package_vars$storage_location = path
}


#' @title Return local storage path
#' 
#' @description NLDAS data can be stored locally. The
#' package uses an internally-cached path, which can 
#' be accessed and defined using get_* and set_local_storage.
#' 
#' 
#' @export
get_local_storage = function(){
  return(package_vars$storage_location)
}
