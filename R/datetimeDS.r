datetimeDS = function(spec = NULL){
  
  if ( is.null(spec)){
    stop("must specify either spec = date or datehourmin")
  }
  
  if ( spec == "date"){
    return(substr(Sys.time(),1,10))
  }
  
  if ( spec == "datehourmin"){
    return(paste0(substr(Sys.time(),1,10) , "_",
                  substr(Sys.time(),12,13), "-",
                  substr(Sys.time(),15,16)))
  }
}