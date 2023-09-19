#' Get sites
#'
#' @param sites Number vector of site ids or NA for all sites
#' @param clean Logical, should the Name varialbe be cleaned?
#' @param spatial Logical, should the result be converted to spatial data.frame
#'
#' @export
#'
#' @details
#' This function returns sites data for the given WebTRIS site based
#'
WebTRIS_sites <- function(sites = NA, clean = TRUE, spatial = TRUE)
{
  # Check Valid Inputs
  api <- "v1.0"
  checkmate::assert_numeric(sites)

  # Construct URL
  if(is.na(sites)){
    reqUrl = paste0("http://webtris.highwaysengland.co.uk/api/",api,"/sites")
  }else{
    reqUrl = paste0("http://webtris.highwaysengland.co.uk/api/",api,"/sites/",paste(sites,collapse = ","))
  }

  # Get Data
  req <- httr::GET(reqUrl)
  text <- httr::content(req, as = "text", encoding = "UTF-8")
  asjson <- jsonlite::fromJSON(text)

  res_header <- asjson$row_count
  res_data <- asjson$sites

  if(spatial){
    res_data <- sf::st_as_sf(res_data, coords = c("Longitude","Latitude"), crs = 4326)
  }

  if(clean){
    res_data = WebTRIS_clean_sites(res_data)
  }

  return(res_data)
}

#' Get Cleans Site Names
#' The Name field contains useful data such as the direction of flow, this function extracts this data.
#'
#' @param x Data.Frame from WebTRIS_sites()
#'
#' @export
#'
#' @details
#'
#'
WebTRIS_clean_sites <- function(x){
  nms = strsplit(x$Name,";")
  nms1 = sapply(nms, function(l){if(length(l)>0){l[[1]]}else{NA}} )
  #nms2 = sapply(nms, function(l){if(length(l)>1){l[[2]]}else{NA}} )
  #nms3 = sapply(nms, function(l){if(length(l)>2){l[[3]]}else{NA}} )
  nms4 = sapply(nms, function(l){if(length(l)>3){l[[4]]}else{NA}} )

  x$direction = as.factor(gsub(" ","",nms4))
  x$Name = nms1
  return(x)
}
