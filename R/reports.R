#' Get reports
#'
#' @param sites Number of the WebTRIS site e.g. 8188
#' @param period Time period of reports can be "monthly" or "daily". Default "daily"
#' @param start_date Start date as object of class Date. Default Today - 60 days
#' @param end_date End date as object of class Date. Default Today
#'
#' @export
#'
#' @details
#' This function returns a report for the given WebTRIS site based
#'
WebTRIS_reports <- function(sites = NA,
                     period = "daily",
                     start_date = Sys.Date() - 60,
                     end_date = Sys.Date()
                    )
{
  # Check Valid Inputs
  api <- "v1.0"
  checkmate::assert_numeric(sites, lower =  0, upper = 100000, len = 1)
  checkmate::assert_subset(period, choices = c("daily","monthly","annual"), empty.ok = F)
  checkmate::assert_date(start_date, upper = Sys.Date())
  checkmate::assert_date(end_date, lower = start_date, upper = Sys.Date())
  start_date <- as.character(format(start_date, "%d%m%Y"))
  end_date <-  as.character(format(end_date, "%d%m%Y"))


  #Get Data function
  get_data <- function(page, sites, start_date, end_date){
    reqUrl = paste0("http://webtris.highwaysengland.co.uk/api/",api,"/reports/",period)
    # Construct URL
    req <- httr::GET(
      reqUrl,
      query = list(
        sites = sites,
        start_date = start_date,
        end_date = end_date,
        page = page,
        page_size = 40000
      )
    )

    # convert response content into text
    text <- httr::content(req, as = "text", encoding = "UTF-8")

    # Check for no data returned
    if(nchar(text) < 100){
      if(text == ""){
        return("No data returned for that site and time period")
        #stop()
      }
      if(grepl('Report Request Invalid',text_fail)){
        return("Invalid request")
        #stop()
      }
    }else{
      # parse text to json
      asjson <- jsonlite::fromJSON(text)

      if(page == 1){
        return(asjson)
      }else{
        return(asjson$Rows)
      }
    }



  }

  result <- get_data(1, sites, start_date, end_date)
  if(class(result) == "character"){
    message(result)
    return(NA)
  }else{
    res_header <- result$Header
    res_data <- result$Rows


    # Check if multiple requests needed
    if(res_header$row_count > nrow(res_data)){
      nreq = ceiling(res_header$row_count / 40000)
      res_list <- lapply(seq(2,nreq),get_data, sites = sites, start_date = start_date, end_date = end_date)
      res_data <- list(res_data)
      res_list <- c(res_data, res_list)
      res_data <- dplyr::bind_rows(res_list)
      rm(res_list)
    }


    # Format the Data
    res_data$DateTime_Ending <- lubridate::ymd_hms(paste0(substr(res_data$`Report Date`,1,10)," ",res_data$`Time Period Ending`))
    res_data <- res_data[,c("Site Name","DateTime_Ending","Time Interval",
                            names(res_data)[!names(res_data) %in% c("Site Name","DateTime_Ending","Time Interval","Report Date","Time Period Ending")])]
    res_data[,3:23] <- lapply(res_data[,3:23], as.numeric)

    return(res_data)
  }

}
