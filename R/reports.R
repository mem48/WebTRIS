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
                     period = "annual",
                     start_date = lubridate::ymd("2014-01-01"),
                     end_date = lubridate::ymd("2022-12-31")
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


#Get Data function
get_data <- function(page, sites, start_date, end_date, api = "v1.0", period = "annual"){
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

    report_annual = asjson$AnnualReportBody
    report_month = report_annual$AnnualReportMonthlyDataRows
    report_annual$AnnualReportMonthlyDataRows = NULL
    report_month = dplyr::bind_rows(report_month, .id = "year")
    report_month$year = report_annual$Year[as.numeric(report_month$year)]

    monthrow = report_month$AnnualReportRow
    report_month$AnnualReportRow = NULL
    report_month = cbind(report_month, monthrow)
    report_month$SiteId = report_annual$SiteId[1]

    annualrow_tot = report_annual$AnnualReportTotals
    annualrow_ave = report_annual$AnnualReportAverages
    report_annual$AnnualReportTotals = NULL
    report_annual$AnnualReportAverages = NULL
    names(annualrow_tot) = paste0(names(annualrow_tot),"_total")
    names(annualrow_ave) = paste0(names(annualrow_ave),"_ave")

    report_annual = cbind(report_annual, annualrow_tot, annualrow_ave)

    # Format numeric/date
    for(i in 1:ncol(report_annual)){
      if(!names(report_annual)[i] %in% c("SiteId",
                                        "PeakDailyFlowDate_total","PeakHourlyFlowDate_total")){
        report_annual[,names(report_annual)[i]] = as.numeric(report_annual[,names(report_annual)[i]])
      }
    }

    report_annual$PeakDailyFlowDate_total <- lubridate::dmy_hms(report_annual$PeakDailyFlowDate_total)
    report_annual$PeakHourlyFlowDate_total <- lubridate::dmy_hms(report_annual$PeakHourlyFlowDate_total)

    for(i in 1:ncol(report_month)){
      if(!names(report_month)[i] %in% c("SiteId","MonthName")){
        report_month[,names(report_month)[i]] = as.numeric(report_month[,names(report_month)[i]])
      }
    }



    return(list(annual = report_annual,
                month = report_month
                ))

  }



}


get_all_annual_reports = function(SiteIds){

  res = pbapply::pblapply(SiteIds, get_data, page = 1, start_date = "01012014", end_date = "31122022")
  return(res)

  # res_annual = lapply(res, `[[`, 1)
  # #res_annual = dplyr::bind_rows(res_annual)
  #
  # res_month = lapply(res, `[[`, 2)
  # #res_month = dplyr::bind_rows(res_month)
  #
  # return(list(annual = res_annual,
  #             month = res_month
  # ))

}
sites = readRDS("sites.Rds")
dat = get_all_annual_reports(sites$Id)
annual = dat$annual
month = dat$month
saveRDS(dat, "traffic_annual_2014_2022raw.Rds")
saveRDS(month, "traffic_monthly_2014_2022.Rds")

