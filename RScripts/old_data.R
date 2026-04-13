annual = readRDS("traffic_annual_2014_2022.Rds") # Only 4 sites
annual_raw = readRDS("traffic_annual_2014_2022raw.Rds")
monthyl = readRDS("traffic_monthly_2014_2022.Rds") # Only 3 sites

foo = annual_raw[[1]]
foo_annual = foo$annual
foo_month = foo$month

raw_annual = pbapply::pblapply(annual_raw, function(x){
  if(inherits(x, "list")){
    return(x$annual)
  }
  NULL
})

raw_monthly = pbapply::pblapply(annual_raw, function(x){
  if(inherits(x, "list")){
    return(x$month)
  }
  NULL
})

raw_annual = dplyr::bind_rows(raw_annual)
raw_monthly = dplyr::bind_rows(raw_monthly)

saveRDS(raw_annual,"traffic_annual_2014_2022.Rds")
saveRDS(raw_monthly,"traffic_monthly_2014_2022.Rds")
