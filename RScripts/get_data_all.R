library(sf)

sites = WebTRIS_sites()
saveRDS(sites,"sites2026.Rds")
dat = get_all_annual_reports(SiteIds = sites$Id[sites$Status == "Active"], start_date = "01012017", end_date = "31122025")
annual = dat$annual
month = dat$month
saveRDS(annual, "WebTRIS_traffic_active_annual_2017_2025.Rds")
saveRDS(month, "WebTRIS_monthly_active_2017_2025.Rds")

#Restrucutre

dat_annual = lapply(dat, `[[`, 1)
dat_annual = dat_annual[lengths(dat_annual) > 1]
dat_annual = dplyr::bind_rows(dat_annual)
