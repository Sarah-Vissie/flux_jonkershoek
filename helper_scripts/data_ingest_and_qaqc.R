##' Ingest and QA input dataset, select variables of interest, expand time column for filtering
##' @return data.frame in long format with measurements every 30 minutes as rows and observed columns 
ingest_and_qa = function() {
  
  # Data inputs ----
  
  ecdat <- read_delim("data/Jonkershoek_EC_all.csv", delim = ";") %>%
    select(TIMESTAMP, Fc_molar, Tc_Avg, Rn, u_star, Fc_qc_grade) %>%
    mutate(
      time = lubridate::parse_date_time(TIMESTAMP,orders = "ymd HM"),
      minute = format(time, "%M"),
      hour = format(time, "%H"),
      day = format(time, "%d"),
      month = format(time, "%m"),
      year = format(time, "%Y"),
      doy = lubridate::yday(time),
      hod = if_else(minute == 30, as.numeric(hour) + 0.5, as.numeric(hour))
      ) %>%
    select(-TIMESTAMP)
    
  # QAQC ----
  
  ecdat$Fc_molar[which(ecdat$Fc_molar > 20)] = NA 
  ecdat$Fc_molar[which(ecdat$Fc_molar < -30)] = NA
  
  ## ustar filtering
  ecdat$Fc_molar[which(ecdat$u_star < 0.3)] = NA 
  
  ## quality control filtering
  ## scores defined in Appendix F of EasyFlux manual, with 1 being the best score and 9 being the worst
  ecdat$Fc_molar[which(ecdat$Fc_qc_grade > 6)] = NA 
  
  ## convert NaN values to NA
  ecdat$Fc_molar[is.nan(ecdat$Fc_molar)] = NA
  
  return(ecdat %>% select(-u_star, -Fc_qc_grade))
}
