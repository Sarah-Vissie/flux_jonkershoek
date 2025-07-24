ingest_and_qa = function() {
  # Data inputs ----
  
  ecdat <- read_delim("data/Jonkershoek_EC_all.csv", delim = ";") %>%
    mutate(time = lubridate::parse_date_time(TIMESTAMP,orders = "ymd HM")) 
  
  # QAQC ----
  
  ## remove implausible values
  ecdat$LE[which(ecdat$LE > 400)] = NA 
  ecdat$LE[which(ecdat$LE < -100)] = NA 
  #hist(ecdat$LE,breaks = 100)
  
  ecdat$Fc_molar[which(ecdat$Fc_molar > 20)] = NA 
  ecdat$Fc_molar[which(ecdat$Fc_molar < -30)] = NA
  #hist(ecdat$Fc_molar,breaks = 100)
  
  ## ustar filtering
  ecdat$Fc_molar[which(ecdat$u_star < 0.3)] = NA 
  ecdat$LE[which(ecdat$u_star < 0.3)] = NA 
  
  ## quality control filtering
  ## scores defined in Appendix F of EasyFlux manual, with 1 being the best score and 9 being the worst
  ecdat$Fc_molar[which(ecdat$Fc_qc_grade > 6)] = NA 
  ecdat$LE[which(ecdat$LE_qc_grade > 6)] = NA 
  
  ecdat$Fc_molar[is.nan(ecdat$Fc_molar)] = NA
  ecdat$LE[is.nan(ecdat$LE)] = NA
  
}