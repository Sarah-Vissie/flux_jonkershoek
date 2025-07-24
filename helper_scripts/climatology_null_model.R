climatology_null_model_prediction = function(input_df) {
  #doy = lubridate::yday(input_df$time)
  #hod = if_else(input_df$minute == 30, as.numeric(input_df$hour) + 0.5, as.numeric(input_df$hour))
  doy[doy==366] = 365 ## eliminate leap year
  NEE_clim = mgcv::gam(Fc_molar ~ te(hod,doy),
                       data = input_df,
                       method = "REML")
  
  newdata = expand.grid(hod = sort(unique(hod)),doy=sort(unique(doy)))
  newdata$predicted_NEE <- predict(NEE_clim, newdata = newdata, type = "response", se.fit = TRUE)
  
  return(newdata)
}