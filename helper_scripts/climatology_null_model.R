climatology_null_model_prediction = function(input_df) {
  input_df$doy[input_df$doy==366] = 365 ## eliminate leap year
  NEE_clim = mgcv::gam(Fc_molar ~ te(hod,doy),
                       data = input_df,
                       method = "REML")
  
  newdata = expand.grid(hod = sort(unique(input_df$hod)),doy=sort(unique(input_df$doy)))
  
  pred <- predict(NEE_clim, newdata = newdata, type = "response", se.fit = TRUE)
  
  newdata$predicted_NEE <- pred$fit
  newdata$se_NEE <- pred$se.fit
  
  return(newdata)
}
