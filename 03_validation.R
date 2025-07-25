# validation

load("jagsoutput_temp_light.Rdata")

# Extract CI from the output matrix from jags.out
# summary(jags.out)
out.matrix <- as.matrix(jags.out)
out.matrix[,1:5] # first 5 colums stores b1, b2, rho, tau_add, tau_obs, 3000 rows

ci_parms <- apply(out.matrix[,1:5],2,quantile,c(0.025,0.5,0.975)) ## get ci  from the matrix
t(ci_parms)

ci_states <- apply(out.matrix[,-(1:5)],2,quantile,c(0.025,0.5,0.975)) ## get ci  from the matrix
t(ci_states)


ecdat_sub <- ecdat[ecdat$month=='03' & ecdat$year=='2022',]

out_merged <- cbind(ecdat_sub, t(ci_states))

daytime <- c("05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20")
nighttime <- c("21","22","23","00","01","02","03","04")

modelcheckingplots <- function(out){
  par(mfrow=c(2,3))
  out1 <- out
  out <- out[out$hour%in%daytime,]
  plot(out$Fc_molar, out$`50%`, ylim=c(min(out$`2.5%`), max(max(out$`97.5%`))))
  abline(a=0,b=1)
  for (i in 1:dim(out)[1]) {segments(x0=out$Fc_molar[i],y0=out$`2.5%`[i],y1=out$`97.5%`[i])}

  plot(out$Tc_Avg, out$`50%`- out$Fc_molar, ylab="Predicted - observed", xlab = "Temperature")
  plot(out$Rn, out$`50%`- out$Fc_molar, ylab="Predicted - observed", xlab = "Light")

  out <- out1[out1$hour%in%nighttime,]
  plot(out$Fc_molar, out$`50%`, ylim=c(min(out$`2.5%`), max(max(out$`97.5%`))))
  abline(a=0,b=1)
  for (i in 1:dim(out)[1]) {segments(x0=out$Fc_molar[i],y0=out$`2.5%`[i],y1=out$`97.5%`[i])}

  plot(out$Tc_Avg, out$`50%`- out$Fc_molar, ylab="Predicted - observed", xlab = "Temperature")
  plot(out$Rn, out$`50%`- out$Fc_molar, ylab="Predicted - observed", xlab = "Light")

  }



# examining model fit to in-sample observations
out_insample <- out_merged[(1:(48*14)),]

modelcheckingplots(out_insample)

# examining out-of sample predictions
out_validation <- out_merged[-(1:(48*14)),]

modelcheckingplots(out_validation)
