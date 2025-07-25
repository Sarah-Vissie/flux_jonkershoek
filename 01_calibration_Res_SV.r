
library(rjags)
library(ecoforecastR)
library(tidyverse)


# Run the first few code chunks in the README.Rmd file to read in the data and do some cleaning.

ecdat <- read_delim("data/Jonkershoek_EC_all.csv", delim = ";")
ecdat$time = lubridate::parse_date_time(ecdat$TIMESTAMP,orders = "ymd HM")

ecdat$LE[which(ecdat$LE > 400)] = NA ## remove implausible values
ecdat$LE[which(ecdat$LE < -100)] = NA ## remove implausible values
hist(ecdat$LE,breaks = 100)

ecdat$Fc_molar[which(ecdat$Fc_molar > 20)] = NA ## remove implausible values
ecdat$Fc_molar[which(ecdat$Fc_molar < -30)] = NA ## remove implausible values
hist(ecdat$Fc_molar,breaks = 100)

## ustar filtering
ecdat$Fc_molar[which(ecdat$u_star < 0.3)] = NA
ecdat$LE[which(ecdat$u_star < 0.3)] = NA

## quality control filtering
## scores defined in Appendix F of EasyFlux manual, with 1 being the best score and 9 being the worst
ecdat$Fc_molar[which(ecdat$Fc_qc_grade > 6)] = NA
ecdat$LE[which(ecdat$LE_qc_grade > 6)] = NA

# This code take it from there.

month = lubridate::month(ecdat$time)
year = lubridate::year(ecdat$time)

# pick a subset (one month)
ecdat_sub <- ecdat[month==3 & year==2022,]

hist(ecdat_sub$Fc_molar)

# plot the data
#pdf("March2023.pdf",8,4)
plot(ecdat_sub$time,ecdat_sub$Fc_molar, type='l')
#dev.off()


# fit the random walk model
# ************************

RandomWalk = "
model{

  #### Data Model
  for(t in 1:n){
    y[t] ~ dnorm(x[t],tau_obs)
  }

  #### Process Model
  for(t in 2:n){
    x[t]~dnorm(x[t-1],tau_add)
  }

  #### Priors
  x[1] ~ dnorm(x_ic,tau_ic)
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_add ~ dgamma(a_add,r_add)
}
"



y <- ecdat_sub$Fc_molar
y[(train_len + 1):length(y)] <- NA  # Set the *rest* to NA, keep only the first N as training
y[is.nan(y)] <- NA


data <- list(y=y,n=length(y),      ## data
             x_ic=0,tau_ic=0.1, ## initial condition prior
             a_obs=1,r_obs=1,           ## obs error prior
             a_add=1,r_add=1            ## process error prior
)

nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(y,length(y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(y.samp, na.rm=T), na.rm=T),  ## initial guess on process precision
                    tau_obs=5/var(y.samp, na.rm=T))        ## initial guess on obs precision
}

j.model   <- jags.model (file = textConnection(RandomWalk),
                         data = data,
                         inits = init,
                         n.chains = nchain)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c("tau_add","tau_obs"),
                            n.iter = 1000)
plot(jags.out)
dic.samples(j.model, 2000)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c("x","tau_add","tau_obs"),
                            n.iter = 10000)



# plot time series and fitted model
###define the observed 

####Start of what is relevant for the plot_ts()

y_obs <- ecdat_sub$Fc_molar
#We trained on the first 14 days of the month - with 48 time steps in each day (half hourly)
train_days <- 14
steps_per_day <- 48
train_len <- train_days * steps_per_day

plot_ts <- function(jags.out, time, y_obs, train_len,
                    time_range = NULL, ci_level = 0.95,
                    ci_alpha = 0.75, ci_col = "lightblue",
                    col_train = "black", col_test = "red",
                    add_title = NULL){
  
  if (is.null(time_range)) time_range <- c(1, length(time))
  if (length(time) != length(y_obs)) stop("Length of time and y_obs must match")
  
  out_matrix <- as.matrix(jags.out)
  x_cols <- grep("^x\\[", colnames(out_matrix))
  
  ci_bounds <- c(0.025, 0.5, 0.975)  # Lower 2.5%, median, upper 97.5%
  ci <- apply(out_matrix[, x_cols], 2, quantile, probs = ci_bounds, na.rm = TRUE)
  
  ci_low <- ci[1,]
  ci_med <- ci[2,]
  ci_high <- ci[3,]
  
  if (length(x_cols) == 0) stop("No latent state variables (x) found in jags.out")

  plot(y_obs ~ time, type = "n", ylim = range(y_obs, na.rm = TRUE),
       xlim = time[time_range], ylab = "NEE", xlab = "Time", main = add_title)
  
  if(diff(time_range) < 100){
    axis.Date(1, at = seq(time[time_range[1]], time[time_range[2]], by = "month"), format = "%Y-%m")
  }
  
  ecoforecastR::ciEnvelope(time, ci_low, ci_high, col = ecoforecastR::col.alpha(ci_col, ci_alpha))
  
  col_obs <- c(rep(col_train, train_len), rep(col_test, length(y_obs) - train_len))
  points(time, y_obs, pch = "+", cex = 0.5, col = col_obs)
  
  lines(time, ci_med, col = "black", lwd = 1.5)
}

# Plot results
pdf("March2022_random_walk.pdf", 8, 4)
plot_ts(jags.out = jags.out,
        time = ecdat_sub$time,
        y_obs = y_obs,
        train_len = train_len,
        time_range = c(1, length(ecdat_sub$time)),
        add_title = "Random Walk Forecast: March 2022")
dev.off()


