# Libraries and dependencies ----

library(tidyverse)
library(rjags)

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

# Filter for target period and do preliminary checks ----

## Subset Feb 2022 data (including NA rows)
feb22_full <- ecdat %>%
  filter(time >= as.POSIXct("2022-02-01 00:00", tz = attr(ecdat$time, "tzone")),
         time < as.POSIXct("2022-03-01 00:00", , tz = attr(ecdat$time, "tzone"))) %>%
  arrange(time)

## Number of rows 
nrow(feb22_full) # should be 1344 for 30-min resolution

ggplot(feb22_full, aes(x = time, y = Fc_molar)) +
  geom_line(color = "steelblue") +  # breaks automatically at NA
  labs(title = "Fc_molar (NEE) with Gaps in February 2022",
       y = "NEE (umol/m2/s)", x = "Time") +
  theme_minimal()


# Calibrate/build the null model (state-space, random walk) ----

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

y = feb22_full$Fc_molar

data <- list(y=y,n=length(y),      ## data
             x_ic=0,tau_ic=0.1,    ## initial condition prior
             a_obs=1,r_obs=1,      ## obs error prior
             a_add=1,r_add=1       ## process error prior
)

# time = 1:data$n 
# ysmoother = mgcv::gam(data$y ~ time,method = "REML",knots = 100)
# ypred = mgcv::predict.gam(ysmoother,list(time=time))
# plot(data$y)
# lines(ypred)

nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(y,length(y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(y.samp), na.rm = TRUE),  ## initial guess on process precision
                    tau_obs=5/var(y.samp, na.rm = TRUE))        ## initial guess on obs precision
}

j.model   <- jags.model (file = textConnection(RandomWalk),
                         data = data,
                         inits = init,
                         n.chains = 3)

## burn-in
jags.out   <- rjags::coda.samples (model = j.model,
                            variable.names = c("tau_add","tau_obs"),
                            n.iter = 5000)

summary(jags.out)
plot(jags.out)
