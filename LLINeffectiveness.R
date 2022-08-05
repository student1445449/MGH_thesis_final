# Import packages ####
library(pacman)
p_load(deSolve, tidyverse, doParallel, manipulate,readxl,reshape2,ggfortify, knitr, printr,
       plyr, dplyr, lubridate, gridExtra, reshape2, TTR, unikn, ggpubr, zoo, scales)

dates_98_30<-seq(ymd('1998-01-03'),ymd('2029-11-23'),by='weeks')

# Build the model ####

#Read in intervention, treatment availability, birth/death data
interventions<-read_csv("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/interventiondata2030.csv", show_col_types = FALSE) #
treatment<-read_csv("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/treatmentdata2030.csv", show_col_types = FALSE) #
birthdeath<-read_csv("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/birthdeath2030.csv", show_col_types = FALSE) #



# Time steps ####

# time step vector in weeks
times <- seq(0, 32*52 - 1, 1)

# Parameters ####

# read in parameters from excel 
params <- as_tibble(as.data.frame(read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/rec2.csv"), show_col_types = FALSE))) # Read excel file with all parameters
parameters <- rep(0,length(params$symbol)) # create empty parameter vector

for (i in 1:length(params$symbol)){ # populate parameter vector with parameter names and values
  parameters[i] <- params$value[i]
  names(parameters) <- params$symbol
}


# calculate coverage ####
cum_cov_list <- list()

for (t in 1:length(times)) {
  print(t)
  att1 = 1 - 1/(3.25)
  att2 = 1 - 2/(3.25) 
  att3 = 1 - 3/(3.25) 
  LLIN_cov = (interventions$LLIN_sucre*parameters["no_cov"])/(interventions$`population (sucre)`) #proportion of the population covered by NEW nets
  LLIN_t = interventions$time
  cum_cov1 = LLIN_cov 
  cum_cov2 = lag(cum_cov1, default = 0, n = 1)
  cum_cov3 = lag(cum_cov1, default = 0, n = 2)
  cum_cov = att1*cum_cov1+ att2*cum_cov2 +att3* cum_cov3
  LLIN_effcov = parameters["LLIN_use"]*parameters["LLIN_eff"]*pmin(cum_cov, 1)
  LLINs<-approx(LLIN_t, LLIN_effcov, t)$y
  
  cum_cov_list[[t]] <- LLINs
}

cum_cov_list2 = do.call(cbind, cum_cov_list)



ggplot() + 
  geom_line(aes(x=dates_98_30, y=cum_cov_list2*100, colour="Data")) + 
  theme_gray() +  
  scale_x_date(breaks = scales::breaks_pretty(10))+ 
  labs(y =("Effective coverage (%)"), x=("Date"), colour="Data")


