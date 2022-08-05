# Description ####

# Models for:
# - Business-as-usual scenario of no MDA
# - MDA using either PQ or TQ for 3, 5 or 7 years

# Required files to run the code are imported from github. Sensitive
# non-required features of the code have been removed.

# This file is sourced in the code for each scenario explored.

# Import packages ####
library(pacman)
p_load(deSolve, tidyverse, doParallel, manipulate,readxl,reshape2,ggfortify, knitr, printr,
       plyr, dplyr, lubridate, gridExtra, reshape2, TTR, unikn, ggpubr, zoo, scales)


# USER REQUIRES INPUT: Import data ####


data <- read_csv("Data_To_Be_Input.csv", show_col_types = FALSE)



df_data <- as.data.frame(data)

# Dates ####

dates_23_30<-seq(ymd('2023-01-04'),ymd('2029-12-25'),by='weeks')
dates_98_30_years<-seq(ymd('1998-01-01'),ymd('2030-01-01'),by='year')
dates_08_21_years<-seq(ymd('2008-01-01'),ymd('2022-01-01'),by='year')
dates_98_30<-seq(ymd('1998-01-03'),ymd('2029-11-23'),by='weeks')
dates_02_21<-seq(ymd('2002-01-01'),ymd('2021-12-01'),by='weeks')

# Total incidence summation (local + imported) ####

# Subset data into dataframes for each year where we only keep P.vivax 
# and mixed (P.vivax/P.falciparum) infections

inc_2002_tot <- subset(df_data, (Year == 2002 & (`Species` == "PV" | `Species` == "MI" )))
inc_2003_tot <- subset(df_data, (Year == 2003 & (`Species` == "PV" | `Species` == "MI" )))
inc_2004_tot <- subset(df_data, (Year == 2004 & (`Species` == "PV" | `Species` == "MI" )))
inc_2005_tot <- subset(df_data, (Year == 2005 & (`Species` == "PV" | `Species` == "MI" )))
inc_2006_tot <- subset(df_data, (Year == 2006 & (`Species` == "PV" | `Species` == "MI" )))
inc_2007_tot <- subset(df_data, (Year == 2007 & (`Species` == "PV" | `Species` == "MI" )))
inc_2008_tot <- subset(df_data, (Year == 2008 & (`Species` == "PV" | `Species` == "MI" )))
inc_2009_tot <- subset(df_data, (Year == 2009 & (`Species` == "PV" | `Species` == "MI" )))
inc_2010_tot <- subset(df_data, (Year == 2010 & (`Species` == "PV" | `Species` == "MI" )))
inc_2011_tot <- subset(df_data, (Year == 2011 & (`Species` == "PV" | `Species` == "MI" )))
inc_2012_tot <- subset(df_data, (Year == 2012 & (`Species` == "PV" | `Species` == "MI" )))
inc_2013_tot <- subset(df_data, (Year == 2013 & (`Species` == "PV" | `Species` == "MI" )))
inc_2014_tot <- subset(df_data, (Year == 2014 & (`Species` == "PV" | `Species` == "MI" )))
inc_2015_tot <- subset(df_data, (Year == 2015 & (`Species` == "PV" | `Species` == "MI" )))
inc_2016_tot <- subset(df_data, (Year == 2016 & (`Species` == "PV" | `Species` == "MI" )))
inc_2017_tot <- subset(df_data, (Year == 2017 & (`Species` == "PV" | `Species` == "MI" )))
inc_2018_tot <- subset(df_data, (Year == 2018 & (`Species` == "PV" | `Species` == "MI" )))
inc_2019_tot <- subset(df_data, (Year == 2019 & (`Species` == "PV" | `Species` == "MI" )))

yearlyincidence_tot <- c() #  Empty vector for annual incidence

# Dataframe for P.vivax and mixed infection (P.vivax/P.falciparum) 
df_data_PV_IM_only <- subset(df_data, (`Species` == "PV" | `Species` == "MI" ))

# Fill annual incidence vector with data
for (i in seq(2002,2019)) {
  yearlyincidence_tot <- c(yearlyincidence_tot,sum(df_data_PV_IM_only[which(df_data_PV_IM_only$Year == i), 1])/i)
}

# Create empty vectors for weekly incidence each year 
weeklyincidence_2002_tot <- c()
weeklyincidence_2003_tot <- c()
weeklyincidence_2004_tot <- c()
weeklyincidence_2005_tot <- c()
weeklyincidence_2006_tot <- c()
weeklyincidence_2007_tot <- c()
weeklyincidence_2008_tot <- c()
weeklyincidence_2009_tot <- c()
weeklyincidence_2010_tot <- c()
weeklyincidence_2011_tot <- c()
weeklyincidence_2012_tot <- c()
weeklyincidence_2013_tot <- c()
weeklyincidence_2014_tot <- c()
weeklyincidence_2015_tot <- c()
weeklyincidence_2016_tot <- c()
weeklyincidence_2017_tot <- c()
weeklyincidence_2018_tot <- c()
weeklyincidence_2019_tot <- c()


# Fill weekly incidence vector for each year independently 
for (i in seq(1,52)) {
  weeklyincidence_2002_tot <- c(weeklyincidence_2002_tot,length(which(inc_2002_tot$"Week" == i)))
  weeklyincidence_2003_tot <- c(weeklyincidence_2003_tot,length(which(inc_2003_tot$"Week" == i)))
  weeklyincidence_2004_tot <- c(weeklyincidence_2004_tot,length(which(inc_2004_tot$"Week" == i)))
  weeklyincidence_2005_tot <- c(weeklyincidence_2005_tot,length(which(inc_2005_tot$"Week" == i)))
  weeklyincidence_2006_tot <- c(weeklyincidence_2006_tot,length(which(inc_2006_tot$"Week" == i)))
  weeklyincidence_2007_tot <- c(weeklyincidence_2007_tot,length(which(inc_2007_tot$"Week" == i)))
  weeklyincidence_2008_tot <- c(weeklyincidence_2008_tot,length(which(inc_2008_tot$"Week" == i)))
  weeklyincidence_2009_tot <- c(weeklyincidence_2009_tot,length(which(inc_2009_tot$"Week" == i)))
  weeklyincidence_2010_tot <- c(weeklyincidence_2010_tot,length(which(inc_2010_tot$"Week" == i)))
  weeklyincidence_2011_tot <- c(weeklyincidence_2011_tot,length(which(inc_2011_tot$"Week" == i)))
  weeklyincidence_2012_tot <- c(weeklyincidence_2012_tot,length(which(inc_2012_tot$"Week" == i)))
  weeklyincidence_2013_tot <- c(weeklyincidence_2013_tot,length(which(inc_2013_tot$"Week" == i)))
  weeklyincidence_2014_tot <- c(weeklyincidence_2014_tot,length(which(inc_2014_tot$"Week" == i)))
  weeklyincidence_2015_tot <- c(weeklyincidence_2015_tot,length(which(inc_2015_tot$"Week" == i)))
  weeklyincidence_2016_tot <- c(weeklyincidence_2016_tot,length(which(inc_2016_tot$"Week" == i)))
  weeklyincidence_2017_tot <- c(weeklyincidence_2017_tot,length(which(inc_2017_tot$"Week" == i)))
  weeklyincidence_2018_tot <- c(weeklyincidence_2018_tot,length(which(inc_2018_tot$"Week" == i)))
  weeklyincidence_2019_tot <- c(weeklyincidence_2019_tot,length(which(inc_2019_tot$"Week" == i)))
  
}

# Weekly incidence from 2002-2019 (all cases)
overall_weeklyincidence_tot <- c(weeklyincidence_2002_tot,weeklyincidence_2003_tot,weeklyincidence_2004_tot,
                                 weeklyincidence_2005_tot,weeklyincidence_2006_tot,weeklyincidence_2007_tot,
                                 weeklyincidence_2008_tot,weeklyincidence_2009_tot,weeklyincidence_2010_tot,
                                 weeklyincidence_2011_tot,weeklyincidence_2012_tot,weeklyincidence_2013_tot,
                                 weeklyincidence_2014_tot,weeklyincidence_2015_tot,weeklyincidence_2016_tot,
                                 weeklyincidence_2017_tot,weeklyincidence_2018_tot,weeklyincidence_2019_tot)

# Weekly incidence from 2008-2019 (all cases)
overall_weeklyincidence_tot_08_19 <- c(weeklyincidence_2008_tot,weeklyincidence_2009_tot,weeklyincidence_2010_tot,
                                       weeklyincidence_2011_tot,weeklyincidence_2012_tot,weeklyincidence_2013_tot,
                                       weeklyincidence_2014_tot,weeklyincidence_2015_tot,weeklyincidence_2016_tot,
                                       weeklyincidence_2017_tot,weeklyincidence_2018_tot, weeklyincidence_2019_tot)


# Local incidence summation ####


# # Subset data into dataframes for each year where we only keep local 
# P.vivax and mixed (P.vivax/P.falciparum) infections

inc_2002 <- subset(df_data, (Year == 2002 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2003 <- subset(df_data, (Year == 2003 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2004 <- subset(df_data, (Year == 2004 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2005 <- subset(df_data, (Year == 2005 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2006 <- subset(df_data, (Year == 2006 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2007 <- subset(df_data, (Year == 2007 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2008 <- subset(df_data, (Year == 2008 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2009 <- subset(df_data, (Year == 2009 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2010 <- subset(df_data, (Year == 2010 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2011 <- subset(df_data, (Year == 2011 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2012 <- subset(df_data, (Year == 2012 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2013 <- subset(df_data, (Year == 2013 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2014 <- subset(df_data, (Year == 2014 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2015 <- subset(df_data, (Year == 2015 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2016 <- subset(df_data, (Year == 2016 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2017 <- subset(df_data, (Year == 2017 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2018 <- subset(df_data, (Year == 2018 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))
inc_2019 <- subset(df_data, (Year == 2019 & `Origin` == "S" & (`Species` == "PV" | `Species` == "MI" )))


yearlyincidence <- c() # Empty vector for local annual incidence vector 

for (i in seq(2002,2019)) {
  yearlyincidence <- c(yearlyincidence,sum(df_data_PV_IM_only[which(df_data_PV_IM_only$Year == i & df_data_PV_IM_only$`Origin` == "S"), 1])/i)
}


# Create empty vectors for weekly local incidence each year 
weeklyincidence_2002 <- c()
weeklyincidence_2003 <- c()
weeklyincidence_2004 <- c()
weeklyincidence_2005 <- c()
weeklyincidence_2006 <- c()
weeklyincidence_2007 <- c()
weeklyincidence_2008 <- c()
weeklyincidence_2009 <- c()
weeklyincidence_2010 <- c()
weeklyincidence_2011 <- c()
weeklyincidence_2012 <- c()
weeklyincidence_2013 <- c()
weeklyincidence_2014 <- c()
weeklyincidence_2015 <- c()
weeklyincidence_2016 <- c()
weeklyincidence_2017 <- c()
weeklyincidence_2018 <- c()
weeklyincidence_2019 <- c()


# Weekly local incidence for each year
for (i in seq(1,52)) {
  weeklyincidence_2002 <- c(weeklyincidence_2002,length(which(inc_2002$"Week" == i)))
  weeklyincidence_2003 <- c(weeklyincidence_2003,length(which(inc_2003$"Week" == i)))
  weeklyincidence_2004 <- c(weeklyincidence_2004,length(which(inc_2004$"Week" == i)))
  weeklyincidence_2005 <- c(weeklyincidence_2005,length(which(inc_2005$"Week" == i)))
  weeklyincidence_2006 <- c(weeklyincidence_2006,length(which(inc_2006$"Week" == i)))
  weeklyincidence_2007 <- c(weeklyincidence_2007,length(which(inc_2007$"Week" == i)))
  weeklyincidence_2008 <- c(weeklyincidence_2008,length(which(inc_2008$"Week" == i)))
  weeklyincidence_2009 <- c(weeklyincidence_2009,length(which(inc_2009$"Week" == i)))
  weeklyincidence_2010 <- c(weeklyincidence_2010,length(which(inc_2010$"Week" == i)))
  weeklyincidence_2011 <- c(weeklyincidence_2011,length(which(inc_2011$"Week" == i)))
  weeklyincidence_2012 <- c(weeklyincidence_2012,length(which(inc_2012$"Week" == i)))
  weeklyincidence_2013 <- c(weeklyincidence_2013,length(which(inc_2013$"Week" == i)))
  weeklyincidence_2014 <- c(weeklyincidence_2014,length(which(inc_2014$"Week" == i)))
  weeklyincidence_2015 <- c(weeklyincidence_2015,length(which(inc_2015$"Week" == i)))
  weeklyincidence_2016 <- c(weeklyincidence_2016,length(which(inc_2016$"Week" == i)))
  weeklyincidence_2017 <- c(weeklyincidence_2017,length(which(inc_2017$"Week" == i)))
  weeklyincidence_2018 <- c(weeklyincidence_2018,length(which(inc_2018$"Week" == i)))
  weeklyincidence_2019 <- c(weeklyincidence_2019,length(which(inc_2019$"Week" == i)))
  
}

# Weekly local incidence 2002-2018
overall_weeklyincidence <- c(weeklyincidence_2002,weeklyincidence_2003,weeklyincidence_2004,
                             weeklyincidence_2005,weeklyincidence_2006,weeklyincidence_2007,
                             weeklyincidence_2008,weeklyincidence_2009,weeklyincidence_2010,
                             weeklyincidence_2011,weeklyincidence_2012,weeklyincidence_2013,
                             weeklyincidence_2014,weeklyincidence_2015,weeklyincidence_2016,
                             weeklyincidence_2017,weeklyincidence_2018,weeklyincidence_2019)

# Weekly local incidence 2008-2018
overall_weeklyincidence_08_19 <- c(weeklyincidence_2008,weeklyincidence_2009,weeklyincidence_2010,
                                   weeklyincidence_2011,weeklyincidence_2012,weeklyincidence_2013,
                                   weeklyincidence_2014,weeklyincidence_2015,weeklyincidence_2016,
                                   weeklyincidence_2017,weeklyincidence_2018,weeklyincidence_2019)


# Dataframe for local weekly incidence 2002-2018
df2 <- data.frame(seq(1,52),cbind(weeklyincidence_2002,weeklyincidence_2003,weeklyincidence_2004,
                                  weeklyincidence_2005,weeklyincidence_2006,weeklyincidence_2007,
                                  weeklyincidence_2008,weeklyincidence_2009,weeklyincidence_2010,
                                  weeklyincidence_2011,weeklyincidence_2012,weeklyincidence_2013,
                                  weeklyincidence_2014,weeklyincidence_2015,weeklyincidence_2016,
                                  weeklyincidence_2017,weeklyincidence_2018,weeklyincidence_2019 ))

# Dataframe for local weekly incidence 2008-2018
df3 <- data.frame(seq(1,52*12),cbind(weeklyincidence_2008,weeklyincidence_2009,weeklyincidence_2010,weeklyincidence_2011,
                                     weeklyincidence_2012,weeklyincidence_2013,weeklyincidence_2014,weeklyincidence_2015,
                                     weeklyincidence_2016,weeklyincidence_2017,weeklyincidence_2018,weeklyincidence_2019))


# Read in data ####

# Intervention, treatment availability, birth/death data
interventions<-read_csv("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/interventiondata2030.csv", show_col_types = FALSE) #
treatment<-read_csv("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/treatmentdata2030.csv", show_col_types = FALSE) #
birthdeath<-read_csv("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/birthdeath2030.csv", show_col_types = FALSE) #


# Read in parameters ####

parameters <- rep(0,length(params$symbol)) # Create empty parameter vector

for (i in 1:length(params$symbol)){ # Populate parameter vector with names and values
  parameters[i] <- params$value[i]
  names(parameters) <- params$symbol
}

# Start values ####

# Population of Sucre in 1998 is approx 0.8 million
# Use available data for prevalence and making some reasoned assumption,
# we distributed cases in each compartment to sum to ~0.8 million
start<-c(S=317000, E=10, A=10, C=20, T_na = 5, T_a = 20, T_a_lag = 1, Ci=0, Ti_na=0, Ti_a=0, Ti_a_lag=1, R = 317000, L = 160000, Re=10, Exposed=20,CtotInc=0, CIncA_from_trt_na=0, CInc1 =0, CInc2=0, CInc3=0, CInc4=0, Ctrt1=0, Rel=0, Imported=0, Exported=0, Imported_Ci=0, Imported_A=0, Diag=0)
start_pq<-c(S=317000, E=10, A=10, C=20, T_na = 5, T_a = 20, T_a_lag = 1, Ci=0, Ti_na=0, Ti_a=0, Ti_a_lag=1, R = 317000, L = 160000, Re=10, Sm=0, Tm=0, Tm_lag=0, Rm=0, Lm=0, Exposed=20,CtotInc=0, CIncA_from_trt_na=0, CInc1 =0, CInc2=0, CInc3=0, CInc4=0, Ctrt1=0, Rel=0, Imported=0, Exported=0, Imported_Ci=0, Imported_A=0, Diag=0, MDA=0)
start_tq<-c(S=317000, E=10, A=10, C=20, T_na = 5, T_a = 20, T_a_lag = 1, Ci=0, Ti_na=0, Ti_a=0, Ti_a_lag=1, R = 317000, L = 160000, Re=10, Sm=0, Sm_d=0, Tm=0, Tm_d=0, Tm_lag=0, Tm_d_lag=0, Rm=0, Rm_d=0, Lm=0, Lm_d=0, Exposed=20,CtotInc=0, CIncA_from_trt_na=0, CInc1 =0, CInc2=0, CInc3=0, CInc4=0, Ctrt1=0, Rel=0, Imported=0, Exported=0, Imported_Ci=0, Imported_A=0, Diag=0, MDA_tot=0, MDA_TQ=0, MDA_PQ=0)

# Time steps ####

# Simulation period from 1998 to 2030
times <- seq(0, 32*52 - 1, 1)


###############################################################################
# Base scenario 
# Build the model ####

# Define the business-as-usual dynamic model
venmodel <- function(t, x, parameters)  {
  with(as.list(c(parameters, x)), {
    
    # Population functions ####
    
    # Total population
    P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re 
    
    # Infectious population
    Infectious = (zeta2*A)+C+T_na+Ci+Ti_na+T_a+Ti_a
    
    # LLIN function ####
    
    # Proportion of nets used 1, 2 and 3 years after distribution
    att1 = 1 - 1/(3.25)
    att2 = 1 - 2/(3.25) 
    att3 = 1 - 3/(3.25) 
    
    # Cumulative coverage through time 
    LLIN_cov = (interventions$LLIN_sucre*no_cov)/P #proportion of the population covered by NEW nets
    LLIN_t = interventions$time
    cum_cov1 = LLIN_cov 
    cum_cov2 = lag(cum_cov1, default = 0, n = 1)
    cum_cov3 = lag(cum_cov1, default = 0, n = 2)
    cum_cov = att1*cum_cov1+ att2*cum_cov2 +att3* cum_cov3
    
    # LLIN effectiveness at reducing force of infection through time 
    LLIN_effcov = LLIN_use*LLIN_eff*pmin(cum_cov, 1)
    LLIN<-approx(LLIN_t, LLIN_effcov, t)$y
    
    # Seasonality component of local transmission ####
    seas = 0.5+(0.2*cos(pi*(2*t/52) + 6.5))
    
    # Births and deaths ####
    beta = approx(birthdeath$time, birthdeath$birthrate, t)$y # births
    mu = approx(birthdeath$time, birthdeath$deathrate, t)$y # deaths
    
    # Importation functions ####
    
    # Imported population matched with Sucre's population
    imp_pop <- P
    
    # Seasonality component of importation of infections 
    imp_seas = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.5)) )
    imp = imp_pop*imp_seas*tau6 # Rate and level of importation of infections
    
    # Inflate rate and level of importation to approximate variation seen
    # in available importation data from 1998-2019
    
    if (t>(52*15) & t<(52*17)) { # Fitted to data
      imp<-imp*2
    }
    
    if (t>(52*17) & t<(52*20)) { # Fitted to data 
      imp<-imp*10
    }
    
    if (t>(52*20) & t<(52*22)) { # Fitted to data
      imp<-imp*6
    }
    
    if (t > (52*22) & t < (52*24)) { # 2020-2022 COVID reduced migration
      imp<-imp*0.5
    }
    
    if (t > (52*24) & t<(52*26)) { # Assumed that pre-COVID migration levels begin to return
      imp<-imp*4
    }
    
    if (t > (52*26)) { # Assumed pre-COVID migration levels return
      imp<-imp*6
    }
    
    # Exportation functions ####
    
    # Assume same seasonal component of exportation but with a 2-week lag 
    imp_seas_lag = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.75)) )
    ex <- imp_seas_lag*tau6
    
    # Treatment and diagnostic test availability ####
    pa5 <- diminish*(approx(treatment$time, treatment$approx_cov, t, method="constant")$y)/100
    
    # Force of infection function ####
    lambda = ptrans1*(1-LLIN)*seas*(w^2*x*y*m*Infectious/P)/(w*y*Infectious/P+mu_m)*(gamma_m/(gamma_m+mu_m))
    
    # Model equations ####
    
    # Local transmission
    
    dS = (beta*P + 
            rho*R + 
            dhyp*L - 
            lambda*S - 
            mu*S - 
            ex*S) 
    
    dE = (lambda*S - 
            gamma*E - 
            mu*E - 
            ex*E) 
    
    dA = (pa1*gamma*E + 
            (1-pa3)*tau2*(C+Ci) + 
            ptrans2*pa7*pa8*tau5*L +  
            pa2*gamma*Re + 
            tau2*(T_na+Ti_na) + 
            z2*imp - 
            tau3*A -
            mu*A - 
            ex*A) 
    
    dC = ((1-pa1)*gamma*E + 
            ptrans2*(1-pa7)*pa8*tau5*L +  
            (1-pa2)*gamma*Re -  
            (1-pa3)*tau2*C -   
            pa3*(1-pa5)*tau1*C -  
            pa3*pa5*sens*tau1*C - 
            mu*C - 
            ex*C) 
    
    dT_na = (pa3*(1-pa5)*tau1*C - 
               tau2*T_na - 
               mu*T_na - 
               ex*T_na)
    
    dT_a = (pa3*pa5*sens*tau1*C - 
              tau7*T_a - 
              mu*T_a - 
              ex*T_a)
    
    dT_a_lag = (tau7*T_a - 
                  tau4*T_a_lag - 
                  mu*T_a_lag - 
                  ex*T_a_lag)
    
    # Imported infections
    
    dCi = (z1*imp - 
             pa3*(1-pa5)*tau1*Ci - 
             pa3*pa5*sens*tau1*Ci - 
             (1-pa3)*tau2*Ci - 
             mu*Ci - 
             ex*Ci)
    
    dTi_na = (pa3*(1-pa5)*tau1*Ci - 
                tau2*Ti_na -
                mu*Ti_na - 
                ex*Ti_na)
    
    dTi_a = (pa3*pa5*sens*tau1*Ci - 
               tau7*Ti_a - 
               mu*Ti_a - 
               ex*Ti_a)
    
    dTi_a_lag = (tau7*Ti_a - 
                   tau4*Ti_a_lag - 
                   mu*Ti_a_lag -
                   ex*Ti_a_lag)
    
    
    dR = ((1-phyp1)*tau4*(T_a_lag + Ti_a_lag) -
            rho*R - 
            lambda*R - 
            mu*R - 
            ex*R)
    
    dL = (tau3*A + 
            phyp1*tau4*(T_a_lag + Ti_a_lag) -
            dhyp*L - 
            ptrans2*pa8*tau5*L - 
            lambda*L - 
            mu*L - 
            ex*L)
    
    dRe = (lambda*(R + L) - 
             gamma*Re - 
             mu*Re - 
             ex*Re) 
    
    # Counters ####
    
    # Used to verify the functionality of the model and calculate incidence
    
    dExposed = dE + dRe # Total exposed
    dCtotInc = lambda*S + lambda*(R+L) + ptrans2*pa8*tau5*L # Total local incidence including relapses
    
    # Asymptomatic incidence (local and imported) caused by lack of treatment availability
    dCIncA_from_trt_na = tau2*(T_na + Ti_na) 
    
    dCInc1 = pa3*(1-pa5)*tau1*C # T_na incidence
    dCInc2 = pa3*(1-pa5)*tau1*Ci # Ti_na incidence
    dCInc3 = pa3*pa5*sens*tau1*C # T_a incidence
    dCInc4 = pa3*pa5*sens*tau1*Ci # Ti_a incidence
    dCtrt1 = pa3*pa5*sens*tau1*(C + Ci) # Treated at public facility (local and imported)
    dRel = ptrans2*pa8*tau5*L # Relapses
    dImported = imp*(z1 + z2) # Imported cases (asymptomatic and clinical)
    dExported = ex*P # Exported population 
    dImported_Ci = imp*z1 # Clinical imported cases
    dImported_A = imp*z2 # Asymptomatic imported cases 
    
    # Diagnosed incidence
    dDiag = pa3*(1-pa5)*tau1*(C+Ci) + pa3*pa5*sens*tau1*(C+Ci)
    
    # Model output ####
    output <- c(dS, dE, dA, dC, dT_na, dT_a, dT_a_lag, dCi, dTi_na, dTi_a, dTi_a_lag,  dR, dL, dRe, dExposed, dCtotInc, dCIncA_from_trt_na, dCInc1, dCInc2, dCInc3, dCInc4, dCtrt1, dRel, dImported, dExported, dImported_Ci, dImported_A, dDiag)
    list(output)
  })
}

# Run models ####
hm<-ode(times=times, y=start, func=venmodel,parms=parameters)

# Plots ####

# model output as a tibble in long format
df1<-as_tibble(as.data.frame(hm)) %>% 
  mutate(P = S+E+A+C+T_na+T_a+T_a_lag+R+L+Re,
         P2 = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re, # population incl. imported cases
         IncA_from_trt_na= c(0, diff(CIncA_from_trt_na)), # asymptomatic incidence from treatment not available
         totInc = c(0, diff(CtotInc)), # total incidence (diagnosed)
         Inc1 = c(0, diff(CInc1)), # T_na incidence
         Inc2 = c(0, diff(CInc2)), # Ti_na incidence
         Inc3 = c(0, diff(CInc3)), # T_a incidence
         Inc4 = c(0, diff(CInc4)),# Ti_a incidence 
         Diagnosed = c(0, diff(Diag)), # both imported and local official diagnosed
         LocDiag = Inc1 + Inc3, #local diagnosed cases
         ImpDiag = Inc2 + Inc4, # imported diagnosed cases
         Trt1 = c(0, diff(Ctrt1)), # treated at public health system
         ImpP = c(0, diff(Imported)), # total imported cases
         ExpP = c(0, diff(Exported)), # total exported cases
         ImpP_Ci = c(0, diff(Imported_Ci)), # total clinical imported cases
         ImpP_A = c(0, diff(Imported_A)), # total asymptomatic imported cases
         Rel = c(0, diff(Rel)), # total relapses 
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2 # malaria prevalence
  ) %>% 
  pivot_longer(names_to = "variable", cols = !1)%>% 
  mutate(model = "Model output")

# model output as tibble 
df1_norm<-as_tibble(as.data.frame(hm)) %>% 
  mutate(P = S+E+A+C+T_na+T_a+T_a_lag+R+L+Re,
         P2 = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re, # population incl. imported cases
         IncA_from_trt_na= c(0, diff(CIncA_from_trt_na)), # asymptomatic incidence from treatment not available
         totInc = c(0, diff(CtotInc)), # total incidence (diagnosed)
         Inc1 = c(0, diff(CInc1)), # T_na incidence
         Inc2 = c(0, diff(CInc2)), # Ti_na incidence
         Inc3 = c(0, diff(CInc3)), # T_a incidence
         Inc4 = c(0, diff(CInc4)),# Ti_a incidence 
         Diagnosed = c(0, diff(Diag)), # both imported and local official diagnosed
         LocDiag = Inc1 + Inc3, #local diagnosed cases
         ImpDiag = Inc2 + Inc4, # imported diagnosed cases
         Trt1 = c(0, diff(Ctrt1)), # treated at public health system
         ImpP = c(0, diff(Imported)), # total imported cases
         ExpP = c(0, diff(Exported)), # total exported cases
         ImpP_Ci = c(0, diff(Imported_Ci)), # total clinical imported cases
         ImpP_A = c(0, diff(Imported_A)), # total asymptomatic imported cases
         Rel = c(0, diff(Rel)), # total relapses 
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2 # malaria prevalence
  ) %>% 
  mutate(model = "Model output")



###############################################################################
# PQ MODEL (3 rounds)
# Build the PQ MDA model ####

# Define dynamic Human-static Vector model #
venmodel_pq3 <- function(t, x, parameters)  {
  with(as.list(c(parameters, x)), {
    
    # Population functions ####
    
    # Total population
    P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Tm+Tm_lag+Rm+Lm 
    
    # Infectious population
    Infectious = (zeta2*A)+C+T_na+Ci+Ti_na+T_a+Ti_a+Tm
    
    # LLIN function ####
    
    # Proportion of nets used 1, 2 and 3 years after distribution
    att1 = 1 - 1/(3.25)
    att2 = 1 - 2/(3.25) 
    att3 = 1 - 3/(3.25) 
    
    # Cumulative coverage through time 
    LLIN_cov = (interventions$LLIN_sucre*no_cov)/P #proportion of the population covered by NEW nets
    LLIN_t = interventions$time
    cum_cov1 = LLIN_cov 
    cum_cov2 = lag(cum_cov1, default = 0, n = 1)
    cum_cov3 = lag(cum_cov1, default = 0, n = 2)
    cum_cov = att1*cum_cov1+ att2*cum_cov2 +att3* cum_cov3
    
    # LLIN effectiveness at reducing force of infection through time 
    LLIN_effcov = LLIN_use*LLIN_eff*pmin(cum_cov, 1)
    LLIN<-approx(LLIN_t, LLIN_effcov, t)$y
    
    # Seasonality component of local transmission ####
    seas = 0.5+(0.2*cos(pi*(2*t/52) + 6.5))
    
    # Births and deaths ####
    beta = approx(birthdeath$time, birthdeath$birthrate, t)$y # births
    mu = approx(birthdeath$time, birthdeath$deathrate, t)$y # deaths
    
    # Importation functions
    
    # Imported population matched with Sucre's population
    imp_pop <- P
    
    # Seasonality component of importation of infections 
    imp_seas = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.5)) )
    imp = imp_pop*imp_seas*tau6 # Rate and level of importation of infections
    
    # Inflate rate and level of importation to approximate variation seen
    # in available importation data from 1998-2019
    
    if (t>(52*15) & t<(52*17)) { # Fitted to data
      imp<-imp*2
    }
    
    if (t>(52*17) & t<(52*20)) { # Fitted to data 
      imp<-imp*10
    }
    
    if (t>(52*20) & t<(52*22)) { # Fitted to data
      imp<-imp*6
    }
    
    if (t > (52*22) & t < (52*24)) { # 2020-2022 COVID reduced migration
      imp<-imp*0.5
    }
    
    if (t > (52*24) & t<(52*26)) { # Assumed that pre-COVID migration levels begin to return
      imp<-imp*4
    }
    
    if (t > (52*26)) { # Assumed pre-COVID migration levels return
      imp<-imp*6
    }
    
    # Exportation functions ####
    
    # Assume same seasonal component of exportation but with a 2-week lag 
    imp_seas_lag = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.75)) )
    ex <- imp_seas_lag*tau6
    
    # Treatment and diagnostic test availability ####
    pa5 <- diminish*(approx(treatment$time, treatment$approx_cov, t, method="constant")$y)/100
    
    # Force of infection function ####
    lambda = ptrans1*(1-LLIN)*seas*(w^2*x*y*m*Infectious/P)/(w*y*Infectious/P+mu_m)*(gamma_m/(gamma_m+mu_m))
    
    # MDA component ####
    
    rounds = 3 # Programme length (years)
    mdur = 6 # 6 week duration to complete MDA round to target population
    
    # Rounds start in epidemiological weeks 1 and 16, starting in 2023
    pulse = c(c(rep(0,1300), rep(c(rep(1,mdur), rep(0, 26-mdur), rep(1,mdur), rep(0,26-mdur)),rounds)), rep(0, 1664-length(c(rep(0,1300), rep(c(rep(1,mdur), rep(0, 26-mdur), rep(1,mdur), rep(0,26-mdur)),rounds))))) # 1300 corresponds to first week in 2023

    # No MDA from 1998 to 2023
    mrate<-0 
    t2<-0 
    
    # MDA implementation during 2023-2025
    if(t>(52*25) & t<(52*(25+rounds))) {
      mrate = approx(1:length(pulse), (-log(1-mcov)/mdur)*pulse, t, method="constant", rule=2)$y
      t2<-1
    }
    
    # Model equations ####
    
    # Local transmission 
    
    dS = (beta*P + 
            rho*R + 
            dhyp*L - 
            lambda*S - 
            mu*S - 
            ex*S +
            t2*(-mrate*S + tau8*Sm)) 
    
    dE = (lambda*S - 
            gamma*E - 
            mu*E - 
            ex*E + 
            t2*(-mrate*E)) 
    
    dA = (pa1*gamma*E + 
            (1-pa3)*tau2*(C+Ci) + 
            ptrans2*pa7*pa8*tau5*L +  
            pa2*gamma*Re + 
            tau2*(T_na+Ti_na) + 
            z2*imp - 
            tau3*A -
            mu*A - 
            ex*A + 
            t2*(-mrate*A)) 
    
    dC = ((1-pa1)*gamma*E + 
            ptrans2*(1-pa7)*pa8*tau5*L +  
            (1-pa2)*gamma*Re -  
            (1-pa3)*tau2*C -   
            pa3*(1-pa5)*tau1*C -  
            pa3*pa5*sens*tau1*C - 
            mu*C - 
            ex*C + 
            t2*(-mrate*C)) 
    
    dT_na = (pa3*(1-pa5)*tau1*C - 
               tau2*T_na - 
               mu*T_na - 
               ex*T_na + 
               t2*(-mrate*T_na))
    
    dT_a = (pa3*pa5*sens*tau1*C - 
              tau7*T_a - 
              mu*T_a - 
              ex*T_a)
    
    dT_a_lag = (tau7*T_a - 
                  tau4*T_a_lag - 
                  mu*T_a_lag - 
                  ex*T_a_lag)
    
    # Imported infections
    
    dCi = (z1*imp - 
             pa3*(1-pa5)*tau1*Ci - 
             pa3*pa5*sens*tau1*Ci - 
             (1-pa3)*tau2*Ci - 
             mu*Ci - 
             ex*Ci + 
             t2*(-mrate*Ci))
    
    dTi_na = (pa3*(1-pa5)*tau1*Ci - 
                tau2*Ti_na -
                mu*Ti_na - 
                ex*Ti_na + 
                t2*(-mrate*Ti_na))
    
    dTi_a = (pa3*pa5*sens*tau1*Ci - 
               tau7*Ti_a - 
               mu*Ti_a - 
               ex*Ti_a)
    
    dTi_a_lag = (tau7*Ti_a - 
                   tau4*Ti_a_lag - 
                   mu*Ti_a_lag -
                   ex*Ti_a_lag)
    
    dR = ((1-phyp1)*tau4*(T_a_lag + Ti_a_lag) -
            rho*R - 
            lambda*R - 
            mu*R - 
            ex*R + 
            t2*(-mrate*R + (1-phyp1)*tau4*Tm_lag + tau8*Rm))
    
    dL = (tau3*A + 
            phyp1*tau4*(T_a_lag + Ti_a_lag) -
            dhyp*L - 
            ptrans2*pa8*tau5*L - 
            lambda*L - 
            mu*L - 
            ex*L + 
            t2*(-mrate*L + phyp1*tau4*Tm_lag + tau8*Lm))
    
    dRe = (lambda*(R + L) - 
             gamma*Re - 
             mu*Re - 
             ex*Re + 
             t2*(-mrate*Re)) 
    
    dSm = t2*(mrate*S - 
                tau8*Sm - 
                mu*Sm - 
                ex*Sm)
    
    dTm = t2*(mrate*(E+A+C+T_na+Ci+Ti_na+Re) - 
                tau7*Tm - 
                mu*Tm - 
                ex*Tm)
    
    dTm_lag = t2*(tau7*Tm - 
                    tau4*Tm_lag - 
                    mu*Tm_lag - 
                    ex*Tm_lag)
    
    dRm = t2*(mrate*R - 
                tau8*Rm - 
                mu*Rm - 
                ex*Rm)
    
    dLm = t2*(mrate*L - 
                tau8*Lm - 
                mu*Lm - 
                ex*Lm)
    
    # Counters ####
    
    # Used to verify the functionality of the model and calculate incidence
    
    dExposed = dE + dRe # Total exposed
    dCtotInc = lambda*S + lambda*(R+L) + pa8*tau5*L # Total local incidence including relapse
    
    # Asymptomatic incidence (local and imported) caused by lack of treatment availability
    dCIncA_from_trt_na = tau2*(T_na + Ti_na) 
    
    dCInc1 = pa3*(1-pa5)*tau1*C # T_na incidence
    dCInc2 = pa3*(1-pa5)*tau1*Ci # Ti_na incidence
    dCInc3 = pa3*pa5*sens*tau1*C # T_a incidence
    dCInc4 = pa3*pa5*sens*tau1*Ci # Ti_a incidence
    dCtrt1 = pa3*pa5*sens*tau1*(C + Ci) # Treated at public facility (local and imported)
    dRel = ptrans2*pa8*tau5*L # Relapses
    dImported = imp*(z1 + z2) # Imported cases (asymptomatic and clinical)
    dExported = ex*P # Exported population 
    dImported_Ci = imp*z1 # Clinical imported cases
    dImported_A = imp*z2 # Asymptomatic imported cases 
    
    # Diagnosed incidence
    dDiag = pa3*(1-pa5)*tau1*(C+Ci) + pa3*pa5*sens*tau1*(C+Ci)
    
    dMDA = t2*mrate*(S+E+A+C+T_na+Ci+Ti_na+R+L+Re) # Number of people treated with MDA
    
    # Model output ####
    output <- c(dS, dE, dA, dC, dT_na, dT_a, dT_a_lag, dCi, dTi_na, dTi_a, dTi_a_lag,  dR, dL, dRe, dSm, dTm, dTm_lag, dRm, dLm, dExposed, dCtotInc, dCIncA_from_trt_na, dCInc1, dCInc2, dCInc3, dCInc4, dCtrt1, dRel, dImported, dExported, dImported_Ci, dImported_A, dDiag, dMDA)
    list(output)
  })
}

# Run models ####
hm_pq3<-ode(times=times, y=start_pq, func=venmodel_pq3,parms=parameters)

# Plots ####

# model output as a tibble in long format
df1_pq3<-as_tibble(as.data.frame(hm_pq3)) %>% 
  mutate(P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Tm+Tm_lag+Rm+Lm, 
         P2 = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re, # population w/o MDA
         IncA_from_trt_na= c(0, diff(CIncA_from_trt_na)), # asymptomatic incidence from treatment not available
         totInc = c(0, diff(CtotInc)), # total incidence (diagnosed)
         Inc1 = c(0, diff(CInc1)), # T_na incidence
         Inc2 = c(0, diff(CInc2)), # Ti_na incidence
         Inc3 = c(0, diff(CInc3)), # T_a incidence
         Inc4 = c(0, diff(CInc4)),# Ti_a incidence 
         Diagnosed = c(0, diff(Diag)), # both imported and local official diagnosed
         LocDiag = Inc1 + Inc3, #local diagnosed cases
         ImpDiag = Inc2 + Inc4, # imported diagnosed cases
         Trt1 = c(0, diff(Ctrt1)), # treated at public health system
         ImpP = c(0, diff(Imported)), # total imported cases
         ExpP = c(0, diff(Exported)), # total exported cases
         ImpP_Ci = c(0, diff(Imported_Ci)), # total clinical imported cases
         ImpP_A = c(0, diff(Imported_A)), # total asymptomatic imported cases
         Rel = c(0, diff(Rel)), # total relapses 
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2, # malaria prevalence
         MDA_count = c(0,diff(MDA)) # MDA in each week
  ) %>% 
  pivot_longer(names_to = "variable", cols = !1)%>% 
  mutate(model = "Model output")

# model output as tibble 
df1_pq_norm3<-as_tibble(as.data.frame(hm_pq3)) %>% 
  mutate(P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Tm+Tm_lag+Rm+Lm,
         P2 = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re, # population w/o MDA
         IncA_from_trt_na= c(0, diff(CIncA_from_trt_na)), # asymptomatic incidence from treatment not available
         totInc = c(0, diff(CtotInc)), # total incidence (diagnosed)
         Inc1 = c(0, diff(CInc1)), # T_na incidence
         Inc2 = c(0, diff(CInc2)), # Ti_na incidence
         Inc3 = c(0, diff(CInc3)), # T_a incidence
         Inc4 = c(0, diff(CInc4)),# Ti_a incidence 
         Diagnosed = c(0, diff(Diag)), # both imported and local official diagnosed
         LocDiag = Inc1 + Inc3, #local diagnosed cases
         ImpDiag = Inc2 + Inc4, # imported diagnosed cases
         Trt1 = c(0, diff(Ctrt1)), # treated at public health system
         ImpP = c(0, diff(Imported)), # total imported cases
         ExpP = c(0, diff(Exported)), # total exported cases
         ImpP_Ci = c(0, diff(Imported_Ci)), # total clinical imported cases
         ImpP_A = c(0, diff(Imported_A)), # total asymptomatic imported cases
         Rel = c(0, diff(Rel)), # total relapses 
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2, # malaria prevalence
         MDA_count = c(0, diff(MDA)) # MDA count per week 
  ) %>% 
  mutate(model = "Model output")



###############################################################################

# PQ MODEL (5 rounds) 
# Build the PQ MDA model ####


# Define dynamic Human-static Vector model #
venmodel_pq5 <- function(t, x, parameters)  {
  with(as.list(c(parameters, x)), {
    
    # Population functions ####
    
    # Total population
    P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Tm+Tm_lag+Rm+Lm 
    
    # Infectious population
    Infectious = (zeta2*A)+C+T_na+Ci+Ti_na+T_a+Ti_a+Tm
    
    # LLIN function ####
    
    # Proportion of nets used 1, 2 and 3 years after distribution
    att1 = 1 - 1/(3.25)
    att2 = 1 - 2/(3.25) 
    att3 = 1 - 3/(3.25) 
    
    # Cumulative coverage through time 
    LLIN_cov = (interventions$LLIN_sucre*no_cov)/P #proportion of the population covered by NEW nets
    LLIN_t = interventions$time
    cum_cov1 = LLIN_cov 
    cum_cov2 = lag(cum_cov1, default = 0, n = 1)
    cum_cov3 = lag(cum_cov1, default = 0, n = 2)
    cum_cov = att1*cum_cov1+ att2*cum_cov2 +att3* cum_cov3
    
    # LLIN effectiveness at reducing force of infection through time 
    LLIN_effcov = LLIN_use*LLIN_eff*pmin(cum_cov, 1)
    LLIN<-approx(LLIN_t, LLIN_effcov, t)$y
    
    # Seasonality component of local transmission ####
    seas = 0.5+(0.2*cos(pi*(2*t/52) + 6.5))
    
    # Births and deaths ####
    beta = approx(birthdeath$time, birthdeath$birthrate, t)$y # births
    mu = approx(birthdeath$time, birthdeath$deathrate, t)$y # deaths
    
    # Importation functions
    
    # Imported population matched with Sucre's population
    imp_pop <- P
    
    # Seasonality component of importation of infections 
    imp_seas = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.5)) )
    imp = imp_pop*imp_seas*tau6 # Rate and level of importation of infections
    
    # Inflate rate and level of importation to approximate variation seen
    # in available importation data from 1998-2019
    
    if (t>(52*15) & t<(52*17)) { # Fitted to data
      imp<-imp*2
    }
    
    if (t>(52*17) & t<(52*20)) { # Fitted to data 
      imp<-imp*10
    }
    
    if (t>(52*20) & t<(52*22)) { # Fitted to data
      imp<-imp*6
    }
    
    if (t > (52*22) & t < (52*24)) { # 2020-2022 COVID reduced migration
      imp<-imp*0.5
    }
    
    if (t > (52*24) & t<(52*26)) { # Assumed that pre-COVID migration levels begin to return
      imp<-imp*4
    }
    
    if (t > (52*26)) { # Assumed pre-COVID migration levels return
      imp<-imp*6
    }
    
    # Exportation functions ####
    
    # Assume same seasonal component of exportation but with a 2-week lag 
    imp_seas_lag = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.75)) )
    ex <- imp_seas_lag*tau6
    
    # Treatment and diagnostic test availability ####
    pa5 <- diminish*(approx(treatment$time, treatment$approx_cov, t, method="constant")$y)/100
    
    # Force of infection function ####
    lambda = ptrans1*(1-LLIN)*seas*(w^2*x*y*m*Infectious/P)/(w*y*Infectious/P+mu_m)*(gamma_m/(gamma_m+mu_m))
    
    # MDA component ####
    
    rounds = 5 # Programme length (years)
    mdur = 6 # 6 week duration to complete MDA round to target population
    
    # Rounds start in epidemiological weeks 1 and 16, starting in 2023
    pulse = c(c(rep(0,1300), rep(c(rep(1,mdur), rep(0, 26-mdur), rep(1,mdur), rep(0,26-mdur)),rounds)), rep(0, 1664-length(c(rep(0,1300), rep(c(rep(1,mdur), rep(0, 26-mdur), rep(1,mdur), rep(0,26-mdur)),rounds))))) # 1300 corresponds to first week in 2023
    
    # No MDA from 1998 to 2023
    mrate<-0 
    t2<-0 
    
    # MDA implementation during 2023-2025
    if(t>(52*25) & t<(52*(25+rounds))) {
      mrate = approx(1:length(pulse), (-log(1-mcov)/mdur)*pulse, t, method="constant", rule=2)$y
      t2<-1
    }
    
    # Model equations ####
    dS = (beta*P + 
            rho*R + 
            dhyp*L - 
            lambda*S - 
            mu*S - 
            ex*S +
            t2*(-mrate*S + tau8*Sm)) 
    
    dE = (lambda*S - 
            gamma*E - 
            mu*E - 
            ex*E + 
            t2*(-mrate*E)) 
    
    dA = (pa1*gamma*E + 
            (1-pa3)*tau2*(C+Ci) + 
            ptrans2*pa7*pa8*tau5*L +  
            pa2*gamma*Re + 
            tau2*(T_na+Ti_na) + 
            z2*imp - 
            tau3*A -
            mu*A - 
            ex*A + 
            t2*(-mrate*A)) 
    
    dC = ((1-pa1)*gamma*E + 
            ptrans2*(1-pa7)*pa8*tau5*L +  
            (1-pa2)*gamma*Re -  
            (1-pa3)*tau2*C -   
            pa3*(1-pa5)*tau1*C -  
            pa3*pa5*sens*tau1*C - 
            mu*C - 
            ex*C + 
            t2*(-mrate*C)) 
    
    dT_na = (pa3*(1-pa5)*tau1*C - 
               tau2*T_na - 
               mu*T_na - 
               ex*T_na + 
               t2*(-mrate*T_na))
    
    dT_a = (pa3*pa5*sens*tau1*C - 
              tau7*T_a - 
              mu*T_a - 
              ex*T_a)
    
    dT_a_lag = (tau7*T_a - 
                  tau4*T_a_lag - 
                  mu*T_a_lag - 
                  ex*T_a_lag)
    
    # imported cases
    
    dCi = (z1*imp - 
             pa3*(1-pa5)*tau1*Ci - 
             pa3*pa5*sens*tau1*Ci - 
             (1-pa3)*tau2*Ci - 
             mu*Ci - 
             ex*Ci + 
             t2*(-mrate*Ci))
    
    dTi_na = (pa3*(1-pa5)*tau1*Ci - 
                tau2*Ti_na -
                mu*Ti_na - 
                ex*Ti_na + 
                t2*(-mrate*Ti_na))
    
    dTi_a = (pa3*pa5*sens*tau1*Ci - 
               tau7*Ti_a - 
               mu*Ti_a - 
               ex*Ti_a)
    
    dTi_a_lag = (tau7*Ti_a - 
                   tau4*Ti_a_lag - 
                   mu*Ti_a_lag -
                   ex*Ti_a_lag)
    
    
    dR = ((1-phyp1)*tau4*(T_a_lag + Ti_a_lag) -
            rho*R - 
            lambda*R - 
            mu*R - 
            ex*R + 
            t2*(-mrate*R + (1-phyp1)*tau4*Tm_lag + tau8*Rm))
    
    dL = (tau3*A + 
            phyp1*tau4*(T_a_lag + Ti_a_lag) -
            dhyp*L - 
            ptrans2*pa8*tau5*L - 
            lambda*L - 
            mu*L - 
            ex*L + 
            t2*(-mrate*L + phyp1*tau4*Tm_lag + tau8*Lm))
    
    dRe = (lambda*(R + L) - 
             gamma*Re - 
             mu*Re - 
             ex*Re + 
             t2*(-mrate*Re)) 
    
    
    dSm = t2*(mrate*S - 
                tau8*Sm - 
                mu*Sm - 
                ex*Sm)
    
    dTm = t2*(mrate*(E+A+C+T_na+Ci+Ti_na+Re) - 
                tau7*Tm - 
                mu*Tm - 
                ex*Tm)
    
    dTm_lag = t2*(tau7*Tm - 
                    tau4*Tm_lag - 
                    mu*Tm_lag - 
                    ex*Tm_lag)
    
    dRm = t2*(mrate*R - 
                tau8*Rm - 
                mu*Rm - 
                ex*Rm)
    
    dLm = t2*(mrate*L - 
                tau8*Lm - 
                mu*Lm - 
                ex*Lm)
    
    
    # Counters ####
    
    # Used to verify the functionality of the model and calculate incidence
    
    dExposed = dE + dRe # Total exposed
    dCtotInc = lambda*S + lambda*(R+L) + pa8*tau5*L # Total local incidence including relapse
    
    # Asymptomatic incidence (local and imported) caused by lack of treatment availability
    dCIncA_from_trt_na = tau2*(T_na + Ti_na) 
    
    dCInc1 = pa3*(1-pa5)*tau1*C # T_na incidence
    dCInc2 = pa3*(1-pa5)*tau1*Ci # Ti_na incidence
    dCInc3 = pa3*pa5*sens*tau1*C # T_a incidence
    dCInc4 = pa3*pa5*sens*tau1*Ci # Ti_a incidence
    dCtrt1 = pa3*pa5*sens*tau1*(C + Ci) # Treated at public facility (local and imported)
    dRel = ptrans2*pa8*tau5*L # Relapses
    dImported = imp*(z1 + z2) # Imported cases (asymptomatic and clinical)
    dExported = ex*P # Exported population 
    dImported_Ci = imp*z1 # Clinical imported cases
    dImported_A = imp*z2 # Asymptomatic imported cases 
    
    # Diagnosed incidence
    dDiag = pa3*(1-pa5)*tau1*(C+Ci) + pa3*pa5*sens*tau1*(C+Ci)
    
    dMDA = t2*mrate*(S+E+A+C+T_na+Ci+Ti_na+R+L+Re) # Number of people treated with MDA
    
    # Model output ####
    output <- c(dS, dE, dA, dC, dT_na, dT_a, dT_a_lag, dCi, dTi_na, dTi_a, dTi_a_lag,  dR, dL, dRe, dSm, dTm, dTm_lag, dRm, dLm, dExposed, dCtotInc, dCIncA_from_trt_na, dCInc1, dCInc2, dCInc3, dCInc4, dCtrt1, dRel, dImported, dExported, dImported_Ci, dImported_A, dDiag, dMDA)
    list(output)
  })
}

# Run models ####
hm_pq5<-ode(times=times, y=start_pq, func=venmodel_pq5,parms=parameters)

# Plots ####

# model output as a tibble in long format
df1_pq5<-as_tibble(as.data.frame(hm_pq5)) %>% 
  mutate(P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Tm+Tm_lag+Rm+Lm, 
         P2 = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re, # population w/o MDA
         IncA_from_trt_na= c(0, diff(CIncA_from_trt_na)), # asymptomatic incidence from treatment not available
         totInc = c(0, diff(CtotInc)), # total incidence (diagnosed)
         Inc1 = c(0, diff(CInc1)), # T_na incidence
         Inc2 = c(0, diff(CInc2)), # Ti_na incidence
         Inc3 = c(0, diff(CInc3)), # T_a incidence
         Inc4 = c(0, diff(CInc4)),# Ti_a incidence 
         Diagnosed = c(0, diff(Diag)), # both imported and local official diagnosed
         LocDiag = Inc1 + Inc3, #local diagnosed cases
         ImpDiag = Inc2 + Inc4, # imported diagnosed cases
         Trt1 = c(0, diff(Ctrt1)), # treated at public health system
         ImpP = c(0, diff(Imported)), # total imported cases
         ExpP = c(0, diff(Exported)), # total exported cases
         ImpP_Ci = c(0, diff(Imported_Ci)), # total clinical imported cases
         ImpP_A = c(0, diff(Imported_A)), # total asymptomatic imported cases
         Rel = c(0, diff(Rel)), # total relapses 
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2, # malaria prevalence
         MDA_count = c(0,diff(MDA)) # MDA in each week
  ) %>% 
  pivot_longer(names_to = "variable", cols = !1)%>% 
  mutate(model = "Model output")

# model output as tibble 
df1_pq_norm5<-as_tibble(as.data.frame(hm_pq5)) %>% 
  mutate(P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Tm+Tm_lag+Rm+Lm,
         P2 = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re, # population w/o MDA
         IncA_from_trt_na= c(0, diff(CIncA_from_trt_na)), # asymptomatic incidence from treatment not available
         totInc = c(0, diff(CtotInc)), # total incidence (diagnosed)
         Inc1 = c(0, diff(CInc1)), # T_na incidence
         Inc2 = c(0, diff(CInc2)), # Ti_na incidence
         Inc3 = c(0, diff(CInc3)), # T_a incidence
         Inc4 = c(0, diff(CInc4)),# Ti_a incidence 
         Diagnosed = c(0, diff(Diag)), # both imported and local official diagnosed
         LocDiag = Inc1 + Inc3, #local diagnosed cases
         ImpDiag = Inc2 + Inc4, # imported diagnosed cases
         Trt1 = c(0, diff(Ctrt1)), # treated at public health system
         ImpP = c(0, diff(Imported)), # total imported cases
         ExpP = c(0, diff(Exported)), # total exported cases
         ImpP_Ci = c(0, diff(Imported_Ci)), # total clinical imported cases
         ImpP_A = c(0, diff(Imported_A)), # total asymptomatic imported cases
         Rel = c(0, diff(Rel)), # total relapses 
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2, # malaria prevalence
         MDA_count = c(0, diff(MDA)) # MDA count per week 
  ) %>% 
  mutate(model = "Model output")


###############################################################################

# PQ MODEL (7 rounds) 
# Build the PQ MDA model ####

# Define dynamic Human-static Vector model #
venmodel_pq7 <- function(t, x, parameters)  {
  with(as.list(c(parameters, x)), {
    
    # Population functions ####
    
    # Total population
    P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Tm+Tm_lag+Rm+Lm 

    # Infectious population
    Infectious = (zeta2*A)+C+T_na+Ci+Ti_na+T_a+Ti_a+Tm
    
    # LLIN function ####
    
    # Proportion of nets used 1, 2 and 3 years after distribution
    att1 = 1 - 1/(3.25)
    att2 = 1 - 2/(3.25) 
    att3 = 1 - 3/(3.25) 
    
    # Cumulative coverage through time 
    LLIN_cov = (interventions$LLIN_sucre*no_cov)/P #proportion of the population covered by NEW nets
    LLIN_t = interventions$time
    cum_cov1 = LLIN_cov 
    cum_cov2 = lag(cum_cov1, default = 0, n = 1)
    cum_cov3 = lag(cum_cov1, default = 0, n = 2)
    cum_cov = att1*cum_cov1+ att2*cum_cov2 +att3* cum_cov3
    
    # LLIN effectiveness at reducing force of infection through time 
    LLIN_effcov = LLIN_use*LLIN_eff*pmin(cum_cov, 1)
    LLIN<-approx(LLIN_t, LLIN_effcov, t)$y
    
    # Seasonality component of local transmission ####
    seas = 0.5+(0.2*cos(pi*(2*t/52) + 6.5))
    
    # Births and deaths ####
    beta = approx(birthdeath$time, birthdeath$birthrate, t)$y # births
    mu = approx(birthdeath$time, birthdeath$deathrate, t)$y # deaths
    
    # Importation functions ####
    
    # Imported population matched with Sucre's population
    imp_pop <- P
    
    # Seasonality component of importation of infections 
    imp_seas = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.5)) )
    imp = imp_pop*imp_seas*tau6 # Rate and level of importation of infections
    
    # Inflate rate and level of importation to approximate variation seen
    # in available importation data from 1998-2019
    
    if (t>(52*15) & t<(52*17)) { # Fitted to data
      imp<-imp*2
    }
    
    if (t>(52*17) & t<(52*20)) { # Fitted to data 
      imp<-imp*10
    }
    
    if (t>(52*20) & t<(52*22)) { # Fitted to data
      imp<-imp*6
    }
    
    if (t > (52*22) & t < (52*24)) { # 2020-2022 COVID reduced migration
      imp<-imp*0.5
    }
    
    if (t > (52*24) & t<(52*26)) { # Assumed that pre-COVID migration levels begin to return
      imp<-imp*4
    }
    
    if (t > (52*26)) { # Assumed pre-COVID migration levels return
      imp<-imp*6
    }
    
    # Exportation functions ####
    
    # Assume same seasonal component of exportation but with a 2-week lag 
    imp_seas_lag = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.75)) )
    ex <- imp_seas_lag*tau6
    
    # Treatment and diagnostic test availability ####
    pa5 <- diminish*(approx(treatment$time, treatment$approx_cov, t, method="constant")$y)/100
    
    # Force of infection function ####
    lambda = ptrans1*(1-LLIN)*seas*(w^2*x*y*m*Infectious/P)/(w*y*Infectious/P+mu_m)*(gamma_m/(gamma_m+mu_m))
    
    # MDA component ####
    
    rounds = 7 # Programme length (years)
    mdur = 6 # 6 week duration to complete MDA round to target population
    
    # Rounds start in epidemiological weeks 1 and 16, starting in 2023
    pulse = c(c(rep(0,1300), rep(c(rep(1,mdur), rep(0, 26-mdur), rep(1,mdur), rep(0,26-mdur)),rounds)), rep(0, 1664-length(c(rep(0,1300), rep(c(rep(1,mdur), rep(0, 26-mdur), rep(1,mdur), rep(0,26-mdur)),rounds))))) # 1300 corresponds to first week in 2023
    
    # No MDA from 1998 to 2023
    mrate<-0 
    t2<-0 
    
    # MDA implementation during 2023-2025
    if(t>(52*25) & t<(52*(25+rounds))) {
      mrate = approx(1:length(pulse), (-log(1-mcov)/mdur)*pulse, t, method="constant", rule=2)$y
      t2<-1
    }
    
    # Model equations ####
    dS = (beta*P + 
            rho*R + 
            dhyp*L - 
            lambda*S - 
            mu*S - 
            ex*S +
            t2*(-mrate*S + tau8*Sm)) 
    
    dE = (lambda*S - 
            gamma*E - 
            mu*E - 
            ex*E + 
            t2*(-mrate*E)) 
    
    dA = (pa1*gamma*E + 
            (1-pa3)*tau2*(C+Ci) + 
            ptrans2*pa7*pa8*tau5*L +  
            pa2*gamma*Re + 
            tau2*(T_na+Ti_na) + 
            z2*imp - 
            tau3*A -
            mu*A - 
            ex*A + 
            t2*(-mrate*A)) 
    
    dC = ((1-pa1)*gamma*E + 
            ptrans2*(1-pa7)*pa8*tau5*L +  
            (1-pa2)*gamma*Re -  
            (1-pa3)*tau2*C -   
            pa3*(1-pa5)*tau1*C -  
            pa3*pa5*sens*tau1*C - 
            mu*C - 
            ex*C + 
            t2*(-mrate*C)) 
    
    dT_na = (pa3*(1-pa5)*tau1*C - 
               tau2*T_na - 
               mu*T_na - 
               ex*T_na + 
               t2*(-mrate*T_na))
    
    dT_a = (pa3*pa5*sens*tau1*C - 
              tau7*T_a - 
              mu*T_a - 
              ex*T_a)
    
    dT_a_lag = (tau7*T_a - 
                  tau4*T_a_lag - 
                  mu*T_a_lag - 
                  ex*T_a_lag)
    
    # imported cases
    
    dCi = (z1*imp - 
             pa3*(1-pa5)*tau1*Ci - 
             pa3*pa5*sens*tau1*Ci - 
             (1-pa3)*tau2*Ci - 
             mu*Ci - 
             ex*Ci + 
             t2*(-mrate*Ci))
    
    dTi_na = (pa3*(1-pa5)*tau1*Ci - 
                tau2*Ti_na -
                mu*Ti_na - 
                ex*Ti_na + 
                t2*(-mrate*Ti_na))
    
    dTi_a = (pa3*pa5*sens*tau1*Ci - 
               tau7*Ti_a - 
               mu*Ti_a - 
               ex*Ti_a)
    
    dTi_a_lag = (tau7*Ti_a - 
                   tau4*Ti_a_lag - 
                   mu*Ti_a_lag -
                   ex*Ti_a_lag)
    
    
    dR = ((1-phyp1)*tau4*(T_a_lag + Ti_a_lag) -
            rho*R - 
            lambda*R - 
            mu*R - 
            ex*R + 
            t2*(-mrate*R + (1-phyp1)*tau4*Tm_lag + tau8*Rm))
    
    dL = (tau3*A + 
            phyp1*tau4*(T_a_lag + Ti_a_lag) -
            dhyp*L - 
            ptrans2*pa8*tau5*L - 
            lambda*L - 
            mu*L - 
            ex*L + 
            t2*(-mrate*L + phyp1*tau4*Tm_lag + tau8*Lm))
    
    dRe = (lambda*(R + L) - 
             gamma*Re - 
             mu*Re - 
             ex*Re + 
             t2*(-mrate*Re)) 
    
    
    dSm = t2*(mrate*S - 
                tau8*Sm - 
                mu*Sm - 
                ex*Sm)
    
    dTm = t2*(mrate*(E+A+C+T_na+Ci+Ti_na+Re) - 
                tau7*Tm - 
                mu*Tm - 
                ex*Tm)
    
    dTm_lag = t2*(tau7*Tm - 
                    tau4*Tm_lag - 
                    mu*Tm_lag - 
                    ex*Tm_lag)
    
    dRm = t2*(mrate*R - 
                tau8*Rm - 
                mu*Rm - 
                ex*Rm)
    
    dLm = t2*(mrate*L - 
                tau8*Lm - 
                mu*Lm - 
                ex*Lm)
    
    
    # Counters ####
    
    # Used to verify the functionality of the model and calculate incidence
    
    dExposed = dE + dRe # Total exposed
    dCtotInc = lambda*S + lambda*(R+L) + pa8*tau5*L # Total local incidence including relapse
    
    # Asymptomatic incidence (local and imported) caused by lack of treatment availability
    dCIncA_from_trt_na = tau2*(T_na + Ti_na) 
    
    dCInc1 = pa3*(1-pa5)*tau1*C # T_na incidence
    dCInc2 = pa3*(1-pa5)*tau1*Ci # Ti_na incidence
    dCInc3 = pa3*pa5*sens*tau1*C # T_a incidence
    dCInc4 = pa3*pa5*sens*tau1*Ci # Ti_a incidence
    dCtrt1 = pa3*pa5*sens*tau1*(C + Ci) # Treated at public facility (local and imported)
    dRel = ptrans2*pa8*tau5*L # Relapses
    dImported = imp*(z1 + z2) # Imported cases (asymptomatic and clinical)
    dExported = ex*P # Exported population 
    dImported_Ci = imp*z1 # Clinical imported cases
    dImported_A = imp*z2 # Asymptomatic imported cases 
    
    # Diagnosed incidence
    dDiag = pa3*(1-pa5)*tau1*(C+Ci) + pa3*pa5*sens*tau1*(C+Ci)
    
    dMDA = t2*mrate*(S+E+A+C+T_na+Ci+Ti_na+R+L+Re) # Number of people treated with MDA
    
    # Model output ####
    output <- c(dS, dE, dA, dC, dT_na, dT_a, dT_a_lag, dCi, dTi_na, dTi_a, dTi_a_lag,  dR, dL, dRe, dSm, dTm, dTm_lag, dRm, dLm, dExposed, dCtotInc, dCIncA_from_trt_na, dCInc1, dCInc2, dCInc3, dCInc4, dCtrt1, dRel, dImported, dExported, dImported_Ci, dImported_A, dDiag, dMDA)
    list(output)
  })
}

# Run models ####
hm_pq7<-ode(times=times, y=start_pq, func=venmodel_pq7,parms=parameters)

# Plots ####

# model output as a tibble in long format
df1_pq7<-as_tibble(as.data.frame(hm_pq7)) %>% 
  mutate(P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Tm+Tm_lag+Rm+Lm, 
         P2 = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re, # population w/o MDA
         IncA_from_trt_na= c(0, diff(CIncA_from_trt_na)), # asymptomatic incidence from treatment not available
         totInc = c(0, diff(CtotInc)), # total incidence (diagnosed)
         Inc1 = c(0, diff(CInc1)), # T_na incidence
         Inc2 = c(0, diff(CInc2)), # Ti_na incidence
         Inc3 = c(0, diff(CInc3)), # T_a incidence
         Inc4 = c(0, diff(CInc4)),# Ti_a incidence 
         Diagnosed = c(0, diff(Diag)), # both imported and local official diagnosed
         LocDiag = Inc1 + Inc3, #local diagnosed cases
         ImpDiag = Inc2 + Inc4, # imported diagnosed cases
         Trt1 = c(0, diff(Ctrt1)), # treated at public health system
         ImpP = c(0, diff(Imported)), # total imported cases
         ExpP = c(0, diff(Exported)), # total exported cases
         ImpP_Ci = c(0, diff(Imported_Ci)), # total clinical imported cases
         ImpP_A = c(0, diff(Imported_A)), # total asymptomatic imported cases
         Rel = c(0, diff(Rel)), # total relapses 
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2, # malaria prevalence
         MDA_count = c(0,diff(MDA)) # MDA in each week
  ) %>% 
  pivot_longer(names_to = "variable", cols = !1)%>% 
  mutate(model = "Model output")

# model output as tibble 
df1_pq_norm7<-as_tibble(as.data.frame(hm_pq7)) %>% 
  mutate(P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Tm+Tm_lag+Rm+Lm,
         P2 = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re, # population w/o MDA
         IncA_from_trt_na= c(0, diff(CIncA_from_trt_na)), # asymptomatic incidence from treatment not available
         totInc = c(0, diff(CtotInc)), # total incidence (diagnosed)
         Inc1 = c(0, diff(CInc1)), # T_na incidence
         Inc2 = c(0, diff(CInc2)), # Ti_na incidence
         Inc3 = c(0, diff(CInc3)), # T_a incidence
         Inc4 = c(0, diff(CInc4)),# Ti_a incidence 
         Diagnosed = c(0, diff(Diag)), # both imported and local official diagnosed
         LocDiag = Inc1 + Inc3, #local diagnosed cases
         ImpDiag = Inc2 + Inc4, # imported diagnosed cases
         Trt1 = c(0, diff(Ctrt1)), # treated at public health system
         ImpP = c(0, diff(Imported)), # total imported cases
         ExpP = c(0, diff(Exported)), # total exported cases
         ImpP_Ci = c(0, diff(Imported_Ci)), # total clinical imported cases
         ImpP_A = c(0, diff(Imported_A)), # total asymptomatic imported cases
         Rel = c(0, diff(Rel)), # total relapses 
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2, # malaria prevalence
         MDA_count = c(0, diff(MDA)) # MDA count per week 
  ) %>% 
  mutate(model = "Model output")


###############################################################################

# TAF MODEL (3 rounds) 
# Build the TQ MDA model ####

# Define dynamic Human-static Vector model #
venmodel_taf3 <- function(t, x, parameters)  {
  with(as.list(c(parameters, x)), {
    
    # Population functions ####
    
    # Total population
    P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Sm_d+Tm+Tm_d+Tm_lag+Tm_d_lag+Rm+Rm_d+Lm+Lm_d

    # Infectious population
    Infectious = (zeta2*A)+C+T_na+Ci+Ti_na+T_a+Ti_a+Tm+Tm_d
    
    # LLIN function ####
    
    # Proportion of nets used 1, 2 and 3 years after distribution
    att1 = 1 - 1/(3.25)
    att2 = 1 - 2/(3.25) 
    att3 = 1 - 3/(3.25) 
    
    # Cumulative coverage through time 
    LLIN_cov = (interventions$LLIN_sucre*no_cov)/P #proportion of the population covered by NEW nets
    LLIN_t = interventions$time
    cum_cov1 = LLIN_cov 
    cum_cov2 = lag(cum_cov1, default = 0, n = 1)
    cum_cov3 = lag(cum_cov1, default = 0, n = 2)
    cum_cov = att1*cum_cov1+ att2*cum_cov2 +att3* cum_cov3
    
    # LLIN effectiveness at reducing force of infection through time 
    LLIN_effcov = LLIN_use*LLIN_eff*pmin(cum_cov, 1)
    LLIN<-approx(LLIN_t, LLIN_effcov, t)$y
    
    # Seasonality component of local transmission ####
    seas = 0.5+(0.2*cos(pi*(2*t/52) + 6.5))
    
    # Births and deaths ####
    beta = approx(birthdeath$time, birthdeath$birthrate, t)$y # births
    mu = approx(birthdeath$time, birthdeath$deathrate, t)$y # deaths
    
    # Importation functions ####
    
    # Imported population matched with Sucre's population
    imp_pop <- P
    
    # Seasonality component of importation of infections 
    imp_seas = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.5)) )
    imp = imp_pop*imp_seas*tau6 # Rate and level of importation of infections
    
    # Inflate rate and level of importation to approximate variation seen
    # in available importation data from 1998-2019
    
    if (t>(52*15) & t<(52*17)) { # Fitted to data
      imp<-imp*2
    }
    
    if (t>(52*17) & t<(52*20)) { # Fitted to data 
      imp<-imp*10
    }
    
    if (t>(52*20) & t<(52*22)) { # Fitted to data
      imp<-imp*6
    }
    
    if (t > (52*22) & t < (52*24)) { # 2020-2022 COVID reduced migration
      imp<-imp*0.5
    }
    
    if (t > (52*24) & t<(52*26)) { # Assumed that pre-COVID migration levels begin to return
      imp<-imp*4
    }
    
    if (t > (52*26)) { # Assumed pre-COVID migration levels return
      imp<-imp*6
    }
    
    # Exportation functions ####
    
    # Assume same seasonal component of exportation but with a 2-week lag 
    imp_seas_lag = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.75)) )
    ex <- imp_seas_lag*tau6
    
    # Treatment and diagnostic test availability ####
    pa5 <- diminish*(approx(treatment$time, treatment$approx_cov, t, method="constant")$y)/100
    
    # Force of infection function ####
    lambda = ptrans1*(1-LLIN)*seas*(w^2*x*y*m*Infectious/P)/(w*y*Infectious/P+mu_m)*(gamma_m/(gamma_m+mu_m))
    
    # MDA component ####
    
    rounds = 3 # Programme length (years)
    mdur = 6 # 6 week duration to complete MDA round to target population
    
    # Rounds start in epidemiological weeks 1 and 16, starting in 2023
    pulse = c(c(rep(0,1300), rep(c(rep(1,mdur), rep(0, 26-mdur), rep(1,mdur), rep(0,26-mdur)),rounds)), rep(0, 1664-length(c(rep(0,1300), rep(c(rep(1,mdur), rep(0, 26-mdur), rep(1,mdur), rep(0,26-mdur)),rounds))))) # 1300 corresponds to first week in 2023
    
    # No MDA from 1998 to 2023
    mrate<-0 
    t2<-0 
    
    # MDA implementation during 2023-2025
    if(t>(52*25) & t<(52*(25+rounds))) {
      mrate = approx(1:length(pulse), (-log(1-mcov)/mdur)*pulse, t, method="constant", rule=2)$y
      t2<-1
    }
    
    # Model equations ####
    dS = (beta*P + 
            rho*R + 
            dhyp*L - 
            lambda*S - 
            mu*S - 
            ex*S +
            t2*(-mrate*S + tau8*(Sm+Sm_d))) 
    
    dE = (lambda*S - 
            gamma*E - 
            mu*E - 
            ex*E + 
            t2*(-mrate*E)) 
    
    dA = (pa1*gamma*E + 
            (1-pa3)*tau2*(C+Ci) + 
            ptrans2*pa7*pa8*tau5*L +  
            pa2*gamma*Re + 
            tau2*(T_na+Ti_na) + 
            z2*imp - 
            tau3*A -
            mu*A - 
            ex*A + 
            t2*(-mrate*A)) 
    
    dC = ((1-pa1)*gamma*E + 
            ptrans2*(1-pa7)*pa8*tau5*L +  
            (1-pa2)*gamma*Re -  
            (1-pa3)*tau2*C -   
            pa3*(1-pa5)*tau1*C -  
            pa3*pa5*sens*tau1*C - 
            mu*C - 
            ex*C + 
            t2*(-mrate*C)) 
    
    dT_na = (pa3*(1-pa5)*tau1*C - 
               tau2*T_na - 
               mu*T_na - 
               ex*T_na + 
               t2*(-mrate*T_na))
    
    dT_a = (pa3*pa5*sens*tau1*C - 
              tau7*T_a - 
              mu*T_a - 
              ex*T_a)
    
    dT_a_lag = (tau7*T_a - 
                  tau4*T_a_lag - 
                  mu*T_a_lag - 
                  ex*T_a_lag)
    
    # imported cases
    
    dCi = (z1*imp - 
             pa3*(1-pa5)*tau1*Ci - 
             pa3*pa5*sens*tau1*Ci - 
             (1-pa3)*tau2*Ci - 
             mu*Ci - 
             ex*Ci + 
             t2*(-mrate*Ci))
    
    dTi_na = (pa3*(1-pa5)*tau1*Ci - 
                tau2*Ti_na -
                mu*Ti_na - 
                ex*Ti_na + 
                t2*(-mrate*Ti_na))
    
    dTi_a = (pa3*pa5*sens*tau1*Ci - 
               tau7*Ti_a - 
               mu*Ti_a - 
               ex*Ti_a)
    
    dTi_a_lag = (tau7*Ti_a - 
                   tau4*Ti_a_lag - 
                   mu*Ti_a_lag -
                   ex*Ti_a_lag)
    
    
    dR = ((1-phyp1)*tau4*(T_a_lag + Ti_a_lag) -
            rho*R - 
            lambda*R - 
            mu*R - 
            ex*R + 
            t2*(-mrate*R + (1-phyp1)*tau4*Tm_d_lag + (1-phyp2)*tau4*Tm_lag + tau8*(Rm+Rm_d)))
    
    dL = (tau3*A + 
            phyp1*tau4*(T_a_lag + Ti_a_lag) -
            dhyp*L - 
            ptrans2*pa8*tau5*L - 
            lambda*L - 
            mu*L - 
            ex*L + 
            t2*(-mrate*L + phyp1*tau4*Tm_d_lag + phyp2*tau4*Tm_lag + tau8*(Lm+Lm_d)))
    
    dRe = (lambda*(R + L) - 
             gamma*Re - 
             mu*Re - 
             ex*Re + 
             t2*(-mrate*Re)) 
    
    
    dSm = t2*(mrate*(1-gd_det)*S - 
                tau8*Sm - 
                mu*Sm - 
                ex*Sm)
    
    dSm_d = t2*(mrate*gd_det*S - 
                  tau8*Sm_d - 
                  mu*Sm_d - 
                  ex*Sm_d)
    
    dTm = t2*(mrate*(1-gd_det)*(E+A+C+T_na+Ci+Ti_na+Re) - 
                tau7*Tm - 
                mu*Tm - 
                ex*Tm)
    
    dTm_d = t2*(mrate*gd_det*(E+A+C+T_na+Ci+Ti_na+Re) - 
                  tau7*Tm_d - 
                  mu*Tm_d - 
                  ex*Tm_d)
    
    dTm_lag = t2*(tau7*Tm - 
                    tau4*Tm_lag - 
                    mu*Tm_lag - 
                    ex*Tm_lag)
    
    dTm_d_lag = t2*(tau7*Tm_d - 
                      tau4*Tm_d_lag - 
                      mu*Tm_d_lag - 
                      ex*Tm_d_lag)
    
    dRm = t2*(mrate*(1-gd_det)*R - 
                tau8*Rm - 
                mu*Rm - 
                ex*Rm)
    
    dRm_d = t2*(mrate*gd_det*R - 
                  tau8*Rm_d - 
                  mu*Rm_d - 
                  ex*Rm_d)
    
    dLm = t2*(mrate*(1-gd_det)*L - 
                tau8*Lm - 
                mu*Lm - 
                ex*Lm)
    
    dLm_d = t2*(mrate*gd_det*L - 
                  tau8*Lm_d - 
                  mu*Lm_d - 
                  ex*Lm_d)
    
    
    # Counters ####
    
    # Used to verify the functionality of the model and calculate incidence
    
    dExposed = dE + dRe # Total exposed
    dCtotInc = lambda*S + lambda*(R+L) + pa8*tau5*L # Total local incidence including relapse
    
    # Asymptomatic incidence (local and imported) caused by lack of treatment availability
    dCIncA_from_trt_na = tau2*(T_na + Ti_na) 
    
    dCInc1 = pa3*(1-pa5)*tau1*C # T_na incidence
    dCInc2 = pa3*(1-pa5)*tau1*Ci # Ti_na incidence
    dCInc3 = pa3*pa5*sens*tau1*C # T_a incidence
    dCInc4 = pa3*pa5*sens*tau1*Ci # Ti_a incidence
    dCtrt1 = pa3*pa5*sens*tau1*(C + Ci) # Treated at public facility (local and imported)
    dRel = ptrans2*pa8*tau5*L # Relapses
    dImported = imp*(z1 + z2) # Imported cases (asymptomatic and clinical)
    dExported = ex*P # Exported population 
    dImported_Ci = imp*z1 # Clinical imported cases
    dImported_A = imp*z2 # Asymptomatic imported cases 
    
    # Diagnosed incidence
    dDiag = pa3*(1-pa5)*tau1*(C+Ci) + pa3*pa5*sens*tau1*(C+Ci)
    
    # Number of people treated with MDA
    dMDA = t2*mrate*(S+E+A+C+T_na+Ci+Ti_na+R+L+Re) 
    dMDA_TQ = t2*mrate*(1-gd_det)*(S+E+A+C+T_na+Ci+Ti_na+R+L+Re)
    dMDA_PQ = t2*mrate*gd_det*(S+E+A+C+T_na+Ci+Ti_na+R+L+Re)
    
    # Model output ####
    output <- c(dS, dE, dA, dC, dT_na, dT_a, dT_a_lag, dCi, dTi_na, dTi_a, dTi_a_lag,  dR, dL, dRe, dSm, dSm_d, dTm, dTm_d, dTm_lag, dTm_d_lag, dRm, dRm_d, dLm, dLm_d, dExposed, dCtotInc, dCIncA_from_trt_na, dCInc1, dCInc2, dCInc3, dCInc4, dCtrt1, dRel, dImported, dExported, dImported_Ci, dImported_A, dDiag, dMDA, dMDA_TQ, dMDA_PQ)
    list(output)
  })
}

# Run models ####
hm_taf3<-ode(times=times, y=start_tq, func=venmodel_taf3,parms=parameters)

# Plots ####

# model output as a tibble in long format
df1_taf3<-as_tibble(as.data.frame(hm_taf3)) %>% 
  mutate(P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Sm_d+Tm+Tm_d+Tm_lag+Tm_d_lag+Rm+Rm_d+Lm+Lm_d, 
         P2 = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re, # population w/o MDA
         IncA_from_trt_na= c(0, diff(CIncA_from_trt_na)), # asymptomatic incidence from treatment not available
         totInc = c(0, diff(CtotInc)), # total incidence (diagnosed)
         Inc1 = c(0, diff(CInc1)), # T_na incidence
         Inc2 = c(0, diff(CInc2)), # Ti_na incidence
         Inc3 = c(0, diff(CInc3)), # T_a incidence
         Inc4 = c(0, diff(CInc4)),# Ti_a incidence 
         Diagnosed = c(0, diff(Diag)), # both imported and local official diagnosed
         LocDiag = Inc1 + Inc3, #local diagnosed cases
         ImpDiag = Inc2 + Inc4, # imported diagnosed cases
         Trt1 = c(0, diff(Ctrt1)), # treated at public health system
         ImpP = c(0, diff(Imported)), # total imported cases
         ExpP = c(0, diff(Exported)), # total exported cases
         ImpP_Ci = c(0, diff(Imported_Ci)), # total clinical imported cases
         ImpP_A = c(0, diff(Imported_A)), # total asymptomatic imported cases
         Rel = c(0, diff(Rel)), # total relapses 
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2, # malaria prevalence
         MDA_tot_count = c(0,diff(MDA_tot)), # total MDA in each week
         MDA_TQ_count = c(0,diff(MDA_TQ)), # TQ MDA in each week
         MDA_PQ_count = c(0,diff(MDA_PQ)) # PQ MDA in each week
  ) %>% 
  pivot_longer(names_to = "variable", cols = !1)%>% 
  mutate(model = "Model output")

df1_taf_norm3<-as_tibble(as.data.frame(hm_taf3)) %>% 
  mutate(P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Sm_d+Tm+Tm_d+Tm_lag+Tm_d_lag+Rm+Rm_d+Lm+Lm_d, 
         P2 = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re,# population w/o MDA
         IncA_from_trt_na= c(0, diff(CIncA_from_trt_na)), # asymptomatic incidence from treatment not available
         totInc = c(0, diff(CtotInc)), # total incidence (diagnosed)
         Inc1 = c(0, diff(CInc1)), # T_na incidence
         Inc2 = c(0, diff(CInc2)), # Ti_na incidence
         Inc3 = c(0, diff(CInc3)), # T_a incidence
         Inc4 = c(0, diff(CInc4)),# Ti_a incidence 
         Diagnosed = c(0, diff(Diag)), # both imported and local official diagnosed
         LocDiag = Inc1 + Inc3, #local diagnosed cases
         ImpDiag = Inc2 + Inc4, # imported diagnosed cases
         Trt1 = c(0, diff(Ctrt1)), # treated at public health system
         ImpP = c(0, diff(Imported)), # total imported cases
         ExpP = c(0, diff(Exported)), # total exported cases
         ImpP_Ci = c(0, diff(Imported_Ci)), # total clinical imported cases
         ImpP_A = c(0, diff(Imported_A)), # total asymptomatic imported cases
         Rel = c(0, diff(Rel)), # total relapses 
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2, # malaria prevalence
         MDA_tot_count = c(0, diff(MDA_tot)), # total MDA in each week
         MDA_TQ_count = c(0,diff(MDA_TQ)), # TQ MDA in each week
         MDA_PQ_count = c(0,diff(MDA_PQ)) # PQ MDA in each week
  ) %>% 
  mutate(model = "Model output")


##############################################################################
# TAF MODEL (5 rounds) 
# Build the TQ MDA model ####

# Define dynamic Human-static Vector model #
venmodel_taf5 <- function(t, x, parameters)  {
  with(as.list(c(parameters, x)), {
    
    # Population functions ####
    
    # Total population
    P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Sm_d+Tm+Tm_d+Tm_lag+Tm_d_lag+Rm+Rm_d+Lm+Lm_d
  
    # Infectious population
    Infectious = (zeta2*A)+C+T_na+Ci+Ti_na+T_a+Ti_a+Tm+Tm_d
    
    # LLIN function ####
    
    # Proportion of nets used 1, 2 and 3 years after distribution
    att1 = 1 - 1/(3.25)
    att2 = 1 - 2/(3.25) 
    att3 = 1 - 3/(3.25) 
    
    # Cumulative coverage through time 
    LLIN_cov = (interventions$LLIN_sucre*no_cov)/P #proportion of the population covered by NEW nets
    LLIN_t = interventions$time
    cum_cov1 = LLIN_cov 
    cum_cov2 = lag(cum_cov1, default = 0, n = 1)
    cum_cov3 = lag(cum_cov1, default = 0, n = 2)
    cum_cov = att1*cum_cov1+ att2*cum_cov2 +att3* cum_cov3
    
    # LLIN effectiveness at reducing force of infection through time 
    LLIN_effcov = LLIN_use*LLIN_eff*pmin(cum_cov, 1)
    LLIN<-approx(LLIN_t, LLIN_effcov, t)$y
    
    # Seasonality component of local transmission ####
    seas = 0.5+(0.2*cos(pi*(2*t/52) + 6.5))
    
    # Births and deaths ####
    beta = approx(birthdeath$time, birthdeath$birthrate, t)$y # births
    mu = approx(birthdeath$time, birthdeath$deathrate, t)$y # deaths
    
    # Importation functions
    
    # Imported population matched with Sucre's population
    imp_pop <- P
    
    # Seasonality component of importation of infections 
    imp_seas = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.5)) )
    imp = imp_pop*imp_seas*tau6 # Rate and level of importation of infections
    
    # Inflate rate and level of importation to approximate variation seen
    # in available importation data from 1998-2019
    
    if (t>(52*15) & t<(52*17)) { # Fitted to data
      imp<-imp*2
    }
    
    if (t>(52*17) & t<(52*20)) { # Fitted to data 
      imp<-imp*10
    }
    
    if (t>(52*20) & t<(52*22)) { # Fitted to data
      imp<-imp*6
    }
    
    if (t > (52*22) & t < (52*24)) { # 2020-2022 COVID reduced migration
      imp<-imp*0.5
    }
    
    if (t > (52*24) & t<(52*26)) { # Assumed that pre-COVID migration levels begin to return
      imp<-imp*4
    }
    
    if (t > (52*26)) { # Assumed pre-COVID migration levels return
      imp<-imp*6
    }
    
    # Exportation functions ####
    
    # Assume same seasonal component of exportation but with a 2-week lag 
    imp_seas_lag = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.75)) )
    ex <- imp_seas_lag*tau6
    
    # Treatment and diagnostic test availability ####
    pa5 <- diminish*(approx(treatment$time, treatment$approx_cov, t, method="constant")$y)/100
    
    # Force of infection function ####
    lambda = ptrans1*(1-LLIN)*seas*(w^2*x*y*m*Infectious/P)/(w*y*Infectious/P+mu_m)*(gamma_m/(gamma_m+mu_m))
    
    # MDA component ####
    
    rounds = 5 # Programme length (years)
    mdur = 6 # 6 week duration to complete MDA round to target population
    
    # Rounds start in epidemiological weeks 1 and 16, starting in 2023
    pulse = c(c(rep(0,1300), rep(c(rep(1,mdur), rep(0, 26-mdur), rep(1,mdur), rep(0,26-mdur)),rounds)), rep(0, 1664-length(c(rep(0,1300), rep(c(rep(1,mdur), rep(0, 26-mdur), rep(1,mdur), rep(0,26-mdur)),rounds))))) # 1300 corresponds to first week in 2023
    
    # No MDA from 1998 to 2023
    mrate<-0 
    t2<-0 
    
    # MDA implementation during 2023-2025
    if(t>(52*25) & t<(52*(25+rounds))) {
      mrate = approx(1:length(pulse), (-log(1-mcov)/mdur)*pulse, t, method="constant", rule=2)$y
      t2<-1
    }
    
    # Model equations ####
    dS = (beta*P + 
            rho*R + 
            dhyp*L - 
            lambda*S - 
            mu*S - 
            ex*S +
            t2*(-mrate*S + tau8*(Sm+Sm_d))) 
    
    dE = (lambda*S - 
            gamma*E - 
            mu*E - 
            ex*E + 
            t2*(-mrate*E)) 
    
    dA = (pa1*gamma*E + 
            (1-pa3)*tau2*(C+Ci) + 
            ptrans2*pa7*pa8*tau5*L +  
            pa2*gamma*Re + 
            tau2*(T_na+Ti_na) + 
            z2*imp - 
            tau3*A -
            mu*A - 
            ex*A + 
            t2*(-mrate*A)) 
    
    dC = ((1-pa1)*gamma*E + 
            ptrans2*(1-pa7)*pa8*tau5*L +  
            (1-pa2)*gamma*Re -  
            (1-pa3)*tau2*C -   
            pa3*(1-pa5)*tau1*C -  
            pa3*pa5*sens*tau1*C - 
            mu*C - 
            ex*C + 
            t2*(-mrate*C)) 
    
    dT_na = (pa3*(1-pa5)*tau1*C - 
               tau2*T_na - 
               mu*T_na - 
               ex*T_na + 
               t2*(-mrate*T_na))
    
    dT_a = (pa3*pa5*sens*tau1*C - 
              tau7*T_a - 
              mu*T_a - 
              ex*T_a)
    
    dT_a_lag = (tau7*T_a - 
                  tau4*T_a_lag - 
                  mu*T_a_lag - 
                  ex*T_a_lag)
    
    # imported cases
    
    dCi = (z1*imp - 
             pa3*(1-pa5)*tau1*Ci - 
             pa3*pa5*sens*tau1*Ci - 
             (1-pa3)*tau2*Ci - 
             mu*Ci - 
             ex*Ci + 
             t2*(-mrate*Ci))
    
    dTi_na = (pa3*(1-pa5)*tau1*Ci - 
                tau2*Ti_na -
                mu*Ti_na - 
                ex*Ti_na + 
                t2*(-mrate*Ti_na))
    
    dTi_a = (pa3*pa5*sens*tau1*Ci - 
               tau7*Ti_a - 
               mu*Ti_a - 
               ex*Ti_a)
    
    dTi_a_lag = (tau7*Ti_a - 
                   tau4*Ti_a_lag - 
                   mu*Ti_a_lag -
                   ex*Ti_a_lag)
    
    
    dR = ((1-phyp1)*tau4*(T_a_lag + Ti_a_lag) -
            rho*R - 
            lambda*R - 
            mu*R - 
            ex*R + 
            t2*(-mrate*R + (1-phyp1)*tau4*Tm_d_lag + (1-phyp2)*tau4*Tm_lag + tau8*(Rm+Rm_d)))
    
    dL = (tau3*A + 
            phyp1*tau4*(T_a_lag + Ti_a_lag) -
            dhyp*L - 
            ptrans2*pa8*tau5*L - 
            lambda*L - 
            mu*L - 
            ex*L + 
            t2*(-mrate*L + phyp1*tau4*Tm_d_lag + phyp2*tau4*Tm_lag + tau8*(Lm+Lm_d)))
    
    dRe = (lambda*(R + L) - 
             gamma*Re - 
             mu*Re - 
             ex*Re + 
             t2*(-mrate*Re)) 
    
    
    dSm = t2*(mrate*(1-gd_det)*S - 
                tau8*Sm - 
                mu*Sm - 
                ex*Sm)
    
    dSm_d = t2*(mrate*gd_det*S - 
                  tau8*Sm_d - 
                  mu*Sm_d - 
                  ex*Sm_d)
    
    dTm = t2*(mrate*(1-gd_det)*(E+A+C+T_na+Ci+Ti_na+Re) - 
                tau7*Tm - 
                mu*Tm - 
                ex*Tm)
    
    dTm_d = t2*(mrate*gd_det*(E+A+C+T_na+Ci+Ti_na+Re) - 
                  tau7*Tm_d - 
                  mu*Tm_d - 
                  ex*Tm_d)
    
    dTm_lag = t2*(tau7*Tm - 
                    tau4*Tm_lag - 
                    mu*Tm_lag - 
                    ex*Tm_lag)
    
    dTm_d_lag = t2*(tau7*Tm_d - 
                      tau4*Tm_d_lag - 
                      mu*Tm_d_lag - 
                      ex*Tm_d_lag)
    
    dRm = t2*(mrate*(1-gd_det)*R - 
                tau8*Rm - 
                mu*Rm - 
                ex*Rm)
    
    dRm_d = t2*(mrate*gd_det*R - 
                  tau8*Rm_d - 
                  mu*Rm_d - 
                  ex*Rm_d)
    
    dLm = t2*(mrate*(1-gd_det)*L - 
                tau8*Lm - 
                mu*Lm - 
                ex*Lm)
    
    dLm_d = t2*(mrate*gd_det*L - 
                  tau8*Lm_d - 
                  mu*Lm_d - 
                  ex*Lm_d)
    
    
    # Counters ####
    
    # Used to verify the functionality of the model and calculate incidence
    
    dExposed = dE + dRe # Total exposed
    dCtotInc = lambda*S + lambda*(R+L) + pa8*tau5*L # Total local incidence including relapse
    
    # Asymptomatic incidence (local and imported) caused by lack of treatment availability
    dCIncA_from_trt_na = tau2*(T_na + Ti_na) 
    
    dCInc1 = pa3*(1-pa5)*tau1*C # T_na incidence
    dCInc2 = pa3*(1-pa5)*tau1*Ci # Ti_na incidence
    dCInc3 = pa3*pa5*sens*tau1*C # T_a incidence
    dCInc4 = pa3*pa5*sens*tau1*Ci # Ti_a incidence
    dCtrt1 = pa3*pa5*sens*tau1*(C + Ci) # Treated at public facility (local and imported)
    dRel = ptrans2*pa8*tau5*L # Relapses
    dImported = imp*(z1 + z2) # Imported cases (asymptomatic and clinical)
    dExported = ex*P # Exported population 
    dImported_Ci = imp*z1 # Clinical imported cases
    dImported_A = imp*z2 # Asymptomatic imported cases 
    
    # Diagnosed incidence
    dDiag = pa3*(1-pa5)*tau1*(C+Ci) + pa3*pa5*sens*tau1*(C+Ci)
    
    # Number of people treated with MDA
    dMDA = t2*mrate*(S+E+A+C+T_na+Ci+Ti_na+R+L+Re) 
    dMDA_TQ = t2*mrate*(1-gd_det)*(S+E+A+C+T_na+Ci+Ti_na+R+L+Re)
    dMDA_PQ = t2*mrate*gd_det*(S+E+A+C+T_na+Ci+Ti_na+R+L+Re)
    
    # Model output ####
    output <- c(dS, dE, dA, dC, dT_na, dT_a, dT_a_lag, dCi, dTi_na, dTi_a, dTi_a_lag,  dR, dL, dRe, dSm, dSm_d, dTm, dTm_d, dTm_lag, dTm_d_lag, dRm, dRm_d, dLm, dLm_d, dExposed, dCtotInc, dCIncA_from_trt_na, dCInc1, dCInc2, dCInc3, dCInc4, dCtrt1, dRel, dImported, dExported, dImported_Ci, dImported_A, dDiag, dMDA, dMDA_TQ, dMDA_PQ)
    list(output)
  })
}

# Run models ####
hm_taf5<-ode(times=times, y=start_tq, func=venmodel_taf5,parms=parameters)

# Plots ####

# model output as a tibble in long format
df1_taf5<-as_tibble(as.data.frame(hm_taf5)) %>% 
  mutate(P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Sm_d+Tm+Tm_d+Tm_lag+Tm_d_lag+Rm+Rm_d+Lm+Lm_d, 
         P2 = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re, # population w/o MDA
         IncA_from_trt_na= c(0, diff(CIncA_from_trt_na)), # asymptomatic incidence from treatment not available
         totInc = c(0, diff(CtotInc)), # total incidence (diagnosed)
         Inc1 = c(0, diff(CInc1)), # T_na incidence
         Inc2 = c(0, diff(CInc2)), # Ti_na incidence
         Inc3 = c(0, diff(CInc3)), # T_a incidence
         Inc4 = c(0, diff(CInc4)),# Ti_a incidence 
         Diagnosed = c(0, diff(Diag)), # both imported and local official diagnosed
         LocDiag = Inc1 + Inc3, #local diagnosed cases
         ImpDiag = Inc2 + Inc4, # imported diagnosed cases
         Trt1 = c(0, diff(Ctrt1)), # treated at public health system
         ImpP = c(0, diff(Imported)), # total imported cases
         ExpP = c(0, diff(Exported)), # total exported cases
         ImpP_Ci = c(0, diff(Imported_Ci)), # total clinical imported cases
         ImpP_A = c(0, diff(Imported_A)), # total asymptomatic imported cases
         Rel = c(0, diff(Rel)), # total relapses 
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2, # malaria prevalence
         MDA_tot_count = c(0,diff(MDA_tot)), # total MDA in each week
         MDA_TQ_count = c(0,diff(MDA_TQ)), # TQ MDA in each week
         MDA_PQ_count = c(0,diff(MDA_PQ)) # PQ MDA in each week
  ) %>% 
  pivot_longer(names_to = "variable", cols = !1)%>% 
  mutate(model = "Model output")

df1_taf_norm5<-as_tibble(as.data.frame(hm_taf5)) %>% 
  mutate(P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Sm_d+Tm+Tm_d+Tm_lag+Tm_d_lag+Rm+Rm_d+Lm+Lm_d, 
         P2 = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re,# population w/o MDA
         IncA_from_trt_na= c(0, diff(CIncA_from_trt_na)), # asymptomatic incidence from treatment not available
         totInc = c(0, diff(CtotInc)), # total incidence (diagnosed)
         Inc1 = c(0, diff(CInc1)), # T_na incidence
         Inc2 = c(0, diff(CInc2)), # Ti_na incidence
         Inc3 = c(0, diff(CInc3)), # T_a incidence
         Inc4 = c(0, diff(CInc4)),# Ti_a incidence 
         Diagnosed = c(0, diff(Diag)), # both imported and local official diagnosed
         LocDiag = Inc1 + Inc3, #local diagnosed cases
         ImpDiag = Inc2 + Inc4, # imported diagnosed cases
         Trt1 = c(0, diff(Ctrt1)), # treated at public health system
         ImpP = c(0, diff(Imported)), # total imported cases
         ExpP = c(0, diff(Exported)), # total exported cases
         ImpP_Ci = c(0, diff(Imported_Ci)), # total clinical imported cases
         ImpP_A = c(0, diff(Imported_A)), # total asymptomatic imported cases
         Rel = c(0, diff(Rel)), # total relapses 
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2, # malaria prevalence
         MDA_tot_count = c(0, diff(MDA_tot)), # total MDA in each week
         MDA_TQ_count = c(0,diff(MDA_TQ)), # TQ MDA in each week
         MDA_PQ_count = c(0,diff(MDA_PQ)) # PQ MDA in each week
  ) %>% 
  mutate(model = "Model output")


###############################################################################
# TAF MODEL (7 rounds) 
# Build the TQ MDA model ####

# Define dynamic Human-static Vector model #
venmodel_taf7 <- function(t, x, parameters)  {
  with(as.list(c(parameters, x)), {
    
    # Population functions ####
    
    # Total population
    P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Sm_d+Tm+Tm_d+Tm_lag+Tm_d_lag+Rm+Rm_d+Lm+Lm_d

    # Infectious population
    Infectious = (zeta2*A)+C+T_na+Ci+Ti_na+T_a+Ti_a+Tm+Tm_d
    
    # LLIN function ####
    
    # Proportion of nets used 1, 2 and 3 years after distribution
    att1 = 1 - 1/(3.25)
    att2 = 1 - 2/(3.25) 
    att3 = 1 - 3/(3.25) 
    
    # Cumulative coverage through time 
    LLIN_cov = (interventions$LLIN_sucre*no_cov)/P #proportion of the population covered by NEW nets
    LLIN_t = interventions$time
    cum_cov1 = LLIN_cov 
    cum_cov2 = lag(cum_cov1, default = 0, n = 1)
    cum_cov3 = lag(cum_cov1, default = 0, n = 2)
    cum_cov = att1*cum_cov1+ att2*cum_cov2 +att3* cum_cov3
    
    # LLIN effectiveness at reducing force of infection through time 
    LLIN_effcov = LLIN_use*LLIN_eff*pmin(cum_cov, 1)
    LLIN<-approx(LLIN_t, LLIN_effcov, t)$y
    
    # Seasonality component of local transmission ####
    seas = 0.5+(0.2*cos(pi*(2*t/52) + 6.5))
    
    # Births and deaths ####
    beta = approx(birthdeath$time, birthdeath$birthrate, t)$y # births
    mu = approx(birthdeath$time, birthdeath$deathrate, t)$y # deaths
    
    # Importation functions
    
    # Imported population matched with Sucre's population
    imp_pop <- P
    
    # Seasonality component of importation of infections 
    imp_seas = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.5)) )
    imp = imp_pop*imp_seas*tau6 # Rate and level of importation of infections
    
    # Inflate rate and level of importation to approximate variation seen
    # in available importation data from 1998-2019
    
    if (t>(52*15) & t<(52*17)) { # Fitted to data
      imp<-imp*2
    }
    
    if (t>(52*17) & t<(52*20)) { # Fitted to data 
      imp<-imp*10
    }
    
    if (t>(52*20) & t<(52*22)) { # Fitted to data
      imp<-imp*6
    }
    
    if (t > (52*22) & t < (52*24)) { # 2020-2022 COVID reduced migration
      imp<-imp*0.5
    }
    
    if (t > (52*24) & t<(52*26)) { # Assumed that pre-COVID migration levels begin to return
      imp<-imp*4
    }
    
    if (t > (52*26)) { # Assumed pre-COVID migration levels return
      imp<-imp*6
    }
    
    # Exportation functions ####
    
    # Assume same seasonal component of exportation but with a 2-week lag 
    imp_seas_lag = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.75)) )
    ex <- imp_seas_lag*tau6
    
    # Treatment and diagnostic test availability ####
    pa5 <- diminish*(approx(treatment$time, treatment$approx_cov, t, method="constant")$y)/100
    
    # Force of infection function ####
    lambda = ptrans1*(1-LLIN)*seas*(w^2*x*y*m*Infectious/P)/(w*y*Infectious/P+mu_m)*(gamma_m/(gamma_m+mu_m))
    
    # MDA component ####
    
    rounds = 7 # Programme length (years)
    mdur = 6 # 6 week duration to complete MDA round to target population
    
    # Rounds start in epidemiological weeks 1 and 16, starting in 2023
    pulse = c(c(rep(0,1300), rep(c(rep(1,mdur), rep(0, 26-mdur), rep(1,mdur), rep(0,26-mdur)),rounds)), rep(0, 1664-length(c(rep(0,1300), rep(c(rep(1,mdur), rep(0, 26-mdur), rep(1,mdur), rep(0,26-mdur)),rounds))))) # 1300 corresponds to first week in 2023
    
    # No MDA from 1998 to 2023
    mrate<-0 
    t2<-0 
    
    # MDA implementation during 2023-2025
    if(t>(52*25) & t<(52*(25+rounds))) {
      mrate = approx(1:length(pulse), (-log(1-mcov)/mdur)*pulse, t, method="constant", rule=2)$y
      t2<-1
    }
    
    # Model equations ####
    dS = (beta*P + 
            rho*R + 
            dhyp*L - 
            lambda*S - 
            mu*S - 
            ex*S +
            t2*(-mrate*S + tau8*(Sm+Sm_d))) 
    
    dE = (lambda*S - 
            gamma*E - 
            mu*E - 
            ex*E + 
            t2*(-mrate*E)) 
    
    dA = (pa1*gamma*E + 
            (1-pa3)*tau2*(C+Ci) + 
            ptrans2*pa7*pa8*tau5*L +  
            pa2*gamma*Re + 
            tau2*(T_na+Ti_na) + 
            z2*imp - 
            tau3*A -
            mu*A - 
            ex*A + 
            t2*(-mrate*A)) 
    
    dC = ((1-pa1)*gamma*E + 
            ptrans2*(1-pa7)*pa8*tau5*L +  
            (1-pa2)*gamma*Re -  
            (1-pa3)*tau2*C -   
            pa3*(1-pa5)*tau1*C -  
            pa3*pa5*sens*tau1*C - 
            mu*C - 
            ex*C + 
            t2*(-mrate*C)) 
    
    dT_na = (pa3*(1-pa5)*tau1*C - 
               tau2*T_na - 
               mu*T_na - 
               ex*T_na + 
               t2*(-mrate*T_na))
    
    dT_a = (pa3*pa5*sens*tau1*C - 
              tau7*T_a - 
              mu*T_a - 
              ex*T_a)
    
    dT_a_lag = (tau7*T_a - 
                  tau4*T_a_lag - 
                  mu*T_a_lag - 
                  ex*T_a_lag)
    
    # imported cases
    
    dCi = (z1*imp - 
             pa3*(1-pa5)*tau1*Ci - 
             pa3*pa5*sens*tau1*Ci - 
             (1-pa3)*tau2*Ci - 
             mu*Ci - 
             ex*Ci + 
             t2*(-mrate*Ci))
    
    dTi_na = (pa3*(1-pa5)*tau1*Ci - 
                tau2*Ti_na -
                mu*Ti_na - 
                ex*Ti_na + 
                t2*(-mrate*Ti_na))
    
    dTi_a = (pa3*pa5*sens*tau1*Ci - 
               tau7*Ti_a - 
               mu*Ti_a - 
               ex*Ti_a)
    
    dTi_a_lag = (tau7*Ti_a - 
                   tau4*Ti_a_lag - 
                   mu*Ti_a_lag -
                   ex*Ti_a_lag)
    
    
    dR = ((1-phyp1)*tau4*(T_a_lag + Ti_a_lag) -
            rho*R - 
            lambda*R - 
            mu*R - 
            ex*R + 
            t2*(-mrate*R + (1-phyp1)*tau4*Tm_d_lag + (1-phyp2)*tau4*Tm_lag + tau8*(Rm+Rm_d)))
    
    dL = (tau3*A + 
            phyp1*tau4*(T_a_lag + Ti_a_lag) -
            dhyp*L - 
            ptrans2*pa8*tau5*L - 
            lambda*L - 
            mu*L - 
            ex*L + 
            t2*(-mrate*L + phyp1*tau4*Tm_d_lag + phyp2*tau4*Tm_lag + tau8*(Lm+Lm_d)))
    
    dRe = (lambda*(R + L) - 
             gamma*Re - 
             mu*Re - 
             ex*Re + 
             t2*(-mrate*Re)) 
    
    
    dSm = t2*(mrate*(1-gd_det)*S - 
                tau8*Sm - 
                mu*Sm - 
                ex*Sm)
    
    dSm_d = t2*(mrate*gd_det*S - 
                  tau8*Sm_d - 
                  mu*Sm_d - 
                  ex*Sm_d)
    
    dTm = t2*(mrate*(1-gd_det)*(E+A+C+T_na+Ci+Ti_na+Re) - 
                tau7*Tm - 
                mu*Tm - 
                ex*Tm)
    
    dTm_d = t2*(mrate*gd_det*(E+A+C+T_na+Ci+Ti_na+Re) - 
                  tau7*Tm_d - 
                  mu*Tm_d - 
                  ex*Tm_d)
    
    dTm_lag = t2*(tau7*Tm - 
                    tau4*Tm_lag - 
                    mu*Tm_lag - 
                    ex*Tm_lag)
    
    dTm_d_lag = t2*(tau7*Tm_d - 
                      tau4*Tm_d_lag - 
                      mu*Tm_d_lag - 
                      ex*Tm_d_lag)
    
    dRm = t2*(mrate*(1-gd_det)*R - 
                tau8*Rm - 
                mu*Rm - 
                ex*Rm)
    
    dRm_d = t2*(mrate*gd_det*R - 
                  tau8*Rm_d - 
                  mu*Rm_d - 
                  ex*Rm_d)
    
    dLm = t2*(mrate*(1-gd_det)*L - 
                tau8*Lm - 
                mu*Lm - 
                ex*Lm)
    
    dLm_d = t2*(mrate*gd_det*L - 
                  tau8*Lm_d - 
                  mu*Lm_d - 
                  ex*Lm_d)
    
    
    # Counters ####
    
    # Used to verify the functionality of the model and calculate incidence
    
    dExposed = dE + dRe # Total exposed
    dCtotInc = lambda*S + lambda*(R+L) + pa8*tau5*L # Total local incidence including relapse
    
    # Asymptomatic incidence (local and imported) caused by lack of treatment availability
    dCIncA_from_trt_na = tau2*(T_na + Ti_na) 
    
    dCInc1 = pa3*(1-pa5)*tau1*C # T_na incidence
    dCInc2 = pa3*(1-pa5)*tau1*Ci # Ti_na incidence
    dCInc3 = pa3*pa5*sens*tau1*C # T_a incidence
    dCInc4 = pa3*pa5*sens*tau1*Ci # Ti_a incidence
    dCtrt1 = pa3*pa5*sens*tau1*(C + Ci) # Treated at public facility (local and imported)
    dRel = ptrans2*pa8*tau5*L # Relapses
    dImported = imp*(z1 + z2) # Imported cases (asymptomatic and clinical)
    dExported = ex*P # Exported population 
    dImported_Ci = imp*z1 # Clinical imported cases
    dImported_A = imp*z2 # Asymptomatic imported cases 
    
    # Diagnosed incidence
    dDiag = pa3*(1-pa5)*tau1*(C+Ci) + pa3*pa5*sens*tau1*(C+Ci)
    
    # Number of people treated with MDA
    dMDA = t2*mrate*(S+E+A+C+T_na+Ci+Ti_na+R+L+Re) 
    dMDA_TQ = t2*mrate*(1-gd_det)*(S+E+A+C+T_na+Ci+Ti_na+R+L+Re)
    dMDA_PQ = t2*mrate*gd_det*(S+E+A+C+T_na+Ci+Ti_na+R+L+Re)
    
    # Model output ####
    output <- c(dS, dE, dA, dC, dT_na, dT_a, dT_a_lag, dCi, dTi_na, dTi_a, dTi_a_lag,  dR, dL, dRe, dSm, dSm_d, dTm, dTm_d, dTm_lag, dTm_d_lag, dRm, dRm_d, dLm, dLm_d, dExposed, dCtotInc, dCIncA_from_trt_na, dCInc1, dCInc2, dCInc3, dCInc4, dCtrt1, dRel, dImported, dExported, dImported_Ci, dImported_A, dDiag, dMDA, dMDA_TQ, dMDA_PQ)
    list(output)
  })
}

# Run models ####
hm_taf7<-ode(times=times, y=start_tq, func=venmodel_taf7,parms=parameters)

# Plots ####

# model output as a tibble in long format
df1_taf7<-as_tibble(as.data.frame(hm_taf7)) %>% 
  mutate(P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Sm_d+Tm+Tm_d+Tm_lag+Tm_d_lag+Rm+Rm_d+Lm+Lm_d, 
         P2 = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re, # population w/o MDA
         IncA_from_trt_na= c(0, diff(CIncA_from_trt_na)), # asymptomatic incidence from treatment not available
         totInc = c(0, diff(CtotInc)), # total incidence (diagnosed)
         Inc1 = c(0, diff(CInc1)), # T_na incidence
         Inc2 = c(0, diff(CInc2)), # Ti_na incidence
         Inc3 = c(0, diff(CInc3)), # T_a incidence
         Inc4 = c(0, diff(CInc4)),# Ti_a incidence 
         Diagnosed = c(0, diff(Diag)), # both imported and local official diagnosed
         LocDiag = Inc1 + Inc3, #local diagnosed cases
         ImpDiag = Inc2 + Inc4, # imported diagnosed cases
         Trt1 = c(0, diff(Ctrt1)), # treated at public health system
         ImpP = c(0, diff(Imported)), # total imported cases
         ExpP = c(0, diff(Exported)), # total exported cases
         ImpP_Ci = c(0, diff(Imported_Ci)), # total clinical imported cases
         ImpP_A = c(0, diff(Imported_A)), # total asymptomatic imported cases
         Rel = c(0, diff(Rel)), # total relapses 
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2, # malaria prevalence
         MDA_tot_count = c(0,diff(MDA_tot)), # total MDA in each week
         MDA_TQ_count = c(0,diff(MDA_TQ)), # TQ MDA in each week
         MDA_PQ_count = c(0,diff(MDA_PQ)) # PQ MDA in each week
  ) %>% 
  pivot_longer(names_to = "variable", cols = !1)%>% 
  mutate(model = "Model output")

df1_taf_norm7<-as_tibble(as.data.frame(hm_taf7)) %>% 
  mutate(P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re+Sm+Sm_d+Tm+Tm_d+Tm_lag+Tm_d_lag+Rm+Rm_d+Lm+Lm_d, 
         P2 = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re,# population w/o MDA
         IncA_from_trt_na= c(0, diff(CIncA_from_trt_na)), # asymptomatic incidence from treatment not available
         totInc = c(0, diff(CtotInc)), # total incidence (diagnosed)
         Inc1 = c(0, diff(CInc1)), # T_na incidence
         Inc2 = c(0, diff(CInc2)), # Ti_na incidence
         Inc3 = c(0, diff(CInc3)), # T_a incidence
         Inc4 = c(0, diff(CInc4)),# Ti_a incidence 
         Diagnosed = c(0, diff(Diag)), # both imported and local official diagnosed
         LocDiag = Inc1 + Inc3, #local diagnosed cases
         ImpDiag = Inc2 + Inc4, # imported diagnosed cases
         Trt1 = c(0, diff(Ctrt1)), # treated at public health system
         ImpP = c(0, diff(Imported)), # total imported cases
         ExpP = c(0, diff(Exported)), # total exported cases
         ImpP_Ci = c(0, diff(Imported_Ci)), # total clinical imported cases
         ImpP_A = c(0, diff(Imported_A)), # total asymptomatic imported cases
         Rel = c(0, diff(Rel)), # total relapses 
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2, # malaria prevalence
         MDA_tot_count = c(0, diff(MDA_tot)), # total MDA in each week
         MDA_TQ_count = c(0,diff(MDA_TQ)), # TQ MDA in each week
         MDA_PQ_count = c(0,diff(MDA_PQ)) # PQ MDA in each week
  ) %>% 
  mutate(model = "Model output")

