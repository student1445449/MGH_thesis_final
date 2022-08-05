# Description ####

# - model diagram called "modelwithlag.drawio"
# - sensitivity analysis is included on various factors 
# - lag for seasonality - would this be time lag or shift the cosine function 
# back by a t value corresponding to 4 months


# Import packages ####
library(pacman)
p_load(deSolve, tidyverse, doParallel, manipulate,readxl,reshape2,ggfortify, knitr, printr,
       plyr, dplyr, lubridate, gridExtra, reshape2, TTR, unikn, ggpubr, zoo, scales)


# Import data ####
data<-read_excel("C:/Users/iatkinson/Documents/!_Placement Research/Data/Edited 4 SucreBD Final 2002-2018 De-ID.xlsx",
                 col_types = c("numeric", "numeric", "numeric", "text", "text", "text", "text", "text",
                               "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                               "text", "text"))

df_data <- as.data.frame(data)

# DATES ####

dates_23_30<-seq(ymd('2023-01-01'),ymd('2029-12-29'),by='weeks')
dates_98_30_years<-seq(ymd('1998-01-01'),ymd('2030-01-01'),by='year')
dates_08_21_years<-seq(ymd('2008-01-01'),ymd('2022-01-01'),by='year')
dates_98_30<-seq(ymd('1998-01-03'),ymd('2029-11-23'),by='weeks')
dates_02_21<-seq(ymd('2002-01-01'),ymd('2021-12-01'),by='weeks')
dates_08_21<-seq(ymd('2008-01-01'),ymd('2021-12-13'),by='weeks')
dates_08_30<-seq(ymd('2008-01-10'),ymd('2029-12-12'),by='weeks')

# Sum total incidences for all cases both originating in Sucre and imported ####

# only keep PV and MI cases

# subset data into dataframe for each year (PV and IM infections only)
inc_2002_tot <- subset(df_data, (Year == 2002 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2003_tot <- subset(df_data, (Year == 2003 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2004_tot <- subset(df_data, (Year == 2004 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2005_tot <- subset(df_data, (Year == 2005 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2006_tot <- subset(df_data, (Year == 2006 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2007_tot <- subset(df_data, (Year == 2007 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2008_tot <- subset(df_data, (Year == 2008 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2009_tot <- subset(df_data, (Year == 2009 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2010_tot <- subset(df_data, (Year == 2010 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2011_tot <- subset(df_data, (Year == 2011 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2012_tot <- subset(df_data, (Year == 2012 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2013_tot <- subset(df_data, (Year == 2013 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2014_tot <- subset(df_data, (Year == 2014 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2015_tot <- subset(df_data, (Year == 2015 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2016_tot <- subset(df_data, (Year == 2016 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2017_tot <- subset(df_data, (Year == 2017 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2018_tot <- subset(df_data, (Year == 2018 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2019_tot <- subset(df_data, (Year == 2019 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))

yearlyincidence_tot <- c() # annual incidence for each year (all cases)

# subset data for only PV and IM cases 
df_data_PV_IM_only <- subset(df_data, (`General parasite species` == "PV" | `General parasite species` == "MI" ))

for (i in seq(2002,2019)) {
  yearlyincidence_tot <- c(yearlyincidence_tot,sum(df_data_PV_IM_only[which(df_data_PV_IM_only$Year == i), 1])/i)
}

# create empty vectors to fill 
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


# weekly incidence for each year (all cases)
for (i in seq(1,52)) {
  weeklyincidence_2002_tot <- c(weeklyincidence_2002_tot,length(which(inc_2002_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2003_tot <- c(weeklyincidence_2003_tot,length(which(inc_2003_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2004_tot <- c(weeklyincidence_2004_tot,length(which(inc_2004_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2005_tot <- c(weeklyincidence_2005_tot,length(which(inc_2005_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2006_tot <- c(weeklyincidence_2006_tot,length(which(inc_2006_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2007_tot <- c(weeklyincidence_2007_tot,length(which(inc_2007_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2008_tot <- c(weeklyincidence_2008_tot,length(which(inc_2008_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2009_tot <- c(weeklyincidence_2009_tot,length(which(inc_2009_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2010_tot <- c(weeklyincidence_2010_tot,length(which(inc_2010_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2011_tot <- c(weeklyincidence_2011_tot,length(which(inc_2011_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2012_tot <- c(weeklyincidence_2012_tot,length(which(inc_2012_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2013_tot <- c(weeklyincidence_2013_tot,length(which(inc_2013_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2014_tot <- c(weeklyincidence_2014_tot,length(which(inc_2014_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2015_tot <- c(weeklyincidence_2015_tot,length(which(inc_2015_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2016_tot <- c(weeklyincidence_2016_tot,length(which(inc_2016_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2017_tot <- c(weeklyincidence_2017_tot,length(which(inc_2017_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2018_tot <- c(weeklyincidence_2018_tot,length(which(inc_2018_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2019_tot <- c(weeklyincidence_2019_tot,length(which(inc_2019_tot$"Epidemiological week reported" == i)))
  
}

# weekly incidence from 2002-2019 (all cases)
overall_weeklyincidence_tot <- c(weeklyincidence_2002_tot,weeklyincidence_2003_tot,weeklyincidence_2004_tot,
                                 weeklyincidence_2005_tot,weeklyincidence_2006_tot,weeklyincidence_2007_tot,
                                 weeklyincidence_2008_tot,weeklyincidence_2009_tot,weeklyincidence_2010_tot,
                                 weeklyincidence_2011_tot,weeklyincidence_2012_tot,weeklyincidence_2013_tot,
                                 weeklyincidence_2014_tot,weeklyincidence_2015_tot,weeklyincidence_2016_tot,
                                 weeklyincidence_2017_tot,weeklyincidence_2018_tot,weeklyincidence_2019_tot)

# weekly incidence from 2008-2019 (all cases)
overall_weeklyincidence_tot_08_19 <- c(weeklyincidence_2008_tot,weeklyincidence_2009_tot,weeklyincidence_2010_tot,
                                       weeklyincidence_2011_tot,weeklyincidence_2012_tot,weeklyincidence_2013_tot,
                                       weeklyincidence_2014_tot,weeklyincidence_2015_tot,weeklyincidence_2016_tot,
                                       weeklyincidence_2017_tot,weeklyincidence_2018_tot, weeklyincidence_2019_tot)


# Sum incidences for indigenous cases originating in Sucre ####

# subset data into dataframes for each year (indigenous cases)
inc_2002 <- subset(df_data, (Year == 2002 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2003 <- subset(df_data, (Year == 2003 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2004 <- subset(df_data, (Year == 2004 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2005 <- subset(df_data, (Year == 2005 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2006 <- subset(df_data, (Year == 2006 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2007 <- subset(df_data, (Year == 2007 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2008 <- subset(df_data, (Year == 2008 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2009 <- subset(df_data, (Year == 2009 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2010 <- subset(df_data, (Year == 2010 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2011 <- subset(df_data, (Year == 2011 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2012 <- subset(df_data, (Year == 2012 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2013 <- subset(df_data, (Year == 2013 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2014 <- subset(df_data, (Year == 2014 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2015 <- subset(df_data, (Year == 2015 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2016 <- subset(df_data, (Year == 2016 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2017 <- subset(df_data, (Year == 2017 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2018 <- subset(df_data, (Year == 2018 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2019 <- subset(df_data, (Year == 2019 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))


yearlyincidence <- c() # annual incidence vector (indigenous cases)

for (i in seq(2002,2019)) {
  yearlyincidence <- c(yearlyincidence,sum(df_data_PV_IM_only[which(df_data_PV_IM_only$Year == i & df_data_PV_IM_only$`Origin of infection (State)` == "Sucre"), 1])/i)
}


# create empty vectors to fill 
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


# weekly incidence for each year (indigenous cases)
for (i in seq(1,52)) {
  weeklyincidence_2002 <- c(weeklyincidence_2002,length(which(inc_2002$"Epidemiological week reported" == i)))
  weeklyincidence_2003 <- c(weeklyincidence_2003,length(which(inc_2003$"Epidemiological week reported" == i)))
  weeklyincidence_2004 <- c(weeklyincidence_2004,length(which(inc_2004$"Epidemiological week reported" == i)))
  weeklyincidence_2005 <- c(weeklyincidence_2005,length(which(inc_2005$"Epidemiological week reported" == i)))
  weeklyincidence_2006 <- c(weeklyincidence_2006,length(which(inc_2006$"Epidemiological week reported" == i)))
  weeklyincidence_2007 <- c(weeklyincidence_2007,length(which(inc_2007$"Epidemiological week reported" == i)))
  weeklyincidence_2008 <- c(weeklyincidence_2008,length(which(inc_2008$"Epidemiological week reported" == i)))
  weeklyincidence_2009 <- c(weeklyincidence_2009,length(which(inc_2009$"Epidemiological week reported" == i)))
  weeklyincidence_2010 <- c(weeklyincidence_2010,length(which(inc_2010$"Epidemiological week reported" == i)))
  weeklyincidence_2011 <- c(weeklyincidence_2011,length(which(inc_2011$"Epidemiological week reported" == i)))
  weeklyincidence_2012 <- c(weeklyincidence_2012,length(which(inc_2012$"Epidemiological week reported" == i)))
  weeklyincidence_2013 <- c(weeklyincidence_2013,length(which(inc_2013$"Epidemiological week reported" == i)))
  weeklyincidence_2014 <- c(weeklyincidence_2014,length(which(inc_2014$"Epidemiological week reported" == i)))
  weeklyincidence_2015 <- c(weeklyincidence_2015,length(which(inc_2015$"Epidemiological week reported" == i)))
  weeklyincidence_2016 <- c(weeklyincidence_2016,length(which(inc_2016$"Epidemiological week reported" == i)))
  weeklyincidence_2017 <- c(weeklyincidence_2017,length(which(inc_2017$"Epidemiological week reported" == i)))
  weeklyincidence_2018 <- c(weeklyincidence_2018,length(which(inc_2018$"Epidemiological week reported" == i)))
  weeklyincidence_2019 <- c(weeklyincidence_2019,length(which(inc_2019$"Epidemiological week reported" == i)))
  
}

# weekly incidence 2002-2018 (indigenous cases)
overall_weeklyincidence <- c(weeklyincidence_2002,weeklyincidence_2003,weeklyincidence_2004,
                             weeklyincidence_2005,weeklyincidence_2006,weeklyincidence_2007,
                             weeklyincidence_2008,weeklyincidence_2009,weeklyincidence_2010,
                             weeklyincidence_2011,weeklyincidence_2012,weeklyincidence_2013,
                             weeklyincidence_2014,weeklyincidence_2015,weeklyincidence_2016,
                             weeklyincidence_2017,weeklyincidence_2018,weeklyincidence_2019)

# weekly incidence 2008-2018 (indigenous cases)
overall_weeklyincidence_08_19 <- c(weeklyincidence_2008,weeklyincidence_2009,weeklyincidence_2010,
                                   weeklyincidence_2011,weeklyincidence_2012,weeklyincidence_2013,
                                   weeklyincidence_2014,weeklyincidence_2015,weeklyincidence_2016,
                                   weeklyincidence_2017,weeklyincidence_2018,weeklyincidence_2019)


# dataframe for weekly incidence 2002-2018
df2 <- data.frame(seq(1,52),cbind(weeklyincidence_2002,weeklyincidence_2003,weeklyincidence_2004,
                                  weeklyincidence_2005,weeklyincidence_2006,weeklyincidence_2007,
                                  weeklyincidence_2008,weeklyincidence_2009,weeklyincidence_2010,
                                  weeklyincidence_2011,weeklyincidence_2012,weeklyincidence_2013,
                                  weeklyincidence_2014,weeklyincidence_2015,weeklyincidence_2016,
                                  weeklyincidence_2017,weeklyincidence_2018,weeklyincidence_2019 ))

# dataframe for weekly incidence 2008-2018
df3 <- data.frame(seq(1,52*12),cbind(weeklyincidence_2008,weeklyincidence_2009,weeklyincidence_2010,weeklyincidence_2011,
                                     weeklyincidence_2012,weeklyincidence_2013,weeklyincidence_2014,weeklyincidence_2015,
                                     weeklyincidence_2016,weeklyincidence_2017,weeklyincidence_2018,weeklyincidence_2019))



# Read in data ####

# intervention, treatment availability, birth/death data
interventions<-read_csv("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/interventiondata2030.csv", show_col_types = FALSE) #
treatment<-read_csv("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/treatmentdata2030.csv", show_col_types = FALSE) #
birthdeath<-read_csv("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/birthdeath2030.csv", show_col_types = FALSE) #


# Read in parameters ####

params <- as_tibble(as.data.frame(read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/rec2.csv"), show_col_types = FALSE))) # Read excel file with all parameters

# read in parameters from excel 
parameters <- rep(0,length(params$symbol)) # create empty parameter vector

for (i in 1:length(params$symbol)){ # populate parameter vector with parameter names and values
  parameters[i] <- params$value[i]
  names(parameters) <- params$symbol
}

# Start values ####

# Population Sucre in 1998 is approx 0.8 million
# use available data for prevalence to distribute cases in each compartment 
start<-c(S=317000, E=10, A=10, C=20, T_na = 5, T_a = 20, T_a_lag = 1, Ci=0, Ti_na=0, Ti_a=0, Ti_a_lag=1, R = 317000, L = 160000, Re=10, Exposed=20,CtotInc=0, CIncA_from_trt_na=0, CInc1 =0, CInc2=0, CInc3=0, CInc4=0, Ctrt1=0, Rel=0, Imported=0, Exported=0, Imported_Ci=0, Imported_A=0, Diag=0, Rel_A=0, Rel_C=0, Inc_A=0, Inc_C=0)
start_pq<-c(S=317000, E=10, A=10, C=20, T_na = 5, T_a = 20, T_a_lag = 1, Ci=0, Ti_na=0, Ti_a=0, Ti_a_lag=1, R = 317000, L = 160000, Re=10, Sm=0, Tm=0, Tm_lag=0, Rm=0, Lm=0, Exposed=20,CtotInc=0, CIncA_from_trt_na=0, CInc1 =0, CInc2=0, CInc3=0, CInc4=0, Ctrt1=0, Rel=0, Imported=0, Exported=0, Imported_Ci=0, Imported_A=0, Diag=0, MDA=0)
start_tq<-c(S=317000, E=10, A=10, C=20, T_na = 5, T_a = 20, T_a_lag = 1, Ci=0, Ti_na=0, Ti_a=0, Ti_a_lag=1, R = 317000, L = 160000, Re=10, Sm=0, Sm_d=0, Tm=0, Tm_d=0, Tm_lag=0, Tm_d_lag=0, Rm=0, Rm_d=0, Lm=0, Lm_d=0, Exposed=20,CtotInc=0, CIncA_from_trt_na=0, CInc1 =0, CInc2=0, CInc3=0, CInc4=0, Ctrt1=0, Rel=0, Imported=0, Exported=0, Imported_Ci=0, Imported_A=0, Diag=0, MDA_tot=0, MDA_TQ=0, MDA_PQ=0)



# Time steps ####
times <- seq(0, 32*52 - 1, 1)


# Build the model ####

# Define dynamic Human-static Vector model #
venmodel <- function(t, x, parameters)  {
  with(as.list(c(parameters, x)), {
    
    # population functions
    P = S+E+A+C+T_na+T_a+T_a_lag+Ci+Ti_na+Ti_a+Ti_a_lag+R+L+Re 
    
    #browser()
    
    # infectious population
    Infectious = (zeta2*A)+C+T_na+Ci+Ti_na+T_a+Ti_a
    
    # LLIN function ####
    att1 = 1 - 1/(3.25)
    att2 = 1 - 2/(3.25) 
    att3 = 1 - 3/(3.25) 
    LLIN_cov = (interventions$LLIN_sucre*no_cov)/P #proportion of the population covered by NEW nets
    LLIN_t = interventions$time
    cum_cov1 = LLIN_cov 
    cum_cov2 = lag(cum_cov1, default = 0, n = 1)
    cum_cov3 = lag(cum_cov1, default = 0, n = 2)
    cum_cov = att1*cum_cov1+ att2*cum_cov2 +att3* cum_cov3
    LLIN_effcov = LLIN_use*LLIN_eff*pmin(cum_cov, 1)
    LLIN<-approx(LLIN_t, LLIN_effcov, t)$y
    
    # seasonal decomposition estimate function for weekly incidence ####
    seas = 0.5+(0.2*cos(pi*(2*t/52) + 6.5))
    
    # birth and death rates ####
    beta = approx(birthdeath$time, birthdeath$birthrate, t)$y # births
    mu = approx(birthdeath$time, birthdeath$deathrate, t)$y # deaths
    
    
    imp_pop <- P # match imported population cloud to Sucre population
    imp_seas = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.5)) )
    imp = imp_pop*imp_seas*tau6 # tau6 is rate of import/export
    
    # inflate imported rate/population to match contextual factors 
    
    if (t>(52*15) & t<(52*17)) {
      imp<-imp*2
    }
    
    if (t>(52*17) & t<(52*20)) {
      imp<-imp*10
    }
    
    if (t>(52*20) & t<(52*22)) {
      imp<-imp*6
    }
    
    if (t > (52*22) & t < (52*24)) { #2020-2022 covid reduced migration
      imp<-imp*0.5
    }
    
    if (t > (52*24) & t<(52*26)) { # pre covid levels return and grow again?
      imp<-imp*4
    }
    
    if (t > (52*26)) { # pre covid levels return and grow again?
      imp<-imp*6
    }
    
    
    # for covid times
    # if t> covid year start
    # migration = 0.25*imp
    
    # exportation function - seasonal lag by 2 weeks
    imp_seas_lag = t1*(0.5+ (0.2*cos(pi*(2*t/52)+7.75)) )
    ex <- imp_seas_lag*tau6
    
    # treatment and diagnostic test availability ####
    # public health system
    pa5 <- diminish*(approx(treatment$time, treatment$approx_cov, t, method="constant")$y)/100
    
    # lambda calculation ####
    lambda = ptrans1*(1-LLIN)*seas*(w^2*x*y*m*Infectious/P)/(w*y*Infectious/P+mu_m)*(gamma_m/(gamma_m+mu_m))
    
    
    # Model equations ####
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
    
    # imported cases
    
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
    
    dExposed = dE + dRe # total exposed
    dCtotInc = lambda*S + lambda*(R+L) + ptrans2*pa8*tau5*L # total indigenous incidence including relapse
    
    # clinical incidence (indigenous and imported)
    #dCInc = pa3*(1-pa5)*tau1*(C+Ci) + pa3*pa5*sens*tau1*(C+Ci)
    
    # asymptomatic incidence (indigenous and imported) caused by lack of treatment availability
    dCIncA_from_trt_na = tau2*(T_na + Ti_na) 
    
    dCInc1 = pa3*(1-pa5)*tau1*C # T_na incidence
    dCInc2 = pa3*(1-pa5)*tau1*Ci # Ti_na incidence
    dCInc3 = pa3*pa5*sens*tau1*C # T_a incidence
    dCInc4 = pa3*pa5*sens*tau1*Ci # Ti_a incidence
    dCtrt1 = pa3*pa5*sens*tau1*(C + Ci) # treated at public facility (indigenous and imported)
    dRel = ptrans2*pa8*tau5*L # relapses
    dImported = imp*(z1 + z2) # imported cases (asymptomatic and clinical)
    dExported = ex*P # exported population 
    dImported_Ci = imp*z1 # clinical imported cases
    dImported_A = imp*z2 # asymptomatic imported cases 
    
    # diagnosed incidence
    dDiag = pa3*(1-pa5)*tau1*(C+Ci) + pa3*pa5*sens*tau1*(C+Ci)
    
    dRel_A = ptrans2*pa8*pa7*tau5*L
    dRel_C = ptrans2*pa8*(1-pa7)*tau5*L
    dInc_A = pa1*gamma*E + pa2*gamma*Re
    dInc_C = (1-pa1)*gamma*E + (1-pa2)*gamma*Re
    # model output ####
    output <- c(dS, dE, dA, dC, dT_na, dT_a, dT_a_lag, dCi, dTi_na, dTi_a, dTi_a_lag,  dR, dL, dRe, dExposed, dCtotInc, dCIncA_from_trt_na, dCInc1, dCInc2, dCInc3, dCInc4, dCtrt1, dRel, dImported, dExported, dImported_Ci, dImported_A, dDiag, dRel_A, dRel_C, dInc_A, dInc_C)
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
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2, # malaria prevalence
         AR = c(0, diff(Rel_A)),
         CR = c(0, diff(Rel_C)),
         AI = c(0, diff(Inc_A)),
         CI = c(0, diff(Inc_C)), 
         L_count = c(0, diff(L))
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
         Prev = (A+C+T_na+Ci+Ti_na+T_a+Ti_a)/P2, # malaria prevalence
         AR = c(0, diff(Rel_A)),
         CR = c(0, diff(Rel_C)),
         AI = c(0, diff(Inc_A)),
         CI = c(0, diff(Inc_C)),
         L_count = c(0, diff(L))
  ) %>% 
  mutate(model = "Model output")


time_vec <- rep(dates_08_30, each=4)
df <- df1 %>% filter(variable %in% c("AR", "CI", "CR", "AI"), time>519)
df["variable"] <- factor(df[2]$variable, levels=c("AI", "AR","CI", "CR"))

df["Date"] <- time_vec
              
a1<- ggplot(df, aes(Date, value)) + 
  geom_area(aes(fill=variable), position="stack", alpha=0.8) +
  labs(x=("Date"),y =("Weekly Incidence"), fill="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(8)) + 
  theme_gray()


b1<- ggplot() + 
  geom_line(aes(x=dates_08_30, y=df1_norm$L[521:1664], colour="HR"))+
  labs(x=("Date"),y =("Prevalence"), colour="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(8)) + 
  theme_gray()






ggarrange(a1,b1, ncol=2, nrow=1, labels = "AUTO")

ggsave("casebreakdown.png",plot = last_plot(), width=8, height=4, device="png", path = "C:/Users/iatkinson/Documents/!_Placement Research/Plots",  dpi = 900)


relapses_2018<- sum(df1_norm$AR[1041:1092], df1_norm$CR[1041:1092])
totinc_2018<-sum(df1_norm$AR[1041:1092], df1_norm$CR[1041:1092], df1_norm$AI[1041:1092], df1_norm$CI[1041:1092])
  
relapses_2019<- sum(df1_norm$AR[1093:1144], df1_norm$CR[1093:1144])
totinc_2019<-sum(df1_norm$AR[1093:1144], df1_norm$CR[1093:1144], df1_norm$AI[1093:1144], df1_norm$CI[1093:1144])
