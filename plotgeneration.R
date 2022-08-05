
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

dates_98_30_years<-seq(ymd('1998-01-01'),ymd('2030-01-01'),by='year')
dates_08_21_years<-seq(ymd('2008-01-01'),ymd('2022-01-01'),by='year')
dates_98_30<-seq(ymd('1998-01-03'),ymd('2029-11-23'),by='weeks')
dates_02_21<-seq(ymd('2002-01-01'),ymd('2021-12-01'),by='weeks')
dates_08_21<-seq(ymd('2008-01-01'),ymd('2021-12-13'),by='weeks')
dates_08_30<-seq(ymd('2008-01-03'),ymd('2029-11-30'),by='weeks')

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



# Plotting incidence ####

# incidence from 2002-2018 (indigenous)
dates_02_19 <- seq(as.Date("2002-01-15"), as.Date("2019-12-19"), by="weeks")

ggplot()+
  geom_line(aes(x = dates_02_19, y=overall_weeklyincidence, colour="Incidence")) + 
  theme_gray()+
  scale_x_date(breaks = scales::breaks_pretty(10))+ 
  labs(x="Date", y="Weekly incidence", colour="Legend", title="Weekly indigenous malaria incidence in Sucre State,\nVenezuela 2002-2018")


# quick investigation into weekly incidence (indigenous) for each year
Epidemiological_week = rep(seq(1,52,by=1),times=18)

df2 %>% tidyr::gather("weekly_inc", "Incidence", 2:19) %>% 
  ggplot(., aes(Epidemiological_week, Incidence))+
  geom_line()+
  theme_gray()+
  facet_wrap(~weekly_inc)

# weekly incidence from 2013-2019
df2 %>%
  ggplot()+  
  geom_line(aes(x = seq.1..52., y=weeklyincidence_2013, colour="2013"))+ 
  geom_line(aes(x = seq.1..52., y=weeklyincidence_2014, colour="2014"))+ 
  geom_line(aes(x = seq.1..52., y=weeklyincidence_2015, colour="2015"))+
  geom_line(aes(x = seq.1..52., y=weeklyincidence_2016, colour="2016"))+
  geom_line(aes(x = seq.1..52., y=weeklyincidence_2017, colour="2017"))+
  geom_line(aes(x = seq.1..52., y=weeklyincidence_2018, colour="2018"))+
  geom_line(aes(x = seq.1..52., y=weeklyincidence_2019, colour="2019"))+
  theme_gray()+
  scale_color_manual(values=c("#556B2F", "#ffc125", "#cd5c5c", "#009acd", "#8968cd", "#8b8989", "black"))+
  labs(title="Weekly indigenous malaria incidence for years 2013-2019", x="Epidemiological week", y="Incidence", colour="Year")

# incidence data since 2008 which we will run the model with 
dates_08_19<-seq(ymd('2008-01-01'),ymd('2019-12-15'),by='weeks')


since_2009 <- c(weeklyincidence_2008,weeklyincidence_2009,weeklyincidence_2010,weeklyincidence_2011,weeklyincidence_2012,
                weeklyincidence_2013,weeklyincidence_2014, weeklyincidence_2015, weeklyincidence_2016,
                weeklyincidence_2017, weeklyincidence_2018, weeklyincidence_2019 )

# weekly indigenous incidence 2008-2018
df3 %>% ggplot()+  
  geom_line(aes(x=dates_08_19, y=since_2009, colour="Incidence"))+ 
  geom_vline(xintercept=c(as.Date("2008-01-01"),as.Date("2009-01-01"),as.Date("2010-01-01"),as.Date("2011-01-01"),as.Date("2012-01-01"),as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),as.Date("2018-01-01"),as.Date("2019-01-01"),as.Date("2020-01-01")), lty="dashed", color="gray") + 
  geom_smooth(formula=y~x, aes(x=dates_08_19, y=since_2009, colour="LOESS Line of best fit"), method="loess", show.legend = TRUE, size=0.5)+
  labs(title="Weekly indigenous malaria incidence from 2008-2019", x="Year", y="Incidence", colour="Legend:") + 
  theme_gray()+
  scale_color_manual(values=c("#556B2F", "#ffc125"))  


# weekly indigenous incidence 2013-2019
# assume reporting occurs on a monday so dates are first and last mondays of the starting/ending year respectively
dates_prev_7_years <- seq(as.Date("2013-01-07"), as.Date("2019-12-25"), by="weeks")
prev_7_years <- c(weeklyincidence_2013,weeklyincidence_2014, weeklyincidence_2015, weeklyincidence_2016,
                  weeklyincidence_2017, weeklyincidence_2018,weeklyincidence_2019 )

df3[261:624,c(1,7:12)] %>% # need to cut off some times to get last 6 years
  ggplot()+  
  geom_line(aes(x=dates_prev_7_years, y=prev_7_years, colour="Incidence"))+ 
  geom_vline(xintercept=c(as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),as.Date("2018-01-01"),as.Date("2019-01-01")), lty="dashed") + 
  geom_smooth(formula=y~x, aes(x=dates_prev_7_years, y=prev_7_years, colour="LOESS Line of best fit"), method="loess", show.legend = TRUE, size=0.5)+
  labs(title="Weekly malaria incidence originating in Sucre \nState, Venezuela from 2013-2019", x="Year", y="Incidence", colour="Legend:") + 
  scale_color_manual(values=c("#556B2F", "#ffc125"))  


# Time series decomposition to investigate seasonality ####

# dataframe for last 6 years 
df_prev_7_years <- data.frame(dates_prev_7_years,prev_7_years)
p <- ts(prev_7_years, frequency=52, start=c(2013,1), end=c(2019,52))

# Time series decomposition for the last 6 years of data to use in model
p %>%
  stl(s.window = "periodic") %>%
  autoplot(xlab="Date", ylab="Weekly incidence", main="Time series decomposition of weekly malaria incidence \noriginating in Sucre State, Venezuela from 2013-2019", colour="#556B2F")+ 
  geom_vline(xintercept=c(2013,2014,2015,2016,2017,2018,2019), lty="dashed", color="gray")  

# Find an equation for the seasonality component 

# fitting a trig function to the dataset
x<-seq(from=2013, to=2019.99, by=1/52)

ggplot() + 
  geom_line(aes(x=x, y=decompose(p, "additive")$seasonal, colour="Seasonal decomposition")) + 
  geom_line(aes(x=x, y=(220*cos(pi*(2*x) + 6.5 )), colour="Fitted cosine function"))+
  labs(x="Date", y="Seasonal component", colour="Legend", title="Fitted cosine function to seasonal \ndecompositon of indigenous incidence")+
  scale_color_manual(values=c("#ffc125", "#556B2F"))  


# compress cosine function for the range -1 < y < 1 

ggplot() + 
  geom_line(aes(x, (0.5+(0.2*cos(pi*(2*x)+6.5))), colour="Compressed fitted cosine function")) + 
  labs(x="Date", y="Compressed seasonal component", title="Compressed fitted cosine function to seasonal \ndecomposition of indigenous incidence data", colour="Legend")+
  scale_color_manual(values=c("#556B2F")) 


# the function above can be used in the model to impact the FOI 
# but change x to t/52 since the model runs in weeks

# Descriptive analysis ####

# Histogram of indigenous cases by age of patient from 2013-2019
# 336 entries are not included due to missing age data
df_data %>% 
  filter(df_data$`Origin of infection (State)` == "Sucre" & df_data$Year > 2012) %>%
  ggplot(aes(x=Age, fill=Sex)) +
  geom_histogram(binwidth=1, position = "identity", alpha=0.3)+
  theme_gray()+
  scale_color_manual(values=c("#556B2F","#ffc125")) + 
  labs(title="Distribution of patient ages for infections originating in \nSucre State, Venezuela between 2013-2019", y="Sum")  

# histogram of ages by sex for all cases (local and imported)
df_data %>%
  ggplot(aes(x=Age, fill=Sex)) +
  geom_histogram(binwidth=1, position = "identity", alpha=0.4)+
  theme_gray()+
  scale_color_manual(values=c("#556B2F","#ffc125")) + 
  labs(title="Distribution of patient ages for infections originating in \nSucre State, Venezuela between 2002-2018", y="Sum")  


# total indigenous cases by sex between 2013-2019
df_data %>%
  filter(df_data$`Origin of infection (State)` == "Sucre" & df_data$Year > 2012) %>%
  ggplot(aes(x = Sex, fill=Sex)) +
  geom_bar(stat="count", alpha=0.7, position = "identity") + 
  scale_fill_manual("Sex", values = c("Female" = "#556B2F", "Male" = "#ffc125")) + 
  stat_count(geom = "text", colour = "black", size = 2.5,
             aes(label = ..count..),position=position_stack(vjust=1.02)) + 
  theme_gray()+
  labs(title = "Total malaria incidence originating in Sucre State, \nVenezeuala by sex between 2013-2019", x="Sex", y="Incidence")

# bar chart for total incidence by types of species 2002-2018
PV_count <- rep(c("PV"), times=(count(df_data$`General parasite species` == "PV" & df_data$`Origin of infection (State)` == "Sucre")$freq[2]))
MI_count <- rep(c("MI"), times=(count(df_data$`General parasite species` == "MI" & df_data$`Origin of infection (State)` == "Sucre")$freq[2]))
species <- data.frame((c(PV_count, MI_count)))

species %>%
  ggplot(aes(x=X.c.PV_count..MI_count.., fill=X.c.PV_count..MI_count..)) + 
  geom_bar(alpha=0.7) + 
  stat_count(geom = "text", colour = "black", size = 2.5,
             aes(label = ..count..), vjust=-0.5) +   
  scale_fill_manual("Species", values = c("MI" = "#556B2F", "PV" = "#ffc125")) + 
  labs(title = "Malaria incidence originating in Sucre State, \nVenezuela by species from 2002-2018", x="Species", y="Cases")


# bar chart for total incidence by types of species 2013-2019

PV_count_13_19 <- rep(c("PV"), times=(count(df_data$`General parasite species` == "PV" & df_data$`Origin of infection (State)` == "Sucre" & df_data$Year > 2012)$freq[2]))
MI_count_13_19 <- rep(c("MI"), times=(count(df_data$`General parasite species` == "MI" & df_data$`Origin of infection (State)` == "Sucre" & df_data$Year > 2012)$freq[2]))
species_13_19 <- data.frame((c(PV_count_13_19, MI_count_13_19)))

species_13_19 %>%
  ggplot(aes(x=X.c.PV_count_13_19..MI_count_13_19.., fill=X.c.PV_count_13_19..MI_count_13_19..)) + 
  geom_bar(alpha=0.7) + 
  stat_count(geom = "text", colour = "black", size = 2.5,
             aes(label = ..count..), vjust=-0.5) +   
  scale_fill_manual("Species", values = c("MI" = "#556B2F", "PV" = "#ffc125")) + 
  labs(title = "Malaria incidence originating in Sucre State,  \nVenezuela by species from 2013-2019", x="Species", y="Cases")


# bar chart for types of case 2002-2018
indig_count <- rep(c("Indigenous"), times=(count(df_data$`Type of case` == "Indigenous")$freq[2]))
imp_count <- rep(c("Imported"), times=(count(df_data$`Type of case` == "Imported")$freq[2]))
type_case <- data.frame(c(indig_count, imp_count))

type_case %>%
  ggplot(aes(x=c.indig_count..imp_count., fill=c.indig_count..imp_count.)) + 
  geom_bar(alpha=0.7) + 
  stat_count(geom = "text", colour = "black", size = 2.5,
             aes(label = ..count..), vjust=-0.5) +   
  scale_fill_manual("Species", values = c("Indigenous" = "#556B2F", "Imported" = "#ffc125")) + 
  labs(title = "Malaria incidence originating in Sucre State, \nVenezuela by type of case from 2002-2018", x="Type of case", y="Incidence")


# bar chart for types of case 2013-2019
indig_count_13_19 <- rep(c("Indigenous"), times=(count(df_data$`Type of case` == "Indigenous" & df_data$Year > 2012)$freq[2]))
imp_count_13_19 <- rep(c("Imported"), times=(count(df_data$`Type of case` == "Imported"  & df_data$Year > 2012)$freq[2]))
type_case_13_19 <- data.frame(c(indig_count_13_19,  imp_count_13_19))

type_case_13_19 %>%
  ggplot(aes(x=c.indig_count_13_19..imp_count_13_19., fill=c.indig_count_13_19..imp_count_13_19.)) + 
  geom_bar(alpha=0.7) + 
  stat_count(geom = "text", colour = "black", size = 2.5,
             aes(label = ..count..), vjust=-0.5) +  
  scale_fill_manual("Species", values = c("Indigenous" = "#556B2F", "Imported" = "#ffc125")) + 
  labs(title = "Malaria incidence originating in Sucre State, \nVenezuela by type of case from 2013-2019", x="Type of case", y="Incidence") 




# Investigating migration patterns and time series decomposition of imported case data ####

# subset data into imported cases
mig1 <- subset(df_data, (`Type of case` == "Imported") & (`General parasite species` == "PV" | `General parasite species` == "MI")) 

# subset imported cases by year 
inc_2002_mig1 <- subset(mig1, Year == 2002)
inc_2003_mig1 <- subset(mig1, Year == 2003)
inc_2004_mig1 <- subset(mig1, Year == 2004)
inc_2005_mig1 <- subset(mig1, Year == 2005)
inc_2006_mig1 <- subset(mig1, Year == 2006)
inc_2007_mig1 <- subset(mig1, Year == 2007)
inc_2008_mig1 <- subset(mig1, Year == 2008)
inc_2009_mig1 <- subset(mig1, Year == 2009)
inc_2010_mig1 <- subset(mig1, Year == 2010)
inc_2011_mig1 <- subset(mig1, Year == 2011)
inc_2012_mig1 <- subset(mig1, Year == 2012)
inc_2013_mig1 <- subset(mig1, Year == 2013)
inc_2014_mig1 <- subset(mig1, Year == 2014)
inc_2015_mig1 <- subset(mig1, Year == 2015)
inc_2016_mig1 <- subset(mig1, Year == 2016)
inc_2017_mig1 <- subset(mig1, Year == 2017)
inc_2018_mig1 <- subset(mig1, Year == 2018)
inc_2019_mig1 <- subset(mig1, Year == 2019)


yearlyincidence_mig1 <- c() # yearly imported incidence

for (i in seq(2002,2019)) {
  yearlyincidence_mig1 <- c(yearlyincidence_mig1,sum(mig1[which(mig1$Year == i), 1])/i)
}

# create empty vectors to fill
weeklyincidence_2002_mig1 <- c()
weeklyincidence_2003_mig1 <- c()
weeklyincidence_2004_mig1 <- c()
weeklyincidence_2005_mig1 <- c()
weeklyincidence_2006_mig1 <- c()
weeklyincidence_2007_mig1 <- c()
weeklyincidence_2008_mig1 <- c()
weeklyincidence_2009_mig1 <- c()
weeklyincidence_2010_mig1 <- c()
weeklyincidence_2011_mig1 <- c()
weeklyincidence_2012_mig1 <- c()
weeklyincidence_2013_mig1 <- c()
weeklyincidence_2014_mig1 <- c()
weeklyincidence_2015_mig1 <- c()
weeklyincidence_2016_mig1 <- c()
weeklyincidence_2017_mig1 <- c()
weeklyincidence_2018_mig1 <- c()
weeklyincidence_2019_mig1 <- c()


# weekly imported incidence by year 
for (i in seq(1,52)) {
  weeklyincidence_2002_mig1 <- c(weeklyincidence_2002_mig1,length(which(inc_2002_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2003_mig1 <- c(weeklyincidence_2003_mig1,length(which(inc_2003_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2004_mig1 <- c(weeklyincidence_2004_mig1,length(which(inc_2004_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2005_mig1 <- c(weeklyincidence_2005_mig1,length(which(inc_2005_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2006_mig1 <- c(weeklyincidence_2006_mig1,length(which(inc_2006_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2007_mig1 <- c(weeklyincidence_2007_mig1,length(which(inc_2007_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2008_mig1 <- c(weeklyincidence_2008_mig1,length(which(inc_2008_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2009_mig1 <- c(weeklyincidence_2009_mig1,length(which(inc_2009_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2010_mig1 <- c(weeklyincidence_2010_mig1,length(which(inc_2010_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2011_mig1 <- c(weeklyincidence_2011_mig1,length(which(inc_2011_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2012_mig1 <- c(weeklyincidence_2012_mig1,length(which(inc_2012_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2013_mig1 <- c(weeklyincidence_2013_mig1,length(which(inc_2013_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2014_mig1 <- c(weeklyincidence_2014_mig1,length(which(inc_2014_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2015_mig1 <- c(weeklyincidence_2015_mig1,length(which(inc_2015_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2016_mig1 <- c(weeklyincidence_2016_mig1,length(which(inc_2016_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2017_mig1 <- c(weeklyincidence_2017_mig1,length(which(inc_2017_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2018_mig1 <- c(weeklyincidence_2018_mig1,length(which(inc_2018_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2019_mig1 <- c(weeklyincidence_2019_mig1,length(which(inc_2019_mig1$"Epidemiological week reported" == i)))
}

# weekly imported incidence 2002-2018
overall_weeklyincidence_mig1 <- c(weeklyincidence_2002_mig1,weeklyincidence_2003_mig1,weeklyincidence_2004_mig1,
                                  weeklyincidence_2005_mig1,weeklyincidence_2006_mig1,weeklyincidence_2007_mig1,
                                  weeklyincidence_2008_mig1,weeklyincidence_2009_mig1,weeklyincidence_2010_mig1,
                                  weeklyincidence_2011_mig1,weeklyincidence_2012_mig1,weeklyincidence_2013_mig1,
                                  weeklyincidence_2014_mig1,weeklyincidence_2015_mig1,weeklyincidence_2016_mig1,
                                  weeklyincidence_2017_mig1,weeklyincidence_2018_mig1,weeklyincidence_2019_mig1)

# weekly imported incidence 2008-2018
overall_weeklyincidence_mig1_08_19 <- c(weeklyincidence_2008_mig1,weeklyincidence_2009_mig1,weeklyincidence_2010_mig1,
                                        weeklyincidence_2011_mig1,weeklyincidence_2012_mig1,weeklyincidence_2013_mig1,
                                        weeklyincidence_2014_mig1,weeklyincidence_2015_mig1,weeklyincidence_2016_mig1,
                                        weeklyincidence_2017_mig1,weeklyincidence_2018_mig1,weeklyincidence_2019_mig1)


# dataframe for weekly imported incidence 2002-2018
df2_mig1 <- data.frame(seq(1,52),cbind(weeklyincidence_2002_mig1,weeklyincidence_2003_mig1,weeklyincidence_2004_mig1,
                                       weeklyincidence_2005_mig1,weeklyincidence_2006_mig1,weeklyincidence_2007_mig1,
                                       weeklyincidence_2008_mig1,weeklyincidence_2009_mig1,weeklyincidence_2010_mig1,
                                       weeklyincidence_2011_mig1,weeklyincidence_2012_mig1,weeklyincidence_2013_mig1,
                                       weeklyincidence_2014_mig1,weeklyincidence_2015_mig1,weeklyincidence_2016_mig1,
                                       weeklyincidence_2017_mig1,weeklyincidence_2018_mig1,weeklyincidence_2019_mig1))

# dataframe for weekly imported incidence 2008-2018
df3_mig1 <- data.frame(seq(1,52*12),cbind(weeklyincidence_2008_mig1,weeklyincidence_2009_mig1,weeklyincidence_2010_mig1,weeklyincidence_2011_mig1,
                                          weeklyincidence_2012_mig1,weeklyincidence_2013_mig1,weeklyincidence_2014_mig1,weeklyincidence_2015_mig1,
                                          weeklyincidence_2016_mig1,weeklyincidence_2017_mig1,weeklyincidence_2018_mig1,weeklyincidence_2019_mig1))

# weekly imported incidence 2013-2019
dates_mig1 <- seq(as.Date("2013-01-07"), as.Date("2019-12-25"), by="weeks")
prev_7_years_mig1 <- c(weeklyincidence_2013_mig1,weeklyincidence_2014_mig1, weeklyincidence_2015_mig1, weeklyincidence_2016_mig1,
                       weeklyincidence_2017_mig1, weeklyincidence_2018_mig1,weeklyincidence_2019_mig1)

df3_mig1[261:624,c(1,7:12)] %>% # need to cut off some times to get last 312 times 
  ggplot()+  
  geom_line(aes(x=dates_mig1, y=prev_7_years_mig1, colour="Incidence"))+ 
  geom_vline(xintercept=c(as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),as.Date("2018-01-01"),as.Date("2019-01-01"),as.Date("2020-01-01")), lty="dashed") + 
  geom_smooth(formula=y~x, aes(x=dates_mig1, y=prev_7_years_mig1, colour="LOESS Line of best fit"), method="loess", show.legend = TRUE, size=0.5)+
  labs(title="Weekly imported malaria incidence imported from 2013-2019", x="Year", y="Incidence", colour="Legend:") + 
  scale_color_manual(values=c("#556B2F", "#ffc125"))  


# time series decomposition of 2013-2019 weekly imported incidence data
df_prev_7_years_mig1 <- data.frame(dates_mig1,prev_7_years_mig1)
p_mig1 <- ts(prev_7_years_mig1, frequency=52, start=c(2013,1), end=c(2019,52))

p_mig1 %>%
  stl(s.window = "periodic") %>%
  autoplot(xlab="Date", ylab="Weekly incidence", main="Time series decomposition of weekly malaria incidence \nintroduced into Sucre State, Venezuela from 2013-2019", colour="#556B2F")+ 
  geom_vline(xintercept=c(2013,2014,2015,2016,2017,2018,2019,2020), lty="dashed", color="gray")  

# do the seasonal decomposition for the data before the unexpected peak to investigate seasonality 

dates_mig12 <- seq(as.Date("2013-01-07"), as.Date("2015-12-30"), by="weeks")
prev_7_years_mig12 <- c(weeklyincidence_2013_mig1,weeklyincidence_2014_mig1, weeklyincidence_2015_mig1)
df_prev_7_years_mig12 <- data.frame(dates_mig12,prev_7_years_mig12)
p_mig12 <- ts(prev_7_years_mig12, frequency=52, start=c(2013,1), end=c(2015,52))

p_mig12 %>%
  stl(s.window = "periodic") %>%
  autoplot(xlab="Date", ylab="Weekly incidence", main="Time series decomposition of weekly malaria incidence \nintroduced into Sucre State, Venezuela from 2013-2015 \n(before unexpected peak in 2016)", colour="#556B2F")+ 
  geom_vline(xintercept=c(2013,2014,2015), lty="dashed", color="gray")  

# Find an equation for the seasonality component of weekly importated incidence

# fitting a trig function to the dataset
x<-seq(from=2013, to=2019.99, by=1/52)

ggplot() + 
  geom_line(aes(x=x, y=decompose(p_mig1, "additive")$seasonal, colour="Seasonal decomposition")) + 
  geom_line(aes(x=x, y=17 + (30*cos(pi*(2*x) +1)), colour="Fitted cosine function"))+
  labs(x="Date", y="Seasonal component", colour="Legend", title="Fitted cosine function to seasonal \ndecompositon of imported incidence") + 
  scale_color_manual(values=c("#ffc125", "#556B2F"))  

# compress cosine function for the range -1 < y < 1 

ggplot() + 
  geom_line(aes(x, (0.5+(0.2*cos(pi*(2*x)+7.5))), colour="Compressed fitted cosine function")) + 
  labs(x="Date", y="Compressed seasonal component", title="Compressed fitted cosine function to seasonal \ndecomposition of imported incidence data", colour="Legend") +
  scale_color_manual(values=c("#556B2F")) 

# the function above can be used in the model to impact the imported seasonality 
# variable but change x to t/52 since the model runs in weeks



# ORIGINAL MODEL

# Build the model ####

#Read in intervention, treatment availability, birth/death data
interventions<-read_excel("C:/Users/iatkinson/Documents/!_Placement Research/Data/interventiondata2030.xlsx")
treatment<-read_excel("C:/Users/iatkinson/Documents/!_Placement Research/Data/treatmentdata2030.xlsx")
birthdeath<-read_excel("C:/Users/iatkinson/Documents/!_Placement Research/Data/birthdeath2030.xlsx")

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
    pa5 <- (approx(treatment$time, treatment$approx_cov, t, method="constant")$y)/100
    
    # informal market
    #pa6 <-pa5*1
    
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
    
    # model output ####
    output <- c(dS, dE, dA, dC, dT_na, dT_a, dT_a_lag, dCi, dTi_na, dTi_a, dTi_a_lag,  dR, dL, dRe, dExposed, dCtotInc, dCIncA_from_trt_na, dCInc1, dCInc2, dCInc3, dCInc4, dCtrt1, dRel, dImported, dExported, dImported_Ci, dImported_A, dDiag)
    list(output)
  })
}

# Initial values ####

# Population Sucre in 1998 is approx 0.8 million
# use available data for prevalence to distribute cases in each compartment 
start<-c(S=317000, E=10, A=10, C=20, T_na = 5, T_a = 20, T_a_lag = 1, Ci=0, Ti_na=0, Ti_a=0, Ti_a_lag=1, R = 317000, L = 160000, Re=10, Exposed=20,CtotInc=0, CIncA_from_trt_na=0, CInc1 =0, CInc2=0, CInc3=0, CInc4=0, Ctrt1=0, Rel=0, Imported=0, Exported=0, Imported_Ci=0, Imported_A=0, Diag=0)

# Parameters ####

# read in parameters from excel 
params <- as_tibble(as.data.frame(read_excel("C:/Users/iatkinson/Documents/!_Placement Research/Data/rec03.xlsx", range="A1:D51", col_names=TRUE, col_types=c("text","text", "numeric", "text")))) # Read excel file with all parameters
parameters <- rep(0,length(params$symbol)) # create empty parameter vector

for (i in 1:length(params$symbol)){ # populate parameter vector with parameter names and values
  parameters[i] <- params$value[i]
  names(parameters) <- params$symbol
}

# Time steps ####

# time step vector in weeks
times <- seq(0, 32*52 - 1, 1)

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


ggplot() + 
  geom_line(aes(x=dates_98_30, y=df1_norm$LocDiag))


# 95% confidence intervals ####

params <- as_tibble(as.data.frame(read_excel("C:/Users/iatkinson/Documents/!_Placement Research/Data/rec03.xlsx", range="A1:D51", col_names=TRUE, col_types=c("text","text", "numeric", "text")))) # Read excel file with all parameters
parameters <- rep(0,length(params$symbol)) # create empty parameter vector

for (i in 1:length(params$symbol)){ # populate parameter vector with parameter names and values
  parameters[i] <- params$value[i]
  names(parameters) <- params$symbol
}

store_param <- parameters
parameters2<-store_param

ptrans1_parm<- 1.01*c(1.228391, 1.359190, 1.269314, 1.216003, 1.264602, 1.274258, 1.346448, 1.384249, 1.340007, 1.468695, 1.373993, 1.449462,
                1.366033, 1.217961, 1.309850, 1.236237, 1.291008, 1.412272, 1.349870, 1.454024, 1.443839, 1.345600, 1.327972, 1.438366,
                1.263529, 1.240660, 1.284329, 1.324117, 1.373041, 1.478820, 1.300379, 1.336397, 1.417112, 1.267837, 1.298384, 1.328311,
                1.254241, 1.387945, 1.452872, 1.274968, 1.353860, 1.333337, 1.378934, 1.437995, 1.296845, 1.464244, 1.361134, 1.219801,
                1.379180, 1.423373, 1.473718, 1.367578, 1.402356, 1.246460, 1.350609, 1.273853, 1.262557, 1.460302, 1.376556, 1.358251,
                1.370259, 1.223830, 1.354129, 1.243468, 1.409611, 1.476049, 1.235482, 1.274963, 1.370018, 1.275078, 1.319144, 1.233983,
                1.273740, 1.283044, 1.389973, 1.241555, 1.475368, 1.320740, 1.359424, 1.227187, 1.350576, 1.306320, 1.310927, 1.332257,
                1.308154, 1.307261, 1.405805, 1.308119, 1.342987, 1.238247, 1.265923, 1.458558, 1.243260, 1.221252, 1.442672, 1.265518,
                1.270853, 1.307989, 1.281524, 1.405562, 0.8, 0.8, 1.9, 1.9)

tau6_parm<- c(7.396633e-06, 1.589251e-05, 1.279170e-05, 1.515046e-05, 1.723484e-05, 1.484720e-05, 1.633142e-05, 1.612364e-05,
              1.172224e-05, 1.178564e-05, 1.266267e-05, 1.567962e-05, 7.496067e-06, 1.269259e-05, 7.598352e-06, 1.697895e-05,
              1.114045e-05, 1.079275e-05, 1.417555e-05, 9.918899e-06, 1.530714e-05, 1.555056e-05, 7.840595e-06, 8.968812e-06,
              8.040971e-06, 7.535455e-06, 1.174265e-05, 7.810392e-06, 1.416864e-05, 1.298016e-05, 1.669983e-05, 7.133118e-06,
              8.794539e-06, 1.640256e-05, 8.076320e-06, 1.677570e-05, 1.515724e-05, 1.526118e-05, 1.070966e-05, 1.503686e-05,
              1.681714e-05, 8.970094e-06, 7.424312e-06, 9.150304e-06, 1.213858e-05, 1.265769e-05, 8.190018e-06, 1.343380e-05,
              9.353190e-06, 6.990069e-06, 1.197024e-05, 9.665796e-06, 1.183530e-05, 1.114761e-05, 1.613942e-05, 1.189561e-05,
              1.418555e-05, 1.125855e-05, 1.160734e-05, 1.378629e-05, 1.050221e-05, 1.550621e-05, 1.728133e-05, 1.235766e-05,
              1.004471e-05, 1.334411e-05, 7.094536e-06, 1.689113e-05, 8.828532e-06, 1.459604e-05, 1.720960e-05, 1.478000e-05,
              9.630963e-06, 7.668745e-06, 1.089233e-05, 9.196813e-06, 1.578069e-05, 1.057269e-05, 1.531035e-05, 1.624972e-05,
              1.431032e-05, 1.726717e-05, 9.721841e-06, 7.017109e-06, 1.423683e-05, 8.812505e-06, 1.475131e-05, 7.545613e-06,
              9.007474e-06, 1.685328e-05, 8.125311e-06, 1.576385e-05, 1.413231e-05, 1.548551e-05, 1.021849e-05, 9.786091e-06,
              1.138669e-05, 1.596507e-05, 1.055766e-05, 1.384661e-05, 1.384661e-05, 1.384661e-05, 1.384661e-05, 1.384661e-05)

ptrans2_parm<- c(1.843622, 1.130500, 1.799220, 1.677853, 1.774066, 1.605730, 1.497168, 1.562151, 1.348626, 1.867108, 1.728076, 1.194719,
                 1.478720, 1.239244, 1.768198, 1.310010, 1.781410, 1.588375, 1.188726, 1.381084, 1.852597, 1.759709, 1.671652, 1.357203,
                 1.819738, 1.562790, 1.316558, 1.478317, 1.511283, 1.641314, 1.479439, 1.294806, 1.643938, 1.786382, 1.553606, 1.626082,
                 1.772471, 1.246002, 1.869590, 1.490122, 1.579859, 1.709296, 1.289197, 1.193181, 1.674851, 1.539847, 1.220300, 1.173145,
                 1.861209, 1.454410, 1.523484, 1.333450, 1.766105, 1.602602, 1.645777, 1.533332, 1.646807, 1.600776, 1.166686, 1.590517,
                 1.127895, 1.685969, 1.223815, 1.224030, 1.428683, 1.439755, 1.539200, 1.683628, 1.776728, 1.441354, 1.326001, 1.456431,
                 1.595106, 1.148918, 1.526747, 1.296150, 1.482745, 1.369020, 1.617623, 1.542725, 1.571518, 1.639476, 1.792539, 1.491128,
                 1.151497, 1.861023, 1.347259, 1.636401, 1.809984, 1.697059, 1.175665, 1.263382, 1.772305, 1.758217, 1.806751, 1.799157,
                 1.660097, 1.799224, 1.816649, 1.408223, 1.408223, 1.408223, 1.408223)

model_sim_results <- list()



# run models with these stored parameters 

for (i in 1:104) {
  print(i)
  parameters2["ptrans1"] <- ptrans1_parm[i]
  parameters2["tau6"] <- tau6_parm[i]  
  parameters2["ptrans2"] <- ptrans2_parm[i]  
  
  
  # NO MDA model ####
  hm_sim<-ode(times=times, y=start, func=venmodel,parms=parameters2)
  
  df1_sim<-as_tibble(as.data.frame(hm_sim)) %>% 
    mutate(Inc1 = c(0, diff(CInc1)), # T_na incidence
           Inc3 = c(0, diff(CInc3)), # T_a incidence
           LocDiag = Inc1 + Inc3 #local diagnosed cases
    ) %>% 
    mutate(model = "Model output")
  
  df1_sim2<-as_tibble(as.data.frame(hm_sim)) %>% 
    mutate(Inc1 = c(0, diff(CInc1)), # T_na incidence
           Inc3 = c(0, diff(CInc3)), # T_a incidence
           LocDiag = Inc1 + Inc3 #local diagnosed cases
    ) %>% 
    pivot_longer(names_to = "variable", cols = !1)%>% 
    mutate(model = "Model output")
  
  model_sim_results[[i]] <- df1_sim$LocDiag
  
}

# access only 2008-2030 part of model_sim_results

model_sim_results_08_30 <- list()

for (i in 1:104) {
  model_sim_results_08_30[[i]] <- model_sim_results[[i]][521:1664]
}

#model
overall_sim_results = do.call(cbind, model_sim_results_08_30)
write.csv(overall_sim_results,"C:/Users/iatkinson/Documents/!_Placement Research/Data/IncidenceResults.csv", row.names = FALSE)




#model
conf_int <- apply(overall_sim_results, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l <- conf_int[1,]
conf_int_u <- conf_int[2,]

# 3 rounds 
ggplot() + 
  geom_line(aes(x=dates_08_30, y=(df1 %>%
                                    filter(variable %in% c("LocDiag"), time>519))$value, colour="Model fit")) + 
  geom_line(aes(x=dates_08_19, y=overall_weeklyincidence_08_19, colour="Data")) + 
  geom_ribbon(aes(x = dates_08_30, ymin=conf_int_l, ymax=conf_int_u), fill="red", alpha=0.2)+
  labs(title = "", x=("Date"),y =("Incidence"), colour="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(10)) + 
  theme_gray()

ggsave("model_acc_imput.png",plot = last_plot(), width=7, height=4, device="png", path = "C:/Users/iatkinson/Documents/!_Placement Research/Plots/Incidence",  dpi = 900)
