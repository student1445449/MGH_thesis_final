# Import packages ####
library(pacman)
p_load(deSolve, tidyverse, doParallel, manipulate,readxl,reshape2,ggfortify, knitr, printr,
       plyr, dplyr, lubridate, gridExtra, reshape2, TTR, unikn, ggpubr, zoo, scales)


# Define scenario variables ####

coverage <- "50"
param_file <- "rec05"

params <- as_tibble(as.data.frame(read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/", param_file, ".csv"), show_col_types = FALSE))) # Read excel file with all parameters

# Run Models ####

source("Model_Script.R")

# Confidence intervals ####

#model
overall_sim_results = read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis_", coverage, "cov/main/sim_results.csv"), show_col_types = FALSE)
# 3 rounds

#PQ mda
overall_sim_results_pq3 = read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis_", coverage, "cov/main/sim_results_pq3.csv"), show_col_types = FALSE)

# taf mda
overall_sim_results_taf3 = read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis_", coverage, "cov/main/sim_results_taf3.csv"), show_col_types = FALSE)

# 5 rounds

#PQ mda
overall_sim_results_pq5 = read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis_", coverage, "cov/main/sim_results_pq5.csv"), show_col_types = FALSE)
# taf mda
overall_sim_results_taf5 = read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis_", coverage, "cov/main/sim_results_taf5.csv"), show_col_types = FALSE)
# 7 rounds

#PQ mda
overall_sim_results_pq7 = read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis_", coverage, "cov/main/sim_results_pq7.csv"), show_col_types = FALSE)
# taf mda
overall_sim_results_taf7 = read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis_", coverage, "cov/main/sim_results_taf7.csv"), show_col_types = FALSE)


#model
conf_int <- apply(overall_sim_results, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l <- conf_int[1,]
conf_int_u <- conf_int[2,]

# 3 rounds
# pq mda 
conf_int_pq3 <- apply(overall_sim_results_pq3, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l_pq3 <- conf_int_pq3[1,]
conf_int_u_pq3 <- conf_int_pq3[2,]

# taf mda
conf_int_taf3 <- apply(overall_sim_results_taf3, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l_taf3 <- conf_int_taf3[1,]
conf_int_u_taf3 <- conf_int_taf3[2,]


# 5 rounds
# pq mda 
conf_int_pq5 <- apply(overall_sim_results_pq5, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l_pq5 <- conf_int_pq5[1,]
conf_int_u_pq5 <- conf_int_pq5[2,]

# taf mda
conf_int_taf5 <- apply(overall_sim_results_taf5, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l_taf5 <- conf_int_taf5[1,]
conf_int_u_taf5 <- conf_int_taf5[2,]


# 7 rounds
# pq mda 
conf_int_pq7 <- apply(overall_sim_results_pq7, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l_pq7 <- conf_int_pq7[1,]
conf_int_u_pq7 <- conf_int_pq7[2,]

# taf mda
conf_int_taf7 <- apply(overall_sim_results_taf7, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l_taf7 <- conf_int_taf7[1,]
conf_int_u_taf7 <- conf_int_taf7[2,]


# 3 rounds 
ggplot() + 
  geom_line(aes(x=dates_23_30, y=(df1 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="Model output")) + 
  geom_line(aes(x=dates_23_30, y=(df1_pq3 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="Model PQ MDA output")) + 
  geom_line(aes(x=dates_23_30, y=(df1_taf3 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="Model TQ MDA output")) + 
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l[1301:1664], ymax=conf_int_u[1301:1664]), fill="red", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_pq3[1301:1664], ymax=conf_int_u_pq3[1301:1664]), fill="green", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_taf3[1301:1664], ymax=conf_int_u_taf3[1301:1664]), fill="blue", alpha=0.2)+
  labs(title = paste0("Impact of 3 year MDA programme (", coverage, "% \ncoverage) on diagnosed weekly incidence"), x=("Date"),y =("Incidence"), colour="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(8)) + 
  theme_gray()



# 5 rounds 
ggplot() + 
  geom_line(aes(x=dates_23_30, y=(df1 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="Model output")) + 
  geom_line(aes(x=dates_23_30, y=(df1_pq5 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="Model PQ MDA output")) + 
  geom_line(aes(x=dates_23_30, y=(df1_taf5 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="Model TQ MDA output")) + 
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l[1301:1664], ymax=conf_int_u[1301:1664]), fill="red", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_pq5[1301:1664], ymax=conf_int_u_pq5[1301:1664]), fill="green", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_taf5[1301:1664], ymax=conf_int_u_taf5[1301:1664]), fill="blue", alpha=0.2)+
  labs(title = paste0("Impact of 5 year MDA programme (", coverage, "% \ncoverage) on diagnosed weekly incidence"), x=("Date"),y =("Incidence"), colour="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(8)) + 
  theme_gray()

# 7 rounds 
ggplot() + 
  geom_line(aes(x=dates_23_30, y=(df1 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="Model output")) + 
  geom_line(aes(x=dates_23_30, y=(df1_pq7 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="Model PQ MDA output")) + 
  geom_line(aes(x=dates_23_30, y=(df1_taf7 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="Model TQ MDA output")) + 
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l[1301:1664], ymax=conf_int_u[1301:1664]), fill="red", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_pq7[1301:1664], ymax=conf_int_u_pq7[1301:1664]), fill="green", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_taf7[1301:1664], ymax=conf_int_u_taf7[1301:1664]), fill="blue", alpha=0.2)+
  labs(title = paste0("Impact of 7 year MDA programme (", coverage, "% \ncoverage) on diagnosed weekly incidence"), x=("Date"),y =("Incidence"), colour="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(8)) + 
  theme_gray()

# PQ success

# PQ all rounds 
ggplot() + 
  geom_line(aes(x=dates_23_30, y=(df1 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="No MDA")) + 
  geom_line(aes(x=dates_23_30, y=(df1_pq3 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="PQ MDA (3 years)")) + 
  geom_line(aes(x=dates_23_30, y=(df1_pq5 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="PQ MDA (5 years)")) + 
  geom_line(aes(x=dates_23_30, y=(df1_pq7 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="PQ MDA (7 years)")) + 
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l[1301:1664], ymax=conf_int_u[1301:1664]), fill="red", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_pq3[1301:1664], ymax=conf_int_u_pq3[1301:1664]), fill="green", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_pq5[1301:1664], ymax=conf_int_u_pq5[1301:1664]), fill="blue", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_pq7[1301:1664], ymax=conf_int_u_pq7[1301:1664]), fill="purple", alpha=0.2)+
  labs(title = paste0("Impact of PQ MDA for 3,5 and 7 years (", coverage, "% coverage) \non diagnosed weekly incidence"), x=("Date"),y =("Incidence"), colour="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(8)) + 
  theme_gray()

# TQ success

# TQ all rounds 
ggplot() + 
  geom_line(aes(x=dates_23_30, y=(df1 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="No MDA")) + 
  geom_line(aes(x=dates_23_30, y=(df1_taf3 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="TQ MDA (3 years)")) + 
  geom_line(aes(x=dates_23_30, y=(df1_taf5 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="TQ MDA (5 years)")) + 
  geom_line(aes(x=dates_23_30, y=(df1_taf7 %>%
                                    filter(variable %in% c("LocDiag"), time>1299))$value, colour="TQ MDA (7 years)")) + 
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l[1301:1664], ymax=conf_int_u[1301:1664]), fill="red", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_taf3[1301:1664], ymax=conf_int_u_taf3[1301:1664]), fill="green", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_taf5[1301:1664], ymax=conf_int_u_taf5[1301:1664]), fill="blue", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_taf7[1301:1664], ymax=conf_int_u_taf7[1301:1664]), fill="purple", alpha=0.2)+
  labs(title = paste0("Impact of TQ MDA for 3,5 and 7 years (", coverage, "% coverage) \non diagnosed weekly incidence"), x=("Date"),y =("Incidence"), colour="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(8)) + 
  theme_gray()


# Percentage reductions in annual cases 2023-2030 ####

pd <- function(x,y){
  
  return((x-y)/x * 100)
  
}

model<-sum((df1 %>%filter(variable %in% c("LocDiag"), time>1299))$value)
model_upper<-sum(conf_int_u[1301:1664])
model_lower<-sum(conf_int_l[1301:1664])

MDA_pq3<-sum((df1_pq3 %>%filter(variable %in% c("LocDiag"), time>1299))$value)
MDA_upper_pq3<-sum(conf_int_u_pq3[1301:1664]) 
MDA_lower_pq3<-sum(conf_int_l_pq3[1301:1664])

MDA_pq5<-sum((df1_pq5 %>%filter(variable %in% c("LocDiag"), time>1299))$value)
MDA_upper_pq5<-sum(conf_int_u_pq5[1301:1664]) 
MDA_lower_pq5<-sum(conf_int_l_pq5[1301:1664])

MDA_pq7<-sum((df1_pq7 %>%filter(variable %in% c("LocDiag"), time>1299))$value)
MDA_upper_pq7<-sum(conf_int_u_pq7[1301:1664]) 
MDA_lower_pq7<-sum(conf_int_l_pq7[1301:1664])

MDA_taf3<-sum((df1_taf3 %>%filter(variable %in% c("LocDiag"), time>1299))$value)
MDA_upper_taf3<-sum(conf_int_u_taf3[1301:1664]) 
MDA_lower_taf3<-sum(conf_int_l_taf3[1301:1664])

MDA_taf5<-sum((df1_taf5 %>%filter(variable %in% c("LocDiag"), time>1299))$value)
MDA_upper_taf5<-sum(conf_int_u_taf5[1301:1664]) 
MDA_lower_taf5<-sum(conf_int_l_taf5[1301:1664])

MDA_taf7<-sum((df1_taf7 %>%filter(variable %in% c("LocDiag"), time>1299))$value)
MDA_upper_taf7<-sum(conf_int_u_taf7[1301:1664]) 
MDA_lower_taf7<-sum(conf_int_l_taf7[1301:1664])

Scenario <- c(paste0("PQ, 3 years, ", coverage, "% coverage"),
              paste0("PQ, 5 years, ", coverage, "% coverage"),
              paste0("PQ, 7 years, ", coverage, "% coverage"),
              paste0("TQ, 3 years, ", coverage, "% coverage"),
              paste0("TQ, 5 years, ", coverage, "% coverage"),
              paste0("TQ, 7 years, ", coverage, "% coverage"))

Average <- c(paste0(round(pd(model,MDA_pq3), digits=4), "%"),
             paste0(round(pd(model,MDA_pq5), digits=4), "%"),
             paste0(round(pd(model,MDA_pq7), digits=4), "%"),
             paste0(round(pd(model,MDA_taf3), digits=4), "%"),
             paste0(round(pd(model,MDA_taf5), digits=4), "%"),
             paste0(round(pd(model,MDA_taf7), digits=4), "%"))

Lower_Bound <- c(paste0(round(pd(model_lower, MDA_upper_pq3), digits=4),"%"),
                 paste0(round(pd(model_lower, MDA_upper_pq5), digits=4),"%"),
                 paste0(round(pd(model_lower, MDA_upper_pq7), digits=4),"%"),
                 paste0(round(pd(model_lower, MDA_upper_taf3), digits=4),"%"),
                 paste0(round(pd(model_lower, MDA_upper_taf5), digits=4),"%"),
                 paste0(round(pd(model_lower, MDA_upper_taf7), digits=4),"%"))

Upper_Bound <- c(paste0(round(pd(model_upper, MDA_lower_pq3), digits=4), "%"),
                 paste0(round(pd(model_upper, MDA_lower_pq5), digits=4), "%"),
                 paste0(round(pd(model_upper, MDA_lower_pq7), digits=4), "%"),
                 paste0(round(pd(model_upper, MDA_lower_taf3), digits=4), "%"),
                 paste0(round(pd(model_upper, MDA_lower_taf5), digits=4), "%"),
                 paste0(round(pd(model_upper, MDA_lower_taf7), digits=4), "%"))

Results <- data.frame(Scenario, Average, Lower_Bound, Upper_Bound)
print(Results)