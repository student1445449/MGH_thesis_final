# Import packages
library(pacman)
p_load(deSolve, tidyverse, doParallel, manipulate,readxl,reshape2,ggfortify, knitr, printr,
       plyr, dplyr, lubridate, gridExtra, reshape2, TTR, unikn, ggpubr, zoo, scales)


# Scenario 1 (50% cov)
data1 <- data.frame(
  scenario=c("PQ, 3 years", "TQ, 3 years", "PQ, 5 years", "TQ, 5 years", "PQ, 7 years", "TQ, 7 years"),
  #scenario=letters[1:6],
  value=c(43.50,46.83,53.22,56.85,58.25,62.04),
  cf_lower=c(32.90,37.28,44.36,48.98,50.08,54.85),
  cf_higher=c(54.33,57.04,62.29,65.25,66.42,69.52)
)

a<- ggplot(data1) +
  geom_bar( aes(x=scenario, y=value), stat="identity", alpha=0.7, fill="#556B2F") +
  geom_errorbar( aes(x=scenario, ymin=cf_lower, ymax=cf_higher), colour="black", width=0.4, alpha=0.9, size=0.8)+
  labs(title="                  50%", x=(""),y =("Incidence reduction (%)"), colour="Legend") +
  scale_y_continuous(limits = c(0, 90))+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_gray()


# scneario 2 (70%)
data2 <- data.frame(
  scenario=c("PQ, 3 years", "TQ, 3 years", "PQ, 5 years", "TQ, 5 years", "PQ, 7 years", "TQ, 7 years"),
  #scenario=letters[1:6],
  value=c(53.09,56.28,63.82,67.24,69.65,73.18),
  cf_lower=c(45.01,49.08,57.38,61.63,63.95,68.33),
  cf_higher=c(61.96,64.54,70.75,73.53,75.54,78.43)
)




b<- ggplot(data2) +
  geom_bar( aes(x=scenario, y=value), stat="identity", alpha=0.7, fill="#556B2F") +
  geom_errorbar( aes(x=scenario, ymin=cf_lower, ymax=cf_higher), colour="black", width=0.4, alpha=0.9, size=0.8)+
  labs(title="                  70%", x=("Scenario"),y =(""), colour="Legend") +
  scale_y_continuous(limits = c(0, 90))+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_gray()


# scenario 3 (85% cov)
data3 <- data.frame(
  scenario=c("PQ, 3 years", "TQ, 3 years", "PQ, 5 years", "TQ, 5 years", "PQ, 7 years", "TQ, 7 years"),
  #scenario=letters[1:6],
  value=c(58.94,61.89,70.25,73.40,76.58,79.80),
  cf_lower=c(52.41,56.11,65.27,69.14,72.35,76.31),
  cf_higher=c(66.76,69.14,75.98,78.54,81.17,83.80)
)


c<- ggplot(data3) +
  geom_bar( aes(x=scenario, y=value), stat="identity", alpha=0.7, fill="#556B2F") +
  geom_errorbar( aes(x=scenario, ymin=cf_lower, ymax=cf_higher), colour="black", width=0.4, alpha=0.9, size=0.8)+
  labs(title="                  85%", x=(""), y="", colour="Legend") +
  scale_y_continuous(limits = c(0, 90))+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_gray()


ggarrange(a,b,c,ncol=3, nrow=1, labels="AUTO")

ggsave("MDAbarplot3.png",plot = last_plot(), width=8, height=4, device="png", path = "C:/Users/iatkinson/Documents/!_Placement Research/Plots",  dpi = 900)
