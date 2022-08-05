library(pacman)
p_load(deSolve, tidyverse, doParallel, manipulate,readxl,reshape2,ggfortify, knitr, printr,
       plyr, dplyr, lubridate, gridExtra, reshape2, TTR, unikn, ggpubr, zoo, scales)

treatment<-read_csv("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/treatmentdata2030.csv", show_col_types = FALSE) 

dates_08_30 <- seq(ymd('2008-01-01'),ymd('2030-01-01'),by='year')

ggplot() + 
  geom_line(aes(x=dates_08_30, y=treatment$approx_cov[11:33], colour="Data")) + 
  labs(x=("Date"),y =("Treatment availability (%)"), colour="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(10)) + 
  theme_gray()

ggsave("treatmentavail.png",plot = last_plot(), width=7, height=4, device="png", path = "C:/Users/iatkinson/Documents/!_Placement Research/Plots",  dpi = 900)

  
  