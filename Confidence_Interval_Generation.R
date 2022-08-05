# Description ####

# This code will not work as it requires each model and different sets 
# of parameters for each scenario but is shown for illustrative purposes. 

# This is a framework for generating 95% confidence intervals for each scenario

# This general framework has been run for each scenario, with varying
# parameter sets for each and the results saved. These saved files have been 
# imported to remove long code running times. 

# Load parameters ####


params <- as_tibble(as.data.frame(read_csv("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/rec03.csv", show_col_types = FALSE))) # Read excel file with all parameters
parameters <- rep(0,length(params$symbol)) # create empty parameter vector

for (i in 1:length(params$symbol)){ # populate parameter vector with parameter names and values
  parameters[i] <- params$value[i]
  names(parameters) <- params$symbol
}

store_param <- parameters
parameters2<-store_param

# Run simulations ####

model_sim_results <- list()
model_sim_results_pq3 <- list()
model_sim_results_taf3 <- list()
model_sim_results_pq5 <- list()
model_sim_results_taf5 <- list()
model_sim_results_pq7 <- list()
model_sim_results_taf7 <- list()


ptrans1_parm <- runif(100,(parameters2["ptrans1"])*0.9,(parameters2["ptrans1"]*1.1)) 
tau6_parm <- runif(100,(parameters2["tau6"])*0.75,(parameters2["tau6"]*1.25)) 
ptrans2_parm <- runif(100,(parameters2["ptrans2"])*0.75,(parameters2["ptrans2"]*1.25)) 

# run models with these stored parameters 

for (i in 1:100) {
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
  
  # 3 rounds ####
  
  # PQ MDA
  
  hm_sim_pq3<-ode(times=times_pq, y=start_pq, func=venmodel_pq3,parms=parameters2)
  
  df1_sim_pq3<-as_tibble(as.data.frame(hm_sim_pq3)) %>% 
    mutate(Inc1 = c(0, diff(CInc1)), # T_na incidence
           Inc3 = c(0, diff(CInc3)), # T_a incidence
           LocDiag = Inc1 + Inc3 #local diagnosed cases
    ) %>% 
    mutate(model = "Model output")
  
  df1_sim_pq23<-as_tibble(as.data.frame(hm_sim_pq3)) %>% 
    mutate(Inc1 = c(0, diff(CInc1)), # T_na incidence
           Inc3 = c(0, diff(CInc3)), # T_a incidence
           LocDiag = Inc1 + Inc3 #local diagnosed cases
    ) %>% 
    pivot_longer(names_to = "variable", cols = !1)%>% 
    mutate(model = "Model output")
  
  model_sim_results_pq3[[i]] <- df1_sim_pq3$LocDiag
  
  # TQ MDA 
  
  hm_sim_taf3<-ode(times=times_taf, y=start_taf, func=venmodel_taf3,parms=parameters2)
  
  df1_sim_taf3<-as_tibble(as.data.frame(hm_sim_taf3)) %>% 
    mutate(Inc1 = c(0, diff(CInc1)), # T_na incidence
           Inc3 = c(0, diff(CInc3)), # T_a incidence
           LocDiag = Inc1 + Inc3 #local diagnosed cases
    ) %>% 
    mutate(model = "Model output")
  
  df1_sim_taf23<-as_tibble(as.data.frame(hm_sim_taf3)) %>% 
    mutate(Inc1 = c(0, diff(CInc1)), # T_na incidence
           Inc3 = c(0, diff(CInc3)), # T_a incidence
           LocDiag = Inc1 + Inc3 #local diagnosed cases
    ) %>% 
    pivot_longer(names_to = "variable", cols = !1)%>% 
    mutate(model = "Model output")
  
  
  model_sim_results_taf3[[i]] <- df1_sim_taf3$LocDiag
  
  # 5 rounds ####
  
  # PQ MDA
  
  hm_sim_pq5<-ode(times=times_pq, y=start_pq, func=venmodel_pq5,parms=parameters2)
  
  df1_sim_pq5<-as_tibble(as.data.frame(hm_sim_pq5)) %>% 
    mutate(Inc1 = c(0, diff(CInc1)), # T_na incidence
           Inc3 = c(0, diff(CInc3)), # T_a incidence
           LocDiag = Inc1 + Inc3 #local diagnosed cases
    ) %>% 
    mutate(model = "Model output")
  
  df1_sim_pq25<-as_tibble(as.data.frame(hm_sim_pq5)) %>% 
    mutate(Inc1 = c(0, diff(CInc1)), # T_na incidence
           Inc3 = c(0, diff(CInc3)), # T_a incidence
           LocDiag = Inc1 + Inc3 #local diagnosed cases
    ) %>% 
    pivot_longer(names_to = "variable", cols = !1)%>% 
    mutate(model = "Model output")
  
  model_sim_results_pq5[[i]] <- df1_sim_pq5$LocDiag
  
  # TQ MDA 
  
  hm_sim_taf5<-ode(times=times_taf, y=start_taf, func=venmodel_taf5,parms=parameters2)
  
  df1_sim_taf5<-as_tibble(as.data.frame(hm_sim_taf5)) %>% 
    mutate(Inc1 = c(0, diff(CInc1)), # T_na incidence
           Inc3 = c(0, diff(CInc3)), # T_a incidence
           LocDiag = Inc1 + Inc3 #local diagnosed cases
    ) %>% 
    mutate(model = "Model output")
  
  df1_sim_taf25<-as_tibble(as.data.frame(hm_sim_taf5)) %>% 
    mutate(Inc1 = c(0, diff(CInc1)), # T_na incidence
           Inc3 = c(0, diff(CInc3)), # T_a incidence
           LocDiag = Inc1 + Inc3 #local diagnosed cases
    ) %>% 
    pivot_longer(names_to = "variable", cols = !1)%>% 
    mutate(model = "Model output")
  
  
  model_sim_results_taf5[[i]] <- df1_sim_taf5$LocDiag
  
  # 7 rounds ####
  
  # PQ MDA
  
  hm_sim_pq7<-ode(times=times_pq, y=start_pq, func=venmodel_pq7,parms=parameters2)
  
  df1_sim_pq7<-as_tibble(as.data.frame(hm_sim_pq7)) %>% 
    mutate(Inc1 = c(0, diff(CInc1)), # T_na incidence
           Inc3 = c(0, diff(CInc3)), # T_a incidence
           LocDiag = Inc1 + Inc3 #local diagnosed cases
    ) %>% 
    mutate(model = "Model output")
  
  df1_sim_pq27<-as_tibble(as.data.frame(hm_sim_pq7)) %>% 
    mutate(Inc1 = c(0, diff(CInc1)), # T_na incidence
           Inc3 = c(0, diff(CInc3)), # T_a incidence
           LocDiag = Inc1 + Inc3 #local diagnosed cases
    ) %>% 
    pivot_longer(names_to = "variable", cols = !1)%>% 
    mutate(model = "Model output")
  
  model_sim_results_pq7[[i]] <- df1_sim_pq7$LocDiag
  
  # TQ MDA 
  
  hm_sim_taf7<-ode(times=times_taf, y=start_taf, func=venmodel_taf7,parms=parameters2)
  
  df1_sim_taf7<-as_tibble(as.data.frame(hm_sim_taf7)) %>% 
    mutate(Inc1 = c(0, diff(CInc1)), # T_na incidence
           Inc3 = c(0, diff(CInc3)), # T_a incidence
           LocDiag = Inc1 + Inc3 #local diagnosed cases
    ) %>% 
    mutate(model = "Model output")
  
  df1_sim_taf27<-as_tibble(as.data.frame(hm_sim_taf7)) %>% 
    mutate(Inc1 = c(0, diff(CInc1)), # T_na incidence
           Inc3 = c(0, diff(CInc3)), # T_a incidence
           LocDiag = Inc1 + Inc3 #local diagnosed cases
    ) %>% 
    pivot_longer(names_to = "variable", cols = !1)%>% 
    mutate(model = "Model output")
  
  
  model_sim_results_taf7[[i]] <- df1_sim_taf7$LocDiag
  
  
  
}


# Store simulation results ####

# base model
overall_sim_results = do.call(cbind, model_sim_results)

# 3 rounds

#PQ mda
overall_sim_results_pq3 = do.call(cbind, model_sim_results_pq3)

# taf mda
overall_sim_results_taf3 = do.call(cbind, model_sim_results_taf3)

# 5 rounds

#PQ mda
overall_sim_results_pq5 = do.call(cbind, model_sim_results_pq5)

# taf mda
overall_sim_results_taf5 = do.call(cbind, model_sim_results_taf5)

# 7 rounds

#PQ mda
overall_sim_results_pq7 = do.call(cbind, model_sim_results_pq7)

# taf mda
overall_sim_results_taf7 = do.call(cbind, model_sim_results_taf7)

# Generate confidence intervals ####
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

