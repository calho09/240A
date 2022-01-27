rm(list=ls())    # clear memory
#install.packages("sandwich")
library(sandwich)
library(tidyverse)
set.seed(2022)

setwd("C:/Users/holmc_000/Documents/0My Docs/0UC Davis/240A")

#QUESTION 1


#1.1 pop values of beta hat

# for logx ~ n(0,1)
covxy <- 1*exp(0+.5*1)
varx <- (exp(1)-1)*exp(1)
bhat01 <- covxy/varx

#n31
covxy <- 1*exp(3+.5*1)
varx <- (exp(1)-1)*exp(6+1)
bhat31 <- covxy/varx

#n03
covxy <- 3*exp(0+.5*3)
varx <- (exp(3)-1)*exp(0+3)
bhat03 <- covxy/varx

#1.2

## number of repetitions
num_reps<-1000

## Define a simulation function 
sim_func <- function(rep,n,dist_sim){ 
  df <- tibble(xx = exp( dist_list[[dist_sim]](n)),
               err = rnorm(n=n,0,1),
               Y = log(xx)+err)
  reg <- lm(Y~xx, data=df)
  results <- list(beta=coefficients(reg)[2],cov=cov(x=df$xx,y=df$Y),var=var(df$xx))  # extract the slope coefficient from each simulation
  return(results)  
}


## Parameter Settings
param_list <- expand.grid(rep=1:num_reps, n=c(10,100,1000), dist_sim = c("Normal01", "Normal31", "Normal03")) 

#param_list <- expand.grid(rep=1:num_reps, n=c(1,10,100), dist_sim = c("Normal01", "Normal31", "Normal03")) #for testing

## Define list of distributions to use in simulation function
dist_list = list(Normal01 = function(n) rnorm(n, 0, 1),
                 Normal31 = function(n) rnorm(n, 3, 1),
                 Normal03 = function(n) rnorm(n, 0, 3))


## Run simulation
sim_out <- param_list %>%
  mutate(results=pmap(param_list,sim_func)) %>%
  unnest_wider(results) 


## Print result
sim_print <- group_by(sim_out,n,dist_sim) %>%
  mutate(mean_beta=mean(beta)) %>%
  ungroup() %>%
  select(n,dist_sim,mean_beta) %>%
  distinct() %>%
  pivot_wider(names_from=dist_sim, values_from=mean_beta)

sim_print


#QUESTION 2 

rm(list=ls())    # clear memory
library(tidyverse)
library(sandwich)
set.seed(2022)
setwd("C:/Users/holmc_000/Documents/0My Docs/0UC Davis/240A")


## number of repetitions
num_reps2<-1000

## Define a simulation function 
sim_func2 <- function(rep,n,n1,omega){ 
  df <- tibble(xx = c(rep(1, n1), rep(0, n-n1)),
               err = c(rnorm(n=n1,0,omega^2), rnorm(n=n-n1, 0,1)),
               Y = err)
  reg <- lm(Y~xx, data=df)
  sigmas <- c(rep(omega, n1), rep(1, n-n1))
  results <-
    list(
      beta = coefficients(reg)[2],
      sigB_0 = sqrt(vcovHC(reg, type = 'const')[2,2]),
      sigB_ideal = sqrt(vcovHC(reg, omega = sigmas ^ 2)[2,2]),
      sigB_HC0 = sqrt(vcovHC(reg, type ='HC0')[2,2]),
      sigB_HC1 = sqrt(vcovHC(reg, type ='HC1')[2,2]),
      sigB_HC2 = sqrt(vcovHC(reg, type ='HC2')[2,2]),
      tstat = coef(summary(reg))["xx", "t value"]
    )  # extract the slope coefficient from each simulation
  return(results)
}

# n1<-3
# n<-100
# omega<-1
# rep<-1
# 
# df <- tibble(xx = c(rep(1, n1), rep(0, n-n1)),
#              err = c(rnorm(n=n1,0,omega^2), rnorm(n=n-n1, 0,1)),
#              Y = err)
# reg <- lm(Y~xx, data=df)
# test<-coef(summary(reg))["xx", "t value"]


## Parameter Settings
param_list2 <- expand.grid(rep=1:num_reps2, n=100, n1=c(3,10,25), omega = c(1,2)) 


## Run simulation
sim_out2 <- param_list2 %>%
  mutate(results=pmap(param_list2,sim_func2)) %>%
  unnest_wider(results) 


## Print result
sim_print <- group_by(sim_out2,n1, omega) %>%
  mutate(sd_beta2=sd(beta), mean_0=mean(sigB_0), mean_idl=mean(sigB_ideal), mean_HC0=mean(sigB_HC0), mean_HC1=mean(sigB_HC1), mean_HC2 = mean(sigB_HC2), p_rej= length(tstat[tstat>1.96])/length(tstat)) %>%
  ungroup() %>%
  select(n1,omega,sd_beta2, mean_0, mean_idl, mean_HC0, mean_HC1, mean_HC2, p_rej) %>%
  distinct() 

sim_print

