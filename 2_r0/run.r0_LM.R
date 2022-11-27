## --------------------- EXECUTE: R0
##
##
##
##





## ------------------------------------------------------ LOAD FUNCTIONS

library(deSolve)

source("funcs/AAT_AMR_dens_dep.R")
source("funcs/r0.R")
source("funcs/set1_params_inits.R")
source("funcs/qual_check.R")
source("funcs/quick_plot.R")


## ------------------------------------------------------ Inits & Params

params <- set1(output = "P", birth.adj = 2, fit.adj = 1, K = 5000, prop_treat = 0.3, 
               prop.insecticide = 0.05, NW = 50, prop.prophylaxis = 0.1)
inits <- set1(output = "I", birth.adj = 2, fit.adj = 1, K = 5000, prop_treat = 0.3, 
               prop.insecticide = 0.05, NW = 50, prop.prophylaxis = 0.1)

qual_check_no0(params) # ensure there are no negative values
qual_check_no0(inits) # ensure there are no negative values

#r0_sensitive <- r0_sen_calc(params, inits)
#r0_sensitive

## ------------------------------------------------------ Test: Equilibrium Check

times <- seq(0, 2500, 1)
out <-ode(y = inits, parms = params, func = AAT_AMR_dens_dep, times = times, method = "daspk")
out <- as.data.frame(out)
names(out)[names(out) == 'time'] <- "times"

model <- totals(out)
head(model)
tail(model)

quick_plot(model)

## ------------------------------------------------------ Test: Equilibrium Check

## ------------------------------------------------------ 
#Alternative - do all in one function and feed in relevant susceptible populations
Nc = inits["CS"] + inits["CIs"]
Np = inits["PS"]
Nw = inits["WS"]
Nv = inits["VSt"]
r0_sen_calc_LM2(params, Nc, Np, Nw, Nv)
last <- tail(model,1)

Nc = last$CS
Np = last$PS
Nw = last$WS
Nv = last$VSt + last$VSf
r0_sen_calc_LM2(params, Nc, Np, Nw, Nv)

sen <- "yes"
r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen)

sen <- "no"
Np = last$PS + last$PF
r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen)



