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


## ------------------------------------------------------ Inits & Params

params <- set1(output = "P", birth.adj = 2, fit.adj = 1, K = 10000, prop_treat = 0.9, 
               prop.insecticide = 0, NW = 200, prop.prophylaxis = 0.1)
inits <- set1(output = "I", birth.adj = 2, fit.adj = 1, K = 10000, prop_treat = 0.9, 
               prop.insecticide = 0, NW = 200, prop.prophylaxis = 0.1)

qual_check_no0(params) # ensure there are no negative values
qual_check_no0(inits) # ensure there are no negative values

r0_sensitive <- r0_sen_calc(params, inits)
r0_sensitive

## ------------------------------------------------------ Test: Equilibrium Check

times <- seq(0, 5000, 1)
out <-ode(y = inits, parms = params, func = AAT_AMR_dens_dep, times = times)
out <- as.data.frame(out)
names(out)[names(out) == 'time'] <- "times"
model <- totals(out)
tail(model)


equilibrium_R(r0_sensitive, model)

quick_plot(model)


## ------------------------------------------------------ Test: Vary Wildlife

NW.vec <- c(0,50,100,200)
storage <- rep(NA, length(NW.vec))

for (i in 1:length(NW.vec)){
  params <- set1(output = "P", birth.adj = 2, fit.adj = 1, K = 10000, prop_treat = 0.9, 
                 prop.insecticide = 0, NW = NW.vec[i], prop.prophylaxis = 0)
  inits <- set1(output = "I", birth.adj = 2, fit.adj = 1, K = 10000, prop_treat = 0.9, 
                prop.insecticide = 0, NW = NW.vec[i], prop.prophylaxis = 0)
  
  qual_check_no0(params) # ensure there are no negative values
  qual_check_no0(inits) # ensure there are no negative values
  
  r0_sensitive <- r0_sen_calc(params, inits)
  storage[i] <- r0_sensitive[1]
  
}
storage <- as.numeric(storage)

plot(storage ~ NW.vec, ylim=c(0, max(storage)+10), type = "b", ylab = "R0", xlab = "No. wildlife")


