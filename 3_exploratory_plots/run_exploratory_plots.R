## --------------------- EXECUTE: exploratory_plots
##
##
##
##





## ------------------------------------------------------ LOAD FUNCTIONS

library(deSolve)
library(tictoc)
library(progress)
library(ggplot2)
library(dplyr)
library(gghighlight)
library(cowplot)
library(crayon)

source("funcs/AAT_AMR_dens_dep.R")
source("funcs/r0.R")
source("funcs/set1_params_inits.R")
source("funcs/qual_check.R")
source("funcs/quick_plot.R")








loops <- FALSE


if (loops == TRUE) {
  N_wl <- c(0, 100, 250)
  treat.prop.vecA <- seq(0, 0.9, by = 0.2)      #full from 0-1
  treat.prop.vecB <- seq(0.91, 0.99, by = 0.02)
  treat.prop.vec <- c(treat.prop.vecA, treat.prop.vecB)
  K.vec <- c(10000, 6000, 2000)
  fit.adj.vec <- c(0.6)
  birth.adj.vec <- c(2)
  prop.insecticide.vec <- c(0.0, 0.05, 0.1, 0.15, 0.20, 0.40, 0.50, 0.75,0.99 )
  prop.prophylaxis <- 0
} else {
  N_wl <- 100
  treat.prop.vec <- 0.1
  #equil.vector_pop <- c(3000)
  fit.adj.vec <- c(1)
  K.vec <- 10000
  prop.insecticide.vec <- 0
  birth.adj.vec <- 2
  prop.prophylaxis <- 0
}


df <-data.frame(CS = c(), total.cattle = c(), treat_prop = c(), W_st = c(), R_eq_sen = c(), 
                R0_sen = c(), R_eq_res = c(), R0_res = c(), No_trt_cat = c(), 
                Incidence = c(), Vector_no = c(), Prob_onward_tran = c(), 
                Risk = c(), prevalence = c(), vector_mortality = c(), fit.adj = c(), 
                prop.insecticide =c(), birth.adj =c(), eq_pop = c(), K = c())

df2 <- data.frame()

## ---- Run time estimates
tic()
start.time <- Sys.time()
total_steps <- length(birth.adj.vec) * length(prop.insecticide.vec) * length(fit.adj.vec) * length(K.vec) * length(N_wl) * length(treat.prop.vec)
step_started <- 0
progress_floor <- 0

## ---- Execute model

for (birth.adj in birth.adj.vec) {
  for (prop.insecticide in prop.insecticide.vec) {
    for (fit.adj in fit.adj.vec) {
      for (K in K.vec) {
        for (NW in N_wl) {
          for (prop_treat in treat.prop.vec) {
            
            step_started <- step_started + 1
            progress_old <- progress_floor
            progress <- step_started/total_steps*100
            progress_floor <- floor(progress)
            if (progress_floor == progress_old + 1){
              current.time <- Sys.time()
              time_taken_minutes <- as.numeric(difftime(current.time, start.time, units = "mins"))
              time_remaining <- 100*time_taken_minutes/progress - time_taken_minutes
              cat(red("% Complete = ", round(progress,2), "Time elapsed =", round(time_taken_minutes,2), 
                      "mins", "Time left =", round(time_remaining,2), "\n"))}
            
          
            params <- set1(output = "P", birth.adj = birth.adj, fit.adj = fit.adj, 
                           K = K, prop_treat = prop_treat, prop.insecticide = prop.insecticide, 
                           NW = NW, prop.prophylaxis = prop.prophylaxis)
            inits <- set1(output = "I", birth.adj = birth.adj, fit.adj = fit.adj, 
                          K = K, prop_treat = prop_treat, prop.insecticide = prop.insecticide, 
                          NW = NW, prop.prophylaxis = prop.prophylaxis)
            
            qual_check_no0(params) # ensure there are no negative values
            qual_check_no0(inits) # ensure there are no negative values
            
            
            
            myvars <- names(params) %in% c("resusceptible", "resusceptible.w") 
            params <- params[!myvars]
            
            myvars2 <- names(inits) %in% c("CR", "PR", "WR") 
            inits <- inits[!myvars2]
            
            
            ## R0 calculations
            
            Nc = inits["CS"] + inits["CIs"]
            Np = inits["PS"]
            Nw = inits["WS"]
            Nv = inits["VSt"]
            sen <- "yes"
            R0s <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen)
            sen <- "no"
            R0r <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen)
            
            
          
            ## Times ----
            times <- seq(0, 3000, 1)
            
            ## RUN MODEL ----
            out <-ode(y = inits, parms = params, func = AAT_AMR_dens_dep, times = times,
                      rootfunc = my_rootfun2,events = list(root = TRUE, terminalroot = c(1,2)))
            out <- as.data.frame(out)
            names(out)[names(out) == 'time'] <- "times"
            
            #if (loops == FALSE) { plot(CEs ~ times, data = out)}
            
            last <- tail(out, 1)
            last$total.cattle <- last$CS + last$CEs + last$CEr + last$CIs + last$CIr + last$CTs + last$CTr
            
            last$treat_prop_q <- params["treatment.q"] / (params["treatment.q"] + params["sigma.c"] + params["death.c"])
            last$treat_prop_p <- params["treatment.p"] / (params["treatment.p"] + params["sigma.c"] + params["death.c"])
            
            #Check get 1 at equilibrium when run with only sensitive strains
            #fraction of cattle available for infection by sensitive strain
            fC <- last$CS / (as.numeric(inits["CS"]+inits["CIs"]))
            #fraction of vectors available for infection by sensitive strain
            if (as.numeric(inits["VSt"] + inits["VSf"]) > 0){fV <- (last$VSt + last$VSf) / as.numeric(inits["VSt"] + inits["VSf"])} else {fV <- 0}
            
            #fraction of wildlife available for infection by sensitive strain
            if (as.numeric(inits["WS"]) > 0) {fW = last$WS / as.numeric(inits["WS"])} else {fW = 0}
            
            
            ## R calculations
            
            Nc = last$CS
            Np = last$PS 
            Nw = last$WS
            Nv = last$VSt + last$VSf
            sen <- "yes"
            Rsen <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen)
            
            sen <- "no"
            Np = last$PS + last$PF
            Rres <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen)
            
            
            #Rsen <- fC * R0sen[1] * fV * R0sen[3] + fW * R0sen[2] * fV * R0sen[4]  #Hurrah
            #Rres <- fC * R0res[1] * fV * R0res[3] + fW * R0res[2] * fV * R0res[4]
            
            selected_outputs <- data.frame(W_st = out[1, "WS"], R_eq_sen = Rsen[1], R0_sen = R0s[1], 
                                           R_eq_res = Rres[1], R0_res = R0r[1], 
                                           No_trt_cat = params["treatment.q"] * last$CIs * 365.25, 
                                           Incidence = params["gamma.c"] * last$CEs * 365.25, 
                                           Vector_no = as.numeric(inits["VSt"]), Prob_onward_tran = 1 - dpois(0, Rres[1]), 
                                           Risk = (1 - dpois(0, Rres[1])) * params["treatment.q"] * last$CIs * 365.25 , 
                                           prevalence = last$CIs / (as.numeric(inits["CS"]+inits["CIs"])), vector_birth = params["birth.v"], 
                                           vector_mortality = params["death.v"], fit.adj = params["fit.adj"], 
                                           prop.insecticide = prop.insecticide, birth.adj = birth.adj,
                                           eq_pop = params["equil.vector_pop"], K = params["K"], treat_prop = prop_treat)
            df = rbind(df, selected_outputs)
            
            wide <- cbind(selected_outputs, last)
            df2 = rbind(df2, wide)
            
            
            print(last$times)
            
          }
        }
      }
    }
  }
}  

toc()

Rsen
Rres

test <- df2
time <- format(Sys.time(), "%a %b %d %X %Y")
save(test, file = paste0("output/test_", time, ".Rda"))
save(test,file ="output/test.Rda")

quick_plot(out)

