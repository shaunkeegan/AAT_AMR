#Edits to code

# AT_AMR_dens_dep.R  
# - missing minus at start of dVSf.dt line
# - additions in multiple places in the vector equations to allow for all treated hosts to be infectious (to agree with the R0 expression)
# - moved waning term from CTs equation to CIs as we want the PPs to wane to infected (CIS) not treated (CTs)
# - edited the PF and CS equations to split the prophylatcically treated and non-prophylactically treated at birth,
# consequently, new.prop not needed anymore, but prop.prophylactic is needed
# 
# r0.R
# - created new function r0_sen_calc_LM2 with updates to your version
# - note that after much messing around it seemed simpler to have a function for R0 that you feed in the current susceptible population
#This then calculates the initial value of R0 as well as the equilibrium value all in function. 
# This avoids the messing around with fractions which gets confusing when the prophylactic populaton changes size.
# - added 1/death.v (the length of time a vector spends infected) to the "to host from vector" R elements
# - replaced sigma.c with sigma.st on third line of RVC expression
# - added scaling * 1/(1-p1c *p2c) to third line of the RVC expression
# - correction to denominator in p2c
# - new expression for RVP (I hadn't thought about properly and it doesn't quite take same form as RVC). I can walk you though it.
# 
# set1_params_inits.R
# - sigma and death changed to sigma.c and death.c in defintions of treatment.q and treatment.p
# - sigma.st set to sigma.c * 10, as having them the same makes it harder to check the R0 expressions
# - calculated values of CS, PF and PS (at equilibrium in absence of infection) based on proportion of animals treated 
# prophylactically when born into herd 
# - added prop.prophylaxis and death.p to params list
#
# qual_check.R
# - added a alternative totals function that does the same thing but uses dplyr
