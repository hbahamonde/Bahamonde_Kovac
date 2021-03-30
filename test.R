library(magrittr)


# W -----------------------------------------------------------------------



W_list = list() 

years = 200 ## years matrices with countries x countries
countries = 120

possible_means_0 = 1:years
possible_means_1  = cumsum(rnorm(years,1,1))

for(i in 1:years){
  
  means = possible_means_1
  
  W_i = matrix(rnorm(countries*countries,mean = means[i],sd=1),countries,countries)
  diag(W_i) = 0
  W_list[[i]] = W_i
  
}



# Delta -------------------------------------------------------------------

## ctrl + shift + R


D_list = list() 

# matrices with countries x countries

possible_means = rep(1,years)
deduction = 0.2

for(i in 1:years){

  D_list[[i]] =  matrix(runif(countries*countries,possible_means[i]-deduction,possible_means[i]+deduction),countries,countries)
}


# D*w ---------------------------------------------------------------------

dw = lapply(1:years,function(x){
  D_list[[x]]*W_list[[x]]
})

# str(dw)
# View(dw[[1]])


####

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(tsDyn)

# Simulation presented in Enders2014 p. 289
parameter.1 = 0.7 # 0.7 (x)
parameter.2 = 0.2 # 0.2 (y)
parameters.m <- matrix(c(parameter.1, parameter.2, parameter.2, parameter.1), 2)
years.sim = 200

# var sims
set.seed(1);country.1 <- VAR.sim(B=parameters.m,n=years.sim,include="none")
set.seed(2);country.2 <- VAR.sim(B=parameters.m,n=years.sim,include="none")


alpha = 0
psi_1 = 0.5
psi_2 = 1
lambda_1 = 3
lambda_2 = 1.5
lambda_3 = 0
lambda_4 = 1
e = rnorm(countries, 0, sd = 1)


p_load(rgr)

x = matrix(
  alpha + 
    as.vector(psi_1*lag(country.1[,2],1)) + 
    as.vector(psi_2*lag(country.1[,1],1)) + 
    lambda_1*dw[[2]] + 
    lambda_2*dw[[1]] +
    lambda_3*dw[[2]] +
    lambda_4*dw[[1]] +
    e
  )

p_load(GVARX)

p.1.sim=1 # The number of lag for Xt matrix
FLag.1.sim=2 # The number of lag for foreign variables in country-specific VAR
lag.max.sim=1 # The maximal number of lag for estimating country-specific VAR
type.sim="none" # Model specification for VAR. As in package vars, we have four selection: "none","const","trend", "both".
ic.sim="AIC" # Information criteria for optimal lag.As in package vars, we have four selection: "AIC", "HQ", "SC", and "FPE".

# determinsitic component for the paper
type.sim.paper = as.character(ifelse(type.sim=="both", as.character("Trend and constant"), ifelse(type.sim == "trend", as.character("Trend"), ifelse(type.sim == "constant", "Constant", NA ))))


options(scipen=9999999)
mainOUTPUT.sim = GVECMest(
  data = country.var.d,
  p = p.1.sim,
  FLag = FLag.1.sim,
  lag.max = lag.max.sim,
  type = type.sim,
  ic = ic.sim,
  weight.matrix=sim.w.matrix)












