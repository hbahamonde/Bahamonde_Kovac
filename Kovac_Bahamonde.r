cat("\014")
rm(list=ls())
graphics.off()


# loads pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 


########################################################
# Merge Trade-COW y National Material Capabilities-COW
########################################################

# load National Material Capabilities-COW
p_load(foreign)
mnc.d <- read.dta("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/NMC_5_0.dta") 


# load Trade-COW
p_load(foreign)
trade.d <- read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/National_COW_4.0.csv") 

# merge
cow.d = merge(mnc.d, trade.d, by=c("ccode","year"))

# subset variables
cow.d <- cow.d[c("ccode", "year", "milex", "milper", "irst", "pec")]

# Format Country Name
country = data.frame(
        statename = trade.d$statename,
        ccode = trade.d$ccode,
        year = trade.d$year
)
cow.d = merge(cow.d, country, by = c("ccode", "year"))
cow.d$statename <- as.character(cow.d$statename)

########################################################
# GVARX
########################################################
# install.packages("GVARX")
library(GVARX)
# https://www.rdocumentation.org/packages/GVARX/versions/1.1/topics/grangerGVAR
# data("tradeweightx")


# rename time-ID and panel-ID vars.
colnames(cow.d)[which(names(cow.d) == "statename")] <- "ID"
colnames(cow.d)[which(names(cow.d) == "year")] <- "Time"


# (1)
# Be adviced, this function only computes Granger causality tests for 
# BIVARIATE specifications. Will compute different models for both var types.

# (2)
# Weight matrix will be the COW Trade DF.

# reorder time-ID and panel-ID vars.
cow.d.1 = subset(cow.d,  select=c("ID", "Time","milper", "irst")) # complete var names: "statename", "year", "milex", "milper", "irst", "pec"

# check if panels are "strictly balanced?
p_load(plm)
is.pbalanced(cow.d.1) # False


dat = cow.d.1
dat$Time = as.Date(as.character(dat$Time), "%Y")


# pre dataset
# dat.pre = dat#[dat$Time<=2019,]


# balancing pre
dat.test <- pdata.frame(dat, index=c("ID","Time"))
dat.test = make.pbalanced(dat.test, balance.type = "shared.individuals", index = c('ID', 'Time') ) # shared.individuals / fill
is.pbalanced(dat.test)
# N
unique(dat.test$ID)


# weight matrix
p_load(foreign)
bilateral.d <- read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/Dyadic_COW_4.0.csv") 

# keeping columns i'll need
bilateral.d <- bilateral.d[c("ccode1", "ccode2",  "year", "importer1", "importer2", "flow1", "flow2")]

# keeping obs that I'll need
available.countries = c(unique(dat.test$ID))

bilateral.d2 = subset(bilateral.d, ccode1 %in% c(available.countries) | ccode2 %in% c(available.countries))

data("PriceVol")
data("tradeweight1")
data("tradeweightx")

#Generate country-specific foreign variables
Ft=GVAR_Ft(data=PriceVol,weight.matrix=tradeweight1)
k=17
head(Ft[[k]])
tail(Ft[[k]])


# post dataset
#dat.post = dat[dat$Time>=1945,]
# balancing post
# dat.post <- pdata.frame(dat.post, index=c("ID","Time"))
# dat.post = make.pbalanced(dat.post, balance.type = "shared.individuals", index = c('ID', 'Time') ) # shared.individuals / fill
# is.pbalanced(dat.post)




# balance dataframe
# is.pbalanced(pdata.frame(cow.d.1, index = c("ID", "Time")))


ID = as.vector(as.character(dat.test$ID))
Time = as.character(as.Date(dat.test$Time, "%Y"))
milper = as.vector(as.numeric(dat.test$milper))
irst = as.vector(as.numeric(dat.test$irst))


dat.test = data.frame(
        ID = as.vector(as.character(dat.test$ID)),
        Time = as.character(as.Date(dat.test$Time, "%Y")),
        milper = as.vector(as.numeric(dat.test$milper)),
        irst = as.vector(as.numeric(dat.test$irst))
        )


dat.test$Time = as.character(as.Date(dat.test$Time, "%Y"))
dat.test$ID = as.character(dat.test$ID, as.character)



# Parameters
p = 2 # Domestic lags.
FLag = 2 # Foreign lags.
lag.max = 5 # Max number of lags for estimating country-specific VAR
type = "both" # Model specificaiton for VAR. As in package vars, we have four selection: "none","const","trend", "both".
ic = "AIC" # Information criteria for optimal lag.As in package vars, we have four selection: "AIC", "HQ", "SC", and "FPE".




GC_OUTPUT = grangerGVAR(data=dat.test, p, FLag, type, lag.max, ic)

###
data("PriceVol")
data("tradeweight1")
data("tradeweightx")

p=2
FLag=2
type="const"
lag.max=15
ic="SC"
weight.matrix=tradeweightx

GC_OUTPUT = grangerGVAR(data=PriceVol, p, FLag, type, lag.max, ic, weight.matrix)




########################################################
# GVAR
########################################################
# formatting df as "timeseries data as list (each entry is a matrix of a subsystem of variables)"
cow.d <- split(cow.d, cow.d$ccode)


# Transform DF into TS object
cow = as.ts(cow, 
              start = min(cow$year),
              end = max(cow$year),
              frequency = 1 # 1 year
              )

names(cow) <- unique(country$statename)
c.names <- names(cow)[-length(cow)]


install.packages("GV")

# install.packages("GVAR", repos="http://R-Forge.R-project.org")
library(GVAR)


data(pesaran26)
c.names <- names(Data)[-length(Data)]

p <- c(2,2,2,1,2,2,1,2,2,2,2,1,2,1,1,2,2,2,2,2,2,1,2,2,2,2)
q <- c(2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
lex <- q

endo <- ord <- we <- d <- vector("list",length=length(c.names))
names(endo) <- names(ord) <- names(we) <- names(d) <- c.names
# base country: usa
endo[[1]] <- c(1:3,5:7)
ord[[1]] <- c(1:3,5:7)
we[[1]] <- c(1:2,4)
d[[1]] <- NULL
# countries with 6 endogenous variables:
for (j in c("EuroArea", "Japan", "UK", "Sweden", "Switzerland", "Norway", "Australia", "Canada", "NewZealand", "Korea", "Safrica")) {i <- which(c.names==j); endo[[i]] <- ord[[i]] <- 1:6}
# countries with 5 endogenous variables:
for (j in c("Argentina", "Chile", "Malaysia", "Philippines", "Singapore", "Thailand", "India")) {i <- which(c.names==j); endo[[i]] <- ord[[i]] <- 1:5}
# countries with 4 endogenous variables:
for (j in c("China", "Brazil", "Mexico", "Peru", "Indonesia", "Turkey")) {i <- which(c.names==j); endo[[i]] <- ord[[i]] <- c(1:2,4:5)}
# Saudi Arabia
endo[[21]] <- ord[[21]] <- c(1:2,4)

# all countries but us
for (i in 2:length(we))
{
        we[[i]] <- c(1:3,5,6)
        d[[i]] <- 1
}

Case <- "IV"
r <- c(2,1,1,4,3,3,3,2,2,1,2,3,3,4,4,3,3,4,1,2,3,3,2,1,1,1)

res.GVAR <- GVAR(Data=Data,r=r,p=p,q=q,weight=weight,Case=Case,exo.var=TRUE,d=d,lex=lex,ord=ord,we=we,endo=endo,method="max.eigen")

# view vecm models
res.GVAR$we.vecms
