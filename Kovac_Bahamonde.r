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

# Rename "West Germany" to "Germany" only for 1946-1989 period.
# cow.d.1$ID[cow.d.1$ID == "German Federal Republic"] <- "Germany"

# Partion the DF into two smaller DF's:  (1) 1871-1913, and (2) 1946-today
cow.d.1871.1913 = cow.d.1[cow.d.1$Time >= 1871 & cow.d.1$Time <= 1913,]
cow.d.1946.today = cow.d.1[cow.d.1$Time >= 1955 & cow.d.1$Time <= max(cow.d.1$Time),]
# trim 1945-1955 years for all: we want to have one Germany across time (and West Germany pops un in the database only since 1955). That's why

## countries that entered into "cow.d.1871.1913" and "cow.d.1946.today"
### unique(cow.d.1871.1913$ID)
### unique(cow.d.1946.today$ID)


# append both DF's
cow.d.time.trimmed <- rbind(cow.d.1871.1913,cow.d.1946.today)

# we want to have one Germany across time (and West Germany pops un in the database only since 1955). 
cow.d.time.trimmed$ID[cow.d.time.trimmed$ID == "German Federal Republic" & cow.d.time.trimmed$Time >= 1955 & cow.d.time.trimmed$Time <=1989] <- "Germany"

# drop if ID == "German Federal Republic" | ID == "German Federal Republic" // OTHERWISE, we would have duplicates (two Germanies in 1955 onwards).
cow.d.time.trimmed <- cow.d.time.trimmed[!(cow.d.time.trimmed$ID == "German Federal Republic" | cow.d.time.trimmed$ID == "German Democratic Republic"),]



# check if panels are "strictly balanced?
p_load(plm)
is.pbalanced(cow.d.time.trimmed) # False


dat = cow.d.time.trimmed
dat$Time = as.Date(as.character(dat$Time), "%Y")


# pre dataset
# dat.pre = dat#[dat$Time<=2019,]


# balancing pre
dat.test <- pdata.frame(dat, index=c("ID","Time"))
dat.test = make.pbalanced(dat.test, balance.type = "shared.individuals", index = c('ID', 'Time') ) # shared.individuals / fill
is.pbalanced(dat.test)
# N
unique(dat.test$ID)



##############################
# weight matrix
##############################

# loads pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

p_load(foreign)
bilateral.d <- read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/Dyadic_COW_4.0.csv") 

# keeping columns i'll need
bilateral.d <- bilateral.d[c("ccode1", "ccode2",  "year", "importer1", "importer2", "flow1", "flow2")]

write.table(bilateral.d, "/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/dat.txt", sep="\t")

# p_load(openxlsx) # use "openxlsx" not "xlsx" * function is the same (i.e. "write.xlsx"). Had also to detach "dplyr."
# write.xlsx(bilateral.d, "/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/dat.xlsx")


# keep years that i'll need
bilateral.d = subset(bilateral.d, year >= 1871 & year <= 1913 | year >= 1955 & year <= 2012)

# keeping countries that I'll need
available.countries = as.character(unique(dat.test$ID))
bilateral.d2 = subset(bilateral.d, importer1 %in% c(available.countries) & importer2 %in% c(available.countries))

# Replacing "-9.000" for "NA"
## https://correlatesofwar.org/data-hosting
# bilateral.d2$flow1[bilateral.d2$flow1 == -9.000] <- NA
# bilateral.d2$flow2[bilateral.d2$flow2 == -9.000] <- NA
#
# Generating another column with the absolute market trade distance between the two countries
options(scipen=1000000) 
bilateral.d2$trade = as.numeric(round(abs(bilateral.d2$flow1-bilateral.d2$flow2), 4))











# HERE (TEST AREA)
data("PriceVol")
data("tradeweight1")
data("tradeweightx")

p=2
FLag=2
type="const"
lag.max=15
ic="SC"
weight.matrix=tradeweightx
# HERE (TEST AREA)


# 
list("Text", 1, 2334)




bilateral.d2[bilateral.d2$year==1871,]






# 


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
