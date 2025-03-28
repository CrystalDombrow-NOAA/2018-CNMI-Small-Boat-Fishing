#######################
# 2018 CNMI small boat fishery cost-earnings survey
# FISHING VENDORS section
# Calculations for section paragraphs
#######################

#-------------------------------------
# SET UP WORKSPACE
#-------------------------------------

#Load libraries
library(tidyverse)
library(dplyr)
library(plotrix)


#Run functions used in the analyses below
source("Functions/AnnualExpendituresFunction.R")
source("Functions/DataSummariesFunction.R")
source("Functions/DistributionFunction.R")
source("Functions/TripCostsFunction.R")
source("Functions/DistributionFunction_MultipleVariables_CatchDisposition.R")
source("Functions/DistributionFunction_MultipleVariables_WhereSell.R")


#--------------
# CALCULATIONS
#--------------

# General crew stats
vendor.stats <- cnmi.data.cleaned %>% 
  select(Survey, boat.owner, highliner, vendor, primary.target, Q1.mid.ifelse,
         Q8A.mid.ifelse:Q8C.mid.ifelse, Q9.mid, Q11A.yesno:Q11G.yesno, Q12A, 
         Q17.dummy.total, Q35A:Q35E, Q38A:Q38E, Q42:Q53, Q22.mid) %>% 
  #filter(boat.owner == "not boat owner") %/%
  filter(vendor == "vendor") %>%
  mutate(Q8A.mean = round(mean(Q8A.mid.ifelse, na.rm = T), 1),
         Q8B.mean = round(mean(Q8B.mid.ifelse, na.rm = T), 1),
         Q8C.mean = round(mean(Q8C.mid.ifelse, na.rm = T), 1))


#---------------------------------------------------------------------------

# Q8. Lbs of fish caught



#---------------------------------------------------------------------------

# Q14. In the past 12 months, how were the catch distributed among fisher(men) 
# in a fishing trip?

q14.vendor.data <- cnmi.data.cleaned %>% 
  select(vendor, Q14A:Q14F, Q14.multiple.responses) %>%
  filter(vendor == "vendor")

q14.vendor.percents <- cnmi.data.cleaned %>% 
  select(vendor, Q14A:Q14F, Q14.multiple.responses) %>%
  filter(vendor == "vendor") %>%
  mutate(q14a.per = round(sum(Q14A) / nrow(q14.vendor.data) * 100, 1),
         q14b.mean = round(mean(Q14B, na.rm = T), 1),
         q14c.mean = round(mean(Q14C, na.rm = T), 1),
         q14d.per = round(sum(Q14D) / nrow(q14.vendor.data) * 100, 1),
         q14e.per = round(sum(Q14E) / nrow(q14.vendor.data) * 100, 1),
         q14.mult.per = round(sum(Q14.multiple.responses) / 
                                nrow(q14.vendor.data) * 100, 1),
         kept.fish = ifelse(Q14A == 1 | !is.na(Q14B), 1, 0),
         kept.fish.per = round(sum(kept.fish) / nrow(q14.vendor.data) * 100, 1))


#---------------------------------------------------------------------------

# Q16. Catch distribution
q16.vendor <- distribution.function.catch.disposition(cnmi.data.cleaned, 
                                                           breakdown = "vendor")


#---------------------------------------------------------------------------

# Q17. Where did you sell your catch?
q17.vendor <- distribution.function.where.sell(cnmi.data.cleaned, 
                                                    breakdown = "vendor")


#---------------------------------------------------------------------------

#Q22. Income from fishing

vendor.income.fishing <- vendor.stats %>% 
  select(Q22.mid) %>% 
  mutate(across(everything(), ~if_else(. == 0, NA, .))) %>% 
  mutate(mean = mean(Q22.mid, na.rm = T)) 


#---------------------------------------------------------------------------

# Q36.39 Trip costs

q36.39.pelagic.vendor <- 
  trip.costs.function(cnmi.data.cleaned, gear.type.trip = "pelagic", 
                      breakdown = "vendor")

q36.39.bottomfish.vendor <- 
  trip.costs.function(cnmi.data.cleaned, gear.type.trip = "bottomfish", 
                      breakdown = "vendor")

q36.39.nearshore.vendor <- 
  trip.costs.function(cnmi.data.cleaned, gear.type.trip = "nearshore", 
                      breakdown = "vendor")

q36.39.mixed.vendor <- 
  trip.costs.function(cnmi.data.cleaned, gear.type.trip = "mixed", 
                      breakdown = "vendor")


#---------------------------------------------------------------------------

# Q40. Annual expenditures

q40.vendor <- annual.expenditures.function(cnmi.data.cleaned, 
                                           breakdown = "vendor")

