#----------------
#FUNCTION FOR DISTRIBUTION CALCULATIONS
#2018 Guam and CNMI Small Boat Fisheries Cost-Earnings Survey
#----------------


#Load libraries
library(dplyr)
library(tidyverse)



#Write function
distribution.no.table.function <- function(cnmi.data.cleaned, q.number){
  
  #Rename argument for group_by to work below
  cnmi.data.cleaned <- cnmi.data.cleaned %>% 
    rename(q.num = q.number)
  
  
  #-----------------------------------------------------
  #-----------------------------------------------------
  
  #FULL SAMPLE
  
  #Create object to calculate percentages below
  n.full.sample <- cnmi.data.cleaned %>%
    select(any_of(c("q.num"))) %>%
    drop_na()
  
  #Calculate full sample distribution
  q.full.sample.dist <-  cnmi.data.cleaned %>%
    select(any_of(c("q.num"))) %>%
    drop_na() %>%
    group_by(q.num) %>%
    mutate(percent = round(100 * n() / nrow(n.full.sample), 1)) %>%
    arrange(q.num)
  
  
  #-----------------------------------------------------
  #ISLAND
  
  #Create objects to calculate n rows
  n.saipan <- cnmi.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(Island == "Saipan") 
  
  n.tinian <- cnmi.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(Island == "Tinian") 
  
  n.rota <- cnmi.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(Island == "Rota") 
  
  
  
  #Calculate distribution
  q.saipan.dist <- cnmi.data.cleaned %>%
    select(any_of(c("Island", "q.num"))) %>%
    filter(Island == "Saipan") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.saipan), 1)) %>%
    arrange(q.num)
  
  q.tinian.dist <- cnmi.data.cleaned %>%
    select(any_of(c("Island", "q.num"))) %>%
    filter(Island == "Tinian") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.tinian), 1)) %>%
    arrange(q.num)
  
  q.rota.dist <- cnmi.data.cleaned %>%
    select(any_of(c("Island", "q.num"))) %>%
    filter(Island == "Rota") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.rota), 1)) %>%
    arrange(q.num)

  
  #-----------------------------------------------------
  #SELL FISH
  
  #Create objects to calculate n rows
  n.sell.fish <- cnmi.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(sell.fish.chr == "sold fish") 
  
  n.did.not.sell.fish <- cnmi.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(sell.fish.chr == "did not sell fish") 
  
  
  #Calculate distribution
  q.sell.fish.dist <- cnmi.data.cleaned %>%
    select(any_of(c("sell.fish.chr", "q.num"))) %>%
    filter(sell.fish.chr == "sold fish") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.sell.fish), 1)) %>%
    arrange(q.num)
  
  q.did.not.sell.fish.dist <- cnmi.data.cleaned %>%
    select(any_of(c("sell.fish.chr", "q.num"))) %>%
    filter(sell.fish.chr == "did not sell fish") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.did.not.sell.fish), 1)) %>%
    arrange(q.num)

  
  
  #-----------------------------------------------------
  #HIGHLINER
  
  #Create objects to calculate n rows
  n.highliner <- cnmi.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(highliner == "highliner") 
  
  n.not.highliner <- cnmi.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(highliner == "not highliner") 
  
  
  #Calculate distribution
  q.highliner.dist <- cnmi.data.cleaned %>%
    select(any_of(c("highliner", "q.num"))) %>%
    filter(highliner == "highliner") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.highliner), 1)) %>%
    arrange(q.num)
  
  q.not.highliner.dist <- cnmi.data.cleaned %>%
    select(any_of(c("highliner", "q.num"))) %>%
    filter(highliner == "not highliner") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.not.highliner), 1)) %>%
    arrange(q.num)

  

  #-----------------------------------------------------
  #PRIMARY TARGET
  
  #Create objects to calculate n rows
  n.pelagic <- cnmi.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(primary.target == "pelagic") 
  
  n.bottomfish <- cnmi.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(primary.target == "bottomfish") 
  
  n.reef <- cnmi.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(primary.target == "reef") 
  
  n.no.primary <- cnmi.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(primary.target == "no primary") 
  
  
  
  #Calculate distribution
  q.pelagic.dist <- cnmi.data.cleaned %>%
    select(any_of(c("primary.target", "q.num"))) %>%
    filter(primary.target == "pelagic") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.pelagic), 1)) %>%
    arrange(q.num)
  
  q.bottomfish.dist <- cnmi.data.cleaned %>%
    select(any_of(c("primary.target", "q.num"))) %>%
    filter(primary.target == "bottomfish") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.bottomfish), 1)) %>%
    arrange(q.num)
  
  q.reef.dist <- cnmi.data.cleaned %>%
    select(any_of(c("primary.target", "q.num"))) %>%
    filter(primary.target == "reef") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.reef), 1)) %>%
    arrange(q.num)
  
  q.no.primary.dist <- cnmi.data.cleaned %>%
    select(any_of(c("primary.target", "q.num"))) %>%
    filter(primary.target == "no primary") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.no.primary), 1)) %>%
    arrange(q.num)
  
  
  
  #-----------------------------------------------------
  #BOAT OWNERSHIP
  
  #Create objects to calculate n rows
  n.boat.owner <- cnmi.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(boat.owner == "boat owner") 
  
  n.not.boat.owner <- cnmi.data.cleaned %>%
    filter(!is.na(q.num)) %>% 
    filter(boat.owner == "not boat owner") 
  
  
  #Calculate distribution
  q.boat.owner.dist <- cnmi.data.cleaned %>%
    select(any_of(c("boat.owner", "q.num"))) %>%
    filter(boat.owner == "boat owner") %>% 
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.boat.owner), 1)) %>%
    arrange(q.num)
  
  q.not.boat.owner.dist <- cnmi.data.cleaned %>%
    select(any_of(c("boat.owner", "q.num"))) %>%
    filter(boat.owner == "not boat owner") %>%
    group_by(q.num) %>%
    drop_na() %>%
    mutate(percent = round(100 * n() / nrow(n.not.boat.owner), 1)) %>%
    arrange(q.num)
  
  
  
  
  #-----------------------------------------------------
  #-----------------------------------------------------
  
  #Return final list of above objects
  q.dist.table.list <- list("q.full.sample.dist" = q.full.sample.dist,
                            "q.saipan.dist" = q.saipan.dist,
                            "q.tinian.dist" = q.tinian.dist,
                            "q.rota.dist" = q.rota.dist,
                            "q.sell.fish.dist" = q.sell.fish.dist,
                            "q.did.not.sell.fish.dist" = q.did.not.sell.fish.dist,
                            "q.highliner.dist" = q.highliner.dist,
                            "q.not.highliner.dist" = q.not.highliner.dist,
                            "q.pelagic.dist" = q.pelagic.dist,
                            "q.bottomfish.dist" = q.bottomfish.dist,
                            "q.reef.dist" = q.reef.dist,
                            "q.no.primary.dist" = q.no.primary.dist,
                            "q.boat.owner.dist" = q.boat.owner.dist,
                            "q.not.boat.owner.dist" = q.not.boat.owner.dist)
  
  return(q.dist.table.list)
  
}

