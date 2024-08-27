#----------------
#FUNCTION FOR DISTRIBUTION CALCULATIONS
  #2018 Guam and CNMI Small Boat Fisheries Cost-Earnings Survey
#----------------


#Load libraries
library(dplyr)
library(tidyverse)



#Write function
distribution.function <- function(cnmi.data.cleaned, q.number, categories){
  
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
    
  
  
  #Create data frame to later rbind into a table
  q.full.sample.dist <- q.full.sample.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(full.sample = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1, 2)) %>%
    mutate(n = nrow(n.full.sample), .before = V1)
  
  
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
  
  
  
  #Create data frame to later rbind into a table 
  q.saipan.dist <- q.saipan.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(saipan = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.saipan), .before = V1)
  
  q.tinian.dist <- q.tinian.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(tinian = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.tinian), .before = V1)
  
  q.rota.dist <- q.rota.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(rota = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.rota), .before = V1)
  
  
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
  
  
  
  #Create data frame to later rbind into a table 
  q.sell.fish.dist <- q.sell.fish.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(sold.fish = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.sell.fish), .before = V1)
  
  q.did.not.sell.fish.dist <- q.did.not.sell.fish.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(did.not.sell.fish = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.did.not.sell.fish), .before = V1)
  
  
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
  
  
  
  #Create data frame to later rbind into a table 
  q.highliner.dist <- q.highliner.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(highliner. = ifelse(is.na(percent), 0, percent)) %>%  #extra . in name to preserve breakdown for rbind table
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.highliner), .before = V1)
  
  q.not.highliner.dist <- q.not.highliner.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(not.highliner = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.not.highliner), .before = V1)
  
  
  
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
  
  
  
  #Create data frame to later rbind into a table 
  q.pelagic.dist <- q.pelagic.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(pelagic = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.pelagic), .before = V1)
  
  q.bottomfish.dist <- q.bottomfish.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(bottomfish = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.bottomfish), .before = V1)
  
  q.reef.dist <- q.reef.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(reef = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n =  nrow(n.reef), .before = V1)
  
  q.no.primary.dist <- q.no.primary.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(no.primary = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.no.primary), .before = V1)
  
  
  
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

  
  
  #Create data frame to later rbind into a table 
  q.boat.owner.dist <- q.boat.owner.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(boat.owner. = ifelse(is.na(percent), 0, percent)) %>% #extra . in name to preserve breakdown for rbind table
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n =  nrow(n.boat.owner), .before = V1)
  
  q.not.boat.owner.dist <- q.not.boat.owner.dist %>%
    unique() %>%
    right_join(data.frame(q.num = categories)) %>%
    mutate(not.boat.owner = ifelse(is.na(percent), 0, percent)) %>%
    arrange(q.num) %>%
    t() %>%
    as.data.frame() %>%
    slice(-c(1:3)) %>%
    mutate(n = nrow(n.not.boat.owner), .before = V1)
  
  
  
  #-----------------------------------------------------
  #-----------------------------------------------------
  #Rbind together, for glory
  q.distribution.table <- rbind(q.full.sample.dist, 
                                q.saipan.dist,
                                q.tinian.dist,
                                q.rota.dist,
                                q.sell.fish.dist,
                                q.highliner.dist,
                                q.not.highliner.dist,
                                q.did.not.sell.fish.dist,
                                q.pelagic.dist,
                                q.bottomfish.dist, 
                                q.reef.dist, 
                                q.no.primary.dist, 
                                q.boat.owner.dist,
                                q.not.boat.owner.dist) 
  
  
  #Save final table 
  write.csv(q.distribution.table, na = "0.0", 
            paste("Tables/", q.number, sep = "", "_distribution.csv"))
  
  
  
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

