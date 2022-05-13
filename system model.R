library(tidyverse)
library(lubridate)
library(reshape2)

rrhpct <- 70

setwd("C:/Users/aflicker/Strategies To End Homelessness, Inc/CommonFiles - CommonFiles/CONTINUUM OF CARE FILES/Data Analysis/System model/Current/vulnerable/inputs")

entry <- function(df){
  x <- df %>% 
      mutate(Rank = 1:n(),
             RunPE = cumsum(ProgramEligible),
             Entries = ifelse(Rank == 1,
                              ifelse(Openings > ProgramEligible, ProgramEligible, Openings),
                              ifelse(RunPE <= Openings, ProgramEligible,
                                     ifelse(lag(RunPE) < Openings, Openings-lag(RunPE), 0))))
  data.frame(x)
}

active <- read.csv("active.csv")
exitrates <- read.csv("exitrates.csv")
fth <- read.csv("fth.csv") %>%
  mutate(ProgramType = "Homeless")
inactive <- read.csv("inactive.csv")
returnrates <- read.csv("returnrates.csv")
cost <- read.csv("cost.csv") %>%
  mutate(ProgramType = as.character(ProgramType),
         ProgramType = ifelse(ProgramType == "3", "PSH", "RRH"))
cost <- cost %>%
  rbind(c("Prevention", cost$SpendingPerMonth[cost$ProgramType == "RRH"])) %>%
  mutate(SpendingPerMonth = as.numeric(SpendingPerMonth))
#correx <- read.csv("rrhcorrection.csv")

rrh <- 10200000*rrhpct/100
psh <- 10200000-rrh
months <- 1:60
homelesstotal <- c(sum(active$Active[active$ProgramType == "Homeless"]), rep(NA, length(months)))
hbtotal <- c(sum(active$Active[active$ProgramType == "Homeless" & active$HighBarrier == 1]), rep(NA, length(months)))
recidtotal <- rep(NA, length(months))

capacity <- active %>%
  filter(ProgramType != "Homeless") %>%
  group_by(ProgramType) %>%
  summarise(Active = sum(Active)) %>%
  left_join(cost) %>%
  mutate(Spending = Active*SpendingPerMonth*12,
         CoCSpending = ifelse(ProgramType == "RRH", 2600000,
                              ifelse(ProgramType == "PSH", 7600000, 0)),
         NonCoC = Spending-CoCSpending,
         ModelSpending = NonCoC + ifelse(ProgramType == "RRH", rrh,
                                         ifelse(ProgramType == "PSH", psh, 0)),
         Capacity = ModelSpending/(SpendingPerMonth*12),
         Capacity = ifelse(is.na(Capacity), Active, Capacity)) %>%
  ungroup() %>%
  select(ProgramType, Capacity)
# pshstart <- correx$PSH
# rrhstart <- correx$RRH
# baserrhexit <- exitrates$ExitRate[exitrates$ProgramType == "RRH" & exitrates$Disabled == 1]
# baserrhreturn <- returnrates$ReturnRate[returnrates$ProgramType == "RRH" & returnrates$Disabled == 1]

for (i in months){
  # extrarrh <- sum(active$Active[active$ProgramType == "RRH"])-rrhstart
  # missingpsh <- pshstart-sum(active$Active[active$ProgramType == "PSH"])
  # overage <- min(missingpsh, extrarrh)
  activeafterprogramexits <- left_join(active, exitrates) %>%
    mutate(Exits = ifelse(ProgramType == "Homeless", 0, Active*ExitRate),
           Active = Active-Exits) %>%
    select(-ExitRate)
  inactiveafterprogramexits <- activeafterprogramexits %>%
    full_join(inactive) %>%
    mutate(Inactive = ifelse(is.na(Inactive), 0, Inactive),
           Inactive = Inactive+Exits) %>%
    select(ProgramType, Disabled, HighBarrier, Inactive)
  activeafterprogramexits$Exits <- NULL
  extracapacity <- activeafterprogramexits %>%
    group_by(ProgramType) %>%
    summarise(Active = sum(Active)) %>%
    inner_join(capacity) %>%
    mutate(Openings = Capacity-Active,
           ForcedExits = ifelse(Openings < 0, abs(Openings), 0)) %>%
    select(ProgramType, Openings, ForcedExits)
  if(sum(extracapacity$Openings < 0) > 0){
    forcedexits <- extracapacity %>%
      filter(ForcedExits > 0) %>%
      select(-Openings) %>%
      right_join(activeafterprogramexits) %>%
      mutate(ForcedExits = ifelse(is.na(ForcedExits), 0, ForcedExits)) %>%
      arrange(ProgramType, Disabled, HighBarrier) %>%
      group_by(ProgramType) %>%
      mutate(RunActive = cumsum(Active),
             Rank = 1:n(),
             ForcedToExit = NA)
    for (l in 1:nrow(forcedexits)){
      forcedexits$ForcedToExit[l] = ifelse(forcedexits$Rank[l] == 1,
                                           ifelse(forcedexits$RunActive[l] <= forcedexits$ForcedExits[l], forcedexits$Active[l], 
                                                  forcedexits$ForcedExits[l]),
                                           ifelse(forcedexits$RunActive[l] <= forcedexits$ForcedExits[l], forcedexits$Active[l],
                                                  ifelse(forcedexits$RunActive[l-1] <= forcedexits$ForcedExits[l],
                                                         forcedexits$ForcedExits[l]-forcedexits$RunActive[l-1], 0)))
    }
    forcedexits2 <- forcedexits %>%
      group_by(Disabled, HighBarrier) %>%
      summarise(ForcedExits = sum(ForcedToExit)) %>%
      mutate(ProgramType = "Homeless")
    forcedtotal <- sum(forcedexits2$ForcedToExit)
    activeafterforcedexits <- forcedexits %>%
      select(-ForcedExits) %>%
      full_join(forcedexits2) %>%
      mutate(ForcedExits = ifelse(is.na(ForcedExits), 0, ForcedExits),
             Active = Active-ForcedToExit+ForcedExits) %>%
      select(ProgramType, Disabled, HighBarrier, Active)
    # inactiveafterforcedexits <- forcedexits %>%
    #   right_join(inactiveafterprogramexits) %>%
    #   mutate(Inactive = Inactive+ForcedExits) %>%
    #   select(ProgramType, Disabled, Inactive)
    }else{
      #inactiveafterforcedexits <- inactiveafterprogramexits
      activeafterforcedexits <- activeafterprogramexits
      forcedtotal <- 0
      }
  activeafterfth <- activeafterforcedexits %>%
    left_join(fth) %>%
    mutate(FTH = ifelse(is.na(FTH), 0, FTH),
           PreventionEligible = ifelse(is.na(PreventionEligible), 0, PreventionEligible))
  preventcapacity <- extracapacity$Openings[extracapacity$ProgramType == "Prevention"]
  if(preventcapacity > 0){
    preventactive <- activeafterfth %>%
      filter(FTH > 0) %>%
      mutate(Openings = preventcapacity,
             ProgramEligible = PreventionEligible) %>%
      arrange(HighBarrier, -Disabled) %>%
      entry()
    preventactive1 <- preventactive %>%
      mutate(Exits = 0,
             ProgramType = "Prevention") %>%
      select(ProgramType, Disabled, HighBarrier, Entries, Exits)
    preventactive2 <- preventactive %>%
      mutate(Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, Disabled, HighBarrier, Entries, Exits)
    preventactive <- rbind(preventactive1, preventactive2)
    activeafterprevent <- activeafterfth %>%
      left_join(preventactive) %>%
      mutate(Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Entries+Active+FTH-Exits) %>%
      select(-c(Exits, Entries)) %>%
      mutate(ProgramEligible = ifelse(Disabled == 1 & HighBarrier == 1 & ProgramType == "Homeless", Active, 0)) %>%
      select(ProgramType, Disabled, HighBarrier, Active, ProgramEligible)
    }else{
      activeafterprevent <- activeafterfth %>%
        mutate(ProgramEligible = ifelse(Disabled == 1 & HighBarrier == 1 & ProgramType == "Homeless", Active, 0)) %>%
        select(ProgramType, Disabled, HighBarrier, Active, ProgramEligible)
    }
  pshcapacity <- extracapacity$Openings[extracapacity$ProgramType == "PSH"]
  if(pshcapacity > 0){
    pshactive <- activeafterprevent %>%
      filter(ProgramEligible > 0,
             ProgramType == "Homeless") %>%
      mutate(Openings = pshcapacity) %>%
      entry()
    pshactive1 <- pshactive %>%
      mutate(ProgramType = "PSH") %>%
      group_by(ProgramType, Disabled, HighBarrier) %>%
      summarise(Entries = sum(Entries),
                Exits = 0)
    pshactive2 <- pshactive %>%
      mutate(Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, Disabled, HighBarrier, Entries, Exits)
    pshactive <- rbind(data.frame(pshactive1), pshactive2)
    activeafterpsh <- activeafterprevent %>%
      left_join(pshactive) %>%
      mutate(Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active+Entries-Exits,
             ProgramEligible = ifelse(ProgramType == "Homeless", Active, 0)) %>%
      select(-c(Entries, Exits))
    }else{
      activeafterpsh <- activeafterprevent %>%
        mutate(ProgramEligible = ifelse(ProgramType == "Homeless", Active, 0))
    }
  rrhcapacity <- extracapacity$Openings[extracapacity$ProgramType == "RRH"]
  if(rrhcapacity > 0){
    #rrhover <- (rrhcapacity+sum(activeafterpsh$Active[activeafterpsh$ProgramType == "RRH"])-rrhstart)/rrhstart
    rrhactive <- activeafterpsh %>%
      filter(ProgramEligible > 0) %>%
      mutate(Openings = rrhcapacity) %>%
      arrange(-Disabled, HighBarrier) %>%
      entry()
    rrhactive1 <- rrhactive %>%
      mutate(ProgramType = "RRH",
             Exits = 0) %>%
      select(ProgramType, Disabled, HighBarrier, Entries, Exits)
    rrhactive2 <- rrhactive %>%
      mutate(Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, Disabled, HighBarrier, Entries, Exits)
    rrhactive <- rbind(rrhactive1, rrhactive2)
    activeafterrrh <- activeafterpsh %>%
      left_join(rrhactive) %>%
      mutate(Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active-Exits+Entries) %>%
        select(ProgramType, Disabled, HighBarrier, Active)
    }else{
      activeafterrrh <- select(activeafterpsh, ProgramType:Active)
      #rrhover <- 0
    }
  thcapacity <- extracapacity$Openings[extracapacity$ProgramType == "TH"]
  if(thcapacity > 0){
    thactive <- data.frame(activeafterrrh) %>%
      filter(Active > 0,
             ProgramType == "Homeless") %>%
      mutate(Openings = thcapacity) %>%
      arrange(-Disabled, -HighBarrier)
    if(nrow(thactive) > 0){
      thactive <- thactive %>%
        mutate(ProgramEligible = Active) %>%
        entry()
      thactive1 <- thactive %>%
        mutate(ProgramType = "TH",
               Exits = 0) %>%
        select(ProgramType, Disabled, HighBarrier, Entries, Exits)
      thactive2 <- thactive %>%
        mutate(ProgramType = "Homeless",
               Exits = Entries,
               Entries = 0) %>%
        select(ProgramType, Disabled, HighBarrier, Entries, Exits)
      thactive <- rbind(thactive1, thactive2)
      activeafterth <- activeafterrrh %>%
        left_join(thactive) %>%
        mutate(Entries = ifelse(is.na(Entries), 0, Entries),
               Exits = ifelse(is.na(Exits), 0, Exits),
               Active = Active-Exits+Entries) %>%
        select(ProgramType, Disabled, HighBarrier, Active)
      }else{
        activeafterth <- activeafterrrh
      }
    }else{
      activeafterth <- activeafterrrh
    }
  sr <- activeafterth %>%
    filter(ProgramType == "Homeless",
           Active > 0) %>%
    left_join(exitrates) %>%
    mutate(Exits = Active*ExitRate) %>%
    select(-c(Active, ExitRate))
  if(nrow(sr) > 0){
    activeaftersr <- left_join(activeafterth, sr) %>%
      mutate(Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active-Exits) %>%
      select(-Exits)
    inactiveaftersr <- sr %>%
      right_join(inactiveafterprogramexits) %>%
      mutate(Exits = ifelse(is.na(Exits), 0, Exits),
             Inactive = Exits+Inactive) %>%
      select(-Exits)
    }else{
      activeaftersr <- activeafterth
      inactiveaftersr <- inactiveafterprogramexits
    }
  recid <- left_join(inactiveaftersr, returnrates) %>%
      mutate(Recid = Inactive*ReturnRate) %>%
      select(ProgramType, Disabled, HighBarrier, Recid, Inactive)
  recidtotal[i] <- sum(recid$Recid)+forcedtotal
  inactive <- recid %>%
    mutate(Inactive = Inactive-Recid,
           Inactive = Inactive*23/24) %>%
    select(ProgramType, Disabled, HighBarrier, Inactive)
  active <- recid %>%
    group_by(Disabled, HighBarrier) %>%
    summarise(Recid = sum(Recid)) %>%
    mutate(ProgramType = "Homeless") %>%
    right_join(activeaftersr) %>%
    mutate(Recid = ifelse(is.na(Recid), 0, Recid),
           Active = Active+Recid) %>%
    select(-Recid)
  homelesstotal[i+1] <- sum(active$Active[active$ProgramType == "Homeless"])
  hbtotal[i+1] <- sum(active$Active[active$ProgramType == "Homeless" & active$HighBarrier == 1])
}
plot(homelesstotal)
plot(hbtotal)
plot(recidtotal)
