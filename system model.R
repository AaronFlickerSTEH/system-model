adultprevention <- 13
adultpsh <- 949
adultrrh <- 197
adultth <- 36
veteranprevention <- 7
veteranpsh <- 268
veteranrrh <- 35
veteranth <- 101
youthprevention <- 2
youthpsh <- 86
youthrrh <- 34
youthth <- 11

library(tidyverse)
library(lubridate)
library(reshape2)
source("U:/theme.R")
setwd("U:/system model/No winter/survival/inputs")
active <- read.csv("active.csv")[,-1] %>%
  mutate(ProgramPop = ClientPop)
exitrates <- read.csv("exitrates.csv")[,-1]
fth <- read.csv("fth.csv")[,-1] %>%
  mutate(Months = 0,
         ProgramType = "Homeless")
inactive <- read.csv("inactive.csv")[,-1]
returnrates <- read.csv("returnrates.csv")[,-1]
base <- read.csv("base.csv")[,-1] %>%
  mutate(prediction = "baseline")

ProgramType <- c("Prevention", "PSH", "RRH", "TH")
Adult <- c(adultprevention, adultpsh, adultrrh, adultth)
Veteran <- c(veteranprevention, veteranpsh, veteranrrh, veteranth)
Youth <- c(youthprevention, youthpsh, youthrrh, youthth)
capacity <- data.frame(ProgramType, Adult, Veteran, Youth) %>%
  melt(id.vars = "ProgramType") %>%
  mutate(ProgramPop = variable,
         Capacity = value) %>%
  select(ProgramType, ProgramPop, Capacity)
months <- 1:60

homelesstotal <- rep(NA, length(months)+1)
homelessyouth <- rep(NA, length(months)+1)
homelessvets <- rep(NA, length(months)+1)
homeless <- active %>%
  filter(ProgramType == "Homeless") %>%
  mutate(Exits = 0)
homelessnums <- homeless %>%
  summarise(Total = sum(Active),
            Youth = sum(Active[ClientPop == "Youth"]),
            Veterans = sum(Active[ClientPop == "Veteran"]))
homelesstotal[1] <- sum(homelessnums$Total)
homelessyouth[1] <- sum(homelessnums$Youth)
homelessvets[1] <- sum(homelessnums$Veterans)

specialentry <- function(df){
  if("PreventionEligible" %in% colnames(df)){
    x <- df %>% group_by(ProgramPop) %>%
    mutate(Rank = 1:n(),
           RunPE = cumsum(PreventionEligible),
           Entries = ifelse(Rank == 1,
                            ifelse(Openings > PreventionEligible, PreventionEligible, Openings),
                            ifelse(RunPE <= Openings, PreventionEligible,
                                   ifelse(lag(RunPE) < Openings, Openings-lag(RunPE), 0))))
  }
  else{
    x <- df %>%
      group_by(ProgramPop) %>%
      mutate(Rank = 1:n(),
             RunActive = cumsum(Active),
             Entries = ifelse(Rank == 1,
                              ifelse(Openings > Active, Active, Openings),
                              ifelse(RunActive <= Openings, Active,
                                     ifelse(lag(RunActive) < Openings, Openings-lag(RunActive), 0))))
  }
  data.frame(x)
}

entry <- function(df){
  if("PreventionEligible" %in% colnames(df)){
    x <- df %>% group_by(ProgramPop) %>%
      mutate(Rank = 1:n(),
             RunPE = cumsum(PreventionEligible),
             Entries = ifelse(Rank == 1,
                              ifelse(Openings > PreventionEligible, PreventionEligible, Openings),
                              ifelse(RunPE <= Openings, PreventionEligible,
                                     ifelse(lag(RunPE) < Openings, Openings-lag(RunPE), 0))))
  }
  else{
    x <- df %>%
      mutate(Rank = 1:n(),
             RunActive = cumsum(Active),
             Entries = ifelse(Rank == 1,
                              ifelse(Openings > Active, Active, Openings),
                              ifelse(RunActive <= Openings, Active,
                                     ifelse(lag(RunActive) < Openings, Openings-lag(RunActive), 0))))
  }
  data.frame(x)
}

for (i in months){
  nonhomeless <- filter(active, ProgramType != "Homeless")
  homeless <- active %>%
    filter(ProgramType == "Homeless") %>%
    mutate(Exits = 0)
  activeafterprogramexits <- left_join(nonhomeless, exitrates) %>%
    mutate(Exits = NA)
  for (j in 1:nrow(activeafterprogramexits)){
    set.seed(j)
    activeafterprogramexits$Exits[j] <- sum(runif(activeafterprogramexits$Active[j]) < activeafterprogramexits$ExitProb[j])
  }
  activeafterprogramexits <- activeafterprogramexits %>%
    mutate(Active = Active-Exits) %>%
    select(-ExitProb) %>%
    rbind(homeless)
  programexits <- activeafterprogramexits %>%
    group_by(ProgramType, ClientPop, Disabled) %>%
    summarise(Exits = sum(Exits)) %>%
    mutate(RRH = NA,
           PSH = NA)
  thexits <- filter(programexits, ProgramType == "TH")
  nothexits <- programexits %>%
    filter(ProgramType != "TH") %>%
    mutate(RRH = 0,
           PSH = 0)
  for (k in 1:nrow(thexits)){
    set.seed(k+100)
    thexits$RRH[k] <- sum(runif(thexits$Exits[k]) < 0.122)
    thexits$PSH[k] <- sum(runif(thexits$Exits[k]-thexits$RRH[k]) < 0.023)
  }
  programexits <- rbind(thexits, nothexits)
  activeafterreentries <- programexits %>%
    select(ClientPop, Disabled, RRH, PSH) %>%
    melt(id.vars = c("ClientPop", "Disabled")) %>%
    filter(variable != "ProgramType") %>%
    mutate(ProgramType = variable,
           Reentries = as.numeric(value)) %>%
    group_by(ClientPop, Disabled, ProgramType) %>%
    summarise(Reentries = sum(Reentries)) %>%
    mutate(Months = 0) %>%
    right_join(activeafterprogramexits) %>%
    mutate(Reentries = ifelse(is.na(Reentries), 0, Reentries)) %>%
    select(-Exits)
  inactiveafterreentries <- programexits %>%
    mutate(Months = 0,
           Exits = Exits-RRH-PSH) %>%
    select(ProgramType, ClientPop, Disabled, Exits, Months) %>%
    right_join(inactive) %>%
    mutate(Exits = ifelse(is.na(Exits), 0, Exits),
           Inactive = ifelse(is.na(Inactive), 0, Inactive))
  extracapacity <- activeafterreentries %>%
    group_by(ProgramType, ProgramPop) %>%
    summarise(Active = sum(Active)+sum(Reentries)) %>%
    full_join(capacity) %>%
    mutate(Openings = Capacity-Active,
           ForcedExits = ifelse(Openings < 0, abs(Openings), 0)) %>%
    filter(ProgramType != "Homeless") %>%
    select(ProgramType, ProgramPop, Openings, ForcedExits)
  if(sum(extracapacity$Openings < 0) > 0){
    forcedexits <- extracapacity %>%
      filter(ForcedExits > 0) %>%
      select(ProgramType, ProgramPop, ForcedExits) %>%
      right_join(activeafterreentries) %>%
      mutate(ForcedExits = ifelse(is.na(ForcedExits), 0, ForcedExits))
    forcedexits <- forcedexits[order(forcedexits$ProgramType, forcedexits$ClientPop, -forcedexits$Months, forcedexits$Disabled),]
    forcedexits <- forcedexits %>%
      group_by(ProgramType, ProgramPop) %>%
      mutate(RunActive = cumsum(Active),
             Rank = 1:n())
    forcedexits$ForcedToExit <- NA
    for (l in 1:nrow(forcedexits)){
      forcedexits$ForcedToExit[l] = ifelse(forcedexits$Rank[l] == 1,
                                           ifelse(forcedexits$RunActive[l] <= forcedexits$ForcedExits[l], forcedexits$Active[l], 
                                                  forcedexits$ForcedExits[l]),
                                           ifelse(forcedexits$RunActive[l] <= forcedexits$ForcedExits[l], forcedexits$Active[l],
                                                  ifelse(forcedexits$RunActive[l-1] <= forcedexits$ForcedExits[l],
                                                         forcedexits$ForcedExits[l]-forcedexits$RunActive[l-1], 0)))
    }
    activeafterforcedexits <- forcedexits %>%
      mutate(Active = Active-ForcedToExit) %>%
      select(ProgramType, ProgramPop, ClientPop, Disabled, Reentries, Months, Active)
    inactiveafterforcedexits <- forcedexits %>%
      group_by(ProgramType, ClientPop, Disabled) %>%
      summarise(ForcedExits = sum(ForcedToExit)) %>%
      mutate(Months = 0) %>%
      filter(ForcedExits > 0) %>%
      right_join(inactiveafterreentries) %>%
      mutate(ForcedExits = ifelse(is.na(ForcedExits), 0, ForcedExits),
             Exits = Exits+ForcedExits) %>%
      select(ProgramType, ClientPop, Disabled, Months, Exits, Inactive)
  }else{
    inactiveafterforcedexits <- inactiveafterreentries
    activeafterforcedexits <- activeafterreentries
  }
  activeafterfth <- activeafterforcedexits %>%
    left_join(fth) %>%
    mutate(FTH = ifelse(is.na(FTH), 0, FTH),
           PreventionEligible = ifelse(is.na(PreventionEligible), 0, PreventionEligible))
  preventspecialcapacity <- extracapacity %>%
    filter(ProgramType == "Prevention",
           ProgramPop != "Adult",
           Openings > 0)
  if(nrow(preventspecialcapacity) > 0){
    preventspecialcapacity <- activeafterfth %>%
      filter(FTH > 0) %>%
      right_join(preventspecialcapacity, by = c("ClientPop" = "ProgramPop"))
    preventspecialcapacity <- preventspecialcapacity[order(preventspecialcapacity$ClientPop, -preventspecialcapacity$Disabled),]
    preventspecialcapacity <- specialentry(preventspecialcapacity)
    preventspecialactive1 <- preventspecialcapacity %>%
      mutate(ProgramType = ProgramType.y,
             Exits = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Months, Entries, Exits)
    preventspecialactive2 <- preventspecialcapacity %>%
      mutate(ProgramType = ProgramType.x,
             Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Months, Entries, Exits)
    preventspecialactive <- rbind(preventspecialactive1, preventspecialactive2)
    activeafterpreventspecial <- activeafterfth %>%
      left_join(preventspecialactive) %>%
      mutate(Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             FTH = FTH-Exits,
             PreventionEligible = PreventionEligible-Exits,
             Entries = Entries+Reentries) %>%
      select(ClientPop, Disabled, ProgramType, Entries, Months, ProgramPop, Active, FTH, PreventionEligible)
  }else{
    activeafterpreventspecial <- activeafterfth %>%
      mutate(Entries = Reentries) %>%
      select(-Reentries)
  }
  preventcapacity <- extracapacity %>%
    filter(ProgramType == "Prevention",
           ProgramPop == "Adult",
           Openings > 0)
  if(nrow(preventcapacity) > 0){
    openings <- preventcapacity$Openings[1]
    preventactive <- data.frame(activeafterpreventspecial) %>%
      filter(PreventionEligible > 0) %>%
      mutate(Openings = openings,
             ProgramPop = "Adult")
    preventactive <- preventactive[order(-preventactive$Disabled, preventactive$ClientPop),]
    preventactive <- entry(preventactive)
    preventactive1 <- data.frame(preventactive) %>%
      mutate(ProgramType = "Prevention",
             Exits = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Months, Entries, Exits)
    preventactive2 <- data.frame(preventactive) %>%
      mutate(Exits = Entries,
             Entries = 0,
             ProgramPop = ClientPop) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Months, Entries, Exits)
    preventactive <- rbind(preventactive1, preventactive2)
    activeafterprevent <- data.frame(activeafterpreventspecial %>%
      full_join(preventactive, by = c("ProgramType", "ProgramPop", "ClientPop", "Disabled", "Months")) %>%
      mutate(Active = ifelse(is.na(Active), 0, Active),
             FTH = ifelse(is.na(FTH), 0, FTH),
             Entries.x = ifelse(is.na(Entries.x), 0, Entries.x),
             Entries.y = ifelse(is.na(Entries.y), 0, Entries.y),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Entries = Entries.x+Entries.y,
             FTH = FTH-Exits) %>%
      filter(ProgramPop == ClientPop | Active > 0 | FTH > 0 | Entries > 0) %>%
      select(ClientPop, Disabled, ProgramType, Months, ProgramPop, Active, FTH, Entries))
  }else{
    activeafterprevent <- select(activeafterpreventspecial, -PreventionEligible)
  }
  pshspecialcapacity <- extracapacity %>%
    filter(ProgramType == "PSH",
           ProgramPop != "Adult",
           Openings > 0)
  if(nrow(pshspecialcapacity) > 0){
    pshspecialactive <- activeafterprevent %>%
      filter(Active > 0,
             ProgramType == "Homeless",
             Disabled == 1) %>%
      right_join(pshspecialcapacity, by = "ProgramPop")
    pshspecialactive <- pshspecialactive[order(pshspecialactive$ClientPop, -pshspecialactive$Months),]
    pshspecialactive <- specialentry(pshspecialactive)
    pshspecialactive1 <- pshspecialactive %>%
      mutate(ProgramType = ProgramType.y) %>%
      group_by(ProgramType, ProgramPop, ClientPop, Disabled) %>%
      summarise(Entries = sum(Entries),
                Exits = 0,
                Months = 0)
    pshspecialactive2 <- pshspecialactive %>%
      mutate(ProgramType = ProgramType.x,
             Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Months, Entries, Exits)
    pshspecialactive <- rbind(data.frame(pshspecialactive1), pshspecialactive2)
    activeafterpshspecial <- activeafterprevent %>%
      left_join(pshspecialactive, by = c("ProgramType", "ProgramPop", "Disabled", "Months", "ClientPop")) %>%
      mutate(Entries.x = ifelse(is.na(Entries.x), 0, Entries.x),
             Entries.y = ifelse(is.na(Entries.y), 0, Entries.y),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Entries = Entries.x+Entries.y,
             Active = Active-Exits) %>%
      select(ClientPop, Disabled, ProgramType, Months, ProgramPop, Active, FTH, Entries)
  }else{
    activeafterpshspecial <- activeafterprevent
  }
  pshcapacity <- extracapacity %>%
    filter(ProgramType == "PSH",
           ProgramPop == "Adult")
  pshopenings <- pshcapacity$Openings[1]
  if(pshopenings > 0){
    pshactive <- activeafterpshspecial %>%
      filter(Active > 0,
             ProgramType == "Homeless",
             Disabled == 1) %>%
      mutate(Openings = pshopenings)
    pshactive <- pshactive[order(-pshactive$Months, pshactive$ClientPop),]
    pshactive <- data.frame(entry(pshactive))
    pshactive1 <- pshactive %>%
      mutate(ProgramType = "PSH") %>%
      group_by(ProgramType, ProgramPop, ClientPop, Disabled) %>%
      summarise(Entries = sum(Entries),
                Exits = 0,
                Months = 0) %>%
      filter(Entries > 0)
    pshactive2 <- pshactive %>%
      mutate(Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Months, Entries, Exits) %>%
      filter(Exits > 0)
    pshactive <- rbind(data.frame(pshactive1), pshactive2)
    activeafterpsh <- activeafterpshspecial %>%
      left_join(pshactive, by = c("ProgramType", "ProgramPop", "Disabled", "Months", "ClientPop")) %>%
      mutate(Entries.y = ifelse(is.na(Entries.y), 0, Entries.y),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Entries = Entries.x+Entries.y,
             Active = Active-Exits) %>%
      select(ClientPop, Disabled, ProgramType, Months, ProgramPop, Active, FTH, Entries)
  }else{
    activeafterpsh <- activeafterpshspecial
  }
  rrhspecialcapacity <- extracapacity %>%
    filter(ProgramType == "RRH",
           ProgramPop != "Adult",
           Openings > 0)
  if(nrow(rrhspecialcapacity) > 0){
    rrhspecialactive <- activeafterpsh %>%
      filter(Active > 0,
             ProgramType == "Homeless") %>%
      right_join(rrhspecialcapacity, by = "ProgramPop")
    rrhspecialactive <- rrhspecialactive[order(rrhspecialactive$ClientPop, -rrhspecialactive$Months, -rrhspecialactive$Disabled),]
    rrhspecialactive <- specialentry(rrhspecialactive)
    rrhspecialactive1 <- rrhspecialactive %>%
      mutate(ProgramType = ProgramType.y) %>%
      group_by(ProgramType, ProgramPop, ClientPop, Disabled) %>%
      summarise(Entries = sum(Entries),
                Exits = 0,
                Months = 0)
    rrhspecialactive2 <- rrhspecialactive %>%
      mutate(ProgramType = ProgramType.x,
             Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Months, Entries, Exits)
    rrhspecialactive <- rbind(data.frame(rrhspecialactive1), rrhspecialactive2)
    activeafterrrhspecial <- activeafterpsh %>%
      left_join(rrhspecialactive, by = c("ProgramType", "ProgramPop", "Disabled", "Months", "ClientPop")) %>%
      mutate(Entries.y = ifelse(is.na(Entries.y), 0, Entries.y),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Entries = Entries.x+Entries.y,
             Active = Active-Exits) %>%
      select(ClientPop, Disabled, ProgramType, Months, ProgramPop, Active, FTH, Entries)
  }else{
    activeafterrrhspecial <- activeafterpsh
  }
  rrhcapacity <- extracapacity %>%
    filter(ProgramType == "RRH",
           ProgramPop == "Adult")
  rrhopenings <- rrhcapacity$Openings[1]
  if(rrhopenings > 0){
    rrhactive <- activeafterrrhspecial %>%
      filter(Active > 0,
             ProgramType == "Homeless") %>%
      mutate(Openings = rrhopenings)
    rrhactive <- rrhactive[order(-rrhactive$Months, -rrhactive$Disabled, rrhactive$ClientPop),]
    rrhactive <- data.frame(entry(rrhactive))
    rrhactive1 <- rrhactive %>%
      mutate(ProgramType = "RRH") %>%
      group_by(ProgramType, ProgramPop, ClientPop, Disabled) %>%
      summarise(Entries = sum(Entries),
                Exits = 0,
                Months = 0) %>%
      filter(Entries > 0)
    rrhactive2 <- rrhactive %>%
      mutate(Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Months, Entries, Exits) %>%
      filter(Exits > 0)
    rrhactive <- rbind(data.frame(rrhactive1), rrhactive2)
    activeafterrrh <- activeafterrrhspecial %>%
      left_join(rrhactive, by = c("ProgramType", "ProgramPop", "Disabled", "Months", "ClientPop")) %>%
      mutate(Entries.y = ifelse(is.na(Entries.y), 0, Entries.y),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Entries = Entries.x+Entries.y,
             Active = Active-Exits) %>%
      select(ClientPop, Disabled, ProgramType, Months, ProgramPop, Active, FTH, Entries)
  }else{
    activeafterrrh <- activeafterrrhspecial
  }
  thspecialcapacity <- extracapacity %>%
    filter(ProgramType == "TH",
           ProgramPop != "Adult",
           Openings > 0)
  if(nrow(thspecialcapacity) > 0){
    thspecialactive <- data.frame(activeafterrrh) %>%
      filter(Active > 0,
             ProgramType == "Homeless") %>%
      inner_join(thspecialcapacity, by = "ProgramPop")
    if(nrow(thspecialactive) > 0){
      thspecialactive <- thspecialactive[order(thspecialactive$ClientPop, -thspecialactive$Months, -thspecialactive$Disabled),]
      thspecialactive <- data.frame(specialentry(thspecialactive))
      thspecialactive1 <- thspecialactive %>%
        mutate(ProgramType = ProgramType.y) %>%
        group_by(ProgramType, ProgramPop, ClientPop, Disabled) %>%
        summarise(Entries = sum(Entries),
                  Exits = 0,
                  Months = 0)
      thspecialactive2 <- thspecialactive %>%
        mutate(ProgramType = ProgramType.x,
               Exits = Entries,
               Entries = 0) %>%
        select(ProgramType, ProgramPop, Disabled, ClientPop, Months, Entries, Exits)
      thspecialactive <- rbind(data.frame(thspecialactive1), thspecialactive2)
      activeafterthspecial <- activeafterrrh %>%
        left_join(thspecialactive, by = c("ProgramType", "ProgramPop", "Disabled", "Months", "ClientPop")) %>%
        mutate(Entries.y = ifelse(is.na(Entries.y), 0, Entries.y),
               Exits = ifelse(is.na(Exits), 0, Exits),
               Entries = Entries.x+Entries.y,
               Active = Active-Exits) %>%
        select(ClientPop, Disabled, ProgramType, Months, ProgramPop, Active, FTH, Entries)
    }else{activeafterthspecial <- activeafterrrh}
  }else{
    activeafterthspecial <- activeafterrrh
  }
  thcapacity <- extracapacity %>%
    filter(ProgramType == "TH",
           ProgramPop == "Adult")
  thopenings <- thcapacity$Openings[1]
  if(thopenings > 0){
    thactive <- activeafterthspecial %>%
      filter(Active > 0,
             ProgramType == "Homeless") %>%
      mutate(Openings = thopenings)
    thactive <- thactive[order(-thactive$Months, -thactive$Disabled, thactive$ClientPop),]
    thactive <- data.frame(entry(thactive))
    thactive1 <- thactive %>%
      mutate(ProgramType = "TH") %>%
      group_by(ProgramType, ProgramPop, ClientPop, Disabled) %>%
      summarise(Entries = sum(Entries),
                Exits = 0,
                Months = 0) %>%
      filter(Entries > 0)
    thactive2 <- thactive %>%
      mutate(Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Months, Entries, Exits) %>%
      filter(Exits > 0)
    thactive <- rbind(data.frame(thactive1), thactive2)
    activeafterth <- activeafterthspecial %>%
      left_join(thactive, by = c("ProgramType", "ProgramPop", "Disabled", "Months", "ClientPop")) %>%
      mutate(Entries.y = ifelse(is.na(Entries.y), 0, Entries.y),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Entries = Entries.x+Entries.y,
             Active = Active-Exits) %>%
      select(ClientPop, Disabled, ProgramType, Months, ProgramPop, Active, FTH, Entries)
  }else{
    activeafterth <- activeafterthspecial
  }
  sr <- activeafterth %>%
    filter(ProgramType == "Homeless",
           Active > 0 | FTH > 0) %>%
    left_join(exitrates) %>%
    mutate(ActiveExits = as.numeric(NA),
           FTHExits = as.numeric(NA))
  if(nrow(sr) > 0){
    for (m in 1:nrow(sr)){
      set.seed(m+200)
      sr$ActiveExits[m] <- sum(runif(sr$Active[m]) < sr$ExitProb[m])
      sr$FTHExits[m] <- sum(runif(sr$FTH[m]) < sr$ExitProb[m])
    }
    activeaftersr <- left_join(activeafterth, sr, by = c("ProgramType", "ProgramPop", "ClientPop", "Disabled", "Months", "Active",
                                                         "FTH")) %>%
      mutate(ActiveExits = ifelse(is.na(ActiveExits), 0, ActiveExits),
             FTHExits = ifelse(is.na(FTHExits), 0, FTHExits),
             FTH = FTH-FTHExits,
             Active = Active-ActiveExits,
             Entries = Entries.x) %>%
      select(ClientPop, Disabled, ProgramType, Months, ProgramPop, Active, FTH, Entries)
    inactiveaftersr <- sr %>%
      mutate(Exits = ActiveExits+FTHExits) %>%
      group_by(ProgramType, ClientPop, Disabled) %>%
      summarise(Exits = sum(Exits),
                Months = 0) %>%
      right_join(inactiveafterforcedexits, by = c("ProgramType", "ClientPop", "Disabled", "Months")) %>%
      mutate(Exits.x = ifelse(is.na(Exits.x), 0, Exits.x),
             Exits = Exits.x+Exits.y) %>%
      select(ProgramType, ClientPop, Disabled, Months, Inactive, Exits)
  }else{
    activeaftersr <- activeafterth
    inactiveaftersr <- inactiveafterforcedexits
  }
  recid <- left_join(inactiveaftersr, returnrates) %>%
    mutate(Recid = NA)
  for (n in 1:nrow(recid)){
    set.seed(n+300)
    recid$Recid[n] <- sum(runif(recid$Inactive[n]) < recid$ReturnProb[n])
  }
  inactiveafterrecid1 <- recid %>%
    mutate(Inactive = Inactive-Recid,
           Months = Months+1) %>%
    filter(Months < 24) %>%
    select(ProgramType, ClientPop, Disabled, Months, Inactive)
  inactiveafterrecid2 <- recid %>%
    mutate(Inactive = Exits) %>%
    filter(Months == 0) %>%
    select(ProgramType, ClientPop, Disabled, Months, Inactive)
  inactiveafterrecid <- rbind(inactiveafterrecid1, inactiveafterrecid2)
  inactiveageout <- data.frame(inactiveafterrecid) %>%
    mutate(AgeOut = NA)
  for (o in 1:nrow(inactiveageout)){
    set.seed(o+400)
    inactiveageout$AgeOut[o] = ifelse(inactiveageout$ClientPop[o] == "Youth", sum(runif(inactiveageout$Inactive[o]) > 71/72), 0)
  }
  inactiveageout$Inactive <- inactiveageout$Inactive-inactiveageout$AgeOut
  inactive <- inactiveageout %>%
    filter(AgeOut > 0) %>%
    mutate(ClientPop = "Adult",
           AgeIn = AgeOut) %>%
    select(ProgramType, ClientPop, Disabled, Months, AgeIn) %>%
    right_join(inactiveageout) %>%
    mutate(AgeIn = ifelse(is.na(AgeIn), 0, AgeIn),
           Inactive = AgeIn+Inactive) %>%
    select(ProgramType, ClientPop, Disabled, Months, Inactive)
  activeafterrecid <- data.frame(recid) %>%
    mutate(ProgramType = "Homeless",
           ProgramPop = ClientPop,
           Months = 0) %>%
    group_by(ProgramType, ProgramPop, ClientPop, Disabled, Months) %>%
    summarise(Recid = sum(Recid)) %>%
    right_join(activeaftersr) %>%
    mutate(Recid = ifelse(is.na(Recid), 0, Recid),
           New = FTH+Entries+Recid)
  activeafterrecid1 <- activeafterrecid %>%
    mutate(Months = Months+1,
           Months = ifelse(ProgramType == "Homeless" & Months > 18, 18, Months),
           Months = ifelse(ProgramType == "Prevention" & Months > 12, 12, Months),
           Months = ifelse(ProgramType == "PSH" & Months > 60, 60, Months),
           Months = ifelse(ProgramType %in% c("RRH", "TH") & Months > 24, 24, Months)) %>%
    group_by(ProgramType, ProgramPop, ClientPop, Disabled, Months) %>%
    summarise(Active = sum(Active)) %>%
    filter(ProgramPop == ClientPop | Active > 0)
  activeafterrecid2 <- activeafterrecid %>%
    filter(Months == 0) %>%
    group_by(ProgramType, ProgramPop, ClientPop, Disabled, Months) %>%
    summarise(Active = sum(New))
  activeafterrecid <- rbind(activeafterrecid1, activeafterrecid2) %>%
    mutate(AgeOut = NA)
  for (p in 1:nrow(activeafterrecid)){
    set.seed(p+500)
    activeafterrecid$AgeOut[p] = ifelse(activeafterrecid$ClientPop[p] == "Youth", sum(runif(activeafterrecid$Active[p]) > 71/72), 0)
  }
  activeafterrecid$Active <- activeafterrecid$Active-activeafterrecid$AgeOut
  active <- data.frame(activeafterrecid) %>%
    filter(AgeOut > 0) %>%
    mutate(AgeIn = AgeOut,
           ClientPop = "Adult") %>%
    select(ProgramType, ProgramPop, ClientPop, Disabled, Months, AgeIn) %>%
    full_join(activeafterrecid) %>%
    mutate(AgeIn = ifelse(is.na(AgeIn), 0, AgeIn),
           Active = ifelse(is.na(Active), 0, Active),
           Active = AgeIn+Active,
           ProgramPop = ifelse(ProgramType == "Homeless", ClientPop, as.character(ProgramPop))) %>%
    group_by(ProgramType, ProgramPop, ClientPop, Disabled, Months) %>%
    summarise(Active = sum(Active))
  homeless <- active %>%
    filter(ProgramType == "Homeless") %>%
    mutate(Exits = 0)
  homelessnums <- homeless %>%
    group_by(ClientPop) %>%
    summarise(Total = sum(Active))
  homelesstotal[i+1] <- sum(homelessnums$Total)
  homelessyouth[i+1] <- homelessnums$Total[homelessnums$ClientPop == "Youth"]
  homelessvets[i+1] <- homelessnums$Total[homelessnums$ClientPop == "Veteran"]
}
totalframe <- data.frame(c(0, months), homelesstotal)
colnames(totalframe) <- c("Months", "Total")
totalframe$Veterans <- homelessvets
totalframe$Youth <- homelessyouth
totalframe <- melt(totalframe, id.vars = "Months") %>%
  mutate(prediction = "model") %>%
  rbind(base)

x <- ggplot(totalframe, aes(x=Months, y=value, color=variable, linetype=prediction)) 

x + stat_smooth(se=FALSE) + labs(x="Months out", y=NULL, title="Individuals experiencing homelessness", color=NULL, linetype=NULL) + 
  scale_x_continuous(breaks = seq(0, 60, 12)) + scale_color_manual(values = c(stehblue, stehgreen, stehorange)) + stehtheme +
  scale_linetype_manual(values = c("dotted", "solid")) + theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 250), labels = format(seq(0, 1000, 250), big.mark = ","))