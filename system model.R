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
youthrrh <- 35
youthth <- 11

library(tidyverse)
library(lubridate)
library(reshape2)
source("U:/theme.R")
setwd("U:/system model/No winter/inputs")
active <- read.csv("active.csv")[,-1] %>%
  mutate(ProgramPop = ClientPop)
exitrates <- read.csv("exitrates.csv")[,-1]
fth <- read.csv("fth.csv")[,-1]
inactive <- read.csv("inactive.csv")[,-1]
returnrates <- read.csv("returnrates.csv")[,-1]

ProgramType <- c("Prevention", "PSH", "RRH", "TH")
Adult <- c(adultprevention, adultpsh, adultrrh, adultth)
Veteran <- c(veteranprevention, veteranpsh, veteranrrh, veteranth)
Youth <- c(youthprevention, youthpsh, youthrrh, youthth)
capacity <- data.frame(ProgramType, Adult, Veteran, Youth) %>%
  melt(id.vars = "ProgramType") %>%
  mutate(ProgramPop = variable,
         Capacity = value) %>%
  select(ProgramType, ProgramPop, Capacity)

homelesstotal <- rep(NA, 2)
homelessyouth <- rep(NA, 2)
homelessvets <- rep(NA, 2)
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
rm(homelessnums)

prevententry <- function(df){
  for (i in 1:nrow(df)){
    df$Entries[i] = ifelse(df$Rank[i] == 1,
                           ifelse(df$RunPE[i] <= df$Openings[i], df$PreventionEligible[i], df$Openings[i]),
                           ifelse(df$RunPE[i] <= df$Openings[i], df$PreventionEligible[i],
                                  ifelse(df$RunPE[i-1] <= df$Openings[i], df$Openings[i]-df$RunPE[i-1], 0)))
  }
  df
}
entry <- function(df){
  for (i in 1:nrow(df)){
    df$Entries[i] = ifelse(df$Rank[i] == 1,
                           ifelse(df$RunActive[i] <= df$Openings[i], df$Active[i], df$Openings[i]),
                           ifelse(df$RunActive[i] <= df$Openings[i], df$Active[i],
                                  ifelse(df$RunActive[i-1] <= df$Openings[i], df$Openings[i]-df$RunActive[i-1], 0)))
  }
  df
}

for (i in 1:60){
  nonhomeless <- filter(active, ProgramType != "Homeless")
  homeless <- active %>%
    filter(ProgramType == "Homeless") %>%
    mutate(Exits = 0)
  activeafterprogramexits <- left_join(nonhomeless, exitrates) %>%
    mutate(Exits = NA)
  for (j in 1:nrow(activeafterprogramexits)){
    set.seed(j)
    activeafterprogramexits$Exits[j] <- sum(runif(activeafterprogramexits$Active[j]) < activeafterprogramexits$ExitRate[j])
  }
  activeafterprogramexits <- activeafterprogramexits %>%
    mutate(Active = Active-Exits) %>%
    select(ProgramType, ClientPop, ProgramPop, Disabled, Active, Exits) %>%
    rbind(homeless)
  rm(active)
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
    right_join(activeafterprogramexits) %>%
    mutate(Reentries = ifelse(is.na(Reentries), 0, Reentries),
           Active = Active+Reentries) %>%
    select(ProgramType, ClientPop, ProgramPop, Disabled, Active)
  rm(activeafterprogramexits)
  rm(nothexits)
  rm(thexits)
  inactiveafterreentries <- programexits %>%
    mutate(Months = 0,
           Exits = Exits-RRH-PSH) %>%
    select(ProgramType, ClientPop, Disabled, Exits, Months) %>%
    right_join(inactive) %>%
    mutate(Exits = ifelse(is.na(Exits), 0, Exits),
           Inactive = ifelse(is.na(Inactive), 0, Inactive))
  rm(inactive)
  extracapacity <- activeafterreentries %>%
    group_by(ProgramType, ProgramPop) %>%
    summarise(Active = sum(Active)) %>%
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
    forcedexits <- forcedexits[order(forcedexits$ProgramType, forcedexits$ProgramPop, forcedexits$Disabled),]
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
      select(ProgramType, ProgramPop, ClientPop, Disabled, Reentries, Active)
    inactiveafterforcedexits <- forcedexits %>%
      group_by(ProgramType, ClientPop, Disabled) %>%
      summarise(ForcedExits = sum(ForcedToExit)) %>%
      mutate(Months = 0) %>%
      filter(ForcedExits > 0) %>%
      right_join(inactiveafterreentries) %>%
      mutate(ForcedExits = ifelse(is.na(ForcedExits), 0, ForcedExits),
             Exits = Exits+ForcedExits) %>%
      select(ProgramType, ClientPop, Disabled, Months, Exits, Inactive)
    rm(forcedexits)
  }
  if(sum(extracapacity$Openings < 0) == 0){
    inactiveafterforcedexits <- inactiveafterreentries
    activeafterforcedexits <- activeafterreentries
  }
  rm(inactiveafterreentries)
  rm(activeafterreentries)
  activeafterfth <- activeafterforcedexits %>%
    left_join(fth) %>%
    mutate(FTH = ifelse(is.na(FTH), 0, FTH),
           PreventionEligible = ifelse(is.na(PreventionEligible), 0, PreventionEligible),
           Active = Active+FTH) %>%
    select(-FTH)
  rm(activeafterforcedexits)
  preventspecialcapacity <- extracapacity %>%
    filter(ProgramType == "Prevention",
           ProgramPop != "Adult",
           Openings > 0)
  if(nrow(preventspecialcapacity) > 0){
    preventspecialcapacity <- activeafterfth %>%
      filter(PreventionEligible > 0) %>%
      right_join(preventspecialcapacity, by = c("ClientPop" = "ProgramPop"))
    preventspecialcapacity <- preventspecialcapacity[order(preventspecialcapacity$ClientPop, -preventspecialcapacity$Disabled),]
    preventspecialcapacity <- preventspecialcapacity %>%
      group_by(ProgramPop) %>%
      mutate(Rank = 1:n(),
             RunPE = cumsum(PreventionEligible))
    preventspecialcapacity <- prevententry(preventspecialcapacity)
    preventspecialactive1 <- preventspecialcapacity %>%
      mutate(ProgramType = ProgramType.y,
             Exits = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Entries, Exits)
    preventspecialactive2 <- preventspecialcapacity %>%
      mutate(ProgramType = ProgramType.x,
             Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Entries, Exits)
    preventspecialactive <- rbind(preventspecialactive1, preventspecialactive2)
    activeafterpreventspecial <- activeafterfth %>%
      left_join(preventspecialactive) %>%
      mutate(Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active-Exits+Entries,
             PreventionEligible = PreventionEligible-Exits) %>%
      select(ProgramType, ClientPop, ProgramPop, Disabled, Active, PreventionEligible)
    rm(preventspecialactive)
    rm(preventspecialactive1)
    rm(preventspecialactive2)
  }
  if(nrow(preventspecialcapacity) == 0){
    activeafterpreventspecial <- activeafterfth %>%
      mutate(Entries = Reentries) %>%
      select(-Reentries)
  }
  rm(activeafterfth)
  rm(preventspecialcapacity)
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
    preventactive$Rank <- 1:nrow(preventactive)
    preventactive$RunPE <- cumsum(preventactive$PreventionEligible)
    preventactive <- prevententry(preventactive)
    preventactive1 <- data.frame(preventactive) %>%
      mutate(ProgramType = "Prevention",
             Exits = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Entries, Exits)
    preventactive2 <- preventactive %>%
      mutate(Exits = Entries,
             Entries = 0,
             ProgramPop = ClientPop) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Entries, Exits)
    preventactive <- rbind(preventactive1, preventactive2)
    activeafterprevent <- activeafterpreventspecial %>%
      full_join(preventactive, by = c("ProgramType", "ProgramPop", "ClientPop", "Disabled")) %>%
      mutate(Active = ifelse(is.na(Active), 0, Active),
             Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active-Exits+Entries) %>%
      filter(ProgramPop == ClientPop | Active > 0) %>%
      select(ProgramType, ClientPop, ProgramPop, Disabled, Active)
    rm(preventactive)
    rm(preventactive1)
    rm(preventactive2)
  }
  if(nrow(preventcapacity) == 0){
    activeafterprevent <- select(activeafterpreventspecial, -PreventionEligible)
  }
  rm(activeafterpreventspecial)
  rm(preventcapacity)
  pshspecialcapacity <- extracapacity %>%
    filter(ProgramType == "PSH",
           ProgramPop != "Adult",
           Openings > 0)
  if(nrow(pshspecialcapacity) > 0){
    pshspecialactive <- activeafterprevent %>%
      filter(Active > 0,
             ProgramType == "Homeless",
             Disabled == 1) %>%
      right_join(pshspecialcapacity, by = "ProgramPop") %>%
      group_by(ProgramPop) %>%
      mutate(Rank = 1:n(),
             RunActive = cumsum(Active))
    pshspecialactive <- entry(pshspecialactive)
    pshspecialactive1 <- pshspecialactive %>%
      mutate(ProgramType = ProgramType.y) %>%
      group_by(ProgramType, ProgramPop, ClientPop, Disabled) %>%
      summarise(Entries = sum(Entries),
                Exits = 0)
    pshspecialactive2 <- pshspecialactive %>%
      mutate(ProgramType = ProgramType.x,
             Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Entries, Exits)
    pshspecialactive <- rbind(pshspecialactive1, pshspecialactive2)
    activeafterpshspecial <- activeafterprevent %>%
      left_join(pshspecialactive, by = c("ProgramType", "ProgramPop", "Disabled", "ClientPop")) %>%
      mutate(Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active-Exits+Entries) %>%
      select(ProgramType, ClientPop, ProgramPop, Disabled, Active)
    rm(pshspecialactive)
    rm(pshspecialactive1)
    rm(pshspecialactive2)
  }
  if(nrow(pshspecialcapacity) == 0){
    activeafterpshspecial <- activeafterprevent
  }
  rm(activeafterprevent)
  rm(pshspecialcapacity)
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
    pshactive <- data.frame(pshactive) %>%
      mutate(Rank = 1:n(),
             RunActive = cumsum(Active))
    pshactive <- entry(pshactive)
    pshactive1 <- pshactive %>%
      mutate(ProgramType = "PSH") %>%
      group_by(ProgramType, ProgramPop, ClientPop, Disabled) %>%
      summarise(Entries = sum(Entries),
                Exits = 0) %>%
      filter(Entries > 0)
    pshactive2 <- pshactive %>%
      mutate(Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Entries, Exits) %>%
      filter(Exits > 0)
    pshactive <- rbind(data.frame(pshactive1), pshactive2)
    activeafterpsh <- activeafterpshspecial %>%
      left_join(pshactive, by = c("ProgramType", "ProgramPop", "Disabled", "ClientPop")) %>%
      mutate(Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active-Exits+Entries) %>%
      select(ProgramType, ClientPop, ProgramPop, Disabled, Active)
    rm(pshactive)
    rm(pshactive1)
    rm(pshactive2)
  }
  if(pshopenings <= 0){
    activeafterpsh <- activeafterpshspecial
  }
  rm(activeafterpshspecial)
  rm(pshcapacity)
  rrhspecialcapacity <- extracapacity %>%
    filter(ProgramType == "RRH",
           ProgramPop != "Adult",
           Openings > 0)
  if(nrow(rrhspecialcapacity) > 0){
    rrhspecialactive <- activeafterpsh %>%
      filter(Active > 0,
             ProgramType == "Homeless") %>%
      right_join(rrhspecialcapacity, by = "ProgramPop")
    rrhspecialactive <- rrhspecialactive[order(rrhspecialactive$ClientPop, -rrhspecialactive$Disabled),]
    rrhspecialactive <- rrhspecialactive %>%
      group_by(ProgramPop) %>%
      mutate(Rank = 1:n(),
             RunActive = cumsum(Active))
    rrhspecialactive <- entry(rrhspecialactive)
    rrhspecialactive1 <- rrhspecialactive %>%
      mutate(ProgramType = ProgramType.y) %>%
      group_by(ProgramType, ProgramPop, ClientPop, Disabled) %>%
      summarise(Entries = sum(Entries),
                Exits = 0)
    rrhspecialactive2 <- rrhspecialactive %>%
      mutate(ProgramType = ProgramType.x,
             Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Entries, Exits)
    rrhspecialactive <- rbind(rrhspecialactive1, rrhspecialactive2)
    activeafterrrhspecial <- activeafterpsh %>%
      left_join(rrhspecialactive, by = c("ProgramType", "ProgramPop", "Disabled", "ClientPop")) %>%
      mutate(Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active-Exits+Entries) %>%
      select(ProgramType, ClientPop, ProgramPop, Disabled, Active)
    rm(rrhspecialactive)
    rm(rrhspecialactive1)
    rm(rrhspecialactive2)
  }
  if(nrow(rrhspecialcapacity) == 0){
    activeafterrrhspecial <- activeafterpsh
  }
  rm(activeafterpsh)
  rm(rrhspecialcapacity)
  rrhcapacity <- extracapacity %>%
    filter(ProgramType == "RRH",
           ProgramPop == "Adult")
  rrhopenings <- rrhcapacity$Openings[1]
  if(rrhopenings > 0){
    rrhactive <- activeafterrrhspecial %>%
      filter(Active > 0,
             ProgramType == "Homeless") %>%
      mutate(Openings = rrhopenings)
    rrhactive <- rrhactive[order(-rrhactive$Disabled, rrhactive$ClientPop),]
    rrhactive <- data.frame(rrhactive) %>%
      mutate(Rank = 1:n(),
             RunActive = cumsum(Active))
    rrhactive <- entry(rrhactive)
    rrhactive1 <- rrhactive %>%
      mutate(ProgramType = "RRH") %>%
      group_by(ProgramType, ProgramPop, ClientPop, Disabled) %>%
      summarise(Entries = sum(Entries),
                Exits = 0) %>%
      filter(Entries > 0)
    rrhactive2 <- rrhactive %>%
      mutate(Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Entries, Exits) %>%
      filter(Exits > 0)
    rrhactive <- rbind(data.frame(rrhactive1), rrhactive2)
    activeafterrrh <- activeafterrrhspecial %>%
      left_join(rrhactive, by = c("ProgramType", "ProgramPop", "Disabled", "ClientPop")) %>%
      mutate(Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active-Exits+Entries) %>%
      select(ProgramType, ClientPop, ProgramPop, Disabled, Active)
    rm(rrhactive)
    rm(rrhactive1)
    rm(rrhactive2)
  }
  if(rrhopenings <= 0){
    activeafterrrh <- activeafterrrhspecial
  }
  rm(activeafterrrhspecial)
  rm(rrhcapacity)
  thspecialcapacity <- extracapacity %>%
    filter(ProgramType == "TH",
           ProgramPop != "Adult",
           Openings > 0)
  if(nrow(thspecialcapacity) > 0){
    thspecialactive <- data.frame(activeafterrrh) %>%
      filter(Active > 0,
             ProgramType == "Homeless") %>%
      inner_join(thspecialcapacity, by = "ProgramPop")
    thspecialactive <- thspecialactive[order(thspecialactive$ClientPop, -thspecialactive$Disabled),]
    thspecialactive <- thspecialactive %>%
      group_by(ProgramPop) %>%
      mutate(Rank = 1:n(),
             RunActive = cumsum(Active))
    thspecialactive <- entry(thspecialactive)
    thspecialactive1 <- thspecialactive %>%
      mutate(ProgramType = ProgramType.y) %>%
      group_by(ProgramType, ProgramPop, ClientPop, Disabled) %>%
      summarise(Entries = sum(Entries),
                Exits = 0)
    thspecialactive2 <- thspecialactive %>%
      mutate(ProgramType = ProgramType.x,
             Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Entries, Exits)
    thspecialactive <- rbind(thspecialactive1, thspecialactive2)
    activeafterthspecial <- activeafterrrh %>%
      left_join(thspecialactive, by = c("ProgramType", "ProgramPop", "Disabled", "ClientPop")) %>%
      mutate(Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active-Exits+Entries) %>%
      select(ProgramType, ClientPop, ProgramPop, Disabled, Active)
    rm(thspecialactive)
    rm(thspecialactive1)
    rm(thspecialactive2)
  }
  if(nrow(thspecialcapacity) == 0){
    activeafterthspecial <- activeafterrrh
  }
  rm(activeafterrrh)
  rm(thspecialcapacity)
  thcapacity <- extracapacity %>%
    filter(ProgramType == "TH",
           ProgramPop == "Adult")
  thopenings <- thcapacity$Openings[1]
  if(thopenings > 0){
    thactive <- activeafterthspecial %>%
      filter(Active > 0,
             ProgramType == "Homeless") %>%
      mutate(Openings = thopenings)
    thactive <- thactive[order(-thactive$Disabled, thactive$ClientPop),]
    thactive <- data.frame(thactive) %>%
      mutate(Rank = 1:n(),
             RunActive = cumsum(Active))
    thactive <- entry(thactive)
    thactive1 <- thactive %>%
      mutate(ProgramType = "TH") %>%
      group_by(ProgramType, ProgramPop, ClientPop, Disabled) %>%
      summarise(Entries = sum(Entries),
                Exits = 0) %>%
      filter(Entries > 0)
    thactive2 <- thactive %>%
      mutate(Exits = Entries,
             Entries = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Entries, Exits) %>%
      filter(Exits > 0)
    thactive <- rbind(data.frame(thactive1), thactive2)
    activeafterth <- activeafterthspecial %>%
      left_join(thactive, by = c("ProgramType", "ProgramPop", "Disabled", "ClientPop")) %>%
      mutate(Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active-Exits+Entries) %>%
      select(ProgramType, ClientPop, ProgramPop, Disabled, Active)
    rm(thactive)
    rm(thactive1)
    rm(thactive2)
  }
  if(thopenings <= 0){
    activeafterth <- activeafterthspecial
  }
  rm(activeafterthspecial)
  rm(thcapacity)
  sr <- activeafterth %>%
    filter(ProgramType == "Homeless",
           Active > 0) %>%
    left_join(exitrates)
  if(nrow(sr) > 0){
    for (m in 1:nrow(sr)){
      set.seed(m+200)
      sr$Exits[m] <- sum(runif(sr$Active[m]) < sr$ExitRate[m])
    }
    activeaftersr <- left_join(activeafterth, sr, by = c("ProgramType", "ProgramPop", "ClientPop", "Disabled", "Active")) %>%
      mutate(Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active-Exits) %>%
      select(ProgramType, ClientPop, ProgramPop, Disabled, Active)
    inactiveaftersr <- sr %>%
      mutate(Months = 0) %>%
      right_join(inactiveafterforcedexits, by = c("ProgramType", "ClientPop", "Disabled", "Months")) %>%
      mutate(Exits.x = ifelse(is.na(Exits.x), 0, Exits.x),
             Exits = Exits.x+Exits.y) %>%
      select(ProgramType, ClientPop, Disabled, Months, Inactive, Exits)
  }
  if(nrow(sr) == 0){
    activeaftersr <- activeafterth
    inactiveaftersr <- inactiveafterforcedexits
  }
  rm(activeafterth)
  rm(inactiveafterforcedexits)
  recid <- left_join(inactiveaftersr, returnrates) %>%
    mutate(InactiveRecid = NA,
           ExitRecid = NA)
  for (n in 1:nrow(recid)){
    set.seed(n+300)
    recid$InactiveRecid[n] <- sum(runif(recid$Inactive[n]) < recid$ReturnRate[n])
    recid$ExitRecid[n] <- sum(runif(recid$Exits[n]) < recid$ReturnRate[n])
  }
  inactiveafterrecid1 <- recid %>%
    mutate(Inactive = Inactive-InactiveRecid,
           Months = Months+1) %>%
    filter(Months < 24) %>%
    select(ProgramType, ClientPop, Disabled, Months, Inactive)
  inactiveafterrecid2 <- recid %>%
    mutate(Inactive = Exits-ExitRecid) %>%
    filter(Months == 0) %>%
    select(ProgramType, ClientPop, Disabled, Months, Inactive)
  inactive <- rbind(inactiveafterrecid1, inactiveafterrecid2)
  rm(inactiveafterrecid1)
  rm(inactiveafterrecid2)
  active <- data.frame(recid) %>%
    mutate(ProgramType = "Homeless",
           ProgramPop = ClientPop,
           Recid = InactiveRecid+ExitRecid) %>%
    group_by(ProgramType, ProgramPop, ClientPop, Disabled) %>%
    summarise(Recid = sum(Recid)) %>%
    right_join(activeaftersr) %>%
    mutate(Recid = ifelse(is.na(Recid), 0, Recid),
           Active = Active+Recid) %>%
    select(-Recid)
  rm(activeaftersr)
  rm(extracapacity)
  rm(inactiveaftersr)
  rm(homeless)
  rm(programexits)
  rm(recid)
  rm(sr)
  rm(nonhomeless)
  }
homeless <- active %>%
  filter(ProgramType == "Homeless") %>%
  mutate(Exits = 0)
homelessnums <- homeless %>%
  summarise(Total = sum(Active),
            Youth = sum(Active[ClientPop == "Youth"]),
            Veterans = sum(Active[ClientPop == "Veteran"]))
homelesstotal[2] <- sum(homelessnums$Total)
homelessyouth[2] <- sum(homelessnums$Youth)
homelessvets[2] <- sum(homelessnums$Veterans)
rm(homelessnums)

endframe <- data.frame(homelesstotal, homelessvets) %>%
  cbind(homelessyouth) %>%
  mutate(time = c("Start", "Finish"))
