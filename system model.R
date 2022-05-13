adultprevention <- 15
adultpsh <- 1017
adultrrh <- 201
adultth <- 44
veteranprevention <- 10
veteranpsh <- 300
veteranrrh <- 40
veteranth <- 100
youthprevention <- 5
youthpsh <- 100
youthrrh <- 40
youthth <- 20

library(tidyverse)
library(lubridate)
library(reshape2)
source("U:/theme.R")
setwd("U:/system model/inputs")
active <- read.csv("active.csv")[,-1] %>%
  mutate(ClientPop = factor(ClientPop, levels = c("Veteran", "Youth", "Adult")),
         ProgramPop = ClientPop)
exitrates <- read.csv("exitrates.csv")[,-1] %>%
  mutate(ClientPop = factor(ClientPop, levels = c("Veteran", "Youth", "Adult")))
fth <- read.csv("fth.csv")[,-1] %>%
  mutate(Months = 0,
         ProgramType = "Homeless",
         ClientPop = factor(ClientPop, levels = c("Veteran", "Youth", "Adult")),
         ProgramPop = ClientPop)
inactive <- read.csv("inactive.csv")[,-1] %>%
  mutate(ClientPop = factor(ClientPop, levels = c("Veteran", "Youth", "Adult")))
returnrates <- read.csv("returnrates.csv")[,-1] %>%
  mutate(ClientPop = factor(ClientPop, levels = c("Veteran", "Youth", "Adult")))
winter <- read.csv("winter.csv")[,-1]

clientpops <- c("Veteran", "Youth", "Adult")

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
winternums <- winter %>%
  filter(Month == 0) %>%
  select(-Month)
totalnums <- rbind(homelessnums, winternums)

homelesstotal[1] <- sum(totalnums$Total)
homelessyouth[1] <- sum(totalnums$Youth)
homelessvets[1] <- sum(totalnums$Veterans)
rm(homelessnums)
rm(totalnums)
rm(winternums)

programexitnum <- rep(NA, length(months))
forcedexitnum <- rep(NA, length(months))
fthnum <- rep(NA, length(months))
programentrynum <- rep(NA, length(months))
activesrnum <- rep(NA, length(months))
fthsrnum <- rep(NA, length(months))
recidnum <- rep(NA, length(months))

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
  programexitnum[i] <- sum(activeafterprogramexits$Exits)
  activeafterprogramexits <- activeafterprogramexits %>%
    mutate(Active = Active-Exits) %>%
    select(-ExitProb) %>%
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
    mutate(Months = 0) %>%
    right_join(activeafterprogramexits) %>%
    mutate(Reentries = ifelse(is.na(Reentries), 0, Reentries)) %>%
    select(-Exits)
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
    forcedexitnum[i] <- sum(forcedexits$ForcedToExit)
    rm(forcedexits)
  }
  if(sum(extracapacity$Openings < 0) == 0){
    inactiveafterforcedexits <- inactiveafterreentries
    activeafterforcedexits <- activeafterreentries
    forcedexitnum[i] <- 0
  }
  rm(inactiveafterreentries)
  rm(activeafterreentries)
  activeafterfth <- activeafterforcedexits %>%
    mutate(Month = ifelse(i%%12 == 0, 12, i%%12)) %>%
    left_join(fth) %>%
    mutate(FTH = ifelse(is.na(FTH), 0, FTH),
           PreventionEligible = ifelse(is.na(PreventionEligible), 0, PreventionEligible)) %>%
    select(-Month)
  fthnum[i] <- sum(activeafterfth$FTH)
  rm(activeafterforcedexits)
  preventspecialcapacity <- extracapacity %>%
    filter(ProgramType == "Prevention",
           ProgramPop != "Adult",
           Openings > 0)
  if(nrow(preventspecialcapacity) > 0){
    preventspecialcapacity <- activeafterfth %>%
      filter(FTH > 0) %>%
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
           ProgramPop == "Adult")
  if(nrow(preventcapacity) > 0){
    openings <- preventcapacity$Openings[1]
    preventactive <- data.frame(activeafterpreventspecial) %>%
      filter(PreventionEligible > 0) %>%
      mutate(Openings = openings,
             ClientPop = factor(ClientPop, levels = clientpops),
             ProgramPop = "Adult")
    preventactive <- preventactive[order(-preventactive$Disabled, preventactive$ClientPop),]
    preventactive$Rank <- 1:nrow(preventactive)
    preventactive$RunPE <- cumsum(preventactive$PreventionEligible)
    preventactive <- prevententry(preventactive)
    preventactive1 <- data.frame(preventactive) %>%
      mutate(ProgramType = "Prevention",
             Exits = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Months, Entries, Exits)
    preventactive2 <- preventactive %>%
      mutate(Exits = Entries,
             Entries = 0,
             ProgramPop = ClientPop) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Months, Entries, Exits)
    preventactive <- rbind(preventactive1, preventactive2)
    activeafterprevent <- activeafterpreventspecial %>%
      full_join(preventactive, by = c("ProgramType", "ProgramPop", "ClientPop", "Disabled", "Months")) %>%
      mutate(Active = ifelse(is.na(Active), 0, Active),
             FTH = ifelse(is.na(FTH), 0, FTH),
             Entries.x = ifelse(is.na(Entries.x), 0, Entries.x),
             Entries.y = ifelse(is.na(Entries.y), 0, Entries.y),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Entries = Entries.x+Entries.y,
             FTH = FTH-Exits) %>%
      filter(ProgramPop == ClientPop | Active > 0 | FTH > 0 | Entries > 0) %>%
      select(ClientPop, Disabled, ProgramType, Months, ProgramPop, Active, FTH, Entries)
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
      right_join(pshspecialcapacity, by = "ProgramPop")
    pshspecialactive <- pshspecialactive[order(pshspecialactive$ClientPop, -pshspecialactive$Months),]
    pshspecialactive <- pshspecialactive %>%
      group_by(ProgramPop) %>%
      mutate(Rank = 1:n(),
             RunActive = cumsum(Active))
    pshspecialactive <- entry(pshspecialactive)
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
    pshspecialactive <- rbind(pshspecialactive1, pshspecialactive2)
    activeafterpshspecial <- activeafterprevent %>%
      left_join(pshspecialactive, by = c("ProgramType", "ProgramPop", "Disabled", "Months", "ClientPop")) %>%
      mutate(Entries.x = ifelse(is.na(Entries.x), 0, Entries.x),
             Entries.y = ifelse(is.na(Entries.y), 0, Entries.y),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Entries = Entries.x+Entries.y,
             Active = Active-Exits) %>%
      select(ClientPop, Disabled, ProgramType, Months, ProgramPop, Active, FTH, Entries)
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
    pshactive <- pshactive[order(-pshactive$Months, pshactive$ClientPop),]
    pshactive <- data.frame(pshactive) %>%
      mutate(Rank = 1:n(),
             RunActive = cumsum(Active))
    pshactive <- entry(pshactive)
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
    rrhspecialactive <- rrhspecialactive[order(rrhspecialactive$ClientPop, -rrhspecialactive$Months, -rrhspecialactive$Disabled),]
    rrhspecialactive <- rrhspecialactive %>%
      group_by(ProgramPop) %>%
      mutate(Rank = 1:n(),
             RunActive = cumsum(Active))
    rrhspecialactive <- entry(rrhspecialactive)
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
    rrhspecialactive <- rbind(rrhspecialactive1, rrhspecialactive2)
    activeafterrrhspecial <- activeafterpsh %>%
      left_join(rrhspecialactive, by = c("ProgramType", "ProgramPop", "Disabled", "Months", "ClientPop")) %>%
      mutate(Entries.y = ifelse(is.na(Entries.y), 0, Entries.y),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Entries = Entries.x+Entries.y,
             Active = Active-Exits) %>%
      select(ClientPop, Disabled, ProgramType, Months, ProgramPop, Active, FTH, Entries)
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
    rrhactive <- rrhactive[order(-rrhactive$Months, -rrhactive$Disabled, rrhactive$ClientPop),]
    rrhactive <- data.frame(rrhactive) %>%
      mutate(Rank = 1:n(),
             RunActive = cumsum(Active))
    rrhactive <- entry(rrhactive)
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
    thspecialactive <- activeafterrrh %>%
      filter(Active > 0,
             ProgramType == "Homeless") %>%
      right_join(thspecialcapacity, by = "ProgramPop")
    thspecialactive <- thspecialactive[order(thspecialactive$ClientPop, -thspecialactive$Months, -thspecialactive$Disabled),]
    thspecialactive <- thspecialactive %>%
      group_by(ProgramPop) %>%
      mutate(Rank = 1:n(),
             RunActive = cumsum(Active))
    thspecialactive <- entry(thspecialactive)
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
    thspecialactive <- rbind(thspecialactive1, thspecialactive2)
    activeafterthspecial <- activeafterrrh %>%
      left_join(thspecialactive, by = c("ProgramType", "ProgramPop", "Disabled", "Months", "ClientPop")) %>%
      mutate(Entries.y = ifelse(is.na(Entries.y), 0, Entries.y),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Entries = Entries.x+Entries.y,
             Active = Active-Exits) %>%
      select(ClientPop, Disabled, ProgramType, Months, ProgramPop, Active, FTH, Entries)
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
    thactive <- thactive[order(-thactive$Months, -thactive$Disabled, thactive$ClientPop),]
    thactive <- data.frame(thactive) %>%
      mutate(Rank = 1:n(),
             RunActive = cumsum(Active))
    thactive <- entry(thactive)
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
    rm(thactive)
    rm(thactive1)
    rm(thactive2)
  }
  if(thopenings <= 0){
    activeafterth <- activeafterthspecial
  }
  programentrynum[i] <- sum(activeafterth$Entries)
  rm(activeafterthspecial)
  rm(thcapacity)
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
  }
  activesrnum[i] <- sum(sr$ActiveExits)
  fthsrnum[i] <- sum(sr$FTHExits)
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
    recid$InactiveRecid[n] <- sum(runif(recid$Inactive[n]) < recid$ReturnProb[n])
    recid$ExitRecid[n] <- sum(runif(recid$Exits[n]) < recid$ReturnProb[n])
  }
  recidnum[i] <- sum(recid$InactiveRecid)+sum(recid$ExitRecid)
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
  activeafterrecid <- data.frame(recid) %>%
    mutate(ProgramType = "Homeless",
           ProgramPop = ClientPop,
           Months = 0,
           Recid = InactiveRecid+ExitRecid) %>%
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
  active <- rbind(activeafterrecid1, activeafterrecid2)
  rm(activeafterrecid)
  rm(activeafterrecid1)
  rm(activeafterrecid2)
  rm(activeaftersr)
  rm(extracapacity)
  rm(inactiveaftersr)
  rm(homeless)
  rm(programexits)
  rm(recid)
  rm(sr)
  rm(nonhomeless)
  homeless <- active %>%
    filter(ProgramType == "Homeless") %>%
    mutate(Exits = 0)
  homelessnums <- homeless %>%
    group_by(ClientPop) %>%
    summarise(Total = sum(Active))
  homelessnums <- t(homelessnums)
  cols <- homelessnums[1,]
  homelessnums <- data.frame(homelessnums, row.names = NULL)
  colnames(homelessnums) <- cols
  homelessnums <- homelessnums[2,] %>%
    mutate(Adult = as.numeric(as.character(Adult)),
           Veterans = as.numeric(as.character(Veteran)),
           Youth = as.numeric(as.character(Youth))) %>%
    select(-Veteran)
  homelessnums$Total <- apply(homelessnums, 1, sum)
  homelessnums$Adult <- NULL
  if(i%%12 < 3){
    homelessnums <- winter %>%
      filter(Month == i%%12) %>%
      select(-Month) %>%
      rbind(homelessnums)
  }
  homelesstotal[i+1] <- sum(homelessnums$Total)
  homelessyouth[i+1] <- homelessnums$Youth
  homelessvets[i+1] <- homelessnums$Veterans
  rm(homelessnums)
  rm(homeless)
}
totalframe <- data.frame(c(0, months), homelesstotal)
colnames(totalframe) <- c("Months", "Total")
totalframe$Veterans <- homelessvets
totalframe$Youth <- homelessyouth
totalframe <- melt(totalframe, id.vars = "Months")
ggplot(totalframe, aes(x=Months, y=value, color=variable)) + geom_line(size=1) + stehtheme + 
  labs(x="Months out", y=NULL, title="Individuals experiencing homelessness", color=NULL) + scale_x_continuous(breaks = seq(0, 60, 12)) +
  scale_y_continuous(limits = c(0, 1700), breaks = seq(0, 1500, 250), labels = format(seq(0, 1500, 250), big.mark = ",")) +
  scale_color_manual(values = c(stehblue, stehgreen, stehorange)) + theme(legend.position = "bottom")
  
