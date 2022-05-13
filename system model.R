library(tidyverse)
library(lubridate)
library(reshape2)

source("P:/CONTINUUM OF CARE FILES/Data Analysis/theme.R")
setwd("P:/CONTINUUM OF CARE FILES/Data Analysis/System model/2020 fall/inputs")
active <- read.csv("active.csv") %>%
  mutate(ProgramPop = ClientPop)
exitrates <- read.csv("exitrates.csv")
fth <- read.csv("fth.csv") %>%
  mutate(ProgramType = "Homeless")
inactive <- read.csv("inactive.csv")
returnrates <- read.csv("returnrates.csv")

# base <- active %>%
#   group_by(ProgramType, ProgramPop) %>%
#   summarise(Active = sum(Active)) %>%
#   mutate(Active = ifelse(ProgramType == "PSH", Active*1320*12, Active*660*12))

preventadult <- 95040
preventvet <- 190080
preventyouth <- 491040
pshadult <- 13479840
pshvet <- 4245120
pshyouth <- 1235520
rrhadult <- 2140080
rrhvet <- 348480
rrhyouth <- 910800
thadult <- 269280
thvet <- 823680
thyouth <- 110880

capacity <- data.frame(ProgramType = c(rep("Prevention", 3), rep("PSH", 3), rep("RRH", 3), rep("TH", 3)),
                       ProgramPop = rep(c("Adult", "Veteran", "Youth"), 4),
                       Spending = c(preventadult, preventvet, preventyouth, pshadult, pshvet, pshyouth, rrhadult, rrhvet, rrhyouth, thadult, thvet, 
                                    thyouth)) %>%
  mutate(Capacity = ifelse(ProgramType == "PSH", Spending/(12*1320), Spending/(12*660))) %>%
  select(-Spending)

months <- 1:60

homelesstotal <- rep(NA, length(months)+1)
homelessyouth <- rep(NA, length(months)+1)
homeless <- active %>%
  filter(ProgramType == "Homeless") %>%
  mutate(Exits = 0)
homelessnums <- homeless %>%
  summarise(Total = sum(Active),
            Youth = sum(Active[ClientPop == "Youth"]))
homelesstotal[1] <- sum(homelessnums$Total)
homelessyouth[1] <- sum(homelessnums$Youth)

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

parts <- data.frame(value = NA,
                    variable = NA,
                    Month = NA)

for (i in months){
  activeafterprogramexits <- left_join(active, exitrates) %>%
    mutate(Exits = ifelse(ProgramType == "Homeless", 0, Active*ExitRate),
           Active = Active-Exits) %>%
    select(-ExitRate)
  programexits <- activeafterprogramexits %>%
    group_by(ProgramType, ClientPop, Disabled) %>%
    summarise(Exits = sum(Exits)) %>%
    mutate(RRH = ifelse(ProgramType == "TH", Exits*.122, 0),
           PSH = ifelse(ProgramType == "TH" & Disabled == 1, Exits*.023, 0))
  programexittotal <- sum(programexits$Exits)-sum(programexits$RRH)-sum(programexits$PSH)
  activeafterreentries <- data.frame(programexits) %>%
    select(ClientPop, Disabled, RRH, PSH) %>%
    melt(id.vars = c("ClientPop", "Disabled")) %>%
    mutate(ProgramType = variable,
           Reentries = as.numeric(value)) %>%
    group_by(ClientPop, Disabled, ProgramType) %>%
    summarise(Reentries = sum(Reentries)) %>%
    filter(Reentries > 0) %>%
    right_join(activeafterprogramexits) %>%
    mutate(Reentries = ifelse(is.na(Reentries), 0, Reentries),
           Active = Active+Reentries) %>%
    select(-c(Exits, Reentries))
  inactiveafterreentries <- programexits %>%
    mutate(Exits = Exits-RRH-PSH) %>%
    select(-c(RRH, PSH)) %>%
    right_join(inactive) %>%
    mutate(Inactive = Inactive+Exits) %>%
    select(-Exits)
  extracapacity <- activeafterreentries %>%
    group_by(ProgramType, ProgramPop) %>%
    summarise(Active = sum(Active)) %>%
    full_join(capacity) %>%
    filter(!is.na(Capacity)) %>%
    mutate(Openings = Capacity-Active,
           ForcedExits = ifelse(Openings < 0, abs(Openings), 0)) %>%
    select(ProgramType, ProgramPop, Openings, ForcedExits)
  if(sum(extracapacity$Openings < 0) > 0){
    forcedexits <- extracapacity %>%
      filter(ForcedExits > 0) %>%
      select(ProgramType, ProgramPop, ForcedExits) %>%
      right_join(activeafterreentries) %>%
      mutate(ForcedExits = ifelse(is.na(ForcedExits), 0, ForcedExits))
    forcedexits <- forcedexits[order(forcedexits$ProgramType, forcedexits$ClientPop, forcedexits$Disabled),]
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
      select(ClientPop, Disabled, ProgramType, Active, ProgramPop)
    inactiveafterforcedexits <- forcedexits %>%
      group_by(ProgramType, ClientPop, Disabled) %>%
      summarise(ForcedExits = sum(ForcedToExit)) %>%
      right_join(inactiveafterreentries) %>%
      mutate(Inactive = Inactive+ForcedExits) %>%
      select(-ForcedExits)
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
             FTH = FTH-Exits,
             PreventionEligible = PreventionEligible-Exits,
             Active = Entries+Active) %>%
      select(-c(Exits, Entries))
  }else{
    activeafterpreventspecial <- activeafterfth
  }
  preventcapacity <- extracapacity %>%
    filter(ProgramType == "Prevention",
           ProgramPop == "Adult",
           Openings > 0)
  if(nrow(preventcapacity) > 0){
    preventactive <- data.frame(activeafterpreventspecial) %>%
      filter(PreventionEligible > 0) %>%
      mutate(Openings = preventcapacity$Openings[1],
             ProgramPop = "Adult")
    preventactive <- preventactive[order(-preventactive$Disabled, preventactive$ClientPop),]
    preventactive <- entry(preventactive)
    preventactive1 <- data.frame(preventactive) %>%
      mutate(ProgramType = "Prevention",
             Exits = 0) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Entries, Exits)
    preventactive2 <- data.frame(preventactive) %>%
      mutate(Exits = Entries,
             Entries = 0,
             ProgramPop = ClientPop) %>%
      select(ProgramType, ProgramPop, Disabled, ClientPop, Entries, Exits)
    preventactive <- rbind(preventactive1, preventactive2) %>%
      filter(Entries > 0 | Exits > 0)
    activeafterprevent <- activeafterpreventspecial %>%
      full_join(preventactive, by = c("ProgramType", "ProgramPop", "ClientPop", "Disabled")) %>%
      mutate(Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             FTH = FTH-Exits,
             Active = Active+Entries+FTH) %>%
      filter(ProgramPop == ClientPop | Active > 0 | FTH > 0 | Entries > 0) %>%
      select(-c(Exits, FTH, Entries, PreventionEligible))
  }else{
    activeafterprevent <- select(activeafterpreventspecial, -c(PreventionEligible, FTH))
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
    pshspecialactive <- pshspecialactive[order(pshspecialactive$ClientPop),]
    pshspecialactive <- specialentry(pshspecialactive)
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
    pshspecialactive <- rbind(data.frame(pshspecialactive1), pshspecialactive2)
    activeafterpshspecial <- activeafterprevent %>%
      left_join(pshspecialactive, by = c("ProgramType", "ProgramPop", "Disabled", "ClientPop")) %>%
      mutate(Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active+Entries-Exits) %>%
      select(-c(Entries, Exits))
  }else{
    activeafterpshspecial <- activeafterprevent
  }
  pshcapacity <- extracapacity %>%
    filter(ProgramType == "PSH",
           ProgramPop == "Adult")
  if(pshcapacity$Openings[1] > 0){
    pshactive <- activeafterpshspecial %>%
      filter(Active > 0,
             ProgramType == "Homeless",
             Disabled == 1) %>%
      mutate(Openings = pshcapacity$Openings[1])
    pshactive <- pshactive[order(pshactive$ClientPop),]
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
      select(-c(Entries, Exits))
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
    rrhspecialactive <- rrhspecialactive[order(rrhspecialactive$ClientPop, -rrhspecialactive$Disabled),]
    rrhspecialactive <- specialentry(rrhspecialactive)
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
    rrhspecialactive <- rbind(data.frame(rrhspecialactive1), rrhspecialactive2)
    activeafterrrhspecial <- activeafterpsh %>%
      left_join(rrhspecialactive, by = c("ProgramType", "ProgramPop", "Disabled", "ClientPop")) %>%
      mutate(Entries = ifelse(is.na(Entries), 0, Entries),
             Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active-Exits+Entries) %>%
      select(-c(Entries, Exits))
  }else{
    activeafterrrhspecial <- activeafterpsh
  }
  rrhcapacity <- extracapacity %>%
    filter(ProgramType == "RRH",
           ProgramPop == "Adult")
  if(rrhcapacity$Openings[1] > 0){
    rrhactive <- activeafterrrhspecial %>%
      filter(Active > 0,
             ProgramType == "Homeless") %>%
      mutate(Openings = rrhcapacity$Openings[1])
    rrhactive <- rrhactive[order(-rrhactive$Disabled, rrhactive$ClientPop),]
    rrhactive <- data.frame(entry(rrhactive))
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
      select(ClientPop, Disabled, ProgramType, ProgramPop, Active)
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
      thspecialactive <- thspecialactive[order(thspecialactive$ClientPop, --thspecialactive$Disabled),]
      thspecialactive <- data.frame(specialentry(thspecialactive))
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
      thspecialactive <- rbind(data.frame(thspecialactive1), thspecialactive2)
      activeafterthspecial <- activeafterrrh %>%
        left_join(thspecialactive, by = c("ProgramType", "ProgramPop", "Disabled", "ClientPop")) %>%
        mutate(Entries = ifelse(is.na(Entries), 0, Entries),
               Exits = ifelse(is.na(Exits), 0, Exits),
               Active = Active-Exits+Entries) %>%
        select(ClientPop, Disabled, ProgramType, ProgramPop, Active)
    }else{
      activeafterthspecial <- activeafterrrh
      }
  }else{
    activeafterthspecial <- activeafterrrh
  }
  thcapacity <- extracapacity %>%
    filter(ProgramType == "TH",
           ProgramPop == "Adult")
  if(thcapacity$Openings[1] > 0){
    thactive <- activeafterthspecial %>%
      filter(Active > 0,
             ProgramType == "Homeless") %>%
      mutate(Openings = thcapacity$Openings[1])
    thactive <- thactive[order(-thactive$Disabled, thactive$ClientPop),]
    thactive <- entry(data.frame(thactive))
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
      select(ClientPop, Disabled, ProgramType, ProgramPop, Active)
  }else{
    activeafterth <- activeafterthspecial
  }
  sr <- activeafterth %>%
    filter(ProgramType == "Homeless",
           Active > 0) %>%
    left_join(exitrates) %>%
    mutate(Exits = Active*ExitRate) %>%
    select(-c(Active, ExitRate))
  if(nrow(sr) > 0){
    selfresolves <- sum(sr$Exits)
    activeaftersr <- left_join(activeafterth, sr) %>%
      mutate(Exits = ifelse(is.na(Exits), 0, Exits),
             Active = Active-Exits) %>%
      select(-Exits)
    inactiveaftersr <- sr %>%
      group_by(ProgramType, ClientPop, Disabled) %>%
      summarise(Exits = sum(Exits)) %>%
      right_join(inactiveafterforcedexits) %>%
      mutate(Exits = ifelse(is.na(Exits), 0, Exits),
             Inactive = Exits+Inactive) %>%
      select(-Exits)
  }else{
    activeaftersr <- activeafterth
    inactiveaftersr <- inactiveafterforcedexits
    selfresolves <- 0
  }
  recid <- left_join(inactiveaftersr, returnrates) %>%
    mutate(Recid = Inactive*ReturnRate)
  recidtotal <- sum(recid$Recid)
  inactiveafterrecid <- recid %>%
    mutate(Inactive = Inactive-Recid,
           Inactive = Inactive*23/24) %>%
    select(ProgramType, ClientPop, Disabled, Inactive)
  inactiveageout <- data.frame(inactiveafterrecid) %>%
    mutate(AgeOut = ifelse(ClientPop == "Youth", Inactive/72, 0))
  inactive <- inactiveageout %>%
    filter(AgeOut > 0) %>%
    mutate(ClientPop = "Adult",
           AgeIn = AgeOut) %>%
    select(-c(Inactive, AgeOut)) %>%
    right_join(inactiveageout) %>%
    mutate(AgeIn = ifelse(is.na(AgeIn), 0, AgeIn),
           Inactive = Inactive+AgeIn-AgeOut) %>%
    select(ProgramType, ClientPop, Disabled, Inactive)
  activeafterrecid <- data.frame(recid) %>%
    mutate(ProgramType = "Homeless",
           ProgramPop = ClientPop) %>%
    group_by(ProgramType, ProgramPop, ClientPop, Disabled) %>%
    summarise(Recid = sum(Recid)) %>%
    right_join(activeaftersr) %>%
    mutate(Recid = ifelse(is.na(Recid), 0, Recid),
           Active = Active+Recid) %>%
    select(-Recid) %>%
    filter(ProgramPop == ClientPop | Active > 0)
  activeageout <- activeafterrecid %>%
    mutate(AgeOut = ifelse(ClientPop == "Youth", Active/72, 0))
  active <- data.frame(activeageout) %>%
    filter(AgeOut > 0) %>%
    mutate(ClientPop = "Adult",
           AgeIn = AgeOut,
           ProgramPop = ifelse(ProgramType == "Homeless", ClientPop, ProgramPop)) %>%
    select(-c(Active, AgeOut)) %>%
    full_join(activeageout) %>%
    mutate(AgeOut = ifelse(is.na(AgeOut), 0, AgeOut),
           AgeIn = ifelse(is.na(AgeIn), 0, AgeIn),
           Active = ifelse(is.na(Active), 0, Active),
           Active = Active+AgeIn-AgeOut) %>%
    select(-c(AgeOut, AgeIn))
  homelesstotal[i+1] <- sum(active$Active[active$ProgramType == "Homeless"])
  homelessyouth[i+1] <- sum(active$Active[active$ProgramType == "Homeless" & active$ClientPop == "Youth"])
  parttemp <- data.frame(value = c(programexittotal, recidtotal, selfresolves),
                         variable = c("HousingExits", "Returns", "Self resolves"),
                         Month = i)
  parts <<- rbind(parts, parttemp)
}

totalframe <- data.frame(Month = c(0, months),
                         Total = homelesstotal,
                         Youth = homelessyouth) %>%
  melt(id.vars = "Month")

parts <- filter(parts, !is.na(Month))

ggplot(totalframe, aes(x=Month, y=value, color=variable)) + geom_line(size=1) + stehtheme + scale_color_manual(values = c(stehblue2, stehblue4)) +
  labs(x="Months out", y=NULL, title = "Individuals experiencing homelessness", color=NULL) + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1, 61, 12), labels = seq(0, 60, 12))

ggplot(parts, aes(x=Month, y= value, color=variable)) + geom_line(size=1) + stehtheme  


