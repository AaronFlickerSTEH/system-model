library(tidyverse)
library(lubridate)
library(survival)
library(reshape2)
setwd("U:/system model")
mod <- read.csv("singles.csv")
colnames(mod) <- c("EnrollmentID", "PersonalID", "HouseholdID", "Program", "ProgramType", "Veteran", "Entry", "MoveIn", "Exit", 
                   "Age", "ChronicHealth", "Developmental", "HIV", "MentalHealth", "Physical", "SubstanceAbuse", "NightBefore")

mod2 <- mod %>%
  filter(Program != "KEYS Youth Dedicated Service Team - OH0598",
         Program != "LYS Youth Crisis Center Shelter",
         Program != "SHVG Winter Shelter",
         Program !=  "Prince of Peace Winter Shelter",
         Program != "LYS Youth Crisis Center - BCP Preventions") %>%
  mutate(ProgramType = ifelse(Program == "KEYS Diversion - OH0598" | ProgramType == "Homeless Prevention", "Prevention",
                              ifelse(ProgramType %in% c("Emergency Shelter", "Safe Haven", "Street Outreach"), "Homeless",
                                     ifelse(ProgramType == "Transitional Housing", "TH",
                                            ifelse(ProgramType == "PH - Permanent Supportive Housing (disability required)", "PSH",
                                                   ifelse(ProgramType == "PH - Rapid Re-Housing", "RRH", ProgramType))))),
         Veteran = ifelse(Veteran == "Yes", 1, 0),
         Entry = ymd(Entry),
         MoveIn = ymd(MoveIn),
         Exit = ymd(Exit),
         Disabled = ifelse(ChronicHealth == "Yes" | Developmental == "Yes" | HIV == "Yes" | MentalHealth == "Yes" |
                             SubstanceAbuse == "Yes" | Physical == "Yes", 1, 0),
         PreventionEligible = ifelse(NightBefore %in% c("", "Client doesn't know", "Client refused", "Data not collected",
          "Emergency shelter, including hotel or motel paid for with emergency shelter voucher, or RHY-funded Host Home shelter",
          "Hospital or other residential non-psychiatric medical facility", "Jail, prison or juvenile detention facility",
          "Place not meant for habitation (e.g., a vehicle, an abandoned building, bus/train/subway station/airport or anywhere outside)",
          "Safe Haven"), 0, 1),
         ExitMonth = month(Exit),
         ExitMonth = ifelse(is.na(ExitMonth), 0, ExitMonth),
         ExitYear = year(Exit),
         ExitYear = ifelse(is.na(ExitYear), 0, ExitYear)) %>%
  filter(is.na(Exit) | Exit >= as.Date("2016-01-01"))

hh <- mod2 %>%
  group_by(HouseholdID) %>%
  summarise(maxage = max(Age)) %>%
  right_join(mod2) %>%
  mutate(Youth = ifelse(maxage < 25, 1, 0),
         NewEntry = Entry) %>%
  filter((ProgramType != "RRH" & ProgramType != "PSH") | !is.na(MoveIn))
hh$NewEntry[!is.na(hh$MoveIn)] <- hh$MoveIn[!is.na(hh$MoveIn)]
hh$Youth[is.na(hh$Youth)] <- 0

mod3 <- hh %>%
  filter(is.na(Exit) | Exit > NewEntry) %>%
  mutate(Entry = NewEntry) %>%
  select(EnrollmentID, PersonalID, ProgramType:Entry, Exit, Disabled:Youth) %>%
  filter(Entry < as.Date("2019-01-01"))
mod3$Exit[mod3$Exit > as.Date("2018-12-31")] <- NA
mod3$FakeExit <- mod3$Exit
mod3$FakeExit[is.na(mod3$FakeExit)] <- as.Date("2019-01-01")

dates <- select(mod3, PersonalID, ProgramType, Entry, FakeExit)
people <- unique(dates$PersonalID)
dateframe <- data.frame(t(rep(NA, 4)))
colnames(dateframe) <- c("PersonalID", "Entry", "Exit", "ProgramType")
dateframe$Entry <- ymd(dateframe$Entry)
dateframe$Exit <- ymd(dateframe$Exit)

for (i in 1:length(people)){
  frame <- filter(dates, PersonalID == people[i])
  hlframe <- filter(frame, ProgramType == "Homeless")
  thframe <- filter(frame, ProgramType == "TH")
  rrhframe <- filter(frame, ProgramType == "RRH")
  preventframe <- filter(frame, ProgramType == "Prevention")
  pshframe <- filter(frame, ProgramType == "PSH")
  hldates <- as.Date(NA)
  thdates <- as.Date(NA)
  rrhdates <- as.Date(NA)
  preventdates <- as.Date(NA)
  pshdates <- as.Date(NA)
  while (nrow(hlframe) > 0) {
    hldates <- append(hldates, seq.Date(hlframe$Entry[1], hlframe$FakeExit[1]-1, "day"))
    hlframe <- hlframe[-1,]
  }
  hldates <- hldates[!is.na(hldates)]
  hldates <- unique(hldates)
  while (nrow(thframe) > 0) {
    thdates <- append(thdates, seq.Date(thframe$Entry[1], thframe$FakeExit[1]-1, "day"))
    thframe <- thframe[-1,]
  }
  thdates <- thdates[!is.na(thdates)]
  thdates <- unique(thdates)
  while (nrow(rrhframe) > 0) {
    rrhdates <- append(rrhdates, seq.Date(rrhframe$Entry[1], rrhframe$FakeExit[1]-1, "day"))
    rrhframe <- rrhframe[-1,]
  }
  rrhdates <- rrhdates[!is.na(rrhdates)]
  rrhdates <- unique(rrhdates)
  while (nrow(preventframe) > 0) {
    preventdates <- append(preventdates, seq.Date(preventframe$Entry[1], preventframe$FakeExit[1]-1, "day"))
    preventframe <- preventframe[-1,]
  }
  preventdates <- preventdates[!is.na(preventdates)]
  preventdates <- unique(preventdates)
  while (nrow(pshframe) > 0) {
    pshdates <- append(pshdates, seq.Date(pshframe$Entry[1], pshframe$FakeExit[1]-1, "day"))
    pshframe <- pshframe[-1,]
  }
  pshdates <- pshdates[!is.na(pshdates)]
  pshdates <- unique(pshdates)
  rrhdates <- rrhdates[rrhdates %in% pshdates == FALSE]
  thdates <- thdates[thdates %in% pshdates == FALSE]
  thdates <- thdates[thdates %in% rrhdates == FALSE]
  preventdates <- preventdates[preventdates %in% pshdates == FALSE]
  preventdates <- preventdates[preventdates %in% rrhdates == FALSE]
  preventdates <- preventdates[preventdates %in% thdates == FALSE]
  hldates <- hldates[hldates %in% pshdates == FALSE]
  hldates <- hldates[hldates %in% rrhdates == FALSE]
  hldates <- hldates[hldates %in% thdates == FALSE]
  hldates <- hldates[hldates %in% preventdates == FALSE]
  if(length(hldates) > 0){
    hlalldates <- seq.Date(min(hldates), max(hldates)+1, "days")
    nhldates <- hlalldates[hlalldates %in% hldates == FALSE]
    hlentries <- min(hldates)
    hlexits <- min(nhldates)
    while (length(hldates) > 0){
      hldates <- hldates[hldates > max(hlexits)]
      if(length(hldates) > 0){
        hlentries <- append(hlentries, min(hldates))
        nhldates <- nhldates[nhldates > max(hlentries)]
        hlexits <- append(hlexits, min(nhldates))
        hldates <- hldates[hldates > max(hlexits)]
      }
    }
    hldateframe <- data.frame(hlentries, hlexits)
    colnames(hldateframe) <- c("Entry", "Exit")
    hldateframe$ProgramType <- "Homeless"
    rm(hlalldates)
    rm(nhldates)
    rm(hlentries)
    rm(hlexits)
    rm(hldates)
  }
  if(length(rrhdates) > 0){
    rrhalldates <- seq.Date(min(rrhdates), max(rrhdates)+1, "days")
    nrrhdates <- rrhalldates[rrhalldates %in% rrhdates == FALSE]
    rrhentries <- min(rrhdates)
    rrhexits <- min(nrrhdates)
    while (length(rrhdates) > 0){
      rrhdates <- rrhdates[rrhdates > max(rrhexits)]
      if(length(rrhdates) > 0){
        rrhentries <- append(rrhentries, min(rrhdates))
        nrrhdates <- nrrhdates[nrrhdates > max(rrhentries)]
        rrhexits <- append(rrhexits, min(nrrhdates))
        rrhdates <- rrhdates[rrhdates > max(rrhexits)]
      }
    }
    rrhdateframe <- data.frame(rrhentries, rrhexits)
    colnames(rrhdateframe) <- c("Entry", "Exit")
    rrhdateframe$ProgramType <- "RRH"
    rm(rrhalldates)
    rm(nrrhdates)
    rm(rrhentries)
    rm(rrhexits)
    rm(rrhdates)
  }
  if(length(thdates) > 0){
    thalldates <- seq.Date(min(thdates), max(thdates)+1, "days")
    nthdates <- thalldates[thalldates %in% thdates == FALSE]
    thentries <- min(thdates)
    thexits <- min(nthdates)
    while (length(thdates) > 0){
      thdates <- thdates[thdates > max(thexits)]
      if(length(thdates) > 0){
        thentries <- append(thentries, min(thdates))
        nthdates <- nthdates[nthdates > max(thentries)]
        thexits <- append(thexits, min(nthdates))
        thdates <- thdates[thdates > max(thexits)]
      }
    }
    thdateframe <- data.frame(thentries, thexits)
    colnames(thdateframe) <- c("Entry", "Exit")
    thdateframe$ProgramType <- "TH"
    rm(thalldates)
    rm(nthdates)
    rm(thentries)
    rm(thexits)
    rm(thdates)
  }
  if(length(preventdates) > 0){
    preventalldates <- seq.Date(min(preventdates), max(preventdates)+1, "days")
    npreventdates <- preventalldates[preventalldates %in% preventdates == FALSE]
    prevententries <- min(preventdates)
    preventexits <- min(npreventdates)
    while (length(preventdates) > 0){
      preventdates <- preventdates[preventdates > max(preventexits)]
      if(length(preventdates) > 0){
        prevententries <- append(prevententries, min(preventdates))
        npreventdates <- npreventdates[npreventdates > max(prevententries)]
        preventexits <- append(preventexits, min(npreventdates))
        preventdates <- preventdates[preventdates > max(preventexits)]
      }
    }
    preventdateframe <- data.frame(prevententries, preventexits)
    colnames(preventdateframe) <- c("Entry", "Exit")
    preventdateframe$ProgramType <- "Prevention"
    rm(preventalldates)
    rm(npreventdates)
    rm(prevententries)
    rm(preventexits)
    rm(preventdates)
  }
  if(length(pshdates) > 0){
    pshalldates <- seq.Date(min(pshdates), max(pshdates)+1, "days")
    npshdates <- pshalldates[pshalldates %in% pshdates == FALSE]
    pshentries <- min(pshdates)
    pshexits <- min(npshdates)
    while (length(pshdates) > 0){
      pshdates <- pshdates[pshdates > max(pshexits)]
      if(length(pshdates) > 0){
        pshentries <- append(pshentries, min(pshdates))
        npshdates <- npshdates[npshdates > max(pshentries)]
        pshexits <- append(pshexits, min(npshdates))
        pshdates <- pshdates[pshdates > max(pshexits)]
      }
    }
    pshdateframe <- data.frame(pshentries, pshexits)
    colnames(pshdateframe) <- c("Entry", "Exit")
    pshdateframe$ProgramType <- "PSH"
    rm(pshalldates)
    rm(npshdates)
    rm(pshentries)
    rm(pshexits)
    rm(pshdates)
  }
  dateframe1 <- rbind(get0("hldateframe"), get0("rrhdateframe"))
  dateframe2 <- rbind(get0("dateframe1"), get0("thdateframe"))
  dateframe3 <- rbind(get0("dateframe2"), get0("preventdateframe"))
  dateframe4 <- rbind(get0("dateframe3"), get0("pshdateframe"))
  dateframe4$PersonalID <- people[i]
  dateframe <- rbind(dateframe, dateframe4)
  rm(dateframe1)
  rm(dateframe2)
  rm(dateframe3)
  rm(dateframe4)
  rm(hldateframe)
  rm(rrhdateframe)
  rm(thdateframe)
  rm(preventdateframe)
  rm(pshdateframe)
}
dateframe <- filter(dateframe, !is.na(dateframe$PersonalID))
dateframe$Exit[dateframe$Exit == as.Date("2019-01-01")] <- NA
dateframe <- filter(dateframe, is.na(Exit) | Exit >= "2016-01-01")

clients <- mod3 %>%
  group_by(PersonalID) %>%
  summarise(Disabled = max(Disabled),
            Veteran = max(Veteran))

dateframenew <- left_join(dateframe, clients)

youth <- mod3 %>%
  group_by(PersonalID, Entry) %>%
  summarise(Youth = min(Youth, na.rm = TRUE),
            PreventionEligible = min(PreventionEligible))

dateframenewer <- left_join(dateframenew, youth) %>%
  mutate(Youth = ifelse(Veteran == 1, 0, Youth))
done <- filter(dateframenewer, !is.na(Youth) & !is.na(PreventionEligible))
notdone <- filter(dateframenewer, is.na(Youth) | is.na(PreventionEligible))
notdone2 <- left_join(notdone, youth, by = "PersonalID") %>%
  mutate(diff = abs(as.numeric(Entry.x-Entry.y)))

mindiff <- notdone2 %>%
  group_by(PersonalID, Entry.x) %>%
  summarise(diff = min(diff)) %>%
  inner_join(notdone2) %>%
  mutate(Entry = Entry.x,
         Youth = Youth.y,
         PreventionEligible = PreventionEligible.y) %>%
  select(PersonalID, Entry, Exit:Veteran, Youth, PreventionEligible)
mindiff <- unique(mindiff)

mindiff2 <- mindiff %>%
  group_by(PersonalID, Entry) %>%
  summarise(Youth = max(Youth),
            PreventionEligible = min(PreventionEligible)) %>%
  inner_join(mindiff) %>%
  select(PersonalID, Entry, Exit, ProgramType:Veteran, Youth, PreventionEligible)
mindiff3 <- data.frame(mindiff2)

dateframenewest <- rbind(done, mindiff3) %>%
  mutate(ClientPop = ifelse(Veteran == 1, "Veteran",
                            ifelse(Youth == 1, "Youth", "Adult")),
         Disabled = ifelse(ProgramType == "PSH", 1, Disabled),
         ExitMonth = update(Exit, mday = 1),
         FakeEntry = as.Date(NA),
         Months = NA) %>%
  select(PersonalID:Disabled, PreventionEligible:Months)
exitmonths <- sort(unique(dateframenewest$ExitMonth))
dateframenewest$ExitMonth[is.na(dateframenewest$Exit)] <- as.Date("2019-01-01")
dateframenewest$FakeEntry[dateframenewest$Entry < as.Date("2016-01-01")] <- as.Date("2015-12-31")
dateframenewest$FakeEntry[dateframenewest$Entry >= as.Date("2016-01-01")] <- 
  dateframenewest$Entry[dateframenewest$Entry >= "2016-01-01"]

for (i in 1:nrow(dateframenewest)){
  dateframenewest$Months[i] <- sum(exitmonths > dateframenewest$FakeEntry[i] & exitmonths <= dateframenewest$ExitMonth[i])
}
dateframenewest$Months[dateframenewest$ProgramType == "Homeless"] <- dateframenewest$Months[dateframenewest$ProgramType == "Homeless"]+1

homeless <- filter(dateframenewest, ProgramType == "Homeless")
nonhomeless <- filter(dateframenewest, ProgramType != "Homeless")

exitto <- left_join(dateframenewest, nonhomeless, by = "PersonalID") %>%
  filter(!is.na(Exit.x),
         Entry.y >= Exit.x,
         as.numeric(Entry.y-Exit.x) <= 14) %>%
  mutate(Entry = Entry.x,
         ExitedTo = ProgramType.y) %>%
  select(PersonalID, Entry, ExitedTo, Entry.y)

exitto2 <- exitto %>%
  group_by(PersonalID, Entry) %>%
  summarise(Entry.y = min(Entry.y)) %>%
  inner_join(exitto)

dateframenewest2 <- left_join(dateframenewest, exitto2) %>%
  select(-Entry.y)
dateframenewest2$ExitedTo[dateframenewest2$ExitedTo == dateframenewest2$ProgramType] <- NA
dateframenewest2$SR <- ifelse(!is.na(dateframenewest2$Exit) & 
                                (is.na(dateframenewest2$ExitedTo) | dateframenewest2$ProgramType %in% c("Prevention", "PSH", "RRH")), 1,0)

recidivism <- inner_join(dateframenewest2, homeless, by = "PersonalID") %>%
  filter(!is.na(Exit.x),
         Entry.y >= Exit.x,
         as.numeric(Entry.y-Exit.x) <= 730) %>%
  mutate(Entry = Entry.x,
         ExitTo = ProgramType.y)

recidivism2 <- recidivism %>%
  group_by(PersonalID, Entry) %>%
  summarise(Return = min(Entry.y))

dateframenewest3 <- left_join(dateframenewest2, recidivism2) %>%
  mutate(Returned = ifelse(is.na(Return), 0, 1)) %>%
  select(PersonalID:ClientPop, SR, Returned, Months, ExitMonth)

ProgramType <- c(rep(c("Homeless", "Prevention", "RRH", "TH"), each = 6), rep("PSH", 3))
ClientPop <- c(rep(c("Adult", "Veteran", "Youth"), each = 2, 4),
               c("Adult", "Veteran", "Youth"))
activeskeleton <- data.frame(ProgramType, ClientPop) 
activeskeleton$Disabled <- c(rep(c(0, 1), 12), rep(1, 3))

inactivept <- c(rep("Homeless", 144), rep("Prevention", 144), rep("PSH", 72), rep("RRH", 144), rep("TH", 144))
inactivecp <- c(rep(c("Adult", "Veteran", "Youth"), each = 48, 2), rep(c("Adult", "Veteran", "Youth"), each = 24), 
                rep(c("Adult", "Veteran", "Youth"), each = 48, 2))
inactiveskeleton <- data.frame(inactivept, inactivecp)
colnames(inactiveskeleton) <- c("ProgramType", "ClientPop")
inactiveskeleton$Disabled <- c(rep(c(0, 1), each = 24, 6), rep(1, 72), rep(c(0, 1), each = 24, 6))
inactiveskeleton$Months <- rep(0:23, 27)

active <- dateframenewest3 %>%
  filter(is.na(Exit)) %>%
  group_by(ProgramType, ClientPop, Disabled) %>%
  summarise(Active = n()) %>%
  right_join(activeskeleton) %>%
  mutate(Active = ifelse(is.na(Active), 0 , Active))

inactive <- dateframenewest3 %>%
  filter(!is.na(Exit)) %>%
  mutate(Months = as.numeric(as.Date("2019-01-01")-Exit)%/%30) %>%
  filter(Months < 24) %>%
  group_by(ProgramType, ClientPop, Disabled, Months) %>%
  summarise(Inactive = n()) %>%
  right_join(inactiveskeleton) %>%
  mutate(Inactive = ifelse(is.na(Inactive), 0, Inactive))

fth <- dateframenewest3 %>%
  filter(ProgramType == "Homeless") %>%
  group_by(PersonalID) %>%
  summarise(Entry = min(Entry)) %>%
  inner_join(dateframenewest3) %>%
  filter(Entry >= "2018-01-01") %>%
  group_by(ProgramType, ClientPop, Disabled) %>%
  summarise(FTH = round(n()/12, 0),
            PreventionEligible = round(sum(PreventionEligible)/12, 0))


test <- dateframenewest3 %>%
  filter(ProgramType != "Homeless",
         Entry < "2016-02-01" & (is.na(Exit) | Exit >= "2016-02-01"),
         Months > 0)


exitrates <- dateframenewest3 %>%
  group_by(ProgramType, ClientPop, Disabled) %>%
  summarise(ExitRate = sum(SR)/sum(Months))

returnrates <- dateframenewest3 %>%
  filter(!is.na(Exit)) %>%
  group_by(ProgramType, ClientPop, Disabled) %>%
  summarise(ReturnRate = sum(Returned)/(n()*24))
returnrates$ReturnRate[returnrates$ReturnRate == 0] <- 0.00125


setwd("U:/system model/No winter/inputs")
write.csv(active, "active.csv")
write.csv(inactive, "inactive.csv")
write.csv(fth, "fth.csv")
write.csv(exitrates, "exitrates.csv")
write.csv(returnrates, "returnrates.csv")
