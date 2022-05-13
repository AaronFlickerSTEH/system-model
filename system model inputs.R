library(tidyverse)
library(lubridate)
library(survival)
library(reshape2)
setwd("U:/system model")
mod <- read.csv("singles.csv")
psh <- read.csv("psh.csv")
colnames(mod) <- c("EnrollmentID", "PersonalID", "HouseholdID", "Program", "ProgramType", "Veteran", "Entry", "MoveIn", "Exit", 
                   "Age", "ChronicHealth", "Developmental", "HIV", "MentalHealth", "Physical", "SubstanceAbuse", "NightBefore")
colnames(psh) <- c("EnrollmentID", "PersonalID", "Entry", "MoveIn", "Exit", "Veteran", "ChronicHealth", "Developmental", "HIV",
                   "MentalHealth", "Physical", "SubstanceAbuse", "OldestHH")
mod2 <- mod %>%
  filter(Program != "KEYS Youth Dedicated Service Team - OH0598",
         Program != "LYS Youth Crisis Center Shelter",
#         Program != "SHVG Winter Shelter",
#         Program !=  "Prince of Peace Winter Shelter",
         Program != "LYS Youth Crisis Center - BCP Preventions") %>%
  mutate(ProgramType = ifelse(Program == "KEYS Diversion - OH0598" | ProgramType == "Homeless Prevention", "Prevention",
                              ifelse(Program %in% c("Prince of Peace Winter Shelter", "SHVG Winter Shelter"), "Winter shelter",
                                     ifelse(ProgramType %in% c("Emergency Shelter", "Safe Haven", "Street Outreach"), "Homeless",
                                            ifelse(ProgramType == "Transitional Housing", "TH",
                                                   ifelse(ProgramType == "PH - Permanent Supportive Housing (disability required)", "PSH",
                                                          ifelse(ProgramType == "PH - Rapid Re-Housing", "RRH", ProgramType)))))),
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
mod3$FakeExit[mod3$ProgramType == "Winter shelter" & mod3$ExitMonth > 2 & mod3$ExitMonth < 7] <- 
  as.Date(paste(mod3$ExitYear[mod3$ProgramType == "Winter shelter" & mod3$ExitMonth > 2 & mod3$ExitMonth < 7], "-03-01", sep = ""))
mod3$FakeExit[mod3$FakeExit > "2019-01-01"] <- as.Date("2019-01-01")
mod3 <- filter(mod3, Entry < FakeExit)

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
  winterframe <- filter(frame, ProgramType == "Winter shelter")
  hldates <- as.Date(NA)
  thdates <- as.Date(NA)
  rrhdates <- as.Date(NA)
  preventdates <- as.Date(NA)
  pshdates <- as.Date(NA)
  winterdates <- as.Date(NA)
  while (nrow(winterframe) > 0) {
    winterdates <- append(winterdates, seq.Date(winterframe$Entry[1], winterframe$FakeExit[1]-1, "day"))
    winterframe <- winterframe[-1,]
  }
  winterdates <- winterdates[!is.na(winterdates)]
  winterdates <- unique(winterdates)
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
  winterdates <- winterdates[winterdates %in% pshdates == FALSE]
  winterdates <- winterdates[winterdates %in% rrhdates == FALSE]
  winterdates <- winterdates[winterdates %in% thdates == FALSE]
  winterdates <- winterdates[winterdates %in% preventdates == FALSE]
  winterdates <- winterdates[winterdates %in% hldates == FALSE]
  if(length(winterdates) > 0){
    winteralldates <- seq.Date(min(winterdates), max(winterdates)+1, "days")
    nwinterdates <- winteralldates[winteralldates %in% winterdates == FALSE]
    winterentries <- min(winterdates)
    winterexits <- min(nwinterdates)
    while (length(winterdates) > 0){
      winterdates <- winterdates[winterdates > max(winterexits)]
      if(length(winterdates) > 0){
        winterentries <- append(winterentries, min(winterdates))
        nwinterdates <- nwinterdates[nwinterdates > max(winterentries)]
        winterexits <- append(winterexits, min(nwinterdates))
        winterdates <- winterdates[winterdates > max(winterexits)]
      }
    }
    winterdateframe <- data.frame(winterentries, winterexits)
    colnames(winterdateframe) <- c("Entry", "Exit")
    winterdateframe$ProgramType <- "Winter shelter"
    rm(winteralldates)
    rm(nwinterdates)
    rm(winterentries)
    rm(winterexits)
    rm(winterdates)
  }
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
  dateframe0 <- rbind(get0("winterdateframe"), get0("hldateframe"))
  dateframe1 <- rbind(get0("dateframe0"), get0("rrhdateframe"))
  dateframe2 <- rbind(get0("dateframe1"), get0("thdateframe"))
  dateframe3 <- rbind(get0("dateframe2"), get0("preventdateframe"))
  dateframe4 <- rbind(get0("dateframe3"), get0("pshdateframe"))
  dateframe4$PersonalID <- people[i]
  dateframe <- rbind(dateframe, dateframe4)
  rm(dateframe0)
  rm(dateframe1)
  rm(dateframe2)
  rm(dateframe3)
  rm(dateframe4)
  rm(winterdateframe)
  rm(hldateframe)
  rm(rrhdateframe)
  rm(thdateframe)
  rm(preventdateframe)
  rm(pshdateframe)
}
dateframe <- filter(dateframe, !is.na(dateframe$PersonalID))
dateframe$Exit[dateframe$Exit == as.Date("2019-01-01")] <- NA

clients <- mod3 %>%
  group_by(PersonalID) %>%
  summarise(Disabled = max(Disabled),
            Veteran = max(Veteran))

dateframenew <- left_join(dateframe, clients)

youth <- mod3 %>%
  group_by(PersonalID, Entry) %>%
  summarise(Youth = min(Youth, na.rm = TRUE),
            PreventionEligible = min(PreventionEligible))
youth$Youth[is.infinite(youth$Youth)] <- 0

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
  mutate(MonthsSinceEntry = as.numeric(as.Date("2019-01-01")-Entry)%/%30,
         LoS = as.numeric(Exit-Entry)%/%30,
         exited = ifelse(is.na(Exit), 0, 1),
         exitedtime = ifelse(exited == 1, LoS, MonthsSinceEntry))
homeless <- filter(dateframenewest, ProgramType %in% c("Homeless", "Winter shelter"))
nonhomeless <- filter(dateframenewest, ProgramType != "Homeless" & ProgramType != "Winter shelter")

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
dateframenewest2$exited[!is.na(dateframenewest2$ExitedTo) & dateframenewest2$ProgramType == "Homeless"] <- 0

recidivism <- inner_join(dateframenewest, homeless, by = "PersonalID") %>%
  filter(!is.na(Exit.x),
         Entry.y >= Exit.x,
         as.numeric(Entry.y-Exit.x) <= 730) %>%
  mutate(Entry = Entry.x,
         ExitTo = ProgramType.y)

recidivism2 <- recidivism %>%
  group_by(PersonalID, Entry) %>%
  summarise(Return = min(Entry.y))

dateframenewest3 <- left_join(dateframenewest2, recidivism2) %>%
  mutate(returned = ifelse(is.na(Return), 0, 1),
         MonthsSinceExit = as.numeric(as.Date("2019-01-01")-Exit)%/%30,
         MonthsToReturn = as.numeric(Return-Exit)%/%30,
         returntime = ifelse(returned == 1, MonthsToReturn, MonthsSinceExit)) %>%
  select(PersonalID:PreventionEligible, exited:ExitedTo, returned, returntime)

return <- filter(dateframenewest3, !is.na(Exit) & ProgramType != "Winter shelter")
returnsurv <- Surv(return$returntime, return$returned)
kmcurves <- survfit(returnsurv~ProgramType, return)
kmnums <- c(1:24, 43:66, 80:103, 117:140, 154:177)
returnrates <- kmcurves$n.event[kmnums]/kmcurves$n.risk[kmnums]
programtypes <- c(rep("Homeless", 24), rep("Prevention", 24), rep("PSH", 24), rep("RRH", 24), rep("TH", 24))
survframe <- data.frame(programtypes, returnrates)
survframe$Months <- rep(0:23, 5)
colnames(survframe) <- c("ProgramType", "ReturnProb", "Months")

returntest <- filter(survframe, ReturnProb > 0) %>%
  group_by(ProgramType) %>%
  summarise(ReturnProb = min(ReturnProb))

zeroes <- filter(survframe, ReturnProb == 0) %>%
  left_join(returntest, by = "ProgramType") %>%
  mutate(ReturnProb = ReturnProb.y) %>%
  select(ProgramType, ReturnProb, Months)

survframe2 <- filter(survframe, ReturnProb > 0) %>%
  rbind(zeroes)

hlreturn <- filter(return, ProgramType == "Homeless")
hlreturnmod <- coxph(Surv(returntime, returned)~Disabled+Veteran+Youth, data = hlreturn)
hlreturn$RiskRatio <- predict(hlreturnmod, hlreturn, type = "risk")
hlreturn <- hlreturn %>%
  mutate(Months = returntime) %>%
  select(ProgramType, Disabled:Youth, RiskRatio)
hlreturn <- unique(hlreturn)

preventreturn <- filter(return, ProgramType == "Prevention")
preventreturnmod <- coxph(Surv(returntime, returned)~Disabled+Veteran+Youth, data = preventreturn)
preventreturn$RiskRatio <- predict(preventreturnmod, preventreturn, type = "risk")
preventreturn <- preventreturn %>%
  mutate(Months = returntime) %>%
  select(ProgramType, Disabled:Youth, RiskRatio)
preventreturn <- unique(preventreturn)

threturn <- filter(return, ProgramType == "TH")
threturnmod <- coxph(Surv(returntime, returned)~Disabled+Veteran+Youth, data = threturn)
threturn$RiskRatio <- predict(threturnmod, threturn, type = "risk")
threturn <- threturn %>%
  mutate(Months = returntime) %>%
  select(ProgramType, Disabled:Youth, RiskRatio)
threturn <- unique(threturn)

rrhreturn <- filter(return, ProgramType == "RRH")
rrhreturnmod <- coxph(Surv(returntime, returned)~Disabled+Veteran+Youth, data = rrhreturn)
rrhreturn$RiskRatio <- predict(rrhreturnmod, rrhreturn, type = "risk")
rrhreturn <- rrhreturn %>%
  mutate(Months = returntime) %>%
  select(ProgramType, Disabled:Youth, RiskRatio)
rrhreturn <- unique(rrhreturn)

pshreturn <- filter(return, ProgramType == "PSH")
pshreturnmod <- coxph(Surv(returntime, returned)~Disabled+Veteran+Youth, data = pshreturn)
pshreturn$RiskRatio <- predict(pshreturnmod, pshreturn, type = "risk")
pshreturn <- pshreturn %>%
  mutate(Months = returntime) %>%
  select(ProgramType, Disabled:Youth, RiskRatio)
pshreturn <- unique(pshreturn)

riskratios <- rbind(hlreturn, preventreturn) %>%
  rbind(threturn) %>%
  rbind(rrhreturn) %>%
  rbind(pshreturn) %>%
  mutate(ProgramType = factor(ProgramType))

returntable <- left_join(survframe2, riskratios) %>%
  mutate(SimpleReturnOdds = (1-ReturnProb)/ReturnProb,
         ReturnOdds = (1-ReturnProb)/(ReturnProb*RiskRatio),
         ReturnProb = 1/(ReturnOdds+1),
         ClientPop = ifelse(Veteran == 1, "Veteran",
                            ifelse(Youth == 1, "Youth", "Adult"))) %>%
  select(ProgramType:Disabled, ClientPop)

inactiveskeleton <- select(returntable, -ReturnProb)
activeprogramtypes <- c(rep("Homeless", 114), rep("Prevention", 78), rep("PSH", 366), rep("RRH", 150), rep("TH", 150))
activemonths <- c(rep(0:18, each = 6),
                  rep(0:12, each = 6),
                  rep(0:60, each = 6),
                  rep(rep(0:24, each = 6), 2))
activeskeleton <- data.frame(activeprogramtypes, activemonths)
colnames(activeskeleton) <- c("ProgramType", "Months")
activeskeleton$ClientPop <- rep(c("Adult", "Youth", "Veteran"), each = 2)
activeskeleton$Disabled <- c(0, 1)

psh2 <- psh %>%
  mutate(Disability = ifelse(ChronicHealth == "Yes" | Developmental == "Yes" | HIV == "Yes" | MentalHealth == "Yes" |
                               Physical == "Yes" | SubstanceAbuse == "Yes", 1, 0),
         Youth = ifelse(OldestHH < 25 & Veteran != "Yes", 1, 0),
         Veteran = ifelse(Veteran == "Yes", 1, 0),
         Entry = ymd(Entry),
         MoveIn = ymd(MoveIn),
         Exit = ymd(Exit)) %>%
  filter(!is.na(MoveIn),
         MoveIn < "2019-01-01",
         MoveIn > "2012-01-01")
psh2$Exit[psh2$Exit >= "2019-01-01"] <- NA

pshdates <- psh2 %>%
  mutate(Entry = MoveIn,
         FakeExit = as.Date(NA))
pshdates$FakeExit[is.na(pshdates$Exit)] <- as.Date("2019-01-01")
pshdates$FakeExit[!is.na(pshdates$Exit)] <- pshdates$Exit[!is.na(pshdates$Exit)]
pshdates <- pshdates %>% select(PersonalID, Entry, FakeExit) %>%
  filter(FakeExit > Entry)

newpshdates <- data.frame(t(rep(NA, 3)))
colnames(newpshdates) <- c("PersonalID", "Entry", "Exit")
newpshdates$Entry <- as.Date(newpshdates$Entry)
newpshdates$Exit <- as.Date(newpshdates$Exit)
pshpeople <- unique(pshdates$PersonalID)
for (i in 1:length(pshpeople)){
  frame <- filter(pshdates, PersonalID == pshpeople[i])
  phdates <- as.Date(NA)
  while (nrow(frame) > 0) {
    phdates <- append(phdates, seq.Date(frame$Entry[1], frame$FakeExit[1]-1, "day"))
    frame <- frame[-1,]
  }
  phdates <- phdates[!is.na(phdates)]
  phdates <- unique(phdates)
  alldates <- seq.Date(min(phdates), max(phdates)+1, "days")
  nphdates <- alldates[alldates %in% phdates == FALSE]
  entries <- min(phdates)
  exits <- min(nphdates)
  while (length(phdates) > 0){
    phdates <- phdates[phdates > max(exits)]
    if(length(phdates) > 0){
      entries <- append(entries, min(phdates))
      nphdates <- nphdates[nphdates > max(entries)]
      exits <- append(exits, min(nphdates))
      phdates <- phdates[phdates > max(exits)]
    }
  }
  phdateframe <- data.frame(entries, exits)
  phdateframe$PersonalID <- pshpeople[i]
  colnames(phdateframe) <- c("Entry", "Exit", "PersonalID")
  newpshdates <- rbind(newpshdates, phdateframe)
  rm(alldates)
  rm(nphdates)
  rm(entries)
  rm(exits)
  rm(phdates)
}
newpshdates <- filter(newpshdates, !is.na(PersonalID))
newpshdates$Exit[newpshdates$Exit == as.Date("2019-01-01")] <- NA
newpshdates$MonthsIn <- as.numeric(newpshdates$Exit-newpshdates$Entry)%/%30
newpshdates$MonthsSinceEntry <- as.numeric(as.Date("2019-01-01")-newpshdates$Entry)%/%30
newpshdates$exited <- ifelse(is.na(newpshdates$Exit), 0, 1)
newpshdates$exittime <- ifelse(newpshdates$exited == 1, newpshdates$MonthsIn, newpshdates$MonthsSinceEntry)  
newpshdates$MonthsIn <- NULL
newpshdates$MonthsSinceEntry <- NULL

psh3 <- select(psh2, PersonalID, MoveIn, Veteran, Disability, Youth)
psh3 <- unique(psh3)
psh4 <- left_join(newpshdates, psh3, by = c("PersonalID", c("Entry" = "MoveIn"))) %>%
  mutate(ProgramType = "PSH")

pshsurv <- Surv(psh4$exittime, psh4$exited)
kmcurvepsh <- survfit(pshsurv~ProgramType, psh4)
pshexitrate <- kmcurvepsh$n.event/kmcurvepsh$n.risk

nopsh <- filter(dateframenewest3, ProgramType != "PSH", ProgramType != "Winter shelter")
exitsurv <- Surv(nopsh$exitedtime, nopsh$exited)
kmcurvesexit <- survfit(exitsurv~ProgramType, nopsh)
exitnums <- c(1:53, 56:80, 83:107)
survratesexit <- c(kmcurvesexit$n.event[exitnums]/kmcurvesexit$n.risk[exitnums], pshexitrate)
programtypes <- c(rep("Homeless", 40), rep("Prevention", 13), rep("RRH", 25), rep("TH", 25), rep("PSH", 85))
survframeexit <- data.frame(programtypes, survratesexit)
survframeexit$Months <- c(0:39, 0:12, 0:24, 0:24, 0:84)
colnames(survframeexit) <- c("ProgramType", "ExitRate", "Months")
exitframe <- left_join(activeskeleton, survframeexit)

pshexitmod <- coxph(Surv(exittime, exited)~Disability+Veteran+Youth, data = psh4)
psh4$RiskRatio <- predict(pshexitmod, psh4, type = "risk")
pshexit <- psh4 %>%
  mutate(Months = exittime,
         Disabled = Disability) %>%
  select(ProgramType, Disabled, Veteran, Youth, RiskRatio)
pshexit <- unique(pshexit)

hlexit <- filter(dateframenewest3, ProgramType == "Homeless")
hlexitmod <- coxph(Surv(exitedtime, exited)~Disabled+Veteran+Youth, data = hlexit)
hlexit$RiskRatio <- predict(hlexitmod, hlexit, type = "risk")
hlexit <- hlexit %>%
  mutate(Months = exitedtime) %>%
  select(ProgramType, Disabled:Youth, RiskRatio)
hlexit <- unique(hlexit)

preventexit <- filter(dateframenewest3, ProgramType == "Prevention")
preventexitmod <- coxph(Surv(exitedtime, exited)~Disabled+Veteran+Youth, data = preventexit)
preventexit$RiskRatio <- predict(preventexitmod, preventexit, type = "risk")
preventexit <- preventexit %>%
  mutate(Months = exitedtime) %>%
  select(ProgramType, Disabled:Youth, RiskRatio)
preventexit <- unique(preventexit)

thexit <- filter(dateframenewest3, ProgramType == "TH")
thexitmod <- coxph(Surv(exitedtime, exited)~Disabled+Veteran+Youth, data = thexit)
thexit$RiskRatio <- predict(thexitmod, thexit, type = "risk")
thexit <- thexit %>%
  mutate(Months = exitedtime) %>%
  select(ProgramType, Disabled:Youth, RiskRatio)
thexit <- unique(thexit)

rrhexit <- filter(dateframenewest3, ProgramType == "RRH")
rrhexitmod <- coxph(Surv(exitedtime, exited)~Disabled+Veteran+Youth, data = rrhexit)
rrhexit$RiskRatio <- predict(rrhexitmod, rrhexit, type = "risk")
rrhexit <- rrhexit %>%
  mutate(Months = exitedtime) %>%
  select(ProgramType, Disabled:Youth, RiskRatio)
rrhexit <- unique(rrhexit)

exitriskratios <- rbind(hlexit, preventexit) %>%
  rbind(thexit) %>%
  rbind(rrhexit) %>%
  rbind(pshexit) %>%
  mutate(ProgramType = factor(ProgramType),
         ClientPop = ifelse(Veteran == 1, "Veteran",
                            ifelse(Youth == 1, "Youth", "Adult"))) %>%
  select(ProgramType, Disabled, RiskRatio, ClientPop)

exittable <- left_join(exitframe, exitriskratios) %>%
  mutate(SimpleExitOdds = (1-ExitRate)/ExitRate,
         ExitOdds = (1-ExitRate)/(ExitRate*RiskRatio),
         ExitProb = 1/(ExitOdds+1),
         ExitProb = ifelse((ProgramType == "Prevention" & Months == 12) | (ProgramType %in% c("RRH", "TH") & Months == 24), 1, 
                           ExitProb)) %>%
  select(ProgramType:Disabled, ExitProb)

active <- filter(dateframenewest3, is.na(Exit),
                 ProgramType != "Winter shelter") %>%
  mutate(Months = as.numeric(as.Date("2019-01-01")-Entry)%/%30,
         ClientPop = ifelse(Veteran == 1, "Veteran",
                            ifelse(Youth == 1, "Youth", "Adult")))
active$Months[active$Months > 60] <- 60
active$Months[active$Months > 12 & active$ProgramType == "Prevention"] <- 12
active$Months[active$Months > 18 & active$ProgramType == "Homeless"] <- 18
active$Months[active$Months > 24 & active$ProgramType %in% c("TH", "RRH")] <- 24
activetable <- active %>%
  group_by(ProgramType, Months, Disabled, ClientPop) %>%
  summarise(Active = n()) %>%
  right_join(activeskeleton) %>%
  mutate(Active = ifelse(is.na(Active), 0, Active))

inactive <- anti_join(dateframenewest3, active, by = "PersonalID")
inactive2 <- inactive %>%
  group_by(PersonalID) %>%
  summarise(Exit = max(Exit)) %>%
  inner_join(inactive) %>%
  mutate(Months = as.numeric(as.Date("2019-01-01")-Exit)%/%30,
         ClientPop = ifelse(Veteran == 1, "Veteran",
                            ifelse(Youth == 1, "Youth", "Adult"))) %>%
  filter(Months < 24)
inactivetable <- inactive2 %>%
  group_by(ProgramType, Months, Disabled, ClientPop) %>%
  summarise(Inactive = n()) %>%
  right_join(inactiveskeleton) %>%
  mutate(Inactive = ifelse(is.na(Inactive), 0, Inactive))

fth <- dateframenewest3 %>%
  filter(ProgramType %in% c("Homeless", "Prevention")) %>%
  group_by(PersonalID) %>%
  summarise(Entry = min(Entry)) %>%
  inner_join(dateframenewest3) %>%
  mutate(EntryMonth = update(Entry, mday = 1)) %>%
  group_by(EntryMonth, Disabled, Veteran, Youth) %>%
  summarise(FTH = n(),
            PreventionEligible = sum(PreventionEligible)) %>%
  filter(EntryMonth >= as.Date("2018-01-01")) %>%
  mutate(Month = month(EntryMonth),
         ClientPop = ifelse(Veteran == 1, "Veteran",
                            ifelse(Youth == 1, "Youth", "Adult")))
fth <- data.frame(fth)
fthskeleton <- data.frame(rep(1:12, each = 6), rep(c(rep("Adult", 2), rep("Youth", 2), rep("Veteran", 2)), 6), rep(c(0,1), 36))
colnames(fthskeleton) <- c("Month", "ClientPop", "Disabled")

fth <- fth %>%
  select(Disabled, FTH:ClientPop) %>%
  right_join(fthskeleton) %>%
  mutate(FTH = ifelse(is.na(FTH), 0, FTH),
         PreventionEligible = ifelse(is.na(PreventionEligible), 0, PreventionEligible))

winter <- dateframenewest3 %>%
  filter(Entry < "2018-10-01",
         ProgramType == "Winter shelter") %>%
  mutate(EntryMonth = update(Entry, mday = 1),
         ExitMonth = update(Exit-1, mday =1)) %>%
  select(PersonalID, Youth, Veteran, EntryMonth, ExitMonth)
winter <- unique(winter)

nonwinter <- dateframenewest %>%
  filter(ProgramType == "Homeless") %>%
  mutate(EntryMonth = update(Entry, mday = 1),
         ExitMonth = update(Exit-1, mday =1)) %>%
  select(PersonalID, EntryMonth, ExitMonth)
nonwinter <- unique(nonwinter)

winter2 <- left_join(winter, nonwinter, by = c("PersonalID", c("EntryMonth" = "ExitMonth")))
winter2$EntryMonth[!is.na(winter2$EntryMonth.y)] <- 
  update(winter2$EntryMonth[!is.na(winter2$EntryMonth.y)], month = month(winter2$EntryMonth[!is.na(winter2$EntryMonth.y)])+1)
winter2 <- filter(winter2, EntryMonth <= ExitMonth)
winter2$EntryMonth.y <- NULL
winter2 <- unique(winter2)

winter3 <- left_join(winter2, nonwinter, by = c("PersonalID", c("ExitMonth" = "EntryMonth")))
winter3$ExitMonth[!is.na(winter3$ExitMonth.y)] <- 
  update(winter3$ExitMonth[!is.na(winter3$ExitMonth.y)], month = month(winter3$ExitMonth[!is.na(winter3$ExitMonth.y)])-1)
winter3$ClientPop <- ifelse(winter3$Veteran == 1, "Veteran",
                            ifelse(winter3$Youth == 1, "Youth", "Adult"))
dates <- sort(unique(winter3$EntryMonth))
Adult <- rep(NA, length(dates))
Youth <- rep(NA, length(dates))
Veteran <- rep(NA, length(dates))
for (i in 1:length(dates)){
  winter4 <- filter(winter3, EntryMonth <= dates[i] & ExitMonth >= dates[i])
  adults <- filter(winter4, ClientPop == "Adult")
  youths <- filter(winter4, ClientPop == "Youth")
  vets <- filter(winter4, ClientPop == "Veteran")
  Adult[i] <- nrow(adults)
  Youth[i] <- nrow(youths)
  Veteran[i] <- nrow(vets)
}
winter5 <- data.frame(Adult, Youth) %>%
  cbind(Veteran)
winter5$Total <- apply(winter5, 1, sum)
winter5$Adult <- NULL
winter5$Month <- rep(0:2, times = 3)
winter6 <- winter5 %>%
  group_by(Month) %>%
  summarise(Total = round(mean(Total), 0),
            Youth = round(mean(Youth), 0),
            Veterans = round(mean(Veteran), 0))

setwd("U:/system model/inputs")
write.csv(activetable, "active.csv")
write.csv(inactivetable, "inactive.csv")
write.csv(exittable, "exitrates.csv")
write.csv(returntable, "returnrates.csv")
write.csv(fth, "fth.csv")
write.csv(winter6, "winter.csv")
