library(tidyverse)
library(lubridate)
library(survival)
library(reshape2)
library(readxl)
setwd("P:/CONTINUUM OF CARE FILES/Data Analysis/System model/Current/CoC")
mod <- read.csv("singles.csv")
colnames(mod) <- c("EnrollmentID", "PersonalID", "HouseholdID", "Program", "ProgramType", "Veteran", "Entry", "MoveIn", "Exit", "Age", 
                   "ChronicHealth", "Developmental", "HIV", "MentalHealth", "Physical", "SubstanceAbuse", "NightBefore")
mod2 <- mod %>%
   mutate(ProgramType = ifelse(ProgramType == "Homeless Prevention", "Prevention",
                              ifelse(ProgramType %in% c("Emergency Shelter", "Safe Haven", "Street Outreach"), "Homeless",
                                     ifelse(ProgramType == "Transitional Housing", "TH",
                                            ifelse(ProgramType == "PH - Permanent Supportive Housing (disability required)", "PSH",
                                                   ifelse(ProgramType == "PH - Rapid Re-Housing", "RRH", ProgramType))))),
         Entry = ymd(Entry),
         MoveIn = ymd(MoveIn),
         Exit = ymd(Exit),
         Disabled = ifelse(ChronicHealth == "Yes" | Developmental == "Yes" | HIV == "Yes" | MentalHealth == "Yes" |
                             SubstanceAbuse == "Yes" | Physical == "Yes", 1, 0),
         PreventionEligible = ifelse(NightBefore %in% c("", "Client doesn't know", "Client refused", "Data not collected",
          "Emergency shelter, including hotel or motel paid for with emergency shelter voucher, or RHY-funded Host Home shelter",
          "Hospital or other residential non-psychiatric medical facility", "Jail, prison or juvenile detention facility",
    "Place not meant for habitation (e.g., a vehicle, an abandoned building, bus/train/subway station/airport or anywhere outside)",
    "Safe Haven"), 0, 1)) %>%
  filter(is.na(Exit) | Exit >= today()-1461,
         Entry < today(),
         is.na(MoveIn) | MoveIn < today(),
         Veteran != "Yes",
         Program != "TAL - POP Winter Shelter",
         Program != "[BETA] LYS KEYS - OH0598",
         Program != "BHS ESG-CV Aftercare",
         Program != "CGM Winter Shelter",
         Program != "IHN Aftercare",
         (ProgramType != "RRH" & ProgramType != "PSH") | !is.na(MoveIn))

###Determine youth status; filter out housing clients without move-in dates
mod3 <- mod2 %>%
  group_by(HouseholdID) %>%
  summarise(maxage = max(Age)) %>%
  right_join(mod2) %>%
  mutate(Youth = ifelse(maxage < 25, 1, 0),
         NewEntry = Entry) %>%
  filter((ProgramType != "RRH" & ProgramType != "PSH") | !is.na(MoveIn))
mod3$NewEntry[!is.na(mod3$MoveIn)] <- mod3$MoveIn[!is.na(mod3$MoveIn)]
mod3$Youth[is.na(mod3$Youth)] <- 0

###Filter out enrollments with exit dates on or before entry/move-in
mod4 <- mod3 %>%
  filter(is.na(Exit) | Exit > NewEntry) %>%
  mutate(Entry = NewEntry,
         FakeExit = Exit) %>%
  select(EnrollmentID, PersonalID, ProgramType, Entry, Exit, Disabled:Youth, FakeExit)
mod4$FakeExit[is.na(mod4$FakeExit)] <- today()

###Determine date ranges during which each client was enrolled in each program type
dates <- select(mod4, PersonalID, ProgramType, Entry, FakeExit)
people <- unique(dates$PersonalID)
types <- c("PSH", "RRH", "TH", "Prevention", "Homeless")
dateframe <- data.frame(PersonalID = NA,
                        ProgramType = NA,
                        Entry = as.Date(NA),
                        Exit = as.Date(NA))

for (i in 1:length(people)){
  datelist <- as.list(rep(NA, length(types)))
  clientframe <- filter(dates, PersonalID == people[i])
  for (j in 1:length(types)){
    typeframe <- filter(clientframe, ProgramType == types[j])
    typedates <- as.Date(NA)
    while (nrow(typeframe) > 0){
      typedates <- append(typedates, seq.Date(typeframe$Entry[1], typeframe$FakeExit[1]-1, "days"))
      typeframe <- typeframe[-1,]
    }
    datelist[[j]] <- unique(typedates[!is.na(typedates)])
  }
  datelist[[2]] <- datelist[[2]][datelist[[2]] %in% datelist[[1]] == FALSE]
  datelist[[3]] <- datelist[[3]][datelist[[3]] %in% c(datelist[[1]], datelist[[2]]) == FALSE]
  datelist[[4]] <- datelist[[4]][datelist[[4]] %in% c(datelist[[1]], datelist[[2]], datelist[[3]]) == FALSE]
  datelist[[5]] <- datelist[[5]][datelist[[5]] %in% c(datelist[[1]], datelist[[2]], datelist[[3]], datelist[[4]]) == FALSE]
  for (k in 1:length(datelist)){
    if (length(datelist[[k]]) > 0){
      entries <- min(datelist[[k]])
      alldates <- seq.Date(entries, today(), "days")
      nondates <- alldates[alldates %in% datelist[[k]] == FALSE]
      exits <- min(nondates)
      while (length(datelist[[k]]) > 0){
        datelist[[k]] <- datelist[[k]][datelist[[k]] > max(exits)]
        entries <- append(entries, min(datelist[[k]]))
        nondates <- nondates[nondates > max(entries)]
        exits <- append(exits, min(nondates))
      }
      tempframe <- data.frame(PersonalID = people[i],
                              ProgramType = types[k],
                              Entry = entries,
                              Exit = exits)
      tempframe <- filter(tempframe, !is.infinite(Entry))
      dateframe <<- rbind(dateframe, tempframe)
    }
  }
}
dateframe <- filter(dateframe, !is.na(dateframe$PersonalID))
dateframe$Exit[dateframe$Exit == today()] <- NA

disab <- mod4 %>%
  group_by(PersonalID) %>%
  summarise(Disabled = max(Disabled))

dateframe2 <- left_join(dateframe, disab)

youth <- mod4 %>%
  group_by(PersonalID, Entry) %>%
  summarise(Youth = min(Youth, na.rm = TRUE),
            PreventionEligible = min(PreventionEligible))

dateframe3 <- left_join(dateframe2, youth)
done <- filter(dateframe3, !is.na(Youth) & !is.na(PreventionEligible))
notdone <- filter(dateframe3, is.na(Youth) | is.na(PreventionEligible)) %>%
  left_join(youth, by = "PersonalID") %>%
  mutate(diff = abs(as.numeric(Entry.x-Entry.y)))

mindiff <- notdone %>%
  group_by(PersonalID, Entry.x) %>%
  summarise(diff = min(diff)) %>%
  inner_join(notdone) %>%
  mutate(Entry = Entry.x,
         Youth = Youth.y,
         PreventionEligible = PreventionEligible.y) %>%
  select(PersonalID, Entry, ProgramType:Disabled, Youth, PreventionEligible)
mindiff <- unique(mindiff)

dateframe4 <- rbind(done, data.frame(mindiff))

secondentry <- dateframe4 %>%
  select(PersonalID:Entry)

dtr <- inner_join(dateframe4, secondentry, by = "PersonalID") %>%
  filter(Entry.y >= Exit,
         !is.na(Exit)) %>%
  mutate(DTR = as.numeric(Entry.y-Exit)) %>%
  filter(DTR <= 14,
         ProgramType.x != ProgramType.y,
         ProgramType.y != "Homeless")

dateframe5 <- dtr %>%
  group_by(PersonalID, Exit) %>%
  summarise(DTR = min(DTR)) %>%
  inner_join(dtr) %>%
  mutate(SecondEntry = ProgramType.y) %>%
  select(PersonalID, Exit, SecondEntry) %>%
  right_join(dateframe4) %>%
  mutate(SecondEntry = ifelse(is.na(SecondEntry), "No", SecondEntry))

fth <- filter(dateframe5, ProgramType == "Homeless",
              Entry >= today()-1461) %>%
  select(PersonalID, Entry, Exit)

hl <- filter(dateframe5, ProgramType != "Prevention")

fth2 <- inner_join(fth, hl, by = "PersonalID") %>%
  filter(Exit.y < Entry.x) %>%
  mutate(DTR = as.numeric(Entry.x-Exit.y)) %>%
  filter(DTR <= 730) %>%
  mutate(FTH = 0,
         Entry = Entry.x) %>%
  select(PersonalID, Entry, FTH)
fth2 <- unique(fth2)
dateframe6 <- left_join(fth, fth2) %>%
  mutate(FTH = ifelse(is.na(FTH), 1, FTH)) %>%
  select(-Exit) %>%
  right_join(dateframe5)

dateframe7 <- inner_join(dateframe6, hl, by = "PersonalID") %>%
  filter(!is.na(Exit.x),
         Entry.y >= Exit.x) %>%
  mutate(DTR = as.numeric(Entry.y-Exit.x)) %>%
  filter(DTR <= 730) %>%
  group_by(PersonalID, Exit.x) %>%
  summarise(DTR = min(DTR)) %>%
  mutate(Exit = Exit.x) %>%
  select(-Exit.x) %>%
  right_join(dateframe6) %>%
  mutate(Return = ifelse(is.na(Exit), NA,
                         ifelse(is.na(DTR), 0, 1)),
         ReturnTime = ifelse(Return == 1, DTR%/%30,
                             ifelse(is.na(Return), NA, as.numeric(today()-Exit)%/%30))) %>%
  select(-DTR) %>%
  mutate(Disabled = ifelse(ProgramType == "PSH", 1, Disabled),
         SR = ifelse(is.na(Exit), NA,
                     ifelse(SecondEntry == "No" | ProgramType != "Homeless", 1, 0)),
         ReturnTime = ifelse(ReturnTime > 24, 24, ReturnTime))

reentry1 <- data.frame(round(prop.table(xtabs(~Youth+SecondEntry,
                                              dateframe7[dateframe7$ProgramType == "TH" & 
                                                           dateframe7$Disabled == 1,]), 1), 3)) %>%
  filter(SecondEntry %in% c("PSH", "RRH"),
         Freq > 0) %>%
  mutate(Disabled = 1)

reentry <- data.frame(round(prop.table(xtabs(~Youth+SecondEntry, dateframe7[dateframe7$ProgramType == "TH" & dateframe7$Disabled == 0,]), 
                                       1), 3)) %>%
  filter(SecondEntry %in% c("PSH", "RRH"),
         Freq > 0) %>%
  mutate(Disabled = 0) %>%
  rbind(reentry1)
reentrypsh <- filter(reentry, SecondEntry == "PSH") %>%
  mutate(PSH = Freq) %>%
  select(Youth, Disabled, PSH)
reentry2 <- filter(reentry, SecondEntry == "RRH") %>%
  mutate(RRH = Freq) %>%
  select(Youth, Disabled, RRH) %>%
  full_join(reentrypsh) %>%
  mutate(ProgramType = "TH",
         RRH = ifelse(is.na(RRH), 0, RRH),
         PSH = ifelse(is.na(PSH), 0, PSH))

active <- filter(dateframe7, is.na(Exit))

activetable <- dateframe7 %>%
  filter(is.na(Exit)) %>%
  group_by(ProgramType, Disabled) %>%
  summarise(Youth = sum(Youth),
            Adult = n()-Youth) %>%
  melt(id.vars = c("ProgramType", "Disabled"))
colnames(activetable)[3:4] <- c("ClientPop", "Active")

inactive <- anti_join(dateframe7, active, by = "PersonalID") %>%
  group_by(PersonalID) %>%
  summarise(Exit = max(Exit)) %>%
  inner_join(dateframe7) %>%
  filter(ReturnTime < 24)

inactivetable <- inactive %>%
  group_by(ProgramType, Disabled) %>%
  summarise(Youth = sum(Youth),
            Adult = n()-Youth) %>%
  melt(id.vars = c("ProgramType", "Disabled"))
colnames(inactivetable)[3:4] <- c("ClientPop", "Inactive")

fthtable <- dateframe7 %>%
  filter(FTH == 1 | ProgramType == "Prevention") %>%
  mutate(EntryMonth = as.Date(paste(year(Entry), month(Entry), "01", sep = "-"))) %>%
  group_by(Disabled, Youth, EntryMonth) %>%
  summarise(FTH = n(),
            PreventionEligible = sum(PreventionEligible)) %>%
  filter(EntryMonth < as.Date(paste(year(today()), month(today()), "01", sep = "-")),
         EntryMonth >= as.Date(paste(year(today())-4, month(today()), "01", sep = "-"))) %>%
  group_by(Disabled, Youth) %>%
  summarise(FTH = round(mean(FTH), 0),
            PreventionEligible = round(mean(PreventionEligible), 0)) %>%
  mutate(ClientPop = ifelse(Youth == 1, "Youth", "Adult")) %>%
  select(-Youth)

dateframe7$ExitTime <- as.numeric(dateframe7$Exit-dateframe7$Entry)%/%30
dateframe7$ExitTime[is.na(dateframe7$ExitTime)] <- as.numeric(today()-dateframe7$Entry[is.na(dateframe7$Exit)])%/%30

exitsurv <- Surv(dateframe7$ExitTime, dateframe7$SR)
exitkmcurves <- survfit(exitsurv~ProgramType+Youth+Disabled, dateframe7)
exitstrata <- rep(names(exitkmcurves$strata[1]), as.numeric(exitkmcurves$strata[1]))

exitrateframe <- data.frame(
  Time = 1:as.numeric(exitkmcurves$strata[1]),
  ProgramType = rep(str_remove(strsplit(exitstrata, ",")[[1]][1], "ProgramType="), as.numeric(exitkmcurves$strata[1])),
  Youth = rep(trimws(str_remove(strsplit(exitstrata, ", ")[[1]][2], "Youth=")), as.numeric(exitkmcurves$strata[1])),
  Disabled = as.numeric(rep(str_remove(strsplit(exitstrata, ", ")[[1]][3], "Disabled="), as.numeric(exitkmcurves$strata[1])))
)

for (m in 2:length(exitkmcurves$strata)){
  exitstrata <- rep(names(exitkmcurves$strata[m]), as.numeric(exitkmcurves$strata[m]))
  tempframe <- data.frame(Time = 1:as.numeric(exitkmcurves$strata[m]),
                          ProgramType = rep(str_remove(strsplit(exitstrata, ",")[[1]][1], "ProgramType="), 
                                            as.numeric(exitkmcurves$strata[m])),
                          Youth = rep(trimws(str_remove(strsplit(exitstrata, ", ")[[1]][2], "Youth=")), 
                                           as.numeric(exitkmcurves$strata[m])),
                          Disabled = as.numeric(rep(str_remove(strsplit(exitstrata, ", ")[[1]][3], "Disabled="), 
                                                    as.numeric(exitkmcurves$strata[m]))))
  exitrateframe <<- rbind(exitrateframe, tempframe)
}

exitrateframe$Risk <- exitkmcurves$n.risk
exitrateframe$Exited <- exitkmcurves$n.event

exitrates <- exitrateframe %>%
  group_by(ProgramType, Youth, Disabled) %>%
  summarise(Risk = sum(Risk),
            Exited = sum(Exited)) %>%
  mutate(ExitRate = Exited/Risk)


returnsurv <- Surv(dateframe7$ReturnTime, dateframe7$Return)
returnkmcurves <- survfit(returnsurv~ProgramType+Youth+Disabled, dateframe7)
returnstrata <- rep(names(returnkmcurves$strata[1]), as.numeric(returnkmcurves$strata[1]))

returnrateframe <- data.frame(
  Time = 1:as.numeric(returnkmcurves$strata[1]),
  ProgramType = rep(str_remove(strsplit(returnstrata, ",")[[1]][1], "ProgramType="), as.numeric(returnkmcurves$strata[1])),
  Youth = rep(trimws(str_remove(strsplit(returnstrata, ", ")[[1]][2], "Youth=")), as.numeric(returnkmcurves$strata[1])),
  Disabled = as.numeric(rep(str_remove(strsplit(returnstrata, ", ")[[1]][3], "Disabled="), as.numeric(returnkmcurves$strata[1])))
)

for (n in 2:length(returnkmcurves$strata)){
  returnstrata <- rep(names(returnkmcurves$strata[n]), as.numeric(returnkmcurves$strata[n]))
  tempframe <- data.frame(Time = 1:as.numeric(returnkmcurves$strata[n]),
                          ProgramType = rep(str_remove(strsplit(returnstrata, ",")[[1]][1], "ProgramType="), 
                                            as.numeric(returnkmcurves$strata[n])),
                          Youth = rep(trimws(str_remove(strsplit(returnstrata, ", ")[[1]][2], "Youth=")), 
                                      as.numeric(returnkmcurves$strata[n])),
                          Disabled = as.numeric(rep(str_remove(strsplit(returnstrata, ", ")[[1]][3], "Disabled="), 
                                                    as.numeric(returnkmcurves$strata[n]))))
  returnrateframe <<- rbind(returnrateframe, tempframe)
}

returnrateframe$Risk <- returnkmcurves$n.risk
returnrateframe$Returned <- returnkmcurves$n.event

returnrates <- returnrateframe %>%
  group_by(ProgramType, Youth, Disabled) %>%
  summarise(Risk = sum(Risk),
            Returned = sum(Returned)) %>%
  mutate(ReturnRate = Returned/Risk)






cost <- read_excel("Program costs.xlsx")[c(1, 2, 4, 5, 7)]
colnames(cost) <- c("Program", "ProgramType", "GrantSpending", "MatchRate", "ClientDays")
cost2 <- cost %>%
  mutate(MatchRate = ifelse(is.na(MatchRate), 0, MatchRate),
         TotalSpending = GrantSpending*(1+MatchRate)) %>%
  group_by(ProgramType) %>%
  summarise(TotalSpending = sum(TotalSpending),
            ClientDays = sum(ClientDays)) %>%
  mutate(SpendingPerMonth = TotalSpending*30/ClientDays) %>%
  select(ProgramType, SpendingPerMonth)

write.csv(activetable, "P:/CONTINUUM OF CARE FILES/Data Analysis/System model/CoC/inputs/active.csv", row.names = FALSE)
write.csv(inactivetable, "P:/CONTINUUM OF CARE FILES/Data Analysis/System model/CoC/inputs/inactive.csv", row.names = FALSE)
write.csv(exitrates, "P:/CONTINUUM OF CARE FILES/Data Analysis/System model/CoC/inputs/exitrates.csv", row.names = FALSE)
write.csv(returnrates, "P:/CONTINUUM OF CARE FILES/Data Analysis/System model/CoC/inputs/returnrates.csv", row.names = FALSE)
write.csv(fthtable, "P:/CONTINUUM OF CARE FILES/Data Analysis/System model/CoC/inputs/fth.csv", row.names = FALSE)
write.csv(reentry2, "P:/CONTINUUM OF CARE FILES/Data Analysis/System model/CoC/inputs/secondentry.csv", row.names = FALSE)
write.csv(cost2, "P:/CONTINUUM OF CARE FILES/Data Analysis/System model/CoC/inputs/cost.csv", row.names = FALSE)
