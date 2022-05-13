library(tidyverse)
library(lubridate)
library(survival)
library(reshape2)
library(readxl)
setwd("C:/Users/aflicker/Strategies To End Homelessness, Inc/CommonFiles - CommonFiles/CONTINUUM OF CARE FILES/Data Analysis/System model/Current/vulnerable")
mod <- read.csv("singles.csv", col.names = c("EnrollmentID", "PersonalID", "HouseholdID", "Program", "ProgramType", "Veteran", "Entry", "MoveIn", "Exit", 
                                             "Age", "ChronicHealth", "Developmental", "HIV", "MentalHealth", "Physical", "SubstanceAbuse", "NightBefore")) %>%
   mutate(ProgramType = ifelse(ProgramType == "Homelessness Prevention", "Prevention",
                              ifelse(ProgramType %in% c("Emergency Shelter", "Safe Haven", "Street Outreach"), "Homeless",
                                     ifelse(ProgramType == "Transitional Housing", "TH",
                                            ifelse(ProgramType == "PH - Permanent Supportive Housing (disability required for entry)", "PSH",
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
         Program %in% c("TAL - POP Winter Shelter", "[BETA] LYS KEYS - OH0598", "BHS ESG-CV Aftercare", "CGM Winter Shelter", "IHN Aftercare",
                        "BHS KEYS - OH0598", "IHN KEYS - OH0598", "LYS KEYS - OH0598", "Maslow's Army Shelter", "Maslows Army Hotel/Motel",
                        "SA KEYS - OH0598") == FALSE,
         (ProgramType != "RRH" & ProgramType != "PSH") | !is.na(MoveIn))
vi <- read.csv("vispdat.csv", 
               col.names = c("PersonalID", "VIType", "VIDate", "Score"),
               colClasses = c("character", "character", "Date", "numeric"))

###Filter out unaccompanied youth
mod2 <- mod %>%
  group_by(HouseholdID) %>%
  summarise(maxage = max(Age)) %>%
  right_join(mod) %>%
  filter(maxage > 24) %>%
  select(-maxage) %>%
  mutate(NewEntry = Entry)
###Set entry to move-in date where applicable
mod2$NewEntry[!is.na(mod2$MoveIn)] <- mod2$MoveIn[!is.na(mod2$MoveIn)]

###Filter out enrollments with exit dates on or before entry/move-in
mod3 <- mod2 %>%
  filter(is.na(Exit) | Exit > NewEntry) %>%
  mutate(Entry = NewEntry,
         FakeExit = Exit) %>%
  select(EnrollmentID, PersonalID, ProgramType, Entry, Exit, Disabled, PreventionEligible, FakeExit)
mod3$FakeExit[is.na(mod3$FakeExit)] <- today()

###Determine date ranges during which each client was enrolled in each program type
dates <- select(mod3, PersonalID, ProgramType, Entry, FakeExit)
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
dateframe$Exit[dateframe$Exit == today()] <- NA

## Determine disability status for each client
disab <- mod3 %>%
  group_by(PersonalID) %>%
  summarise(Disabled = max(Disabled))

## Determine prevention eligibility status for each entry
pe <- mod3 %>%
  group_by(PersonalID, Entry) %>%
  summarise(PreventionEligible = min(PreventionEligible))

dateframe2 <- filter(dateframe, !is.na(PersonalID)) %>%
  left_join(disab) %>%
  left_join(pe) %>%
  mutate(PreventionEligible = ifelse(is.na(PreventionEligible), 0, PreventionEligible))

## Find moves from one program type to another
secondentry <- dateframe2 %>%
  select(PersonalID:Entry)

dtr <- inner_join(dateframe2, secondentry, by = "PersonalID") %>%
  filter(Entry.y >= Exit,
         !is.na(Exit)) %>%
  mutate(DTR = as.numeric(Entry.y-Exit)) %>%
  filter(DTR <= 14,
         ProgramType.x != ProgramType.y,
         ProgramType.y != "Homeless")

dateframe3 <- dtr %>%
  group_by(PersonalID, Exit) %>%
  summarise(DTR = min(DTR)) %>%
  inner_join(dtr) %>%
  mutate(SecondEntry = ProgramType.y) %>%
  select(PersonalID, Exit, SecondEntry) %>%
  right_join(dateframe2) %>%
  mutate(SecondEntry = ifelse(is.na(SecondEntry), "No", SecondEntry)) %>%
  left_join(vi) %>%
  mutate(diff = abs(as.numeric(Entry-VIDate)),
         diff = ifelse(is.na(diff), 0, diff)) %>%
  select(-VIType)

## Find the most current available VI-SPDAT score for each entry
dateframe4 <- dateframe3 %>%
  group_by(PersonalID, Entry) %>%
  summarise(diff = min(diff)) %>%
  inner_join(dateframe3)

dateframe5 <- dateframe4 %>%
  group_by(PersonalID, Entry) %>%
  summarise(Score = max(Score)) %>%
  inner_join(dateframe4) %>%
  mutate(HighBarrier = ifelse(Score >= 12, 1, 0)) %>%
  select(-c(VIDate, diff))

## Determine average number of first-time system users entering each month
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

vifth <- dateframe6 %>%
  filter(FTH == 1 | ProgramType == "Prevention",
         !is.na(HighBarrier)) %>%
  group_by(Disabled, PreventionEligible) %>%
  summarise(HighBarrierRate = mean(HighBarrier))

fthtable <- dateframe6 %>%
  filter(FTH == 1 | ProgramType == "Prevention") %>%
  mutate(EntryMonth = as.Date(paste(year(Entry), month(Entry), "01", sep = "-"))) %>%
  group_by(Disabled, EntryMonth) %>%
  summarise(FTH = n(),
            PreventionEligible = sum(PreventionEligible)) %>%
  filter(EntryMonth < as.Date(paste(year(today()), month(today()), "01", sep = "-")),
         EntryMonth >= as.Date(paste(year(today())-4, month(today()), "01", sep = "-"))) %>%
  group_by(Disabled) %>%
  summarise(FTH = round(mean(FTH), 0),
            PreventionEligible = round(mean(PreventionEligible), 0)) %>%
  mutate(NotEligible = FTH-PreventionEligible) %>%
  melt(id.vars = "Disabled") %>%
  mutate(PreventionEligible = ifelse(variable == "PreventionEligible", 1, 0)) %>%
  filter(variable != "FTH") %>%
  rename(FTH = value) %>%
  select(-variable) %>%
  inner_join(vifth) %>%
  mutate(HighBarrier = FTH*HighBarrierRate,
         LowBarrier = FTH-HighBarrier) %>%
  select(-c(HighBarrierRate, FTH)) %>%
  melt(id.vars = c("Disabled", "PreventionEligible")) %>%
  mutate(HighBarrier = ifelse(variable == "HighBarrier", 1, 0)) %>%
  group_by(Disabled, HighBarrier) %>%
  summarise(FTH = sum(value),
            PreventionEligible = sum(value[PreventionEligible == 1]))

### Determine recidivism rates
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

returnsurv <- Surv(dateframe7$ReturnTime, dateframe7$Return)
returnkmcurves <- survfit(returnsurv~ProgramType+Disabled, dateframe7)
returnstrata <- rep(names(returnkmcurves$strata[1]), as.numeric(returnkmcurves$strata[1]))

returnrateframe <- data.frame(
  Time = 1:as.numeric(returnkmcurves$strata[1]),
  ProgramType = rep(str_remove(strsplit(returnstrata, ",")[[1]][1], "ProgramType="), as.numeric(returnkmcurves$strata[1])),
  Disabled = as.numeric(rep(str_remove(strsplit(returnstrata, ", ")[[1]][2], "Disabled="), as.numeric(returnkmcurves$strata[1])))
)

for (n in 2:length(returnkmcurves$strata)){
  returnstrata <- rep(names(returnkmcurves$strata[n]), as.numeric(returnkmcurves$strata[n]))
  tempframe <- data.frame(Time = 1:as.numeric(returnkmcurves$strata[n]),
                          ProgramType = rep(str_remove(strsplit(returnstrata, ",")[[1]][1], "ProgramType="), 
                                            as.numeric(returnkmcurves$strata[n])),
                          Disabled = as.numeric(rep(str_remove(strsplit(returnstrata, ", ")[[1]][2], "Disabled="), 
                                                    as.numeric(returnkmcurves$strata[n]))))
  returnrateframe <<- rbind(returnrateframe, tempframe)
}

returnrateframe$Risk <- returnkmcurves$n.risk
returnrateframe$Returned <- returnkmcurves$n.event

returnrates <- returnrateframe %>%
  group_by(ProgramType, Disabled) %>%
  summarise(Risk = sum(Risk),
            Returned = sum(Returned)) %>%
  mutate(ReturnRate = Returned/Risk)

##Determine what proportion of clients in each program type are high barrier
hb <- filter(dateframe7, !is.na(HighBarrier))
hbrate <- hb %>%
  group_by(ProgramType, Disabled) %>%
  summarise(HBRate = mean(HighBarrier)) %>%
  filter(ProgramType != "PSH")

hbreturnsurv <- Surv(hb$ReturnTime, hb$Return)
hbreturncurves <- survfit(hbreturnsurv~ProgramType+Disabled+HighBarrier, hb)
hbreturnstrata <- rep(names(hbreturncurves$strata[1]), as.numeric(hbreturncurves$strata[1]))

hbreturnrateframe <- data.frame(
  Time = 1:as.numeric(hbreturncurves$strata[1]),
  ProgramType = rep(str_remove(strsplit(hbreturnstrata, ",")[[1]][1], "ProgramType="), as.numeric(hbreturncurves$strata[1])),
  Disabled = as.numeric(rep(str_remove(strsplit(hbreturnstrata, ", ")[[1]][2], "Disabled="), as.numeric(hbreturncurves$strata[1]))),
  HighBarrier = as.numeric(rep(str_remove(strsplit(hbreturnstrata, ", ")[[1]][3], "HighBarrier="), as.numeric(hbreturncurves$strata[1])))
)

for (p in 2:length(hbreturncurves$strata)){
  hbreturnstrata <- rep(names(hbreturncurves$strata[p]), as.numeric(hbreturncurves$strata[p]))
  tempframe <- data.frame(Time = 1:as.numeric(hbreturncurves$strata[p]),
                          ProgramType = rep(str_remove(strsplit(hbreturnstrata, ",")[[1]][1], "ProgramType="), 
                                            as.numeric(hbreturncurves$strata[p])),
                          Disabled = as.numeric(rep(str_remove(strsplit(hbreturnstrata, ", ")[[1]][2], "Disabled="), 
                                                    as.numeric(hbreturncurves$strata[p]))),
                          HighBarrier = as.numeric(rep(str_remove(strsplit(hbreturnstrata, ", ")[[1]][3], "HighBarrier="))))
  hbreturnrateframe <<- rbind(hbreturnrateframe, tempframe)
}

hbreturnrateframe$Risk <- hbreturncurves$n.risk
hbreturnrateframe$Returned <- hbreturncurves$n.event

hbreturnrates <- hbreturnrateframe %>%
  group_by(ProgramType, Disabled, HighBarrier) %>%
  summarise(Risk = sum(Risk),
            Returned = sum(Returned)) %>%
  mutate(ReturnRate = Returned/Risk)

lbreturnrate <- filter(hbreturnrates, HighBarrier == 0) %>%
  rename(LowBarrierRate = ReturnRate) %>%
  select(ProgramType, Disabled, LowBarrierRate)

hbreturnratio <- filter(hbreturnrates, HighBarrier == 1) %>%
  rename(HighBarrierRate = ReturnRate) %>%
  select(ProgramType, Disabled, HighBarrierRate) %>%
  full_join(lbreturnrate) %>%
  filter(ProgramType != "PSH") %>%
  mutate(HBRatio = HighBarrierRate/LowBarrierRate,
         HBRatio = ifelse(is.na(HBRatio), 1, HBRatio)) %>%
  select(ProgramType, Disabled, HBRatio)

returntable <- select(returnrates, ProgramType, Disabled, ReturnRate) %>%
  left_join(hbreturnratio) %>%
  left_join(hbrate) %>%
  mutate(LowRate = ReturnRate/((1-HBRate)+(HBRate*HBRatio)),
         HighRate = ifelse(ProgramType == "PSH", ReturnRate, LowRate*HBRatio)) %>%
  select(ProgramType, Disabled, LowRate, HighRate) %>%
  melt(id.vars = c("ProgramType", "Disabled")) %>%
  filter(!is.na(value)) %>%
  mutate(HighBarrier = ifelse(variable == "LowRate", 0, 1)) %>%
  rename(ReturnRate = value) %>%
  select(-variable)

### Determine exit rates
dateframe7$ExitTime <- as.numeric(dateframe7$Exit-dateframe7$Entry)%/%30
dateframe7$ExitTime[is.na(dateframe7$ExitTime)] <- as.numeric(today()-dateframe7$Entry[is.na(dateframe7$Exit)])%/%30

exitsurv <- Surv(dateframe7$ExitTime, dateframe7$SR)
exitkmcurves <- survfit(exitsurv~ProgramType+Disabled, dateframe7)
exitstrata <- rep(names(exitkmcurves$strata[1]), as.numeric(exitkmcurves$strata[1]))

exitrateframe <- data.frame(
  Time = 1:as.numeric(exitkmcurves$strata[1]),
  ProgramType = rep(str_remove(strsplit(exitstrata, ",")[[1]][1], "ProgramType="), as.numeric(exitkmcurves$strata[1])),
  Disabled = as.numeric(rep(str_remove(strsplit(exitstrata, ", ")[[1]][2], "Disabled="), as.numeric(exitkmcurves$strata[1])))
)

for (q in 2:length(returnkmcurves$strata)){
  exitstrata <- rep(names(exitkmcurves$strata[q]), as.numeric(exitkmcurves$strata[q]))
  tempframe <- data.frame(Time = 1:as.numeric(exitkmcurves$strata[q]),
                          ProgramType = rep(str_remove(strsplit(exitstrata, ",")[[1]][1], "ProgramType="), 
                                            as.numeric(exitkmcurves$strata[q])),
                          Disabled = as.numeric(rep(str_remove(strsplit(exitstrata, ", ")[[1]][2], "Disabled="), 
                                                    as.numeric(exitkmcurves$strata[q]))))
  exitrateframe <<- rbind(exitrateframe, tempframe)
}

exitrateframe$Risk <- exitkmcurves$n.risk
exitrateframe$Exited <- exitkmcurves$n.event

exitrates <- exitrateframe %>%
  group_by(ProgramType, Disabled) %>%
  summarise(Risk = sum(Risk),
            Exited = sum(Exited)) %>%
  mutate(ExitRate = Exited/Risk)

hbexit <- filter(dateframe7, !is.na(HighBarrier)) %>%
  mutate(HighBarrier = ifelse(ProgramType == "PSH", 1, HighBarrier))
hbexitsurv <- Surv(hbexit$ExitTime, hbexit$SR)
hbexitcurves <- survfit(hbexitsurv~ProgramType+Disabled+HighBarrier, hbexit)
hbexitstrata <- rep(names(hbexitcurves$strata[1]), as.numeric(hbexitcurves$strata[1]))

hbexitrateframe <- data.frame(
  Time = 1:as.numeric(hbexitcurves$strata[1]),
  ProgramType = rep(str_remove(strsplit(hbexitstrata, ",")[[1]][1], "ProgramType="), as.numeric(hbexitcurves$strata[1])),
  Disabled = as.numeric(rep(str_remove(strsplit(hbexitstrata, ", ")[[1]][2], "Disabled="), as.numeric(hbexitcurves$strata[1]))),
  HighBarrier = as.numeric(rep(str_remove(strsplit(hbexitstrata, ", ")[[1]][3], "HighBarrier="), as.numeric(hbexitcurves$strata[1])))
)

for (r in 2:length(hbexitcurves$strata)){
  hbexitstrata <- rep(names(hbexitcurves$strata[r]), as.numeric(hbexitcurves$strata[r]))
  tempframe <- data.frame(Time = 1:as.numeric(hbexitcurves$strata[r]),
                          ProgramType = rep(str_remove(strsplit(hbexitstrata, ",")[[1]][1], "ProgramType="), 
                                            as.numeric(hbexitcurves$strata[r])),
                          Disabled = as.numeric(rep(str_remove(strsplit(hbexitstrata, ", ")[[1]][2], "Disabled="), 
                                                    as.numeric(hbexitcurves$strata[r]))),
                          HighBarrier = as.numeric(rep(str_remove(strsplit(hbexitstrata, ", ")[[1]][3], "HighBarrier="), as.numeric(hbexitcurves$strata[r]))))
  hbexitrateframe <<- rbind(hbexitrateframe, tempframe)
}

hbexitrateframe$Risk <- hbexitcurves$n.risk
hbexitrateframe$Exited <- hbexitcurves$n.event

hbexitrates <- hbexitrateframe %>%
  group_by(ProgramType, Disabled, HighBarrier) %>%
  summarise(Risk = sum(Risk),
            Exited = sum(Exited)) %>%
  mutate(ExitRate = Exited/Risk)

lbexitrate <- filter(hbexitrates, HighBarrier == 0) %>%
  rename(LowBarrierRate = ExitRate) %>%
  select(ProgramType, Disabled, LowBarrierRate)

hbexitratio <- filter(hbexitrates, HighBarrier == 1) %>%
  rename(HighBarrierRate = ExitRate) %>%
  select(ProgramType, Disabled, HighBarrierRate) %>%
  full_join(lbexitrate) %>%
  filter(ProgramType != "PSH") %>%
  mutate(HBRatio = HighBarrierRate/LowBarrierRate,
         HBRatio = ifelse(is.na(HBRatio), 1, HBRatio)) %>%
  select(ProgramType, Disabled, HBRatio)

exittable <- select(exitrates, ProgramType, Disabled, ExitRate) %>%
  left_join(hbexitratio) %>%
  left_join(hbrate) %>%
  mutate(LowRate = ExitRate/((1-HBRate)+(HBRate*HBRatio)),
         HighRate = ifelse(ProgramType == "PSH", ExitRate, LowRate*HBRatio)) %>%
  select(ProgramType, Disabled, LowRate, HighRate) %>%
  melt(id.vars = c("ProgramType", "Disabled")) %>%
  filter(!is.na(value)) %>%
  mutate(HighBarrier = ifelse(variable == "LowRate", 0, 1)) %>%
  rename(ExitRate = value) %>%
  select(-variable)

## Count the number of people active in each program type
active <- filter(dateframe7, is.na(Exit)) %>%
  mutate(HighBarrier = ifelse(ProgramType == "PSH", 1, HighBarrier)) %>%
  group_by(ProgramType, Disabled, HighBarrier) %>%
  summarise(Active = n())
knownhb <- filter(active, !is.na(HighBarrier))

activetable <- filter(active, is.na(HighBarrier)) %>%
  inner_join(hbrate) %>%
  mutate(HighBarrier = Active*HBRate,
         LowBarrier = Active-HighBarrier) %>%
  select(-c(Active, HBRate)) %>%
  melt(id.vars = c("ProgramType", "Disabled")) %>%
  mutate(HighBarrier = ifelse(variable == "HighBarrier", 1, 0)) %>%
  select(-variable) %>%
  full_join(knownhb) %>%
  mutate(value = ifelse(is.na(value), 0, value),
         Active = ifelse(is.na(Active), 0, Active),
         Active = Active+value) %>%
  select(-value)

enrolled <- filter(dateframe7, is.na(Exit))

## Count the number of inactive clients who could recidivate
inactive <- anti_join(dateframe7, enrolled, by = "PersonalID") %>%
  group_by(PersonalID) %>%
  summarise(Exit = max(Exit)) %>%
  inner_join(dateframe7) %>%
  filter(ReturnTime < 24)

inactivetable <- inactive %>%
  group_by(ProgramType, Disabled) %>%
  summarise(Inactive = n()) %>%
  full_join(hbrate) %>%
  mutate(HBRate = ifelse(is.na(HBRate), 1, HBRate),
         HighBarrier = Inactive*HBRate,
         LowBarrier = Inactive-HighBarrier) %>%
  select(ProgramType, Disabled, HighBarrier, LowBarrier) %>%
  melt(id.vars = c("ProgramType", "Disabled")) %>%
  filter(value > 0) %>%
  mutate(HighBarrier = ifelse(variable == "HighBarrier", 1, 0)) %>%
  rename(Inactive = value) %>%
  select(-variable)

projects <- read_excel("C:/Users/aflicker/Strategies To End Homelessness, Inc/CommonFiles - CommonFiles/CONTINUUM OF CARE FILES/Grants Yearly Work & Applications/2021 CoC/Scoring/Scorecard 2021.xlsx",
                   sheet = "projects",
                   col_types = c("text", rep("skip", 12), "numeric", rep("skip", 7)))
colnames(projects) <- c("Program", "Spending")
hmis <- read_excel("C:/Users/aflicker/Strategies To End Homelessness, Inc/CommonFiles - CommonFiles/CONTINUUM OF CARE FILES/Grants Yearly Work & Applications/2021 CoC/Scoring/Scorecard 2021.xlsx",
                   sheet = "HMIS",
                   col_types = c("text", "skip", "numeric", rep("skip", 109), "numeric", "numeric"),
                   col_names = c("Program", NA, "ProgramType", rep(NA, 109), "Individuals", "HH"),
                   skip = 1)
cost <- inner_join(projects, hmis) %>%
  mutate(HHSize = Individuals/HH) %>%
  filter(HHSize < 1.5)
clientdates <- mod2 %>%
  filter(Program %in% cost$Program,
         is.na(Exit) | Exit > "2020-07-01",
         Entry < "2021-04-01") %>%
  mutate(FakeEntry = Entry,
         FakeExit = Exit)
clientdates$FakeEntry[clientdates$Entry < "2020-07-01"] <- "2020-07-01"
clientdates$FakeExit[is.na(clientdates$Exit) | clientdates$Exit > "2021-04-01"] <- "2021-04-01"
costtable <- clientdates %>%
  mutate(ClientDays = as.numeric(FakeExit-FakeEntry)) %>%
  group_by(Program, HouseholdID) %>%
  summarise(ClientDays = mean(ClientDays)) %>%
  group_by(Program) %>%
  summarise(ClientDays = sum(ClientDays)) %>%
  filter(ClientDays > 1000) %>%
  inner_join(cost) %>%
  group_by(ProgramType) %>%
  summarise(Spending = sum(Spending),
            ClientDays = sum(ClientDays)) %>%
  mutate(SpendingPerMonth = Spending*30/ClientDays) %>%
  select(ProgramType, SpendingPerMonth)
setwd("C:/Users/aflicker/Strategies To End Homelessness, Inc/CommonFiles - CommonFiles/CONTINUUM OF CARE FILES/Data Analysis/System model/Current/vulnerable/inputs")
write_csv(activetable, "active.csv")
write_csv(inactivetable, "inactive.csv")
write_csv(exittable, "exitrates.csv")
write_csv(returntable, "returnrates.csv")
write_csv(fthtable, "fth.csv")
write_csv(costtable, "cost.csv")
