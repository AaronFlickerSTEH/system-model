library(tidyverse)
library(lubridate)
library(survival)
library(reshape2)
library(readxl)
setwd("C:/Users/aflicker/Strategies To End Homelessness, Inc/CommonFiles - CommonFiles/CONTINUUM OF CARE FILES/Data Analysis/System model/Current/CoC/RRH correction")

mod <- read.csv("singles.csv", col.names = c("EnrollmentID", "PersonalID", "HouseholdID", "Program", "ProgramType", "Veteran", "Entry", 
                                             "MoveIn", "Exit", "Age", "ChronicHealth", "Developmental", "HIV", "MentalHealth", "Physical", 
                                             "SubstanceAbuse", "NightBefore"))

##Filters out veterans, housing clients without move-in dates, KEYS clients, eviction prevention clients, winter shelters, 
## post-dated move-ins, exits more than four years old; determines disability status; determines eligibility for prevention based on
## previous location; changes entry date to move-in date for housing programs
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
         Program %in% c("TAL HC Rental Assistance Program", "IHN Aftercare", "BHS ESG-CV Aftercare", "BHS TANF RRH", "CGM Winter Shelter",
                        "TAL - POP Winter Shelter", "BHS KEYS - OH0598", "IHN KEYS - OH0598", "LYS KEYS - OH0598", "TAL RAP - TANF",
                        "SA KEYS - OH0598") == FALSE,
         (ProgramType != "RRH" & ProgramType != "PSH") | !is.na(MoveIn),
         Veteran != "Yes") %>%
  mutate(Program = factor(Program),
         NewEntry = Entry)
mod2$NewEntry[!is.na(mod2$MoveIn)] <- mod2$MoveIn[!is.na(mod2$MoveIn)]

###Filter out unaccompanied youths
mod3 <- mod2 %>%
  group_by(HouseholdID) %>%
  summarise(maxage = max(Age)) %>%
  right_join(mod2) %>%
  filter(maxage > 24) %>%
  select(-maxage)

###filter out housing clients without move-in dates
mod4 <- mod3 %>%
  filter(is.na(Exit) | Exit > NewEntry) %>%
  mutate(Entry = NewEntry) %>%
  select(EnrollmentID, PersonalID, ProgramType:Entry, Exit, Disabled, PreventionEligible)
mod4$FakeExit <- mod4$Exit
mod4$FakeExit[is.na(mod4$FakeExit)] <- today()

###Determine date ranges during which each client was enrolled in each program type
dates <- select(mod4, PersonalID, ProgramType, Entry, FakeExit)
people <- unique(dates$PersonalID)
types <- c("PSH", "RRH", "TH", "Prevention", "Homeless")
dateframe <- data.frame(PersonalID = NA,
                        ProgramType = NA,
                        Entry = as.Date(NA),
                        Exit = as.Date(NA))

for (i in people){
  datelist <- as.list(rep(NA, length(types)))
  clientframe <- filter(dates, PersonalID == i)
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
      tempframe <- data.frame(PersonalID = i,
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

## Resolve discrepancies in disability/prevention eligibility for each entry
disab <- mod4 %>%
  group_by(PersonalID, Entry) %>%
  summarise(Disabled = max(Disabled),
            PreventionEligible = min(PreventionEligible))

dateframe2 <- left_join(dateframe, disab)

## Determine disability status for entries without one
disab2 <- filter(dateframe2, is.na(Disabled)) %>%
  select(-c(Disabled, PreventionEligible)) %>%
  left_join(disab, by = "PersonalID") %>%
  mutate(diff = abs(as.numeric(Entry.x-Entry.y)))
disab3 <- disab2 %>%
  group_by(PersonalID, Entry.x) %>%
  summarise(diff = min(diff)) %>%
  inner_join(disab2) %>%
  ungroup() %>%
  mutate(Entry = Entry.x) %>%
  select(PersonalID, ProgramType, Exit, Disabled:Entry)
dateframe3 <- anti_join(dateframe2, disab3, by = c("PersonalID", "Entry")) %>%
  rbind(disab3)

### Determine which program stays resulted in entries into housing programs
secondentry <- dateframe3 %>%
  select(PersonalID:Entry)

dtr <- inner_join(dateframe3, secondentry, by = "PersonalID") %>%
  filter(Entry.y >= Exit,
         !is.na(Exit)) %>%
  mutate(DTR = as.numeric(Entry.y-Exit)) %>%
  filter(DTR <= 14,
         ProgramType.x != ProgramType.y,
         ProgramType.y != "Homeless")

dateframe4 <- dtr %>%
  group_by(PersonalID, Exit) %>%
  summarise(DTR = min(DTR)) %>%
  inner_join(dtr) %>%
  rename(SecondEntry = ProgramType.y) %>%
  select(PersonalID, Exit, SecondEntry) %>%
  right_join(dateframe3) %>%
  mutate(SecondEntry = ifelse(is.na(SecondEntry), "No", SecondEntry))

## Determine which entries to homelessness are first for the client within two years
hl <- filter(dateframe4, ProgramType == "Homeless")
forfth <- filter(dateframe4, ProgramType != "Prevention") %>%
  select(PersonalID, Entry, Exit)

fth2 <- inner_join(hl, forfth, by = "PersonalID") %>%
  filter(Entry.y < Entry.x,
         is.na(Exit.y) | as.numeric(Entry.x-Exit.y) <= 730) %>%
  mutate(FTH = 0,
         Entry = Entry.x,
         Exit = Exit.x) %>%
  select(PersonalID, SecondEntry, ProgramType, Disabled:PreventionEligible, FTH:Exit)
fth2 <- unique(fth2)

dateframe5 <- anti_join(dateframe4, fth2, by = c("PersonalID", "Entry")) %>%
  mutate(FTH = ifelse(ProgramType == "Homeless", 1, 0)) %>%
  rbind(fth2)

### Determine which clients returned to homelessness and after how many 30-day periods that return occurred;
## Set status to disabled for PSH clients; determine which homeless clients self-resolved, i.e., exited without entering a housing program
dateframe6 <- inner_join(dateframe5, hl, by = "PersonalID") %>%
  filter(!is.na(Exit.x),
         Entry.y >= Exit.x) %>%
  mutate(DTR = as.numeric(Entry.y-Exit.x)) %>%
  filter(DTR <= 730) %>%
  group_by(PersonalID, Exit.x) %>%
  summarise(DTR = min(DTR)) %>%
  mutate(Exit = Exit.x) %>%
  select(-Exit.x) %>%
  right_join(dateframe5) %>%
  mutate(Return = ifelse(is.na(Exit), NA,
                         ifelse(is.na(DTR), 0, 1)),
         ReturnTime = ifelse(Return == 1, DTR%/%30,
                             ifelse(is.na(Return), NA, as.numeric(today()-Exit)%/%30))) %>%
  select(-DTR) %>%
  mutate(Disabled = ifelse(ProgramType == "PSH", 1, Disabled),
         SR = ifelse(is.na(Exit), NA,
                     ifelse(SecondEntry == "No" | ProgramType != "Homeless", 1, 0)),
         ReturnTime = ifelse(ReturnTime > 24, 24, ReturnTime))

## Determine number of clients active currently in each program type
activelist <- filter(dateframe6, is.na(Exit))

## Determine clients who have exited and may return to homelessness
inactivelist <- anti_join(dateframe6, activelist, by = "PersonalID") %>%
  group_by(PersonalID) %>%
  summarise(Exit = max(Exit)) %>%
  inner_join(dateframe6) %>%
  filter(ReturnTime < 24)

## Determine rates of recidivism
forreturn <- filter(dateframe6, SR == 1)
returnsurv <- Surv(forreturn$ReturnTime, forreturn$Return)
returnkmcurves <- survfit(returnsurv~ProgramType+Disabled, forreturn)
returnstrata <- rep(names(returnkmcurves$strata[1]), as.numeric(returnkmcurves$strata[1]))

returnframe <- data.frame(Time = 1:as.numeric(returnkmcurves$strata[1]),
                          ProgramType = rep(str_remove(strsplit(returnstrata, ",")[[1]][1], "ProgramType="), 
                                            as.numeric(returnkmcurves$strata[1])),
                          Disabled = as.numeric(rep(str_remove(strsplit(returnstrata, ", ")[[1]][2], "Disabled="), 
                                                    as.numeric(returnkmcurves$strata[1]))))

for (m in 2:length(returnkmcurves$strata)){
  returnstrata <- rep(names(returnkmcurves$strata[m]), as.numeric(returnkmcurves$strata[m]))
  tempreturnframe <- data.frame(Time = 1:as.numeric(returnkmcurves$strata[m]),
                                ProgramType = rep(str_remove(strsplit(names(returnkmcurves$strata)[m], ",")[[1]][1], "ProgramType="), 
                                                  as.numeric(returnkmcurves$strata[m])),
                                Disabled = as.numeric(rep(str_remove(strsplit(names(returnkmcurves$strata)[m], ",")[[1]][2], "Disabled="),
                                                          as.numeric(returnkmcurves$strata[m]))))
  returnframe <<- rbind(returnframe, tempreturnframe)
}
returnframe$Risk <- returnkmcurves$n.risk
returnframe$Return <- returnkmcurves$n.event

dateframe6$SR = ifelse(dateframe6$SR == 1 & !is.na(dateframe6$SR), 1, 0)
dateframe6$ExitTime <- as.numeric(dateframe6$Exit-dateframe6$Entry)%/%30
dateframe6$ExitTime[is.na(dateframe6$ExitTime)] <- as.numeric(today()-dateframe6$Entry[is.na(dateframe6$ExitTime)])%/%30

spending <- read_excel("C:/Users/aflicker/Strategies To End Homelessness, Inc/CommonFiles - CommonFiles/CONTINUUM OF CARE FILES/Grants Yearly Work & Applications/2021 CoC/Scoring/Scorecard 2021.xlsx",
                       sheet = "projects", col_types = c("text", rep("skip", 12), "numeric", rep("skip", 7)), 
                       col_names = c("Program", "Spending"), skip = 1)
hmis <- read_excel("C:/Users/aflicker/Strategies To End Homelessness, Inc/CommonFiles - CommonFiles/CONTINUUM OF CARE FILES/Grants Yearly Work & Applications/2021 CoC/Scoring/Scorecard 2021.xlsx",
                   sheet = "HMIS", col_types = c("text", "skip", "numeric", rep("skip", 109), "numeric", "numeric"), 
                   col_names = c("Program", "ProgramType", "Individuals", "HH"), skip = 1)

spending2 <- inner_join(spending, hmis) %>%
  filter(ProgramType %in% c(3, 13)) %>%
  mutate(ProgramType = ifelse(ProgramType == 3, "PSH", "RRH"),
         HHSize = Individuals/HH) %>%
  filter(HHSize < 1.5)

clientdates <- mod2 %>%
  filter(Program %in% spending2$Program,
         is.na(Exit) | Exit > "2020-07-01",
         Entry < "2021-04-01") %>%
  mutate(FakeEntry = Entry,
         FakeExit = Exit)
clientdates$FakeEntry[clientdates$Entry < "2020-07-01"] <- "2020-07-01"
clientdates$FakeExit[is.na(clientdates$Exit) | clientdates$Exit > "2021-04-01"] <- "2021-04-01"
cost <- clientdates %>%
  mutate(ClientDays = as.numeric(FakeExit-FakeEntry)) %>%
  group_by(Program, HouseholdID) %>%
  summarise(ClientDays = mean(ClientDays)) %>%
  group_by(Program) %>%
  summarise(ClientDays = sum(ClientDays)) %>%
  inner_join(spending2) %>%
  group_by(ProgramType) %>%
  summarise(Spending = sum(Spending),
            ClientDays = sum(ClientDays)) %>%
  mutate(SpendingPerMonth = Spending*30/ClientDays) %>%
  select(ProgramType, SpendingPerMonth)

vi <- read.csv("vispdat.csv")
colnames(vi) <- c("PersonalID", "VIType", "VIDate", "VIScore")
vi2 <- inner_join(dateframe6, vi)
vispdat <- vi2 %>%
  group_by(PersonalID) %>%
  summarise(Entry = max(Entry)) %>%
  inner_join(vi2) %>%
  mutate(VIRange = ifelse(VIScore >= 12, "PSH",
                          ifelse(VIScore >= 4, "RRH", "None"))) %>%
  group_by(Disabled) %>%
  summarise(PSHRate = sum(VIRange == "PSH")/n(),
            RRHRate = sum(VIRange == "RRH")/n())

### Determine number of first-time homeless clients entering each month
fth <- dateframe6 %>%
  filter(FTH == 1 | ProgramType == "Prevention") %>%
  mutate(EntryMonth = as.Date(paste(year(Entry), month(Entry), "01", sep = "-"))) %>%
  group_by(Disabled, EntryMonth) %>%
  summarise(FTH = n(),
            PreventionEligible = sum(PreventionEligible)) %>%
  filter(EntryMonth < as.Date(paste(year(today()), month(today()), "01", sep = "-")),
         EntryMonth >= as.Date(paste(year(today())-1, month(today()), "01", sep = "-"))) %>%
  group_by(Disabled) %>%
  summarise(FTH = round(mean(FTH), 0),
            PreventionEligible = round(mean(PreventionEligible), 0)) %>%
  mutate(ProgramType = "Homeless")
  
## Determine exit rates from programs
forexit <- filter(dateframe6, is.na(Exit) | Exit > today()-365)
exitsurv <- Surv(forexit$ExitTime, forexit$SR)
exitkmcurves <- survfit(exitsurv~ProgramType+Disabled, forexit)
exitstrata <- rep(names(exitkmcurves$strata[1]), as.numeric(exitkmcurves$strata[1]))
  
exitframe <- data.frame(Time = 1:as.numeric(exitkmcurves$strata[1]),
                        ProgramType = rep(str_remove(strsplit(exitstrata, ",")[[1]][1], "ProgramType="), 
                                          as.numeric(exitkmcurves$strata[1])),
                        Disabled = as.numeric(rep(str_remove(strsplit(exitstrata, ", ")[[1]][2], "Disabled="), 
                                                  as.numeric(exitkmcurves$strata[1]))))
  
for (n in 2:length(exitkmcurves$strata)){
  exitstrata <- rep(names(exitkmcurves$strata[n]), as.numeric(exitkmcurves$strata[n]))
  tempexitframe <- data.frame(Time = 1:as.numeric(exitkmcurves$strata[n]),
                              ProgramType = rep(str_remove(strsplit(names(exitkmcurves$strata)[n], ",")[[1]][1], "ProgramType="), 
                                                as.numeric(exitkmcurves$strata[n])),
                              Disabled = as.numeric(rep(str_remove(strsplit(names(exitkmcurves$strata)[n], ",")[[1]][2], "Disabled="),
                                                        as.numeric(exitkmcurves$strata[n]))))
  exitframe <<- rbind(exitframe, tempexitframe)
}
exitframe$Risk <- exitkmcurves$n.risk
exitframe$Exit <- exitkmcurves$n.event
  
active <- activelist %>%
  group_by(ProgramType, Disabled) %>%
  summarise(Active = n())
inactive <- inactivelist %>%
  group_by(ProgramType, Disabled) %>%
  summarise(Inactive = n())
    
returnrates <- returnframe %>%
  group_by(ProgramType, Disabled) %>%
  summarise(Risk = sum(Risk),
            Return = sum(Return)) %>%
  mutate(ReturnRate = Return/Risk) %>%
  select(ProgramType, Disabled, ReturnRate)

exitrates <- exitframe %>%
  group_by(ProgramType, Disabled) %>%
  summarise(Risk = sum(Risk),
            Exit = sum(Exit)) %>%
  mutate(ExitRate = Exit/Risk) %>%
  select(ProgramType, Disabled, ExitRate)

rrh <- inner_join(dateframe6, vi) %>%
  filter(ProgramType == "RRH",
         Disabled == 1) %>%
  mutate(VIRange = ifelse(VIScore > 9, "10+", "<10"),
         FakeExit = Exit)
rrh$FakeExit[is.na(rrh$FakeExit)] <- today()
rrh$Exited <- ifelse(is.na(rrh$Exit), 0, 1)
rrh$ExitTime <- as.numeric(rrh$FakeExit-rrh$Entry)%/%30
rrhreturn <- filter(rrh, !is.na(Exit))
rrhreturnsurv <- Surv(rrhreturn$ReturnTime, rrhreturn$Return)
rrhreturnkmcurves <- survfit(rrhreturnsurv~VIRange, rrhreturn)
rrhreturnstrata <- rep(names(rrhreturnkmcurves$strata[1]), as.numeric(rrhreturnkmcurves$strata[1]))

rrhreturnframe <- data.frame(VIRange = rep(str_remove(strsplit(names(rrhreturnkmcurves$strata)[1], ",")[[1]][1], "VIRange="), 
                                        as.numeric(rrhreturnkmcurves$strata[1])),
                             Time = rrhreturnkmcurves$time[1:as.numeric(rrhreturnkmcurves$strata[1])])
timebreaks <- c(which(rrhreturnkmcurves$time == 0), length(rrhreturnkmcurves$time)+1)

for (k in 2:length(rrhreturnkmcurves$strata)){
  rrhreturnstrata <- rep(str_remove(strsplit(names(rrhreturnkmcurves$strata)[k], ",")[[1]][1], "VIRange="), 
                         as.numeric(rrhreturnkmcurves$strata[k]))
  rrhreturntime <- rrhreturnkmcurves$time[timebreaks[k]:(timebreaks[k+1]-1)]
  rrhtempreturnframe <- data.frame(VIRange = rrhreturnstrata,
                                   Time = rrhreturntime)
  rrhreturnframe <<- rbind(rrhreturnframe, rrhtempreturnframe)
}
rrhreturnframe$Risk <- rrhreturnkmcurves$n.risk
rrhreturnframe$Return <- rrhreturnkmcurves$n.event
rrhreturnshell <- data.frame(VIRange = rep(unique(rrhreturnframe$VIRange), each = 25),
                             Time = 0:24)
rrhreturnframe2 <- left_join(rrhreturnshell, rrhreturnframe) %>%
  mutate(Risk = ifelse(is.na(Risk), lag(Risk), Risk)) %>%
  mutate(Risk = ifelse(is.na(Risk), lag(Risk), Risk)) %>%
  mutate(Risk = ifelse(is.na(Risk), lag(Risk), Risk),
         Return = ifelse(is.na(Return), 0, Return)) %>%
  group_by(VIRange) %>%
  summarise(Risk = sum(Risk),
            Return = sum(Return)) %>%
  mutate(ReturnRate = Return/Risk)

returnmultiplier <- rrhreturnframe2$ReturnRate[rrhreturnframe2$VIRange == "10+"]/rrhreturnframe2$ReturnRate[rrhreturnframe2$VIRange == "<10"]

rrhexitsurv <- Surv(rrh$ExitTime, rrh$Exited)
rrhexitkmcurves <- survfit(rrhexitsurv~VIRange, rrh)
rrhexitstrata <- rep(names(rrhexitkmcurves$strata[1]), as.numeric(rrhexitkmcurves$strata[1]))

rrhexitframe <- data.frame(VIRange = rep(str_remove(strsplit(names(rrhexitkmcurves$strata)[1], ",")[[1]][1], "VIRange="), 
                                         as.numeric(rrhexitkmcurves$strata[1])),
                        Time = rrhexitkmcurves$time[1:as.numeric(rrhexitkmcurves$strata[1])])
exittimebreaks <- c(which(rrhexitkmcurves$time == 0), length(rrhexitkmcurves$time)+1)

for (k in 2:length(rrhexitkmcurves$strata)){
  rrhexitstrata <- rep(str_remove(strsplit(names(rrhexitkmcurves$strata)[k], ",")[[1]][1], "VIRange="), as.numeric(rrhexitkmcurves$strata[k]))
  rrhexittime <- rrhexitkmcurves$time[exittimebreaks[k]:(exittimebreaks[k+1]-1)]
  rrhtempexitframe <- data.frame(VIRange = rrhexitstrata,
                                 Time = rrhexittime)
  rrhexitframe <<- rbind(rrhexitframe, rrhtempexitframe)
}
rrhexitframe$Risk <- rrhexitkmcurves$n.risk
rrhexitframe$Exited <- rrhexitkmcurves$n.event

rrhexitframe2 <- left_join(rrhreturnshell, rrhexitframe)

rrhexitframe3 <- rrhexitframe2 %>%
  filter(!is.na(Risk)) %>%
  group_by(VIRange) %>%
  summarise(maxtime = max(Time)) %>%
  right_join(rrhexitframe2) %>%
  filter(Time <= maxtime) %>%
  mutate(Risk = ifelse(is.na(Risk), lag(Risk), Risk),
         Exited = ifelse(is.na(Exited), 0, Exited)) %>%
  group_by(VIRange) %>%
  summarise(Risk = sum(Risk),
            Exited = sum(Exited)) %>%
  mutate(ExitRate = Exited/Risk)
exitmultiplier <- rrhexitframe3$ExitRate[rrhexitframe3$VIRange == "10+"]/rrhexitframe3$ExitRate[rrhexitframe3$VIRange == "<10"]

rrhcorrection <- data.frame(RRH = sum(active$Active[active$ProgramType == "RRH"]),
                            PSH = active$Active[active$ProgramType == "PSH"],
                            ExitCorrex = exitmultiplier,
                            ReturnCorrex = returnmultiplier)

setwd("C:/Users/aflicker/Strategies To End Homelessness, Inc/CommonFiles - CommonFiles/CONTINUUM OF CARE FILES/Data Analysis/System model/Current/CoC/RRH correction/inputs")
write_csv(active, "active.csv")
write_csv(cost, "cost.csv")
write_csv(exitrates, "exitrates.csv")
write_csv(fth, "fth.csv")
write_csv(inactive, "inactive.csv")
write_csv(returnrates, "returnrates.csv")
write_csv(vispdat, "vispdat.csv")
write_csv(rrhcorrection, "rrhcorrection.csv")

