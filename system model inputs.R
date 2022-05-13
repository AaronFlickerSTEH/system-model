library(tidyverse)
library(lubridate)
library(survival)
library(reshape2)
setwd("P:/CONTINUUM OF CARE FILES/Data Analysis/System model/2020 fall")
mod <- read.csv("singles.csv")
psh <- read.csv("psh.csv")
colnames(mod) <- c("EnrollmentID", "PersonalID", "HouseholdID", "Program", "ProgramType", "Veteran", "Entry", "MoveIn", "Exit", "Age", 
                   "ChronicHealth", "Developmental", "HIV", "MentalHealth", "Physical", "SubstanceAbuse", "NightBefore")
colnames(psh) <- c("EnrollmentID", "PersonalID", "Entry", "MoveIn", "Exit", "Veteran", "OldestHH")
mod2 <- mod %>%
  mutate(ProgramType = ifelse(ProgramType == "Homeless Prevention", "Prevention",
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
  filter(is.na(Exit) | Exit >= today()-1461,
         Entry < today(),
         is.na(MoveIn) | MoveIn < today())

###Determine youth status; filter out housing clients without move-in dates
hh <- mod2 %>%
  group_by(HouseholdID) %>%
  summarise(maxage = max(Age)) %>%
  right_join(mod2) %>%
  mutate(Youth = ifelse(maxage < 25, 1, 0),
         NewEntry = Entry) %>%
  filter((ProgramType != "RRH" & ProgramType != "PSH") | !is.na(MoveIn))
hh$NewEntry[!is.na(hh$MoveIn)] <- hh$MoveIn[!is.na(hh$MoveIn)]
hh$Youth[is.na(hh$Youth)] <- 0

###Filter out enrollments with exit dates on or before entry/move-in
mod3 <- hh %>%
  filter(is.na(Exit) | Exit > NewEntry) %>%
  mutate(Entry = NewEntry) %>%
  select(EnrollmentID, PersonalID, ProgramType:Entry, Exit, Disabled:Youth)
mod3$FakeExit <- mod3$Exit
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
  for (j in 1:length(datelist)){
    if (length(datelist[[j]]) > 0){
      entries <- min(datelist[[j]])
      alldates <- seq.Date(entries, today(), "days")
      nondates <- alldates[alldates %in% datelist[[j]] == FALSE]
      exits <- min(nondates)
      while (length(datelist[[j]]) > 0){
        datelist[[j]] <- datelist[[j]][datelist[[j]] > max(exits)]
        entries <- append(entries, min(datelist[[j]]))
        nondates <- nondates[nondates > max(entries)]
        exits <- append(exits, min(nondates))
      }
      tempframe <- data.frame(PersonalID = people[i],
                              ProgramType = types[j],
                              Entry = entries,
                              Exit = exits)
      tempframe <- filter(tempframe, !is.infinite(Entry))
      dateframe <<- rbind(dateframe, tempframe)
    }
  }
}
dateframe <- filter(dateframe, !is.na(dateframe$PersonalID))
dateframe$Exit[dateframe$Exit == today()] <- NA

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
notdone <- filter(dateframenewer, is.na(Youth) | is.na(PreventionEligible)) %>%
  left_join(youth, by = "PersonalID") %>%
  mutate(diff = abs(as.numeric(Entry.x-Entry.y)))

mindiff <- notdone %>%
  group_by(PersonalID, Entry.x) %>%
  summarise(diff = min(diff)) %>%
  inner_join(notdone) %>%
  mutate(Entry = Entry.x,
         Youth = Youth.y,
         PreventionEligible = PreventionEligible.y) %>%
  select(PersonalID, Entry, ProgramType:Veteran, Youth, PreventionEligible)
mindiff <- unique(mindiff)

dateframenewest <- rbind(done, data.frame(mindiff))

secondentry <- dateframenewest %>%
  select(PersonalID:Entry)

dtr <- inner_join(dateframenewest, secondentry, by = "PersonalID") %>%
  filter(Entry.y >= Exit,
         !is.na(Exit)) %>%
  mutate(DTR = as.numeric(Entry.y-Exit)) %>%
  filter(DTR <= 14,
         ProgramType.x != ProgramType.y,
         ProgramType.y != "Homeless")

dateframenewest2 <- dtr %>%
  group_by(PersonalID, Exit) %>%
  summarise(DTR = min(DTR)) %>%
  inner_join(dtr) %>%
  mutate(SecondEntry = ProgramType.y) %>%
  select(PersonalID, Exit, SecondEntry) %>%
  right_join(dateframenewest) %>%
  mutate(SecondEntry = ifelse(is.na(SecondEntry), "No", SecondEntry))

hl <- filter(dateframenewest2, ProgramType == "Homeless") %>%
  select(PersonalID, Entry, Exit)
fth <- filter(hl, Entry >= today()-730)
fth2 <- inner_join(fth, hl, by = "PersonalID") %>%
  filter(Exit.y < Entry.x) %>%
  mutate(DTR = as.numeric(Entry.x-Exit.y)) %>%
  filter(DTR <= 730) %>%
  mutate(FTH = 0,
         Entry = Entry.x) %>%
  select(PersonalID, Entry, FTH)
fth2 <- unique(fth2)
dateframenewest3 <- left_join(fth, fth2) %>%
  mutate(FTH = ifelse(is.na(FTH), 1, FTH)) %>%
  select(-Exit) %>%
  right_join(dateframenewest2)

dateframenewest4 <- inner_join(dateframenewest3, hl, by = "PersonalID") %>%
  filter(!is.na(Exit.x),
         Entry.y >= Exit.x) %>%
  mutate(DTR = as.numeric(Entry.y-Exit.x)) %>%
  filter(DTR <= 730) %>%
  group_by(PersonalID, Exit.x) %>%
  summarise(DTR = min(DTR)) %>%
  mutate(Exit = Exit.x) %>%
  select(-Exit.x) %>%
  right_join(dateframenewest3) %>%
  mutate(Return = ifelse(is.na(Exit), NA,
                         ifelse(is.na(DTR), 0, 1)),
         ReturnTime = ifelse(Return == 1, DTR%/%30,
                             ifelse(is.na(Return), NA, as.numeric(today()-Exit)%/%30))) %>%
  select(-DTR) %>%
  mutate(Disabled = ifelse(ProgramType == "PSH", 1, Disabled),
         ClientPop = ifelse(Veteran == 1, "Veteran",
                             ifelse(Youth == 1, "Youth", "Adult")),
         SR = ifelse(is.na(Exit), NA,
                     ifelse(SecondEntry == "No" | ProgramType != "Homeless", 1, 0)),
         ReturnTime = ifelse(ReturnTime > 24, 24, ReturnTime))

months <- seq.Date(as.Date(paste(year(today())-4, month(today()), "01", sep = "-")), by = "month", length.out = 49)

exitframe <- data.frame(ProgramType = c(rep("PSH", 288), rep("RRH", 288), rep("TH", 288), rep("Prevention", 288), rep("Homeless", 288)),
                        ClientPop = rep(c(rep("Adult", 96), rep("Veteran", 96), rep("Youth", 96)), 5),
                        Disabled = rep(rep(c(rep(1, 48), rep(0, 48)), 3), 5),
                        MonthStart = rep(months[1:48], 30),
                        MonthEnd = rep(months[2:49], 30),
                        Enrolled = NA,
                        Exited = NA)


for (i in 1:nrow(exitframe)){
  exitframe$Enrolled[i] <- sum(dateframenewest4$ProgramType == exitframe$ProgramType[i] &
                                 dateframenewest4$ClientPop == exitframe$ClientPop[i] &
                                 dateframenewest4$Disabled == exitframe$Disabled[i] &
                                 dateframenewest4$Entry < exitframe$MonthEnd[i] &
                                 (is.na(dateframenewest4$Exit) | dateframenewest4$Exit >= exitframe$MonthStart[i]))
  
  exitframe$Exited[i] <- sum(dateframenewest4$SR[dateframenewest4$ProgramType == exitframe$ProgramType[i] &
                                                   dateframenewest4$ClientPop == exitframe$ClientPop[i] &
                                                   dateframenewest4$Disabled == exitframe$Disabled[i] &
                                                   !is.na(dateframenewest4$Exit) &
                                                   dateframenewest4$Exit >= exitframe$MonthStart[i] &
                                                   dateframenewest4$Exit < exitframe$MonthEnd[i]])
}

exitrates <- exitframe %>%
  group_by(ProgramType, ClientPop, Disabled) %>%
  summarise(Enrolled = sum(Enrolled),
            Exited = sum(Exited)) %>%
  mutate(ExitRate = Exited/Enrolled) %>%
  filter(Enrolled > 0) %>%
  select(-c(Enrolled, Exited))

active <- filter(dateframenewest4, is.na(Exit))
  
activetable <- dateframenewest4 %>%
  filter(is.na(Exit)) %>%
  group_by(ProgramType, Disabled) %>%
  summarise(Veteran = sum(Veteran),
            Youth = sum(Youth),
            Adult = n()-Veteran-Youth) %>%
  melt(id.vars = c("ProgramType", "Disabled")) %>%
  mutate(Active = value,
         ClientPop = variable) %>%
  select(-c(value, variable))

inactive <- anti_join(dateframenewest4, active, by = "PersonalID") %>%
  group_by(PersonalID) %>%
  summarise(Exit = max(Exit)) %>%
  inner_join(dateframenewest4) %>%
  filter(ReturnTime < 24)

inactivetable <- inactive %>%
  group_by(ProgramType, Disabled, ClientPop) %>%
  summarise(Inactive = n())

fth <- dateframenewest4 %>%
  filter(FTH == 1 | ProgramType == "Prevention") %>%
  mutate(EntryMonth = as.Date(paste(year(Entry), month(Entry), "01", sep = "-"))) %>%
  group_by(Disabled, ClientPop, EntryMonth) %>%
  summarise(FTH = n(),
            PreventionEligible = sum(PreventionEligible)) %>%
  filter(EntryMonth < as.Date(paste(year(today()), month(today()), "01", sep = "-")),
         EntryMonth >= as.Date(paste(year(today())-2, month(today()), "01", sep = "-"))) %>%
  group_by(Disabled, ClientPop) %>%
  summarise(FTH = round(mean(FTH), 0),
            PreventionEligible = round(mean(PreventionEligible), 0))

returnsurv <- Surv(dateframenewest4$ReturnTime, dateframenewest4$Return)
kmcurves <- survfit(returnsurv~ProgramType+ClientPop+Disabled, dateframenewest4)

strata <- rep(names(kmcurves$strata[1]), as.numeric(kmcurves$strata[1]))
returntimes <- 1:as.numeric(kmcurves$strata[1])
for (k in 2:length(kmcurves$strata)){
  strata <- append(strata, rep(names(kmcurves$strata[k]), as.numeric(kmcurves$strata[k])))
  returntimes <- append(returntimes, 1:as.numeric(kmcurves$strata[k]))
}

returnrates <- data.frame(Strata = strata,
                          Risk = kmcurves$n.risk,
                          Return = kmcurves$n.event,
                          ProgramType = c(rep("Homeless", 150), rep("Prevention", 69), rep("PSH", 66), rep("RRH", 141), rep("TH", 120)),
                          ClientPop = c(rep("Adult", 50), rep("Veteran", 50), rep("Youth", 50),
                                         rep("Adult", 15), rep("Veteran", 27), rep("Youth", 27),
                                         rep("Adult", 25), rep("Veteran", 17), rep("Youth", 24),
                                         rep("Adult", 50), rep("Veteran", 49), rep("Youth", 42),
                                         rep("Adult", 40), rep("Veteran", 50), rep("Youth", 30)),
                          Disabled = c(rep(0, 25), rep(1, 25),
                                       rep(0, 25), rep(1, 25),
                                       rep(0, 25), rep(1, 25),
                                       rep(0, 9), rep(1, 6),
                                       rep(0, 15), rep(1, 12),
                                       rep(0, 14), rep(1, 79),
                                       rep(0, 25), rep(1, 25),
                                       rep(0, 24), rep(1, 25),
                                       rep(0, 20), rep(1, 22),
                                       rep(0, 15), rep(1, 25),
                                       rep(0, 25), rep(1, 25),
                                       rep(0, 15), rep(1, 15)),
                          ReturnTime = returntimes) %>%
  group_by(ProgramType, Disabled, ClientPop) %>%
  filter(ReturnTime < 25) %>%
  summarise(Risk = sum(Risk),
            Return = sum(Return)) %>%
  mutate(ReturnRate = Return/Risk)

write.csv(activetable, "P:/CONTINUUM OF CARE FILES/Data Analysis/System model/2020 fall/inputs/active.csv", row.names = FALSE)
write.csv(inactivetable, "P:/CONTINUUM OF CARE FILES/Data Analysis/System model/2020 fall/inputs/inactive.csv", row.names = FALSE)
write.csv(exitrates, "P:/CONTINUUM OF CARE FILES/Data Analysis/System model/2020 fall/inputs/exitrates.csv", row.names = FALSE)
write.csv(returnrates, "P:/CONTINUUM OF CARE FILES/Data Analysis/System model/2020 fall/inputs/returnrates.csv", row.names = FALSE)
write.csv(fth, "P:/CONTINUUM OF CARE FILES/Data Analysis/System model/2020 fall/inputs/fth.csv", row.names = FALSE)
