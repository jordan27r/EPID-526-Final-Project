library(tidyverse)

arrests <- read.csv("C://Users//jorda//OneDrive//Documents//EPID 526//Tucson_Police_Arrests.csv")

arrestsSimp <- arrests %>% 
  select(IncidentNumber, ArrestID) %>% 
  group_by(IncidentNumber) %>%
  distinct() %>%
  summarize(n=n())

samp <- arrests %>% 
  filter(ArrestID == 'AR220008285')

polAct <- read.csv("C://Users//jorda//OneDrive//Documents//EPID 526//Tucson_Police_Activity_2022.csv")

polActSimp <- polAct %>% 
  select(EventID, ESRI_OID) %>% 
  mutate(incID = str_extract(EventID, "[:digit:]{9,10}"))


polRec <- read.csv("C://Users//jorda//OneDrive//Documents//EPID 526//Tucson_Police_Incidents_-_2022_-_Open_Data.csv")

polRecSimp <- polRec %>% 
  select(INCI_ID, PrimaryKey)

# polRec2 <- polRec %>% 
#   filter(WEAPON_CATEGORY == "FIREARM")
# 
# polAct2 <- polAct %>% 
#   mutate(incID = str_extract(EventID, "[:digit:]{9,10}")) %>% 
#   filter(incID %in% polRec$INCI_ID)
# dat <- arrests %>% 
#   filter(ArrestYear == 2022) %>% 
#   left_join(polAct, by=join_by(ESRI_OID)) %>% 
#   filter(EventType %in% c("Weapon Offense")) %>% 
#   filter(ArrestChargeDescription %in% c("MISCONDUCT INVOLVING WEAPONS","AGG ASSAULT- DISFIGUREMENT (DV)"))

arrestType <- arrests %>% 
  filter(ArrestYear == 2022) %>% 
  # filter(str_detect(ArrestChargeDescription, "FIREARM|GUN|DEADLY|ASLT|ASSAULT|HOMICIDE|MURDER")) %>% 
  select(IncidentNumber, ArrestID, ArrestDate, ArrestCharge, ArrestChargeDescription,Severity, Age, AgeGroup, ArresteeSex, ArresteeRace, ArresteeEthnicity) %>% 
  distinct()

polRecFilt <- polRec %>% 
  select(INCI_ID, WARD, UCRsummary, STATUTDESC, WEAPON1DESC,WEAPON2DESC, WEAPON_CATEGORY, Crime, CrimeType, CrimeCategory, DATE_REPT) %>% 
  filter(!WEAPON1DESC %in% c("NONE", ""))

allDat <- polRecFilt %>% 
  left_join(arrestType, by=join_by(INCI_ID == IncidentNumber)) %>% 
  distinct() %>% 
  filter(WEAPON_CATEGORY == "FIREARM")

write.csv(allDat, "polRecArrestData.csv")
