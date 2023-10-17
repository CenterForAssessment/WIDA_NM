#######################################################################
###
### Data prep script for WIDA NM data
###
########################################################################

### Load packages
require(data.table)
require(openxlsx)
require(foreign)
require(cfaTools)


### Load base files
tmp.2018 <- fread("Data/Base_Files/WIDA, ACCESS Summative SY 2017-18.csv")
tmp.2019 <- fread("Data/Base_Files/WIDA, ACCESS Summative SY 2018-19.csv")
tmp.2020 <- fread("Data/Base_Files/WIDA, ACCESS Summative SY 2019-20.csv")
tmp.2021 <- fread("Data/Base_Files/WIDA, ACCESS Summative SY 2020-21.csv")
tmp.2022 <- fread("Data/Base_Files/WIDA, ACCESS & Alt-ACCESS SY 2021-22 Summative, Stage 2.csv")
tmp.2023 <- as.data.table(openxlsx::read.xlsx("Data/Base_Files/WIDA, ACCESS & Alt-ACCESS SY2022-23 Summative, Stage 2.xlsx"))
tmp.2023.demo <- as.data.table(read.spss("Data/Base_Files/Vistas SY 2022-23, AVE, Deduplicated Excludes Ineligibles, V20230927 KF.sav"))
load("Data/Base_Files/NM_Targets_Formatted_2023.Rdata")

### Variable names
variables.to.get.2018 <- c("stid", "distcode", "distname", "schcode", "schname", "last", "first", "grade", "SS_composite", "PL_composite")
variable.names.2018 <- c("ID", "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME", "LAST_NAME", "FIRST_NAME", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL_ORIGINAL")
variables.to.get.2019 <- c("stid", "distcode", "distname", "schcode", "schname", "last", "first", "STARS_grade", "SS_composite", "PL_composite")
variable.names.2019 <- c("ID", "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME", "LAST_NAME", "FIRST_NAME", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL_ORIGINAL")
variables.to.get.2020 <- c("State Student ID", "District Number", "District Name", "School Number", "School Name", "Student Last Name", "Student First Name", "Grade", "Composite (Overall) Scale Score", "Composite (Overall) Proficiency Level", "Reported Record", "Length of Time in LEP/ELL Program")
variable.names.2020 <- c("ID", "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME", "LAST_NAME", "FIRST_NAME", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL_ORIGINAL", "REPORTED_RECORD", "YEARS_IN_NM")
variables.to.get.2021 <- c("State Student ID", "District Number", "District Name", "School Number", "School Name", "Student Last Name", "Student First Name", "Grade", "Composite (Overall) Scale Score", "Composite (Overall) Proficiency Level", "Length of Time in LEP/ELL Program")
variable.names.2021 <- c("ID", "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME", "LAST_NAME", "FIRST_NAME", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL_ORIGINAL", "YEARS_IN_NM")
variables.to.get.2022 <- c("StID", "DistCode", "DistName", "SchCode", "SchName", "LastName", "FirstName", "Grade", "ScaleScore", "PL", "InELPSince")
variable.names.2022 <- c("ID", "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME", "LAST_NAME", "FIRST_NAME", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL_ORIGINAL", "YEARS_IN_NM")
variables.to.get.2023 <- c("State.Student.ID", "District.Number", "District.Name", "School.Number", "School.Name", "Student.Last.Name", "Student.First.Name", "Grade", "Scale.Score.-.Overall", "Proficiency.Level.-.Overall", "Length.of.Time.in.LEP/ELL.Program")
variable.names.2023 <- c("ID", "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME", "LAST_NAME", "FIRST_NAME", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL_ORIGINAL", "YEARS_IN_NM")
variables.to.get.2023.demo <- c("STUDENT_ID", "SY2023_Accountable_School", "SY2023_Accountable_District", "STUDENT_GENDER", "RaceEthnicity_NM", "RPTG_RACE_ETHNICITY_DESC", "ECONOMIC_CODE_IFEVER", "POVERTY_CODE_IFEVER", "SPECIAL_ED_CODE_IFEVER", "ENG_PROFICIENCY_IFEVER", "HOMELESS_IFEVER", "YEARS_US_SCHOOLS",  "CURR_GRADE_LVL")
variable.names.2023.demo <- c("ID", "SCHOOL_NUMBER_ACCOUNTABLE", "DISTRICT_NUMBER_ACCOUNTABLE", "GENDER", "ETHNICITY", "ETHNICITY_DESCRIPTION", "SES_STATUS", "POVERTY_STATUS", "SPECIAL_EDUCATION_STATUS", "ENGLISH_PROFICIENCY_STATUS", "HOMELESS_STATUS", "YEARS_IN_US_SCHOOLS", "GRADE")

### Rename variables
tmp.2018 <- tmp.2018[,variables.to.get.2018, with=FALSE]
tmp.2019 <- tmp.2019[,variables.to.get.2019, with=FALSE]
tmp.2020 <- tmp.2020[,variables.to.get.2020, with=FALSE]
tmp.2021 <- tmp.2021[,variables.to.get.2021, with=FALSE]
tmp.2022 <- tmp.2022[,variables.to.get.2022, with=FALSE]
tmp.2023 <- tmp.2023[,variables.to.get.2023, with=FALSE]
tmp.2023.demo <- tmp.2023.demo[,variables.to.get.2023.demo, with=FALSE]

setnames(tmp.2018, variables.to.get.2018, variable.names.2018)
setnames(tmp.2019, variables.to.get.2019, variable.names.2019)
setnames(tmp.2020, variables.to.get.2020, variable.names.2020)
setnames(tmp.2021, variables.to.get.2021, variable.names.2021)
setnames(tmp.2022, variables.to.get.2022, variable.names.2022)
setnames(tmp.2023, variables.to.get.2023, variable.names.2023)
setnames(tmp.2023.demo, variables.to.get.2023.demo, variable.names.2023.demo)

### Clean up data

## 2018
tmp.2018 <- tmp.2018[!is.na(ID)]
tmp.2018 <- tmp.2018[!is.na(SCALE_SCORE)]
tmp.2018[,DISTRICT_NUMBER:=strtail(paste0("000", DISTRICT_NUMBER), 3)]
tmp.2018[,SCHOOL_NUMBER:=strtail(paste0("000", SCHOOL_NUMBER), 3)]
tmp.2018[,ACHIEVEMENT_LEVEL_ORIGINAL:=as.character(ACHIEVEMENT_LEVEL_ORIGINAL)]
tmp.2018[!is.na(ACHIEVEMENT_LEVEL_ORIGINAL), ACHIEVEMENT_LEVEL:=paste("WIDA Level", strhead(ACHIEVEMENT_LEVEL_ORIGINAL, 1))]
tmp.2018[!is.na(ACHIEVEMENT_LEVEL_ORIGINAL), ACHIEVEMENT_LEVEL_ORIGINAL:=strhead(paste0(ACHIEVEMENT_LEVEL_ORIGINAL, ".0"), 3)]
tmp.2018[,SCALE_SCORE:=as.numeric(SCALE_SCORE)]
tmp.2018[,GRADE:=as.character(GRADE)]
tmp.2018[,ID:=as.character(ID)]
tmp.2018[,LAST_NAME:=as.factor(LAST_NAME)]
setattr(tmp.2018$LAST_NAME, "levels", sapply(levels(tmp.2018$LAST_NAME), SGP::capwords))
tmp.2018[,FIRST_NAME:=as.factor(FIRST_NAME)]
setattr(tmp.2018$FIRST_NAME, "levels", sapply(levels(tmp.2018$FIRST_NAME), SGP::capwords))
tmp.2018[,YEAR:="2018"]
tmp.2018[,CONTENT_AREA:="READING"]

## 2019
tmp.2019 <- tmp.2019[!is.na(ID)]
tmp.2019 <- tmp.2019[!is.na(SCALE_SCORE)]
tmp.2019[,DISTRICT_NUMBER:=strtail(paste0("000", DISTRICT_NUMBER), 3)]
tmp.2019[,SCHOOL_NUMBER:=strtail(paste0("000", SCHOOL_NUMBER), 3)]
tmp.2019[,ACHIEVEMENT_LEVEL_ORIGINAL:=as.character(ACHIEVEMENT_LEVEL_ORIGINAL)]
tmp.2019[!is.na(ACHIEVEMENT_LEVEL_ORIGINAL), ACHIEVEMENT_LEVEL:=paste("WIDA Level", strhead(ACHIEVEMENT_LEVEL_ORIGINAL, 1))]
tmp.2019[!is.na(ACHIEVEMENT_LEVEL_ORIGINAL), ACHIEVEMENT_LEVEL_ORIGINAL:=strhead(paste0(ACHIEVEMENT_LEVEL_ORIGINAL, ".0"), 3)]
tmp.2019[,SCALE_SCORE:=as.numeric(SCALE_SCORE)]
tmp.2019[,GRADE:=as.character(GRADE)]
tmp.2019[,ID:=as.character(ID)]
tmp.2019[,LAST_NAME:=as.factor(LAST_NAME)]
setattr(tmp.2019$LAST_NAME, "levels", sapply(levels(tmp.2019$LAST_NAME), SGP::capwords))
tmp.2019[,FIRST_NAME:=as.factor(FIRST_NAME)]
setattr(tmp.2019$FIRST_NAME, "levels", sapply(levels(tmp.2019$FIRST_NAME), SGP::capwords))
tmp.2019[,YEAR:="2019"]
tmp.2019[,CONTENT_AREA:="READING"]

## 2020
tmp.2020 <- tmp.2020[!is.na(ID)]
tmp.2020 <- tmp.2020[!is.na(SCALE_SCORE)]
tmp.2020 <- tmp.2020[REPORTED_RECORD==1]
tmp.2020[,DISTRICT_NUMBER:=strtail(DISTRICT_NUMBER, 3)]
tmp.2020[,SCHOOL_NUMBER:=strtail(paste0("000", SCHOOL_NUMBER), 3)]
tmp.2020[,ACHIEVEMENT_LEVEL_ORIGINAL:=as.character(ACHIEVEMENT_LEVEL_ORIGINAL)]
tmp.2020[!is.na(ACHIEVEMENT_LEVEL_ORIGINAL), ACHIEVEMENT_LEVEL:=paste("WIDA Level", strhead(ACHIEVEMENT_LEVEL_ORIGINAL, 1))]
tmp.2020[!is.na(ACHIEVEMENT_LEVEL_ORIGINAL), ACHIEVEMENT_LEVEL_ORIGINAL:=strhead(paste0(ACHIEVEMENT_LEVEL_ORIGINAL, ".0"), 3)]
tmp.2020[,SCALE_SCORE:=as.numeric(SCALE_SCORE)]
tmp.2020[,GRADE:=as.character(GRADE)]
tmp.2020[,ID:=as.character(ID)]
tmp.2020[,LAST_NAME:=as.factor(LAST_NAME)]
setattr(tmp.2020$LAST_NAME, "levels", sapply(levels(tmp.2020$LAST_NAME), SGP::capwords))
tmp.2020[,FIRST_NAME:=as.factor(FIRST_NAME)]
setattr(tmp.2020$FIRST_NAME, "levels", sapply(levels(tmp.2020$FIRST_NAME), SGP::capwords))
tmp.2020[,YEAR:="2020"]
tmp.2020[,CONTENT_AREA:="READING"]
tmp.2020[,REPORTED_RECORD:=NULL]

## 2021
tmp.2021 <- tmp.2021[!is.na(ID)]
tmp.2021 <- tmp.2021[!is.na(SCALE_SCORE)]
tmp.2021[,DISTRICT_NUMBER:=strtail(DISTRICT_NUMBER, 3)]
tmp.2021[,SCHOOL_NUMBER:=strtail(paste0("000", SCHOOL_NUMBER), 3)]
tmp.2021[,ACHIEVEMENT_LEVEL_ORIGINAL:=as.character(ACHIEVEMENT_LEVEL_ORIGINAL)]
tmp.2021[!is.na(ACHIEVEMENT_LEVEL_ORIGINAL), ACHIEVEMENT_LEVEL:=paste("WIDA Level", strhead(ACHIEVEMENT_LEVEL_ORIGINAL, 1))]
tmp.2021[!is.na(ACHIEVEMENT_LEVEL_ORIGINAL), ACHIEVEMENT_LEVEL_ORIGINAL:=strhead(paste0(ACHIEVEMENT_LEVEL_ORIGINAL, ".0"), 3)]
tmp.2021[,SCALE_SCORE:=as.numeric(SCALE_SCORE)]
tmp.2021[,GRADE:=as.character(GRADE)]
tmp.2021[,ID:=as.character(ID)]
tmp.2021[,LAST_NAME:=as.factor(LAST_NAME)]
setattr(tmp.2021$LAST_NAME, "levels", sapply(levels(tmp.2021$LAST_NAME), SGP::capwords))
tmp.2021[,FIRST_NAME:=as.factor(FIRST_NAME)]
setattr(tmp.2021$FIRST_NAME, "levels", sapply(levels(tmp.2021$FIRST_NAME), SGP::capwords))
tmp.2021[,YEAR:="2021"]
tmp.2021[,CONTENT_AREA:="READING"]

## 2022
tmp.2022 <- tmp.2022[!is.na(ID)]
tmp.2022 <- tmp.2022[!is.na(SCALE_SCORE)]
tmp.2022[,DISTRICT_NUMBER:=strtail(paste0("000", DISTRICT_NUMBER), 3)]
tmp.2022[,SCHOOL_NUMBER:=strtail(paste0("000", SCHOOL_NUMBER), 3)]
tmp.2022[ACHIEVEMENT_LEVEL_ORIGINAL=="",ACHIEVEMENT_LEVEL_ORIGINAL:=as.character(NA)]
tmp.2022[!is.na(ACHIEVEMENT_LEVEL_ORIGINAL), ACHIEVEMENT_LEVEL:=paste("WIDA Level", strhead(ACHIEVEMENT_LEVEL_ORIGINAL, 1))]
tmp.2022[!is.na(ACHIEVEMENT_LEVEL_ORIGINAL), ACHIEVEMENT_LEVEL_ORIGINAL:=strhead(paste0(ACHIEVEMENT_LEVEL_ORIGINAL, ".0"), 3)]
tmp.2022[,SCALE_SCORE:=as.numeric(SCALE_SCORE)]
tmp.2022[,GRADE:=as.character(GRADE)]
tmp.2022[,ID:=as.character(ID)]
tmp.2022[,LAST_NAME:=as.factor(LAST_NAME)]
setattr(tmp.2022$LAST_NAME, "levels", sapply(levels(tmp.2022$LAST_NAME), SGP::capwords))
tmp.2022[,FIRST_NAME:=as.factor(FIRST_NAME)]
setattr(tmp.2022$FIRST_NAME, "levels", sapply(levels(tmp.2022$FIRST_NAME), SGP::capwords))
tmp.2022[,YEAR:="2022"]
tmp.2022[,CONTENT_AREA:="READING"]
tmp.2022[GRADE=="KF", GRADE:="0"]
tmp.2022[GRADE=="", GRADE:=NA]

## 2023
tmp.2023 <- tmp.2023[!is.na(ID)]
tmp.2023 <- tmp.2023[!is.na(SCALE_SCORE)]
tmp.2023[,DISTRICT_NUMBER:=strtail(DISTRICT_NUMBER, 3)]
tmp.2023[,SCHOOL_NUMBER:=strtail(paste0("000", SCHOOL_NUMBER), 3)]
tmp.2023[,ACHIEVEMENT_LEVEL_ORIGINAL:=as.character(ACHIEVEMENT_LEVEL_ORIGINAL)]
tmp.2023[!is.na(ACHIEVEMENT_LEVEL_ORIGINAL), ACHIEVEMENT_LEVEL:=paste("WIDA Level", strhead(ACHIEVEMENT_LEVEL_ORIGINAL, 1))]
tmp.2023[!is.na(ACHIEVEMENT_LEVEL_ORIGINAL), ACHIEVEMENT_LEVEL_ORIGINAL:=strhead(paste0(ACHIEVEMENT_LEVEL_ORIGINAL, ".0"), 3)]
tmp.2023[,SCALE_SCORE:=as.numeric(SCALE_SCORE)]
tmp.2023[,GRADE:=as.character(GRADE)]
tmp.2023[,ID:=as.character(ID)]
tmp.2023[,LAST_NAME:=as.factor(LAST_NAME)]
setattr(tmp.2023$LAST_NAME, "levels", sapply(levels(tmp.2023$LAST_NAME), SGP::capwords))
tmp.2023[,FIRST_NAME:=as.factor(FIRST_NAME)]
setattr(tmp.2023$FIRST_NAME, "levels", sapply(levels(tmp.2023$FIRST_NAME), SGP::capwords))
tmp.2023[,YEAR:="2023"]
tmp.2023[,CONTENT_AREA:="READING"]

### 2023 DEMO
tmp.2023.demo[,CONTENT_AREA:="READING"]
tmp.2023.demo[,YEAR:="2023"]
tmp.2023.demo[,ID:=as.character(ID)]
tmp.2023.demo[,VALID_CASE:="VALID_CASE"]

### rbind all the files together
WIDA_NM_Data_LONG <- rbindlist(list(tmp.2018, tmp.2019, tmp.2020, tmp.2021, tmp.2022, tmp.2023), fill=TRUE)
WIDA_NM_Data_LONG[,VALID_CASE:="VALID_CASE"]
WIDA_NM_Data_LONG <- WIDA_NM_Data_LONG[!ACHIEVEMENT_LEVEL_ORIGINAL %in% c("A1.", "A2.", "A3.", "P1.", "P2.")]

### Setkey and check for duplicates
setkey(WIDA_NM_Data_LONG, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)
setkey(WIDA_NM_Data_LONG, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
dups <- WIDA_NM_Data_LONG[WIDA_NM_Data_LONG[duplicated(WIDA_NM_Data_LONG, by=key(WIDA_NM_Data_LONG))][,key(WIDA_NM_Data_LONG), with=FALSE]] ## 26 duplicate student IDs
dups2 <- dups[,list(COUNT=.N, UNIQUE_LAST_NAMES=length(unique(LAST_NAME)), UNIQUE_FIRST_NAMES=length(unique(FIRST_NAME))), by=key(dups)][dups]
WIDA_NM_Data_LONG[which(duplicated(WIDA_NM_Data_LONG, by=key(WIDA_NM_Data_LONG)))-1, VALID_CASE:="INVALID_CASE"]
setkey(WIDA_NM_Data_LONG, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)

### Merge in demographic data
setkey(tmp.2023.demo, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
WIDA_NM_Data_LONG <- tmp.2023.demo[WIDA_NM_Data_LONG]

### Merge in targets
#shift.key <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "VALID_CASE")
#setkeyv(WIDA_NM_Data_LONG, shift.key)

#getShiftedValues(WIDA_NM_Data_LONG,
#                 shift_amount = 1L,
#                 shift_variable = "ACHIEVEMENT_LEVEL_ORIGINAL")

#setnames(WIDA_NM_Data_LONG, "ACHIEVEMENT_LEVEL_ORIGINAL_LAG_1", "ACHIEVEMENT_LEVEL_ORIGINAL_INITIAL")

tmp.dt <- WIDA_NM_Data_LONG[VALID_CASE=="VALID_CASE"]
setkey(tmp.dt, ID, YEAR)
setkey(tmp.dt, ID)
id.first.year <- tmp.dt[!duplicated(tmp.dt, by=key(tmp.dt))][,c("ID", "YEAR", "ACHIEVEMENT_LEVEL_ORIGINAL"), with=FALSE]
id.first.year[,YEARS_SINCE_FIRST_WIDA:=2023 - as.numeric(YEAR)]
setnames(id.first.year, "ACHIEVEMENT_LEVEL_ORIGINAL", "ACHIEVEMENT_LEVEL_ORIGINAL_INITIAL")
id.first.year[,VALID_CASE:="VALID_CASE"][,YEAR:="2023"][,CONTENT_AREA:="READING"]
setkey(id.first.year, VALID_CASE, CONTENT_AREA, YEAR, ID)
setkey(WIDA_NM_Data_LONG, VALID_CASE, CONTENT_AREA, YEAR, ID)
WIDA_NM_Data_LONG <- id.first.year[WIDA_NM_Data_LONG]

setkey(WIDA_NM_Data_LONG, VALID_CASE, CONTENT_AREA, YEAR, GRADE, YEARS_SINCE_FIRST_WIDA, ACHIEVEMENT_LEVEL_ORIGINAL_INITIAL)
NM_Targets_Formatted_2023[,VALID_CASE:="VALID_CASE"][,CONTENT_AREA:="READING"][,YEAR:="2023"]
setnames(NM_Targets_Formatted_2023, "ACHIEVEMENT_LEVEL_ORIGINAL", "ACHIEVEMENT_LEVEL_ORIGINAL_INITIAL")
setkey(NM_Targets_Formatted_2023, VALID_CASE, CONTENT_AREA, YEAR, GRADE, YEARS_SINCE_FIRST_WIDA, ACHIEVEMENT_LEVEL_ORIGINAL_INITIAL)
WIDA_NM_Data_LONG <- NM_Targets_Formatted_2023[WIDA_NM_Data_LONG]
setkey(WIDA_NM_Data_LONG, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
WIDA_NM_Data_LONG[ACHIEVEMENT_LEVEL_ORIGINAL >= NEW_MEXICO_GROWTH_MODEL_TARGET, NEW_MEXICO_GROWTH_MODEL_TARGET_MET:="Met New Mexico Growth Target"]
WIDA_NM_Data_LONG[ACHIEVEMENT_LEVEL_ORIGINAL < NEW_MEXICO_GROWTH_MODEL_TARGET, NEW_MEXICO_GROWTH_MODEL_TARGET_MET:="Did not meet New Mexico Growth Target"]

names.order <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE", "ID", "ACHIEVEMENT_LEVEL_ORIGINAL_INITIAL", "ACHIEVEMENT_LEVEL_ORIGINAL", "ACHIEVEMENT_LEVEL", "YEARS_SINCE_FIRST_WIDA", "NEW_MEXICO_GROWTH_MODEL_TARGET", "SCALE_SCORE")
names.order <- c(names.order, setdiff(names(WIDA_NM_Data_LONG), names.order))

setcolorder(WIDA_NM_Data_LONG, names.order)


### Save output
save(WIDA_NM_Data_LONG, file="Data/WIDA_NM_Data_LONG.Rdata")