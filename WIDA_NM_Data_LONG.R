#######################################################################
###
### Data prep script for WIDA NM data
###
########################################################################

### Load packages
require(data.table)
require(openxlsx)


### Load base files
tmp.2018 <- fread("Data/Base_Files/WIDA, ACCESS Summative SY 2017-18.csv")
tmp.2019 <- fread("Data/Base_Files/WIDA, ACCESS Summative SY 2018-19.csv")
tmp.2020 <- fread("Data/Base_Files/WIDA, ACCESS Summative SY 2019-20.csv")
tmp.2021 <- fread("Data/Base_Files/WIDA, ACCESS Summative SY 2020-21.csv")
tmp.2022 <- fread("Data/Base_Files/WIDA, ACCESS & Alt-ACCESS SY 2021-22 Summative, Stage 2.csv")
tmp.2023 <- as.data.table(openxlsx::read.xlsx("Data/Base_Files/WIDA, ACCESS & Alt-ACCESS SY2022-23 Summative, Stage 2.xlsx"))

### Variable names
variables.to.get.2018 <- c("stid", "distcode", "distname", "schcode", "schname", "last", "first", "grade", "SS_composite", "PL_composite")
variable.names.2018 <- c("ID", "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME", "LAST_NAME", "FIRST_NAME", "GRADE", "SCALE_SCORE", "WIDA_ACHIEVEMENT_LEVEL")
variables.to.get.2019 <- c("stid", "distcode", "distname", "schcode", "schname", "last", "first", "STARS_grade", "SS_composite", "PL_composite")
variable.names.2019 <- c("ID", "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME", "LAST_NAME", "FIRST_NAME", "GRADE", "SCALE_SCORE", "WIDA_ACHIEVEMENT_LEVEL")
variables.to.get.2020 <- c("State Student ID", "District Number", "District Name", "School Number", "School Name", "Student Last Name", "Student First Name", "Grade", "Composite (Overall) Scale Score", "Composite (Overall) Proficiency Level", "Reported Record", "Length of Time in LEP/ELL Program")
variable.names.2020 <- c("ID", "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME", "LAST_NAME", "FIRST_NAME", "GRADE", "SCALE_SCORE", "WIDA_ACHIEVEMENT_LEVEL", "REPORTED_RECORD", "TIME_IN_NM")
variables.to.get.2021 <- c("State Student ID", "District Number", "District Name", "School Number", "School Name", "Student Last Name", "Student First Name", "Grade", "Composite (Overall) Scale Score", "Composite (Overall) Proficiency Level", "Length of Time in LEP/ELL Program")
variable.names.2021 <- c("ID", "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME", "LAST_NAME", "FIRST_NAME", "GRADE", "SCALE_SCORE", "WIDA_ACHIEVEMENT_LEVEL", "TIME_IN_NM")
variables.to.get.2022 <- c("StID", "DistCode", "DistName", "SchCode", "SchName", "LastName", "FirstName", "Grade", "ScaleScore", "PL", "InELPSince")
variable.names.2022 <- c("ID", "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME", "LAST_NAME", "FIRST_NAME", "GRADE", "SCALE_SCORE", "WIDA_ACHIEVEMENT_LEVEL", "TIME_IN_NM")
variables.to.get.2023 <- c("State.Student.ID", "District.Number", "District.Name", "School.Number", "School.Name", "Student.Last.Name", "Student.First.Name", "Grade", "Scale.Score.-.Overall", "Proficiency.Level.-.Overall", "Length.of.Time.in.LEP/ELL.Program")
variable.names.2023 <- c("ID", "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME", "LAST_NAME", "FIRST_NAME", "GRADE", "SCALE_SCORE", "WIDA_ACHIEVEMENT_LEVEL", "TIME_IN_NM")

### Rename variables
tmp.2018 <- tmp.2018[,variables.to.get.2018, with=FALSE]
tmp.2019 <- tmp.2019[,variables.to.get.2019, with=FALSE]
tmp.2020 <- tmp.2020[,variables.to.get.2020, with=FALSE]
tmp.2021 <- tmp.2021[,variables.to.get.2021, with=FALSE]
tmp.2022 <- tmp.2022[,variables.to.get.2022, with=FALSE]
tmp.2023 <- tmp.2023[,variables.to.get.2023, with=FALSE]

setnames(tmp.2018, variables.to.get.2018, variable.names.2018)
setnames(tmp.2019, variables.to.get.2019, variable.names.2019)
setnames(tmp.2020, variables.to.get.2020, variable.names.2020)
setnames(tmp.2021, variables.to.get.2021, variable.names.2021)
setnames(tmp.2022, variables.to.get.2022, variable.names.2022)
setnames(tmp.2023, variables.to.get.2023, variable.names.2023)

### Clean up data

## 2018
tmp.2018 <- tmp.2018[!is.na(ID)]
tmp.2018 <- tmp.2018[!is.na(SCALE_SCORE)]
tmp.2018[,DISTRICT_NUMBER:=strtail(paste0("000", DISTRICT_NUMBER), 3)]
tmp.2018[,SCHOOL_NUMBER:=strtail(paste0("000", SCHOOL_NUMBER), 3)]
tmp.2018[,WIDA_ACHIEVEMENT_LEVEL:=as.character(WIDA_ACHIEVEMENT_LEVEL)]
tmp.2018[!is.na(WIDA_ACHIEVEMENT_LEVEL), ACHIEVEMENT_LEVEL:=paste("WIDA Level", strhead(WIDA_ACHIEVEMENT_LEVEL, 1))]
tmp.2018[!is.na(WIDA_ACHIEVEMENT_LEVEL), WIDA_ACHIEVEMENT_LEVEL:=strhead(paste0(WIDA_ACHIEVEMENT_LEVEL, ".0"), 3)]
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
tmp.2019[,WIDA_ACHIEVEMENT_LEVEL:=as.character(WIDA_ACHIEVEMENT_LEVEL)]
tmp.2019[!is.na(WIDA_ACHIEVEMENT_LEVEL), ACHIEVEMENT_LEVEL:=paste("WIDA Level", strhead(WIDA_ACHIEVEMENT_LEVEL, 1))]
tmp.2019[!is.na(WIDA_ACHIEVEMENT_LEVEL), WIDA_ACHIEVEMENT_LEVEL:=strhead(paste0(WIDA_ACHIEVEMENT_LEVEL, ".0"), 3)]
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
tmp.2020[,WIDA_ACHIEVEMENT_LEVEL:=as.character(WIDA_ACHIEVEMENT_LEVEL)]
tmp.2020[!is.na(WIDA_ACHIEVEMENT_LEVEL), ACHIEVEMENT_LEVEL:=paste("WIDA Level", strhead(WIDA_ACHIEVEMENT_LEVEL, 1))]
tmp.2020[!is.na(WIDA_ACHIEVEMENT_LEVEL), WIDA_ACHIEVEMENT_LEVEL:=strhead(paste0(WIDA_ACHIEVEMENT_LEVEL, ".0"), 3)]
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
tmp.2021[,WIDA_ACHIEVEMENT_LEVEL:=as.character(WIDA_ACHIEVEMENT_LEVEL)]
tmp.2021[!is.na(WIDA_ACHIEVEMENT_LEVEL), ACHIEVEMENT_LEVEL:=paste("WIDA Level", strhead(WIDA_ACHIEVEMENT_LEVEL, 1))]
tmp.2021[!is.na(WIDA_ACHIEVEMENT_LEVEL), WIDA_ACHIEVEMENT_LEVEL:=strhead(paste0(WIDA_ACHIEVEMENT_LEVEL, ".0"), 3)]
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
tmp.2022[WIDA_ACHIEVEMENT_LEVEL=="",WIDA_ACHIEVEMENT_LEVEL:=as.character(NA)]
tmp.2022[!is.na(WIDA_ACHIEVEMENT_LEVEL), ACHIEVEMENT_LEVEL:=paste("WIDA Level", strhead(WIDA_ACHIEVEMENT_LEVEL, 1))]
tmp.2022[!is.na(WIDA_ACHIEVEMENT_LEVEL), WIDA_ACHIEVEMENT_LEVEL:=strhead(paste0(WIDA_ACHIEVEMENT_LEVEL, ".0"), 3)]
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
tmp.2023[,WIDA_ACHIEVEMENT_LEVEL:=as.character(WIDA_ACHIEVEMENT_LEVEL)]
tmp.2023[!is.na(WIDA_ACHIEVEMENT_LEVEL), ACHIEVEMENT_LEVEL:=paste("WIDA Level", strhead(WIDA_ACHIEVEMENT_LEVEL, 1))]
tmp.2023[!is.na(WIDA_ACHIEVEMENT_LEVEL), WIDA_ACHIEVEMENT_LEVEL:=strhead(paste0(WIDA_ACHIEVEMENT_LEVEL, ".0"), 3)]
tmp.2023[,SCALE_SCORE:=as.numeric(SCALE_SCORE)]
tmp.2023[,GRADE:=as.character(GRADE)]
tmp.2023[,ID:=as.character(ID)]
tmp.2023[,LAST_NAME:=as.factor(LAST_NAME)]
setattr(tmp.2023$LAST_NAME, "levels", sapply(levels(tmp.2023$LAST_NAME), SGP::capwords))
tmp.2023[,FIRST_NAME:=as.factor(FIRST_NAME)]
setattr(tmp.2023$FIRST_NAME, "levels", sapply(levels(tmp.2023$FIRST_NAME), SGP::capwords))
tmp.2023[,YEAR:="2023"]
tmp.2023[,CONTENT_AREA:="READING"]


### rbind all the files together
WIDA_NM_Data_LONG <- rbindlist(list(tmp.2018, tmp.2019, tmp.2020, tmp.2021, tmp.2022, tmp.2023), fill=TRUE)
WIDA_NM_Data_LONG[,VALID_CASE:="VALID_CASE"]
WIDA_NM_Data_LONG[WIDA_ACHIEVEMENT_LEVEL %in% c("A1.", "A2.", "A3.", "P1.", "P2."), VALID_CASE := "INVALID_CASE"]

### Setkey and check for duplicates
setkey(WIDA_NM_Data_LONG, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)
setkey(WIDA_NM_Data_LONG, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
dups <- WIDA_NM_Data_LONG[WIDA_NM_Data_LONG[duplicated(WIDA_NM_Data_LONG, by=key(WIDA_NM_Data_LONG))][,key(WIDA_NM_Data_LONG), with=FALSE]] ## 26 duplicate student IDs
dups2 <- dups[,list(COUNT=.N, UNIQUE_LAST_NAMES=length(unique(LAST_NAME)), UNIQUE_FIRST_NAMES=length(unique(FIRST_NAME))), by=key(dups)][dups]
WIDA_NM_Data_LONG[which(duplicated(WIDA_NM_Data_LONG, by=key(WIDA_NM_Data_LONG)))-1, VALID_CASE:="INVALID_CASE"]

### Save output
save(WIDA_NM_Data_LONG, file="Data/WIDA_NM_Data_LONG.Rdata")