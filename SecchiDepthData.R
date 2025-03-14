# Package ID: edi.198.13 Cataloging System:https://pasta.edirepository.org.
# Data set title: Secchi depth data and discrete depth profiles of water temperature, dissolved oxygen, conductivity, specific conductance, photosynthetic active radiation, oxidation-reduction potential, and pH for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2024.
# Data set creator:  Cayelan Carey - Virginia Tech
# Data set creator:  Adrienne Breef-Pilz - Virginia Tech
# Data set creator:  Heather Wander - Virginia Tech
# Data set creator:  Dexter Howard - Virginia Tech
# Data set creator:  George Haynie - Virginia Tech
# Data set creator:  Michael Kricheldorf - Virginia Tech
# Data set creator:  Sierra Tannheiser - Virginia Tech
# Data set creator:  Evelyn Tipper - Virginia Tech
# Contact:  Cayelan Carey -  Virginia Tech  - cayelan@vt.edu
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())

# setwd("C:/users/my_name/my_dir")



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/198/13/3ee0ddb9f2183ad4d8c955d50d1b8fba"
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F
               ,skip=1
               ,sep=","
               , col.names=c(
                 "Reservoir",
                 "Site",
                 "DateTime",
                 "Secchi_m",
                 "Flag_DateTime",
                 "Flag_Secchi_m"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Reservoir)!="factor") dt1$Reservoir<- as.factor(dt1$Reservoir)
if (class(dt1$Site)=="factor") dt1$Site <-as.numeric(levels(dt1$Site))[as.integer(dt1$Site) ]
if (class(dt1$Site)=="character") dt1$Site <-as.numeric(dt1$Site)
# attempting to convert dt1$DateTime dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d %H:%M:%S"
tmp1DateTime<-as.POSIXct(dt1$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$DateTime != "",]) == length(tmp1DateTime[!is.na(tmp1DateTime)])){dt1$DateTime <- tmp1DateTime } else {print("Date conversion failed for dt1$DateTime. Please inspect the data and do the date conversion yourself.")}

if (class(dt1$Secchi_m)=="factor") dt1$Secchi_m <-as.numeric(levels(dt1$Secchi_m))[as.integer(dt1$Secchi_m) ]
if (class(dt1$Secchi_m)=="character") dt1$Secchi_m <-as.numeric(dt1$Secchi_m)
if (class(dt1$Flag_DateTime)!="factor") dt1$Flag_DateTime<- as.factor(dt1$Flag_DateTime)
if (class(dt1$Flag_Secchi_m)!="factor") dt1$Flag_Secchi_m<- as.factor(dt1$Flag_Secchi_m)

# Convert Missing Values to NA for non-dates

dt1$Reservoir <- as.factor(ifelse((trimws(as.character(dt1$Reservoir))==trimws("NA")),NA,as.character(dt1$Reservoir)))
dt1$Site <- ifelse((trimws(as.character(dt1$Site))==trimws("NA")),NA,dt1$Site)
suppressWarnings(dt1$Site <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Site))==as.character(as.numeric("NA"))),NA,dt1$Site))
dt1$Secchi_m <- ifelse((trimws(as.character(dt1$Secchi_m))==trimws("NA")),NA,dt1$Secchi_m)
suppressWarnings(dt1$Secchi_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Secchi_m))==as.character(as.numeric("NA"))),NA,dt1$Secchi_m))
dt1$Flag_DateTime <- as.factor(ifelse((trimws(as.character(dt1$Flag_DateTime))==trimws("NA")),NA,as.character(dt1$Flag_DateTime)))
dt1$Flag_Secchi_m <- as.factor(ifelse((trimws(as.character(dt1$Flag_Secchi_m))==trimws("NA")),NA,as.character(dt1$Flag_Secchi_m)))


# Here is the structure of the input data frame:
str(dt1)
attach(dt1)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

summary(Reservoir)
summary(Site)
summary(DateTime)
summary(Secchi_m)
summary(Flag_DateTime)
summary(Flag_Secchi_m)
# Get more details on character variables

summary(as.factor(dt1$Reservoir))
summary(as.factor(dt1$Flag_DateTime))
summary(as.factor(dt1$Flag_Secchi_m))
detach(dt1)



inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/198/13/e50a50d062ee73f4d85e4f20b360ce4f"
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F
               ,skip=1
               ,sep=","
               , col.names=c(
                 "Reservoir",
                 "Site",
                 "DateTime",
                 "Depth_m",
                 "Temp_C",
                 "DO_mgL",
                 "DOsat_percent",
                 "Cond_uScm",
                 "SpCond_uScm",
                 "PAR_umolm2s",
                 "ORP_mV",
                 "pH",
                 "Flag_DateTime",
                 "Flag_Temp_C",
                 "Flag_DO_mgL",
                 "Flag_DOsat_percent",
                 "Flag_Cond_uScm",
                 "Flag_SpCond_uScm",
                 "Flag_PAR_umolm2s",
                 "Flag_ORP_mV",
                 "Flag_pH"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Reservoir)!="factor") dt2$Reservoir<- as.factor(dt2$Reservoir)
if (class(dt2$Site)=="factor") dt2$Site <-as.numeric(levels(dt2$Site))[as.integer(dt2$Site) ]
if (class(dt2$Site)=="character") dt2$Site <-as.numeric(dt2$Site)
# attempting to convert dt2$DateTime dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d %H:%M:%S"
tmp2DateTime<-as.POSIXct(dt2$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt2[dt2$DateTime != "",]) == length(tmp2DateTime[!is.na(tmp2DateTime)])){dt2$DateTime <- tmp2DateTime } else {print("Date conversion failed for dt2$DateTime. Please inspect the data and do the date conversion yourself.")}

if (class(dt2$Depth_m)=="factor") dt2$Depth_m <-as.numeric(levels(dt2$Depth_m))[as.integer(dt2$Depth_m) ]
if (class(dt2$Depth_m)=="character") dt2$Depth_m <-as.numeric(dt2$Depth_m)
if (class(dt2$Temp_C)=="factor") dt2$Temp_C <-as.numeric(levels(dt2$Temp_C))[as.integer(dt2$Temp_C) ]
if (class(dt2$Temp_C)=="character") dt2$Temp_C <-as.numeric(dt2$Temp_C)
if (class(dt2$DO_mgL)=="factor") dt2$DO_mgL <-as.numeric(levels(dt2$DO_mgL))[as.integer(dt2$DO_mgL) ]
if (class(dt2$DO_mgL)=="character") dt2$DO_mgL <-as.numeric(dt2$DO_mgL)
if (class(dt2$DOsat_percent)=="factor") dt2$DOsat_percent <-as.numeric(levels(dt2$DOsat_percent))[as.integer(dt2$DOsat_percent) ]
if (class(dt2$DOsat_percent)=="character") dt2$DOsat_percent <-as.numeric(dt2$DOsat_percent)
if (class(dt2$Cond_uScm)=="factor") dt2$Cond_uScm <-as.numeric(levels(dt2$Cond_uScm))[as.integer(dt2$Cond_uScm) ]
if (class(dt2$Cond_uScm)=="character") dt2$Cond_uScm <-as.numeric(dt2$Cond_uScm)
if (class(dt2$SpCond_uScm)=="factor") dt2$SpCond_uScm <-as.numeric(levels(dt2$SpCond_uScm))[as.integer(dt2$SpCond_uScm) ]
if (class(dt2$SpCond_uScm)=="character") dt2$SpCond_uScm <-as.numeric(dt2$SpCond_uScm)
if (class(dt2$PAR_umolm2s)=="factor") dt2$PAR_umolm2s <-as.numeric(levels(dt2$PAR_umolm2s))[as.integer(dt2$PAR_umolm2s) ]
if (class(dt2$PAR_umolm2s)=="character") dt2$PAR_umolm2s <-as.numeric(dt2$PAR_umolm2s)
if (class(dt2$ORP_mV)=="factor") dt2$ORP_mV <-as.numeric(levels(dt2$ORP_mV))[as.integer(dt2$ORP_mV) ]
if (class(dt2$ORP_mV)=="character") dt2$ORP_mV <-as.numeric(dt2$ORP_mV)
if (class(dt2$pH)=="factor") dt2$pH <-as.numeric(levels(dt2$pH))[as.integer(dt2$pH) ]
if (class(dt2$pH)=="character") dt2$pH <-as.numeric(dt2$pH)
if (class(dt2$Flag_DateTime)!="factor") dt2$Flag_DateTime<- as.factor(dt2$Flag_DateTime)
if (class(dt2$Flag_Temp_C)!="factor") dt2$Flag_Temp_C<- as.factor(dt2$Flag_Temp_C)
if (class(dt2$Flag_DO_mgL)!="factor") dt2$Flag_DO_mgL<- as.factor(dt2$Flag_DO_mgL)
if (class(dt2$Flag_DOsat_percent)!="factor") dt2$Flag_DOsat_percent<- as.factor(dt2$Flag_DOsat_percent)
if (class(dt2$Flag_Cond_uScm)!="factor") dt2$Flag_Cond_uScm<- as.factor(dt2$Flag_Cond_uScm)
if (class(dt2$Flag_SpCond_uScm)!="factor") dt2$Flag_SpCond_uScm<- as.factor(dt2$Flag_SpCond_uScm)
if (class(dt2$Flag_PAR_umolm2s)!="factor") dt2$Flag_PAR_umolm2s<- as.factor(dt2$Flag_PAR_umolm2s)
if (class(dt2$Flag_ORP_mV)!="factor") dt2$Flag_ORP_mV<- as.factor(dt2$Flag_ORP_mV)
if (class(dt2$Flag_pH)!="factor") dt2$Flag_pH<- as.factor(dt2$Flag_pH)

# Convert Missing Values to NA for non-dates

dt2$Reservoir <- as.factor(ifelse((trimws(as.character(dt2$Reservoir))==trimws("NA")),NA,as.character(dt2$Reservoir)))
dt2$Site <- ifelse((trimws(as.character(dt2$Site))==trimws("NA")),NA,dt2$Site)
suppressWarnings(dt2$Site <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Site))==as.character(as.numeric("NA"))),NA,dt2$Site))
dt2$Depth_m <- ifelse((trimws(as.character(dt2$Depth_m))==trimws("NA")),NA,dt2$Depth_m)
suppressWarnings(dt2$Depth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Depth_m))==as.character(as.numeric("NA"))),NA,dt2$Depth_m))
dt2$Temp_C <- ifelse((trimws(as.character(dt2$Temp_C))==trimws("NA")),NA,dt2$Temp_C)
suppressWarnings(dt2$Temp_C <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Temp_C))==as.character(as.numeric("NA"))),NA,dt2$Temp_C))
dt2$DO_mgL <- ifelse((trimws(as.character(dt2$DO_mgL))==trimws("NA")),NA,dt2$DO_mgL)
suppressWarnings(dt2$DO_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DO_mgL))==as.character(as.numeric("NA"))),NA,dt2$DO_mgL))
dt2$DOsat_percent <- ifelse((trimws(as.character(dt2$DOsat_percent))==trimws("NA")),NA,dt2$DOsat_percent)
suppressWarnings(dt2$DOsat_percent <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DOsat_percent))==as.character(as.numeric("NA"))),NA,dt2$DOsat_percent))
dt2$Cond_uScm <- ifelse((trimws(as.character(dt2$Cond_uScm))==trimws("NA")),NA,dt2$Cond_uScm)
suppressWarnings(dt2$Cond_uScm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Cond_uScm))==as.character(as.numeric("NA"))),NA,dt2$Cond_uScm))
dt2$SpCond_uScm <- ifelse((trimws(as.character(dt2$SpCond_uScm))==trimws("NA")),NA,dt2$SpCond_uScm)
suppressWarnings(dt2$SpCond_uScm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SpCond_uScm))==as.character(as.numeric("NA"))),NA,dt2$SpCond_uScm))
dt2$PAR_umolm2s <- ifelse((trimws(as.character(dt2$PAR_umolm2s))==trimws("NA")),NA,dt2$PAR_umolm2s)
suppressWarnings(dt2$PAR_umolm2s <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$PAR_umolm2s))==as.character(as.numeric("NA"))),NA,dt2$PAR_umolm2s))
dt2$ORP_mV <- ifelse((trimws(as.character(dt2$ORP_mV))==trimws("NA")),NA,dt2$ORP_mV)
suppressWarnings(dt2$ORP_mV <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ORP_mV))==as.character(as.numeric("NA"))),NA,dt2$ORP_mV))
dt2$pH <- ifelse((trimws(as.character(dt2$pH))==trimws("NA")),NA,dt2$pH)
suppressWarnings(dt2$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$pH))==as.character(as.numeric("NA"))),NA,dt2$pH))
dt2$Flag_DateTime <- as.factor(ifelse((trimws(as.character(dt2$Flag_DateTime))==trimws("NA")),NA,as.character(dt2$Flag_DateTime)))
dt2$Flag_Temp_C <- as.factor(ifelse((trimws(as.character(dt2$Flag_Temp_C))==trimws("NA")),NA,as.character(dt2$Flag_Temp_C)))
dt2$Flag_DO_mgL <- as.factor(ifelse((trimws(as.character(dt2$Flag_DO_mgL))==trimws("NA")),NA,as.character(dt2$Flag_DO_mgL)))
dt2$Flag_DOsat_percent <- as.factor(ifelse((trimws(as.character(dt2$Flag_DOsat_percent))==trimws("NA")),NA,as.character(dt2$Flag_DOsat_percent)))
dt2$Flag_Cond_uScm <- as.factor(ifelse((trimws(as.character(dt2$Flag_Cond_uScm))==trimws("NA")),NA,as.character(dt2$Flag_Cond_uScm)))
dt2$Flag_SpCond_uScm <- as.factor(ifelse((trimws(as.character(dt2$Flag_SpCond_uScm))==trimws("NA")),NA,as.character(dt2$Flag_SpCond_uScm)))
dt2$Flag_PAR_umolm2s <- as.factor(ifelse((trimws(as.character(dt2$Flag_PAR_umolm2s))==trimws("NA")),NA,as.character(dt2$Flag_PAR_umolm2s)))
dt2$Flag_ORP_mV <- as.factor(ifelse((trimws(as.character(dt2$Flag_ORP_mV))==trimws("NA")),NA,as.character(dt2$Flag_ORP_mV)))
dt2$Flag_pH <- as.factor(ifelse((trimws(as.character(dt2$Flag_pH))==trimws("NA")),NA,as.character(dt2$Flag_pH)))


# Here is the structure of the input data frame:
str(dt2)
attach(dt2)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

summary(Reservoir)
summary(Site)
summary(DateTime)
summary(Depth_m)
summary(Temp_C)
summary(DO_mgL)
summary(DOsat_percent)
summary(Cond_uScm)
summary(SpCond_uScm)
summary(PAR_umolm2s)
summary(ORP_mV)
summary(pH)
summary(Flag_DateTime)
summary(Flag_Temp_C)
summary(Flag_DO_mgL)
summary(Flag_DOsat_percent)
summary(Flag_Cond_uScm)
summary(Flag_SpCond_uScm)
summary(Flag_PAR_umolm2s)
summary(Flag_ORP_mV)
summary(Flag_pH)
# Get more details on character variables

summary(as.factor(dt2$Reservoir))
summary(as.factor(dt2$Flag_DateTime))
summary(as.factor(dt2$Flag_Temp_C))
summary(as.factor(dt2$Flag_DO_mgL))
summary(as.factor(dt2$Flag_DOsat_percent))
summary(as.factor(dt2$Flag_Cond_uScm))
summary(as.factor(dt2$Flag_SpCond_uScm))
summary(as.factor(dt2$Flag_PAR_umolm2s))
summary(as.factor(dt2$Flag_ORP_mV))
summary(as.factor(dt2$Flag_pH))
detach(dt2)



inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/198/13/3adaa450de61e03dedd998dd8a8418cf"
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F
               ,skip=1
               ,sep=","
               , col.names=c(
                 "Reservoir",
                 "Site",
                 "DataStream",
                 "TIMESTAMP_start",
                 "TIMESTAMP_end",
                 "start_parameter",
                 "end_parameter",
                 "flag",
                 "update_value",
                 "notes"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$Reservoir)!="factor") dt3$Reservoir<- as.factor(dt3$Reservoir)
if (class(dt3$Site)=="factor") dt3$Site <-as.numeric(levels(dt3$Site))[as.integer(dt3$Site) ]
if (class(dt3$Site)=="character") dt3$Site <-as.numeric(dt3$Site)
if (class(dt3$DataStream)!="factor") dt3$DataStream<- as.factor(dt3$DataStream)
# attempting to convert dt3$TIMESTAMP_start dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d %H:%M:%S"
tmp3TIMESTAMP_start<-as.POSIXct(dt3$TIMESTAMP_start,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt3[dt3$TIMESTAMP_start != "",]) == length(tmp3TIMESTAMP_start[!is.na(tmp3TIMESTAMP_start)])){dt3$TIMESTAMP_start <- tmp3TIMESTAMP_start } else {print("Date conversion failed for dt3$TIMESTAMP_start. Please inspect the data and do the date conversion yourself.")}

# attempting to convert dt3$TIMESTAMP_end dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d %H:%M:%S"
tmp3TIMESTAMP_end<-as.POSIXct(dt3$TIMESTAMP_end,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt3[dt3$TIMESTAMP_end != "",]) == length(tmp3TIMESTAMP_end[!is.na(tmp3TIMESTAMP_end)])){dt3$TIMESTAMP_end <- tmp3TIMESTAMP_end } else {print("Date conversion failed for dt3$TIMESTAMP_end. Please inspect the data and do the date conversion yourself.")}

if (class(dt3$start_parameter)!="factor") dt3$start_parameter<- as.factor(dt3$start_parameter)
if (class(dt3$end_parameter)!="factor") dt3$end_parameter<- as.factor(dt3$end_parameter)
if (class(dt3$flag)!="factor") dt3$flag<- as.factor(dt3$flag)
if (class(dt3$update_value)!="factor") dt3$update_value<- as.factor(dt3$update_value)
if (class(dt3$notes)!="factor") dt3$notes<- as.factor(dt3$notes)

# Convert Missing Values to NA for non-dates

dt3$Reservoir <- as.factor(ifelse((trimws(as.character(dt3$Reservoir))==trimws("NA")),NA,as.character(dt3$Reservoir)))
dt3$Site <- ifelse((trimws(as.character(dt3$Site))==trimws("NA")),NA,dt3$Site)
suppressWarnings(dt3$Site <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Site))==as.character(as.numeric("NA"))),NA,dt3$Site))
dt3$DataStream <- as.factor(ifelse((trimws(as.character(dt3$DataStream))==trimws("NA")),NA,as.character(dt3$DataStream)))
dt3$start_parameter <- as.factor(ifelse((trimws(as.character(dt3$start_parameter))==trimws("NA")),NA,as.character(dt3$start_parameter)))
dt3$end_parameter <- as.factor(ifelse((trimws(as.character(dt3$end_parameter))==trimws("NA")),NA,as.character(dt3$end_parameter)))
dt3$flag <- as.factor(ifelse((trimws(as.character(dt3$flag))==trimws("NA")),NA,as.character(dt3$flag)))
dt3$update_value <- as.factor(ifelse((trimws(as.character(dt3$update_value))==trimws("NA")),NA,as.character(dt3$update_value)))
dt3$notes <- as.factor(ifelse((trimws(as.character(dt3$notes))==trimws("NA")),NA,as.character(dt3$notes)))


# Here is the structure of the input data frame:
str(dt3)
attach(dt3)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

summary(Reservoir)
summary(Site)
summary(DataStream)
summary(TIMESTAMP_start)
summary(TIMESTAMP_end)
summary(start_parameter)
summary(end_parameter)
summary(flag)
summary(update_value)
summary(notes)
# Get more details on character variables

summary(as.factor(dt3$Reservoir))
summary(as.factor(dt3$DataStream))
summary(as.factor(dt3$start_parameter))
summary(as.factor(dt3$end_parameter))
summary(as.factor(dt3$flag))
summary(as.factor(dt3$update_value))
summary(as.factor(dt3$notes))
detach(dt3)



inUrl4  <- "https://pasta.lternet.edu/package/data/eml/edi/198/13/46a2ed26adbf7455fd0cd8488a8ea834"
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


dt4 <-read.csv(infile4,header=F
               ,skip=1
               ,sep=","
               , col.names=c(
                 "Reservoir",
                 "Site",
                 "Depth",
                 "DataStream",
                 "TIMESTAMP_start",
                 "TIMESTAMP_end",
                 "start_parameter",
                 "end_parameter",
                 "flag",
                 "update_value",
                 "notes"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$Reservoir)!="factor") dt4$Reservoir<- as.factor(dt4$Reservoir)
if (class(dt4$Site)=="factor") dt4$Site <-as.numeric(levels(dt4$Site))[as.integer(dt4$Site) ]
if (class(dt4$Site)=="character") dt4$Site <-as.numeric(dt4$Site)
if (class(dt4$Depth)=="factor") dt4$Depth <-as.numeric(levels(dt4$Depth))[as.integer(dt4$Depth) ]
if (class(dt4$Depth)=="character") dt4$Depth <-as.numeric(dt4$Depth)
if (class(dt4$DataStream)!="factor") dt4$DataStream<- as.factor(dt4$DataStream)
# attempting to convert dt4$TIMESTAMP_start dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d %H:%M:%S"
tmp4TIMESTAMP_start<-as.POSIXct(dt4$TIMESTAMP_start,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt4[dt4$TIMESTAMP_start != "",]) == length(tmp4TIMESTAMP_start[!is.na(tmp4TIMESTAMP_start)])){dt4$TIMESTAMP_start <- tmp4TIMESTAMP_start } else {print("Date conversion failed for dt4$TIMESTAMP_start. Please inspect the data and do the date conversion yourself.")}

# attempting to convert dt4$TIMESTAMP_end dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d %H:%M:%S"
tmp4TIMESTAMP_end<-as.POSIXct(dt4$TIMESTAMP_end,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt4[dt4$TIMESTAMP_end != "",]) == length(tmp4TIMESTAMP_end[!is.na(tmp4TIMESTAMP_end)])){dt4$TIMESTAMP_end <- tmp4TIMESTAMP_end } else {print("Date conversion failed for dt4$TIMESTAMP_end. Please inspect the data and do the date conversion yourself.")}

if (class(dt4$start_parameter)!="factor") dt4$start_parameter<- as.factor(dt4$start_parameter)
if (class(dt4$end_parameter)!="factor") dt4$end_parameter<- as.factor(dt4$end_parameter)
if (class(dt4$flag)!="factor") dt4$flag<- as.factor(dt4$flag)
if (class(dt4$update_value)!="factor") dt4$update_value<- as.factor(dt4$update_value)
if (class(dt4$notes)!="factor") dt4$notes<- as.factor(dt4$notes)

# Convert Missing Values to NA for non-dates

dt4$Reservoir <- as.factor(ifelse((trimws(as.character(dt4$Reservoir))==trimws("NA")),NA,as.character(dt4$Reservoir)))
dt4$Site <- ifelse((trimws(as.character(dt4$Site))==trimws("NA")),NA,dt4$Site)
suppressWarnings(dt4$Site <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$Site))==as.character(as.numeric("NA"))),NA,dt4$Site))
dt4$Depth <- ifelse((trimws(as.character(dt4$Depth))==trimws("NA")),NA,dt4$Depth)
suppressWarnings(dt4$Depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$Depth))==as.character(as.numeric("NA"))),NA,dt4$Depth))
dt4$DataStream <- as.factor(ifelse((trimws(as.character(dt4$DataStream))==trimws("NA")),NA,as.character(dt4$DataStream)))
dt4$start_parameter <- as.factor(ifelse((trimws(as.character(dt4$start_parameter))==trimws("NA")),NA,as.character(dt4$start_parameter)))
dt4$end_parameter <- as.factor(ifelse((trimws(as.character(dt4$end_parameter))==trimws("NA")),NA,as.character(dt4$end_parameter)))
dt4$flag <- as.factor(ifelse((trimws(as.character(dt4$flag))==trimws("NA")),NA,as.character(dt4$flag)))
dt4$update_value <- as.factor(ifelse((trimws(as.character(dt4$update_value))==trimws("NA")),NA,as.character(dt4$update_value)))
dt4$notes <- as.factor(ifelse((trimws(as.character(dt4$notes))==trimws("NA")),NA,as.character(dt4$notes)))


# Here is the structure of the input data frame:
str(dt4)
attach(dt4)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

summary(Reservoir)
summary(Site)
summary(Depth)
summary(DataStream)
summary(TIMESTAMP_start)
summary(TIMESTAMP_end)
summary(start_parameter)
summary(end_parameter)
summary(flag)
summary(update_value)
summary(notes)
# Get more details on character variables

summary(as.factor(dt4$Reservoir))
summary(as.factor(dt4$DataStream))
summary(as.factor(dt4$start_parameter))
summary(as.factor(dt4$end_parameter))
summary(as.factor(dt4$flag))
summary(as.factor(dt4$update_value))
summary(as.factor(dt4$notes))
detach(dt4)



inUrl5  <- "https://pasta.lternet.edu/package/data/eml/edi/198/13/5a1454801605a1237489e9c14d10ff2c"
infile5 <- tempfile()
try(download.file(inUrl5,infile5,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile5))) download.file(inUrl5,infile5,method="auto")


dt5 <-read.csv(infile5,header=F
               ,skip=1
               ,sep=","
               , col.names=c(
                 "Reservoir",
                 "Site",
                 "Site_description",
                 "Latitude",
                 "Longitude"    ), check.names=TRUE)

unlink(infile5)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt5$Reservoir)!="factor") dt5$Reservoir<- as.factor(dt5$Reservoir)
if (class(dt5$Site)=="factor") dt5$Site <-as.numeric(levels(dt5$Site))[as.integer(dt5$Site) ]
if (class(dt5$Site)=="character") dt5$Site <-as.numeric(dt5$Site)
if (class(dt5$Site_description)!="factor") dt5$Site_description<- as.factor(dt5$Site_description)
if (class(dt5$Latitude)=="factor") dt5$Latitude <-as.numeric(levels(dt5$Latitude))[as.integer(dt5$Latitude) ]
if (class(dt5$Latitude)=="character") dt5$Latitude <-as.numeric(dt5$Latitude)
if (class(dt5$Longitude)=="factor") dt5$Longitude <-as.numeric(levels(dt5$Longitude))[as.integer(dt5$Longitude) ]
if (class(dt5$Longitude)=="character") dt5$Longitude <-as.numeric(dt5$Longitude)

# Convert Missing Values to NA for non-dates

dt5$Reservoir <- as.factor(ifelse((trimws(as.character(dt5$Reservoir))==trimws("NA")),NA,as.character(dt5$Reservoir)))
dt5$Site <- ifelse((trimws(as.character(dt5$Site))==trimws("NA")),NA,dt5$Site)
suppressWarnings(dt5$Site <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt5$Site))==as.character(as.numeric("NA"))),NA,dt5$Site))
dt5$Site_description <- as.factor(ifelse((trimws(as.character(dt5$Site_description))==trimws("NA")),NA,as.character(dt5$Site_description)))
dt5$Latitude <- ifelse((trimws(as.character(dt5$Latitude))==trimws("NA")),NA,dt5$Latitude)
suppressWarnings(dt5$Latitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt5$Latitude))==as.character(as.numeric("NA"))),NA,dt5$Latitude))
dt5$Longitude <- ifelse((trimws(as.character(dt5$Longitude))==trimws("NA")),NA,dt5$Longitude)
suppressWarnings(dt5$Longitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt5$Longitude))==as.character(as.numeric("NA"))),NA,dt5$Longitude))


# Here is the structure of the input data frame:
str(dt5)
attach(dt5)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

summary(Reservoir)
summary(Site)
summary(Site_description)
summary(Latitude)
summary(Longitude)
# Get more details on character variables

summary(as.factor(dt5$Reservoir))
summary(as.factor(dt5$Site_description))
detach(dt5)






## SELCTING ONLY BEAVER DAM AND FALLING CREEK-HELEN BVR and FCR
library(tidyverse)
SecchiDepth <- dt1 |>
  filter(Reservoir == "BVR" | Reservoir == "FCR")

SensorData <- dt2 |>
  filter(Reservoir == "BVR" | Reservoir == "FCR")





