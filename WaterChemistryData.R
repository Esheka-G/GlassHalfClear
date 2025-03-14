# Package ID: edi.199.12 Cataloging System:https://pasta.edirepository.org.
# Data set title: Water chemistry time series for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2023.
# Data set creator:  Cayelan Carey - Virginia Tech
# Data set creator:  Dexter Howard - Virginia Tech
# Data set creator:  Kathryn Hoffman - Virginia Tech
# Data set creator:  Heather Wander - Virginia Tech
# Data set creator:  Adrienne Breef-Pilz - Virginia Tech
# Data set creator:  B. Niederlehner - Virginia Tech
# Data set creator:  George Haynie - Virginia Tech
# Data set creator:  Ryan Keverline - Virginia Tech
# Data set creator:  Michael Kricheldorf - Virginia Tech
# Data set creator:  Evelyn Tipper - Virginia Tech
# Contact:  Cayelan Carey -  Virginia Tech  - Cayelan@vt.edu
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())

# setwd("C:/users/my_name/my_dir")



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/199/12/a33a5283120c56e90ea414e76d5b7ddb"
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
                 "Depth_m",
                 "Rep",
                 "TN_ugL",
                 "TP_ugL",
                 "NH4_ugL",
                 "NO3NO2_ugL",
                 "SRP_ugL",
                 "DOC_mgL",
                 "DIC_mgL",
                 "DC_mgL",
                 "DN_mgL",
                 "Flag_DateTime",
                 "Flag_TN_ugL",
                 "Flag_TP_ugL",
                 "Flag_NH4_ugL",
                 "Flag_NO3NO2_ugL",
                 "Flag_SRP_ugL",
                 "Flag_DOC_mgL",
                 "Flag_DIC_mgL",
                 "Flag_DC_mgL",
                 "Flag_DN_mgL"    ), check.names=TRUE)

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

if (class(dt1$Depth_m)=="factor") dt1$Depth_m <-as.numeric(levels(dt1$Depth_m))[as.integer(dt1$Depth_m) ]
if (class(dt1$Depth_m)=="character") dt1$Depth_m <-as.numeric(dt1$Depth_m)
if (class(dt1$Rep)=="factor") dt1$Rep <-as.numeric(levels(dt1$Rep))[as.integer(dt1$Rep) ]
if (class(dt1$Rep)=="character") dt1$Rep <-as.numeric(dt1$Rep)
if (class(dt1$TN_ugL)=="factor") dt1$TN_ugL <-as.numeric(levels(dt1$TN_ugL))[as.integer(dt1$TN_ugL) ]
if (class(dt1$TN_ugL)=="character") dt1$TN_ugL <-as.numeric(dt1$TN_ugL)
if (class(dt1$TP_ugL)=="factor") dt1$TP_ugL <-as.numeric(levels(dt1$TP_ugL))[as.integer(dt1$TP_ugL) ]
if (class(dt1$TP_ugL)=="character") dt1$TP_ugL <-as.numeric(dt1$TP_ugL)
if (class(dt1$NH4_ugL)=="factor") dt1$NH4_ugL <-as.numeric(levels(dt1$NH4_ugL))[as.integer(dt1$NH4_ugL) ]
if (class(dt1$NH4_ugL)=="character") dt1$NH4_ugL <-as.numeric(dt1$NH4_ugL)
if (class(dt1$NO3NO2_ugL)=="factor") dt1$NO3NO2_ugL <-as.numeric(levels(dt1$NO3NO2_ugL))[as.integer(dt1$NO3NO2_ugL) ]
if (class(dt1$NO3NO2_ugL)=="character") dt1$NO3NO2_ugL <-as.numeric(dt1$NO3NO2_ugL)
if (class(dt1$SRP_ugL)=="factor") dt1$SRP_ugL <-as.numeric(levels(dt1$SRP_ugL))[as.integer(dt1$SRP_ugL) ]
if (class(dt1$SRP_ugL)=="character") dt1$SRP_ugL <-as.numeric(dt1$SRP_ugL)
if (class(dt1$DOC_mgL)=="factor") dt1$DOC_mgL <-as.numeric(levels(dt1$DOC_mgL))[as.integer(dt1$DOC_mgL) ]
if (class(dt1$DOC_mgL)=="character") dt1$DOC_mgL <-as.numeric(dt1$DOC_mgL)
if (class(dt1$DIC_mgL)=="factor") dt1$DIC_mgL <-as.numeric(levels(dt1$DIC_mgL))[as.integer(dt1$DIC_mgL) ]
if (class(dt1$DIC_mgL)=="character") dt1$DIC_mgL <-as.numeric(dt1$DIC_mgL)
if (class(dt1$DC_mgL)=="factor") dt1$DC_mgL <-as.numeric(levels(dt1$DC_mgL))[as.integer(dt1$DC_mgL) ]
if (class(dt1$DC_mgL)=="character") dt1$DC_mgL <-as.numeric(dt1$DC_mgL)
if (class(dt1$DN_mgL)=="factor") dt1$DN_mgL <-as.numeric(levels(dt1$DN_mgL))[as.integer(dt1$DN_mgL) ]
if (class(dt1$DN_mgL)=="character") dt1$DN_mgL <-as.numeric(dt1$DN_mgL)
if (class(dt1$Flag_DateTime)!="factor") dt1$Flag_DateTime<- as.factor(dt1$Flag_DateTime)
if (class(dt1$Flag_TN_ugL)!="factor") dt1$Flag_TN_ugL<- as.factor(dt1$Flag_TN_ugL)
if (class(dt1$Flag_TP_ugL)!="factor") dt1$Flag_TP_ugL<- as.factor(dt1$Flag_TP_ugL)
if (class(dt1$Flag_NH4_ugL)!="factor") dt1$Flag_NH4_ugL<- as.factor(dt1$Flag_NH4_ugL)
if (class(dt1$Flag_NO3NO2_ugL)!="factor") dt1$Flag_NO3NO2_ugL<- as.factor(dt1$Flag_NO3NO2_ugL)
if (class(dt1$Flag_SRP_ugL)!="factor") dt1$Flag_SRP_ugL<- as.factor(dt1$Flag_SRP_ugL)
if (class(dt1$Flag_DOC_mgL)!="factor") dt1$Flag_DOC_mgL<- as.factor(dt1$Flag_DOC_mgL)
if (class(dt1$Flag_DIC_mgL)!="factor") dt1$Flag_DIC_mgL<- as.factor(dt1$Flag_DIC_mgL)
if (class(dt1$Flag_DC_mgL)!="factor") dt1$Flag_DC_mgL<- as.factor(dt1$Flag_DC_mgL)
if (class(dt1$Flag_DN_mgL)!="factor") dt1$Flag_DN_mgL<- as.factor(dt1$Flag_DN_mgL)

# Convert Missing Values to NA for non-dates

dt1$Reservoir <- as.factor(ifelse((trimws(as.character(dt1$Reservoir))==trimws("NA")),NA,as.character(dt1$Reservoir)))
dt1$Site <- ifelse((trimws(as.character(dt1$Site))==trimws("NA")),NA,dt1$Site)
suppressWarnings(dt1$Site <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Site))==as.character(as.numeric("NA"))),NA,dt1$Site))
dt1$Depth_m <- ifelse((trimws(as.character(dt1$Depth_m))==trimws("NA")),NA,dt1$Depth_m)
suppressWarnings(dt1$Depth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Depth_m))==as.character(as.numeric("NA"))),NA,dt1$Depth_m))
dt1$Rep <- ifelse((trimws(as.character(dt1$Rep))==trimws("NA")),NA,dt1$Rep)
suppressWarnings(dt1$Rep <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Rep))==as.character(as.numeric("NA"))),NA,dt1$Rep))
dt1$TN_ugL <- ifelse((trimws(as.character(dt1$TN_ugL))==trimws("NA")),NA,dt1$TN_ugL)
suppressWarnings(dt1$TN_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TN_ugL))==as.character(as.numeric("NA"))),NA,dt1$TN_ugL))
dt1$TP_ugL <- ifelse((trimws(as.character(dt1$TP_ugL))==trimws("NA")),NA,dt1$TP_ugL)
suppressWarnings(dt1$TP_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TP_ugL))==as.character(as.numeric("NA"))),NA,dt1$TP_ugL))
dt1$NH4_ugL <- ifelse((trimws(as.character(dt1$NH4_ugL))==trimws("NA")),NA,dt1$NH4_ugL)
suppressWarnings(dt1$NH4_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$NH4_ugL))==as.character(as.numeric("NA"))),NA,dt1$NH4_ugL))
dt1$NO3NO2_ugL <- ifelse((trimws(as.character(dt1$NO3NO2_ugL))==trimws("NA")),NA,dt1$NO3NO2_ugL)
suppressWarnings(dt1$NO3NO2_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$NO3NO2_ugL))==as.character(as.numeric("NA"))),NA,dt1$NO3NO2_ugL))
dt1$SRP_ugL <- ifelse((trimws(as.character(dt1$SRP_ugL))==trimws("NA")),NA,dt1$SRP_ugL)
suppressWarnings(dt1$SRP_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SRP_ugL))==as.character(as.numeric("NA"))),NA,dt1$SRP_ugL))
dt1$DOC_mgL <- ifelse((trimws(as.character(dt1$DOC_mgL))==trimws("NA")),NA,dt1$DOC_mgL)
suppressWarnings(dt1$DOC_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOC_mgL))==as.character(as.numeric("NA"))),NA,dt1$DOC_mgL))
dt1$DIC_mgL <- ifelse((trimws(as.character(dt1$DIC_mgL))==trimws("NA")),NA,dt1$DIC_mgL)
suppressWarnings(dt1$DIC_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DIC_mgL))==as.character(as.numeric("NA"))),NA,dt1$DIC_mgL))
dt1$DC_mgL <- ifelse((trimws(as.character(dt1$DC_mgL))==trimws("NA")),NA,dt1$DC_mgL)
suppressWarnings(dt1$DC_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DC_mgL))==as.character(as.numeric("NA"))),NA,dt1$DC_mgL))
dt1$DN_mgL <- ifelse((trimws(as.character(dt1$DN_mgL))==trimws("NA")),NA,dt1$DN_mgL)
suppressWarnings(dt1$DN_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DN_mgL))==as.character(as.numeric("NA"))),NA,dt1$DN_mgL))
dt1$Flag_DateTime <- as.factor(ifelse((trimws(as.character(dt1$Flag_DateTime))==trimws("NA")),NA,as.character(dt1$Flag_DateTime)))
dt1$Flag_TN_ugL <- as.factor(ifelse((trimws(as.character(dt1$Flag_TN_ugL))==trimws("NA")),NA,as.character(dt1$Flag_TN_ugL)))
dt1$Flag_TP_ugL <- as.factor(ifelse((trimws(as.character(dt1$Flag_TP_ugL))==trimws("NA")),NA,as.character(dt1$Flag_TP_ugL)))
dt1$Flag_NH4_ugL <- as.factor(ifelse((trimws(as.character(dt1$Flag_NH4_ugL))==trimws("NA")),NA,as.character(dt1$Flag_NH4_ugL)))
dt1$Flag_NO3NO2_ugL <- as.factor(ifelse((trimws(as.character(dt1$Flag_NO3NO2_ugL))==trimws("NA")),NA,as.character(dt1$Flag_NO3NO2_ugL)))
dt1$Flag_SRP_ugL <- as.factor(ifelse((trimws(as.character(dt1$Flag_SRP_ugL))==trimws("NA")),NA,as.character(dt1$Flag_SRP_ugL)))
dt1$Flag_DOC_mgL <- as.factor(ifelse((trimws(as.character(dt1$Flag_DOC_mgL))==trimws("NA")),NA,as.character(dt1$Flag_DOC_mgL)))
dt1$Flag_DIC_mgL <- as.factor(ifelse((trimws(as.character(dt1$Flag_DIC_mgL))==trimws("NA")),NA,as.character(dt1$Flag_DIC_mgL)))
dt1$Flag_DC_mgL <- as.factor(ifelse((trimws(as.character(dt1$Flag_DC_mgL))==trimws("NA")),NA,as.character(dt1$Flag_DC_mgL)))
dt1$Flag_DN_mgL <- as.factor(ifelse((trimws(as.character(dt1$Flag_DN_mgL))==trimws("NA")),NA,as.character(dt1$Flag_DN_mgL)))


# Here is the structure of the input data frame:
str(dt1)
attach(dt1)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

summary(Reservoir)
summary(Site)
summary(DateTime)
summary(Depth_m)
summary(Rep)
summary(TN_ugL)
summary(TP_ugL)
summary(NH4_ugL)
summary(NO3NO2_ugL)
summary(SRP_ugL)
summary(DOC_mgL)
summary(DIC_mgL)
summary(DC_mgL)
summary(DN_mgL)
summary(Flag_DateTime)
summary(Flag_TN_ugL)
summary(Flag_TP_ugL)
summary(Flag_NH4_ugL)
summary(Flag_NO3NO2_ugL)
summary(Flag_SRP_ugL)
summary(Flag_DOC_mgL)
summary(Flag_DIC_mgL)
summary(Flag_DC_mgL)
summary(Flag_DN_mgL)
# Get more details on character variables

summary(as.factor(dt1$Reservoir))
summary(as.factor(dt1$Flag_DateTime))
summary(as.factor(dt1$Flag_TN_ugL))
summary(as.factor(dt1$Flag_TP_ugL))
summary(as.factor(dt1$Flag_NH4_ugL))
summary(as.factor(dt1$Flag_NO3NO2_ugL))
summary(as.factor(dt1$Flag_SRP_ugL))
summary(as.factor(dt1$Flag_DOC_mgL))
summary(as.factor(dt1$Flag_DIC_mgL))
summary(as.factor(dt1$Flag_DC_mgL))
summary(as.factor(dt1$Flag_DN_mgL))
detach(dt1)



inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/199/12/827ba6e64c468dd35740db707de8873e"
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F
               ,skip=1
               ,sep=","
               , col.names=c(
                 "Reservoir",
                 "Site",
                 "Site_description",
                 "Latitude",
                 "Longitude"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Reservoir)!="factor") dt2$Reservoir<- as.factor(dt2$Reservoir)
if (class(dt2$Site)=="factor") dt2$Site <-as.numeric(levels(dt2$Site))[as.integer(dt2$Site) ]
if (class(dt2$Site)=="character") dt2$Site <-as.numeric(dt2$Site)
if (class(dt2$Site_description)!="factor") dt2$Site_description<- as.factor(dt2$Site_description)
if (class(dt2$Latitude)=="factor") dt2$Latitude <-as.numeric(levels(dt2$Latitude))[as.integer(dt2$Latitude) ]
if (class(dt2$Latitude)=="character") dt2$Latitude <-as.numeric(dt2$Latitude)
if (class(dt2$Longitude)=="factor") dt2$Longitude <-as.numeric(levels(dt2$Longitude))[as.integer(dt2$Longitude) ]
if (class(dt2$Longitude)=="character") dt2$Longitude <-as.numeric(dt2$Longitude)

# Convert Missing Values to NA for non-dates

dt2$Reservoir <- as.factor(ifelse((trimws(as.character(dt2$Reservoir))==trimws("NA")),NA,as.character(dt2$Reservoir)))
dt2$Site <- ifelse((trimws(as.character(dt2$Site))==trimws("NA")),NA,dt2$Site)
suppressWarnings(dt2$Site <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Site))==as.character(as.numeric("NA"))),NA,dt2$Site))
dt2$Site_description <- as.factor(ifelse((trimws(as.character(dt2$Site_description))==trimws("NA")),NA,as.character(dt2$Site_description)))
dt2$Latitude <- ifelse((trimws(as.character(dt2$Latitude))==trimws("NA")),NA,dt2$Latitude)
suppressWarnings(dt2$Latitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Latitude))==as.character(as.numeric("NA"))),NA,dt2$Latitude))
dt2$Longitude <- ifelse((trimws(as.character(dt2$Longitude))==trimws("NA")),NA,dt2$Longitude)
suppressWarnings(dt2$Longitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Longitude))==as.character(as.numeric("NA"))),NA,dt2$Longitude))


# Here is the structure of the input data frame:
str(dt2)
attach(dt2)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

summary(Reservoir)
summary(Site)
summary(Site_description)
summary(Latitude)
summary(Longitude)
# Get more details on character variables

summary(as.factor(dt2$Reservoir))
summary(as.factor(dt2$Site_description))
detach(dt2)


## SELCTING ONLY BEAVER DAM AND FALLING CREEK-HELEN BVR and FCR
library(tidyverse)
ChemistryData <- dt1 |>
  filter(Reservoir == "BVR" | Reservoir == "FCR")


