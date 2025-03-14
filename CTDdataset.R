# Package ID: edi.200.14 Cataloging System:https://pasta.edirepository.org.
# Data set title: Time series of high-frequency profiles of depth, temperature, dissolved oxygen, conductivity, specific conductance, chlorophyll a, turbidity, pH, oxidation-reduction potential, photosynthetically active radiation, colored dissolved organic matter, phycocyanin, phycoerythrin, and descent rate for Beaverdam Reservoir, Carvins Cove Reservoir, Falling Creek Reservoir, Gatewood Reservoir, and Spring Hollow Reservoir in southwestern Virginia, USA 2013-2023.
# Data set creator:  Cayelan Carey - Virginia Tech
# Data set creator:  Abigail Lewis - Virginia Tech
# Data set creator:  Adrienne Breef-Pilz - Virginia Tech
# Contact:  Cayelan Carey -  Virginia Tech  - cayelan@vt.edu
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())

# setwd("C:/users/my_name/my_dir")



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/14/0432a298a90b2b662f26c46071f66b8a"
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F
               ,skip=1
               ,sep=","
               , col.names=c(
                 "Reservoir",
                 "Site",
                 "SN",
                 "DateTime",
                 "Depth_m",
                 "Temp_C",
                 "DO_mgL",
                 "DOsat_percent",
                 "Cond_uScm",
                 "SpCond_uScm",
                 "Chla_ugL",
                 "Turbidity_NTU",
                 "pH",
                 "ORP_mV",
                 "PAR_umolm2s",
                 "CDOM_ugL",
                 "Phycoerythrin_ugL",
                 "Phycocyanin_ugL",
                 "DescRate_ms",
                 "Flag_DateTime",
                 "Flag_Temp_C",
                 "Flag_DO_mgL",
                 "Flag_DOsat_percent",
                 "Flag_Cond_uScm",
                 "Flag_SpCond_uScm",
                 "Flag_Chla_ugL",
                 "Flag_Turbidity_NTU",
                 "Flag_pH",
                 "Flag_ORP_mV",
                 "Flag_PAR_umolm2s",
                 "Flag_CDOM_ugL",
                 "Flag_Phycoerythrin_ugL",
                 "Flag_Phycocyanin_ugL",
                 "Flag_DescRate_ms"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Reservoir)!="factor") dt1$Reservoir<- as.factor(dt1$Reservoir)
if (class(dt1$Site)=="factor") dt1$Site <-as.numeric(levels(dt1$Site))[as.integer(dt1$Site) ]
if (class(dt1$Site)=="character") dt1$Site <-as.numeric(dt1$Site)
if (class(dt1$SN)!="factor") dt1$SN<- as.factor(dt1$SN)
# attempting to convert dt1$DateTime dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d %H:%M:%S"
tmp1DateTime<-as.POSIXct(dt1$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$DateTime != "",]) == length(tmp1DateTime[!is.na(tmp1DateTime)])){dt1$DateTime <- tmp1DateTime } else {print("Date conversion failed for dt1$DateTime. Please inspect the data and do the date conversion yourself.")}

if (class(dt1$Depth_m)=="factor") dt1$Depth_m <-as.numeric(levels(dt1$Depth_m))[as.integer(dt1$Depth_m) ]
if (class(dt1$Depth_m)=="character") dt1$Depth_m <-as.numeric(dt1$Depth_m)
if (class(dt1$Temp_C)=="factor") dt1$Temp_C <-as.numeric(levels(dt1$Temp_C))[as.integer(dt1$Temp_C) ]
if (class(dt1$Temp_C)=="character") dt1$Temp_C <-as.numeric(dt1$Temp_C)
if (class(dt1$DO_mgL)=="factor") dt1$DO_mgL <-as.numeric(levels(dt1$DO_mgL))[as.integer(dt1$DO_mgL) ]
if (class(dt1$DO_mgL)=="character") dt1$DO_mgL <-as.numeric(dt1$DO_mgL)
if (class(dt1$DOsat_percent)=="factor") dt1$DOsat_percent <-as.numeric(levels(dt1$DOsat_percent))[as.integer(dt1$DOsat_percent) ]
if (class(dt1$DOsat_percent)=="character") dt1$DOsat_percent <-as.numeric(dt1$DOsat_percent)
if (class(dt1$Cond_uScm)=="factor") dt1$Cond_uScm <-as.numeric(levels(dt1$Cond_uScm))[as.integer(dt1$Cond_uScm) ]
if (class(dt1$Cond_uScm)=="character") dt1$Cond_uScm <-as.numeric(dt1$Cond_uScm)
if (class(dt1$SpCond_uScm)=="factor") dt1$SpCond_uScm <-as.numeric(levels(dt1$SpCond_uScm))[as.integer(dt1$SpCond_uScm) ]
if (class(dt1$SpCond_uScm)=="character") dt1$SpCond_uScm <-as.numeric(dt1$SpCond_uScm)
if (class(dt1$Chla_ugL)=="factor") dt1$Chla_ugL <-as.numeric(levels(dt1$Chla_ugL))[as.integer(dt1$Chla_ugL) ]
if (class(dt1$Chla_ugL)=="character") dt1$Chla_ugL <-as.numeric(dt1$Chla_ugL)
if (class(dt1$Turbidity_NTU)=="factor") dt1$Turbidity_NTU <-as.numeric(levels(dt1$Turbidity_NTU))[as.integer(dt1$Turbidity_NTU) ]
if (class(dt1$Turbidity_NTU)=="character") dt1$Turbidity_NTU <-as.numeric(dt1$Turbidity_NTU)
if (class(dt1$pH)=="factor") dt1$pH <-as.numeric(levels(dt1$pH))[as.integer(dt1$pH) ]
if (class(dt1$pH)=="character") dt1$pH <-as.numeric(dt1$pH)
if (class(dt1$ORP_mV)=="factor") dt1$ORP_mV <-as.numeric(levels(dt1$ORP_mV))[as.integer(dt1$ORP_mV) ]
if (class(dt1$ORP_mV)=="character") dt1$ORP_mV <-as.numeric(dt1$ORP_mV)
if (class(dt1$PAR_umolm2s)=="factor") dt1$PAR_umolm2s <-as.numeric(levels(dt1$PAR_umolm2s))[as.integer(dt1$PAR_umolm2s) ]
if (class(dt1$PAR_umolm2s)=="character") dt1$PAR_umolm2s <-as.numeric(dt1$PAR_umolm2s)
if (class(dt1$CDOM_ugL)=="factor") dt1$CDOM_ugL <-as.numeric(levels(dt1$CDOM_ugL))[as.integer(dt1$CDOM_ugL) ]
if (class(dt1$CDOM_ugL)=="character") dt1$CDOM_ugL <-as.numeric(dt1$CDOM_ugL)
if (class(dt1$Phycoerythrin_ugL)=="factor") dt1$Phycoerythrin_ugL <-as.numeric(levels(dt1$Phycoerythrin_ugL))[as.integer(dt1$Phycoerythrin_ugL) ]
if (class(dt1$Phycoerythrin_ugL)=="character") dt1$Phycoerythrin_ugL <-as.numeric(dt1$Phycoerythrin_ugL)
if (class(dt1$Phycocyanin_ugL)=="factor") dt1$Phycocyanin_ugL <-as.numeric(levels(dt1$Phycocyanin_ugL))[as.integer(dt1$Phycocyanin_ugL) ]
if (class(dt1$Phycocyanin_ugL)=="character") dt1$Phycocyanin_ugL <-as.numeric(dt1$Phycocyanin_ugL)
if (class(dt1$DescRate_ms)=="factor") dt1$DescRate_ms <-as.numeric(levels(dt1$DescRate_ms))[as.integer(dt1$DescRate_ms) ]
if (class(dt1$DescRate_ms)=="character") dt1$DescRate_ms <-as.numeric(dt1$DescRate_ms)
if (class(dt1$Flag_DateTime)!="factor") dt1$Flag_DateTime<- as.factor(dt1$Flag_DateTime)
if (class(dt1$Flag_Temp_C)!="factor") dt1$Flag_Temp_C<- as.factor(dt1$Flag_Temp_C)
if (class(dt1$Flag_DO_mgL)!="factor") dt1$Flag_DO_mgL<- as.factor(dt1$Flag_DO_mgL)
if (class(dt1$Flag_DOsat_percent)!="factor") dt1$Flag_DOsat_percent<- as.factor(dt1$Flag_DOsat_percent)
if (class(dt1$Flag_Cond_uScm)!="factor") dt1$Flag_Cond_uScm<- as.factor(dt1$Flag_Cond_uScm)
if (class(dt1$Flag_SpCond_uScm)!="factor") dt1$Flag_SpCond_uScm<- as.factor(dt1$Flag_SpCond_uScm)
if (class(dt1$Flag_Chla_ugL)!="factor") dt1$Flag_Chla_ugL<- as.factor(dt1$Flag_Chla_ugL)
if (class(dt1$Flag_Turbidity_NTU)!="factor") dt1$Flag_Turbidity_NTU<- as.factor(dt1$Flag_Turbidity_NTU)
if (class(dt1$Flag_pH)!="factor") dt1$Flag_pH<- as.factor(dt1$Flag_pH)
if (class(dt1$Flag_ORP_mV)!="factor") dt1$Flag_ORP_mV<- as.factor(dt1$Flag_ORP_mV)
if (class(dt1$Flag_PAR_umolm2s)!="factor") dt1$Flag_PAR_umolm2s<- as.factor(dt1$Flag_PAR_umolm2s)
if (class(dt1$Flag_CDOM_ugL)!="factor") dt1$Flag_CDOM_ugL<- as.factor(dt1$Flag_CDOM_ugL)
if (class(dt1$Flag_Phycoerythrin_ugL)!="factor") dt1$Flag_Phycoerythrin_ugL<- as.factor(dt1$Flag_Phycoerythrin_ugL)
if (class(dt1$Flag_Phycocyanin_ugL)!="factor") dt1$Flag_Phycocyanin_ugL<- as.factor(dt1$Flag_Phycocyanin_ugL)
if (class(dt1$Flag_DescRate_ms)!="factor") dt1$Flag_DescRate_ms<- as.factor(dt1$Flag_DescRate_ms)

# Convert Missing Values to NA for non-dates

dt1$Depth_m <- ifelse((trimws(as.character(dt1$Depth_m))==trimws("NA")),NA,dt1$Depth_m)
suppressWarnings(dt1$Depth_m <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Depth_m))==as.character(as.numeric("NA"))),NA,dt1$Depth_m))
dt1$Temp_C <- ifelse((trimws(as.character(dt1$Temp_C))==trimws("NA")),NA,dt1$Temp_C)
suppressWarnings(dt1$Temp_C <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Temp_C))==as.character(as.numeric("NA"))),NA,dt1$Temp_C))
dt1$DO_mgL <- ifelse((trimws(as.character(dt1$DO_mgL))==trimws("NA")),NA,dt1$DO_mgL)
suppressWarnings(dt1$DO_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DO_mgL))==as.character(as.numeric("NA"))),NA,dt1$DO_mgL))
dt1$DOsat_percent <- ifelse((trimws(as.character(dt1$DOsat_percent))==trimws("NA")),NA,dt1$DOsat_percent)
suppressWarnings(dt1$DOsat_percent <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOsat_percent))==as.character(as.numeric("NA"))),NA,dt1$DOsat_percent))
dt1$Cond_uScm <- ifelse((trimws(as.character(dt1$Cond_uScm))==trimws("NA")),NA,dt1$Cond_uScm)
suppressWarnings(dt1$Cond_uScm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Cond_uScm))==as.character(as.numeric("NA"))),NA,dt1$Cond_uScm))
dt1$SpCond_uScm <- ifelse((trimws(as.character(dt1$SpCond_uScm))==trimws("NA")),NA,dt1$SpCond_uScm)
suppressWarnings(dt1$SpCond_uScm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SpCond_uScm))==as.character(as.numeric("NA"))),NA,dt1$SpCond_uScm))
dt1$Chla_ugL <- ifelse((trimws(as.character(dt1$Chla_ugL))==trimws("NA")),NA,dt1$Chla_ugL)
suppressWarnings(dt1$Chla_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Chla_ugL))==as.character(as.numeric("NA"))),NA,dt1$Chla_ugL))
dt1$Turbidity_NTU <- ifelse((trimws(as.character(dt1$Turbidity_NTU))==trimws("NA")),NA,dt1$Turbidity_NTU)
suppressWarnings(dt1$Turbidity_NTU <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Turbidity_NTU))==as.character(as.numeric("NA"))),NA,dt1$Turbidity_NTU))
dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("NA")),NA,dt1$pH)
suppressWarnings(dt1$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$pH))==as.character(as.numeric("NA"))),NA,dt1$pH))
dt1$ORP_mV <- ifelse((trimws(as.character(dt1$ORP_mV))==trimws("NA")),NA,dt1$ORP_mV)
suppressWarnings(dt1$ORP_mV <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$ORP_mV))==as.character(as.numeric("NA"))),NA,dt1$ORP_mV))
dt1$PAR_umolm2s <- ifelse((trimws(as.character(dt1$PAR_umolm2s))==trimws("NA")),NA,dt1$PAR_umolm2s)
suppressWarnings(dt1$PAR_umolm2s <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$PAR_umolm2s))==as.character(as.numeric("NA"))),NA,dt1$PAR_umolm2s))
dt1$CDOM_ugL <- ifelse((trimws(as.character(dt1$CDOM_ugL))==trimws("NA")),NA,dt1$CDOM_ugL)
suppressWarnings(dt1$CDOM_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$CDOM_ugL))==as.character(as.numeric("NA"))),NA,dt1$CDOM_ugL))
dt1$Phycoerythrin_ugL <- ifelse((trimws(as.character(dt1$Phycoerythrin_ugL))==trimws("NA")),NA,dt1$Phycoerythrin_ugL)
suppressWarnings(dt1$Phycoerythrin_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Phycoerythrin_ugL))==as.character(as.numeric("NA"))),NA,dt1$Phycoerythrin_ugL))
dt1$Phycocyanin_ugL <- ifelse((trimws(as.character(dt1$Phycocyanin_ugL))==trimws("NA")),NA,dt1$Phycocyanin_ugL)
suppressWarnings(dt1$Phycocyanin_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Phycocyanin_ugL))==as.character(as.numeric("NA"))),NA,dt1$Phycocyanin_ugL))
dt1$DescRate_ms <- ifelse((trimws(as.character(dt1$DescRate_ms))==trimws("NA")),NA,dt1$DescRate_ms)
suppressWarnings(dt1$DescRate_ms <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DescRate_ms))==as.character(as.numeric("NA"))),NA,dt1$DescRate_ms))


# Here is the structure of the input data frame:
str(dt1)
attach(dt1)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

summary(Reservoir)
summary(Site)
summary(SN)
summary(DateTime)
summary(Depth_m)
summary(Temp_C)
summary(DO_mgL)
summary(DOsat_percent)
summary(Cond_uScm)
summary(SpCond_uScm)
summary(Chla_ugL)
summary(Turbidity_NTU)
summary(pH)
summary(ORP_mV)
summary(PAR_umolm2s)
summary(CDOM_ugL)
summary(Phycoerythrin_ugL)
summary(Phycocyanin_ugL)
summary(DescRate_ms)
summary(Flag_DateTime)
summary(Flag_Temp_C)
summary(Flag_DO_mgL)
summary(Flag_DOsat_percent)
summary(Flag_Cond_uScm)
summary(Flag_SpCond_uScm)
summary(Flag_Chla_ugL)
summary(Flag_Turbidity_NTU)
summary(Flag_pH)
summary(Flag_ORP_mV)
summary(Flag_PAR_umolm2s)
summary(Flag_CDOM_ugL)
summary(Flag_Phycoerythrin_ugL)
summary(Flag_Phycocyanin_ugL)
summary(Flag_DescRate_ms)
# Get more details on character variables

summary(as.factor(dt1$Reservoir))
summary(as.factor(dt1$SN))
summary(as.factor(dt1$Flag_DateTime))
summary(as.factor(dt1$Flag_Temp_C))
summary(as.factor(dt1$Flag_DO_mgL))
summary(as.factor(dt1$Flag_DOsat_percent))
summary(as.factor(dt1$Flag_Cond_uScm))
summary(as.factor(dt1$Flag_SpCond_uScm))
summary(as.factor(dt1$Flag_Chla_ugL))
summary(as.factor(dt1$Flag_Turbidity_NTU))
summary(as.factor(dt1$Flag_pH))
summary(as.factor(dt1$Flag_ORP_mV))
summary(as.factor(dt1$Flag_PAR_umolm2s))
summary(as.factor(dt1$Flag_CDOM_ugL))
summary(as.factor(dt1$Flag_Phycoerythrin_ugL))
summary(as.factor(dt1$Flag_Phycocyanin_ugL))
summary(as.factor(dt1$Flag_DescRate_ms))
detach(dt1)



inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/200/14/61d9898e36d75efb6c6fcdf052e6e284"
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



inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/200/14/fc2f3f8b61e3d19090f38a55a23e88a6"
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F
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
                 "SN",
                 "notes"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$Reservoir)!="factor") dt3$Reservoir<- as.factor(dt3$Reservoir)
if (class(dt3$Site)=="factor") dt3$Site <-as.numeric(levels(dt3$Site))[as.integer(dt3$Site) ]
if (class(dt3$Site)=="character") dt3$Site <-as.numeric(dt3$Site)
if (class(dt3$Depth)=="factor") dt3$Depth <-as.numeric(levels(dt3$Depth))[as.integer(dt3$Depth) ]
if (class(dt3$Depth)=="character") dt3$Depth <-as.numeric(dt3$Depth)
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
if (class(dt3$flag)=="factor") dt3$flag <-as.numeric(levels(dt3$flag))[as.integer(dt3$flag) ]
if (class(dt3$flag)=="character") dt3$flag <-as.numeric(dt3$flag)
if (class(dt3$update_value)!="factor") dt3$update_value<- as.factor(dt3$update_value)
if (class(dt3$SN)!="factor") dt3$SN<- as.factor(dt3$SN)
if (class(dt3$notes)!="factor") dt3$notes<- as.factor(dt3$notes)

# Convert Missing Values to NA for non-dates

dt3$Reservoir <- as.factor(ifelse((trimws(as.character(dt3$Reservoir))==trimws("NA")),NA,as.character(dt3$Reservoir)))
dt3$Site <- ifelse((trimws(as.character(dt3$Site))==trimws("NA")),NA,dt3$Site)
suppressWarnings(dt3$Site <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Site))==as.character(as.numeric("NA"))),NA,dt3$Site))
dt3$Depth <- ifelse((trimws(as.character(dt3$Depth))==trimws("NA")),NA,dt3$Depth)
suppressWarnings(dt3$Depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$Depth))==as.character(as.numeric("NA"))),NA,dt3$Depth))
dt3$DataStream <- as.factor(ifelse((trimws(as.character(dt3$DataStream))==trimws("NA")),NA,as.character(dt3$DataStream)))
dt3$start_parameter <- as.factor(ifelse((trimws(as.character(dt3$start_parameter))==trimws("NA")),NA,as.character(dt3$start_parameter)))
dt3$end_parameter <- as.factor(ifelse((trimws(as.character(dt3$end_parameter))==trimws("NA")),NA,as.character(dt3$end_parameter)))
dt3$flag <- ifelse((trimws(as.character(dt3$flag))==trimws("NA")),NA,dt3$flag)
suppressWarnings(dt3$flag <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$flag))==as.character(as.numeric("NA"))),NA,dt3$flag))
dt3$update_value <- as.factor(ifelse((trimws(as.character(dt3$update_value))==trimws("NA")),NA,as.character(dt3$update_value)))
dt3$SN <- as.factor(ifelse((trimws(as.character(dt3$SN))==trimws("NA")),NA,as.character(dt3$SN)))
dt3$notes <- as.factor(ifelse((trimws(as.character(dt3$notes))==trimws("NA")),NA,as.character(dt3$notes)))


# Here is the structure of the input data frame:
str(dt3)
attach(dt3)
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
summary(SN)
summary(notes)
# Get more details on character variables

summary(as.factor(dt3$Reservoir))
summary(as.factor(dt3$DataStream))
summary(as.factor(dt3$start_parameter))
summary(as.factor(dt3$end_parameter))
summary(as.factor(dt3$update_value))
summary(as.factor(dt3$SN))
summary(as.factor(dt3$notes))
detach(dt3)


## SELCTING ONLY BEAVER DAM AND FALLING CREEK-HELEN BVR and FCR
library(tidyverse)
CTD <- dt1 |>
  filter(Reservoir == "BVR" | Reservoir == "FCR")





