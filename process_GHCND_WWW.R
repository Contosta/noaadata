# Download and format GHCN daily precipitation (PRCP), maximum temeperature (TMAX), minimum temperature (TMIN),
# snowfall (SNOW), and snowdepth (SNWD)
# Project: Winter Weather Whiplash
# Original GHCN download code by Danielle Grogan for obtaining snow water equivalent data as part of 
# NSF MSB vernal windows analysis
# Revised code by Alix Contosta for obtaining and formatting daily weather data to support 
# identification of winter weather whiplash events
# last updated: 2019-06-19

#load libraries

library(data.table)
library(lubridate)
library(raster)
library(rgdal)
library(rgeos)
library(rnoaa)
library(stringr)
library(zoo)

######################################################################################################
#   Functions and required online data access 
######################################################################################################

# Function to download NCDC data (including GHCN data) from NOAA website
source("ncdc_ts.R")

# token to access NOAA data online. 
# Obtained from NCDC's Climate Data Online access token generator (http://www.ncdc.noaa.gov/cdo-web/token)
#my.token = "yfxXaGcvhjSfiMTxbHgcNFzKHtajTJjo"
#my.token = "FFDOTOgteLqSsxcdXqueAGTmomSfaspe"

#my.token = "SOcocOoAdqtGwMBQokCePiZSPHLplQVs"
#my.token = "UQukHLpMnMyPAzcOAojosWafjXqBAUBL"

#my.token = "FViTtvrszFvmyHufVMjJtLvHGafNFBvH"
my.token = "CClTsxbcVkxPlNoNbAhcWHhzZLOQQcjN"

#my.token = "IulLcWjJTyfnoRjtCUliaBXUpCufiUxK"
#my.token = "WaIJWipsgerfHvIVGWdKjrAZVTWKXHkS"
#my.token = "JWkGYNwyrgLUwLZeaGglUJXHmSXRwiHa"

#my.token = "txGmRofpgFxOUzOonEiGXUMCHVLmrdYI"
######################################################################################################
#   Idenfity GHCN stations within study domain with PRCP, TMAX, TMIN, SNOW, SNWD data
######################################################################################################

#use level II ecoregion classification to select northern and eastern temperate forest types in 
#North America. Defines spatial extent.

#read in shapefile 
study.region = readOGR(dsn = "Spatial_Domain/", layer = "NA_Eco_Level2_Clip_SSC")

#set the extent of the whole study region
ex = extent(study.region)

#divide extent into smaller units (ncdc only allows you to get metadata on 1000 sites at a time)

# loop through each small extent; get station metadata
for(i in 1:20){
  ymin.new = ex[3]+((i-1)/2)
  ymax.new = ex[3]+(i/2)
  ex.small = extent(ex[1], ex[2], ymin.new, ymax.new)
  
  st.meta.small = ncdc_stations(datasetid='GHCND',
                                extent = c(ex.small[3], ex.small[1], ex.small[4], ex.small[2]),
                                datatypeid = c("PRCP", "TMAX", "TMIN", "SNOW", "SNWD"), 
                                startdate = "1918-01-01",
                                limit = 1000,
                                token = my.token)
  
  if(i == 1){
    st.meta = st.meta.small$data
  }else{
    st.meta = rbind(st.meta, st.meta.small$data)
  }
  
}

#make min and max dates posix-compliant objects

st.meta$startdate = as.Date(st.meta$mindate)
st.meta$enddate = as.Date(st.meta$maxdate)

#calculate record length

#first pull out year from date

st.meta$startyear = as.numeric(strftime(st.meta$startdate, "%Y"))
st.meta$endyear = as.numeric(strftime(st.meta$enddate, "%Y"))

st.meta$reclen = as.numeric(st.meta$endyear - st.meta$startyear)

#split the name string to determine which sites are located in the US and which in Canada and add do data frame

sps.1 <- data.frame(do.call(rbind, str_split(st.meta$name, ",")))
names(sps.1) <- c("city", "location")

#omit rows 1344 and 1361
sps.1 = sps.1[-c(1344, 1361), ]
st.meta.1 = st.meta[-c(1344, 1361), ]
  
sps.2 = data.frame(do.call(rbind, str_split(sps.1$location, " ")))
sps.2 = sps.2[ , -(1:4)]

names(sps.2) = c("ST", "NAT")

st.meta.2 = cbind(sps.2, st.meta.1)

#subset for US and Canadian stations to get an idea of overall record length and completeness

st.meta.us = st.meta.2[st.meta.2$NAT == "US", ]
st.meta.ca = st.meta.2[st.meta.2$NAT == "CA", ]

#omit stations with records that do not start before 1928, end before 2018, and have > 90% record completeness
st.meta.2.sub.1 = st.meta.2[st.meta.2$startdate < as.Date("1928-01-01") & st.meta.2$enddate > as.Date("2018-12-31")
                      & st.meta.2$datacoverage >= 0.9, ]

#omit stations with records that do not start before 1958, end before 2018, and have > 90% record completeness
st.meta.2.sub.2 = st.meta.2[st.meta.2$startdate < as.Date("1958-01-01") & st.meta.2$enddate > as.Date("2018-12-31")
                            & st.meta.2$datacoverage >= 0.9, ]

#omit stations with records that do not start before 1958, end before 2018, and have > 90% record completeness
st.meta.2.sub.3 = st.meta.2[st.meta.2$startdate < as.Date("1988-01-01") & st.meta.2$enddate > as.Date("2018-12-31")
                            & st.meta.2$datacoverage >= 0.9, ]


#export table

write.table(st.meta.2.sub.3, file = paste("ghcn_station_WWW_metadata.csv"), sep=",", append=FALSE, 
            col.names=TRUE, row.names = FALSE)

######################################################################################################
#   Download and save all GHCN data within the study domain that meets metadata parameters
######################################################################################################
st.meta.sub = read.table("ghcn_station_WWW_metadata.csv", head = T, sep = ",")

files.1 = list.files(path = "TMINN_Daily/",full.names = T)

datasetid  ='GHCND'
datatypeid = c("TMIN")#, "TMAX", "TMAX", "TMIN", "TMAX", "TMAX")
            #TMAX = daily total precipitation
            #TMAX = daily maximum temperature
            #TMIN = daily minimum temperature
            #TMAX = daily total TMAXfall
            #TMAX = daily total TMAX depth

yrs = seq(1928, 2018)  # years to download

for(i in 700:nrow(st.meta.sub)){

  st.id = sub("GHCND:", "", st.meta.sub$id[i])
    
  f1 = sum(grepl(paste("GHCND_", st.id, "_TMIN_Daily.csv", sep=""), c(files.1)))
  
  stationid = as.character(st.meta.sub$id[i])

  if(f1 == 0){
    out.nm    = paste("TMINN_Daily/", 
                      sub(":", "_", stationid), 
                      "_", 
                      "TMIN_Daily", 
                      ".csv", 
                      sep="")
    ncdc_ts(datasetid, stationid, datatypeid, yrs, my.token, out.nm)
    print(i)
  
    }else{
    text = paste(i, "file exists")
    print(text)
  }


}

######################################################################################################
#Stitch files together so that all variables are in same file
######################################################################################################

#call file names in different subdirectories
files.PRCP = list.files(path = "PRCP_Daily/",full.names = T)
files.SNOW = list.files(path = "SNOW_Daily/",full.names = T)
files.SNWD = list.files(path = "SNWD_Daily/",full.names = T)
files.TMAX = list.files(path = "TMAX_Daily/",full.names = T)
files.TMIN = list.files(path = "TMINN_Daily/",full.names = T)

#split file names to isolate just the station ID

#for PRCP
PRCP_spl.1 = data.frame(do.call(rbind, str_split(files.PRCP, "//")))
names(PRCP_spl.1) = c("fdir", "fname")
PRCP_spl.2 = data.frame(do.call(rbind, str_split(PRCP_spl.1$fname, ".csv")))
names(PRCP_spl.2) = c("fname", "ftype")
PRCP_spl.3 = data.frame(do.call(rbind, str_split(PRCP_spl.2$fname, "_")))
names(PRCP_spl.3) = c("PRCP_fdat", "PRCP_fid", "PRCP_fvar", "PRCP_fts")


#for SNOW
SNOW_spl.1 = data.frame(do.call(rbind, str_split(files.SNOW, "//")))
names(SNOW_spl.1) = c("fdir", "fname")
SNOW_spl.2 = data.frame(do.call(rbind, str_split(SNOW_spl.1$fname, ".csv")))
names(SNOW_spl.2) = c("fname", "ftype")
SNOW_spl.3 = data.frame(do.call(rbind, str_split(SNOW_spl.2$fname, "_")))
names(SNOW_spl.3) = c("SNOW_fdat", "SNOW_fid", "SNOW_fvar", "SNOW_fts")

#for SNWD
SNWD_spl.1 = data.frame(do.call(rbind, str_split(files.SNWD, "//")))
names(SNWD_spl.1) = c("fdir", "fname")
SNWD_spl.2 = data.frame(do.call(rbind, str_split(SNWD_spl.1$fname, ".csv")))
names(SNWD_spl.2) = c("fname", "ftype")
SNWD_spl.3 = data.frame(do.call(rbind, str_split(SNWD_spl.2$fname, "_")))
names(SNWD_spl.3) = c("SNWD_fdat", "SNWD_fid", "SNWD_fvar", "SNWD_fts")

#for TMAX
TMAX_spl.1 = data.frame(do.call(rbind, str_split(files.TMAX, "//")))
names(TMAX_spl.1) = c("fdir", "fname")
TMAX_spl.2 = data.frame(do.call(rbind, str_split(TMAX_spl.1$fname, ".csv")))
names(TMAX_spl.2) = c("fname", "ftype")
TMAX_spl.3 = data.frame(do.call(rbind, str_split(TMAX_spl.2$fname, "_")))
names(TMAX_spl.3) = c("TMAX_fdat", "TMAX_fid", "TMAX_fvar", "TMAX_fts")

#for TMIN
TMIN_spl.1 = data.frame(do.call(rbind, str_split(files.TMIN, "//")))
names(TMIN_spl.1) = c("fdir", "fname")
TMIN_spl.2 = data.frame(do.call(rbind, str_split(TMIN_spl.1$fname, ".csv")))
names(TMIN_spl.2) = c("fname", "ftype")
TMIN_spl.3 = data.frame(do.call(rbind, str_split(TMIN_spl.2$fname, "_")))
names(TMIN_spl.3) = c("TMIN_fdat", "TMIN_fid", "TMIN_fvar", "TMIN_fts")

#stitch together master list
PRCP_SNWD = merge(PRCP_spl.3, SNWD_spl.3, by.x = "PRCP_fid", by.y = "SNWD_fid")
PRCP_SNWD_SNOW = merge(PRCP_SNWD, SNOW_spl.3, by.x = "PRCP_fid", by.y = "SNOW_fid")
TMAX_TMIN = merge(TMAX_spl.3, TMIN_spl.3, by.x = "TMAX_fid", by.y = "TMIN_fid")

st.all = merge(PRCP_SNWD_SNOW, TMAX_TMIN, by.x = "PRCP_fid", by.y = "TMAX_fid")

#loop through all four directories to match data from the same stations and paste together in long form using 
#rbind

for(i in seq(nrow(st.all))){
    
  st.PRCP[i] = paste("PRCP_Daily//GHCND_", st.all$PRCP_fid[i], "_PRCP_Daily.csv", sep = "")
  st.SNWD[i] = paste("SNWD_Daily//GHCND_", st.all$PRCP_fid[i], "_SNWD_Daily.csv", sep = "")
  st.SNOW[i] = paste("SNOW_Daily//GHCND_", st.all$PRCP_fid[i], "_SNOW_Daily.csv", sep = "")
  st.TMAX[i] = paste("TMAX_Daily//GHCND_", st.all$PRCP_fid[i], "_TMAX_Daily.csv", sep = "")
  st.TMIN[i] = paste("TMINN_Daily//GHCND_", st.all$PRCP_fid[i], "_TMIN_Daily.csv", sep = "")
  
  PRCP.dat = read.table(st.PRCP[i], header = T, sep = ",")
  SNOW.dat = read.table(st.SNOW[i], header = T, sep = ",")
  SNWD.dat = read.table(st.SNWD[i], header = T, sep = ",")
  TMAX.dat = read.table(st.TMAX[i], header = T, sep = ",")
  TMIN.dat = read.table(st.TMIN[i], header = T, sep = ",")
  
  alldat = rbind(PRCP.dat, SNOW.dat, SNWD.dat, TMAX.dat, TMIN.dat)
  
  all.nm  = paste("COMB_Daily/", "GHCN_", st.all$PRCP_fid[i], ".csv", sep="")
  
  write.csv(alldat, all.nm, row.names = F)
  
  }


######################################################################################################
#Transpose files from long into wide format
######################################################################################################

files.COMB = list.files(path = "COMB_Daily/",full.names = T)


# start the loop to go through the whole directory
for (i in seq(length(files.COMB))) {

    #read in file
    ex = read.table(files.COMB[i], header = TRUE, sep = ',',)

    #extract station ID
    station = unique(ex$station)

    #subset file to contain different dataframes for PRCP, SNOW, SNWD, TMAX, and TMIN.
  
    PRCP.1 = ex[ex$datatype == "PRCP", ]
    PRCP.2 = PRCP.1[ , c("date", "station", "value", "fl_m", "fl_q", "fl_so", "fl_t")]
    names(PRCP.2) = c("date", "PRCP_station", "PRCP", "PRCP_fl_m", "PRCP_fl_q", "PRCP_fl_so", "PRCP_fl_t" )

    SNWD.1 = ex[ex$datatype == "SNWD", ]
    SNWD.2 = SNWD.1[ , c("date", "station", "value", "fl_m", "fl_q", "fl_so", "fl_t")]
    names(SNWD.2) = c("date", "SNWD_station", "SNWD", "SNWD_fl_m", "SNWD_fl_q", "SNWD_fl_so", "SNWD_fl_t" )
    
    SNOW.1 = ex[ex$datatype == "SNOW", ]
    SNOW.2 = SNOW.1[ , c("date", "station", "value", "fl_m", "fl_q", "fl_so", "fl_t")]
    names(SNOW.2) = c("date",  "SNOW_station", "SNOW", "SNOW_fl_m", "SNOW_fl_q", "SNOW_fl_so", "SNOW_fl_t" )

    TMAX.1 = ex[ex$datatype == "TMAX", ]
    TMAX.2 = TMAX.1[ , c("date", "station", "value", "fl_m", "fl_q", "fl_so", "fl_t")]
    names(TMAX.2) = c("date", "TMAX_station", "TMAX", "TMAX_fl_m", "TMAX_fl_q", "TMAX_fl_so", "TMAX_fl_t" )

    TMIN.1 = ex[ex$datatype == "TMIN", ]
    TMIN.2 = TMIN.1[ , c("date", "station", "value", "fl_m", "fl_q", "fl_so", "fl_t")]
    names(TMIN.2) = c("date", "TMIN_station", "TMIN", "TMIN_fl_m", "TMIN_fl_q", "TMIN_fl_so", "TMIN_fl_t" )

    #join separate tables together with date as identifier

    met.1 = merge(PRCP.2, SNOW.2, by.x = "date", by.y = "date", all.x =T, all.y = T)
    met.2 = merge(met.1, SNWD.2, by.x = "date", by.y = "date", all.x =T, all.y = T)
    met.3 = merge(met.2, TMAX.2, by.x = "date", by.y = "date", all.x =T, all.y = T)
    met.4 = merge(met.3, TMIN.2, by.x = "date", by.y = "date", all.x =T, all.y = T)

    #create unique station column
    met.4$station = station
    
    #write wide form table to Daily_Reformat
    
    out_name = paste("Daily_Reformat/", 
                     sub(":", "_", station), 
                     "_", 
                     "Wide_Daily", 
                     ".csv", 
                     sep="")
   
    
    write.csv(met.4, out_name, row.names = F)
      
}
    
######################################################################################################
#  Evaulate stations for record completeness
######################################################################################################

#allmet.1 = met.4

#create object listing daily data files that have been converted into wide format (Daily_Reformat)
files.REF = list.files(path = "Daily_Reformat/",full.names = T)

#create blank dataframe for pasting in record completeness stats for stations that pass record completeness screening
finrec = data.frame(
  station = NA,
  PRCPna = NA,
  PRCPlo = NA,
  PRCPhi = NA,
  SNWDna = NA,
  SNWDlo = NA,
  SNWDhi = NA,
  SNOWna = NA,
  SNOWlo = NA,
  SNOWhi = NA,
  TMAXna = NA,
  TMAXlo = NA,
  TMAXhi = NA,
  TMINna = NA,
  TMINlo = NA,
  TMINhi = NA
)

#Screen stations that have large amounts of missing values following Huntington et al. (2004) 
#and Feng and Hu (2007)

# start the loop to go through the whole Daily_Reformat directory
for (i in seq(length(files.REF))) {
  
  #read in file
  allmet.1 = read.csv(files.REF[i], header = TRUE, sep = ',',)
  
  #extract station ID
  station = unique(allmet.1$station)
  
  #split date into two strings and then add to dataframe
  dt = data.frame(do.call(rbind, str_split(allmet.1$date, "T")))
  names(dt) = c("DATE", "TIME")
  allmet.2 = cbind(dt, allmet.1)

  #convert Date column into a posix-compliant object
  allmet.2$DATETIME = as.POSIXct(strptime(allmet.2$DATE, "%Y-%m-%d"))
  
  #extract values for year, month, and day of year and add to dataframe
  allmet.2$year = as.integer(strftime(allmet.2$DATETIME, "%Y"))
  allmet.2$month = as.integer(strftime(allmet.2$DATETIME, "%m"))
  allmet.2$doy = as.integer(strftime(allmet.2$DATETIME, "%j"))

  #Determine whether there are 5 days of missing data per month or 10 days of missing data per year
  #Flag NA values

   allmet.2$PRCPna = ifelse(is.na(allmet.2$PRCP) == T, 1, 0)
   allmet.2$SNWDna = ifelse(is.na(allmet.2$SNWD) == T, 1, 0)
   allmet.2$SNOWna = ifelse(is.na(allmet.2$SNOW) == T, 1, 0)
   allmet.2$TMAXna = ifelse(is.na(allmet.2$TMAX) == T, 1, 0)
   allmet.2$TMINna = ifelse(is.na(allmet.2$TMIN) == T, 1, 0)
  
   #Create Year_Month (ym) column for data reduction
   allmet.2$ym =  paste(allmet.2$year, allmet.2$month, sep = " ")
  
   #make data.frame a data.table
   metsum = data.table(allmet.2)
  
   #calculate nrows that do not have data for each by year by month combination (for determining missingness within a month)
   metsum.1 = as.data.frame(metsum[, list(PRCPmo = sum(PRCPna), SNWDmo = sum(SNWDna), SNOWmo = sum(SNOWna),
                                         TMAXmo = sum(TMAXna), TMINmo = sum(TMINna)), by = ym])
  
   #calculate nrows that do or do not have data for each by year (for determining missingness with a year)
   metsum.2 = as.data.frame(metsum[, list(PRCPyr = sum(PRCPna), SNWDyr = sum(SNWDna), SNOWyr = sum(SNOWna),
                                         TMAXyr = sum(TMAXna), TMINyr = sum(TMINna)), by = year])
  
   #merge with allmet.2
   metmo = merge(allmet.2, metsum.1, by.x = "ym", by.y = "ym", all.x = TRUE, all.y = TRUE)
   metyr = merge(metmo, metsum.2, by.x = "year", by.y = "year", all.x = TRUE, all.y = TRUE)
  
   #remove PRCP, SNWD, SNOW, TMAX, and TMIN values where there are gaps > 5 observations per month and/or 10 observations per year
   metyr$PRCPcorr = ifelse(metyr$PRCPmo > 5 | metyr$PRCPyr > 10, NA, metyr$PRCP)
   metyr$SNWDcorr = ifelse(metyr$SNWDmo > 5 | metyr$SNWDyr > 10, NA, metyr$SNWD)
   metyr$SNOWcorr = ifelse(metyr$SNOWmo > 5 | metyr$SNOWyr > 10, NA, metyr$SNOW)
   metyr$TMAXcorr = ifelse(metyr$TMAXmo > 5 | metyr$TMAXyr > 10, NA, metyr$TMAX)
   metyr$TMINcorr = ifelse(metyr$TMINmo > 5 | metyr$TMINyr > 10, NA, metyr$TMIN)
  
   #flag values that are NA for determine completeness of records for each year
   metyr$PRCPna = ifelse(is.na(metyr$PRCPcorr) == T, 1, 0)
   metyr$SNWDna = ifelse(is.na(metyr$SNWDcorr) == T, 1, 0)
   metyr$SNOWna = ifelse(is.na(metyr$SNOWcorr) == T, 1, 0)
   metyr$TMAXna = ifelse(is.na(metyr$TMAXcorr) == T, 1, 0)
   metyr$TMINna = ifelse(is.na(metyr$TMINcorr) == T, 1, 0)
  
   #determine years with and without complete records
   reccomp = data.table(metyr)
  
   #first total number of incomplete records per year
   recomp.1 = as.data.frame(reccomp[ , list(PRCPna = sum(PRCPna, na.rm = TRUE),
                                           SNWDna = sum(SNWDna, na.rm = TRUE),
                                           SNOWna = sum(SNOWna, na.rm = TRUE),
                                           TMAXna = sum(TMAXna, na.rm = TRUE),
                                           TMINna = sum(TMINna, na.rm = TRUE)), by = year])
  
   #flag years when total #na's is > 10
   recomp.1$PRCPyna = ifelse(recomp.1$PRCPna > 10, 1, 0)
   recomp.1$SNWDyna = ifelse(recomp.1$SNWDna > 10, 1, 0)
   recomp.1$SNOWyna = ifelse(recomp.1$SNOWna > 10, 1, 0)
   recomp.1$TMAXyna = ifelse(recomp.1$TMAXna > 10, 1, 0)
   recomp.1$TMINyna = ifelse(recomp.1$TMINna > 10, 1, 0)
  
   #flag years when corr values are NA to determine when measurement period begins and ends
   #metyr$PRCPs = ifelse(is.na(metyr$PRCPcorr) == T, NA, metyr$year)
   #metyr$SNWDs = ifelse(is.na(metyr$SNWDcorr) == T, NA, metyr$year)
   #metyr$SNOWs = ifelse(is.na(metyr$SNOWcorr) == T, NA, metyr$year)
   #metyr$TMAXs = ifelse(is.na(metyr$TMAXcorr) == T, NA, metyr$year)
   #metyr$TMINs = ifelse(is.na(metyr$TMINcorr) == T, NA, metyr$year)
  
   #determine length of record for each variable / station combination
   #reclen = data.table(metyr)
  
   #first calculate start and end of record
   #reclen.1 = as.data.frame(reclen[ , list(PRCPlo = min(PRCPs, na.rm = TRUE), PRCPhi = max(PRCPs, na.rm = TRUE),
  #                                        SNWDlo = min(SNWDs, na.rm = TRUE), SNWDhi = max(SNWDs, na.rm = TRUE),
   #                                       SNOWlo = min(SNOWs, na.rm = TRUE), SNOWhi = max(SNOWs, na.rm = TRUE),
    #                                      TMAXlo = min(TMAXs, na.rm = TRUE), TMAXhi = max(TMAXs, na.rm = TRUE),
     #                                     TMINlo = min(TMINs, na.rm = TRUE), TMINhi = max(TMINs, na.rm = TRUE))])
  #
   #merge reclen.1 with recomp.2
   #recomp.2 = merge(recomp.1, reclen.1, all.x = TRUE, all.y = TRUE)
  
   #create year flag to only calculate rolling sums within period of record
   #recomp.2$PRCPFlag = ifelse(recomp.2$year >= recomp.2$PRCPlo & recomp.2$year <= recomp.2$PRCPhi, 1, 0)
   #recomp.2$SNWDFlag = ifelse(recomp.2$year >= recomp.2$SNWDlo & recomp.2$year <= recomp.2$SNWDhi, 1, 0)
   #recomp.2$SNOWFlag = ifelse(recomp.2$year >= recomp.2$SNOWlo & recomp.2$year <= recomp.2$SNOWhi, 1, 0)
   #recomp.2$TMAXFlag = ifelse(recomp.2$year >= recomp.2$TMAXlo & recomp.2$year <= recomp.2$TMAXhi, 1, 0)
   #recomp.2$TMINFlag = ifelse(recomp.2$year >= recomp.2$TMINlo & recomp.2$year <= recomp.2$TMINhi, 1, 0)
  
   #calculate rolling sums of incomplete data years over 10 year increments
   recomp.1$PRCPteny = ave(recomp.1$PRCPyna, FUN= function(x) rollsum(x, k=10, na.pad=T))
   
   recomp.1$SNWDteny = ave(recomp.1$SNWDyna,FUN= function(x) rollsum(x, k=10, na.pad=T))
   recomp.1$SNOWteny = ave(recomp.1$SNOWyna, FUN= function(x) rollsum(x, k=10, na.pad=T))
   recomp.1$TMAXteny = ave(recomp.1$TMAXyna, FUN= function(x) rollsum(x, k=10, na.pad=T))
   recomp.1$TMINteny = ave(recomp.1$TMINyna, FUN= function(x) rollsum(x, k=10, na.pad=T))
  
   #recomp.1$SNWDteny = ifelse(recomp.1$SNWDFlag == 1, ave(recomp.1$SNWDyna, 
  #                                                      FUN= function(x) rollsum(x, k=10, na.pad=T) ), NA)
   
   #calculate the maximum number of incomplete 10-year increments
   recomp.2 = data.table(recomp.1)
  
   recomp.3 = recomp.2[ , list(PRCPgap = max(PRCPteny, na.rm = TRUE),
                              SNWDgap = max(SNWDteny, na.rm = TRUE),
                              SNOWgap = max(SNOWteny, na.rm = TRUE),
                              TMAXgap = max(TMAXteny, na.rm = TRUE),
                              TMINgap = max(TMINteny, na.rm = TRUE))]
  
   #flag sites where maxgap > 5 (this would be > 50% data incompleteness over a 10 year period)
   recomp.3$PRCPFlag = ifelse(is.infinite(recomp.3$PRCPgap) == T | 
                                recomp.3$PRCPgap > 5, 1, 0)
   recomp.3$SNWDFlag = ifelse(is.infinite(recomp.3$SNWDgap) == T | 
                                recomp.3$SNWDgap > 5, 1, 0)
   recomp.3$SNOWFlag = ifelse(is.infinite(recomp.3$SNOWgap) == T |  
                                recomp.3$SNOWgap > 5, 1, 0)
   recomp.3$TMAXFlag = ifelse(is.infinite(recomp.3$TMAXgap) == T | 
                                recomp.3$TMAXgap > 5, 1, 0)
   recomp.3$TMINFlag = ifelse(is.infinite(recomp.3$TMINgap) == T | 
                        recomp.3$TMINgap > 5, 1, 0)
  
   #merge with metyr
   metrec = merge(metyr, recomp.3, all.x = TRUE, all.y = TRUE)
  
   #omit values from stations that did not reach threshold for record completeness
   metrec$PRCPfin = ifelse(metrec$PRCPFlag == 1, NA, metrec$PRCPcorr)
   metrec$SNWDfin = ifelse(metrec$SNWDFlag == 1, NA, metrec$SNWDcorr)
   metrec$SNOWfin = ifelse(metrec$SNOWFlag == 1, NA, metrec$SNOWcorr)
   metrec$TMAXfin = ifelse(metrec$TMAXFlag == 1, NA, metrec$TMAXcorr)
   metrec$TMINfin = ifelse(metrec$TMINFlag == 1, NA, metrec$TMINcorr)
  
   #obtain stats on record completeness
   #flag years when fin values are NA
   metrec$PRCPna = ifelse(is.na(metrec$PRCPfin) == T, 1, 0)
   metrec$SNWDna = ifelse(is.na(metrec$SNWDfin) == T, 1, 0)
   metrec$SNOWna = ifelse(is.na(metrec$SNOWfin) == T, 1, 0)
   metrec$TMAXna = ifelse(is.na(metrec$TMAXfin) == T, 1, 0)
   metrec$TMINna = ifelse(is.na(metrec$TMINfin) == T, 1, 0)
   
   #flag station if it does not pass record completeness based on PRCP, TMAX, and TMIN
   stationNA = unique(ifelse(metrec$PRCPFlag == 1 | metrec$TMAXFlag == 1 | metrec$TMINFlag == 1, 1, 2))
   
   #if passsed, write station name, record length, and start and end to finrec
   #subset columns to only include data
   #write csv to Daily_Complete folder
   
   if(stationNA == 2) {
     
      #calculate start and end of record and number of missing years
      metrec.1 = data.table(metrec)
      finrec.1 = as.data.frame(metrec.1[ , list(station = unique(station), 
                              PRCPna = sum(PRCPna),
                              SNWDna = sum(SNWDna),
                              SNOWna = sum(SNOWna),
                              TMAXna = sum(TMAXna),
                              TMINna = sum(TMINna)), by = year])
                              
      #flag years where there are >=365 missing records
      finrec.1$PRCPin = ifelse(finrec.1$PRCPna >= 365, NA, finrec.1$year)
      finrec.1$SNOWin = ifelse(finrec.1$SNOWna >= 365, NA, finrec.1$year)
      finrec.1$SNWDin = ifelse(finrec.1$SNWDna >= 365, NA, finrec.1$year)
      finrec.1$TMAXin = ifelse(finrec.1$TMAXna >= 365, NA, finrec.1$year)
      finrec.1$TMINin = ifelse(finrec.1$TMINna >= 365, NA, finrec.1$year)
      
     
      
      #write station, record length, start, and end to finrec
       finrec[i , 1] = station[1]
       finrec[i, 2] = nrow(finrec.1[complete.cases(finrec.1$PRCPin), ])
       finrec[i, 3] = ifelse(unique(is.na(as.integer(min(finrec.1$PRCPin, na.rm =T) == T))), NA,
                            as.integer(min(finrec.1$PRCPin, na.rm = T))) 
       finrec[i, 4] = ifelse(unique(is.na(as.integer(max(finrec.1$PRCPin, na.rm =T) == T))), NA,
                            as.integer(max(finrec.1$PRCPin, na.rm = T)))
       finrec[i, 5] = nrow(finrec.1[complete.cases(finrec.1$SNWDin), ])
       finrec[i, 6] = ifelse(unique(is.na(as.integer(min(finrec.1$SNWDin, na.rm =T) == T))), NA,
                            as.integer(min(finrec.1$SNWDin, na.rm = T)))  
       finrec[i, 7] = ifelse(unique(is.na(as.integer(max(finrec.1$SNWDin, na.rm =T) == T))), NA,
                             as.integer(max(finrec.1$SNWDin, na.rm = T))) 
       finrec[i, 8] = nrow(finrec.1[complete.cases(finrec.1$SNOWin), ])
       finrec[i, 9] = ifelse(unique(is.na(as.integer(min(finrec.1$SNOWin, na.rm =T) == T))), NA,
                             as.integer(min(finrec.1$SNOWin, na.rm = T))) 
       finrec[i, 10] = ifelse(unique(is.na(as.integer(max(finrec.1$SNOWin, na.rm =T) == T))), NA,
                              as.integer(max(finrec.1$SNOWin, na.rm = T))) 
       finrec[i, 11] = nrow(finrec.1[complete.cases(finrec.1$TMAXin), ])
       finrec[i, 12] = ifelse(unique(is.na(as.integer(min(finrec.1$TMAXin, na.rm =T) == T))), NA,
                              as.integer(min(finrec.1$TMAXin, na.rm = T))) 
       finrec[i, 13] = ifelse(unique(is.na(as.integer(max(finrec.1$TMAXin, na.rm =T) == T))), NA,
                              as.integer(max(finrec.1$TMAXin, na.rm = T)))
       finrec[i, 14] = nrow(finrec.1[complete.cases(finrec.1$TMINin), ])
       finrec[i, 15] = ifelse(unique(is.na(as.integer(min(finrec.1$TMINin, na.rm =T) == T))), NA,
                              as.integer(min(finrec.1$TMINin, na.rm = T))) 
       finrec[i, 16] = ifelse(unique(is.na(as.integer(max(finrec.1$TMINin, na.rm =T) == T))), NA,
                              as.integer(max(finrec.1$TMINin, na.rm = T))) 
       
       #finrec[i, 6] = ifelse(unique(is.na(as.integer(min(SNOWin, na.rm =T) == T))), NA,
        #                         as.integer(min(SNOWin, na.rm = TRUE)))
       
       
       #if passed, subset metrec to only contains data columns (do not need to retain all of the completness stats. These will be 
       #passsed to finrec)
       metsub = metrec[ , c("year", "month", "ym", "doy", "DATE", "DATETIME", "station",
                            "PRCP", "SNWD", "SNOW", "TMAX", "TMIN",
                            "PRCPfin", "SNWDfin", "SNOWfin", "TMAXfin", "TMINfin")]
       
       #if passed, write QA'd files to Daily_Complete
       out_name = paste("Daily_Complete/", 
                   sub(":", "_", station), 
                   "_", 
                   "Daily_Pass", 
                   ".csv", 
                   sep="")
  
        write.csv(metsub, out_name)
  
  
  }else{
    
    #if failed, subset metrec to only contain data columns  
    metsubf = metrec[ , c("year", "month", "ym", "doy", "DATE", "DATETIME", "station",
                         "PRCP", "SNWD", "SNOW", "TMAX", "TMIN",
                         "PRCPfin", "SNWDfin", "SNOWfin", "TMAXfin", "TMINfin")]
    
    #then write to Daily_Missing   
    out_namef = paste("Daily_Missing/", 
                     sub(":", "_", station), 
                     "_", 
                     "Daily_Fail", 
                     ".csv", 
                     sep="")
      write.csv(metsubf, out_namef)
    
      }   
       
}

#check the number of files in the complete and missing folders. Should add up to 679

files.COMP = list.files(path = "Daily_Complete/",full.names = T)
files.MISS = list.files(path = "Daily_Missing/",full.names = T)

#replace finrec station name
finrec$station = st.all$PRCP_fid

#omit rows with no records
finrec.2 = finrec[complete.cases(finrec$PRCPna), ]

#replace SNWDhi and SNOWhi columns with NA where no data were retained
finrec.2$SNWDhi = ifelse(is.na(finrec.2$SNWDlo) == T, NA, finrec.2$SNWDhi)
finrec.2$SNOWhi = ifelse(is.na(finrec.2$SNOWlo) == T, NA, finrec.2$SNOWhi)


#then determine record length

finrec.2$PRCPle = (finrec.2$PRCPna / (finrec.2$PRCPhi - finrec.2$PRCPlo + 1)) * 100
finrec.2$SNWDle = (finrec.2$SNWDna / (finrec.2$SNWDhi - finrec.2$SNWDlo + 1)) * 100
finrec.2$SNOWle = (finrec.2$SNOWna / (finrec.2$SNOWhi - finrec.2$SNOWlo + 1)) * 100
finrec.2$TMAXle = (finrec.2$TMAXna / (finrec.2$TMAXhi - finrec.2$TMAXlo + 1)) * 100
finrec.2$TMINle = (finrec.2$TMINna / (finrec.2$TMINhi - finrec.2$TMINlo + 1)) * 100
