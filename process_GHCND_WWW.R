# Download and format GHCN daily precipitation (PRCP), maximum temeperature (TMAX), minimum temperature (TMIN),
# snowfall (SNOW), and snowdepth (SNWD)
# Project: Winter Weather Whiplash
# Original GHCN download code by Danielle Grogan for obtaining snow water equivalent data as part of 
# NSF MSB vernal windows analysis
# Revised code by Alix Contosta for obtaining and formatting daily weather data to support 
# identification of winter weather whiplash events
# last updated: 2019-06-09

#load libraries

library(data.table)
library(lubridate)
library(raster)
library(rgdal)
library(rgeos)
library(rnoaa)
library(stringr)

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
#my.token = "CClTsxbcVkxPlNoNbAhcWHhzZLOQQcjN"

#my.token = "IulLcWjJTyfnoRjtCUliaBXUpCufiUxK"
#my.token = "WaIJWipsgerfHvIVGWdKjrAZVTWKXHkS"
#my.token = "JWkGYNwyrgLUwLZeaGglUJXHmSXRwiHa"

my.token = "txGmRofpgFxOUzOonEiGXUMCHVLmrdYI"
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

#omit stations with records that start after 1918, end before 2018, and have < 90% record completeness

st.meta.sub = st.meta[st.meta$startdate < as.Date("1928-01-01") & st.meta$enddate > as.Date("2018-12-31")
                      & st.meta$datacoverage >= 0.9, ]

#export table

write.table(st.meta.sub, file = paste("ghcn_station_WWW_metadata.csv"), sep=",", append=FALSE, 
            col.names=TRUE, row.names = FALSE)

######################################################################################################
#   Download and save all GHCN data within the study domain that meets metadata parameters
######################################################################################################
st.meta.sub = read.table("ghcn_station_WWW_metadata.csv", head = T, sep = ",")

files.1 = list.files(path = "TMAX_Daily/",full.names = T)

datasetid  ='GHCND'
datatypeid = c("TMAX")#, "PRCP", , "TMIN", "SNOW", "SNWD")
            #PRCP = daily total precipitation
            #TMAX = daily maximum temperature
            #TMIN = daily minimum temperature
            #SNOW = daily total snowfall
            #SNWD = daily total snow depth

yrs = seq(1928, 2018)  # years to download

for(i in 272:nrow(st.meta.sub)){

  st.id = sub("GHCND:", "", st.meta.sub$id[i])
    
  f1 = sum(grepl(paste("GHCND_", st.id, "Daily.csv", sep=""), c(files.1)))
  
  stationid = as.character(st.meta.sub$id[i])

  if(f1 == 0){
    out.nm    = paste("TMAX_Daily/", 
                      sub(":", "_", stationid), 
                      "_", 
                      "TMAX_Daily", 
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
#Transpose data to be in wide instead of long format
######################################################################################################

files.1 = list.files(path = "Daily_Data/",full.names = T)

# start the loop to go through the whole directory
for (i in seq(length(files.1))) {

    #read in file
    ex = read.table(files.1[i], header = TRUE, sep = ',',)

    #extract station ID
    station = unique(ex$station)


    #subset file to contain different dataframes for PRCP, SNOW, SNWD, TMAX, and TMIN.
  
    PRCP.1 = ex[ex$datatype == "PRCP", ]
    PRCP.2 = PRCP.1[ , c("date", "station", "value", "fl_m", "fl_q", "fl_so", "fl_t")]
    names(PRCP.2) = c("date", "station", "PRCP", "PRCP_fl_m", "PRCP_fl_q", "PRCP_fl_so", "PRCP_fl_t" )

    SNWD.1 = ex[ex$datatype == "SNWD", ]
    SNWD.2 = SNWD.1[ , c("date", "value", "fl_m", "fl_q", "fl_so", "fl_t")]
    names(SNWD.2) = c("date", "SNWD", "SNWD_fl_m", "SNWD_fl_q", "SNWD_fl_so", "SNWD_fl_t" )
    
    SNOW.1 = ex[ex$datatype == "SNOW", ]
    SNOW.2 = SNOW.1[ , c("date", "value", "fl_m", "fl_q", "fl_so", "fl_t")]
    names(SNOW.2) = c("date",  "SNOW", "SNOW_fl_m", "SNOW_fl_q", "SNOW_fl_so", "SNOW_fl_t" )

    TMAX.1 = ex[ex$datatype == "TMAX", ]
    TMAX.2 = TMAX.1[ , c("date", "value", "fl_m", "fl_q", "fl_so", "fl_t")]
    names(TMAX.2) = c("date", "TMAX", "TMAX_fl_m", "TMAX_fl_q", "TMAX_fl_so", "TMAX_fl_t" )

    TMIN.1 = ex[ex$datatype == "TMIN", ]
    TMIN.2 = TMIN.1[ , c("date", "value", "fl_m", "fl_q", "fl_so", "fl_t")]
    names(TMIN.2) = c("date", "TMIN", "TMIN_fl_m", "TMIN_fl_q", "TMIN_fl_so", "TMIN_fl_t" )

    #join separate tables together with date as identifier

    met.1 = merge(PRCP.2, SNOW.2, by.x = "date", by.y = "date", all.x =T, all.y = T)
    met.2 = merge(met.1, SNWD.2, by.x = "date", by.y = "date", all.x =T, all.y = T)
    met.3 = merge(met.2, TMAX.2, by.x = "date", by.y = "date", all.x =T, all.y = T)
    met.4 = merge(met.3, TMIN.2, by.x = "date", by.y = "date", all.x =T, all.y = T)

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
files.2 = list.files(path = "Daily_Reformat/",full.names = T)

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
for (i in seq(length(files.2))) {
  
  #read in file
  allmet.1 = read.csv(files.2[i], header = TRUE, sep = ',',)
  
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
   metyr$PRCPs = ifelse(is.na(metyr$PRCPcorr) == T, NA, metyr$year)
   metyr$SNWDs = ifelse(is.na(metyr$SNWDcorr) == T, NA, metyr$year)
   metyr$SNOWs = ifelse(is.na(metyr$SNOWcorr) == T, NA, metyr$year)
   metyr$TMAXs = ifelse(is.na(metyr$TMAXcorr) == T, NA, metyr$year)
   metyr$TMINs = ifelse(is.na(metyr$TMINcorr) == T, NA, metyr$year)
  
   #determine length of record for each variable / station combination
   reclen = data.table(metyr)
  
   #first calculate start and end of record
   reclen.1 = as.data.frame(reclen[ , list(PRCPlo = min(PRCPs, na.rm = TRUE), PRCPhi = max(PRCPs, na.rm = TRUE),
                                          SNWDlo = min(SNWDs, na.rm = TRUE), SNWDhi = max(SNWDs, na.rm = TRUE),
                                          SNOWlo = min(SNOWs, na.rm = TRUE), SNOWhi = max(SNOWs, na.rm = TRUE),
                                          TMAXlo = min(TMAXs, na.rm = TRUE), TMAXhi = max(TMAXs, na.rm = TRUE),
                                          TMINlo = min(TMINs, na.rm = TRUE), TMINhi = max(TMINs, na.rm = TRUE))])
  
   #merge reclen.1 with recomp.2
   recomp.2 = merge(recomp.1, reclen.1, all.x = TRUE, all.y = TRUE)
  
   #create year flag to only calculate rolling sums within period of record
   recomp.2$PRCPFlag = ifelse(recomp.2$year >= recomp.2$PRCPlo & recomp.2$year <= recomp.2$PRCPhi, 1, 0)
   recomp.2$SNWDFlag = ifelse(recomp.2$year >= recomp.2$SNWDlo & recomp.2$year <= recomp.2$SNWDhi, 1, 0)
   recomp.2$SNOWFlag = ifelse(recomp.2$year >= recomp.2$SNOWlo & recomp.2$year <= recomp.2$SNOWhi, 1, 0)
   recomp.2$TMAXFlag = ifelse(recomp.2$year >= recomp.2$TMAXlo & recomp.2$year <= recomp.2$TMAXhi, 1, 0)
   recomp.2$TMINFlag = ifelse(recomp.2$year >= recomp.2$TMINlo & recomp.2$year <= recomp.2$TMINhi, 1, 0)
  
   #calculate rolling sums of incomplete data years over 10 year increments
   recomp.2$PRCPteny = ifelse(recomp.2$PRCPFlag == 1, ave(recomp.2$PRCPyna, 
                                                          FUN= function(x) rollsum(x, k=10, na.pad=T) ), NA)
   recomp.2$SNWDteny = ifelse(recomp.2$SNWDFlag == 1, ave(recomp.2$SNWDyna, 
                                                          FUN= function(x) rollsum(x, k=10, na.pad=T) ), NA)
   recomp.2$SNOWteny = ifelse(recomp.2$SNOWFlag == 1, ave(recomp.2$SNOWyna, 
                                                          FUN= function(x) rollsum(x, k=10, na.pad=T) ), NA)
   recomp.2$TMAXteny = ifelse(recomp.2$TMAXFlag == 1, ave(recomp.2$TMAXyna, 
                                                          FUN= function(x) rollsum(x, k=10, na.pad=T) ), NA)
   recomp.2$TMINteny = ifelse(recomp.2$TMINFlag == 1, ave(recomp.2$TMINyna, 
                                                          FUN= function(x) rollsum(x, k=10, na.pad=T) ), NA)
  
   #calculate the maximum number of incomplete 10-year increments
   recomp.3 = data.table(recomp.2)
  
   recomp.4 = recomp.3[ , list(PRCPgap = max(PRCPteny, na.rm = TRUE),
                              SNWDgap = max(SNWDteny, na.rm = TRUE),
                              SNOWgap = max(SNOWteny, na.rm = TRUE),
                              TMAXgap = max(TMAXteny, na.rm = TRUE),
                              TMINgap = max(TMINteny, na.rm = TRUE))]
  
   #flag sites where maxgap > 5 (this would be > 50% data incompleteness over a 10 year period)
   recomp.4$PRCPFlag = ifelse(recomp.4$PRCPgap > 5, 1, 0)
   recomp.4$SNWDFlag = ifelse(recomp.4$SNWDgap > 5, 1, 0)
   recomp.4$SNOWFlag = ifelse(recomp.4$SNOWgap > 5, 1, 0)
   recomp.4$TMAXFlag = ifelse(recomp.4$TMAXgap > 5, 1, 0)
   recomp.4$TMINFlag = ifelse(recomp.4$TMINgap > 5, 1, 0)
  
   #merge with metyr
   metrec = merge(metyr, recomp.4, all.x = TRUE, all.y = TRUE)
  
   #omit values from stations that did not reach threshold for record completeness
   metrec$PRCPfin = ifelse(metrec$PRCPFlag == 1, NA, metrec$PRCPcorr)
   metrec$SNWDfin = ifelse(metrec$SNWDFlag == 1 |NA, metrec$SNWDcorr)
   metrec$SNOWfin = ifelse(metrec$SNOWFlag == 1, NA, metrec$SNOWcorr)
   metrec$TMAXfin = ifelse(metrec$TMAXFlag == 1, NA, metrec$TMAXcorr)
   metrec$TMINfin = ifelse(metrec$TMINFlag == 1, NA, metrec$TMINcorr)
  
   #obtain stats on record completeness
   #flag years when fin values are NA
   metrec$PRCPin = ifelse(is.na(metrec$PRCPfin) == T, NA, metrec$year)
   metrec$SNWDin = ifelse(is.na(metrec$SNWDfin) == T, NA, metrec$year)
   metrec$SNOWin = ifelse(is.na(metrec$SNOWfin) == T, NA, metrec$year)
   metrec$TMAXin = ifelse(is.na(metrec$TMAXfin) == T, NA, metrec$year)
   metrec$TMINin = ifelse(is.na(metrec$TMINfin) == T, NA, metrec$year)
   
   
   #reverse flag years when fin values are 1
   metrec$PRCPout = ifelse(is.na(metrec$PRCPfin) == T, metrec$year, NA)
   metrec$SNWDout = ifelse(is.na(metrec$SNWDfin) == T, metrec$year, NA)
   metrec$SNOWout = ifelse(is.na(metrec$SNOWfin) == T, metrec$year, NA)
   metrec$TMAXout = ifelse(is.na(metrec$TMAXfin) == T, metrec$year, NA)
   metrec$TMINout = ifelse(is.na(metrec$TMINfin) == T, metrec$year, NA)
   
   #flag station if it does not pass record completeness based on PRCP, TMAX, and TMIN
   stationNA = unique(ifelse(metrec$PRCPFlag == 1 | metrec$TMAXFlag == 1 | metrec$TMINFlag == 1, NA, 1))
   
   #if passsed, write station name, record length, and start and end to finrec
   #subset columns to only include data
   #write csv to Daily_Complete folder
   
   if(stationNA == 1) {
     
       #if passsed, write station, record length, start, and end to finrec
       finrec$station[i] = station[1]
       finrec$PRCPna[i] = length(unique(metrec$PRCPout))
       finrec$PRCPlo[i] = ifelse(unique(is.infinite(as.integer(min(metrec$PRCPin, na.rm =T) == T))), NA,
                                 as.integer(min(metrec$PRCPin, na.rm = TRUE)))
       finrec$PRCPhi[i] = ifelse(unique(is.infinite(as.integer(min(metrec$PRCPout, na.rm =T) == T))), NA,
                                 as.integer(min(metrec$PRCPout, na.rm = TRUE)))
       finrec$SNOWna[i] = length(unique(metrec$SNOWout))
       finrec$SNOWlo[i] = ifelse(unique(is.infinite(as.integer(min(metrec$SNOWin, na.rm =T) == T))), NA,
                                 as.integer(min(metrec$SNOWin, na.rm = TRUE)))
       finrec$SNOWhi[i] = ifelse(unique(is.infinite(as.integer(min(metrec$SNOWout, na.rm =T) == T))), NA,
                                 as.integer(min(metrec$SNOWout, na.rm = TRUE)))
       finrec$SNOWna[i] = length(unique(metrec$SNOWout))
       finrec$SNWDlo[i] = ifelse(unique(is.infinite(as.integer(min(metrec$SNWDin, na.rm =T) == T))), NA,
                                 as.integer(min(metrec$SNWDin, na.rm = TRUE)))
       finrec$SNWDhi[i] = ifelse(unique(is.infinite(as.integer(min(metrec$SNWDout, na.rm =T) == T))), NA,
                                 as.integer(min(metrec$SNWDout, na.rm = TRUE)))
       finrec$TMAXna[i] = length(unique(metrec$TMAXout))
       finrec$TMAXlo[i] = ifelse(unique(is.infinite(as.integer(min(metrec$TMAXin, na.rm =T) == T))), NA,
                                 as.integer(min(metrec$TMAXin, na.rm = TRUE)))
       finrec$TMAXhi[i] = ifelse(unique(is.infinite(as.integer(min(metrec$TMAXout, na.rm =T) == T))), NA,
                                 as.integer(min(metrec$TMAXout, na.rm = TRUE)))
       finrec$TMINna[i] = length(unique(metrec$TMINout))
       finrec$TMINlo[i] = ifelse(unique(is.infinite(as.integer(min(metrec$TMINin, na.rm =T) == T))), NA,
                                 as.integer(min(metrec$TMINin, na.rm = TRUE)))
       finrec$TMINhi[i] = ifelse(unique(is.infinite(as.integer(min(metrec$TMINout, na.rm =T) == T))), NA,
                                 as.integer(min(metrec$TMINout, na.rm = TRUE)))
       
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


#then determine record length

finrec.1$PRCPle = (1 - finrec.1$PRCPna / (finrec.1$PRCPhi - finrec.1$PRCPlo)) * 100
finrec.1$SNWDle = (1 - finrec.1$SNWDna / (finrec.1$SNWDhi - finrec.1$SNWDlo)) * 100
finrec.1$SNOWle = (1 - finrec.1$SNOWna / (finrec.1$SNOWhi - finrec.1$SNOWlo)) * 100
finrec.1$TMAXle = (1 - finrec.1$TMAXna / (finrec.1$TMAXhi - finrec.1$TMAXlo)) * 100
finrec.1$TMINle = (1 - finrec.1$TMINna / (finrec.1$TMINhi - finrec.1$TMINlo)) * 100
