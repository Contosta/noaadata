# Download and format GHCN SWE data for northeastern US
# Project: MSB Vernal Windows
# Danielle S Grogan
# last updated: 2019-04-01

library(raster)
library(rgdal)
library(rgeos)
library(rnoaa)
library(lubridate)

######################################################################################################
#   Functions and required online data access 
######################################################################################################

# Function to download NCDC data (including GHCN data) from NOAA website
source("/net/home/eos/dgrogan/git_repos/data_downloadr/ncdc_ts.R")

# token to access NOAA data online. 
# Obtained from NCDC's Climate Data Online access token generator (http://www.ncdc.noaa.gov/cdo-web/token)
my.token = "SOcocOoAdqtGwMBQokCePiZSPHLplQVs"

######################################################################################################
#   Idenfity GHCN stations within study domain with PRCP, TMAX, TMIN, SNOW, SNWD data
######################################################################################################

#use level II ecoregion classification to select northern and eastern temperate forest types in 
#North America. Defines spatial extent.

#read in shapefile 
study.region = readOGR(dsn = "D://Maps//Winter//NA EcoRegions", layer = "NA_Eco_Level2_Clip_SSC")

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

#omit stations with records that start after 1948, end before 2018, and have < 90% record completeness

st.meta.sub = st.meta[st.meta$startdate < as.Date("1948-01-01") & st.meta$enddate > as.Date("2018-12-21")
                      & st.meta$datacoverage >= 0.9, ]

#export table
setwd("C:\\Users\\alix\\Box Sync\\UNH\\Projects\\SESYNC\\NCDC Data Scripts\\")
  
write.table(st.meta.sub, file = paste("ghcn_station_WWW_metadata.csv"), sep=",", append=FALSE, 
            col.names=TRUE, row.names = FALSE)

######################################################################################################
#   Download and save all GHCN data within the study domain that meets metadata parameters
######################################################################################################
st.meta = read.table("ghcn_station_WWW_metadata.csv", head = T, sep = ",")


 
id = st.meta$id

datasetid  ='GHCND'
datatypeid = c("PRCP", "TMAX", "TMIN", "SNOW", "SNWD")
            #PRCP = daily total precipitation
            #TMAX = daily maximum temperature
            #TMIN = daily minimum temperature
            #SNOW = daily total snowfall
            #SNWD = daily total snow depth

yrs = seq(1948, 2018)  # years to download

files.1 = list.files(path = "C:\\Users\\alix\\Box Sync\\UNH\\Projects\\SESYNC\\NCDC Data Scripts\\Daily_Data\\",full.names = T)


for(i in 1:nrow(st.meta.sub)){

  st.id = sub("GHCND:", "", st.meta.sub$id[i])
    
  f1 = sum(grepl(paste("GHCND_", st.id, "_Daily.csv", sep=""), c(files.1)))
  
  stationid = as.character(st.meta$id[i])

  if(f1 == 0){
    out.nm    = paste("C:\\Users\\alix\\Box Sync\\UNH\\Projects\\SESYNC\\NCDC Data Scripts\\Daily_Data\\", 
                      sub(":", "_", stationid), 
                      "_", 
                      "Daily", 
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
#   Make a single file with all SWE data 
######################################################################################################

#### NB: Could not directly download all pre-formated GHCN data using rnoaa.  
# Therefore, additional data was downloaded directly from ftp.  The formated version of these are in a different directory

files.1 = list.files(path = "/net/nfs/yukon/raid5/projects/Vernal_Windows/data/GHCN/dly_WESD/",full.names = T)
files.2 = list.files(path = "/net/nfs/yukon/raid5/data/GHCN_daily_reformat/",
                     pattern = "WESD",
                     full.names = T)

dates = seq(from = (ymd("1950-01-01")), 
            to = (ymd("2019-12-31")),
            by = "day")

# out.df = data.frame(matrix(nr = nrow(dates), nc=nrow(st.meta)+1))
# out.df[,1] = dates
# colnames(out.df) = c("DATE", st.meta$id)

for(i in 1:nrow(st.meta)){
  station.id = sub("GHCND:", "", st.meta$id[i])
  
  f1 = sum(grepl(paste("GHCND_", station.id, "_WESD.csv", sep=""), c(files.1)))
  f2 = sum(grepl(paste(station.id, "_WESD.csv", sep=""), c(files.2)))
  
  if(f1 == 1){
    read.file = which(grepl(paste("GHCND_", station.id, "_WESD.csv", sep=""), c(files.1)))
    obs = read.delim(files.1[read.file], header=T, sep=",")
    
    obs.dates = ymd(sub("T00:00:00", "", as.character(obs$date)))
    obs.short = cbind(as.character(obs.dates), obs$value/10)  # original unit: 0.1 mm.  Convert to mm
    colnames(obs.short) = c("DATE", as.character(st.meta$id[i]))
    
  }else if(f2 == 1){
    read.file = which(grepl(paste(station.id, "_WESD.csv", sep=""), c(files.2)))
    obs = read.delim(files.2[read.file], header=T, sep=",")
    obs.short = obs[,1:2]
    obs.short$WESD = obs.short$WESD / 10 # original unit: 0.1 mm.  Convert to mm
    colnames(obs.short) = c("DATE", as.character(st.meta$id[i]))
    
  }else{
    text = paste("no data found for", st.meta$id[i])
    print(text)
    
    obs.short = matrix(nr = length(dates), nc=2)
    obs.short[,1] = dates
    obs.short[,2] = NA
    
    colnames(obs.short) = c("DATE", as.character(st.meta$id[i]))
  }
  
    if(i == 1){
      out.df = obs.short
    }else{
      out.df = merge(out.df, obs.short, by="DATE", all=T)
    }
  
  print(i)
}
# 
# for(i in 1:length(files)){
#   obs = read.delim(files[i], header=T, sep=",")
#   obs.dates = ymd(sub("T00:00:00", "", as.character(obs$date)))
#   obs.short = cbind(as.character(obs.dates), obs$value/10)
#   #st.name = strsplit(as.character(obs$station[1]), ":")[[1]][2]
#   colnames(obs.short) = c("date", as.character(obs$station[1]))
#   
#   if(i == 1){
#     out.df = obs.short
#   }else{
#     out.df = merge(out.df, obs.short, by="date", all=T)
#   }
#   print(i)
# }
#   
write.table(out.df, 
            "/net/nfs/yukon/raid5/projects/Vernal_Windows/data/SWE_data/SWE_data_formatted/GHCND/GHCND_SWE.csv",
            row.names=F, 
            sep=",")

######################################################################################################
#   Make shapefile of station locations
######################################################################################################

# Station locations
loc.ghcn = as.data.frame(cbind(st.meta$id, st.meta$latitude, st.meta$longitude))
colnames(loc.ghcn) = c("SITE_ID", "LATITUDE", "LONGITUDE")
coordinates(loc.ghcn) = ~ LONGITUDE + LATITUDE # Make a SpatialPointsDataFrame                                
loc.ghcn$SITE_ID = as.character(st.meta$id)

# write locataions as shapefile (note: original data IS a shapefile, so this is redundant)
writeOGR(loc.ghcn, "SWE_data/SWE_data_formatted/GHCND/GHCND_stations", "GHCND_stations", driver="ESRI Shapefile", overwrite=T)



###############################################################################################
######### test from tutorial: https://ropensci.org/tutorials/rnoaa_tutorial/
# NB: if code below does not return any data, then there is a problem with the NCDC site (this has happened before)
# IMPORTANT: each token has a download limit of 10,000 downloads per day
ncdc_stations(datasetid='GHCND', locationid='FIPS:12017', stationid='GHCND:USC00084289',
              token = my.token)
# 
# 
# #####
# # compare to NY
# ny.loc = readOGR("/net/nfs/yukon/raid5/projects/Vernal_Windows/data/SWE_data/SWE_data_formatted/NY_snow_survey/NY_snow_survey_stations", 
#                  "NY_snow_survey_stations")
# 
# 
# plot(ny.loc, pch=19, cex = 0.8, col='red')
# plot(loc.ghcn, pch=19, cex=0.2, add=T)
# END