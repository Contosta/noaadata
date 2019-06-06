# ncdc_ts()

# Download and save a time series of NCDC data using the rnoaa package (https://github.com/ropensci/rnoaa)
# Danielle S Grogan
# last updated: 2019-04-01

library(rnoaa)

ncdc_ts = function(datasetid,       # character string; example: "GHCN". Use ncdc_datasets() to get a table of all dataset options 
                   stationid,       # character string; example: "GHCND:US1MDSM0001". Use ncdc_stations() to find station ids
                   datatypeid,      # character string; example: "PRCP".  Use ncdc_datatypes() to find options
                   yrs,             # vector of integers. A sequence of years for which to download data.
                   token,           # This must be a valid token token supplied to you by NCDC's Climate Data Online access token generator (http://www.ncdc.noaa.gov/cdo-web/token). 
                   out.nm = NA,     # character sting; if provided, the data is written to this file name.
                   write.empty = F  # T or F;  if T then empty data frames will be saved to file (not recommended).
                   )
  { 
  
  test = ncdc_stations(datasetid='GHCND', locationid='FIPS:12017', stationid='GHCND:USC00084289',
                       token = my.token)
  
  if(is.null(test$data)){
    warning = "ERROR: rnoaa download failure"
    out = warning
  }else{
    st.data.all  = lapply(yrs, 
                          FUN = function(x) ncdc(datasetid  = datasetid, 
                                                 stationid  = stationid,
                                                 datatypeid = datatypeid, 
                                                 startdate  = paste(x, '-01-01', sep=""), 
                                                 enddate    = paste(x, '-12-31', sep=""), 
                                                 limit      = 365,
                                                 token      = token)
    )
    
    st.data.list = lapply(st.data.all, FUN = function(x) x$data)
    st.data.df = do.call(rbind, st.data.list)
    
    if(is.na(out.nm)){
      out = st.data.df
    }else{
      if(length(st.data.df) > 0){
        write.table(st.data.df, out.nm, row.names=F, sep=",")
        out = paste("data written to", out.nm)
      }else{
        if(write.empty == T){
          write.table(st.data.df, out.nm, row.names=F, sep=",")
          out = paste("Empty data frame written to", out.nm)
        }else{
          out = "Empty data frame. No file written."
        }
      }
      
    }
  }
  
  return(out)
}
# END OF FUNCTION
