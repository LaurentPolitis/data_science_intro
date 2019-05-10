ephemeris <- function(lat, lon, date, span=1, tz="UTC") {
    
    # input:    lattitude, longitude
    # output:    dataframe with sunset, sunrise, daulength, solarnoon      
    
    lon.lat <- matrix(c(lon, lat), nrow=1)
    
    # using noon gets us around daylight saving time issues
    day <- as.POSIXct(sprintf("%s 12:00:00", date), tz=tz)
    sequence <- seq(from=day, length.out=span , by="days")
    
    sunrise <- sunriset(lon.lat, sequence, direction="sunrise", POSIXct.out=TRUE)
    sunset <- sunriset(lon.lat, sequence, direction="sunset", POSIXct.out=TRUE)
    solar_noon <- solarnoon(lon.lat, sequence, POSIXct.out=TRUE)
    
    data.frame(date=as.Date(sunrise$time),
               sunrise=as.numeric(format(sunrise$time, "%H%M")),
               solarnoon=as.numeric(format(solar_noon$time, "%H%M")),
               sunset=as.numeric(format(sunset$time, "%H%M")),
               day_length=as.numeric(sunset$time-sunrise$time)) 
    
}