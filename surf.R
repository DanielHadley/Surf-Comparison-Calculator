
#### Meta data ####
# http://www.ndbc.noaa.gov/historical_data.shtml
# http://www.ndbc.noaa.gov/measdes.shtml#stdmet

# WDIR  Wind direction (the direction the wind is coming from in degrees clockwise from true N) during the same period used for WSPD. See Wind Averaging Methods
# WSPD	Wind speed (m/s) averaged over an eight-minute period for buoys and a two-minute period for land stations. Reported Hourly. See Wind Averaging Methods.
# GST	Peak 5 or 8 second gust speed (m/s) measured during the eight-minute or two-minute period. The 5 or 8 second period can be determined by payload, See the Sensor Reporting, Sampling, and Accuracy section.
# WVHT	Significant wave height (meters) is calculated as the average of the highest one-third of all of the wave heights during the 20-minute sampling period. See the Wave Measurements section.
# DPD	Dominant wave period (seconds) is the period with the maximum wave energy. See the Wave Measurements section.
# APD	Average wave period (seconds) of all waves during the 20-minute period. See the Wave Measurements section.
# MWD	The direction from which the waves at the dominant period (DPD) are coming. The units are degrees from true North, increasing clockwise, with North as 0 (zero) degrees and East as 90 degrees. See the Wave Measurements section.
# PRES	Sea level pressure (hPa). For C-MAN sites and Great Lakes buoys, the recorded pressure is reduced to sea level using the method described in NWS Technical Procedures Bulletin 291 (11/14/80). ( labeled BAR in Historical files)
# ATMP	Air temperature (Celsius). For sensor heights on buoys, see Hull Descriptions. For sensor heights at C-MAN stations, see C-MAN Sensor Locations
# WTMP	Sea surface temperature (Celsius). For sensor depth, see Hull Description.
# DEWP	Dewpoint temperature taken at the same height as the air temperature measurement.
# VIS	Station visibility (nautica miles). Note that buoy stations are limited to reports from 0 to 1.6 nmi.
# PTDY	Pressure Tendency is the direction (plus or minus) and the amount of pressure change (hPa)for a three hour period ending at the time of observation. (not in Historical files)
# TIDE	The water level in feet above or below Mean Lower Low Water (MLLW).


#### Download data ####

# place = c("Massachusetts", "Scripps")
# buoy = c(44029, 46223)
# df = data.frame(place,buoy)


# raw1 <- read.table("http://www.ndbc.noaa.gov/view_text_file.php?filename=44029h2013.txt.gz&dir=data/historical/stdmet/")
# raw2 <- read.table("http://www.ndbc.noaa.gov/view_text_file.php?filename=44029h2012.txt.gz&dir=data/historical/stdmet/")
# 
# d <- rbind(raw1, raw2)

# Use data from this because it has MWD: http://magicseaweed.com/Boston-16-Nm-East-Of-Boston-Ma-Wave-Buoy/69807/
# http://www.ndbc.noaa.gov/station_page.php?station=44013

d <- read.table("http://www.ndbc.noaa.gov/view_text_file.php?filename=44013h2013.txt.gz&dir=data/historical/stdmet/")

colnames(d) <- c("#YY", "MM", "DD", "hh", "mm", "WDIR", "WSPD", "GST", "WVHT", 
          "DPD", "APD", "MWD", "PRES", "ATMP", "WTMP", "DEWP", "VIS", "TIDE")

d[d==99 | d==999]<-NA

d$WVHT.Ft <- d$WVHT * 3.28084 # convert to feet
d$MMDD <- paste(d$MM, d$DD)


#### Melt/Cast ####

sapply(d[1,],class) #look at these again to see which columns to include
names(d) #look at the names
data.m <- melt(d, id=c(1:3, 20), measure=c(6:15, 19))
data.c <- dcast(data.m, MMDD ~ variable, mean, na.rm=TRUE)

hist(data.c$WVHT.Ft)

big <- data.c[which(data.c$WVHT.Ft > 10),]

BostonWavePredictor <- function(){
  ifelse(data.c$MWD > 66 & data.c$MWD < 113, data.c$WVHT.Ft * 1,
         ifelse(data.c$MWD < 136 & data.c$WMD > 44, data.c$WVHT.Ft * .8, data.c$WVHT.Ft * .6)
         )
}

data.c$Wave.Pred = BostonWavePredictor()

IsBostonSurfable <- function(){
  ifelse(data.c$Wave.Pre > 1 & data.c$DPD > 10, 1,
         ifelse(data.c$Wave.Pre > 2 & data.c$DPD > 8, 1, 0))
}

data.c$Surfable <- IsBostonSurfable()