######## Pulse metrics from quantmod for LTER data ########
######## LTER pulse dynamics working group, July 2022, Jennifer Rudgers ########

## This tutorial uses SEV climate data from one SEV ecosystem/met station as an example ##

######## Data steps  ########
rm(list=ls(all=TRUE)) #give R a blank slate

#set the working directory (wd) on your computer - optional time saver
#will save any files you create into that folder
getwd()
setwd("/Users/jkominos/Kominoski Lab Dropbox/John Kominoski/LTER/LTER_Pulses/quantmod_tutorial")

# http://www.quantmod.com/examples/intro/

#Read data from your working directory
#or use read.csv(file.choose()) to import from browser
fce<-read.csv("fce.srs2.level.csv")
summary(fce)
# SEV LTER example metadata
# station -- is the location of the met station, numbers given
# date -- we will fix later
# mean_temp is mean daily temp in degrees C (NOAA format - average of max and min)
# max_temp and min_temp are in degrees C for daily temp data based originally on hourly data
# hourly data are here: Moore, D.I. and K.M. Hall. 2022. Meteorology Data from the Sevilleta National Wildlife Refuge, New Mexico ver 15. Environmental Data Initiative. https://doi.org/10.6073/pasta/d56307b398e28137dabaa6994f0f5f92 (Accessed 2022-07-14).
# precip is total daily precip measured in millimeters (mm)

#call libraries (make sure you have installed packages first)
library(tidyverse)
library(quantmod)
library(xts)
library(emmeans)
library(ggthemes)

#with SEV LTER example - subset dataset to one location and one climate variable 
#QUANTMOD requires a single time series ordered by the order of observations (time)
#!!! WARNING: xts will not accept any NAs, NaN, or Inf !!!
# for temperature datasets, you may want to fill missing values 
# in xts using last observation
# xts_last <- na.locf(xtsdata) 
# or interpolate NAs na.approx(xtsdata)  

#in this example, we filter to location called 40 and remove NAs
sev40<-data.frame(sevmet %>% filter (station==40,!is.na(date),!is.na(precip)) %>% 
                    select (date, precip) )
summary(sev40) #1988 to 2020
# note: date is being read as character format, we need to format date

#make date data into ISO format
fce$Date<-as.POSIXct(fce$Date,"%Y-%m-%d",tz = "UTC") 
summary(fce) 


# STEP 1: Convert to xts (time series) object -----------
#!!! WARNING assumes records of observations are ordered by time !!!
# this function sorts by date so the records are in the correct order 
# prior to analysis
fcexts <- xts(fce[,-1], order.by=fce[,1])

# a few functions to check attributes
str(fcexts)
tzone(fcexts)
nmonths(fcexts)
nyears(fcexts)

######## Apply quantmod to raw data ########
#### make a quantmod graphic - time series of pulses
chartSeries(fcexts,theme="white")
# if you get this error: Error in plot.new() : figure margins too large
# make your plot window larger

#look at one year only, we picked 2018
chartSeries(fcexts, subset='2018::2019-01',theme="white")

#look at monthly deviations for last 10 years
chartSeries(to.monthly(fcexts),up.col='blue',dn.col='red',
            subset='2009::2019-01',theme="white")
#blue shows large rains, red shows large drought

#### find peaks and valleys
# type findPeaks in console to see the function
# A peak[valley] is defined as the highest[lowest] value in a timeseries, 
# The identification of a peak/valley depends on the values 
# in the immediately preceding time series.
# So, the function can only define it after a change in direction has occurred. 
# This means that the function will always return the first period of time
# AFTER the peak/valley of the timeseries, 
# so as not to introduce a look-ahead bias.
# So in every case you need to subtract 1 to plot the actual timepoint 
# of the peak or valley


#thresh is the threshhold
#you should explore different threshold levels for your dataset
#it is important to go into this with a biologically informed threshold value
#the threshold is thresh and represents the slope as described below:
#"The test x[pks - 1] - x[pks] > thresh compares each peak value to the value immediately succeeding it in the series (not to the next trough in the series).
#It uses a (crude) estimate of the size of the slope of the function immediately after the peak and selects only those peaks where that slope exceeds thresh in size.
#e.g., for FCE, 10 cm of water level change is considered a large event that can impact plant growth, try thresh = 10 cm per day as slope
#anything less than that threshold could be argued to be a less biologically meaningful pulse

# STEP 2: set a threshold -------
threshold.peak<-2 #Use for SRS & TS/Ph marsh
threshold.peak<-10 #Use for SRS or TS/Ph tidal estuary
threshold.peak<-5 #Use for TS/Ph estuary

# STEP 3: Function to identify peaks[valleys] --------
fcepeaks<-findPeaks(fcexts, thresh=threshold.peak)
plot(fcexts[fcepeaks-1])

#contrast this result against a 5 cm rain deviation threshold
threshold.5<-5 
fcepeaks.5<-findpeaks(fcexts, thresh=threshold.5)
plot(fcexts[fcepeaks.5-1])


# GRAPH PEAKS
#turn peaks into a dataframe to add it to a ggplot of the raw data
#and calculate metrics
#here we use 5 threshold
peaks<-as.data.frame(fcexts[fcepeaks-1])

#plot peaks onto raw data for precipitation
peaks_graphic<-ggplot(fce, aes(x = Date, y = SRS2)) +
  geom_line(colour='blue') +
  labs(x = "Date",
       y = "Water Level (cm)")+
  geom_point(data=peaks,aes(x = as.POSIXct(row.names(peaks)), y = V1), 
             colour='red', size=4)
peaks_graphic
#save graphic - revise file name for your dataset
ggsave("fce_tsph7_level_peaks.jpg",peaks_graphic,dpi=300)
#*** why are some of the seemingly 5 cm daily events not indicated 
#*** as a peak (red dot)?
#*** peaks are defined relative to the preceding period of water level
#*** if preceding period was wet, a 5 cm or greater absolute size 
#*** of the event will not be designated as a peak because thresh is a slope

# STEP 4: Create a data frame to add to the group project -----------
#name your LTER
lter<-"FCE"

#name your site
site<-"TS/Ph7"

#name your driver
driver<-"water level"

#units = units of driver (e.g., mm)
units<-"cm"

#are you analyzing peaks or valleys? [options: peak or valley]
pv="peak"

#save number of months and years
nmonths<-nmonths(fcexts)
nyears<-nyears(fcexts)

# STEP 5: Calculate metrics of peaks ----------
#how many peaks per total obs, which are in units of days (here, obs = 365 days*33 years) ?
peak_per_d<-length(peaks$V1)/length(fce$TS.Ph7)
peak_per_d

#how many peaks per year?
peak_per_y<-length(peaks$V1)/nyears(fcexts)
peak_per_y

#average peak magnitude (in cm for water level)
peak_mean<-mean(peaks$V1)
peak_mean

#peak standard deviation
peak_sd<-sd(peaks$V1)

#peak CV
peak_CV<-sd(peaks$V1)/mean(peaks$V1)
peak_CV


# STEP 6: Standardized regression models for temporal change--------
#peak number vs. time: Has the number of peaks increased/decreased over time?
# get slope of (number of peaks per year for each year) vs. year (and p-value) to look for temporal change in number of peaks

# add year and time columns to peaks dataset
peaks$time<-as.POSIXct(row.names(peaks))
peaks$year<-as.numeric(format(as.POSIXct(row.names(peaks)),"%Y"))
summary(peaks)

# first, add any missing years that had no peaks (add zeros) - probably a more efficient way to do this...
year<-min(as.numeric(format(as.POSIXct(fce$Date),"%Y"))):max(as.numeric(format(as.POSIXct(fce$Date),"%Y")))
years<-as.data.frame(year)
years
peak.sum<-peaks %>% group_by(year) %>% summarise(mean.peak=mean(V1), count=n())
peak.sum
peak.number<-merge(years,peak.sum,by.x="year",by.y="year",all.x=TRUE)
peak.number[is.na(peak.number)] <- 0 
peak.number

# second, build the stats models and save the slope and p as output
peak.number.lm<-lm(scale(count)~scale(year),data=peak.number)

# centering is done by subtracting the column means (omitting NAs) of V1 or year from their corresponding column
# scaling is done by dividing the (centered) column of V1 or year by their standard deviations 
# this returns a standardized regression coefficient that makes it possible to compare across drivers that differ in measured units and/or timescales

#plot(peak.number.lm) # turn this on to check model statistical assumptions
lmsum.number<-summary(peak.number.lm)
peak.number.slope<-peak.number.lm$coefficients[2]
peak.number.slope
peak.number.p<-lmsum.number$coefficients[2,4]
peak.number.p

peak.number.graph<-ggplot(data=peak.number, aes(x = year, y = count)) + 
  geom_point(color='darkslategrey',size=4) +
  geom_smooth(method = "lm",  se = FALSE, color="darkslategrey")+
  ylab("Number of peaks")+
  xlab("Year")+
  theme_tufte(base_size = 14, base_family="sans")
peak.number.graph
#save graphic - revise file name for your dataset
ggsave("TSPh7_level_peak.number.jpg",peak.number.graph,dpi=300,width=10, height=4)

# peak magnitude (all peaks) vs. time: Has the magnitude of peaks increased/decreased over time?
# get slope of magnitude of peaks vs. time (and p-value)
peak.magnitude.lm<-lm(scale(V1)~scale(time),data=peaks)

# plot(peak.magnitude.lm) # turn this on to check model statistical assumptions
lmsum.mag<-summary(peak.magnitude.lm)
peak.magnitude.slope<-peak.magnitude.lm$coefficients[2]
peak.magnitude.slope
peak.magnitude.p<-lmsum.mag$coefficients[2,4]
peak.magnitude.p

peak.mag.graph<-ggplot(data=peaks, aes(x = time, y = V1)) + 
  geom_point(color='darkslategrey',size=4) +
  geom_smooth(method = "lm",  se = FALSE, color="darkslategrey")+
  ylab("Average peak magnitude (mm)")+
  xlab("Year")+
  theme_tufte(base_size = 14, base_family="sans")
peak.mag.graph
#save graphic - revise file name for your dataset
ggsave("TSPh7_level_peak.magnitude.jpg",peak.mag.graph,dpi=300,width=10, height=4)

# STEP 7:  Save metrics into your site location data frame ------------
pulse_metrics_fce<-data.frame(lter,site,driver,units,pv,nyears,nmonths,peak_mean,peak_sd,peak_CV,peak_per_y,peak_per_d,
                                peak.number.slope,peak.number.p,peak.magnitude.slope,peak.magnitude.p,threshold.peak)
write.csv(pulse_metrics_fce,"pulse_metrics_fce_quantmod.csv")
#then you will paste your data into our cross-site dataframe




# METADATA for pulse quantmod metrics (long format) ----------
#lter = 3 letter lter site acronym ALL CAPS
#site = local site code within lter site (e.g., sev40)
#driver = abiotic variable for pulse regime analysis
#units = units of driver (e.g., mm)
#[options: precipitation, temperature, waterdepth, flow, ... ]
#pv = are you analyzing peaks or valleys? [options: peak or valley]
#nyears = number of years in the data set (units = years)
#nmonths = number of months in the data set (units = months)
#peak_mean = average magnitude of peak (units = units, defined above)
#peak_sd = standard deviation of the average magnitude of peak (units = units, defined above)
#peak_CV = coefficient of variation of the average magnitude of peak (peak_sd/peak_mean) (unitless)
#peaks_per_y = number of peaks per year averaged over time series (count data)
#peak_per_d = number of peaks per day averaged over time series (count data, fraction)
#peak.number.slope = slope of the number of peaks per year regressed on year (has the number of pulses increased/decreased over time?)
#peak.number.p = p-value for whether slope of the number of peaks per year regressed on year
#                is significantly different from zero, t-test from lm (has the number of pulses increased/decreased over time?)
#peak.magnitude.slope = the slope of the peak magnitude regressed on calendar date (has the magnitude of pulses increased/decreased over time?)
#peak.magnitude.p = p-value for whether the slope of the peak magnitude regressed on calendar date
#                   is significantly different from zero, t-test from lm (has the magnitude of pulses increased/decreased over time?)



#### Additional Considerations / Tools / Not Used for Cross-Site Data Collection
# are here cases where we should apply quantmod only after smoothing data?
# what additional functions in xts may be useful?
# https://github.com/stas-g/findPeaks

########### Series Analysis - not using this in metrics database
#series
sev40decr<-seriesDecr(sev40xts,thresh=20,diff.=1L)
seriesIncr(sev40xts,thresh=0,diff.=1L)
# you can ignore: timezone of object (UTC) is different than current timezone (). 

#can convert to monthly
sev40month<-to.monthly(sev40xts)

#calculate the yearly mean
sev40year<-to.yearly(sev40xts)
lapply(sev40year,FUN=mean) 

#calculate yearly sum
ep<- endpoints(sev40xts,on="years") 
period.apply(sev40xts,INDEX=ep,FUN=sum)


# alternative find valleys method -------------
# https://stats.stackexchange.com/questions/22974/how-to-find-local-valleys-valleys-in-a-series-of-data
find_valleys <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

#valleys
sev40valleys2<-find_valleys(sev40xts, m = 1)

# PRACTICE! Copy paste above code and revise for FCE water level data to look for valleys (lows) for practice
#FCE data on water level
#https://drive.google.com/file/d/17QmdKu37mNKOTaYvrytViUsrQk0OAXoO/view?usp=sharing


