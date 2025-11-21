## ----temperature, message=TRUE, eval=TRUE, echo=TRUE--------------------------
library("climatrends")

data("innlandet", package = "climatrends")

temp1 = temperature(innlandet$tmax, innlandet$tmin)

temp1

## ----temperature2, message=TRUE, eval=TRUE, echo=TRUE-------------------------
temp2 = temperature(innlandet$tmax, innlandet$tmin,
                    dates = innlandet$dates,
                    timeseries = TRUE, 
                    intervals = 30)

temp2

## ----gdd, message=TRUE, eval=TRUE, echo=TRUE----------------------------------
gdd = GDD(innlandet$tmax, innlandet$tmin, tbase = 2, equation = "b")

gdd

## ----gdd2, message=FALSE, eval=FALSE, echo=TRUE-------------------------------
#  lonlat = data.frame(lon = 129.19,
#                      lat = 36.39)
#  
#  GDD(lonlat,
#      day.one = "2019-04-01",
#      last.day = "2019-10-01",
#      degree.days = 150,
#      return.as = "ndays")

## ----latefrost, message=TRUE, eval=TRUE, echo=TRUE----------------------------
lf = late_frost(innlandet$tmax, 
                 innlandet$tmin, 
                 dates = innlandet$date, 
                 base = 2)

lf

## ----rain, message=FALSE, eval=FALSE, echo=TRUE-------------------------------
#  library("nasapower")
#  
#  lonlat = data.frame(lon = c(-73.3, -74.5),
#                       lat = c(-6.1, - 6.2))
#  
#  rain = rainfall(lonlat,
#                   day.one = "2018-11-01",
#                   last.day = "2018-12-31")
#  
#  rain

## ----csenstive, message=FALSE, eval=FALSE, echo=TRUE--------------------------
#  library("sf")
#  data("lonlatsf", package = "climatrends")
#  
#  crop_sensitive(lonlatsf,
#                 day.one = "2018-12-01",
#                 last.day = "2019-01-31",
#                 as.sf = FALSE)

## ----eto, message=TRUE, eval=TRUE, echo=TRUE----------------------------------
data("temp_dat", package = "climatrends")

eto = ETo(temp_dat, 
           day.one = "2013-10-28",
           span = c(9, 10, 11, 12, 8, 10, 11, 11, 12, 10),
           lat = rep(25, 10),
           Kc = 0.92)

eto

