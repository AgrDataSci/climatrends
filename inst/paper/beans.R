#set paraters for spatial analysis
my_proj <- "+proj=longlat +datum=WGS84"
my_ext <- raster::extent(-87, -83, 12, 15)
modis_factor <- 0.02
k_to_c <- 273.15

#add modis data 
cat("Readind MODIS data. Time:", date(), "\n")
# #read modis
# dates <- NULL
# for(i in c(2003:2016))
#   dates <- c(dates, c(seq(as.Date(paste0(i, "-01-01"), "%Y-%m-%d"), as.Date(paste0(i, "-12-31"), "%Y-%m-%d"), 8)))
# dates <- as.Date(c(dates, seq(as.Date("2017-01-01", "%Y-%m-%d"), as.Date("2017-09-06", "%Y-%m-%d"), 8)), origin = "1970-01-01")
# 
# full_dates <- seq(dates[1], dates[length(dates)], 1)
# #read modis day and night
# modis <- array(NA, dim = c(n, length(dates), 2), dimnames = list(NULL, as.character(dates), c(1:2)))
# for(i in 1:2){
#   if(i == 1 ) m="day" else m="night"
#   file <- raster::stack(list.files("E:/rasters/modis/smooth_Nicaragua/", pattern = m, full.names = T))
#   raster::crs(file) <- my_proj
#   x <- raster::extract(file, mydata[,c("lon","lat")])
#   x <- x * modis_factor - k_to_c #convert to Celsius
#   x[x<1] <- NA #remove wrong data
#   modis[ , , i] <- x
# }
# sum(is.na(modis))
# #apply linear interpolation to the 8-day temperature
# modis_approx <- array(NA, c(n, length(full_dates), 2), dimnames = list(NULL,  as.character(full_dates), c(1:2) ))
# for(i in 1:dim(modis)[1]){
#   modis_approx[i,,1] <- approx(modis[i,,1], n = length(full_dates))[[2]]
#   modis_approx[i,,2] <- approx(modis[i,,2], n = length(full_dates))[[2]]
# }
# save(modis, modis_approx, file = "./processing/modis_nicaragua.RData")
load("./processing/modis_nicaragua.RData")

# # #take CHIRPS info
# cat("Readind CHIRPS data. Time:", date(), "\n")
# #source CHIRPS (precipitation data)
# #http://chg.geog.ucsb.edu/data/chirps/
# years <- seq(2003 , 2017, 1)
# chirps <- list()
# #run CHIRPS over the chosen years
# for (i in years){
#   print(i)
#   info <- raster::stack(list.files(paste0("E:/rasters/CHIRPS/", i), pattern = ".tif", full.names = T))
#   #change name and number order in dates of chirps info
#   names(info) <- gsub("chirps.v2.0.", "d", names(info))
#   chirps[paste("y", i ,sep="")] <- info
#   rm(info)
# }
# #convert list into one large raster stack
# chirps <- raster::stack(chirps)
# #chirps <- raster::crop(chirps, my_ext)
# #extract precipitation info using lat and lon of each farm
# chirps <- raster::extract(chirps, mydata[,c("lon","lat")])
# sum(is.na(chirps))
# dimnames(chirps)[[2]] <- gsub("d", "", dimnames(chirps)[[2]])
# dimnames(chirps)[[2]] <- gsub("[.]", "-", dimnames(chirps)[[2]])
# save(chirps, file = "./processing/chirps_nicaragua.RData")
load("./processing/chirps_nicaragua.RData")