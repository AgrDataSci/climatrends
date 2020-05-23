library("climatrends")
library("tidyverse")
library("PlackettLuce")

load("inst/paper/redbeans.rda")


gdd <- GDD(modis, 
          day.one = redbeans$planting_date, 
          degree.days = 900, 
          return.as = "ndays")


gdd %<>%
  mutate(season = redbeans$season) %>% 
  group_by(season) %>% 
  summarise(gdd = round(mean(gdd), 0))
  

redbeans <- merge(redbeans, gdd, by = "season")

temp <- temperature(modis, day.one = redbeans$planting_date, span = redbeans$gdd)

dat <- cbind(G = redbeans$G, temp)

plot(density(dat$maxNT))

pl <- pltree(G ~ maxNT, data = dat, minsize = 200, bonferroni = TRUE)

print(pl)

plot(pl)
