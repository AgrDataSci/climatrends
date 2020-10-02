# Activity, home range and thus trapability  vs.  variation in weather per session?
# Activity, home range and thus trapability  vs.  season?
# climatrends paper

# # Import rawdata
# rm(list=ls())
# source("scripts/1_tidying_rawdata.R")
# 
# df <- alldata %>% # OBS this is bank voles from two different habitats / trapping designs
#   select(session_start,
#          individual_id,
#          live_myodes_glareolus,
#          dead_myodes_glareolus,
#          trap_id,
#          grid_id)
# 
# df <- df %>%
#   filter(live_myodes_glareolus > 0 | dead_myodes_glareolus > 0) %>%
#   select(-live_myodes_glareolus, -dead_myodes_glareolus)
# 
# df$session_start <- as.Date(as.character(df$session_start))
# df <- df %>%
#   filter(session_start > "2017-05-10") %>%
#   filter(session_start < "2021-01-01")
# 
# write.csv(df, "processed_files/bank_vole_data_to_climatrends.csv",
#           row.names = FALSE)

# set environment
library(dplyr)
library(lubridate)
library(ggplot2)
se <- function(x) sqrt(var(x)/length(x))
df <- read.csv("inst/paper/vole_data.csv")

# select habitat / study design
## Cross design:
# df <- df %>%
#   filter(grid_id != "M1") %>%
#   filter(grid_id != "M2") %>%
#   select(-grid_id)

## or line transect:
# df <- df %>%
#   filter(grid_id == "M1" | grid_id == "M2") %>%
#   select(-grid_id)

## Merge month id in case we want to combine the two datasets/habitats and treat as one
# df$session_start <- as.Date(paste(substr(df$session_start, 0,7), "10", sep = "-"))


# make overview / table of what traps that was used by whom in each session (ie. month)
dfSession <- df %>%
  group_by(session_start) %>%
  distinct(individual_id, trap_id) %>%
  ungroup()


# count number of unique traps an individual is trapped in during the same trapping session
dfCount <- as.data.frame(table(dfSession$individual_id, dfSession$session_start)) 
dfCount <- dfCount %>% #remove ghosts
  filter(Freq > 0)

# rename
dfCount <- dfCount %>% 
  rename("individual_id" = Var1) %>%
  rename("date" = Var2)
dfCount$individual_id <- as.character(dfCount$individual_id)
dfCount$date <- as.Date(dfCount$date)


# combine months into season if too little data
# ...


# estimate trapability statistics, but what statistics?
dfStats <- dfCount %>%
  group_by(date) %>%
  summarise(mean = mean(Freq), sd = sd(Freq), se = se(Freq)) %>%
  ungroup()


# plot timeseries
ggplot(dfStats, aes(month(date), se)) +
  geom_point() +
  geom_smooth(method = "loess")


###############
# Climate data over the period: 
min(dfStats$date)
max(dfStats$date)

# Downloaded from eklima.no
weatherDescription <- read.csv("inst/paper/vole_temp.txt",
                               skip = 8,
                               nrows = 13)

weatherData <- read.csv("inst/paper/vole_temp.txt",
                    skip = 40,
                    sep = "")

weatherData$Date <- as.Date(weatherData$Date, "%d.%m.%Y")


# select only daily mean temp. for now
tempData <- weatherData %>%
  select(Date, TAM, TAN, TAX) %>%
  rename("date" = Date)


# Each date in dfStats$date corresponds to trapping data during:
# t, t+1 morning, t+1 evening, t+2 morning, t+2 evening, t+3 morning.

# Make function to make mean temp. for each t of t(0-3). Or can climatrends functions do this?
#...


#plot movement vs. weather
dfJoin <- left_join(dfStats, tempData, by = "date")

ggplot(dfJoin, aes(TAM, se)) +
  geom_point()
#tbc..

dfJoin$date






