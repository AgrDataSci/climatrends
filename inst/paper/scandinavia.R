library("climatrends")
library("patchwork")
library("sf")
library("nasapower")
library("tidyverse")
library("mapview")

e <- matrix(c(7, 59,
              17, 59,
              17, 63,
              7, 63,
              7, 59),
            nrow = 5, ncol = 2, byrow = TRUE)

# e <- matrix(c(-7.5, 37.2,
#                  -2, 37.2,
#                  -2, 40.5,
#               -7.5, 40.5,
#               -7.5, 37.2),
#             nrow = 5, ncol = 2, byrow = TRUE)
e

e <- st_polygon(list(e))
plot(e)
set.seed(435)
p <- st_sample(e, 100, type = "hexagonal")
p <- st_as_sf(p, crs = 4326)

mapview(p)

temp <- temperature(p, 
                    day.one = "2020-03-01", 
                    last.day = "2020-04-01", 
                    timeseries = TRUE, 
                    intervals = 365)

i <- c("CSDI","WSDI")


temp %>% 
  #filter(index %in% i) %>% 
  group_by(index) %>% 
  mutate(ab = mean(value)) %>% 
  ggplot() +
  geom_line(aes(x = date, y = value, group = id)) + 
  geom_hline(aes(yintercept = ab), colour = "red", lwd = 0.7) +
  geom_smooth(aes(x = date, y = value), method = "loess", se = FALSE) +
  facet_wrap(. ~ index, scales = "free") +
  labs(x = "", y = "Index (days)") +
  theme(axis.text.y = element_text(size = 12, angle = 0, hjust = 1, 
                                   vjust = 0.5, face = "plain", colour = "black"),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, 
                                   vjust = 1, face = "plain", colour = "black"),
        axis.title = element_text(size = 12, face = "plain"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        plot.background = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 12, colour = "black"),
        strip.text.y = element_text(size = 12, colour = "black"),
        strip.background = element_rect(colour = "black", fill="#FFFFFF")) ->
  gg1

gg1


i <- c("T10p","T90p")


temp %>% 
  filter(index %in% i) %>% 
  group_by(index) %>% 
  mutate(ab = mean(value)) %>% 
  ggplot() +
  geom_line(aes(x = date, y = value, group = id)) + 
  geom_hline(aes(yintercept = ab), colour = "red", lwd = 0.7) +
  geom_smooth(aes(x = date, y = value), method = "loess", se = FALSE) +
  facet_wrap(. ~ index, scales = "free") +
  labs(x = "Year", 
       y = expression(paste('Index (',~degree,'C)',sep=''))) +
  theme(axis.text.y = element_text(size = 12, angle = 0, hjust = 1, 
                                   vjust = 0.5, face = "plain", colour = "black"),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, 
                                   vjust = 1, face = "plain", colour = "black"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        plot.background = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_text(size = 12, colour = "black"),
        strip.text.y = element_text(size = 12, colour = "black"),
        strip.background = element_rect(colour = "black", fill="#FFFFFF")) ->
  gg2

gg2


gg1 / gg2


