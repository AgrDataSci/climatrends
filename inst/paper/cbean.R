library("climatrends")
library("tidyverse")
library("PlackettLuce")

load("inst/paper/cbean.rda")


# compute the number of days required to accumulate gdd from 
# planting date to maturity
gdd <- GDD(modis, 
           day.one = cbean$planting_date, 
           degree.days = 900, 
           return.as = "ndays")

# add gdd to the cbean data
# and take the average of gdd per season
cbean %<>%  
  mutate(gdd = gdd$gdd) %>% 
  group_by(season) %>% 
  mutate(gdds = as.integer(mean(gdd)))
  
# compute the temperature indices from planting date to the number of days 
# required to accumulate the gdd in each season
temp <- temperature(modis, 
                    day.one = cbean$planting_date, 
                    span = cbean$gdds)

cbean <- cbind(cbean, temp)

plot(density(cbean$maxNT))
plot(density(cbean$gdd))

plt <- pltree(G ~ maxNT, data = cbean, minsize = 200)

plot(plt, abbrev = 6)

library("patchwork")

A <- 
ggplot(cbean, aes(gdd)) +
  geom_density() +
  labs(x = "GDD", y = "Density", title = "A") +
  theme_bw() + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        plot.title = element_text(size=16, 
                                  colour = "grey20", 
                                  face = "bold"),
        axis.text.x = element_text(size = 12, angle = 0,
                                   face="plain", colour = "grey20"),
        axis.title.x = element_text(size=12, colour = "grey20"),
        axis.text.y = element_text(size=12, angle = 0,
                                   hjust=1, vjust=0.5,
                                   face="plain", colour = "grey20"),
        axis.title.y = element_text(size=12, colour = "grey20"))

B <- 
ggplot(cbean, aes(season, maxNT)) +
  geom_violin(aes(fill = season), show.legend = FALSE) +
  geom_jitter(height = 0, width = 0.1) +
  scale_fill_manual(values = c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')) +
  labs(x = "Season", y = "maxNT", title = "B") +
  theme_bw() + 
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        plot.title = element_text(size=16, 
                                  colour = "grey20", 
                                  face = "bold"),
        axis.text.x = element_text(size = 12, angle = 0,
                                   face="plain", colour = "grey20"),
        axis.title.x = element_text(size=12, colour = "grey20"),
        axis.text.y = element_text(size=12, angle = 0,
                                   hjust=1, vjust=0.5,
                                   face="plain", colour = "grey20"),
        axis.title.y = element_text(size=12, colour = "grey20"))

ggparty(plt) +
  geom_edge() +
  geom_edge_label() +
  geom_node_label(aes(label = splitvar), ids = "inner") 

pf <- (A / B) | C

pf




