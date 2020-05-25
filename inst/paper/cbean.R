library("climatrends")
library("tidyverse")
library("PlackettLuce")
library("patchwork")
library("ggparty")


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

# round values to 3 decimals, this will not change the main result, but will 
# help in visualizing the tree 
temp[1:ncol(temp)] <- lapply(temp[1:ncol(temp)], function(x) round(x, 3))

# combine the indices with the main data
cbean <- cbind(cbean, temp)

# fit a PL tree
plt <- pltree(G ~ maxNT, data = cbean, minsize = 200)

# ..................................
# ..................................
# Prepare charts ####
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
  #geom_jitter(height = 0, width = 0.1) +
  scale_fill_manual(values = c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')) +
  labs(x = "", y = "maxNT", title = "B") +
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



coeff <- as.vector(coef(plt, log = FALSE))
items <- rep(dimnames(coef(plt))[[2]], each = 2)
nit <- length(unique(items))

coeff <- data.frame(coeff = coeff, items = items, 
                    node = rep(c("Node 2","Node 3"),
                               times = length(items)))

D <-
ggplot(coeff, aes(x = coeff, y = items, group = node)) +
  geom_vline(xintercept = 1/nit, 
             colour = "#E5E7E9", size = 0.8) +
  geom_point() +
  facet_wrap(~ node) +
  labs(x = "", y="") +
  theme_bw() + 
  theme(panel.background = element_blank(), 
        strip.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        plot.title = element_text(size=16, 
                                  colour = "grey20", 
                                  face = "bold"),
        axis.text.x = element_text(size = 12, angle = 50, vjust = 0.5,
                                   face="plain", colour = "grey20"),
        axis.title.x = element_text(size=12, colour = "grey20"),
        axis.text.y = element_text(size=12, angle = 0,
                                   hjust=1, vjust=0.5,
                                   face="plain", colour = "grey20"),
        axis.title.y = element_text(size=12, colour = "grey20"))

C <-
ggparty(plt) +
  geom_edge() +
  labs(title = "C") +
  geom_edge_label() +
  geom_node_splitvar() +
  theme(plot.title = element_text(size=16, 
                                  colour = "grey20", 
                                  face = "bold"))

p <- A / B | (C / D)

p

ggsave(paste0("inst/paper/cbean.png"),
       plot = p,
       width = 21,
       height = 14,
       dpi = 1000,
       units = "cm")



