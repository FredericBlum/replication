library(readr)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

###################################
languages <- read_csv('data/data.csv', na=c('')) %>% 
  group_by(language, latitude, longitude, family) %>% 
  count()

################################################
#####             Maps                     #####
################################################
languages$longitude<-sapply(languages$longitude,function(x) ifelse(x<(-25),x + 360,x))
world <- map_data('world', interior=F, wrap=c(-25,335), ylim=c(-54,79))

map_lb <- ggplot() +
  geom_polygon(
    data=world,
    aes(x=long,y=lat,group=group),
    colour="#F2DDC1",linewidth=0.2, fill="#F2DDC1"
  ) + 
  geom_jitter(
    data=languages,
    aes(longitude, latitude, fill=family),
    height=3, width=2, size=6, shape=21, alpha=0.7
  ) +
  scale_fill_viridis_d(option="D") +
  # geom_label_repel(box.padding=0.5, point.padding=0.5,
  #                  data=languages, aes(longitude, latitude, label=Name), 
  #                  min.segment.length=unit(0, 'lines'),
  #                  size=5, max.overlaps=99) +
  scale_x_continuous(name=NULL, breaks=NULL) +
  scale_y_continuous(name=NULL, breaks=NULL) +
  theme_bw() +
  theme(legend.position="none") 

map_lb
ggsave('images/map_lb.png', map_lb, scale=1, width=3000, height=2000, units="px")