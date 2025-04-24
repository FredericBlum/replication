library(readr)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

###################################
languages <- read_csv('data.csv', na=c('')) %>% 
  distinct(wd_id, language, macroarea, family, latitude, longitude) %>% 
  group_by(language, macroarea, family, latitude, longitude) %>% 
  summarise(count=n())

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
  geom_point(
    data=languages,
    aes(longitude, latitude),
    size=4, shape=21, fill='black', alpha=0.5
  ) +
  scale_x_continuous(name=NULL, breaks=NULL) +
  scale_y_continuous(name=NULL, breaks=NULL) +
  theme_bw() +
  guides(alpha = 'none') +
  theme(legend.position="right") 

map_lb
ggsave('map.pdf', map_lb, width=20, height=12, dpi=500)
