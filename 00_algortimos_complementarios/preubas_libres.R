
library(ggimage)
library(ggspatial)
source("2_1_api_diagnosticos_urbanos/utils.R")



pr <- data.frame(
  lon = sample(seq(4.5,4.6,0.001),size = 5),
  lat = sample(seq(4.5,4.6,0.001),size = 5)) %>% 
  mutate(lon2 = lon,
         lat2 = lat) %>% 
  st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_transform(crs = 4326)





 
pr$image = "..\\0_raw_data/4_iconos/metro_bogota.PNG"
pr$image = "..\\0_raw_data/4_iconos/regio_tram.png"
pr$image = "..\\0_raw_data/4_iconos/transmilenio.png"

ggplot() +
  geom_sf(data = pr,alpha = .001, size = .001)+
  geom_image(data = pr, aes(x = lon2,y=lat2, image = image))+
  annotation_scale(location = "tr", width_hint = .5,
                   style = "ticks", line_width =.5, 
                   text_cex = .5, unit_category = "metric")+
  map_format

#####

z0_area_estudio <- st_read("..\\0_raw_data/1_geo_data/Actuaciones_estrategicas_12042024.shp") %>%
  st_transform(crs = 4326) %>%
  st_set_crs(4326) %>% 
  filter(NOMBRE =="AE Calle 72")

my_box <- nst_bbox(z0_area_estudio, aumento_p = 0.01)

my_basemap <- gen_base_map(my_box)


my_basemap+
  geom_sf(data = z0_area_estudio, fill =NA)

ggsave(filename = "mapa_prueba.png", h = 10, w = 10)




## 
pr <- readRDS("..\\0_raw_data/2_rds_geo_data/red_slmb.RDS")


ggplot()+
  geom_sf(data = pr)



