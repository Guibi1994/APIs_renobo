# Conversión de información








# Mapa base
b01_rios <- st_read("0_raw_data/1_geo_data/CuerpoAgua.shp") %>%
  st_make_valid() %>% 
  st_transform(crs = 4326) %>% 
  st_set_crs(4326)

saveRDS(b01_rios,"0_raw_data/2_rds_geo_data/cuerpos_hidricos.RDS")

b02_parques <- st_read("0_raw_data/1_geo_data/parques.shp") %>%
  st_make_valid() %>% 
  st_transform(crs = 4326) %>% 
  st_set_crs(4326)

saveRDS(b02_parques,"0_raw_data/2_rds_geo_data/parques.RDS")


b03_manzanas <- st_read("0_raw_data/1_geo_data/ManzanaEstratificacion.shp") %>%
  st_make_valid() %>% 
  st_transform(crs = 4326) %>% 
  st_set_crs(4326)

saveRDS(b03_manzanas,"0_raw_data/2_rds_geo_data/manzanas.RDS")




# Bases de análisis

## sisben  -----
a1_sisben_city <- read.delim2("0_raw_data/s4_31marzo.txt", sep = ";") %>% 
  mutate(lon = as.numeric(coord_x_auto_rec),
         lat = as.numeric(coord_y_auto_rec)) %>%  
  filter(!is.na(lon),!is.na(lat)) %>% 
  st_as_sf(coords = c("lon","lat")) %>% 
  st_make_valid() %>%
  st_transform(crs = 4326) %>% 
  st_set_crs(4326)


saveRDS(a1_sisben_city,"0_raw_data/2_rds_geo_data/sisben.RDS")


## estratos
a2_stratum <- read_sf("0_raw_data/1_geo_data/ManzanaEstratificacion.shp") %>%
  mutate(
    color = case_when(
      ESTRATO == 0~"grey20",
      ESTRATO ==1~renovo_scale[1],
      ESTRATO ==2~renovo_scale[2],
      ESTRATO ==3~renovo_scale[3],
      ESTRATO ==4~renovo_scale[4],
      ESTRATO ==5~renovo_scale[5],
      T~renovo_scale[6]),
    ESTRATO = ifelse(ESTRATO==0,"Sin estrato",ESTRATO),
    ESTRATO = as.factor(ESTRATO)) %>% 
  st_make_valid() %>%
  st_transform(crs = 4326) %>% 
  st_set_crs(4326)

## Censos

### 2018 
a3_censo2018 <- read_sf("0_raw_data/1_geo_data/MGN_ANM_MANZANA.shp") %>% 
  st_make_valid() %>%
  st_transform(crs = 4326) %>% 
  st_set_crs(4326)

saveRDS(a3_censo2018,"0_raw_data/2_rds_geo_data/censo2018.RDS")




