# API proyección financiera




# 0. Entorno 
source("2_2_api_proyeccion_financiera/0. utils.R")





# 1. Cargar información
z0_study_area <- read_sf("../0_raw_data/3_ejemplo_api/ejemplo_api.kml") %>% 
  st_make_valid() %>% 
  st_transform(4326) %>% 
  st_set_crs(4326)

a0_lotes <- readRDS("../0_raw_data/2_rds_geo_data/lotes_2024.RDS") %>% 
  fast_intersect(poligono = z0_study_area,filter = T)


z1_tratamienos <-  readRDS("../0_raw_data/2_rds_geo_data/pot555_trtamientos.RDS")
z2_areas_actividad <- readRDS("../0_raw_data/2_rds_geo_data/pot555_are_actividad.RDS")
z3_planes_parciales <- readRDS("../0_raw_data/2_rds_geo_data/pot555_planes_parciales.RDS") %>% 
  st_make_valid()



supuestos <- 
  list(
    "tratamientos" =(
  data.frame(
    NOMBRE_TRATAMIENTO = unique(z1_tratamienos$NOMBRE_TRATAMIENTO),
    factor_tratamiento = sample(seq(0,1,.001),length(unique(z1_tratamienos$NOMBRE_TRATAMIENTO))))),
  "areas de actividad" =(
    data.frame(
      NOMBRE_TRATAMIENTO = unique(z2_areas_actividad$CODIGO_AREA_ACTIVIDAD),
      factor_area_actividad = sample(seq(0,1,.001),length(unique(z2_areas_actividad$CODIGO_AREA_ACTIVIDAD))))))


supuestos[1]
supuestos[2]


a1_lotes <- a0_lotes %>% 
  st_join(z1_tratamienos,join = st_within) %>% 
  st_join(z2_areas_actividad,join = st_within) %>%  
  st_join(z3_planes_parciales,join = st_within) %>% 
  as.data.frame()



# Similación de englobes


replicate(
  10,
  sample())
