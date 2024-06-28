# Conversión de información


source("2_1_api_diagnosticos_urbanos/utils.R")
gdb_pot <- "../0_raw_data/1_geo_data/POT_Decreto_555_2021_Bogota_D_C.gdb"



# 1. Mapa base ----

## 1.1. Rios ----
b01_rios <- st_read("0_raw_data/1_geo_data/CuerpoAgua.shp") %>%
  st_make_valid() %>% 
  st_transform(crs = 4326) %>% 
  st_set_crs(4326)
saveRDS(b01_rios,"0_raw_data/2_rds_geo_data/cuerpos_hidricos.RDS")

## 1.2. Parques ----
b02_parques <- st_read("0_raw_data/1_geo_data/parques.shp") %>%
  st_make_valid() %>% 
  st_transform(crs = 4326) %>% 
  st_set_crs(4326)
saveRDS(b02_parques,"0_raw_data/2_rds_geo_data/parques.RDS")

## 1.3. Manzanas ----
b03_manzanas <- st_read("0_raw_data/1_geo_data/ManzanaEstratificacion.shp") %>%
  st_make_valid() %>% 
  st_transform(crs = 4326) %>% 
  st_set_crs(4326)
saveRDS(b03_manzanas,"0_raw_data/2_rds_geo_data/manzanas.RDS")




# 2. Bases de análisis ----

## 2.1. SISBEN  -----
a1_sisben_city <- read.delim2("../0_raw_data/s4_31marzo.txt", sep = ";") %>% 
  mutate(lon = as.numeric(coord_x_auto_rec),
         lat = as.numeric(coord_y_auto_rec)) %>%  
  filter(!is.na(lon),!is.na(lat)) %>% 
  st_as_sf(coords = c("lon","lat"),crs = 4326) %>% 
  st_make_valid() %>%
  st_transform(crs = 4326) %>% 
  st_set_crs(4326)
saveRDS(a1_sisben_city,"../0_raw_data/2_rds_geo_data/sisben.RDS")


## 2.2. estratos ----
a2_stratum <- read_sf("../0_raw_data/1_geo_data/ManzanaEstratificacion.shp") %>%
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
saveRDS(a2_stratum,"../0_raw_data/2_rds_geo_data/estratos.RDS")


## 2.3. Censos ----
### 2.3.1. 2005 ----

### 2.3.2. 2018 ----
a3_censo2018 <- read_sf("../0_raw_data/1_geo_data/MGN_ANM_MANZANA.shp") %>% 
  st_make_valid() %>%
  st_transform(crs = 4326) %>% 
  st_set_crs(4326) %>% 
  filter(MPIO_CDPMP  == "11001") 
saveRDS(a3_censo2018,"../0_raw_data/2_rds_geo_data/censo2018.RDS")


# 3. Bases POT ----
## 3.1. Tratamientos ----
pot01_tratamietnos <- st_read(
  dsn = gdb_pot, layer = "Tratamiento_urbanistico") %>% 
  st_make_valid() %>% 
  st_transform(crs = 4326) %>% 
  st_set_crs(4326) %>% 
  select(NOMBRE_TRATAMIENTO)
saveRDS(pot01_tratamietnos,"../0_raw_data/2_rds_geo_data/pot555_trtamientos.RDS")

## 3.2. Áreas de actividad ----
pot02_areas_atividad <- st_read(
  dsn = gdb_pot, layer = "AreaActividad") %>% 
  st_make_valid() %>% 
  st_transform(crs = 4326) %>% 
  st_set_crs(4326) %>% 
  select(CODIGO_AREA_ACTIVIDAD) %>% 
  mutate(AREA_ACTIVIDAD = case_when(
    CODIGO_AREA_ACTIVIDAD == "AAPRSU" ~ "Área de Actividad de Protección y Restauración de la Estructura Ecológica Principal",
    CODIGO_AREA_ACTIVIDAD == "AAGSM" ~ "Área de Actividad General de Servicios Metropolitanos",
    CODIGO_AREA_ACTIVIDAD == "AAPGSU" ~ "Área de Actividad de Protección y Gestión del Suelo Urbano",
    CODIGO_AREA_ACTIVIDAD == "AAERVIS" ~ "Área de Actividad Especial de Reserva Vial",
    CODIGO_AREA_ACTIVIDAD == "PEMP" ~ "Plan Especial de Manejo y Protección",
    CODIGO_AREA_ACTIVIDAD == "AAERAE" ~ "Área de Actividad Especial de Regulación y Administración del Espacio Público",
    TRUE ~ "Código desconocido"))
saveRDS(pot02_areas_atividad,"../0_raw_data/2_rds_geo_data/pot555_are_actividad.RDS")

## 3.3. Estructura Ecológica Principal (sahpe dañado)----
pot03_eep <- read_sf("../0_raw_data/1_geo_data/EstructuraEcologicaPrincipal.shp") %>%
  # st_make_valid() %>% 
  st_transform(crs = 4326) %>%
  st_set_crs(4326)
saveRDS(pot03_eep,"../0_raw_data/2_rds_geo_data/pot555_estructura_ecologica.RDS")


## 3.4. Planes parciales -----
pot04_plan_parcial <- read_sf("../0_raw_data/1_geo_data/PlanParcial.shp") %>% 
  st_make_valid() %>% 
  st_transform(crs = 4326) %>% 
  st_set_crs(4326)
saveRDS(pot04_plan_parcial,"../0_raw_data/2_rds_geo_data/pot555_planes_parciales.RDS")



# 4. Bases Castrales -----
## 4.1. Lotes
c01_lotes <- read_sf("../0_raw_data/1_geo_data/LOTE.shp") %>% 
  st_make_valid() %>% 
  st_transform(crs = 4326) %>% 
  st_set_crs(4326)

saveRDS(c01_lotes,"../0_raw_data/2_rds_geo_data/lotes_2024.RDS")

