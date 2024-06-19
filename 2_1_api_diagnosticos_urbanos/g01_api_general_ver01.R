# Fast Study API

# Descripción: esta API realiza el anáisis de los siguientes indicadores
#   1. SISBEN (a)
#   2. IPM (b)
#   3. Estratos (c)
#   4. Densidades poblacionlaes (d)


# 0. Preparar entorno ----
source("01_functions/utils.R")
AE = "AE Calle 72"

# 1. Cargar datos ----

## 1.1. Bases básicas ----
### 1.1. Areas de estudio ----
z0_area_estudio <- st_read("0_raw_data/1_geo_data/Actuaciones_estrategicas_12042024.shp") %>% 
 filter(NOMBRE == AE) %>%
  st_transform(crs = 4326) %>% 
  st_set_crs(4326)
  
### 1.2. Área de análisis y mapa base ----
z1_box <- st_as_sfc(nst_bbox(z0_area_estudio, aumento_p = .1))
my_basemap <- gen_base_map(z1_box)
my_basemap +
  geom_sf(data = z0_area_estudio,  aes(linetype = "Study Area"), fill = NA, lty = 2, lwd = 0.5)


saveRDS(pr, "0_raw_data/1_geo_data/IPM_bogota.RDS")

# 2. Análisis automáticos

## 2.1. SISBEN 
a0_sisben <- read.delim2("0_raw_data/s4_31marzo.txt", sep = ";") %>% 
  mutate(lon = as.numeric(coord_x_auto_rec),
         lat = as.numeric(coord_y_auto_rec)) %>%  
  filter(!is.na(lon),!is.na(lat)) %>% 
  st_as_sf(coords = c("lon","lat")) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = 4326)

a1_sisben <- st_intersection(a0_sisben, z0_area_estudio)
my_basemap +
  geom_sf(data = a1_sisben, aes(color = grupo),
          size = 2, alpha = .6)+
  ## 1.2. Target color
  scale_color_manual(
    "Grupo SISBEN",
    values = renovo_scale[1:4])+
  geom_sf(data = z0_area_estudio,  aes(linetype = "Study Area"), fill = NA, lty = 2, lwd = 0.5)+
  theme(legend.position = c(0.15, 0.4),
        legend.background = element_rect(
          fill = rgb(1, 1, 1, alpha = 0.8), color = NA)) 
ggsave("pruebas_sisben.png", h = 8,w = 8)

## 2.2. IPM
a2_ipm <- readRDS("0_raw_data/2_rds_geo_data/IPM_bogota_DANE2018.RDS") %>% 
  mutate(
    study_area = lengths(
      st_intersects(st_centroid(.[]), z0_area_estudio)) > 0)

a3_ipm <- a2_ipm %>% 
  filter(study_area==T) %>% 
  mutate(ipm_ranks = qranks(ipm, include_cero = F))


pr <- colorRampPalette(c("grey","pink","red4"))(5)
my_basemap +
  geom_sf(data = a3_ipm, aes(fill = ipm_ranks), color = NA)+
  scale_fill_manual(
    "IPM",
    values = pr)+
  geom_sf(data = z0_area_estudio,  aes(linetype = "Study Area"), fill = NA, lty = 2, lwd = 0.5)+
  theme(legend.position = c(0.15, 0.4),
        legend.background = element_rect(
          fill = rgb(1, 1, 1, alpha = 0.8), color = NA)) 
  
ggsave("pruebas_ipm.png", h = 8,w = 8)
  
  
  
## 2.3. Estratos
a0_stratum <- read_sf("0_raw_data/1_geo_data/ManzanaEstratificacion.shp")%>% 
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
  st_transform(crs = 4326) %>% 
  st_make_valid() %>%
  mutate(
    study_area = lengths(
      st_intersects(st_centroid(.[]), z0_area_estudio)) > 0)

a1_stratum_sa <- a0_stratum %>% 
  filter(study_area ==T)

my_basemap +
  geom_sf(data = a1_stratum_sa, aes(fill = ESTRATO), color = NA)+
  scale_fill_manual(
    "Estratos",
    values = setNames(a1_stratum_sa$color, a1_stratum_sa$ESTRATO))+
  geom_sf(data = z0_area_estudio,  aes(linetype = "Study Area"), fill = NA, lty = 2, lwd = 0.5)+
  theme(legend.position = c(0.15, 0.4),
        legend.background = element_rect(
          fill = rgb(1, 1, 1, alpha = 0.8), color = NA)) 
ggsave("pruebas_estratos.png", h = 8,w = 8)


# Den




my_basemap+
  geom_sf(data = ab1_2005_2018, aes(fill = den_ranks), color = NA)+
  scale_fill_manual(
    "Hab/ha",
    values = colorRampPalette(c("grey","pink","red4"))(5))+
  geom_sf(data = z0_area_estudio,  aes(linetype = "Study Area"), fill = NA, lty = 2, lwd = 0.5)+
  theme(legend.position = c(0.1, 0.4),
        legend.background = element_rect(
          fill = rgb(1, 1, 1, alpha = 0.8), color = NA))+
    facet_wrap(.~year)

ggsave("pruebas_densidades.png", h = 6,w = 12)
