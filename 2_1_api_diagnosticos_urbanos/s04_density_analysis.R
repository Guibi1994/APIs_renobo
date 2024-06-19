# Population density analisys

source("01_functions/utils.R")
AE = "AE Zona Industrial"



# 1. Load information

## 1.1. Study area
z0_study_area <- st_read("0_raw_data/1_geo_data/Actuaciones_estrategicas_12042024.shp")

## 1.2. Target inputs (IPM blocks)
a0_2005 <- st_read(dsn = gdb_path, "C1_Manzanas_DANE_2005")
b0_2018 <-st_read(dsn = gdb_path, "B6_Manzanas_CENSO_2018")



# 2. Process information ----

## 2.1 Study area
z1_study_area <- z0_study_area %>% 
  filter(NOMBRE ==AE) %>% 
  st_transform(crs = 4326)
  


## 
a1_2005_city <- a0_2005 %>% 
  st_make_valid() %>% 
  select(total_pob = Total_Pobl, 
         m2 = Shape_Area) %>% 
  mutate(hab_ha = total_pob/(m2/10000)) %>% 
  st_transform(crs = 4326) %>% 
  st_set_crs(4326) %>% 
  mutate(
    study_area = lengths(
      st_intersects(st_centroid(.[]), z0_area_estudio)) > 0)

b1_2018_city <- b0_2018 %>%
  st_make_valid() %>% 
  select(total_pob= SEXO_TOTAL, 
         m2 = Shape_Area) %>% 
  mutate(hab_ha = total_pob/(m2/10000)) %>% 
  st_transform(crs = 4326) %>% 
  st_set_crs(4326) %>% 
  mutate(
    study_area = lengths(
      st_intersects(st_centroid(.[]), z0_area_estudio)) > 0)

### B. Targets in the Study area
a2_2005_sa <- a1_2005_city %>% 
  filter(study_area==T) 

b2_2018_sa <- b1_2018_city %>% 
  filter(study_area==T) 


ab1_2005_2018 <- rbind(
  a2_2005_sa %>% 
    mutate(year = 2005),
  b2_2018_sa %>% 
    mutate(year = 2018)) %>% 
  mutate(den_ranks = qranks(hab_ha, include_cero = F))



## 2.3. Extract map limits
limits <- st_bbox(z1_study_area)



# 3. Creating the MAP ----
my_map <- ggplot()+
  # 1. Target
  ## 1.1. Target info
  geom_sf(data = ab1_2005_2018, aes(fill = rangos),
          color = "grey40")+
  ## 1.2. Target color
  scale_fill_manual(
    values = c("white","pink",renovo[3],
    "red3","red4"))+
  # 2. Base map
  ## 1. Study Area
  geom_sf(data = z1_study_area, fill = NA,
          color = "black", lwd = 0.5,lty =1) +
  # 3. Theme, limits and labels
  map_format+
  facet_wrap(.~year)+
  labs(fill = "Hab/Ha")
  

my_map
ggsave("02_test_figures/05. densidades.png",
       h = 4, w = 6)

# Definir los intervalos como números
intervalos <- c(0, 1, 50, 100, 200, 1810)
length(intervalos)

# Definir las etiquetas de los intervalos
etiquetas <- c("0-1", "1-50", "50-100", "100-200", "200-1810")
length(etiquetas)


resume <-   ab1_2005_2018 %>% 
  group_by(year) %>% 
  summarise(hab_ha_avg = mean(hab_ha, na.rm = T),
            hab_ha_sd = sd(hab_ha, na.rm =T)) %>% 
  as.data.frame() %>% .[1:3]


list(
  instruccion = "Escribe dos parrafos. El primero explicando brevemente que
  son las desidades poblacionales su  importancia de tomarlo en cuenta al evaluar políticas e 
  internvenciones urbanas. En el segundo describe los datos, enfocado en comparar
  la pieza con la ciudad, el centro del análisis es la pieza",
  contexto = "Son datos habitantes por hectárea y numero de manzanasen el contexto
  de un analisis de la pieza en la ciudad de Bogotá",
  nombre = paste0("la pieza a evaluar se llama ",AE),
  rol = "Habla como persona experta en pobreza",
  voz = "tercera persona del prural",
  extension = "2 parrafos",
  datos = resume) %>% 
  jsonlite::toJSON(pretty = F)

