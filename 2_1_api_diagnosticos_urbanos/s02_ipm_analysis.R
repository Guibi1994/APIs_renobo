# API IPM

# 0. Prepare environment ----
source("01_functions/utils.R")
AE = "AE Calle 72"

unique(z0_study_area$NOMBRE)

# 1. Load information ----

## 1.1. Study area
z0_study_area <- st_read("0_raw_data/1_geo_data/Actuaciones_estrategicas_12042024.shp")

## 1.2. Target inputs (IPM blocks)
a0_imp <- read_sf("0_raw_data/1_geo_data/VULNRB_IPMxMZ.shp")



# 2. Process information ----

## 2.1 Study area
z1_study_area <- z0_study_area %>% 
  filter(NOMBRE ==AE) %>% 
  st_transform(crs = 4326)

## 2.2. Target inputs
#### A. Targets in the City
a1_ipm_city <- a0_imp %>% 
  filter(COD_MPIO == "11001") %>% 
  st_transform(crs = 4326)

### B. Targets in the Study area
a2_ipm_sa <- geo_intersect(
  target = a1_ipm_city,
  area = z1_study_area)

## 2.3. Extract map limits
limits <- st_bbox(a2_ipm_sa)
map_limits <- coord_sf(xlim = c(limits[1],limits[3]),
                       ylim = c(limits[2],limits[4]))


# 3. Creating the MAP ----
my_map <- ggplot()+
  # 1. Target
  ## 1.1. Target info
  geom_sf(data = a2_ipm_sa, aes(fill = ipm),
          color = "grey40")+
  ## 1.2. Target color
  scale_fill_gradient(
    low = "white",
    high = renovo[3])+
  # 2. Base map
  ## 1. Study Area
  geom_sf(data = z1_study_area, fill = NA,
          color = "black", lwd = 0.5,lty =1)+
  # 3. Theme, limits and labels
  map_format+
  map_limits

my_map

# 4. Save MAP
ggsave(
  paste0("02_test_figures/01_ipm_map_",
         AE,".png"),
       my_map,
       h = 5,w = 5)
ggs


# 5. Crating plot
# Data
my_plot <- rbind(
  data.frame(
    grupo = "Ciudad",
    ipm = a1_ipm_city$ipm),
  data.frame(
    grupo = "Pieza",
    ipm = a2_ipm_sa$ipm)) %>% 
  
  # Plot
  ggplot(aes(ipm, grupo, fill = grupo))+
  geom_boxplot(outlier.shape = NA)+
  scale_fill_manual(values = c("grey",renovo[3]))+
  
  stat_summary(fun.y = mean, geom = "point",
               shape = 20, size = 4, color = "grey20")+
  
  coord_cartesian(xlim = c(0,40))+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "none")+
  labs(y = "")
my_plot

ggpubr::ggarrange(
  my_map, my_plot,
  nrow = 1)

ggsave(
  paste0("02_test_figures/01_ipm_map_",
              AE,".png"),
  w = 5,h = 4)


# 6. Descripción

list(
  instruccion = "Escribe dos parrafos. El primero explicando brevemente que
  es el IPM y la importancia de tomarlo en cuenta al evaluar políticas e 
  internvenciones urbanas. En el segundo describe los datos, enfocado en comparar
  la pieza con la ciudad, el centro del análisis es la pieza",
  contexto = "Son datos del Indice de pobreza multidimensional, 
  representan el porcentage de personas en una manzana consideradas
  en vulnerabilidad en la ciudad de Bogotá",
  nombre = paste0("la pieza a evaluar se llama ",AE),
  rol = "Habla como persona experta en pobreza",
  voz = "tercera persona del prural",
  extension = "2 parrafos",
  datos = 
    rbind(
      data.frame(
        grupo = "Ciudad",
        ipm = a1_ipm_city$ipm),
      data.frame(
        grupo = "Pieza",
        ipm = a2_ipm_sa$ipm)) %>% 
    group_by(grupo) %>% 
    summarise(
      promedio = mean(ipm, na.rm = T),
      std = sd(ipm,na.rm = T),
      max = max(ipm, na.rm = T))) %>% 
  jsonlite::toJSON(pretty = T)





