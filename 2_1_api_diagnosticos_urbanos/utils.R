# 0. Base libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(stringr)
library(patchwork)
options(scipen = 100)


# 1. Catalaogo de datos espaciales (local)
gdb_path <- "F:\\1. Datos Generales/2. Datos Base.gdb/"
# gdb_list <- st_layers(gdb_path)



# 2.Formato visual ----

## 2.1. Formato general ----
map_format <- 
  theme_minimal() +
  theme(
    # Titles
    text = element_text(family = "serif"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(
      hjust = 0.5, color = "grey40"),
    # Grid format
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.text = element_blank(),
    axis.title = element_blank(),
    # Leyend format
    legend.position = "bottom")


## 2.2. Función para extracción de leyendas ----
get_legend <- function(myggplot) {
  tmp <- ggplotGrob(myggplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}



## 2.3. Paletas

### Paleta 1
renovo = c("#a7e85d","#013334","#e36477","#62b7b2","#d1ce84")
### Paleta 2
renovo_scale = c("#006A68","#90CCCB","#d1ce84","#D89A7E","#e36477","#EC3434")
### Paleta 3 
renovo_base = c("#C4DCE2","#D2DAA6", "#F1F1F1")


# 3. Funciones espciales complementarias


## 3.1. Re-aumentado de marco
nst_bbox <- function(st, aumento_p) {
  # sacar coordenadas límite
  my_box = st_bbox(st)
  
  # aumentar las coordenadas en "p" porcentaje
  x = (my_box[3]-my_box[1])*aumento_p
  y = (my_box[4]-my_box[2])*aumento_p
  
  my_box[1] = my_box[1]-(x*3) # Añadido por formato integrado de renobo
  my_box[3] = my_box[3]+x
  my_box[2] = my_box[2]-y
  my_box[4] = my_box[4]+y
  
  # Devlorver el marco reaumentado
  return(my_box)
  
}

## 3.2. Ranguas cuantílicos
qranks <- function(var, n = 5, include_cero = T) {
  
  # Si no incluye "0" hacer "0" como una categoria aparte
  #   y hacer rangos con todos los demas valores
  if (include_cero ==F) {
    var = ifelse(var ==0,NA,var)
    n = n-1
  }
  
  # Probando rangos validos
  n = n
  q = quantile(var, probs = seq(0,1,1/n), na.rm = T)
  maxfreq = max(table(q))
  
  while (maxfreq >1) {
    n = n-1
    message("No hay ",n+1," rangos únicos: ","intentando con ",n," rangos")
    q = quantile(var, probs = seq(0,1,1/n))
    maxfreq = max(table(q))
    
  }
  message("Rangos finales validos: ",n)
  
  # Construyendo rangos finales
  lower = quantile(var,probs = seq(0,1,1/n), na.rm = T)[1:n]
  upper = quantile(var,probs = seq(0,1,1/n), na.rm = T)[2:(n+1)]
  labs = paste0(round(lower,1)," - ",round(upper,1))
  
  x = cut(
    var, 
    breaks = quantile(var,probs = seq(0,1,1/n), na.rm = T),
    labels = labs, include.lowest = T)
  
   x = ifelse(is.na(x),"0",x)
  x = factor(
    x,
    levels = as.character(0:n),
    labels = c("0",labs))
  
  return(x)
}


# 4. Funciones cartográficas

## 4.1. Generar el mapa base
gen_base_map <- function(box) {
  # a. Cargar información base
  b01_rios <- st_read("0_raw_data/1_geo_data/CuerpoAgua.shp") %>%
    st_make_valid() %>% 
    st_transform(crs = 4326) %>% 
    st_set_crs(4326) %>% 
    st_intersection(box)
  b02_parques <- st_read("0_raw_data/1_geo_data/parques.shp") %>%
    st_make_valid() %>% 
    st_transform(crs = 4326) %>% 
    st_set_crs(4326) %>% 
    st_intersection(box)
  b03_manzanas <- st_read("0_raw_data/1_geo_data/ManzanaEstratificacion.shp") %>%
    st_make_valid() %>% 
    st_transform(crs = 4326) %>% 
    st_set_crs(4326) %>% 
    st_intersection(box)
  # b. crear mapa base
  my_basemap <- ggplot()+
    geom_sf(data = b03_manzanas, fill = renovo_base[3], color = NA)+
    geom_sf(data = b02_parques, fill = renovo_base[2], color = NA)+
    geom_sf(data = b01_rios, fill = renovo_base[1], color = NA)+
    map_format
  
  return(my_basemap)
  
}






