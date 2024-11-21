# 0. Base libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(stringr)
library(googledrive)
library(googlesheets4)
library(ggimage)
library(ggspatial)
options(scipen = 100)



# 0. keys
ai_key = readLines("..\\keys/open_ai.txt")


# 1. Catalaogo de datos espaciales (local) ----
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


# 3. Funciones espaciales complementarias ----


## 3.1. Re-aumentado de marco ----
nst_bbox <- function(st, aumento_p) {
  # sacar coordenadas límite
  my_box = st_bbox(st)
  
  # aumentar las coordenadas en "p" porcentaje
  x = (my_box[3]-my_box[1])#*aumento_p
  y = (my_box[4]-my_box[2])#*aumento_p
  
  if (y >= x) {
    y = (y+x)*aumento_p
  } else {
    x = (y+x)*aumento_p
  }
  
  my_box[1] = my_box[1]-x #(x*3) # Añadido por formato integrado de renobo
  my_box[3] = my_box[3]+x
  my_box[2] = my_box[2]-y
  my_box[4] = my_box[4]+y
  
  
  # Devlorver el marco reaumentado
  my_box <- st_as_sfc(my_box)
  return(my_box)
  
}


  


## 3.2. Ranguas cuantílicos ----
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



# 4. Funciones cartográficas ----

## 4.1. Generar el mapa base ----
gen_base_map <- function(box) {
  # a. Cargar información base
  #norte <- png::readPNG("..\0_raw_data/4_iconos/norte.png")
  b01_rios <- readRDS("../data/00_geo_data/2_rds_geo_data/cuerpos_hidricos.RDS") %>%
    st_intersection(box)
  b02_parques <- readRDS("../data/00_geo_data/2_rds_geo_data/parques.RDS") %>%
    st_intersection(box) 
  b03_manzanas <- readRDS("../data/00_geo_data/2_rds_geo_data/manzanas.RDS") %>%
    st_intersection(box)
  b04_estaciones_metro <- readRDS("../data/00_geo_data/2_rds_geo_data/estaciones_metro.RDS") %>% 
    st_intersection(box) %>% 
    mutate(icon = "../data/00_geo_data/3_iconos/metro_bogota.PNG")
  b05_red_metro <- readRDS("../data/00_geo_data/2_rds_geo_data/red_metro.RDS") %>% 
    st_intersection(box)
  b06_estaciones_regiotram <- readRDS("../data/00_geo_data/2_rds_geo_data/estaciones_regiotram.RDS") %>% 
    st_intersection(box) %>% 
    mutate(icon = "../data/00_geo_data/3_iconos/regio_tram.png")
  b07_red_regiotram <- readRDS("../data/00_geo_data/2_rds_geo_data/red_regiotram.RDS") %>% 
    st_intersection(box)
  
  
  # b. crear mapa base
  my_basemap <- ggplot()+
    # Parques, manzanas y riós
    geom_sf(data = b03_manzanas, fill = renovo_base[3], color = NA)+
    geom_sf(data = b02_parques, fill = renovo_base[2], color = NA)+
    geom_sf(data = b01_rios, fill = renovo_base[1], color = NA)+
    # Redes
    ## Regiotram
    geom_sf(data = b06_estaciones_regiotram,size = 5, color = "white")+
    geom_image(data = b06_estaciones_regiotram, 
               aes(x = st_coordinates(b06_estaciones_regiotram)[,1],
                   y = st_coordinates(b06_estaciones_regiotram)[,2],
                   image = icon), size = 0.03)+
    geom_sf(data = b07_red_regiotram, lty = 1,color = "#00427D",lwd = .3)+
    geom_sf(data = b07_red_regiotram, lty = 3,color = "#00427D",lwd = 0.8)+
    ## Metro
    #geom_sf(data = b04_estaciones_plmb,size = 0.1, alpha = 0.001)+
    geom_image(data = b04_estaciones_metro, 
               aes(x = st_coordinates(b04_estaciones_metro)[,1],
                   y = st_coordinates(b04_estaciones_metro)[,2],
                   image = icon), size = 0.03)+
    geom_sf(data = b05_red_metro, lty = 1,color = "#F07C16",lwd = .3)+
    geom_sf(data = b05_red_metro, lty = 3,color = "#F07C16",lwd = 0.8)+
    
    
    # Formato, norte y escala
    map_format +
    annotation_scale(location = "tr", width_hint = .5,
                     style = "ticks", line_width =.5, 
                     text_cex = .5, unit_category = "metric")
  
  return(my_basemap)
  
}

# 5. Funciones complementarias ----
## 5.1. Salvar gráficas en drive ----
ggdrive_save <- function(
    plot,
    drive_location,
    name,w =5,h=5) {
  # Description: This funtion exports an image to drive
  
  # a. Local saving
  ggplot2::ggsave(
    plot = plot,
    filename = name,
    width = w,
    height = h)
  # b. Drive saving
  googledrive::drive_upload(
    media = name,
    path = paste0(drive_location,"/",name),
    overwrite = T)
  # c. Removign local file
  file.remove(name)
}

# 6. Llaves

ai_key = readLines("..\\keys/open_ai.txt")






