# Utils API proyección financiera
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(stringr)
library(patchwork)
library(googledrive)
library(googlesheets4)
options(scipen = 100)






# 1. Geographic functions
## note: Esta función selecciona las entidades cuyo centroide esten dentro
  ## de un polígono de referencia

fast_intersect <- function(base,poligono, filter = F) {
  if (filter == T) {
    base <- base %>% 
      mutate(study_area = lengths(
        st_intersects(st_centroid(.[]), poligono)) > 0) 
    
  } else {
    base <- base %>% 
      mutate(study_area = lengths(
        st_intersects(st_centroid(.[]), poligono)) > 0)
  }
  
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
