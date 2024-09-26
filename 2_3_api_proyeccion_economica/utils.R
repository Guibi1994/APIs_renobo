# 0. Librerias ----
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(broom)
library(stargazer)
library(kableExtra)
library(caret)
library(googlesheets4)
library(googledrive)
library(sf)
options(scipen = 20)



# 1. Funciones ----

## 1.1. Saturación de distancias ----
### Crea dummies a partir de distancias en metros
dist_saturation <- function(
    base, variable, min =0, max =1000,interval = 50) {
  message("Se asumen distancias en metros")
  # Extraer nombre de la variable
  var_name = base %>% select({{variable}}) %>% names()
  length = length(seq(min,max,interval))
  
  # Aplicar la saturación
  base <- base %>% 
    arrange({{variable}}) %>% 
    mutate(temporal_id = row_number()) %>% 
    mutate( 
      #ref = {{variable}},
      {{variable}} := cut({{variable}}, breaks = c(seq(min,max,interval),Inf),
                        labels = 
                          c(paste0(
                            var_name,"_d",
                            str_pad(1:length,width = 2,pad = "0"),"_",
                            seq(min,max,interval),"_",
                            c(seq((min+interval),max,interval),"o_mas"))),right = FALSE)) %>% 
    mutate(dummy = 1) %>%
    pivot_wider(
      id_cols = everything(),
      names_from = {{variable}},
      values_from = dummy,
      values_fill = 0) %>% 
    select(-temporal_id)

  return(base)
  
}

## 1.2 Filtro cuantilico ----
### Filtra un % de la base teniendo en cuenta el valor cuantilico determinado
### por ambas colas (i.e. 5% elimina el 2.5% inferior y 2.5% superior)
quantilic_filter <- function(base, variable, expansion = 1.5) {
  
  var = base %>% pull({{variable}})
  
  lower_limit <- quantile(var,probs = .25, na.rm = T)-(IQR(var, na.rm = T)*expansion)
  upper_limit <- quantile(var,probs = .75, na.rm = T)+(IQR(var, na.rm = T)*expansion)
  
  base <- base %>%
    filter(
      between({{variable}},
              lower_limit,
              upper_limit))
  return(base)
}

## 1.3. Etiquetas  de significancia ----
signicance_labels <- function(tidy_results) {
  tidy_results <- tidy_results %>% 
    mutate(
      effect_type = 
        case_when(
          (p.value <0.05 & estimate <0)==T~"1. Significativo negativo",
          (p.value <0.05 & estimate >0)==T~"2. Significativo positivo",
          T~"0. No significativo"),
      stars = 
        case_when(
          (p.value <0.01)==T~"***",
          (p.value <0.05)==T~"**",
          (p.value <0.1)==T~"*",
          T~""))
  return(tidy_results)
}

## 1.4. Saturación cuantílica ----
### Esta función crea una saturación cuantíica a partir de cualquier variable
###   asumiendo que los ouliers ya fueron previamente retirados

quantile_saturation <-function(base, variable, cohorts = 10) {
  var = base %>% pull({{variable}})
  groups = c(
    as.numeric(
      quantile(var, probs = c(seq(0,1,1/cohorts)), na.rm = T)[1:cohorts]),Inf)
  
  var_name = base %>% select({{variable}}) %>% names()
  
  # Aplicar la saturación
  base <- base %>% 
    arrange({{variable}}) %>% 
    mutate(temporal_id = row_number()) %>% 
    mutate( 
      #ref = {{variable}},
      {{variable}} := cut(
        {{variable}},
        breaks = groups,
        labels = 
          c(paste0(
            var_name,"_Q",
            str_pad(1:cohorts,width = 2,pad = "0"))),right = FALSE)) %>% 
    mutate(dummy = 1) %>%
    pivot_wider(
      id_cols = everything(),
      names_from = {{variable}},
      values_from = dummy,
      values_fill = 0) %>% 
    select(-temporal_id)
  
  
  
}

## 1.5. Conversión integral a número ----
### Esta función detecta si un valor es un número oculto en un string y lo
###   transforma devuelta a número

convert_comma_numbers <- function(value) {
  value = ifelse(value == "",NA,value)
  value = ifelse(
    str_detect(value, "\\,") ==T &
      str_detect(value, "\\d") ==T &
      str_detect(value, "[A-Za-z]") ==F, 
    as.numeric(str_replace(value, ",",".")),value)
  
  return(value)
}

# Links

## Diccionario de datos
link_diccionario <- "https://docs.google.com/spreadsheets/d/1QTTEdmje0WHwJcFuhGlq5sujp-Y2ABH5mKYk5XO-XeE/edit?usp=sharing"



# 1-0.5
# 
# # Imposición Funcional grado 2
# b1 = 2583
# b2 = -26
# (b1*-1)/(2*b2)
# 
# 
# 
# 




# 2. Funciones de diseño ----


## 2.1. Paletas de colores ----
### Paleta 1
renovo = c("#a7e85d","#013334","#e36477","#62b7b2","#d1ce84")
### Paleta 2
renovo_scale = c("#006A68","#90CCCB","#d1ce84","#D89A7E","#e36477","#EC3434")
### Paleta 3 
renovo_base = c("#C4DCE2","#D2DAA6", "#F1F1F1")

## 2.2. Formato de gráficos ----
my_theme <- 
  theme_minimal()+
  theme(text = element_text(family = "serif"))

## 2.3. Orden de variables saturadas
orden_saturaciones = c("Otra cosa", "0-50","50-100","100-150","150-200","200-250","250-300","300-350",
  "350-400","400-450","450-500","500-550","550-600","600-650","650-700",
  "700-750", "750-800", "800-850", "850-900","900-950", "950-1000", 
  "1000 o más","Q01", "Q02", "Q03", "Q04","Q05", "Q06", "Q07","Q08",
  "Q09","Q10")


# 3. Funciones de administración ----

## 3.1. Guardar en Drive ----
ggdrive_save <- function(
    plot,
    drive_path,
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
    path = drive_path,
    overwrite = T)
  # c. Removign local file
  file.remove(name)
}



