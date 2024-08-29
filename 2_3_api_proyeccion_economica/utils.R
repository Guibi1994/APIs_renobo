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
options(scipen = 20)



# 1. Funciones ----

## 1.1. Saturación de distancias ----
### Crea dummies a partir de distancias 
dist_saturation <- function(base, variable) {
  # Extraer nombre de la variable
  var_name = base %>% select({{variable}}) %>% names()
  
  # Aplicar la saturación
  base <- base %>% 
    arrange({{variable}}) %>% 
    mutate(temporal_id = row_number()) %>% 
    mutate( 
      #ref = {{variable}},
      {{variable}} := cut({{variable}}, breaks = c(seq(0,500,50),Inf),
                        labels = 
                          c(paste0(
                            var_name,"_",
                            letters[1:11],"_",
                            seq(0,500,50),"_",
                            c(seq(50,500,50),"o_mas"))),right = FALSE)) %>% 
    mutate(dummy = 1) %>%
    pivot_wider(
      id_cols = everything(),
      names_from = {{variable}},
      values_from = dummy,
      values_fill = 0) %>% 
    select(-temporal_id)

  return(base)
  
}


pr <- data.frame(
  y = "hola",
  id = 1:100,
  x = c(0,sample(0:10000,99)))

pr <- pr %>% dist_saturation(x)

pr <- pr %>% mutate(dummy = 1) %>%
  pivot_wider(
    id_cols = id,
    names_from = x,
    values_from = dummy)


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




# Links

## Diccionario de datos
link_diccionario <- "https://docs.google.com/spreadsheets/d/1QTTEdmje0WHwJcFuhGlq5sujp-Y2ABH5mKYk5XO-XeE/edit?usp=sharing"



1-0.5

# Imposición Funcional grado 2
b1 = 2583
b2 = -26
(b1*-1)/(2*b2)








# Funciones de diseño

### Paleta 1
renovo = c("#a7e85d","#013334","#e36477","#62b7b2","#d1ce84")
### Paleta 2
renovo_scale = c("#006A68","#90CCCB","#d1ce84","#D89A7E","#e36477","#EC3434")
### Paleta 3 
renovo_base = c("#C4DCE2","#D2DAA6", "#F1F1F1")

my_theme <- 
  theme_minimal()+
  theme(text = element_text(family = "serif"))






