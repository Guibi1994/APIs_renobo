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


## base fake

### Paleta 1
renovo = c("#a7e85d","#013334","#e36477","#62b7b2","#d1ce84")
### Paleta 2
renovo_scale = c("#006A68","#90CCCB","#d1ce84","#D89A7E","#e36477","#EC3434")
### Paleta 3 
renovo_base = c("#C4DCE2","#D2DAA6", "#F1F1F1")

my_theme <- 
  theme_minimal()+
  theme(text = element_text(family = "serif"))


# Funcion de saturación espacial
# Crea dummys en fucion de rangos

dist_saturation <- function(base, variable) {
  # Extraer nombre de la variable
  var_name = base %>% select({{variable}}) %>% names()
  
  # Aplicar la saturación
  base <- base %>% 
    arrange({{variable}}) %>% 
    mutate(
      test = {{variable}},
      !!var_name := cut({{variable}}, breaks = c(seq(0,500,50),Inf),
                        labels = 
                          c(paste0(
                            var_name,"_",
                            letters[1:11],"_",
                            seq(0,500,50),"_",
                            c(seq(50,500,50),"o_mas"))))) %>% 
    mutate(dummy = 1) %>% 
    pivot_wider(names_from = {{variable}},
                values_from = dummy,
                values_fill = 0)
  
  return(base)
  
}


# Imposición Funcional grado 2
b1 = 2583
b2 = -26
(b1*-1)/(2*b2)
