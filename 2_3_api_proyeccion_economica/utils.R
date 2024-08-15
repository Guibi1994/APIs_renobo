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
