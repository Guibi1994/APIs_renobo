source("2_1_api_diagnosticos_urbanos/utils.R")
source("2_3_api_proyeccion_economica/utils.R")



# Cargar base del DANE 
d1_personas <- read.csv("..\\data/01_raw/mp04_censo_dane_personas.CSV")
d2_hogares <- read.csv("..\\data/01_raw/mp03_censo_dane_geografico.CSV")
d3_viviendas <- read.csv("..\\data/01_raw/mp02_censo_dane_viviendas.CSV")
d4_fallecidos <- read.csv("..\\data/01_raw/mp05_censo_dane_fallecidos.CSV")
d5_geo <- read.csv("..\\data/01_raw/mp03_censo_dane_geografico.CSV")


a0_cooked <- readRDS("..\\data/02_cooked/m01_cooked_lotes_enrriquecidos.RDS")



length(unique(d5_geo$COD_DANE_ANM))
 


a0_cooked %>% 
  ggplot(aes(y01d_valor_2023))+
  geom_density(
    fill = renovo[1], alpha = .8, color = renovo[2])+
  geom
  my_theme
