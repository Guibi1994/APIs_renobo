





# 0. Preparar entorno

## 0.1. 
source("2_3_api_proyeccion_economica/utils.R")

## 0.2. Datos
a2_tasty <- readRDS("..\\0_raw_data/2_rds_geo_data/m01_cooked_lotes_enrriquecidos.RDS")


## 0.3. Modelos
models <- data.frame(
  tipo = c(
    rep("pull",7),
    rep("fixed effect",7)),
  y = rep(names(a2_tasty)[9:15],2),
  x = c(
    rep(paste(names(a2_tasty)[16:ncol(a2_tasty)], collapse = "+"),7),
    rep(paste(names(a2_tasty)[c(8,16:ncol(a2_tasty))], collapse = "+"),7))) %>% 
  mutate(
    equation = paste0(y,"~",x),
    modelo = paste0("m",str_pad(row_number(),width = 2,pad = "0"))) %>% 
  select(modelo, everything())

m1 <- lm(
  as.formula(models$equation[1]),
  a2_tasty)





r01_city_receptor <- 
  datafa
  



# 1. Regresión nivel ciudad
for (i in 1:nrow(models)) {
  set.seed(1994)
  message("modelo #",i)
  message(paste(models$tipo[i],models$y[i], sep = " "))
  print("Modelo corrido")
  
  
  
}



# 2. Regresión a nivel área de estudio


# 3. Regresión sobre supuestos de intervención

