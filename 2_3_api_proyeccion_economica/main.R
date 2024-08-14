





# 0. Preparar entorno ----

## 0.1. Funciones y librerias generales ----
source("2_3_api_proyeccion_economica/utils.R")

## 0.2. Cargar Datos ----
a2_tasty <- readRDS("..\\0_raw_data/2_rds_geo_data/m01_tasty_lotes_enrriquecidos.RDS")



# 1. Especificar modelos ----
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




r01_effects <- 
  data.frame(
    # Identificación
    grupo = character(),
    subgrupo = character(),
    modelo = character(),
    y = character(),
    tipo = character(),
    # Regresion
    term = character(),
    std.error = numeric(),
    p.value = numeric(),
    conf.low = numeric(),
    conf.high = numeric()
  )
  
r02_adjustment <-  data.frame(
  # Identificación
  grupo = character(),
  subgrupo = character(),
  modelo = character(),
  y = character(),
  tipo = character(),
  # Results
  r2 = numeric(),
  r2_adjusted = numeric(),
  d_freedom = numeric())



# 1. Algórtimo regresivo
for (i in 1:nrow(models)) {
  set.seed(1994)
  message("modelo #",i)
  message(paste(models$tipo[i],models$y[i], sep = " "))
  
  # Modelo general
  test_model <- lm(as.formula(models$equation[i]), a2_tasty)
  
  # Resultados de ciudad
  ## Coeficcientes
  base <- tidy(test_model, conf.int = T) %>% 
    select(term,std.error,p.value,conf.low,conf.high) %>% 
    mutate(
      grupo = "ciudad",
      subgrupo = "Bogotá",
      modelo = models$modelo[i],
      y = models$y[i], tipo = models$tipo[i])
  
  r01_effects <- rbind(r01_effects, base)
  message("1. Modelo general corrido")
  ## Ajuste
  base <- data.frame(
    # Identificación
    grupo = "ciudad",
    subgrupo = "Bogotá",
    modelo = models$modelo[i],
    y = models$y[i], 
    tipo = models$tipo[i],
    # Results
    r2 = broom::glance(test_model)[1],
    r2_adjusted = broom::glance(test_model)[2],
    d_freedom = broom::glance(test_model)[11])
  
  r02_adjustment <- rbind(r02_adjustment, base)
  message("2. Ajustes generales analizados")
  
  
  # Resultados de UPL
  for (u in 1:length(unique(a2_tasty$id04_upl))) {
    tryCatch({
      
      ## Coeficientes
      message("3. Iniciando modelo específico")
      base <- tidy(
        lm(as.formula(models$equation[i]),
           a2_tasty %>% filter(id04_upl == unique(id04_upl)[u])), 
        conf.int = T) %>% 
        select(term,std.error,p.value,conf.low,conf.high) %>% 
        mutate(
          grupo = "upl",
          subgrupo = unique(a2_tasty$id04_upl)[u],
          modelo = models$modelo[i],
          y = models$y[i], 
          tipo = models$tipo[i])
      
      #print(names(base))
      
      r01_effects <- rbind(r01_effects, base)
      message("3. Modelo específico corrido")
      
      ## Ajuste
      message("4. Iniciando analisis de ajuste")
      test_model <- lm(
        as.formula(models$equation[i]),
        a2_tasty %>% filter(id04_upl == unique(id04_upl)[u]))
      
      base <- data.frame(
        # Identificación
        grupo = "upl",
        subgrupo = unique(a2_tasty$id04_upl)[u],
        modelo = models$modelo[i],
        y = models$y[i], 
        tipo = models$tipo[i],
        # Results
        r2 = broom::glance(test_model)[1],
        r2_adjusted = broom::glance(test_model)[2],
        d_freedom = broom::glance(test_model)[11])
      
      r02_adjustment <- rbind(r02_adjustment, base)
      message("4. Ajustes específicos analizados")
      
    }, error = function(e){
      message("MODELO NO EJECUTABLE")
    })
    
    
    
  }
  
}






summary(test_model)

# 2. Regresión a nivel área de estudio


# 3. Regresión sobre supuestos de intervención

