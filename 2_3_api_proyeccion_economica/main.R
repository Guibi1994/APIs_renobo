
# 0. Preparar entorno ----

## 0.1. Funciones y librerias generales ----
source("2_3_api_proyeccion_economica/utils.R")

## 0.2. Cargar Datos ----
a2_tasty <- readRDS("..\\data/03_tasty/m01_tasty_lotes_enrriquecidos.RDS")



# 1. Especificar modelos ----


fixed_effect = a2_tasty %>% select(id04b_upl_name) %>% names()
fixed_effect

models <- data.frame(
  ## 1.1. Modelos de variacion de precios
  y = a2_tasty %>% select(
    y02a_variacion_2a:y02c_variacion_8a) %>% 
    names()) %>% 
  mutate(
    x = a2_tasty %>% select(
      in03a_uso_residencial:ncol(a2_tasty)) %>% 
      names() %>% 
      paste(collapse = "+"),
    tipo = "1. Variacion de precios catastrales") %>% 
  ## 1.2. Modelos de densidad comercial
  # rbind(
  #   data.frame(
  #     y = a2_tasty %>% select(ec01_den_comercio:ec03_den_industria) %>% 
  #       names()) %>%
  #     mutate(
  #       x = a2_tasty %>% select(
  #         y02a_variacion_2a:y02c_variacion_8a,
  #         in03a_uso_residencial:ncol(a2_tasty)) %>% 
  #         select(-ec01_den_comercio,-ec02_den_servicios, -ec03_den_industria) %>% 
  #         names() %>% paste(collapse = "+"),
  #       tipo = "2. Densidades comerciales CCB")) %>% 
  ### Efectos fijos
  mutate(
    equation = paste0(y,"~",fixed_effect,"+",x),
    modelo = row_number(),
    tipo = as.factor(tipo)) %>% 
  select(modelo,tipo, everything())






# 2. Loop de regresiones locales ----

## 2.1. Bases respuesta ----

### 2.1.1. Efectos ----
r01_effects <- 
  data.frame(
    # Identificación
    grupo = character(),
    subgrupo = character(),
    modelo = numeric(),
    y = character(),
    tipo = character(),
    # Regresion
    term = character(),
    estimate = numeric(),
    std.error = numeric(),
    p.value = numeric(),
    conf.low = numeric(),
    conf.high = numeric()
  )

### 2.1.2. Ajustes ----
r02_adjustment <-  data.frame(
  # Identificación
  grupo = character(),
  subgrupo = character(),
  modelo = numeric(),
  y = character(),
  tipo = character(),
  # Results
  r2 = numeric(),
  r2_adjusted = numeric(),
  d_freedom = numeric())



## 2.2. Loop Algórtimo regresivo ----
for (i in 1:nrow(models)) {
  set.seed(1994)
  message("modelo #",i)
  message(paste(models$tipo[i],models$y[i], sep = " "))
  
  ### 2.2.1. Resultados de ciudad ----
  #### a. Coeficcientes ----
  set.seed(1994)
  test_model <- lm(as.formula(models$equation[i]), a2_tasty)
  base <- tidy(test_model, conf.int = T) %>% 
    select(term,estimate,std.error,p.value,conf.low,conf.high) %>% 
    mutate(
      grupo = "ciudad",
      subgrupo = "Bogotá",
      modelo = models$modelo[i],
      y = models$y[i], tipo = models$tipo[i])
  
  r01_effects <- rbind(r01_effects, base)
  message("1. Modelo general corrido")
  #### b. Ajustes ----
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
  
  
  ## 2.2.2. Resultados de UPL ----
  for (u in 1:length(unique(a2_tasty$id04b_upl_name))) {
    tryCatch({
      
      ### a. Coeficientes ----
      message("3. Iniciando modelo específico")
      set.seed(1994)
      base <- tidy(
        lm(as.formula(str_remove(models$equation[i],paste0(fixed_effect,"+"))),
           a2_tasty %>% filter(id04b_upl_name == unique(id04b_upl_name)[u])), 
        conf.int = T) %>% 
        select(term,estimate,std.error,p.value,conf.low,conf.high) %>% 
        mutate(
          grupo = "upl",
          subgrupo = unique(a2_tasty$id04b_upl_name)[u],
          modelo = models$modelo[i],
          y = models$y[i], 
          tipo = models$tipo[i])
      
      r01_effects <- rbind(r01_effects, base)
      message("3. Modelo específico corrido")
      
      ## b. Ajustes ----
      message("4. Iniciando analisis de ajuste")
      set.seed(1994)
      test_model <- lm(
        as.formula(str_remove(models$equation[i],paste0(fixed_effect,"+"))),
        a2_tasty %>% filter(id04b_upl_name == unique(id04b_upl_name)[u]))
      
      base <- data.frame(
        # Identificación
        grupo = "upl",
        subgrupo = unique(a2_tasty$id04b_upl_name)[u],
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

# 3. Agregar etiquetas de coeficientes
r01_effects <- r01_effects %>% 
  signicance_labels()
  
# 4. Agregar muestra efectiva
r02_adjustment <- r02_adjustment %>% 
  merge(
    (a2_tasty %>% 
    group_by(id04a_upl, subgrupo = id04b_upl_name) %>% 
      summarise(n =n()) %>% 
      as.data.frame()),
    by = "subgrupo", all.x = T) %>% 
  mutate(
    n = ifelse(grupo == "ciudad", nrow(a2_tasty),n),
    muestra = df.residual/n) %>% 
  select(id04a_upl, everything())

# 4. Guardar y exportar resultados

## 4.1. Almacenamiento local

saveRDS(r01_effects, "..\\data/03_tasty/r01_regression_cofficients.RDS")
saveRDS(r02_adjustment, "..\\data/03_tasty/r02_regression_adjustments.RDS")


googlesheets4::write_sheet(
  data = r01_effects,
  ss = "https://docs.google.com/spreadsheets/d/1lUPDioJ_-WOtZuXLggjWiJsrAymzpBkA45sfiqTsFb0/edit?usp=sharing",
  sheet = 1)
googlesheets4::write_sheet(
  data = r02_adjustment,
  ss = "https://docs.google.com/spreadsheets/d/1lUPDioJ_-WOtZuXLggjWiJsrAymzpBkA45sfiqTsFb0/edit?usp=sharing",
  sheet = 2)





