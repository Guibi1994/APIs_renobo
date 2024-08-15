





# 0. Preparar entorno ----

## 0.1. Funciones y librerias generales ----
source("2_3_api_proyeccion_economica/utils.R")

## 0.2. Cargar Datos ----
a2_tasty <- readRDS("..\\data/03_tasty/m01_tasty_lotes_enrriquecidos.RDS")



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
    modelo = row_number()) %>% 
  select(modelo, everything())



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
  for (u in 1:length(unique(a2_tasty$id04_upl))) {
    tryCatch({
      
      ### a. Coeficientes ----
      message("3. Iniciando modelo específico")
      set.seed(1994)
      base <- tidy(
        lm(as.formula(models$equation[i]),
           a2_tasty %>% filter(id04_upl == unique(id04_upl)[u])), 
        conf.int = T) %>% 
        select(term,estimate,std.error,p.value,conf.low,conf.high) %>% 
        mutate(
          grupo = "upl",
          subgrupo = unique(a2_tasty$id04_upl)[u],
          modelo = models$modelo[i],
          y = models$y[i], 
          tipo = models$tipo[i])
      
      r01_effects <- rbind(r01_effects, base)
      message("3. Modelo específico corrido")
      
      ## b. Ajustes ----
      message("4. Iniciando analisis de ajuste")
      set.seed(1994)
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




 # 3. Panel wide ----

r03_wide_panel <- 
  r02_adjustment %>% 
  filter(grupo == "upl", 
         modelo >= 8) %>% 
  select(subgrupo, y,adj.r.squared) %>% 
  pivot_wider(id_cols = subgrupo,
              names_from = y, 
              values_from = adj.r.squared) %>% 
  rename_with(.cols = 2:ncol(.[]),~paste0("r2a_",.)) %>% 
  
  merge(
  r01_effects %>% 
  filter(grupo == "upl",
    modelo == 14) %>% 
  filter(
    str_detect(term, "^(eq|^ep|^tr)")) %>%
  mutate(estimate = ifelse(p.value >0.05,NA,estimate)) %>% 
  select(subgrupo, term,estimate) %>% 
  pivot_wider(id_cols = subgrupo,
              names_from = term, 
              values_from = estimate), 
  # rename_with(.cols = 2:ncol(.[]),~paste0("r2a_",.)),
  by = "subgrupo", all = T)










googlesheets4::write_sheet(
  data = r01_effects,
  ss = "https://docs.google.com/spreadsheets/d/1lUPDioJ_-WOtZuXLggjWiJsrAymzpBkA45sfiqTsFb0/edit?usp=sharing",
  sheet = 1)
googlesheets4::write_sheet(
  data = r02_adjustment,
  ss = "https://docs.google.com/spreadsheets/d/1lUPDioJ_-WOtZuXLggjWiJsrAymzpBkA45sfiqTsFb0/edit?usp=sharing",
  sheet = 2)

googlesheets4::write_sheet(
  data = r03_wide_panel,
  ss = "https://docs.google.com/spreadsheets/d/1lUPDioJ_-WOtZuXLggjWiJsrAymzpBkA45sfiqTsFb0/edit?usp=sharing",
  sheet = 3)


summary(test_model)

# 2. Regresión a nivel área de estudio


# 3. Regresión sobre supuestos de intervención

