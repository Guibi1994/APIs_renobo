source("2_3_api_proyeccion_economica/utils.R")



a1_cooked <- readRDS("..\\data/02_cooked/m01_cooked_lotes_enrriquecidos.RDS")



# 1. Análisis univariado ----

## 1.1. Y's ----

### 1.1.1. Y1: Precios ----
a1_cooked %>% 
  sample_n(200) %>% 
  select(id01_lote, y01a_valor_2015:y01d_valor_2023) %>% 
  pivot_longer(cols = y01a_valor_2015:y01d_valor_2023) %>% 
  mutate(name = str_extract(name, "v(.*)") %>% 
           str_replace_all(.,"\\_"," ")) %>% 
  # Plot
  ggplot(aes(value, name))+
  # coord_cartesian(xlim = c(0,6000000))+
  geom_boxplot(outlier.shape = NULL, color = renovo[2])+
  stat_summary(fun.y = mean, geom = "point",
               shape = 20, size = 6, color = renovo[1]) +
  scale_x_continuous(labels = scales::dollar_format())+
  my_theme +
  labs(
    title = "Precios por m2",
    subtitle = "Bogotá 2015-20123",
    caption = "Fuente: RENOBO",
    x = "Precios por m2 (COP)", y = "Año")

### 1.1.2. Y2: Variación de precios ----
a1_cooked %>%
  #sample_n(100) %>% 
  select(id01_lote,y02a_variacion_2a:y02c_variacion_8a) %>% 
  pivot_longer(cols = y02a_variacion_2a:y02c_variacion_8a,
               names_to = "variable") %>% 
  mutate(variable = case_when(
    str_detect(variable,"_2a")==T~"Últimos 2 años\n(2023-2021)\n",
    str_detect(variable,"_5a")==T~"Últimos 5 años\n(2023-2018)\n",
    T~"Últimos 8 años\n(2023-2015)\n")) %>% 
  #PLOT
  ggplot(aes(value))+
  geom_histogram(aes(fill = variable),alpha =.6)+
  geom_vline(
    xintercept = (a1_cooked %>% 
                    select(id01_lote,y02a_variacion_2a:y02c_variacion_8a) %>% 
                    pivot_longer(cols = y02a_variacion_2a:y02c_variacion_8a, names_to = "variable") %>% 
                    group_by(variable) %>% 
                    summarise(value = mean(value, na.rm = T)) %>% 
                    pull(value)),
    lty =2, 
    color = renovo[1:3])+
  scale_fill_manual(values = renovo[1:3])+
  scale_x_continuous(labels = scales::dollar_format())+
  scale_y_continuous(labels = scales::comma)+
  my_theme+
  labs(
    title = "Variaciones de precios",
    subtitle = "Bogotá 2015-20123",
    caption = "Fuente: RENOBO",
    x = "Variación (COP)", y = "", fill = "Y2': Variaciones")


## 1.2. X's ----

### 1.2.1. Intrinsecas ----

a1_cooked %>% 
  select(id01_lote, starts_with("in")) %>% 
  pivot_longer(cols = starts_with("in")) %>%
  mutate(name = str_remove_all(name, "in\\d{2}") %>% 
           str_replace(.,"_uso",". uso") %>% 
           str_remove(.,"^_") %>% 
           str_replace_all(., "_"," ")) %>% 
  # PLOT
  ggplot(aes(value))+
    geom_density(fill = renovo[1], color = renovo[2], alpha = .6)+
    facet_wrap(.~name,scales = "free")+
  scale_x_log10()+
  my_theme+
  labs(
    title = "Distribución de variables intrínsecas",
    subtitle = "Lotes Bogotá 2023",
    caption = "Fuente: RENOBO",
    x = "Variación (log10)", y = "Densidad")

  
### 1.2.2. Normativas ----




### 1.2.3. Hedónincas ----

a1_cooked %>% 
  select(id01_lote, eq01_educacion:tr02_sitp) %>%
  pivot_longer(cols = eq01_educacion:tr02_sitp) %>% 
  mutate(grupo = substr(name,1,2),
         grupo = case_when(
           grupo == "eq"~"Equipamientos",
           grupo =="ep"~"Espacio público",
           T~"Transporte"),
         name = substr(name, 6,50),
         name = paste0("Distancia a ",name)) %>%
  # PLOT
  ggplot(aes(value, reorder(name,value, mean, na.rm = T), fill = grupo))+
  coord_cartesian(xlim = c(5,5000))+
  scale_fill_manual(values = renovo[1:3])+
  geom_violin(alpha = .6)+
  stat_summary(fun.y = mean, geom = "point",
               shape = 20, size = 4, color = "white") +
  scale_x_log10(breaks = c(10, 50, 100, 250, 500, 1000, 2500, 5000), 
                labels = c(10, 50, 100, 250, 500, 1000, 2500, 5000)) +
  my_theme+
  labs(
    title = "Distribución de variables hedónicas",
    subtitle = "Lotes Bogotá 2023",
    caption = "Fuente: RENOBO",
    x = "Distancia en metros", y = "Amanity")

