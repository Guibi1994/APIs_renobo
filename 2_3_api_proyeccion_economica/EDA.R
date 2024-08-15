source("2_3_api_proyeccion_economica/utils.R")



a1_cooked <- readRDS("..\\data/02_cooked/m01_cooked_lotes_enrriquecidos.RDS")



# 1. Análisis univariado

## 1.2. Distribución de variaciones
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

