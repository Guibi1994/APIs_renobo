source("2_3_api_proyeccion_economica/utils.R")



a1_cooked <- readRDS("..\\0_raw_data/2_rds_geo_data/m01_cooked_lotes_enrriquecidos.RDS")
a2_tasty <- readRDS("..\\0_raw_data/2_rds_geo_data/m01_cooked_lotes_enrriquecidos.RDS")



# 1. Análisis univariado

## 1.2. Distribución de variaciones
a1_cooked %>%
  #sample_n() %>% 
  select(id01_lote,y02a_variacion_2a:y02c_variacion_8a) %>% 
  pivot_longer(cols = y02a_variacion_2a:y02c_variacion_8a,
               names_to = "variable") %>% 
  mutate(variable = case_when(
    str_detect(variable,"_2a")==T~"Últimos 2 años\n(2023-2021)\n",
    str_detect(variable,"_5a")==T~"Últimos 5 años\n(2023-2018)\n",
    T~"Últimos 8 años\n(2023-2015)\n")) %>% 
  #PLOT
  ggplot(aes(value))+
  geom_histogram(aes(fill = variable),alpha =.8)+
  geom_vline(
    xintercept = (a1_cooked %>% 
                    select(id01_lote,y02a_variacion_2a:y02c_variacion_8a) %>% 
                    pivot_longer(cols = y02a_variacion_2a:y02c_variacion_8a, names_to = "variable") %>% 
                    group_by(variable) %>% 
                    summarise(value = mean(value, na.rm = T)) %>% 
                    pull(value)),
    lty =2, 
    color = renovo[1:3])+
  geom_vline(xintercep %>% %>% %>% 