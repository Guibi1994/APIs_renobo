source("2_3_api_proyeccion_economica/utils.R")


# 1. Cargar información ----

## 1.1 Base casta ----
a0_raw_catastro <- read.csv("..\\data/01_raw/mp01_matriz_por_lotes.csv")
b0_raw_ccb <- read.csv("..\\data/01_raw/mc01_thesellas_1ha_ccb.csv", sep = ";")
z0_var_dictionary <- read_sheet(link_diccionario)



# 2. Curar información ccb
b1_booked_ccb <- b0_raw_ccb %>% 
  select(id00a_t01ha = GRID_ID, 
         ec01_den_comercio = K_Comercio,
         ec02_den_servicios = K_Servicio,
         ec03_den_industria = K_Industria)




# 3. Base final ----
a1_cooked_catastro <- a0_raw_catastro %>% 
  mutate(or02_cod_area_actividad = ifelse(
    or02_cod_area_actividad ==" ","Sin AA",or02_cod_area_actividad)) %>% 
  rename(in00a_longitude = X_y,
         in00b_latitude = Y_y) %>%  # PENDIENTE: Corregir nombres y hay 2 longs 2 lats
  #sample_n(1000) %>%
  ## 3.1. Agregar información ----

  ### 3.1.1. Información CCB -----
  merge(b1_booked_ccb, by = "id00a_t01ha") %>% 
  ### 3.1.2. Variaciones de precios
  rowwise() %>% 
  mutate(
    ### 2.2.1. Variación neta de pecios ----
    # 2 años
    y02a_variacion_2a = y01d_valor_2023-y01c_valor_2021,
    # 5 años
    y02b_variacion_5a = y01d_valor_2023-y01b_valor_2018,
    # 8 años
    y02c_variacion_8a = y01d_valor_2023-y01a_valor_2015) %>% 
  ungroup() %>%
  
  ## 3.2. Seleccionar y reodenar variables ----
  select(all_of(z0_var_dictionary$variabe_name[z0_var_dictionary$variabe_name %in% names(.[])])) %>% 
  
  ## 3.3. Eliminar ouliers ----
  quantilic_filter(y01d_valor_2023) %>% 
  quantilic_filter(y01c_valor_2021) %>% 
  quantilic_filter(y01b_valor_2018) %>% 
  quantilic_filter(y01a_valor_2015) %>% 
  quantilic_filter(in04_m2_construidos) %>% 
  
  ## 3.4. Coercion Paila de formatos  (ESTO NO TIENE POR QUE PASAR )
  mutate(across(c(28:40,43,45:47), ~as.numeric(str_replace(.,",",".")))) %>%
  
  # (*) REPORTE DE COMPLETITUD  
  # a1_cooked_catastro %>% summarise(across(everything(),~mean(!is.na(.)))) %>% 
  #   reshape2::melt() %>% 
  #   googlesheets4::write_sheet(
  #     ss = link_diccionario,
  #     sheet = "completitud")
  select(-or04_altura_max,-or03_area_actividad) %>% # (Eliminada por completitud) 
  ## 3.4. Eliminar observaciones en missing ----
  filter(if_all(1:ncol(.[]),~!is.na(.))) %>% 
  
  ## 3.5. Coerción regular
  mutate(
    ### 2.6.1. Cambiar a factor o numeric ----
    across(where(is.character),as.factor),
    across(where(is.integer),as.numeric))

  


## 2.7. Exportar la base ----
saveRDS(a1_cooked_catastro, "..\\data/02_cooked/m01_cooked_lotes_enrriquecidos.RDS")
  


# 3. Tasty Base lotes ----
a2_tasty_catastro <- a1_cooked_catastro %>% 
  ## Pre-Saturaciones
  select(
    # Remover generales sin sentido
    -eq01a_educacion_general,
    # Remover saturados sin sentido
    -eq05b_seguridad_cais, -eq05c_seguridad_estaciones) %>% 
  ## Saturaciones
  dist_saturation(eq01b_educacion_colegios) #%>% 
  dist_saturation(eq01c_educacion_universidades) %>% 
  dist_saturation(eq02a_salud_general) %>% 
  dist_saturation(eq05a_seguridad_general) %>% 
  dist_saturation(ep01a_parques_general) %>% 
  dist_saturation(ep02_plazas) %>% 
  dist_saturation(tr01_transmilenio) %>% 
  dist_saturation(tr02_sitp)


  
  names(a2_tasty_catastro)[28:38]


names(a1_cooked_catastro)


saveRDS(a2_tasty, "..\\0_raw_data/2_rds_geo_data/m01_tasty_lotes_enrriquecidos.RDS")





a1_cooked %>% 
  sample_n(1000) %>% 
  ggplot(aes(y = y02c_variacion_8a, x = in03b_uso_comercial))+
  geom_point()+
  geom_smooth()+
  my_theme









# Loop de modelación general
for (i in 1:nrow(models)) {
  set.seed(1994)
  message("modelo #",i)
  message(paste(models$tipo[i],models$y[i], sep = " "))
  print("Modelo corrido")
}



a1_cooked %>%
  sample_n(10) %>% 
  select(starts_with("in")) %>% 
  mutate(
    areta_tot = rowSums(select(.,in03h_uso_otro:in03a_uso_residencial), na.rm = T),
    across(in03h_uso_otro:in03a_uso_residencial,~./areta_tot))
  





a1_cooked %>%
  ggplot(aes(y02c_variacion_8a))+
  geom_histogram(alpha =.8,fill = "cyan4")+
  geom_vline(xintercept = mean(a1_cooked$y02c_variacion_8a), 
             lty =2, color = "red")+
  geom_vline(xintercept = 0)+
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 8),
    labels = scales::dollar_format())+
  theme_minimal()+
  theme(text = element_text(family = "serif"))+
  labs(title = "Variaciones de precios en los últimos 8 años",
       subtitle = "Bogotá 2015-2023",
       caption = "Fuente: RENOBO - con base en Catastro 2023",
       x = "COP", y = "")


