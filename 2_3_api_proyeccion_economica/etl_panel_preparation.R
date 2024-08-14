source("2_3_api_proyeccion_economica/utils.R")


# 1. Cargar información ----

## 1.1 Información Castastral ----
a0_raw <- read.csv("..\\0_raw_data/5_bases_tabulares/Lotes_final.csv")
a0_raw_2 <- read.csv("..\\0_raw_data/5_bases_tabulares/Lotes_distancia_ver2.csv", sep = ";") %>% 
  select(LotCodigo,starts_with("SU"),lon = X,lat = Y)
  

a0_raw_2 %>% 
  select(!starts_with("SU")) %>% 
  head()





b0_raw_2021 <- readRDS("..\\0_raw_data/5_bases_tabulares/20230620EM2021.rds")


# 2. Cooked base lotes ----
a1_cooked <- a0_raw %>% 
  # Remplazar distancias
  select(!starts_with("SU")) %>% 
  merge(a0_raw_2, by = "LotCodigo", all.x = T) %>% 
  
  
  ## 2.1. Edición de variables ----
  mutate(
    ### 2.1.1. Formato de distancias y coordenadas ----
    across(starts_with("SU"),~as.numeric(str_replace(.,",","."))),
    across(lon:lat,~as.numeric(str_replace(.,",","."))),
     
    ### 2.1.2. Cambio de alturas ----
    Altura_Max = as.numeric(Altura_Max),
    Area = as.numeric((str_replace(Area,",",".")))) %>%
  
  ## 2.2. Creación de variables ----
  rowwise() %>% 
  mutate(
    ### 2.2.1. Variación neta de pecios ----
    # 2 años
    y02a_variacion_2a = VR_2023-VR_2021,
    # 5 años
    y02b_variacion_5a =VR_2023-VR_2018,
    # 8 años
    y02c_variacion_8a =VR_2023-VR_2015) %>% 
  ungroup() %>% as.data.frame() %>% 
  
  ## 2.3. Selección de varibles ----
  select(
  ### 2.3.1. Variables de identificación ----
    id01_lote = LotCodigo,
    id02_manzana = ManzCodigo,
    id03_localidad = Localidad,
    id04_upl = UPL,
    id05a_upz = Cod_UPZ,
    id05b_upz_name = UPZ,
    id06a_upz_em = Cod_UPZ_EM,
    id06b_upz_em_name = UPZ_EM,
  ### 2.3.2. Variables dependientes ----
  # Años individuales
  y01a_valor_2015 = VR_2015,
  y01b_valor_2018 = VR_2018,
  y01c_valor_2021 = VR_2021, 
  y01d_valor_2023 = VR_2023,
  # Variaciones netas
  y02a_variacion_2a,
  y02b_variacion_5a,
  y02c_variacion_8a,
  
  ### 2.3.3. Independientes (in: Intrínsecas) ----
  in01_altura = Altura_Max,
  in02_area = Area,
  in03a_uso_residencial = RESIDENCIAL, # Modelado
  in03b_uso_comercial = COMERCIAL, # Modelado
  in03c_uso_industrial = INDUSTRIAL,
  in03d_uso_dotacional = DOTACIONAL,
  in03e_uso_logistico = LOGISTICO,
  in03f_uso_parqueadero = PARQUEADERO,
  #in03g_uso_libre = LIBRE, # Es siempre CERO, revisar
  in03h_uso_otro = OTRO,
  
  ### 2.3.4. Independientes (hedónicas) ----
  # Equipamietnos (eq)
  eq01_educacion = SU_Eq_Edu1, # OJO !!! Falta Saturación por tipo
  eq02_salud = SU_Eq_Salu1,
  eq03_cultura = SU_Eq_Cul1,
  eq04_recreacion = SU_Eq_Rec1,
  # Espacio público (ep)
  ep01_parques = SU_EP_Parq1, # OJO !!! falta saturar por tamaño             
  ep02_plazas = SU_EP_Plaz1,
  
  # Transporte (tr)
  tr01_transmilenio = SU_Tr_TM1,
  tr02_sitp = SU_Tr_SITP1,
  
  # Ordenamiento (or)
  or01_tratamietno = Tratamient,
  or02_area_actividad = Area_Activida,
  or03_altura_max =Altura_Max) %>% 
  
  ## 2.4. Eliminación de NA ----
  filter(if_all(y01a_valor_2015:or03_altura_max,~!is.na(.))) %>% 
  ## 2.5. Remoción de outliers ----
  filter(
    between(y01d_valor_2023, 
            quantile(y01d_valor_2023,probs = .025),
            quantile(y01d_valor_2023,probs = .975)) &
      between(y01c_valor_2021, 
              quantile(y01c_valor_2021,probs = .025),
              quantile(y01c_valor_2021,probs = .975)) &
      between(y01b_valor_2018, 
              quantile(y01b_valor_2018,probs = .025),
              quantile(y01b_valor_2018,probs = .975)) &
      between(y01a_valor_2015, 
              quantile(y01a_valor_2015,probs = .025),
              quantile(y01a_valor_2015,probs = .975)) &
      # por área
      between(in02_area, 
              quantile(in02_area,probs = .025),
              quantile(in02_area,probs = .975))) %>% 
  ## 2.6. Ediciones finales complementarias ----
  mutate(
    ### 2.6.1. Cambiar a factor o numeric ----
    across(where(is.character),as.factor),
    across(where(is.integer),as.numeric),
    ### 2.6.2. Áreas construidas por %
    area_tot = rowSums(select(.,in03a_uso_residencial:in03h_uso_otro), na.rm = T),
    across(in03a_uso_residencial:in03h_uso_otro,~./area_tot)) %>% 
  select(-area_tot)

## 2.7. Exportar la base ----
saveRDS(a1_cooked, "..\\0_raw_data/2_rds_geo_data/m01_cooked_lotes_enrriquecidos.RDS")
  


# 3. Tasty Base lotes ----
a2_tasty <- a1_cooked %>% 
  mutate(
    # Loglinearización de precios
    across(y01a_valor_2015:y02c_variacion_8a,~log(.+1)),
    # Estadarización de variaciones e independientes
    across(y02a_variacion_2a:tr02_sitp,~scale(.))) %>% 
  # Re-eliminar todos los NA
  filter(if_all(y01a_valor_2015:or03_altura_max,~!is.na(.))) 
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


