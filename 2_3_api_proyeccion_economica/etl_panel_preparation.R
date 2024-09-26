source("2_3_api_proyeccion_economica/utils.R")


# 1. Cargar información ----

## 1.1 Base casta ----
a0_raw_catastro <- read.csv("..\\data/01_raw/mp01_matriz_por_lotes.csv")

## 1.2. Bases complementarias ----

### 1.2.1. Theselas y densidades comerciales ----
b0_raw_ccb <- read.csv("..\\data/01_raw/mc01_thesellas_1ha_ccb.csv", sep = ";")

### 1.2.2. Saturaiones cultura y espacio público ----
c0_culture_ps_satuararion <- read.csv("..\\data/01_raw/mc02_culture_pubic_sacpe_saturations.csv") # Errores graves de completitud

##3 1.2.3. Acutaciones Estratégicas (AE) ----
d0_ae_vs_lots <- read.csv("..\\data/01_raw/mc03_ae_and_lots.csv") %>% 
  mutate(id07a_ae = ifelse(is.na(id07a_ae), "999",id07a_ae))


z0_var_dictionary <- read_sheet(link_diccionario)



# 2. Curar información ccb ----
b1_booked_ccb <- b0_raw_ccb %>% 
  select(id00a_t01ha = GRID_ID, 
         ec01_den_comercio = K_Comercio,
         ec02_den_servicios = K_Servicio,
         ec03_den_industria = K_Industria) %>% 
  mutate(across(ec01_den_comercio:ec03_den_industria, 
                ~convert_comma_numbers(.)))




# 3. Base final ----
a1_cooked_catastro <- a0_raw_catastro %>% 
  ## 3.0. Forzar remociónd de duplicados
  distinct(id01a_lote, .keep_all = T) %>% 
  
  ## 3.1. Arreglos menores
  mutate(or02_cod_area_actividad = ifelse(
    or02_cod_area_actividad ==" ","Sin AA",or02_cod_area_actividad)) %>% 
  rename(in00a_longitude = X_y,
         in00b_latitude = Y_y) %>%  
  
  
  ## 3.1. Coherención #1 ----
  mutate(across(eq01a_educacion_general:ep02_plazas,~convert_comma_numbers(.))) %>% 
  ## 3.1. Agregar información ----

### 3.1.1. Información CCB -----
merge(b1_booked_ccb, by = "id00a_t01ha", all.x = T) %>% 
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
  
  ### 3.1.2. Actuaciones estratégicas
  merge(d0_ae_vs_lots, by = "id01a_lote", all.x = T) %>% 
  
  
  ## 3.2. Seleccionar y reodenar variables ----
select(all_of(z0_var_dictionary$variabe_name[z0_var_dictionary$variabe_name %in% names(.[])])) %>% 
  
  ## 3.3. Eliminar ouliers ----
quantilic_filter(y01d_valor_2023) %>% 
  quantilic_filter(y01c_valor_2021) %>% 
  quantilic_filter(y01b_valor_2018) %>% 
  quantilic_filter(y01a_valor_2015) %>% 
  quantilic_filter(in04_m2_construidos) %>% 
  
  ## 3.4. Coercion Paila de formatos  (ESTO NO TIENE POR QUE PASAR )
  #mutate(across(c(28:40,43,45:47), ~as.numeric(str_replace(.,",",".")))) %>%
  
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


a1_cooked_catastro %>% summarise(across(everything(),~mean(!is.na(.)))) %>%
    reshape2::melt() %>%
    googlesheets4::write_sheet(
      ss = link_diccionario,
      sheet = "completitud_posterior")





## 2.7. Exportar la base ----
saveRDS(a1_cooked_catastro, "..\\data/02_cooked/m01_cooked_lotes_enrriquecidos.RDS")
a1_cooked_catastro <- readRDS("..\\data/02_cooked/m01_cooked_lotes_enrriquecidos.RDS")



# 3. Tasty Base lotes ---- %>% %>% %>% 
a2_tasty_catastro <- a1_cooked_catastro %>% 
  ## Pre-Saturaciones
  select(
    # Remover generales sin sentido
    -eq01a_educacion_general,
    # Remover saturados sin sentido
    -eq05b_seguridad_cais, -eq05c_seguridad_estaciones) %>% 
  ## Saturaciones
  ### Por distancias
  dist_saturation(eq01b_educacion_colegios) %>% 
  dist_saturation(eq01c_educacion_universidades) %>% 
  dist_saturation(eq02a_salud_general) %>% 
  dist_saturation(eq03_cultura) %>% 
  dist_saturation(eq05a_seguridad_general) %>% 
  dist_saturation(ep01a_parques_general) %>% 
  dist_saturation(ep02_plazas) %>% 
  dist_saturation(tr01_transmilenio) %>% 
  dist_saturation(tr02_sitp) %>% 
  ### Por cuantiles
  quantile_saturation(ec01_den_comercio) %>% 
  quantile_saturation(ec02_den_servicios) %>% 
  quantile_saturation(ec03_den_industria)

saveRDS(a2_tasty_catastro, "..\\data/03_tasty/m01_tasty_lotes_enrriquecidos.RDS")











# Curaciones complementarias ----

# cc1. Lotes VS AE ----

# pr <- a0_raw_catastro %>%
#   select(
#     id01a_lote,
#     lon = X_x, lat = Y_x) %>% 
#   mutate(across(lon:lat, ~as.numeric(str_replace(.,",",".")))) %>% 
#   st_as_sf(coords = c("lon","lat")) %>% 
#   st_set_crs(4326) %>% 
#   st_transform(4326) %>% 
#   st_make_valid()
# 
# 
# g01_ae <- read_sf("..\\data/00_geo_data/1_raw_geo_data/Actuaciones_estrategicas_12042024.shp") %>% 
#   select(id07a_ae= CODIGO_ID, id07b_ae_name = NOMBRE) %>% 
#   mutate(id07a_ae = ifelse(is.na(id07a_ae),"ind",id07a_ae)) %>% 
#   st_make_valid() %>% 
#   st_transform(4326) 
# 
# pr1 <- st_join(pr, g01_ae, join = st_within)
# 
# 
# pr2 <- pr1 %>% 
#   mutate(id07b_ae_name = ifelse(is.na(id07b_ae_name),"Resto de la ciudad",id07b_ae_name)) %>% 
#   as.data.frame() %>% 
#   select(-geometry) 
# 
# write.csv(pr2, "..\\data/01_raw/mc03_ae_and_lots.csv", row.names = F)




