# Fast Study API

# Descripción: esta API realiza el anáisis de los siguientes indicadores
#   1. SISBEN (a)
#   2. IPM (b)
#   3. Estratos (c)
#   4. Densidades poblacionlaes (d)


# 0. Preparar entorno ----
## 0.1. Hiperparámetos ----
source("2_1_api_diagnosticos_urbanos/utils.R")
AE_dir = "../0_raw_data/3_ejemplo_api/ejemplo_api.kml"
AE_nombre = "Proyecto X"
folder_name = "DEMO_api"

## 0.2. Construcción de entorno ----
### 0.2.1. Crear folder ----
drive_rm(folder_name)
folder <- drive_mkdir(folder_name, overwrite = TRUE)

### 0.2.2. Crear sheet de resultados ----
sheet <- googlesheets4::gs4_create(
  name = paste0("Resultados_",AE_nombre),
  sheets = "prompts")

drive_mv(
  file = paste0("Resultados_",AE_nombre),
  path = as_id(folder$id))

### 0.2.3. Estructura de resultados ----
my_prompts <- data.frame(
  indicador = character(),
  prompt = character())

sheet_write(
  data = my_prompts,
  ss = sheet, sheet = "prompts")






# 1. Cargar datos ----

## 1.1. Bases básicas ----
### 1.1. Areas de estudio ----
# z0_area_estudio <- st_read("0_raw_data/1_geo_data/Actuaciones_estrategicas_12042024.shp") %>% 
#  filter(NOMBRE == AE) %>%
#   st_transform(crs = 4326) %>% 
#   st_set_crs(4326)

z0_area_estudio <- read_sf(AE_dir) %>% 
  st_make_valid() %>% 
    st_transform(crs = 4326) %>%
    st_set_crs(4326)
  
### 1.2. Área de análisis y mapa base ----
z1_box <- st_as_sfc(nst_bbox(z0_area_estudio, aumento_p = .1))
my_basemap <- gen_base_map(z1_box)
p = my_basemap +
  geom_sf(data = z0_area_estudio,  aes(linetype = "Study Area"), fill = NA, lty = 2, lwd = 0.5)

ggdrive_save(
  plot = p, drive_location = folder_name,
  name = paste0("area_estudio_",AE_nombre,".png"), w = 8,h = 8)



# 2. Análisis automáticos

## 2.1. SISBEN
# a0_sisben <- readRDS("../0_raw_data/2_rds_geo_data/sisben.RDS")
# a1_sisben <- st_intersection(a0_sisben,z0_area_estudio) 
# 
# p = my_basemap +
#   geom_sf(data = a1_sisben, aes(color = grupo),
#           size = 2, alpha = .6)+
#   ## 1.2. Target color
#   scale_color_manual(
#     "Grupo SISBEN",
#     values = renovo_scale[1:4])+
#   geom_sf(data = z0_area_estudio,  aes(linetype = "Study Area"), fill = NA, lty = 2, lwd = 0.5)+
#   theme(legend.position = c(0.15, 0.4),
#         legend.background = element_rect(
#           fill = rgb(1, 1, 1, alpha = 0.8), color = NA)) 
# 
# ggdrive_save(
#   plot = p, drive_location = folder_drive,
#   name = paste0("sisben_",AE_nombre,".png"), w = 8,h = 8)



# 2. IPM ----

## 2.1. Cargar información ----
a2_ipm <- readRDS("../0_raw_data/2_rds_geo_data/IPM_bogota_DANE2018.RDS") %>% 
  mutate(
    study_area = lengths(
      st_intersects(st_centroid(.[]), z0_area_estudio)) > 0,
    grupo = ifelse(study_area==T,AE_nombre,"Ciudad"))

## 2.2. Extraer información del AE ----
a3_ipm <- a2_ipm %>% 
  filter(study_area==T) %>% 
  mutate(ipm_ranks = qranks(ipm, include_cero = F))

## 2.3. Crear mapa ----
pr <- colorRampPalette(c("grey","pink","red4"))(5)
p = my_basemap +
  geom_sf(data = a3_ipm, aes(fill = ipm_ranks), color = NA)+
  scale_fill_manual(
    "IPM",
    values = pr)+
  geom_sf(data = z0_area_estudio,  aes(linetype = "Study Area"), fill = NA, lty = 2, lwd = 0.5)+
  theme(legend.position = c(0.15, 0.4),
        legend.background = element_rect(
          fill = rgb(1, 1, 1, alpha = 0.8), color = NA)) 

ggdrive_save(
  plot = p, drive_location = folder_name,
  name = paste0("ipm_map_",AE_nombre,".png"), w = 8,h = 8)

## 2.4. Crear gráfico ----
p = a2_ipm %>% 
  as.data.frame() %>% 
  ggplot(aes(ipm, grupo))+
  coord_cartesian(xlim = c(0, quantile(a2_ipm$ipm,probs = .96)))+
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun.x = mean, geom = "point",
               shape = 20, size = 4, color = renovo[3]) +
  theme_minimal()+
  theme(text = element_text(family = "serif"))

ggdrive_save(
  plot = p, drive_location = folder_name,
  name = paste0("ipm_plot_",AE_nombre,".png"), w = 6,h = 3)

### 2.5. Crear prompt ----
my_data <- a2_ipm %>% 
  as.data.frame() %>% 
  group_by(grupo) %>% 
  summarise(
    manzanas_analizadas = n(),
    ipm_avg = mean(ipm, na.rm = T),
    ipm_sd  = sd(ipm, na.rm = T),
    ipm_q75 = quantile(ipm, probs = 0.75)) %>% 
  mutate(across(ipm_avg:ipm_q75,~round(.,2)))

sheet_append(
  ss = sheet,
  sheet = "prompts",
  data = 
    data.frame(
      indicador = "IPM",
      prompt = paste0(list(
        instruccion = "Escribe un texto técnico. La primera oración del primero explicando brevemente que
  es el IPM y la importancia de tomarlo en cuenta al evaluar políticas e 
  internvenciones urbanas.En adelante segundo describe los datos, enfocado en comparar
  la pieza con la ciudad, el centro del análisis es la pieza",
        contexto = "Son datos del Indice de pobreza multidimensional, 
  representan el porcentage de personas en una manzana consideradas
  en vulnerabilidad en la ciudad de Bogotá",
        nombre = paste0("la pieza a evaluar se llama ",AE_nombre),
        rol = "Habla como persona experta en pobreza",
        voz = "tercera persona del prural",
        extension = "3 parrafos",
        datos = my_data) %>% 
          jsonlite::toJSON(pretty = F))))


  
  
  



# 3. Estratos ----

## 3.1. Crear mapa ----
a0_stratum <- readRDS("../0_raw_data/2_rds_geo_data/estratos.RDS")%>% 
  mutate(
    study_area = lengths(
      st_intersects(st_centroid(.[]), z0_area_estudio)) > 0,
    grupo = ifelse(study_area==T,AE_nombre,"Ciudad"))

a1_stratum_sa <- a0_stratum %>% 
  filter(study_area ==T)

p=my_basemap +
  geom_sf(data = a1_stratum_sa, aes(fill = ESTRATO), color = NA)+
  scale_fill_manual(
    "Estratos",
    values = setNames(a1_stratum_sa$color, a1_stratum_sa$ESTRATO))+
  geom_sf(data = z0_area_estudio,  aes(linetype = "Study Area"), fill = NA, lty = 2, lwd = 0.5)+
  theme(legend.position = c(0.15, 0.4),
        legend.background = element_rect(
          fill = rgb(1, 1, 1, alpha = 0.8), color = NA)) 
ggdrive_save(
  plot = p, drive_location = folder_name,
  name = paste0("estratos_map",AE_nombre,".png"), w = 8,h = 8)



## 3.1. Crear gráfico ----
my_data <- 
  a0_stratum %>% 
  as.data.frame() %>% 
  select(grupo, ESTRATO, SHAPE_AREA, color) %>% 
  group_by(grupo,Estrato = ESTRATO, color) %>% 
  summarise(
    manzanas = n(),
    area = sum(SHAPE_AREA, na.rm = T)) %>% 
  pivot_longer(
    cols = c("area","manzanas")) %>% 
  ## Clasica
  arrange(grupo,desc(Estrato)) %>% 
  group_by(grupo, name) %>% 
  mutate(tot = sum(value),
         pp = value/tot,
         position = (cumsum(pp)-pp)+(pp/2),
         my_text = paste0(100*round(pp,2), " %")) %>% 
  arrange(name, Estrato) %>% 
  as.data.frame()

colors_city <- setNames(my_data$color, my_data$Estrato)

p <- my_data %>% 
  ggplot(aes(pp, name, fill = Estrato))+
  geom_bar(stat = "identity")+
  geom_text(aes(position,name, label = my_text),
            angle =90,color = "white",family = "serif",
            size = 2)+
  scale_fill_manual(values = colors_city)+
  scale_x_continuous(labels = scales::percent_format())+
  theme_minimal()+
  theme(
    text = element_text(family = "serif"),
    legend.position = "bottom")+
  labs(y = "", x = "", fill = "")+
  facet_wrap(.~grupo, scales = "free",
             nrow = 2)+
  guides(fill = guide_legend(nrow = 1))
ggdrive_save(
  plot = p, drive_location = folder_name,
  name = paste0("estratos_plot",AE_nombre,".png"), w = 6,h = 4)

## 3.3. Crear prompt ----
sheet_append(
  ss = sheet,
  sheet = "prompts",
  data = 
    data.frame(
      indicador = "Estratos socioeconomicos",
      prompt = paste0(list(
        instruccion = "Escribe tres parrafos. La primera oración del primero explicando brevemente que
  son los estratos socioeconómicos en colombia y la importancia de tomarlo en cuenta al evaluar políticas e 
  internvenciones urbanas. En el segundo describe los datos, enfocado en comparar
  la pieza con la ciudad, el centro del análisis es la pieza",
        contexto = "Son datos sobre area por estrato y numero de manzanasen el contexto
  de un analisis de la pieza en la ciudad de Bogotá",
        nombre = paste0("la pieza a evaluar se llama ",AE_nombre),
        rol = "Habla como persona experta en pobreza",
        voz = "tercera persona del prural",
        extension = "3 parrafos",
        datos = my_data) %>% 
          jsonlite::toJSON(pretty = F))))


# 4. Densidades ----






# my_basemap+
#   geom_sf(data = ab1_2005_2018, aes(fill = den_ranks), color = NA)+
#   scale_fill_manual(
#     "Hab/ha",
#     values = colorRampPalette(c("grey","pink","red4"))(5))+
#   geom_sf(data = z0_area_estudio,  aes(linetype = "Study Area"), fill = NA, lty = 2, lwd = 0.5)+
#   theme(legend.position = c(0.1, 0.4),
#         legend.background = element_rect(
#           fill = rgb(1, 1, 1, alpha = 0.8), color = NA))+
#     facet_wrap(.~year)
# 
# ggsave("pruebas_densidades.png", h = 6,w = 12)
