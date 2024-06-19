# API Stratums

# 0. Prepare environment ----
source("01_functions/utils.R")
AE = "AE Calle 72"




# 1. Load information ----

## 1.1. Study area
z0_study_area <- st_read("0_raw_data/1_geo_data/Actuaciones_estrategicas_12042024.shp")

## 1.2. Target inputs (Stratum blocks)
a0_stratum <- read_sf("0_raw_data/1_geo_data/ManzanaEstratificacion.shp")



# 2. Process information ----

## 2.1 Study area
z1_study_area <- z0_study_area %>% 
  filter(NOMBRE ==AE) %>% 
  st_transform(crs = 4326)

## 2.2. Target inputs
#### A. Targets in the City
a1_stratum_city <- a0_stratum %>% 
  mutate(
    color = case_when(
      ESTRATO == 0~"grey20",
      ESTRATO ==1~renovo_scale[1],
      ESTRATO ==2~renovo_scale[2],
      ESTRATO ==3~renovo_scale[3],
      ESTRATO ==4~renovo_scale[4],
      ESTRATO ==5~renovo_scale[5],
      T~renovo_scale[6]),
    ESTRATO = ifelse(ESTRATO==0,"Sin estrato",ESTRATO),
    ESTRATO = as.factor(ESTRATO)) %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

### B. Targets in the Study area
a2_stratum_sa <- geo_intersect(
  target = a1_stratum_city,
  area = z1_study_area)



## 2.3. Extract map limits
limits <- st_bbox(a2_stratum_sa)


# 3. Creating the MAP ----
colors_city <- setNames(a1_stratum_city$color, a1_stratum_city$ESTRATO)
colors_sa <- setNames(a2_stratum_sa$color, a2_stratum_sa$ESTRATO)

my_map <- ggplot()+
  # 1. Target
  ## 1.1. Target info
  geom_sf(data = a2_stratum_sa, aes(fill = ESTRATO),
          color = "grey40")+
  # 2. Base map
  scale_fill_manual(values = colors_sa)+
  ## 1. Study Area
  geom_sf(data = z1_study_area, fill = NA,
          color = "black", lwd = 0.5,lty =1) +
  # 3. Theme and labels
  map_format+
  labs(fill = "")
my_map




# 5. Crating plot

c0_data <- 
  a1_stratum_city %>% 
  select(ESTRATO, SHAPE_AREA, color) %>% 
  as.data.frame() %>% 
  group_by(Estrato = ESTRATO, color) %>% 
  summarise(
    manzanas = n(),
    area = sum(SHAPE_AREA, na.rm = T)) %>% 
  mutate(grupo = "Ciudad") %>% 
  rbind(
    a2_stratum_sa %>% 
      select(ESTRATO, SHAPE_AREA, color) %>% 
      as.data.frame() %>% 
      group_by(Estrato = ESTRATO, color) %>% 
      summarise(
        manzanas = n(),
        area = sum(SHAPE_AREA, na.rm = T)) %>% 
      mutate(grupo = "Pieza")) %>% 
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
  
my_plot <- c0_data %>% 
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


# Combine map and plot
my_legend <- get_legend(my_plot)

my_map <- my_map+theme(legend.position = "none",
                       plot.margin = margin(5, 5, 5, 5))
my_plot <- my_plot+theme(legend.position = "none",
                         plot.margin = margin(5, 5, 5, 5))

combined_plot <- (my_map | my_plot) / plot_spacer() / my_legend + 
  plot_layout(heights = c(10, 1,1))
print(combined_plot)

ggsave("02_test_figures/03_stratums_total.png",
       w = 7,h = 4)


# 6. Descripción

list(
  instruccion = "Escribe dos parrafos. El primero explicando brevemente que
  son los estratos socioeconómicos en colombia y la importancia de tomarlo en cuenta al evaluar políticas e 
  internvenciones urbanas. En el segundo describe los datos, enfocado en comparar
  la pieza con la ciudad, el centro del análisis es la pieza",
  contexto = "Son datos sobre area por estrato y numero de manzanasen el contexto
  de un analisis de la pieza en la ciudad de Bogotá",
  nombre = paste0("la pieza a evaluar se llama ",AE),
  rol = "Habla como persona experta en pobreza",
  voz = "tercera persona del prural",
  extension = "2 parrafos",
  datos = c0_data) %>% 
  jsonlite::toJSON(pretty = F)

#####################

# 
# 
# z2_dane2018 <- st_read(dsn = gdb_path, 
#                        layer = "B6_Manzanas_CENSO_2018")
# 
# z3_dane2018 <- 
#   z2_dane2018 %>% 
#   st_make_valid() %>% 
#   st_transform(crs = 4326) %>% 
#   st_set_crs(4326) %>% 
#   st_centroid()
# 
# 
# z4_dane2018 <- geo_intersect(z3_dane2018,area = z1_study_area)
# 
# 
# ggplot()+
#   geom_sf(data = a2_ipm_sa)+
#   geom_sf(data = z4_dane2018)+
#   map_format
# 
# 
# z5_join <- st_join(z4_dane2018,a2_ipm_sa)
# 
# z5_join <- z5_join %>% 
#   select(ipm,SEXO_TOTAL)
# 
# z5_join %>% 
#   as.data.frame() %>% 
#   mutate(ipm_pobres = (ipm/100)*SEXO_TOTAL) %>% 
#   summarise(
#     ipm_pobres = sum(ipm_pobres, na.rm = T),
#     pot_total = sum(SEXO_TOTAL, na.rm = T)) %>% 
#   mutate(pp =ipm_pobres/pot_total)
# 
# 
# z6_join <- st_join(z3_dane2018,a1_ipm_city)
# z6_join <- z6_join %>% 
#   select(ipm,SEXO_TOTAL)
# 
# 
# rbind(
#   z5_join %>% 
#     as.data.frame() %>% 
#     mutate(ipm_pobres = (ipm/100)*SEXO_TOTAL) %>% 
#     summarise(
#       ipm_pobres = sum(ipm_pobres, na.rm = T),
#       pot_total = sum(SEXO_TOTAL, na.rm = T)) %>% 
#     mutate(pp =ipm_pobres/pot_total,
#            grupo = "Pieza"),
#   
#   z6_join %>% 
#     as.data.frame() %>% 
#     mutate(ipm_pobres = (ipm/100)*SEXO_TOTAL) %>% 
#     summarise(
#       ipm_pobres = sum(ipm_pobres, na.rm = T),
#       pot_total = sum(SEXO_TOTAL, na.rm = T)) %>% 
#     mutate(pp =ipm_pobres/pot_total,
#            grupo = "Ciudad"))




