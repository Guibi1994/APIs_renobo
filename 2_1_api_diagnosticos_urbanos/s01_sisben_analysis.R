# API SISBEN

# 0. Prepare environment ----
source("01_functions/utils.R")
AE = "AE Calle 72"



# 1. Load information ----

## 1.1. Study area
z0_study_area <- st_read("0_raw_data/1_geo_data/Actuaciones_estrategicas_12042024.shp")

## 1.2. Target inputs (IPM blocks)
a0_sisben <- read.delim2("0_raw_data/s4_31marzo.txt", sep = ";")


# 2. Process information ----

## 2.1 Study area
z1_study_area <- z0_study_area %>% 
  filter(NOMBRE ==AE) %>% 
  st_transform(crs = 4326)

## 2.2. Target inputs
#### A. Targets in the City
a1_sisben_city <- a0_sisben %>% 
  mutate(lon = as.numeric(coord_x_auto_rec),
         lat = as.numeric(coord_y_auto_rec)) %>%  
  filter(!is.na(lon),!is.na(lat)) %>% 
  st_as_sf(coords = c("lon","lat")) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = 4326)

### B. Targets in the Study area
a2_sisben_sa <- geo_intersect(
  target = a1_sisben_city,
  area = z1_study_area)

## 2.3. Extract map limits
limits <- st_bbox(z1_study_area)



# 3. Creating the MAP ----
my_map <- ggplot()+
  # 1. Target
  ## 1.1. Target info
  geom_sf(data = a2_sisben_sa, aes(color = grupo),
          size = 2, alpha = .6)+
  ## 1.2. Target color
  scale_color_manual(values = renovo_scale[1:4])+
  # 2. Base map
  ## 1. Study Area
  geom_sf(data = z1_study_area, fill = NA,
          color = "black", lwd = 0.5,lty =1) +
  # 3. Theme, limits and labels
  map_format+
  labs(color = "")

my_map



#DATA
c0_data <- a1_sisben_city %>% 
  as.data.frame() %>% 
  select(grupo) %>% 
  mutate(conjunto = "Ciudad") %>% 
  rbind(
    a2_sisben_sa %>% 
      as.data.frame() %>% 
      select(grupo) %>% 
      mutate(conjunto = "Pieza")) %>% 
  group_by(conjunto, grupo) %>% 
  summarise(n =n()) %>% 
  filter(grupo != "NULL") %>% 
  group_by(conjunto) %>% 
  ## Clasica
  arrange(conjunto,desc(grupo)) %>% 
  group_by(conjunto) %>% 
  mutate(tot = sum(n),
         pp = n/tot,
         position = (cumsum(pp)-pp)+(pp/2),
         my_text = paste0(100*round(pp,2), " %")) %>% 
  arrange(conjunto, grupo) %>% 
  as.data.frame() 


## PLOT
my_plot <- c0_data %>% 
  ggplot(aes(pp,conjunto, fill = grupo))+
  geom_bar(stat = "identity")+
  geom_text(aes(position,conjunto, label = my_text),
            angle = 90, color = "white", size = 3)+
  scale_x_continuous(labels = scales::percent_format())+
  scale_fill_manual(values = renovo_scale[1:4])+
  theme_minimal()+
  theme(text = element_text(family = "serif"),
        legend.position = "bottom")+
  labs(y = "", x = "", fill = "")
my_plot

### Combining plot and map
my_legend <- get_legend(my_plot)

my_map <- my_map+theme(legend.position = "none",
                       plot.margin = margin(5, 5, 5, 5))
my_plot <- my_plot+theme(legend.position = "none",
                         plot.margin = margin(5, 5, 5, 5))

combined_plot <- (my_map | my_plot) / plot_spacer() / my_legend + 
  plot_layout(heights = c(10, 1,1))
print(combined_plot)

ggsave("02_test_figures/04_sisben_total.png",
       w = 7,h = 4)




# 6. Descripción

list(
  instruccion = "Escribe dos parrafos. El primero explicando brevemente que
  son los grupos del SISBEN IV en colombia y la importancia de tomarlo en cuenta al evaluar políticas e 
  internvenciones urbanas. En el segundo describe los datos, enfocado en comparar
  la pieza con la ciudad, el centro del análisis es la pieza",
  contexto = "Son datos sobre numeros de reportados por grupo sisben el contexto
  de un analisis de la pieza en la ciudad de Bogotá",
  nombre = paste0("la pieza a evaluar se llama ",AE),
  rol = "Habla como persona experta en pobreza",
  voz = "tercera persona del prural",
  extension = "2 parrafos",
  datos = c0_data) %>% 
  jsonlite::toJSON(pretty = T)
