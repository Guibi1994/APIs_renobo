source("2_3_api_proyeccion_economica/utils.R")  


a1_coefficients <- readRDS("..\\data/03_tasty/r01_regression_cofficients.RDS")
b1_adjustmetns <- readRDS("..\\data/03_tasty/r02_regression_adjustments.RDS")

# Filtrar resultados
a2_coefficients <- a1_coefficients %>% 
  filter(subgrupo %in% c("Bogotá","Chapinero")) 


b2_adjustmetns <- b1_adjustmetns %>% 
  filter(subgrupo %in% c("Bogotá","Chapinero")) 



# 1. Ajustes geneales
b2_adjustmetns %>% 
  ggplot(aes(
    adj.r.squared,
    subgrupo,
    group = y, fill = subgrupo)) + 
  geom_bar(stat = "identity",alpha = .8)+
  coord_cartesian(xlim = c(0,1))+
  facet_wrap(.~y,scales = "free_x",
             nrow = 2)+
  scale_fill_manual(values = c(renovo[1:2]))+
  my_theme+
  theme(legend.position = "bottom")+
  labs(title = "Calidad del modelo",
       subtitle = "R2 ajustado", x = "R2 ajustado", y = "")


# 2. 
a2_coefficients %>% 
  # Modelos
  filter(as.numeric(tipo) == 1) %>% 
  # Dimensiones
  filter(str_detect(term,"^in|^ec")) %>% 
  #PLOT
  ggplot(aes(
    estimate,term, color = effect_type))+
  geom_vline(xintercept = 0,lty =2)+
  geom_point()+
  geom_errorbar(aes(
    xmin = conf.low,
    xmax = conf.high,
    y = term), lwd = 0.1, width = 0.1)+
  facet_wrap(subgrupo~y, nrow = 2, scales = "free_x")+
  scale_color_manual(values = c("grey60",renovo[c(3,1)]))+
  scale_x_continuous(labels = scales::dollar_format())+
  my_theme+
  labs(
    title = "Resultados PoC",
    subtitle = "Dimensión intrínseca",
    caption = "Fuente: RENOBO",
    x = "Variación neta (COP)", y = "",
    color = "")+
  theme(legend.position = "bottom")

a2_coefficients %>% 
  # Modelos
  filter(as.numeric(tipo) == 1) %>% 
  # Dimensiones
  filter(str_detect(term,"^or|^se")) %>% 
  #PLOT
  ggplot(aes(
    estimate,term, color = effect_type))+
  geom_vline(xintercept = 0,lty =2)+
  geom_point()+
  geom_errorbar(aes(
    xmin = conf.low,
    xmax = conf.high,
    y = term), lwd = 0.1, width = 0.1)+
  facet_wrap(subgrupo~y, nrow = 2, scales = "free_x")+
  scale_color_manual(values = c("grey60",renovo[c(3,1)]))+
  scale_x_continuous(labels = scales::dollar_format())+
  my_theme+
  labs(
    title = "Resultados PoC",
    subtitle = "Dimensión intrínseca",
    caption = "Fuente: RENOBO",
    x = "Variación neta (COP)", y = "",
    color = "")+
  theme(legend.position = "bottom")
  







## Graficas para saturaciones de distancias

#"eq01b_educacion_colegios_a_0_50", "eq01c","eq02a_salud","eq05a_seguridad","ep01a_parques", "ep02"

a2_coefficients %>% signicance_labels() %>% 
  filter(as.numeric(tipo)  ==1) %>% 
  filter(str_detect(term,"eq03")) %>%
  mutate(term = str_extract(term,"d\\d+\\_\\d+(.*)") %>%
           str_replace(.,"_",". ") %>% str_replace(.,"_","-")) %>% 
  
  # PLOT
  ggplot(aes(term,estimate, color = effect_type))+
  geom_point()+
  geom_path(aes(term, estimate, group =y))+
  geom_errorbar(aes(
         ymin = conf.low,
         ymax = conf.high,
         x = term), lwd = 0.1, width = 0.1) +
  geom_hline(yintercept = 0,lty =2)+
  scale_y_continuous(labels = scales::dollar_format())+
  
  #coord_flip()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1),
        text = element_text(family = "serif"),
        legend.position = "bottom") + 
  scale_color_manual(values = c("grey","red","cyan3"))+
  facet_wrap(subgrupo~y, scales = "free_y")+
  labs(title = "Efectos de distancias saturadas",
       subtitle = "Equipamientos de cultura", color ="",x = "", 
       caption = "Fuente: RENOBO")



