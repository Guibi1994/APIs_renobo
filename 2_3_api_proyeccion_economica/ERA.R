source("2_3_api_proyeccion_economica/utils.R")  





# 0. Preparar entorno ----
## 0.1. Cargar datos ----
z0_dictionary <- read_sheet(ss = link_diccionario) %>%
  select(variable_name, variable = variable_label)

a1_coefficients <- readRDS("..\\data/03_tasty/r01_regression_cofficients.RDS") %>% 
  mutate(
    # Crear grupos de regresores
    result_type = 
      case_when(
        str_detect(term,"_ae_|\\(In|cod_area|stra")==T~"hide",
        str_detect(term,"Q\\d{2}")==T~"density",
        str_detect(term,"d\\d{2}")==T~"distance",
        T~"regular"),
    # Depurar nombres de X's
    variable_name = str_remove_all(term,"_d\\d+(.*)|_Q\\d+(.*)")) %>% 
  merge(z0_dictionary, by ="variable_name", all.x = T) %>% 
  mutate(
    variable = ifelse(is.na(variable), term, variable),
    variable = case_when(
    result_type == "density"~paste0("Densidad de: ",variable),
    result_type == "distance"~paste0("distancia a: ",variable),
    T~variable)) %>% 
    
    # Depurar nombres de Y's
  mutate(
    y_label = case_when(
      y == "y02a_variacion_2a"~"Panel A\nVariación últimos 2 años\n(2021-2023)", 
      y == "y02b_variacion_5a"~"Panel B\nVariación últimos 5 años\n(2018-2023)",
      y == "y02c_variacion_8a"~"Panel C\nVariación últimos 8 años\n(2015-2023)",
      T~"Otra cosa"),
    # Extraer etiquetas (x'saturadas)
    x_label  = case_when(
      result_type == "distance"~
        str_remove(term,"(.*)_d\\d{2}_") %>%
        str_replace("_o_mas"," o más") %>% 
        str_replace("_","-"),
      result_type == "density"~
        str_extract(term,"Q\\d{2}"),
      T~"Otra cosa"),
    x_label = factor(
      x_label,
      levels  = orden_saturaciones))


b1_adjustmetns <- readRDS("..\\data/03_tasty/r02_regression_adjustments.RDS") %>% 
  mutate(y_label = case_when(
    y == "y02a_variacion_2a"~"Panel A\nVariación últimos 2 años\n(2021-2023)", 
    y == "y02b_variacion_5a"~"Panel B\nVariación últimos 5 años\n(2018-2023)",
    y == "y02c_variacion_8a"~"Panel C\nVariación últimos 8 años\n(2015-2023)",
    T~"Otra cosa"))


## 0.2. Conectarse a Drive ----
### Carpeta permanente ----
parent_folder = "Resultados modelo de equilibrio"
id_parent = drive_get(parent_folder)

### Resultados del día ----
results_folder = paste0("Resultados_",Sys.Date())
drive_mkdir(results_folder,path = id_parent,overwrite = T)
id_results <- drive_get(results_folder) 


## 0.3. Determinar grupos de análisis ----
grupos <- unique(a1_coefficients$subgrupo)[3:length(
  unique(a1_coefficients$subgrupo))]

1:length(grupos)


# LOOP ----
for (i in 1:length(grupos)) {
  message("Resultados: ",grupos[i])
  
  # 1. Crear espacios en Drive ----
  ## 1.1. Crear carpeta del grupo ----
  message("   1. Crear carpeta del AE")
  id = drive_mkdir(grupos[i],path = id_results,overwrite = T)
  id_group <- drive_get(id$id)
  
  ## 1.2. Crear subcarpetas del grupo ----
  message("   2. Crear subcarpetas del AE")
  
  ### 1.2.1. Subcarpeta de Ajuses ----
  drive_mkdir("1. Ajustes",path = id_group, overwrite = T)
  id_ajustes = drive_ls(path =id_group, pattern = "1. Ajustes")
  
  ### 1.2.2. Subcarpeta de Regresiones ----
  drive_mkdir("2. Regresiones",path = id_group, overwrite = T)
  id_regesiones = drive_ls(path =id_group, pattern = "2. Regresiones")

  
  ### 1.2.3. Subcarpeta de Textos de IA ----
  drive_mkdir("3. Textos de IA",path = id_group, overwrite = T)
  id_textos = drive_ls(path =id_group, pattern = "3. Textos de IA")
  
  
  
  # 2. Filtrar las bases ----
  message("   3. Filtando pases por:", grupos[i])
  ## 2.1. Coeficientes ----
  a2_coefficients <- a1_coefficients %>%
    filter(subgrupo %in% c("Bogotá",grupos[i]))
  
  ## 2.2. Ajustes ----
  b2_adjustmetns <- b1_adjustmetns %>%
    filter(subgrupo %in% c("Bogotá",grupos[i]))
  
  
  # 3. Gráficos de Ajustes ----
  message("   4. Gráfica de ajustes")
  p <- b2_adjustmetns %>%
    ggplot(aes(
      adj.r.squared,
      subgrupo,
      group = y_label, fill = subgrupo)) +
    geom_bar(stat = "identity",alpha = .8)+
    coord_cartesian(xlim = c(0,1))+
    facet_wrap(.~y_label,scales = "free_x",
               nrow = 2)+
    scale_fill_manual(values = c(renovo[1:2]))+
    my_theme+
    theme(legend.position = "bottom")+
    labs(title = "Calidad del modelo",
         subtitle = "R2 ajustado", x = "R2 ajustado", y = "")
  
  message("   - Iniciando exportación a Drive")
  ggdrive_save(
    plot = p,drive_path  = id_ajustes, name = "ajuste.png",
    w = 7,h = 4)
  message("   - Exportación a Exitosa")
  
  # 4. Gráficos de Coeficientes regulares ----
  message(" 5. Gráficos de coeficientes regulares")
  
  p <- a2_coefficients %>% 
    # Dimensiones
    filter(subgrupo %in% c("Bogotá", grupos[i])) %>% 
    filter(result_type=="regular") %>% 
    #PLOT
    ggplot(aes(
      estimate,variable, color = effect_type))+
    geom_vline(xintercept = 0,lty =2)+
    geom_point()+
    geom_errorbar(aes(
      xmin = conf.low,
      xmax = conf.high,
      y = variable), lwd = 0.1, width = 0.1)+
    facet_wrap(subgrupo~y_label, nrow = 2, scales = "free_x")+
    scale_color_manual(values = c("grey60",renovo[c(3,1)]))+
    scale_x_continuous(labels = scales::dollar_format())+
    my_theme+
    labs(
      title = grupos[i],
      subtitle = "Modelo de Equilibrio Espacial: Dimensión intrínseca",
      caption = "Fuente: RENOBO",
      x = "Variación neta (COP)", y = "",
      color = "")+
    theme(legend.position = "bottom")
  
  message("   - Iniciando exportación a Drive")
  ggdrive_save(
    plot = p,drive_path  = id_regesiones, name = "1. Dimensión Intrínseca-General.png",
    w = 8,h = 8)
  message("   - Exportación a Exitosa")
  # 5. Gráficos de Coeficientes saturados ----
  
  ## 5.1. Deterinar grupos de variables -----
  saturaciones <- a2_coefficients %>% 
    filter(result_type %in% c("distance","density")) %>% 
    group_by(variable) %>% 
    summarise() %>% pull(variable)
  
  ## 5.2. 
  for (u in 1:length(saturaciones)) {
    
    message("     ->", saturaciones[u],"  ...")
    # Filtrar la base por la variable
    p <- a2_coefficients %>%
      filter(variable == saturaciones[u]) %>% 
      # PLOT
      ggplot(aes(x_label,estimate, color = effect_type))+
      geom_point()+
      geom_line(aes(x_label, estimate, group =y))+
      geom_errorbar(aes(
        ymin = conf.low,
        ymax = conf.high,
        x = x_label), lwd = 0.1, width = 0.1) +
      geom_hline(yintercept = 0,lty =2)+
      scale_y_continuous(labels = scales::dollar_format())+
      my_theme+
      theme(axis.text.x = element_text(angle = 90,hjust = 1),
            legend.position = "bottom") + 
      scale_color_manual(values = c("grey","red","cyan3"))+
      facet_wrap(subgrupo~y_label, scales = "free_y")+
      labs(title = paste0("Efectos de la ",saturaciones[u]),
           subtitle = grupos[1],
           color ="",x = "", y = "",
           caption = "Fuente: RENOBO")
    
    message("   - Iniciando exportación a Drive")
    ggdrive_save(
      plot = p,drive_path  = id_regesiones, 
      name = paste0("2.",u,". Hedónicos_",
                    tolower(str_replace_all(saturaciones[u]," |:|\\/","_")),".png"),
      w = 10,h = 8)
    message("   - Exportación a Exitosa")
  }
  message("----- Finalización exitosa -----")
}
  
 




