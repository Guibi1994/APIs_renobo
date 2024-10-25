source("2_3_api_proyeccion_economica/utils.R")



a0_coef <- readRDS("..\\data/03_tasty/r01_regression_cofficients.RDS")

b0_cooked <- readRDS("..\\data/02_cooked/m01_cooked_lotes_enrriquecidos.RDS")

c0_raw <- read.csv("..\\data/01_raw/mp01_matriz_por_lotes.csv")



b1_cooked <- b0_cooked %>% 
  select(id07a_ae, id07b_ae_name,
         starts_with("y01"), 
         y02a_variacion_2a,y02b_variacion_5a,y02c_variacion_8a)




a1_coef = a0_coef %>% 
  filter(subgrupo %in% c("Bogotá","AE Pieza Reencuentro","AE Campin 7 de Agosto"),
         str_detect(term, "universi")) %>% 
  select(term, subgrupo, y, modelo, estimate,stars) 
a1_coef

unique(a1_coef$y)

# Precios promedio 
b1_cooked <- b0_cooked %>%
  group_by(subgrupo = "Bogotá") %>% 
  summarise(across(y01a_valor_2015:y01c_valor_2021,~mean(., na.rm = T))) %>% 
  rbind(
    b0_cooked %>%
  filter(id07b_ae_name %in% c("AE Pieza Reencuentro","AE Campin 7 de Agosto")) %>% 
  group_by(subgrupo = id07b_ae_name) %>% 
  summarise(across(y01a_valor_2015:y01c_valor_2021, 
                   ~mean(., na.rm = T))) %>% 
    as.data.frame()) %>% 
  rename(y02c_variacion_8a = 2,y02b_variacion_5a=3,y02a_variacion_2a = 4) %>% 
  pivot_longer(cols = 2:4,
               values_to = "avg",
               names_to = "y")

b1_cooked

c1_final <- a1_coef %>% 
  merge(b1_cooked, by = c("subgrupo","y")) %>% 
  mutate(pp = estimate/avg) %>% 
  select(y, subgrupo, term, estimate, pp, stars)

write.csv(c1_final, "..\\data/03_tasty/r03_porcentages_universidades.csv",
          row.names = F)


c1_final %>% 
  mutate(label = 
           str_remove(term,"(.*)_d\\d{2}_") %>%
           str_replace("_o_mas"," o más") %>% 
           str_replace("_","-")) %>% 
  filter(subgrupo == "Bogotá",
         y == "y02c_variacion_8a") %>% 
  mutate(value = paste0(label, "m = ", round(pp*100,1),"% ","*",stars,"*")) %>% 
  select(value) %>% 
  kableExtra::kbl()





