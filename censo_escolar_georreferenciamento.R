### DIRETORIOS E AMBIENTE####
remove(list=ls()) #limpar global environment
setwd("C:/Users/Samsung/Google Drive/SJC_operacional/Equipe_financeiro_economico")
setwd("C:/Users/taina/Google Drive/Trabalho/CEPESP/SJC/SJC_operacional/Equipe_financeiro_economico/")
#-------------------------------

### PACOTES ####
library(tidyverse)
library(readr)
library(sf)
library(readxl)
#----------------------------------

# ABRIR BASE DE MICRODAADOS 2018 ####
matriculas <- read_excel("Dados/Matr?culas Ensino B?sico - 2013/matriculasEB2013.xlsx")
sapply(matriculas, class)

matriculas_sf <- matriculas %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)

matriculas_sf %>% ggplot() + 
  geom_sf() + 
  coord_sf()
#----------------------------------

# ABRIR SHAPES DE REFERENCIA DA CIDADE DE SJC ####
sjc_municipio <- read_sf("Dados/Shapefiles/LIMITE_MUNICIPAL.shp") %>% st_transform(4326)
sjc_zonas_trafego <- read_sf("Dados/Shapefiles/ZART_PM15_06.shp") %>% st_transform(4326)
sjc_viario <- read_sf("Dados/Shapefiles/sjc_viario.shp") %>% st_transform(4326)
#----------------------------------

# CRIAR MAPA DE CALOR DAS OBSERVACOES ####
sjc_municipio %>% ggplot() +
  geom_sf(fill = "#CCCCCC")+
  stat_density2d(data = matriculas,aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), 
                 size = 0.0, bins = 200, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red", name=" Matr?culas em\n cada ponto") + 
  scale_alpha(range = c(0.05, 1), guide = FALSE)+
  labs(x = "", y = "")+
  geom_sf(data = sjc_viario) +
  theme(legend.text.align = 1, legend.title.align = 0.5)

ggsave("sjc_mapa_calor_matriculas_eb2013.png", plot = last_plot(), 
       device = "png", 
       path = "C:/Users/Samsung/Google Drive/SJC_operacional/Equipe_financeiro_economico/Imagens")
#----------------------------------

# TOTAL DE MATRICULA EM CADA ZONA DE TRAFEGO ####
# passar informacao das poligonos para a camada de pontos
st_centroid()

?st_centroid

intersection <- matriculas_sf %>% st_intersection(sjc_zonas_trafego)

intersection <- intersection %>% mutate(contador = 1)

intersection <- intersection %>% 
  group_by(ID) %>% 
  summarise(matriculas = sum(contador)) %>% 
  st_set_geometry(NULL) %>% 
  as.data.frame() 

sjc_zonas_trafego <- sjc_zonas_trafego %>% left_join(intersection, by = "ID")

sum(sjc_zonas_trafego$matriculas, na.rm = T) == sum(matriculas_sf$contador, na.rm = T) #TRUE eh bom

sjc_zonas_trafego <- sjc_zonas_trafego %>% 
  mutate(matriculas = ifelse(is.na(sjc_zonas_trafego$matriculas), 0, sjc_zonas_trafego$matriculas))

sjc_municipio %>% ggplot() +
  geom_sf(fill = "#CCCCCC") +
  geom_sf(data = sjc_zonas_trafego, aes(fill = matriculas)) +
  coord_sf()

ggsave("sjc_mapa_matriculas_zona_trafego_eb2013.png", plot = last_plot(), 
       device = "png", 
       path = "C:/Users/Samsung/Google Drive/SJC_operacional/Equipe_financeiro_economico/Imagens")


# EXPORTAR BASES PARA SHP ####
st_write(matriculas_sf, 
         dsn = "C:/Users/taina/Google Drive/Trabalho/CEPESP/SJC/SJC_operacional/Equipe_financeiro_economico/Dados/Shapefiles/matriculas_2013.shp")
#----------------------------------

# TOTAL DE MATRICULAS EM CADA ZONA DE TR?FEGO 2013 ####
zonas_trafego <- sjc_zonas_trafego %>% mutate(contador = 1) %>%
  group_by(ZAT_55) %>% 
  summarise(area = sum(AREA), pop = sum(POP), emp = sum(EMPREGOS), sub_zonas = sum(contador))

zonas_trafego %>% ggplot() + geom_sf() + coord_sf()

# passar informacao das poligonos para a camada de pontos
intersection <- matriculas_sf %>% st_intersection(zonas_trafego)

intersection <- intersection %>% mutate(contador = 1)

intersection <- intersection %>% 
  group_by(ZAT_55) %>% 
  summarise(matriculas = sum(contador)) %>% 
  st_set_geometry(NULL) %>% 
  as.data.frame()

zonas_trafego <- zonas_trafego %>% left_join(intersection, by = "ZAT_55")

zonas_trafego <- zonas_trafego %>% 
  mutate(matriculas = ifelse(is.na(zonas_trafego$matriculas), 0, zonas_trafego$matriculas))

sum(zonas_trafego$matriculas, na.rm = T) == nrow(matriculas_sf) #TRUE eh bom

# EXPORTAR BASES PARA SHP ####
st_write(zonas_trafego, 
         dsn = "C:/Users/taina/Google Drive/Trabalho/CEPESP/SJC/SJC_operacional/Equipe_financeiro_economico/Dados/Shapefiles/matriculas_zonaOD2013.shp")
#----------------------------------

# IDEM PARA 2018 --------------------------------------------------------------------------------------
remove(list=ls()) #limpar global environment

# ABRIR BASE DE MICRODAADOS 2018 ####
matriculas <- read_excel("Dados/Matr?culas Ensino B?sico - 2018/matriculasEB2018.xlsx")
sapply(matriculas, class)

matriculas_sf <- matriculas %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
#----------------------------------

# EXPORTAR BASES PARA SHP ####
st_write(matriculas_sf, 
         dsn = "C:/Users/taina/Google Drive/Trabalho/CEPESP/SJC/SJC_operacional/Equipe_financeiro_economico/Dados/Shapefiles/matriculas_2018.shp")
#----------------------------------

# ABRIR SHAPES DE REFERENCIA DA CIDADE DE SJC ####
sjc_zonas_trafego <- read_sf("Dados/Shapefiles/ZART_PM15_06.shp") %>% st_transform(4326)
#----------------------------------

# TOTAL DE MATRICULAS EM CADA ZONA DE TR?FEGO 2018 ####
zonas_trafego <- sjc_zonas_trafego %>% mutate(contador = 1) %>%
  group_by(ZAT_55) %>% 
  summarise(area = sum(AREA), pop = sum(POP), emp = sum(EMPREGOS), sub_zonas = sum(contador))

# passar informacao das poligonos para a camada de pontos
intersection <- matriculas_sf %>% st_intersection(zonas_trafego)

intersection <- intersection %>% mutate(contador = 1)

intersection <- intersection %>% 
  group_by(ZAT_55) %>% 
  summarise(matriculas = sum(contador)) %>% 
  st_set_geometry(NULL) %>% 
  as.data.frame()

zonas_trafego <- zonas_trafego %>% left_join(intersection, by = "ZAT_55")

zonas_trafego <- zonas_trafego %>% 
  mutate(matriculas = ifelse(is.na(zonas_trafego$matriculas), 0, zonas_trafego$matriculas))

sum(zonas_trafego$matriculas, na.rm = T) == nrow(matriculas_sf) #TRUE eh bom

# EXPORTAR BASES PARA SHP ####
st_write(zonas_trafego, 
         dsn = "C:/Users/taina/Google Drive/Trabalho/CEPESP/SJC/SJC_operacional/Equipe_financeiro_economico/Dados/Shapefiles/matriculas_zonaOD2018.shp")
#----------------------------------