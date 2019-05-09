### DIRETORIOS E AMBIENTE####
remove(list=ls()) #limpar global environment
setwd("C:/Users/taina/Google Drive/Trabalho/CEPESP/SJC/SJC_operacional/Equipe_financeiro_economico/")
#-------------------------------

### PACOTES ####
library(tidyverse)
library(readr)
library(sf)
library(readxl)
#----------------------------------

### FUNCAO QUE PASSA GEOMETRIA DA CAMADA PARA COLUNAS ####
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}
#----------------------------------

### ABRIR BASE DE ESTABELECICMENTO DA RAIS DE 2017 ####
estabelecimentos_2017 <- read_delim("Dados/RAIS/ESTB2017ID.txt", 
                                    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                                    trim_ws = TRUE)
#8.186.588 observações
sapply(estabelecimentos_2017, class)
#----------------------------------

### SELECIONAR VARIAVEIS DE INTERESSE ####
# variaveis de interesse:
# 1) cepestab (renomear para cep e deixar NUM)
# 2) cnpjcei
# 3) cnpjraiz
# 4) municpio (eh NUM com 6 digitos)
# 5) qtdvnculosativos
# 6) qtdvnculosestatutrios
# 7) razosocial
# 8) indatividadeano
table(estabelecimentos_2017$`Ind Atividade Ano`) #6.437.903 ativos em 2017
table(is.na(estabelecimentos_2017$`CEP Estab`)) #nao tem NA

estabelecimentos_2017 <- estabelecimentos_2017 %>% 
  filter(Município == 3549904 | Município == 354990) %>% 
  mutate(trabalhadores = as.numeric(`Qtd Vínculos CLT`) + as.numeric(`Qtd Vínculos Estatutários`)) %>%
  rename(cep = `CEP Estab`,razao_social =  `Razão Social`) %>%
  dplyr::select(cep, trabalhadores, razao_social, `Ind Atividade Ano`)


table(estabelecimentos_2017$`Ind Atividade Ano`) #25.147 empresas ativas e 6.546 inativas
sum(estabelecimentos_2017$trabalhadores) #187.441 trabalhladores

inativas <- estabelecimentos_2017 %>% filter(`Ind Atividade Ano` == 0)

estabelecimentos_2017 <- estabelecimentos_2017 %>% 
  filter(`Ind Atividade Ano` == 1)
#----------------------------------

### EXCLUIR CEPS ERRADOS DA BASE ####
ceps_errados <- estabelecimentos_2017 %>% filter(cep == 99999999) #108 ceps
sum(ceps_errados$trabalhadores) #372 trabalhadores

estabelecimentos_2017 <- estabelecimentos_2017 %>% filter(cep != 99999999) #25.039
sum(estabelecimentos_2017$trabalhadores) #187.069 trabalhladores

nrow(estabelecimentos_2017 %>% filter(trabalhadores == 0))

sem_trabalhadores <- estabelecimentos_2017 %>% filter(trabalhadores == 0)

estabelecimentos_2017 <- estabelecimentos_2017 %>% filter(trabalhadores > 0)
sum(estabelecimentos_2017$trabalhadores) #187.069 trabalhladores
#----------------------------------

# AGRUPAR TRABALHADORES POR CEP ####
trab_por_cep <- estabelecimentos_2017 %>% 
  group_by(cep) %>% 
  summarise(trabalhadores = sum(trabalhadores)) %>% arrange(-trabalhadores) %>%
  mutate(cep = as.numeric(cep)) #2.101 CEPs

summary(trab_por_cep$trabalhadores)
trab_por_cep <- trab_por_cep %>% filter(trabalhadores > 0) #2.101
summary(trab_por_cep$trabalhadores)

trab_por_cep %>% ggplot() + geom_histogram(aes(x = trabalhadores)) + ylab("firmas")
trab_por_cep %>% filter(trabalhadores < 200) %>% ggplot() + geom_histogram(aes(x = trabalhadores)) + ylab("firmas")
#----------------------------------

### ABRIR BASE DE CEPs ####
load("Dados/cep_taina.Rda")
load("Dados/cep_miguel.Rda")
#----------------------------------

### GEORREFERENCIANDO OS CEPs DOS ESTABELECIMENTOS DE 2017 ####
trab_por_cep <- trab_por_cep %>% left_join(cep_taina, by = "cep")
table(is.na(trab_por_cep$latitude)) #15 nao enconrados

ceps_nao_encontrados <- trab_por_cep %>% filter(is.na(latitude))
ceps_nao_encontrados <- ceps_nao_encontrados %>%
  dplyr::select(cep, trabalhadores) %>%
  left_join(cep_miguel, by = "cep")
table(is.na(ceps_nao_encontrados$latitude)) #13 nao encontrados (achou 2)

mais_ceps <- ceps_nao_encontrados %>% filter(!is.na(latitude))
ceps_nao_encontrados <- ceps_nao_encontrados %>% filter(is.na(latitude))

trab_por_cep <- trab_por_cep %>% filter(!is.na(latitude))

trab_por_cep <- rbind(trab_por_cep, mais_ceps)

write.csv(ceps_nao_encontrados, file = "Dados/RAIS/ceps_nao_encontrados.csv")
#https://pt.stackoverflow.com/questions/400/como-buscar-um-endere%C3%A7o-por-cep-nos-correios
#https://www.republicavirtual.com.br/cep/exemplos.php

ceps_nao_encontrados <- read.csv(file = "Dados/RAIS/ceps_nao_encontrados_latlong.csv", sep = ";")

mais_ceps <- ceps_nao_encontrados %>% filter(!is.na(latitude))
ceps_nao_encontrados <- ceps_nao_encontrados %>% filter(is.na(latitude))

trab_por_cep <- trab_por_cep %>% filter(!is.na(latitude))
trab_por_cep <- rbind(trab_por_cep, mais_ceps)

# apenas 8  CEPs ficaram de fora
sum(trab_por_cep$trabalhadores) #186.975
sum(ceps_nao_encontrados$trabalhadores) #94

trab_por_cep <- trab_por_cep %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) #2.093 pontos

remove(cep_miguel, cep_taina, ceps_errados, ceps_nao_encontrados, mais_ceps)
remove(inativas, sem_trabalhadores)
#----------------------------------

# EXPORTAR SHAPE DE PONTOS ####
st_write(trab_por_cep, 
         dsn = "C:/Users/taina/Google Drive/Trabalho/CEPESP/SJC/SJC_operacional/Equipe_financeiro_economico/Dados/RAIS/trab_por_cep.shp")
#----------------------------------

### ABRIR SHP DO MUNICIPIO DO sjc PARA FAZER O JOIN ####
sjc_municipio <- read_sf("Dados/Shapefiles/LIMITE_MUNICIPAL.shp") %>% st_transform(4326)
sjc_zonas_trafego <- read_sf("Dados/Shapefiles/ZART_PM15_06.shp") %>% st_transform(4326)
sjc_logradouros <- read_sf("Dados/Shapefiles/LOGRADOUROS.shp") %>% st_transform(4326)
sjc_urbano <- read_sf("Dados/Shapefiles/sjc_urbano_2015.shp") %>% st_transform(4326)

sjc_municipio %>% ggplot() + 
  geom_sf() +
  geom_sf(data = trab_por_cep) + 
  coord_sf(xlim = c(-46.10801, -45.72131), ylim = c(-23.30666, -22.81627))

sjc_zonas_trafego %>% ggplot() + 
  geom_sf() + 
  coord_sf()

sjc_logradouros %>% ggplot() + 
  geom_sf() + 
  coord_sf()
#----------------------------------

# CRIANDO VIARIO DE INTERESSE PARA MAPA ####
table(sjc_logradouros$tipo)
dutra <- sjc_logradouros %>% filter(ref == "SP-060;BR-116")
cpinto <- sjc_logradouros %>% filter(nome == "Rodovia Governador Carvalho Pinto")
tamoios <- sjc_logradouros %>% filter(nome == "Rodovia dos Tamoios")
mlobato <- sjc_logradouros %>% filter(ref == "SP-050")
rod66 <- sjc_logradouros %>% filter(ref == "SP-066")
anoes <- sjc_logradouros %>% filter(nome == "Travessa dos Anões")
mcovas <- sjc_logradouros %>% filter(nome == "Avenida Mário Covas")
zarur <- sjc_logradouros %>% filter(nome == "Avenida Jorge Zarur")
cury <- sjc_logradouros %>% filter(nome == "Avenida Doutor Eduardo Cury")
saldanha <- sjc_logradouros %>% filter(nome == "Rua Manoel Saldanha")
felicio <- sjc_logradouros %>% filter(nome == "Rua Sebastião Felício")
jk <- sjc_logradouros %>% filter(nome == "Avenida Presidente Juscelino Kubitschek")
marson <- sjc_logradouros %>% filter(nome == "Avenida João Marson")
cardoso <- sjc_logradouros %>% filter(nome == "Viaduto João Alves Cardoso")
id_2951 <- sjc_logradouros %>% filter(id_0 == 2951)
id_6216 <- sjc_logradouros %>% filter(id_0 == 6216)
barbosa <- sjc_logradouros %>% filter(nome == "Avenida Rui Barbosa")
alfredo <- sjc_logradouros %>% filter(nome == "Avenida Professor Alfredo Fernandes de Almeida")
id_1711 <- sjc_logradouros %>% filter(id_0 == 1711)
id_7802 <- sjc_logradouros %>% filter(id_0 == 7802)
florestan <- sjc_logradouros %>% filter(nome == "Avenida Florestan Fernandes")
vilela <- sjc_logradouros %>% filter(nome == "Avenida Senador Teotônio Vilela")
gualberto <- sjc_logradouros %>% filter(nome == "Avenida Engenheiro Sebastião Gualberto")
dinamarca <- sjc_logradouros %>% filter(nome == "Rua Dinamarca")
tremembe <- sjc_logradouros %>% filter(nome == "Rua Tremembé")
#astronautas <- sjc_logradouros %>% filter(nome == "Avenida dos Astronautas")


sjc_viario <- rbind(dutra, cpinto, tamoios, mlobato, rod66, anoes, 
                    mcovas, zarur, cury, saldanha, felicio, jk, marson, 
                    cardoso, id_2951, id_6216, barbosa, alfredo, id_1711, 
                    id_7802, florestan, vilela, gualberto, dinamarca, tremembe) %>%
  filter(id_0 != 2829) %>% filter(id_0 != 5706) %>% filter(id_0 != 5707)

sjc_viario %>% ggplot() + 
  geom_sf() #+ 
  #coord_sf(xlim = c(-45.9, -45.85), ylim = c(-23.2, -23.15))
  
remove(dutra, cpinto, tamoios, mlobato, rod66, anoes, 
       mcovas, zarur, cury, saldanha, felicio, jk, marson, 
       cardoso, id_2951, id_6216, barbosa, alfredo, id_1711, 
       id_7802, florestan, vilela, gualberto, dinamarca, tremembe,
       sjc_logradouros)

write_sf(sjc_viario, 
         "C:/Users/taina/Google Drive/Trabalho/CEPESP/SJC/SJC_operacional/Equipe_financeiro_economico/Dados/Shapefiles/sjc_viario.shp")
#----------------------------------

# FAZENDO O JOIN ####
# recortando os pontos que cairam fora do sjc (posteriormente esse erro precisa ser verificado!)
pontos_sjc <- trab_por_cep %>% st_intersection(sjc_municipio) #1.938 pontos
sum(pontos_sjc$trabalhadores) #185.262
pontos_sjc <- pontos_sjc %>% mutate(contador = 1)

# plotando o municípios e os pontos
sjc_municipio %>% ggplot() +
  coord_sf() +
  geom_sf() +
  #geom_sf(data = sjc_zonas_trafego) +
  geom_sf(data = sjc_viario) +
  geom_sf(data = pontos_sjc, size = 0.5)
#----------------------------------

# EXPORTAR SHAPE DE PONTOS ####
st_write(pontos_sjc, 
         dsn = "C:/Users/taina/Google Drive/Trabalho/CEPESP/SJC/SJC_operacional/Equipe_financeiro_economico/Dados/RAIS/pontos_sjc.shp")
#----------------------------------

# MAPA DE CALOR PARA OS EMPREGOS ####
#ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2015/UFs/SP/sp_municipios.zip
estado_sp <- read_sf("Dados/Shapefiles/35MUE250GC_SIR.shp") %>% st_transform(4326)

# MAPA DE CALOR DOS POSTOS DE EMPREGO ####
pontos_sjc_df <- pontos_sjc %>% sfc_as_cols(names = c("X", "Y")) %>% st_set_geometry(NULL) %>% as.data.frame()

estado_sp %>% ggplot() +
  geom_sf(fill = "#999999")+
  geom_sf(data = sjc_municipio, fill = "#CCCCCC")+
  stat_density2d(data = pontos_sjc_df,aes(x = X, y = Y, fill = ..level.., alpha = ..level..), 
                 size = 0.0, bins = 200, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red", name="Postos de\n emprego em\n cada ponto") + 
  scale_alpha(range = c(0.05, 1), guide = FALSE)+
  labs(x = "", y = "")+
  geom_sf(data = sjc_viario) + 
  coord_sf(xlim = c(-46.10801, -45.72131), ylim = c(-23.30666, -22.81627))

sjc_municipio %>% ggplot() +
  geom_sf(fill = "#CCCCCC")+
  stat_density2d(data = pontos_sjc_df,aes(x = X, y = Y, fill = ..level.., alpha = ..level..), 
                 size = 0.0, bins = 200, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red", name=" Postos de\n emprego em\n cada ponto") + 
  scale_alpha(range = c(0.05, 1), guide = FALSE)+
  labs(x = "", y = "")+
  geom_sf(data = sjc_viario) + 
  coord_sf(xlim = c(-46.10801, -45.72131), ylim = c(-23.30666, -22.81627))+
  theme(legend.text.align = 1, legend.title.align = 0.5)

ggsave("sjc_mapa_calor_empregos.png", plot = last_plot(), 
       device = "png", 
       path = "C:/Users/taina/Google Drive/Trabalho/CEPESP/SJC/SJC_operacional/Equipe_financeiro_economico/Imagens")

sjc_municipio %>% ggplot() +
  geom_sf(fill = "#CCCCCC")+
  stat_density2d(data = pontos_sjc_df,aes(x = X, y = Y, fill = ..level.., alpha = ..level..), 
                 size = 0.0, bins = 200, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red", name=" Postos de\n emprego em\n cada ponto") + 
  scale_alpha(range = c(0.05, 1), guide = FALSE)+
  labs(x = "", y = "")+
  geom_sf(data = sjc_viario, linetype = "dotted") + #blank, solid, dashed, dotted, dotdash, longdash, twodash
  geom_sf(data = sjc_urbano, fill = NA, color = "#0000CC") +
  coord_sf(xlim = c(-46, -45.72131), ylim = c(-23.30666, -23.1))+
  theme(legend.text.align = 1, legend.title.align = 0.5)

ggsave("sjc_mapa_calor_empregos_mancha_urbana15.png", plot = last_plot(), 
       device = "png", 
       path = "C:/Users/taina/Google Drive/Trabalho/CEPESP/SJC/SJC_operacional/Equipe_financeiro_economico/Imagens")
#----------------------------------

# TOTAL DE TRABALHADORES EM CADA SUBZONA DE TRÁFEGO ####
sjc_municipio %>% ggplot() +
  geom_sf(fill = "#CCCCCC") +
  geom_sf(data = (sjc_zonas_trafego %>% filter(EMPREGOS > 0)), aes(fill = EMPREGOS)) +
  coord_sf()

# passar informacao das poligonos para a camada de pontos
intersection <- pontos_sjc %>% st_intersection(sjc_zonas_trafego) 

intersection <- intersection %>% 
  group_by(ID) %>% 
  summarise(trab = sum(trabalhadores)) %>% 
  st_set_geometry(NULL) %>% 
  as.data.frame()

sum(intersection$trab) == sum(pontos_sjc$trabalhadores) #TRUE eh bom

sjc_zonas_trafego <- sjc_zonas_trafego %>% left_join(intersection, by = "ID")

sjc_municipio %>% ggplot() +
  geom_sf(fill = "#CCCCCC") +
  geom_sf(data = (sjc_zonas_trafego %>% filter(trab > 0)), aes(fill = trab)) +
  coord_sf()

ggsave("sjc_mapa_empregos_zona_trafego.png", plot = last_plot(), 
       device = "png", 
       path = "C:/Users/taina/Google Drive/Trabalho/CEPESP/SJC/SJC_operacional/Equipe_financeiro_economico/Imagens")

summary(sjc_zonas_trafego$trab)

sjc_zonas_trafego %>% ggplot() + geom_histogram(aes(x = trab)) + ylab("postos de emprego")
sjc_zonas_trafego %>% filter(trab > 50) %>% ggplot() + geom_histogram(aes(x = trab)) + ylab("postos de emprego")
#--------------------------------

# TOTAL DE TRABALHADORES EM CADA ZONA DE TRÁFEGO ####
zonas_trafego <- sjc_zonas_trafego %>% mutate(contador = 1) %>%
  group_by(ZAT_55) %>% 
  summarise(area = sum(AREA), pop = sum(POP), emp = sum(EMPREGOS), sub_zonas = sum(contador))

zonas_trafego %>% ggplot() + geom_sf() + coord_sf()

# passar informacao das poligonos para a camada de pontos
intersection <- pontos_sjc %>% st_intersection(zonas_trafego)

intersection <- intersection %>% 
  group_by(ZAT_55) %>% 
  summarise(trab = sum(trabalhadores)) %>% 
  st_set_geometry(NULL) %>% 
  as.data.frame()

sum(intersection$trab) == sum(pontos_sjc$trabalhadores) #TRUE eh bom

zonas_trafego <- zonas_trafego %>% left_join(intersection, by = "ZAT_55")

# EXPORTAR SHAPE DE ZONAS DE TRAFEGO COM EMPREGOS ####
st_write(zonas_trafego, 
         dsn = "C:/Users/taina/Google Drive/Trabalho/CEPESP/SJC/SJC_operacional/Equipe_financeiro_economico/Dados/RAIS/zonas_trafego.shp")
#----------------------------------

# lat long da localização do CEP da Petrobras esta errado ####
#correto: -23.1867969,-45.8376855
#comom esta: -23.21247 -45.89189

pontos_sjc_df <- pontos_sjc_df %>% 
  mutate(X = ifelse(cep == 12223900, -45.8376855, X)) %>% 
  mutate(Y = ifelse(cep == 12223900, -23.1867969, Y))

sjc_municipio %>% ggplot() +
  geom_sf(fill = "#CCCCCC")+
  stat_density2d(data = pontos_sjc_df,aes(x = X, y = Y, fill = ..level.., alpha = ..level..), 
                 size = 0.0, bins = 200, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red", name=" Postos de\n emprego em\n cada ponto") + 
  scale_alpha(range = c(0.05, 1), guide = FALSE)+
  labs(x = "", y = "")+
  geom_sf(data = sjc_viario, linetype = "dotted") + #blank, solid, dashed, dotted, dotdash, longdash, twodash
  geom_sf(data = sjc_urbano, fill = NA, color = "#0000CC") +
  coord_sf(xlim = c(-46, -45.72131), ylim = c(-23.30666, -23.1))+
  theme(legend.text.align = 1, legend.title.align = 0.5)

ggsave("sjc_mapa_calor_empregos_mancha_urbana15_v2.png", plot = last_plot(), 
       device = "png", 
       path = "C:/Users/taina/Google Drive/Trabalho/CEPESP/SJC/SJC_operacional/Equipe_financeiro_economico/Imagens")
