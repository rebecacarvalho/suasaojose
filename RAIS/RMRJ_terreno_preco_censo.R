# lista de tarefas ----
# terrenos + censo

#Fazer um join entre o shp do KML e a planilha resumo com as informações dos terrenos
#Gerar centróides de cada terreno já com as informações
#Abrir dados do censo e selecionar as seguintes informações 
  # densidade
  # número de pessoas
  # renda média e mediana
  # luz
  # pavimentação
  # água
  # esgoto

#SABESP: custo expansão da rede
#YUMI: ver custo da AES
  

# terrenos + preço da terra
# Fazer um grid na mesma área dos pontos que receberam valores da krigagem
# fazer um intersection entre os pontos e os polígonos para jogar valor dos pontos nos polígonos
# gerar um centróide para cada terreno
# fazer um intersection entre os centróides e os polígonos do preço da terra
# gerar variável de preço do terreno
# ver quanto o preço do terreno significou do total contratado

### DIRETORIOS E AMBIENTE ####
remove(list=ls()) #limpar global environment
setwd("C:/Users/taina/Dropbox/Trabalho/CEPESP/Lincoln_MCMV/")
#-------------------------------

### PACOTES ####
library(sf)
library(tidyverse)
ifelse(!require(readxl),install.packages("readxl"),library(readxl)) #para ler excel
ifelse(!require(lwgeom),install.packages("lwgeom"),library(lwgeom))
#----------------------------------

# 1 - Fazer um join entre o shp do KML e a planilha resumo com as informações dos terrenos -------
# abrir shp do kml
terrenos <- read_sf("Shapes_rasters/rmrj_kml.shp") %>% rename(nome = Name) %>% mutate(nome = as.numeric(nome))

base <- read_excel("empreendimentos_RMRJ_terrenos.xlsx", sheet = "base") %>% rename(nome = oper_mae)

terrenos <- terrenos %>% left_join(base, by = "nome")

remove(base)
#----------------------------------

# 2 - Gerar centróides de cada terreno já com as informações ----
centroides <- terrenos %>% st_centroid()
#----------------------------------

# 3 - #Abrir dados do censo e selecionar as seguintes informações -----
#densidade e populacao: ----- 
  # a) shape dos setores censitarios;
setores <- read_sf("Censo_2010/33SEE250GC_SIR.shp")

RM_brasil <- read_excel("Censo_2010/RMs_RIDEs_AglomUrbanas_2010_07_31.xls")
rmrj <- RM_brasil %>% filter(rm == "RM Rio de Janeiro") %>% dplyr::select(cod, nome)
RM_rj_agrupar <- data.frame(cod = c("3300803", "3304300"), nome = c("CACHOEIRAS DE MACACU", "RIO BONITO"))
rmrj <- rbind(rmrj, RM_rj_agrupar)
rmrj <- rmrj[["cod"]]
remove(RM_rj_agrupar, RM_brasil)

# criar vetor com os setores sencitarios de interesse
setores <- setores %>% 
  filter(CD_GEOCODM %in% rmrj)

vetor_setores <- setores[["CD_GEOCODI"]]

#setores %>% ggplot() + geom_sf() + geom_sf(data = centroides, color = "red", size = 2) + coord_sf()

  # b) calcular area;
setores <- setores %>%
  mutate(area = st_area(setores))

setores <- setores %>% dplyr::select(CD_GEOCODI, CD_GEOCODM, area)

  # c) planilha Domicilio02_UF variavel V01 (moradores domocilios particulares e coletivos)
domicilios <- read_delim("Censo_2010/Universo/RJ/Base informacoes setores2010 universo RJ/CSV/Domicilio02_RJ.csv", 
                         ";", escape_double = FALSE, col_types = cols(Cod_setor = col_character()), trim_ws = TRUE)

domicilios <- domicilios %>% 
  filter(Cod_setor %in% vetor_setores) %>% 
  dplyr::select(Cod_setor, V001) %>% 
  rename(CD_GEOCODI = Cod_setor, populacao = V001)

setores <- setores %>% left_join(domicilios, by = "CD_GEOCODI")
setores <- setores %>% 
  mutate(populacao = ifelse(is.na(populacao),0, populacao),
         area_km = area/1000000,
         densidade = populacao/area_km,
         dens_media = sum(populacao)/sum(area_km))
                              
summary(setores$densidade)

#renda media e mediana: -----
# a) planilha Basico_RJ variavel:
# V001 (pessoas responsáveis por domicílios particulares permanentes)
# V005 (Valor do rendimento nominal médio mensal das pessoas responsáveis 
  #por domicílios particulares permanentes (com e sem rendimento))
# V006: Variância do rendimento nominal mensal das pessoas responsáveis 
  #por domicílios particulares permanentes (com e sem rendimento)
rendimento <- read_delim("Censo_2010/Universo/RJ/Base informacoes setores2010 universo RJ/CSV/Basico_RJ.csv", 
                         ";", escape_double = FALSE, col_types = cols(Cod_setor = col_character()), 
                         locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "WINDOWS-1252"), 
                         trim_ws = TRUE)

rendimento <- rendimento %>% dplyr::select(Cod_setor, V001, V005, V006) %>%
  rename(CD_GEOCODI = Cod_setor, pess_responsaveis = V001, renda = V005, variancia_renda = V006)

setores <- setores %>% left_join(rendimento, by = "CD_GEOCODI")

setores <- setores %>% 
  mutate(renda_media = as.numeric(sum(renda*pess_responsaveis, na.rm = T)/sum(pess_responsaveis, na.rm = T)))

setores <- setores %>% mutate(decil_renda = ntile(renda, 10))

#iluminacao, pavimentacao e calcada: -----
# porcentagem dos domicilios particuares permanetes que possuem iluminacao no SC
# V001: domicilios particulares permanentes
# V008 + V010 + V012: total de domicilios particulares permanentes com iluminacao publica
# V014 + V016 + V018: total de domicilios particulares permanentes com pavimentacao
# V020 + V022 + V024: total de domicilios particulares permanentes com calcada

entorno <- read_delim("Censo_2010/Universo/RJ/Base informacoes setores2010 universo RJ/CSV/Entorno01_RJ.csv", 
                         ";", escape_double = FALSE, col_types = cols(Cod_setor = col_character()), 
                         locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "WINDOWS-1252"), 
                         trim_ws = TRUE)

entorno <- entorno %>% 
  dplyr::select(Cod_setor, V001, V008, V010, V012, V014, V016, V018, V020, V022, V024) %>%
  rename(CD_GEOCODI = Cod_setor, dpp = V001) %>%
  mutate(dpp = as.numeric(dpp), 
         V008 = as.numeric(V008), 
         V010 = as.numeric(V010), 
         V012 = as.numeric(V012), 
         V014 = as.numeric(V014), 
         V016 = as.numeric(V016), 
         V018 = as.numeric(V018), 
         V020 = as.numeric(V020), 
         V022 = as.numeric(V022), 
         V024 = as.numeric(V024)) %>%
  mutate(dpp_iluminacao = V008 + V010 + V012,
         dpp_pavimentacao = V014 + V016 + V018,
         dpp_calcada = V020 + V022 + V024) %>%
  dplyr::select(CD_GEOCODI, dpp, dpp_iluminacao, dpp_pavimentacao, dpp_calcada) %>%
  mutate(dpp_iluminacao_pct = dpp_iluminacao/dpp,
         dpp_pavimentacao_pct = dpp_pavimentacao/dpp,
         dpp_calcada_pct = dpp_calcada/dpp)

setores <- setores %>% left_join(entorno, by = "CD_GEOCODI")

sum(entorno$dpp_pavimentacao, na.rm = T)
sum(entorno$dpp, na.rm = T)

#lixo, agua e esgoto ----
# planilha Domicilio01_RJ
# V001: dpp
# V012: Domicílios particulares permanentes com abastecimento de água da rede geral
# V016: Domicílios particulares permanentes com banheiro de uso exclusivo dos moradores ou sanitário
# V017: Domicílios particulares permanentes com banheiro de uso exclusivo dos moradores ou sanitário e 
  #esgotamento sanitário via rede geral de esgoto ou pluvial
# V018: Domicílios particulares permanentes com banheiro de uso exclusivo dos moradores ou sanitário e 
  #esgotamento sanitário via fossa séptica

domicilio01 <- read_delim("Censo_2010/Universo/RJ/Base informacoes setores2010 universo RJ/CSV/Domicilio01_RJ.csv", 
                      ";", escape_double = FALSE, col_types = cols(Cod_setor = col_character()), 
                      locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "WINDOWS-1252"), 
                      trim_ws = TRUE)

domicilio01 <- domicilio01 %>% 
  dplyr::select(Cod_setor, V002, V012, V016, V017, V018) %>%
  rename(CD_GEOCODI = Cod_setor, dpp = V002, dpp_agua = V012) %>%
  mutate(dpp = as.numeric(dpp), 
         dpp_agua = as.numeric(dpp_agua), 
         dpp_banheiro = as.numeric(V016), 
         dpp_banheiro_fossa = as.numeric(V017), 
         dpp_banheiro_rede = as.numeric(V018),
         dpp_banheiro_total = dpp_banheiro_fossa + dpp_banheiro_rede,
         dpp_agua_pct = dpp_agua/dpp,
         dpp_banheiro_pct = dpp_banheiro/dpp,
         dpp_banheiro_total_pct = dpp_banheiro_total/dpp) %>%
  dplyr::select(-V016, -V017, -V018)

setores <- setores %>% left_join(domicilio01, by = "CD_GEOCODI")

# passar informacoes para os pontos dos empreendimentos ----
setores <- setores %>% st_set_crs("+init=epsg:4326")

centroides <- centroides %>% st_intersection(setores)
#View(centroides)

centroides <- centroides %>% mutate(erro = ifelse(is.na(renda), 1, 0))

#setores %>% ggplot() + geom_sf(color = NA) + geom_sf(data = centroides, aes(color = erro)) + coord_sf()

#exportar shp de centroides e setores para ver porque deu erro ----
#write_sf(centroides, dsn = "C:/Users/taina/Dropbox/Trabalho/CEPESP/Lincoln_MCMV/Shapes_rasters/centroides_rj.shp")
#write_sf(setores, dsn = "C:/Users/taina/Dropbox/Trabalho/CEPESP/Lincoln_MCMV/Shapes_rasters/setores_rj_infos.shp")

#summary(centroides)

# o problema com o join acontece porque alguns conjuntos ficaram em setores censitarios que nao tem informacoes.
# olhando no QGIS peguei o numero dos setores censitarios mais proximos aos empreendimentos
filtrar_setores <- c("330455705240501",
                     "330455705230425", 
                     "330455705240092", 
                     "330455705230421", 
                     "330455705230261", 
                     "330455705210520", 
                     "330455705210558", 
                     "330455705080110",
                     "330455705230590")

centroides_erro <- setores %>% 
  filter(CD_GEOCODI  %in% filtrar_setores) %>% 
  mutate(ID = case_when(CD_GEOCODI == "330455705240501" ~ 9,
                        CD_GEOCODI == "330455705230425" ~ 31,
                        CD_GEOCODI == "330455705240092" ~ 48,
                        CD_GEOCODI == "330455705230421" ~ 4,
                        CD_GEOCODI == "330455705230261" ~ 5,
                        CD_GEOCODI == "330455705210520" ~ 57,
                        CD_GEOCODI == "330455705210558" ~ 45,
                        CD_GEOCODI == "330455705080110" ~ 38,
                        CD_GEOCODI == "330455705230590" ~ 21))

centroides_unir <- centroides %>% filter(erro == 1 | ID == 21) %>% 
  dplyr::select(nome                           
                ,Dscrptn                       
                ,ID                            
                ,cod_munic                     
                ,operacoes                     
                ,uh_contratada                 
                ,uh_concluida                  
                ,uh_entregue                   
                ,area_m2                       
                ,valor_contratado              
                ,ano_contrato                  
                ,ano_entrega                   
                ,metragem_minima               
                ,custo_construcao              
                ,custo_unidade                 
                ,custo_todas_UH                
                ,cust_UH_menor_valor_contratado
                ,repasse_sobre                 
                ,custo_terra_m2                
                ,custo_terreno) 

centroides_unir <- centroides_unir %>%
  st_set_geometry(NULL) %>%
  as.data.frame()

centroides_erro <- centroides_erro %>% left_join(centroides_unir, by = "ID")

centroides_erro <- centroides_erro %>% mutate(erro = 1)                

centroides <- centroides %>% filter(erro == 0) %>% filter(ID != 21) %>% rbind(centroides_erro)

remove(centroides_erro, centroides_unir)

# problema corrigido, exportar shape final de centroides e df de centroides ----
write_sf(centroides, dsn = "C:/Users/taina/Dropbox/Trabalho/CEPESP/Lincoln_MCMV/Shapes_rasters/centroides_rj_final.shp")

centroides_df <- centroides %>% st_set_geometry(NULL) %>% as.data.frame()

write.csv(centroides_df, file = "C:/Users/taina/Dropbox/Trabalho/CEPESP/Lincoln_MCMV/Resultados/centroides_rj_df.csv")

# estatisticas descritivas ----
estat_descritivas <- centroides_df %>%
  dplyr::select(dpp.x, dpp_iluminacao, dpp_pavimentacao, dpp_agua, 
                dpp_banheiro_fossa, area, populacao, renda, decil_renda)

estat_descritivas <- estat_descritivas %>%
  mutate(luz_pct = (dpp.x - dpp_iluminacao)/dpp.x,
         pav_pct = (dpp.x - dpp_pavimentacao)/dpp.x,
         agu_pct = (dpp.x - dpp_agua)/dpp.x,
         esg_pct = (dpp.x - dpp_banheiro_fossa)/dpp.x)

estat_descritivas <- estat_descritivas %>%
  dplyr::select(luz_pct, pav_pct, agu_pct, esg_pct, area, populacao, renda, decil_renda)

media <- sapply(estat_descritivas, mean, na.rm=TRUE)
maximo <- sapply(estat_descritivas, max, na.rm=TRUE)
minimo <- sapply(estat_descritivas, min, na.rm=TRUE)
sd <- sapply(estat_descritivas, sd, na.rm=TRUE)
mediana <- sapply(estat_descritivas, median, na.rm=TRUE)

descritivas <- rbind(media, maximo, minimo, sd, mediana) %>% as.data.frame()

write.csv(descritivas, file = "C:/Users/taina/Dropbox/Trabalho/CEPESP/Lincoln_MCMV/Resultados/descritivas_rj.csv")
