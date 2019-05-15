# Titulo: Regioes
# Autora: Rebeca Carvalho


rm(list = ls())

# Pacotes utilizados

library(readr)
library(maptools)
library(rgdal)
library(sf)
library(dplyr)
library(sp)
library(rgeos)


# 1. Dados ----------------------------------------------------------------

# RAIS

load("RAIS/cep_taina.Rda")

load("RAIS/cep_miguel.Rda")

estab_2017 <- read_csv("RAIS/estab_2017.txt")
estab_2017$Ano <- 2017

ibge_sub <- read_delim("RAIS/ibge_sub.txt",";", escape_double = FALSE, trim_ws = TRUE)

# IBGE


basico <- read_delim("demograficos/Basico_SP2.txt", 
                         ";", escape_double = FALSE, locale = locale(), 
                         trim_ws = TRUE)

domicilio <-  read_delim("demograficos/Domicilio02_SP2.txt", 
                         ";", escape_double = FALSE, locale = locale(), 
                         trim_ws = TRUE)

renda_domicilio <- read_delim("demograficos/DomicilioRenda_SP2.txt", 
                              ";", escape_double = FALSE, trim_ws = TRUE)


# 2. Limpeza dos dados ----------------------------------------------------

# RAIS

estab_2017 <- estab_2017 %>%
  dplyr::select(Ano, `CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  dplyr::rename("Código" = "IBGE Subsetor" ,"Trabalhadores" = "trabalhadores")

estab_anos <- left_join(estab_2017, ibge_sub, by = "Código")

# IBGE

basico <- basico %>% 
  dplyr::filter(Cod_municipio == 3549904)

setor_cen <- basico$Cod_setor

domicilio <- domicilio %>% 
  dplyr::filter(Cod_setor %in% setor_cen) %>% 
  select(Cod_setor, V001) %>% 
  rename("População" = "V001")

renda_domicilio <- renda_domicilio %>% 
  dplyr::filter(Cod_setor %in% setor_cen) %>% 
  select(Cod_setor, V002) %>% 
  rename("Renda" = "V002")

# 3. Localizacao dos CEPS -------------------------------------------------

# RAIS

cep_taina <- cep_taina %>%
  rename("CEP Estab" = "cep")

cep_miguel <- cep_miguel %>%
  rename("CEP Estab" = "cep")

estab_anos <- left_join(estab_anos, cep_taina, by = "CEP Estab")

estab_anos <- estab_anos %>% 
  na.omit()

trab_por_cep <- estab_anos %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

macrozonas <- read_sf("RAIS/REGIOES_GEOGRAFICAS_REV12_2017.shp") %>%
  st_transform(4326)

macrozonas$regiao[macrozonas$regiao == "São Francisco Xavier"] <- "Extremo Norte"


pontos_sjc <- trab_por_cep %>% 
  st_intersection(st_buffer(macrozonas, 0))

rais <- pontos_sjc %>% 
  select(regiao, IBGE.Subsetor, Trabalhadores) %>% 
  rename("Região" = "regiao", "Subsetor" = "IBGE.Subsetor") %>% 
  na.omit()

st_geometry(rais) <- NULL


# IBGE

censitario <- read_sf("demograficos/setor_censitario_sjc.shp") %>% 
  st_transform(4326)

centroides <- censitario %>% st_centroid()

pontos_ <- censitario %>% 
  st_intersection(st_buffer(macrozonas, 0))

pontos_ <- pontos_ %>% 
  rename("Cod_setor" = "CD_GEOCODI")

pontos_$Cod_setor <- as.numeric(pontos_$Cod_setor)

censitario <- left_join(domicilio, renda_domicilio, by = "Cod_setor")


censitario <- left_join(censitario, pontos_, by = "Cod_setor")


censitario$V002 <- as.numeric(censitario$Renda) 

renda <- censitario %>% 
  select(População, Renda, regiao) %>% 
  rename("Região" = "regiao") %>% 
  na.omit()



# 4. Mapas ----------------------------------------------------------------


#censitario %>% ggplot() +geom_sf() + coord_sf()

#censitario %>% ggplot() +geom_sf() + geom_sf(data = centroides) + coord_sf()

#macrozonas %>% ggplot() +geom_sf(aes(fill = regiao)) + coord_sf()


#macrozonas <- read_sf("demograficos/REGIOES_GEOGRAFICAS_REV12_2017.shp") %>% st_transform(4326)

#macrozonas %>% ggplot() +geom_sf(aes(fill = regiao)) + coord_sf()

#limite_mun <- read_sf("demograficos/LIMITE_MUNICIPAL.shp") %>% st_transform(4326)

#limite_mun %>% ggplot() + geom_sf() + geom_sf(data = macrozonas) + coord_sf()

#macrozonas <- macrozonas %>% filter(regiao != "São Francisco Xavier")

#extremo_norte <- limite_mun %>% st_difference(macrozonas)



# 5. Arquivo ------------------------------------------------------------------

write.csv(rais, "rais.csv")
write.csv(renda, "renda.csv")
