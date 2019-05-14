# Titulo: Regioes
# Autora: Rebeca Carvalho


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

renda_domicilio <- renda_domicilio %>% 
  dplyr::filter(Cod_setor %in% setor_cen) 

# 3. Localizacao dos CEPS -------------------------------------------------

# RAIS

cep_taina <- cep_taina %>%
  rename("CEP Estab" = "cep")

cep_miguel <- cep_miguel %>%
  rename("CEP Estab" = "cep")

estab_anos <- left_join(estab_anos, cep_taina, by = "CEP Estab")

na <- estab_anos %>% 
  dplyr::filter(is.na(latitude))

estab <- left_join(na,cep_miguel, by = c("CEP Estab", "latitude", "longitude"))

estab_anos <- estab_anos %>% 
  na.omit()

trab_por_cep <- estab_anos %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

macrozonas <- read_sf("RAIS/REGIOES_GEOGRAFICAS_REV12_2017.shp") %>%
  st_transform(4326)

pontos_sjc <- trab_por_cep %>% 
  st_intersection(macrozonas)

rais <- pontos_sjc %>% 
  group_by(IBGE.Subsetor, regiao) %>%
  count()

# IBGE


censitario <- read_sf("demograficos/setor_censitario_sjc.shp") %>% 
  st_transform(4326)

centroides <- censitario %>% st_centroid()

pontos_ <- censitario %>% 
  st_intersection(st_buffer(macrozonas, 0))

pontos_ <- pontos_ %>% 
  rename("Cod_setor" = "CD_GEOCODI")

pontos_$Cod_setor <- as.numeric(pontos_$Cod_setor)

censitario <- left_join(renda_domicilio, pontos_, by = "Cod_setor") 


censitario$V002 <- as.numeric(censitario$V002) 

censitario <- censitario %>% 
  select(V002,regiao) %>% 
  rename("Renda" = "V002", "Região" = "regiao") %>% 
  na.omit()


renda <- censitario %>% 
  group_by(Região) %>% 
 summarise(
   n = n(),
   soma = sum(Renda),
   media = soma/n
 )

table(censitario$regiao)



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


