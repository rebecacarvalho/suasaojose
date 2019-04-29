
# Titulo: Dados SJC
# Autor: Rebeca Carvalho

rm(list = ls())

# Pacotes utilizados

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(maptools)
library(rgdal)
library(sf)



# 1. Dados ----------------------------------------------------------------

# Secao para importacao dos dados

# 1.1. Dados demograficos -------------------------------------------------

Basico_SP2 <- read_delim("dados demográficos/Basico_SP2.txt", 
                         ";", escape_double = FALSE, locale = locale(), 
                         trim_ws = TRUE)

Domicilio01_SP2 <- read_delim("dados demográficos/Domicilio01_SP2.txt", 
                              ";", escape_double = FALSE, 
                              trim_ws = TRUE)

Domicilio02_SP2 <- read_delim("dados demográficos/Domicilio02_SP2.txt", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

DomicilioRenda_SP2 <- read_delim("dados demográficos/DomicilioRenda_SP2.txt", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

Entorno01_SP2 <- read_delim("dados demográficos/Entorno01_SP2.txt", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

Entorno02_SP2 <- read_delim("dados demográficos/Entorno02_SP2.txt", 
                            ";", escape_double = FALSE, trim_ws = TRUE)

Entorno03_SP2 <- read_delim("dados demográficos/Entorno03_SP2.txt", 
                            ";", escape_double = FALSE, trim_ws = TRUE)

Entorno04_SP2 <- read_delim("dados demográficos/Entorno04_SP2.txt", 
                            ";", escape_double = FALSE, trim_ws = TRUE)

Entorno05_SP2 <- read_delim("dados demográficos/Entorno05_SP2.txt", 
                            ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa01_SP <- read_delim("dados demográficos/Pessoa01_SP.txt", 
                            ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa02_SP <- read_delim("dados demográficos/Pessoa02_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa03_SP <- read_delim("dados demográficos/Pessoa03_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa04_SP <- read_delim("dados demográficos/Pessoa04_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa05_SP <- read_delim("dados demográficos/Pessoa05_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa06_SP <- read_delim("dados demográficos/Pessoa06_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa07_SP <- read_delim("dados demográficos/Pessoa07_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa08_SP <- read_delim("dados demográficos/Pessoa08_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa09_SP <- read_delim("dados demográficos/Pessoa09_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa10_SP <- read_delim("dados demográficos/Pessoa10_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa11_SP <- read_delim("dados demográficos/Pessoa11_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa12_SP <- read_delim("dados demográficos/Pessoa12_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa13_SP <- read_delim("dados demográficos/Pessoa13_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

PessoaRenda_SP2 <- read_delim("dados demográficos/PessoaRenda_SP2.txt", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

responsavel01_sp2 <- read_delim("dados demográficos/responsavel01_sp2.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

responsavel02_sp2 <- read_delim("dados demográficos/responsavel02_sp2.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

ResponsavelRenda_SP2 <- read_delim("dados demográficos/ResponsavelRenda_SP2.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)


# 1.2. Dados escolares ----------------------------------------------------



matriculasES2011 <- read_delim("censo escolar/Matrículas Ensino Superior 2011/matriculasES2011.txt", 
                               ";", escape_double = FALSE, trim_ws = TRUE)

matriculasES2016 <- read_delim("censo escolar/Matrículas Ensino Superior - 2016/matriculasES2016.txt", 
                                      ";", escape_double = FALSE, trim_ws = TRUE)


matriculasEB2013 <- read_delim("censo escolar/Matrículas Ensino Básico - 2013/matriculasEB2013.txt", 
                               ";", escape_double = FALSE, trim_ws = TRUE)


matriculasEB2018 <- read_delim("censo escolar/Matrículas Ensino Básico - 2018/matriculasEB2018.txt", 
                               ";", escape_double = FALSE, trim_ws = TRUE)


# 1.3. RAIS ---------------------------------------------------------------


estab_2010 <- read_csv("RAIS/estab_2010.txt")

estab_2011 <- read_csv("RAIS/estab_2011.txt")

estab_2012 <- read_csv("RAIS/estab_2012.txt")

estab_2013 <- read_csv("RAIS/estab_2013.txt")

estab_2014 <- read_csv("RAIS/estab_2014.txt")

estab_2015 <- read_csv("RAIS/estab_2015.txt")

estab_2016 <- read_csv("RAIS/estab_2016.txt")

estab_2017 <- read_csv("RAIS/estab_2017.txt")



# 1.4. Pesquisa OD --------------------------------------------------------



domicilios <- read_delim("dados OD/domicilios.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)

pessoas <- read_delim("dados OD/pessoas.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

viagens <- read_delim("dados OD/viagens.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)

zonas <- read_delim("dados OD/zonas.txt", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)



# 1.5. Linhas -------------------------------------------------------------



linhas <- read_delim("linhas/LInhas.txt", ";", 
                     escape_double = FALSE, col_types = cols(X5 = col_skip()), 
                     trim_ws = TRUE)






# 2. Limpeza e organizacao dos dados --------------------------------------


# Secao para padronizacao e limpeza dos dados importados


# 2.1. Dados demograficos -------------------------------------------------


Basico_SP2$V002 <- as.numeric(Basico_SP2$V002)


glimpse(Basico_SP2)

# 2.2. Dados escolares ----------------------------------------------------


# 2.3. RAIS ---------------------------------------------------------------


# 2.4. Pesquisa OD --------------------------------------------------------


zonas <- rename(zonas, "ZONA" = "Zona", "DENOMINAÇÃO" = "Denominação", 
                "LOCALIZAÇÃO" = "Localização")


od <- left_join(pessoas,domicilios, by = c("CHAVE DOM", "ZONA", "DOMIC", 
                                           "NUM_FAM"))

od <- left_join(viagens, od, by = c("CHAVE DOM + PESS", "CHAVE DOM", "ZONA", 
                                    "DOMIC", "NUM_FAM", "NUM_PESS", "FAT_EXP POP"))

od$ZONA <- as.numeric(od$ZONA)

od <- left_join(od, zonas, by = "ZONA")

od <- od %>% 
  select(`CHAVE DOM + PESS`, `CHAVE DOM`, ZONA,DENOMINAÇÃO, LOCALIZAÇÃO, MACROZONA, 
         DOMIC, NUM_FAM, TOT_PESS, NUM_PESS, SIT_FAM, SEXO,IDADE, GR_INSTR, COND_ATV,
         VINCULO, SET_ATV, ZONA_ATV, `MACROZONA_ATV DESCRIÇÃO`, COND_AT2, VINCULO2, 
         SET_ATV2, ZONA_AT2,`MACROZONA_ATV2 DESCRIÇÃO`,RENDA, `FAT_EXP POP`, TIPO_IMÓVEL,
         Nº_CÔMODOS, ESGOTO, AGUA, CONSUMO, RADIO, TV, MAQ_LAV,GELADEIR, CELULAR, TEL_FIXO,MICRO,
         MICRO_IN, MOTO, AUTO, QTE_FAM, TIPO_DOM, COND_MOR, TMP_CID, TMP_DOM, `FAT_EXP DOM`, NUM_VIAGEM,
         INTEGRAÇÃO, Viagem, O_LOCAL, O_ZONA, `MACROZONA ORIG`, O_MOTIVO, `HORA SAIDA`, D_LOCAL, D_ZONA,
         `MACROZONA DEST`, D_MOTIVO,`HORA CHEGA`, MOD_TRA, MODO_PRONCIPAL, CLASSE, TIPO, TMP_APEO, TMP_APED,
         FORM_PAG, ESTACION, `FAT_EXP POP`, `FAT_EXP GERAL`)




# 2.5. Linhas -------------------------------------------------------------

linhas$NOME <- str_to_title(linhas$NOME) 

linhas <- rename(linhas, "Nome" = "NOME")

# 3. Tabelas  -------------------------------------------------------------


# 3.1. Dados demograficos -------------------------------------------------

Basico_SP2 <- Basico_SP2 %>% 
  dplyr::filter(Cod_municipio == 3549904)

setor_cen <- Basico_SP2$Cod_setor

pop <- Domicilio02_SP2 %>% 
  filter(Cod_setor %in% setor_cen) 

demografia <- data.frame(Demografia = c("População", "População (%)", "Área da macrozona (km²)",
                                        "Área da macrozona (%)", "Densidade demográfica (hab/km²"), 
                                        Centro = "", Sul = "", Leste = "", Oeste = "", Norte = "", 
                                        Sudeste = "",`Extremo Norte` = " ", Município = c(sum(pop$V001), " 100", " 2", "2", "2"))


#demografia$Município <- format(round(as.numeric(demografia$Município[1]), 1), big.mark=".")


bairro <- Basico_SP2 %>% 
  dplyr::group_by(Cod_setor, Nome_do_bairro) %>% 
  summarise(
    n = n())

bairro <- left_join(bairro, pop, by = "Cod_setor") 

bairro <- bairro %>% 
  select(Cod_setor, Nome_do_bairro, Situacao_setor, V001) %>% 
  group_by(Nome_do_bairro) %>% 
  summarise(
    soma = sum(V001)
  )


t <- as.data.frame(rgdal::readOGR("dados demográficos/setor_censitario_sjc.shp"))

t2 <- as.data.frame(sf::st_read("dados demográficos/setor_censitario_sjc.dbf"))

t3 <- as.data.frame(sf::st_read("dados demográficos/setor_censitario_sjc.shx"))


# 3.2. Dados escolares ----------------------------------------------------

matriculas <- data.frame(Ano = c("2013","2018"), `Nível de Ensino`= c("Ensino Básico", "Ensino Superior"), 
                         Centro = "", Sul = "", Leste = "", Oeste = "", Norte = "", Sudeste = "",
                         `Extremo Norte` = " ", Município = c(156496,30852 ))                                                                                                                            





count(matriculasEB2018)
count(matriculasES2016)

# 3.3. RAIS ---------------------------------------------------------------

empregos <- data.frame(Ano = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"), `Administração pública` = "", Agricultura = "",
                       `Alimentos e bebidas` = "", Comércio = "", `Construção civil` = "",
                       Educação = "", Serviços = " ", `Transporte e comunicação` = "")

table(estab_2010$`SUBS IBGE`)

estab_2010 <- estab_2010 %>% 
  
  
?recode

# 3.4. Pesquisa OD --------------------------------------------------------



od2 <- od %>%
  group_by(CLASSE, TIPO) %>% 
  count()
 
od2 <- left_join(od, od2, by = c("CLASSE", "TIPO"))

od2$EXPAN <- od2$n * od2$`FAT_EXP GERAL`

c1 <- od2 %>% 
  filter(CLASSE == "Motorizado" & TIPO == "Individual")

mean(c1$EXPAN)

c2 <- od2 %>% 
  filter(CLASSE == "Motorizado" & TIPO == "Coletivo")

mean(c2$EXPAN)

c3 <- od2 %>% 
  filter(CLASSE == "Não Motorizado" & TIPO == "Não Motorizado")

mean(c3$EXPAN)


od2$`FAT_EXP GERAL` <- od2$`FAT_EXP GERAL`/100

cat_transp <- data.frame(Categorias = c("Transporte individual e não-motorizado", 
                                        "Transporte individual e motorizado", "Transporte coletivo"),
                         Viagens = as.numeric(c("421.943", "764.043", "461.307")), Participação = " ")

cat_transp$Total <- sum(cat_transp$Viagens)

cat_transp$Participação <- round(cat_transp$Viagens * 100/cat_transp$Total, 2) 

cat_transp <- cat_transp %>% 
  select(Categorias, Viagens, Participação)



