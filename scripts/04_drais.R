# Pacotes utilizados

library(dplyr)
library(tidyverse)


# Objetivo
#'        - Limpar e padronizar os dados;
#'        - Organizar e calcular alguns indicadores
#'        - referentes ao setor empregaticio do municipio de 
#'        - Sao Jose dos Campos.



# 1. Reorganizacao --------------------------------------------------------------

## Reorganiza os setores economicos da rais

rais$Setor <- NA
rais$Setor[rais$Subsetor == "Agricultura, silvicultura, criaçao de animais, extrativismo vegetal"] <- "Agricultura"
rais$Setor[rais$Subsetor == "Indústria de produtos alimentícios, bebidas e álcool etílico"] <- "Indústria"
rais$Setor[rais$Subsetor == "Serviços industriais de utilidade pública"] <- "Indústria"
rais$Setor[rais$Subsetor == "Ind. química de produtos farmacêuticos, veterinários, perfumaria"] <- "Indústria"
rais$Setor[rais$Subsetor == "Indústria têxtil do vestuário e artefatos de tecidos"] <- "Indústria"
rais$Setor[rais$Subsetor == "Com. e administraçao de imóveis, valores mobiliários, serv. Técnico"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Comércio varejista"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Ensino"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Instituiçoes de crédito, seguros e capitalizaçao"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Serviços industriais de utilidade pública"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Transportes e comunicaçoes"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Comércio atacadista"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Construçao civil"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Serv. de alojamento, alimentaçao, reparaçao, manutençao, redaçao"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Serviços médicos, odontológicos e veterinários"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Administraçao pública direta e autárquica"] <- "Administração pública"



# 2. Calculo de indicador -------------------------------------------------

## Calcula a quantidade de empregos por setor de atividade e macrozona

rais <- rais %>% 
 group_by(Região, Setor) %>%
  summarise(
    Trabalhadores = sum(Trabalhadores))
  



# 3. Junta as bases -------------------------------------------------------

shape2 <- left_join(shape, matriculas)

shape2 <- rbind.fill(shape2, shape)

shape2 <- shape2[-c(13, 15, 17,18,19), ]  


shape2 <- full_join(shape2, rais, by = "Região")

shape2 <- rbind.fill(shape2, shape)

shape2 <- shape2[-c(53, 55, 56,58,59), ]

shape2 <- rbind.fill(shape2, shape)

shape2 <- shape2[-c(55, 57, 58,60,61), ]


# 4. Salva o arquivo ------------------------------------------------------

## Salva o arquivo 'rais' em .csv

write_sf(shape2, "data/shapes/shape.shp")

rm(rais, matriculas, shape, shape2)
