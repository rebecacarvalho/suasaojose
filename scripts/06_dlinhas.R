# Pacotes utilizados

library(dplyr)
library(tidyverse)


# Objetivo
#'        - Limpar e padronizar os dados;
#'        - Organizar e calcular alguns indicadores
#'        - referentes as linhas de onibus do municipio de 
#'        - Sao Jose dos Campos.



# 1. Reorganizacao e limpeza ----------------------------------------------

## Renomeia as colunas e padroniza-as em caixa alta p/somente a primeira letra
## da palavra

linhas$NOME <- str_to_title(linhas$NOME) 

linhas <- dplyr::rename(linhas, 
                        "Nome" = "NOME")


rotas <- rotas %>% 
  rename("Código" = "route_shor", 
         "Nome" = "route_long")

rotas$Nome <- str_to_title(rotas$Nome)

nrotas <- anti_join(rotas, 
                    linhas, 
                    by = "Código")

linhas <- full_join(linhas, 
                    nrotas, 
                    by = c("Código", 
                           "Nome"))

linhas <- linhas %>% 
  select(Código, 
         Nome, 
         Empresa, 
         CE)

## Reclassifica os nomes das linhas

linhas$Nome[linhas$Nome == "Jd. Das Indãšstrias / OlãMpio Catãƒo"] <- "Jd. das Indústrias / Olímpio Catão"
linhas$Nome[linhas$Nome == "ChãCaras Havaã / Av. Eng. Francisco Josã‰ Longo"] <- "Chácaras Havaí / Av. Eng. Francisco José Longo"
linhas$Nome[linhas$Nome == "Jd. Portugal / OlãMpio Catãƒo"] <- "Jd. Portugal / Olímpio Catão"
linhas$Nome[linhas$Código == "Alt 20"] <- "Colorado / Olímpio Catão"
linhas$Nome[linhas$Nome == "Ch. Reunidas / OlãMpio Catãƒo"] <- "Ch. Reunidas / Olímpio Catão"
linhas$Nome[linhas$Nome == "Jd. Morumbi / OlãMpio Catãƒo"] <- "Jd. Morumbi / Olímpio Catão"              
linhas$Nome[linhas$Nome == "Michigan / Av. Eng. Francisco Josã‰ Longo"] <- "Michigan / Av. Eng. Francisco José Longo"
linhas$Nome[linhas$Nome == "Canindu / Av. Eng. Francisco Josã‰ Longo"] <- "Canindu / Av. Eng. Francisco José Longo"
linhas$Nome[linhas$Nome == "Terras Do Sul / OlãMpio Catãƒo"] <- "Terras do Sul / Olímpio Catão"
linhas$Nome[linhas$Nome == "Jd. Guimarãƒes / Centro"] <- "Jd. Guimarães / Centro"


linhas$Empresa[linhas$Código == "Alt 10"] <- "Alternativo"
linhas$Empresa[linhas$Código == "Alt 11"] <- "Alternativo"
linhas$Empresa[linhas$Código == "Alt 14"] <- "Alternativo"
linhas$Empresa[linhas$Código == "Alt 15"] <- "Alternativo"
linhas$Empresa[linhas$Código == "Alt 20"] <- "Alternativo"
linhas$Empresa[linhas$Código == "Alt 21"] <- "Alternativo"
linhas$Empresa[linhas$Código == "Alt 24"] <- "Alternativo"
linhas$Empresa[linhas$Código == "Alt 25"] <- "Alternativo"
linhas$Empresa[linhas$Código == "Alt 28"] <- "Alternativo"
linhas$Empresa[linhas$Código == "Alt 30"] <- "Alternativo"
linhas$Empresa[linhas$Código == "Alt 31"] <- "Alternativo"
linhas$Empresa[linhas$Código == "Alt 32"] <- "Alternativo"
linhas$Empresa[linhas$Código == "Alt 33"] <- "Alternativo"
linhas$Empresa[linhas$Código == "Alt 38"] <- "Alternativo"

linhas$CE[linhas$Código == "Alt 10"] <- "4"
linhas$CE[linhas$Código == "Alt 11"] <- "4"
linhas$CE[linhas$Código == "Alt 14"] <- "4"
linhas$CE[linhas$Código == "Alt 15"] <- "4"
linhas$CE[linhas$Código == "Alt 20"] <- "4"
linhas$CE[linhas$Código == "Alt 21"] <- "4"
linhas$CE[linhas$Código == "Alt 24"] <- "4"
linhas$CE[linhas$Código == "Alt 25"] <- "4"
linhas$CE[linhas$Código == "Alt 28"] <- "4"
linhas$CE[linhas$Código == "Alt 30"] <- "4"
linhas$CE[linhas$Código == "Alt 31"] <- "4"
linhas$CE[linhas$Código == "Alt 32"] <- "4"
linhas$CE[linhas$Código == "Alt 33"] <- "4"
linhas$CE[linhas$Código == "Alt 38"] <- "4"

linhas <- unique(linhas)

rotas <- rotas %>% 
  select(fid, 
         Código, 
         direction_, 
         geometry)


lrotas <- left_join(linhas,
                    rotas, 
                    by = c("Código"))


lrotas <- subset(lrotas, 
                 !duplicated(subset(lrotas, 
                                    select=c(Código))))

lrotas <- na.omit(lrotas)



## Cria uma tabela que gerara o grafico 'bola'

linhas2 <- data.frame(
  ids = c(
    "CS Brasil", "Expresso Maringá", "Saens Peña", "Alternativo", 
    "CS Brasil - 103", "104", "108", "112", "116", "118", "124", "200", 
    "201", "202", "205", "211", "215","216", "222", "225", "231", "232",
    "237", "240", "242", "243", "244", "245", "246", "250", "251", "130A",
    "130B", "204A", "204B", "206A", "CS Brasil - 206B","Expresso Maringá - 208", 
    "209", "210", "212", "219", "229", "252", "302", "305", "307", "310", "315", "316",
    "317", "318", "319", "322","323", "325", "330", "333", "334", "335", "342", "343", 
    "344", "345", "347", "349","350", "355", "308A", "308OF", "340A", "340B", "341A", 
    "Expresso Maringá - 341B","Saens Peña - 101", "102", "105", "107", "111", "115", 
    "117", "119", "121", "122", "123", "125", "128", "133","134", "135", "140", "141", 
    "142", "150", "214", "230", "300", "303", "304", "306", "309", "311", "313", "314",
    "320", "327", "Saens Peña - 331", "Alternativo - Alt 10", "Alt 11", "Alt 14", "Alt 15",
    "Alt 20", "Alt 21", "Alt 24", "Alt 25", "Alt 28", "Alt 30", "Alt 31", "Alt 32", "Alt 33", 
    "Alternativo - Alt 38"
  ),
  labels = c("CS<br>Brasil<br>33", "Expresso<br>Maringá<br>37", "Saens<br>Peña<br>33", "Alternativo<br>14",
             "103", "104", "108", "112", "116", "118", "124", "200", "201", "202", "205", "211", 
             "215","216", "222", "225", "231", "232", "237", "240", "242", "243", "244", "245", 
             "246", "250", "251", "130A","130B", "204A", "204B", "206A", "206B",  
             "208", "209", "210", "212", "219", "229", "252", "302", "305", 
             "307", "310", "315", "316","317", "318", "319", "322","323", "325", "330", "333", 
             "334", "335", "342", "343", "344", "345", "347", "349","350", "355", "308A", "308OF",
             "340A", "340B", "341A", "341B","101", "102", "105", 
             "107", "111", "115", "117", "119", "121", "122", "123", "125", "128", "133",
             "134", "135", "140", "141", "142", "150", "214", "230", "300", "303", "304", "306", 
             "309", "311", "313", "314","320", "327", "331", "Alt 10", "Alt 11", "Alt 14", "Alt 15",
             "Alt 20", "Alt 21", "Alt 24", "Alt 25", "Alt 28", "Alt 30", "Alt 31", "Alt 32", "Alt 33", "Alt 38"
  ),
  parents = c("","","","","CS Brasil","CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil", 
              "CS Brasil",  "CS Brasil","CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil",
              "CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil","CS Brasil",  "CS Brasil",
              "CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil", "CS Brasil", 
              "CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil",
              "CS Brasil",  "CS Brasil","CS Brasil",  "CS Brasil","Expresso Maringá", 
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", 
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", 
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", 
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", 
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", 
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", 
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá",
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá",
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá",
              "Saens Peña", "Saens Peña", "Saens Peña","Saens Peña", "Saens Peña", "Saens Peña",
              "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña",
              "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", 
              "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", 
              "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", 
              "Saens Peña", "Saens Peña", "Saens Peña", "Alternativo", "Alternativo", "Alternativo",
              "Alternativo", "Alternativo", "Alternativo", "Alternativo", "Alternativo", "Alternativo",
              "Alternativo", "Alternativo", "Alternativo", "Alternativo", "Alternativo"
  ),
  nomes = c("", "","","",
            "Costinha / Terminal Central","Vargem Grande / Terminal Central", 
            "Canindu - Vl. Cândida / Terminal Central","Vl. Terezinha / Cta", 
            "Taquari / Rodoviária", "Sertãozinho – Via Vila Cândida / Terminal Central",
            "Buquirinha Ii / João Guilhermino", "Parque Tecnológico / Terminal Central",
            "Bairrinho / Terminal Central","Bom Retiro / Terminal Central", 
            "Eugenio De Melo - Galo Branco / Praça Afonso Pena",
            "Jd. Califórnia - Via Vista Verde / Santana – Via Vl. Cristina","Tesouro / Aquárius",
            "Santa Maria - Av. Tancredo Neves / Av. Eng. Francisco José Longo", 
            "Galo Branco / Jd. Aquárius","Galo Branco / Av. São José - Semi Expressa", 
            "Tesouro / Vl. Dirce", "Novo Horizonte / Av. São José","Novo Horizonte / Aquárius",
            "Novo Horizonte / Campo dos Alemães", "Majestic / Praça Afonso Pena", 
            "Santa Maria - Jd. São José - Coqueiro / Av. Eng. Francisco José Longo", 
            "Jd. São José - Frei Galvão - Santa Inês / Terminal Central",
            "Praça 1º De Maio / Centro", 
            "Alimentadora Paineiras - Dom Bosco - Santa Helena / Tancredo Neves",
            "Novo Horizonte Corujão", 
            "Eugênio De Melo Corujão", "São Francisco Xavier / Cachoeira Do Roncador", 
            "Cachoeira Do Roncador / São José Dos Campos",
            "Novo Horizonte - Pedro Álvares Cabral / Terminal Central", 
            "Novo Horizonte / Av. Eng. Francisco José Longo", 
            "Santa Inês - Pedro Álvares Cabral / Terminal Central", 
            "Santa Inês / Av. Eng. Francisco José Longo",  
            "Aeroporto / Terminal Central",  "Jd. Uirá / Terminal Central",
            "Diamante / Terminal Central",
            "Putim - Av. Dos Astronautas / Terminal Central",  "Santa Luzia / Terminal Central",  
            "Res. São Francisco / Centro - Direto",  "Jd. Uirá Corujão", 
            "Putim Tamoios / Praça Afonso Pena","São Judas Tadeu / Terminal Central",  
            "Morumbi / Terminal Central",  "Res. União / Terminal Central",
            "Pq. Interlagos / Terminal Central", "Torrão de Ouro / Terminal Central",  
            "Campo dos Alemães / Rodoviária",
            "Dom Pedro I / Terminal Central",  "Dom Pedro I / Rodoviária",  
            "Capuava / Terminal Central","Campo dos Alemães / Terminal Central", 
            "Bosque dos Ipês - Jd. Sul / Satélite","Corredor Sul 1",
            "Jardim Santa Rosa – Via São Judas e Colinas / Terminal Central",  
            "Jardim São Judas Tadeu / Terminal Central via Tecnasa",
            "Vila Adriana - Emha Ii / Praça Afonso Pena",  
            "Alimentadora Pousada do Vale / Eco Campos de São José",
            "Alimentadora Serrote / Eco Campos de São José", 
            "Alimentadora Campos De São José / Eco Campos De São José",
            "Alimentadora Mariana Ii / Santa Cecília Ii",
            "Petrobrás - Marginal Dutra / Praça Afonso Pena", 
            "Alimentadora Monterrey / Eco Campos de São José",
            "Colonial Corujão",  "Recanto Dos Eucaliptos / Res. Flamboyant",  
            "Bosque Dos Eucaliptos - Andrômeda / Terminal Central",
            "Bosque dos Eucaliptos - Ouro Fino / Terminal Central", 
            "Eco Campos de São José / Terminal Central - Semiexpressa",
            "Eco Campos de São José / Av. Eng. Francisco José Longo",  
            "Eco Campos de São José / Terminal Central",
            "Eco Campos de São José / Av. Eng. Francisco José Longo",
            "Represa / Terminal Central","Jaguari / Terminal Central", 
            "Bairro dos Freitas / Av. Eng. Francisco José Longo",
            "Vl. Paiva / Av. Eng. Francisco José Longo", "Vale dos Pinheiros / Monte Castelo",
            "Vl. Dirce - Altos De Santana / Av. Eng. Francisco José Longo","Morumbi / Aquárius",
            "Colonial / Aquárius", "Urbanova - Esplanada / Terminal Central", 
            "Altos de Santana / Pq. Industrial","Minas Gerais / Jd. Augusta", 
            "Buquirinha - Vl. Dirce / Aquárius", "Urbanova - Colinas / Terminal Central", 
            "Alimentadora Nova República - Vl. Das Flores / Colonial",
            "Vale dos Pinheiros / Terminal Central", 
            "Colonial- Vl. das Flores- Vl. São Bento / Rodoviária", "Corredor Norte 1", 
            "Alimentadora Bairro São João - Sobrado / Caetê", "Corredor Sul 2", 
            "Vl. Paiva Corujão", "Tesouro - Sebastião Gualberto / Av. Eng. Francisco José Longo", 
            "Tesouro / Colonial", "Integração Zona Sul - Colonial / Ch. Reunidas", 
            "Colonial / Rodoviária", "Colonial / Praça Afonso Pena",
            "Limoeiro - Dutra / Terminal Central", "Pq. Industrial / Rodoviária",
            "Limoeiro / Praça Afonso Pena", "Aquárius - Colinas / Terminal Central", 
            "Ch. Reunidas / Terminal Central", 
            "Pq. Industrial - Jd. das Indústrias / Praça Afonso Pena", 
            "Res. União / Praça Afonso Pena","Campo dos Alemães / Aquárius", 
            "Canindu / Av. Eng. Francisco José Longo",
            "Altos De Santana / Centro", "Vl. Paiva / Centro", 
            "Jd. Guimarães / Centro", "Colorado / Olímpio Catão",
            "Vl. Do Tesouro / Centro", "Vista Verde / Centro", "Michigan / Av. Eng. Francisco José Longo", 
            "Ch. Reunidas/ Olímpio Catão","Jd. Portugal / Olímpio Catão", "Jd. Morumbi / Olímpio Catão", 
            "Jd. das Indústrias / Olímpio Catão", "Terras do Sul / Olímpio Catão",
            "Chácaras Havaí / Av. Eng. Francisco José Longo"
  ),
  stringsAsFactors = FALSE
)


# 2. Salva o arquivo ------------------------------------------------------

## Salva o banco 'linhas2' em .csv

write_sf(lrotas, "data/shapes/linhas.shp")

rm(list = ls())
