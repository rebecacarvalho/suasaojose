Sua São José
================

## Responsáveis

  - Rebeca Carvalho

## Funcionamento

1.  Para verificar o processamento dos dados utilizados no app, assim
    como o cálculo dos indicadores, basta rodar o script `00_index.R`.

2.  Para acesssar o aplicativo *Sua São José*, basta rodar o script
    `global.R`.

## Divisão dos *scripts*

### Dos dados:

  - `00_index.R`: *script* sumário, responsável por calcular os
    indicadores referentes ao município de São José dos Campos.

  - `01_data.R`: carrega os arquivos contendo dados demográficos,
    socioeconômicos e de mobilidade do município de São José dos Campos.

  - `02_ddemograficos.R`: calcula os indicadores referentes a demografia
    do município de São José dos Campos.

  - `03_descolares.R`: calcula os indicadores referentes a educação do
    município de São José dos Campos.

  - `04_drais.R`: calcula os indicadores referentes ao emprego do
    município de São José dos Campos.

  - `05_dpesqod.R`: calcula os indicadores referentes a Pesquisa Origem
    e Destino (OD/2010) do município de São José dos Campos.

  - `06_dlinhas.R`: calcula os indicadores referentes as linhas de
    ônibus operantes no município de São José dos Campos.

### Do aplicativo:

  - `global.R`: *script* sumário do aplicativo, responsável por carregar
    os indicadores pré-calculados e rodar as demais partes do app.

  - `ui.R`: *script* referente a interface do usuário do aplicativo.

  - `server.R`: *script* referente as configurações do servidor do
    aplicativo.
