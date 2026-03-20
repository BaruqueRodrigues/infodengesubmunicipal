# infodengesubmunicipal

`infodengesubmunicipal` e um pacote em R para preparacao, classificacao e visualizacao de dados epidemiologicos em escala submunicipal. O pacote organiza funcoes do fluxo analitico do projeto InfoDengue submunicipal, incluindo:

- leitura e filtragem de dados
- normalizacao de nomes de bairros
- completude de series semanais
- classificacao espacial de bairros em distritos
- calculo de alerta epidemiologico
- geracao de graficos e mapas

## Instalacao

### Via GitHub

```r
install.packages("remotes")
remotes::install_github("BaruqueRodrigues/infodengesubmunicipal")
```

### A partir do codigo local

```r
install.packages("devtools")
devtools::install(".")
```

### Carregar o pacote

```r
library(infodengesubmunicipal)
```

## Estrutura da API

Hoje o pacote contem dois grupos de funcoes:

- API legada: funcoes originais, preservadas para compatibilidade com o fluxo antigo
- API nova: funcoes refatoradas, com menos dependencia de objetos globais e assinaturas mais explicitas

A recomendacao geral e:

- usar a API legada para reproduzir pipelines antigos
- preferir a API nova em codigo novo

## API Legada

### Leitura e preparacao

```r
dados <- ler_dados_dengue("dados_dengue.dbf")

dados$nm_bairro_norm <- normalizar_bairro(dados$NM_BAIRRO)

dados_completos <- completar_dados_bairros(
  dados_bairros = dados_agregados,
  bairros_unicos = bairros_unicos,
  se_var = "sem_not",
  nm_bairr_var = "nm_bairro_ref",
  group_var = "arbo",
  max_semana_epidemiologica = 202452
)
```

### Classificacao espacial

```r
bairros_distritos <- classificar_bairros_distrito(
  sp_distritos = sp_distritos,
  sp_bairros = sp_bairros
)
```

### Alerta epidemiologico

Importante: a funcao legada `calcular_alerta()` depende de um objeto global chamado `params`.

```r
params <- list(
  codmodelo = "seu_modelo",
  limiar1 = 10,
  limiar2 = 20
)

alerta_unidade <- calcular_alerta(
  id = 1,
  nome = "Distrito Centro",
  coluna_id = "id_unidade",
  data = dados_alerta,
  gtdist = "normal",
  meangt = 3,
  sdgt = 1
)
```

### Indicadores e visualizacao

```r
semanas_alerta <- calcular_semanas_alerta_alto(
  data = dados_alerta,
  semana_atual = 202452,
  bairro = "CENTRO",
  janela = 10
)

breaks_1ano <- criar_breaks_1ano(ano_fim = 2024, semana_fim = 52)

plots_rt <- create_rt_plot(
  api_data = dados_rt,
  weeks_limit = 202452,
  title_suffix = "Rt"
)
```

As funcoes legadas de plot e mapa podem depender de objetos globais auxiliares, como `breaks_1ano`, `weeks_2_plot`, `ano_atual`, `limiar_epidemico`, `df_mun` e `df_mun_ok`.

## API Nova

A API nova explicita argumentos que antes vinham do ambiente global. Isso facilita teste, reuso e manutencao.

### Leitura e preparacao

```r
dados <- ler_dados_dengue_novo(
  caminho = "dados_dengue.dbf",
  codigo_municipio = "420910"
)

dados_completos <- completar_dados_bairros_novo(
  dados_bairros = dados_agregados,
  bairros_unicos = bairros_unicos,
  se_var = "sem_not",
  nm_bairr_var = "nm_bairro_ref",
  group_var = "arbo",
  max_semana_epidemiologica = 202452
)
```

### Pipeline de alerta

```r
params_modelo <- list(
  codmodelo = "seu_modelo",
  limiar1 = 10,
  limiar2 = 20
)

alerta_unidade <- calcular_alerta_novo(
  id = 1,
  data = dados_alerta,
  coluna_id = "id_unidade",
  params = params_modelo,
  gtdist = "normal",
  meangt = 3,
  sdgt = 1
)

alerta_cidade <- calcular_alerta_cidade(
  data = dados_alerta,
  coluna_id = "id_unidade",
  params = params_modelo
)
```

Tambem e possivel usar os blocos menores separadamente:

```r
dados_filtrados <- filtrar_unidade(dados_alerta, "id_unidade", 1)
dados_rt <- calcular_rt(dados_filtrados)
dados_classificados <- classificar_alerta(dados_rt, params_modelo)
```

### Visualizacao refatorada

```r
breaks <- criar_breaks_1ano(ano_fim = 2024, semana_fim = 52)

plots_rt <- create_rt_plot_novo(
  api_data = dados_rt,
  weeks_limit = 202452,
  title_suffix = "Rt",
  breaks_1ano = breaks
)

plot_receptividade <- create_receptivity_plot_novo(
  api_data = dados_clima,
  weeks_limit = 202452,
  breaks_1ano = breaks
)

plot_incidencia <- create_incidence_plot_novo(
  nowcast_data = dados_nowcast,
  observed_data = dados_observados,
  weeks_limit = 202452,
  breaks_1ano = breaks,
  limiar_epidemico = 72,
  title_suffix = "Curva de incidencia"
)
```

### Mapas refatorados

```r
mapas <- generate_maps_novo(
  data = dados_sf,
  weeks_2_plot = c(202450, 202451, 202452),
  arbo = "dengue",
  allocated_data = df_mun_ok,
  total_data = df_mun
)
```

## Funcoes principais

### Legadas

- `ler_dados_dengue()`
- `normalizar_bairro()`
- `completar_dados_bairros()`
- `classificar_bairros_distrito()`
- `calcular_alerta()`
- `calcular_semanas_alerta_alto()`
- `create_rt_plot()`
- `create_receptivity_plot()`
- `create_incidence_plot()`
- `generate_maps()`
- `create_map_panel()`

### Novas

- `ler_dados_dengue_novo()`
- `completar_dados_bairros_novo()`
- `filtrar_unidade()`
- `calcular_rt()`
- `classificar_alerta()`
- `calcular_alerta_novo()`
- `calcular_alerta_cidade()`
- `calcular_semanas_alerta_alto_novo()`
- `create_rt_plot_novo()`
- `create_receptivity_plot_novo()`
- `create_incidence_plot_novo()`
- `generate_maps_novo()`
- `classificar_bairros_distrito_novo()`

## Status do pacote

O pacote esta em refatoracao ativa. A estrategia atual e:

- manter as funcoes antigas para compatibilidade
- expandir cobertura de testes sobre a API legada
- implementar funcoes `_novo` com menos acoplamento e melhor testabilidade

## Desenvolvimento

Para instalar dependencias de desenvolvimento e rodar verificacoes locais:

```r
install.packages(c("devtools", "testthat", "roxygen2"))
devtools::document()
devtools::test()
devtools::check()
```
