dfVI <- readxl::read_excel('dados_ndvi_evi.xlsx') 

dfFert <- readxl::read_excel('dados_fertilidade.xlsx') |> 
  dplyr::select(-c(Latitude,Longitude))

#### recorte banco de dados para média anual, ndvi e fertilidade - analise de correlação
dfanual <- dfVI |> 
  dplyr::mutate(Ano = lubridate::year(Data)) |> #refazer o ano
  dplyr::select(Gleba,Ponto,Ano,NDVI,EVI) |> #selecionar as variaveis q quer do df original
  dplyr::group_by(Gleba,Ponto,Ano) |> #agrupar por varaiveis desejadas
  dplyr::summarise_all(mean) |> #sumarizar por alguma função
  dplyr::left_join(dfFert) |> # juntar com outro df pelas variaveis de MESMO nome
  na.omit() # retira NA

write.csv(dfanual,'bd_parcial_mean.csv')

#### Análise correlação direto (pearson) geral

dfVI |> 
  dplyr::mutate(Ano = lubridate::year(Data)) |> #refazer o ano
  dplyr::select(Gleba,Ponto,Ano,NDVI,EVI) |> #selecionar as variaveis q quer do df original
  dplyr::group_by(Gleba,Ponto,Ano) |> #agrupar por varaiveis desejadas
  dplyr::summarise_all(mean) |> #sumarizar por alguma função
  dplyr::left_join(dfFert) |> # juntar com outro df pelas variaveis de MESMO nome
  na.omit() |># retira NA
  dplyr::ungroup() |> 
  dplyr::select(-c(Gleba,Ponto,Ano)) |> 
  GGally::ggcorr()

#### Análise correlação direto (speraman) geral

dfVI |> 
  dplyr::mutate(Ano = lubridate::year(Data)) |> #refazer o ano
  dplyr::select(Gleba,Ponto,Ano,NDVI,EVI) |> #selecionar as variaveis q quer do df original
  dplyr::group_by(Gleba,Ponto,Ano) |> #agrupar por varaiveis desejadas
  dplyr::summarise_all(mean) |> #sumarizar por alguma função
  dplyr::left_join(dfFert) |> # juntar com outro df pelas variaveis de MESMO nome
  na.omit() |># retira NA
  dplyr::ungroup() |> 
  dplyr::select(-c(Gleba,Ponto,Ano)) |> 
  GGally::ggcorr(method = c('pairwise','spearman'))

cortable <- dfVI |> 
  dplyr::mutate(Ano = lubridate::year(Data)) |> #refazer o ano
  dplyr::select(Gleba,Ponto,Ano,NDVI,EVI) |> #selecionar as variaveis q quer do df original
  dplyr::group_by(Gleba,Ponto,Ano) |> #agrupar por varaiveis desejadas
  dplyr::summarise_all(mean) |> #sumarizar por alguma função
  dplyr::left_join(dfFert) |> # juntar com outro df pelas variaveis de MESMO nome
  na.omit() |>
  dplyr::ungroup() |> 
  dplyr::select(-c(Gleba,Ponto,Ano)) |> 
  cor(method = 'spearman') |> 
  as.data.frame(cortable)

write.csv(cortable,'tabela_correlacao_spearman.csv')

cortable_pearson <- dfVI |> 
  dplyr::mutate(Ano = lubridate::year(Data)) |> #refazer o ano
  dplyr::select(Gleba,Ponto,Ano,NDVI,EVI) |> #selecionar as variaveis q quer do df original
  dplyr::group_by(Gleba,Ponto,Ano) |> #agrupar por varaiveis desejadas
  dplyr::summarise_all(mean) |> #sumarizar por alguma função
  dplyr::left_join(dfFert) |> # juntar com outro df pelas variaveis de MESMO nome
  na.omit() |>
  dplyr::ungroup() |> 
  dplyr::select(-c(Gleba,Ponto,Ano)) |> 
  cor(method = 'pearson') |> 
  as.data.frame(cortable)

write.csv(cortable,'tabela_correlacao_pearson.csv')

#### pairplot geral (distribuição, disperção, correlação de Spearman)

dfVI |> 
  dplyr::mutate(Ano = lubridate::year(Data)) |> #refazer o ano
  dplyr::select(Gleba,Ponto,Ano,NDVI,EVI) |> #selecionar as variaveis q quer do df original
  dplyr::group_by(Gleba,Ponto,Ano) |> #agrupar por varaiveis desejadas
  dplyr::summarise_all(mean) |> #sumarizar por alguma função
  dplyr::left_join(dfFert) |> # juntar com outro df pelas variaveis de MESMO nome
  na.omit() |> 
  dplyr::ungroup() |> 
  dplyr::select(-c(Gleba,Ponto,Ano,EVI)) |>
  GGally::ggpairs(upper=list(continuous = GGally::wrap("cor", 
                                                       method = "spearman")))


#### pairplot por gleba (distribuição, disperção, correlação de Spearman) somente NDVI e P

dfVI |> 
  dplyr::mutate(Ano = lubridate::year(Data)) |> #refazer o ano
  dplyr::select(Gleba,Ponto,Ano,NDVI,EVI) |> #selecionar as variaveis q quer do df original
  dplyr::group_by(Gleba,Ponto,Ano) |> #agrupar por varaiveis desejadas
  dplyr::summarise_all(mean) |> #sumarizar por alguma função
  dplyr::left_join(dfFert) |> # juntar com outro df pelas variaveis de MESMO nome
  na.omit() |> 
  dplyr::filter(P<100) |> 
  dplyr::ungroup() |> 
  dplyr::select(Gleba,NDVI,EVI,P) |> 
  dplyr::mutate(
    Gleba = as.character(Gleba)
  ) |> 
  GGally::ggpairs(ggplot2::aes(colour=Gleba)
                  ,upper=list(continuous = GGally::wrap("cor", 
                                                        method = "spearman")))




####### Pairplot por gleba e periodo do ano 
## (distribuição, disperção, correlação de Spearman) somente NDVI, EVI e P
dfVI |> 
  dplyr::mutate(Ano = lubridate::year(Data),
                Mes = lubridate::month(Data)) |> #refazer o ano
  dplyr::select(Gleba,Ponto,Ano, Mes,NDVI,EVI) |> 
  dplyr::filter(Mes == 05| Mes == 06|Mes == 07|Mes == 08) |> #selecionar as variaveis q quer do df original
  dplyr::group_by(Gleba,Ponto,Ano) |> #agrupar por varaiveis desejadas
  dplyr::summarise_all(mean) |> #sumarizar por alguma função
  dplyr::left_join(dfFert) |> # juntar com outro df pelas variaveis de MESMO nome
  na.omit() |> 
  dplyr::filter(P<100) |> 
  dplyr::ungroup() |> 
  dplyr::select(Gleba,NDVI,EVI,P) |> 
  dplyr::mutate(
    Gleba = as.character(Gleba)
  ) |> 
  GGally::ggpairs(ggplot2::aes(colour=Gleba)
                  ,upper=list(continuous = GGally::wrap("cor", 
                                                        method = "spearman")))
