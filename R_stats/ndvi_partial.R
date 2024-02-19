
###### ndvi para o periodo da fertilidade


dados=readxl::read_excel('dados_ndvi_evi.xlsx') |> 
  dplyr::mutate(
    Ano = lubridate::year(Data)
  )


max(dados$Ano)


### NDVI


ndvi_summary_partial = dados |> 
  dplyr::select(Gleba,Data,NDVI) |> 
  na.omit() |> 
  dplyr::mutate(
    ano=lubridate::year(Data),
    mes=lubridate::month(Data,label=TRUE),
    gleba=dplyr::case_when(
      Gleba==1~'Gleba 1',
      Gleba==2~'Gleba 2',
      Gleba==3~'Gleba 3',
      Gleba==4~'Gleba 4'
    )
  ) |> 
  dplyr::filter(ano>=2009,ano<=2017) |> 
  dplyr::group_by(gleba) |> 
  dplyr::summarise(
    ndvi_mean=mean(NDVI),
    ndvi_sd=sd(NDVI),
    ndvi_max=max(NDVI),
    ndvi_min=min(NDVI),
    ndvi_cv=(ndvi_sd/ndvi_mean)*100,
    ndvi_nobs = dplyr::n()
  )

write.csv(ndvi_summary_partial,'ndvi_summary_partial.csv')

