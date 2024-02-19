dados=readxl::read_excel('dados_ndvi_evi.xlsx') |> 
  dplyr::mutate(
    Ano = lubridate::year(Data)
  )


max(dados$Ano)

 
 ### NDVI

ndvi_summary= dados |> 
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
  dplyr::group_by(gleba,mes) |> 
  dplyr::summarise(
    ndvi_mean=mean(NDVI),
    ndvi_sd=sd(NDVI),
    ndvi_max=max(NDVI),
    ndvi_min=min(NDVI),
    ndvi_cv=(ndvi_sd/ndvi_mean)*100
  )

write.csv(ndvi_summary,'ndvi_summary_monthly.csv')

#### EVI

evi_summary= dados |> 
  dplyr::select(Gleba,Data,EVI) |> 
  na.omit()|>
  dplyr::mutate(
    mes=lubridate::month(Data,label=TRUE),
    gleba=dplyr::case_when(
      Gleba==1~'Gleba 1',
      Gleba==2~'Gleba 2',
      Gleba==3~'Gleba 3',
      Gleba==4~'Gleba 4'
    )
  ) |>
  dplyr::group_by(gleba,mes) |> 
  dplyr::summarise(
    evi_mean=mean(EVI),
    evi_sd=sd(EVI),
    evi_max=max(EVI),
    evi_min=min(EVI),
    evi_cv=(evi_sd/evi_mean)*100
  )

write.csv(evi_summary,'evi_summary.csv')


################### Boxplot ########################
dados |> 
  dplyr::select(Gleba,Data,NDVI) |> 
  na.omit()|>
  dplyr::mutate(
    mes=lubridate::month(Data,label=TRUE),
    gleba=dplyr::case_when(
      Gleba==1~'Gleba 1',
      Gleba==2~'Gleba 2',
      Gleba==3~'Gleba 3',
      Gleba==4~'Gleba 4'
    )
  ) |> 
  ggplot2::ggplot(ggplot2::aes(x=mes,y=NDVI,group=mes,fill=mes))+
  ggplot2::geom_boxplot()+
  ggplot2::facet_wrap(~gleba)+
  ggplot2::xlab('Mês')+
  ggplot2::theme_bw()


ggplot2::ggsave('ndvi.png',units = 'in',width = 12, height=7,dpi=600)

###############################################################

dados |> 
  dplyr::select(Gleba,Data,EVI) |> 
  na.omit()|>
  dplyr::mutate(
    mes=lubridate::month(Data,label=TRUE),
    gleba=dplyr::case_when(
      Gleba==1~'Gleba 1',
      Gleba==2~'Gleba 2',
      Gleba==3~'Gleba 3',
      Gleba==4~'Gleba 4'
    )
  ) |> 
  ggplot2::ggplot(ggplot2::aes(x=mes,y=EVI,group=mes,fill=mes))+
  ggplot2::geom_boxplot()+
  ggplot2::scale_fill_brewer(palette = 'RdYlBu')+
  ggplot2::facet_wrap(~gleba)+
  ggplot2::xlab('Mês')+
  ggplot2::theme_bw()


ggplot2::ggsave('evi.png',units = 'in',width = 12, height=7,dpi=600)



 #### GLEBA



### NDVI

ndvi_summary_gleba= dados |> 
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
  dplyr::group_by(gleba) |> 
  dplyr::summarise(
    ndvi_mean=mean(NDVI),
    ndvi_sd=sd(NDVI),
    ndvi_max=max(NDVI),
    ndvi_min=min(NDVI),
    ndvi_cv=(ndvi_sd/ndvi_mean)*100
  )

write.csv(ndvi_summary_gleba,'ndvi_summary_gleba.csv')

