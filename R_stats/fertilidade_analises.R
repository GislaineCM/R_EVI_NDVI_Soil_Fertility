##### Fertilidade


dados <- readxl::read_excel('dados_fertilidade.xlsx')


fertilidade_mean <- dados |> 
  dplyr::select(-c(Ponto,Latitude,Longitude,Ano)) |> 
  dplyr::group_by(Gleba) |> 
  dplyr::summarise_all(mean)
write.csv(fertilidade_mean,'fertilidade_mean.csv')

fertilidade_sd <- dados |> 
  dplyr::select(-c(Ponto,Latitude,Longitude,Ano)) |> 
  dplyr::group_by(Gleba) |> 
  dplyr::summarise_all(sd)
write.csv(fertilidade_sd,'fertilidade_sd.csv')

fertilidade_max <- dados |> 
  dplyr::select(-c(Ponto,Latitude,Longitude,Ano)) |> 
  dplyr::group_by(Gleba) |> 
  dplyr::summarise_all(max)
write.csv(fertilidade_max,'fertilidade_max.csv')

fertilidade_min <- dados |> 
  dplyr::select(-c(Ponto,Latitude,Longitude,Ano)) |> 
  dplyr::group_by(Gleba) |> 
  dplyr::summarise_all(min)

write.csv(fertilidade_min,'fertilidade_min.csv')


cv <- function(x){
  cv <- 100*(sd(x)/mean(x))
  return(cv)
}

  
fertilidade_cv <- dados |> 
  dplyr::select(-c(Ponto,Latitude,Longitude,Ano)) |> 
  dplyr::group_by(Gleba) |> 
  dplyr::summarise_all(cv)


write.csv(fertilidade_cv,'fertilidade_cv.csv')



