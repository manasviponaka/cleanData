library(dplyr)
cleaning<- function(dataset){
  dataset |>
    dplyr::mutate_if(is.character , as.factor) %>%
    dplyr::view(dataset)
}
