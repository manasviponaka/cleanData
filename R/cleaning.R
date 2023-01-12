#' Easily preprocess insurance data
#' @param dataset A data frame containing insurance data.
#' @importFrom dplyr mutate_if
#' @export
#' @return A data frame
#' @details
#' This function allows the user to preprocess the data frame which includes
#' checking for null values, modifying  the column types.

cleaning <- function(dataset ){
  if (base::is.data.frame(dataset)){
  dataset |>
    dplyr::mutate_if(is.character , as.factor)}
  else {print("error - The function takes only data.frame class obejects as input")}

}



