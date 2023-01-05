#' To visualise the columns of data frame
#' @param df A data frame containing insurance data.
#' @param column1 A column to select from dataset.
#' @param column2 A column to select from dataset.
#' @param plot_func type of plot to display (its is the name of the ggplot function)
#' @importFrom dplyr select
#' @import ggplot2
#' @importFrom rlang enquo quo_text
#' @export
#' @return displays a plot
#' @details
#' This function allows the user to preprocess the data frame which includes
#' checking for null values, modifying  the column types.


get_plot <- function(df , column1 , column2,plot_func){
  column1<-rlang::enquo(column1)
  column2<-rlang::enquo(column2)

  if(rlang::quo_text(column2) %in% colnames(df)){
  title = paste(as.character(substitute(plot_func))[3] , "plot for" ,rlang::quo_text(column1) , rlang::quo_text(column2))

  ggplot2::ggplot(data = df, mapping = ggplot2 :: aes(x = !!column1 , y = !!column2)) +
  ggplot2::labs(title = title)+
  plot_func()+
  theme_bw()}

  else{
    title = paste(as.character(substitute(plot_func))[3] , "plot for" ,rlang::quo_text(column1))

    ggplot2::ggplot(data = df, mapping = ggplot2 :: aes(x = !!column1)) +
    ggplot2::labs(title = title)+
    plot_func()+
    theme_bw()}

}




