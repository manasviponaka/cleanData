#' To visualise the columns of data frame
#' @param df A data frame containing insurance data.
#' @param column1 A column to select from dataset.
#' @param column2 A column to select from dataset. default is
#' @param fill_color A column from dataset to add fill colour to the plot , If any input other than a column name is specified the default is "#bcd4e6"
#' @param plot_func type of plot to display (its is the name of the ggplot function) #check ggplot on what function to choose
#' @importFrom dplyr select
#' @import ggplot2
#' @importFrom rlang enquo quo_text
#' @export
#' @return displays a plot
#' @details
#' This function allows the user to visualise columns in the data frame which includes
#' bar plots  , density plots  , scatter plots and there is an option to add color/fill to show the effect of the confounding variable


get_plot <- function(df , column1= 0 , column2 = 0, fill_color = "#bcd4e6"  ,plot_func ){

  column1<-rlang::enquo(column1)
  column2<-rlang::enquo(column2)
  text_size <- 15
  def_pos <- "right"
  if(rlang::quo_text(fill_color) %in% colnames(df)){

  fill_color<-rlang::enquo(fill_color)

    if((rlang::quo_text(column2) %in% colnames(df)) & (rlang::quo_text(column1) %in% colnames(df)) ){

         title = paste(as.character(substitute(plot_func))[3] , "plot for" ,rlang::quo_text(column1) ,"~" ,rlang::quo_text(column2))
         plot <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = !!column1, y = !!column2, color =  !!fill_color)) +
                 plot_func()
         }


    else if (!(rlang::quo_text(column2) %in% colnames(df))  & (rlang::quo_text(column1) %in% colnames(df))){
       title = paste(as.character(substitute(plot_func))[3] , "plot for" ,rlang::quo_text(column1))
       plot <- ggplot2::ggplot(data = df, mapping = ggplot2 :: aes(x = !!column1 , fill = !!fill_color)) +
               plot_func(alpha = 0.5)
       }

    else if (!(rlang::quo_text(column1) %in% colnames(df)) & (rlang::quo_text(column2) %in% colnames(df))){
       title = paste(as.character(substitute(plot_func))[3] , "plot for" ,rlang::quo_text(column2))
       plot <- ggplot2::ggplot(data = df, mapping = ggplot2 :: aes(x = !!column2 , fill = !!fill_color)) +
               plot_func(alpha = 0.5)+
               ggplot2:: coord_flip()
      }

  else{
    warning("please check the column names provided and specify each of them with their argument name eg: getplot(df = dataframe , column1 = col1, column2 = col2, fill_color = col3,plot_func = ggplot2::geom_point)")
  }

}

else{
    def_pos <- "none"

    if((rlang::quo_text(column2) %in% colnames(df)) & (rlang::quo_text(column1) %in% colnames(df)) ){

      title = paste(as.character(substitute(plot_func))[3] , "plot for" ,rlang::quo_text(column1) ,"~" ,rlang::quo_text(column2))
      plot <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = !!column1, y = !!column2, color =  fill_color)) +
        plot_func()
    }


    else if (!(rlang::quo_text(column2) %in% colnames(df))  & (rlang::quo_text(column1) %in% colnames(df))){
      title = paste(as.character(substitute(plot_func))[3] , "plot for" ,rlang::quo_text(column1))
      plot <- ggplot2::ggplot(data = df, mapping = ggplot2 :: aes(x = !!column1 , fill = fill_color)) +
        plot_func(alpha = 0.5)
    }

    else if (!(rlang::quo_text(column1) %in% colnames(df)) & (rlang::quo_text(column2) %in% colnames(df))){
      title = paste(as.character(substitute(plot_func))[3] , "plot for" ,rlang::quo_text(column2))
      plot <- ggplot2::ggplot(data = df, mapping = ggplot2 :: aes(x = !!column2 , fill = fill_color)) +
        plot_func(alpha = 0.5)+
        ggplot2:: coord_flip()
    }

    else{
      warning("please check the column names provided and specify each of them with their argument name eg: getplot(df = dataframe , column1 = col1, column2 = col2, fill_color = col3,plot_func = ggplot2::geom_point)")
    }
  }

  if((rlang::quo_text(column2) %in% colnames(df)) | (rlang::quo_text(column1) %in% colnames(df)) ){
    plot+
    ggplot2::labs(title = title)+
    ggplot2::theme_classic()+
      ggplot2 ::theme( text= ggplot2 ::element_text(size=13,  family="Times"),
            plot.title = ggplot2 ::element_text(hjust = 0.5, size=text_size),
            axis.title.x= ggplot2 ::element_text(size=text_size),
            axis.title.y= ggplot2 ::element_text(size=text_size),
            axis.text.x = ggplot2 ::element_text(size=text_size),
            axis.text.y = ggplot2 ::element_text(size=text_size),
            legend.title = ggplot2 ::element_text(size = text_size),
            legend.text = ggplot2 ::element_text(size = text_size),
            legend.position = def_pos) }
}



