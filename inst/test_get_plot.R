data(dataset)
text_size <- 15

test_that("getting plot with correct type of graph and variables", {


  returned_value <- get_plot(dataset , age , charges , 0 , ggplot2::geom_point)


  expected_value <- dataset %>% ggplot2::ggplot(ggplot2::aes( x = age , y = charges , fill = "#bcd4e6" , color = "#bcd4e6" ))+
                    ggplot2::geom_point() +
                   ggplot2::labs(title = "geom_point plot for age ~ charges" )+
                   ggplot2::theme_classic()+
                   ggplot2 ::theme( text= ggplot2 ::element_text(size=13,  family="Times"),
                     plot.title = ggplot2 ::element_text(hjust = 0.5, size=text_size),
                     axis.title.x= ggplot2 ::element_text(size=text_size),
                     axis.title.y= ggplot2 ::element_text(size=text_size),
                     axis.text.x = ggplot2 ::element_text(size=text_size),
                     axis.text.y = ggplot2 ::element_text(size=text_size),
                     legend.title = ggplot2 ::element_text(size = text_size),
                     legend.text = ggplot2 ::element_text(size = text_size),
                     legend.position = "none")




  expect_equal(returned_value, expected_value)
})
