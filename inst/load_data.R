# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


library(dplyr)
library(ggplot2)

data <- read.csv("./insurance.csv")

#usethis::use_data(name of variable)

print(summary(data))
print(head(data))
print(any(is.na(data)))


data <- cleaning(data)

print(str(data))




