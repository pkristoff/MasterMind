library(shiny)
library(shinyjs)

library(xtable)
library(DT)
library(dplyr)

source('ui.R')
source('server.R')

xxx <- mmServer('serve')
print(xxx)

shinyApp(ui = mmUI(), server = xxx)
