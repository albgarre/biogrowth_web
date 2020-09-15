
## Load libraries

options(shiny.reactlog=TRUE) 

library(shiny)
library(shinydashboard)
library(rhandsontable)

## Header

header <- dashboardHeader(title = "biogrowth")

## Sidebar

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "sidebar",
        menuItem("Welcome page", tabName = "welcome"),
        menuItem("Static predictions", tabName = "st_prediction"),
        menuItem("Dynamic predictions", tabName = "dyna_prediction"),
        menuItem("Static fitting", tabName = "st_fit"),
        menuItem("Dynamic fitting", tabName = "dyna_fit"),
        menuItem("Cardinal parameters", tabName = "cardinal")
    )
)

## Body

source("my_body.R")

## Lauch the application

dashboardPage(header, sidebar, body)









