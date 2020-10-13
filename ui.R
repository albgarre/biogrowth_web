
## Load libraries

options(shiny.reactlog=TRUE) 

library(shiny)
library(shinydashboard)
library(rhandsontable)

source("tableFileUI.R")
source("tableFile.R")

## Header

header <- dashboardHeader(title = "biogrowth")

## Sidebar

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "sidebar",
        menuItem("Welcome page", tabName = "welcome", icon = icon("gulp")),
        menuItem("Static predictions (deterministic)", tabName = "st_prediction",
                 icon = icon("chart-line")),
        menuItem("Static predictions (stochastic)", tabName = "stoc_prediction",
                 icon = icon("dice-four")),
        menuItem("Dynamic predictions", tabName = "dyna_prediction",
                 icon = icon("globe")),
        menuItem("Static fitting", tabName = "st_fit", icon = icon("crosshairs")),
        menuItem("Dynamic fitting", tabName = "dyna_fit", icon = icon("cogs")),
        menuItem("Cardinal parameters", tabName = "cardinal", icon = icon("cocktail")),
        menuItem("User manual", icon = icon("audible"),
                 href = "https://docs.google.com/document/d/1470mvwo00rjr7j9tYo2PwzGZ036aXDxWOi9Gr9BJD6A/edit?usp=sharing"),
        menuItem("Github page", icon = icon("github"),
                 href = "https://github.com/albgarre/biogrowth_web")
    )
)

## Body

source("my_body.R")

## Lauch the application

dashboardPage(header, sidebar, body)









