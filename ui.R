
## Load libraries

options(shiny.reactlog=TRUE) 

library(tidyverse)
library(biogrowth)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(rhandsontable)
library(dashboardthemes)
library(shinycssloaders)
library(shinyWidgets)
library(colourpicker)
library(shinydashboardPlus)
library(plotly)

source("tableFileUI.R")
source("tableFile.R")

## Header

header <- dashboardHeader(title = shinyDashboardLogo(
    theme = "poor_mans_flatly",
    boldText = "biogrowth",
    mainText = "",
    badgeText = "web"
))

## Sidebar

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "sidebar",
        menuItem("Welcome page", tabName = "welcome", icon = icon("gulp")),
        menuItem("Static predictions", tabName = "st_prediction",
                 icon = icon("chart-line")),
        menuItem("Static predictions (stochastic)", tabName = "stoc_prediction",
                 icon = icon("dice-four")),
        menuItem("Dynamic predictions", tabName = "dyna_prediction",
                 icon = icon("car-side")),
        menuItem("Static fitting", tabName = "st_fit", icon = icon("crosshairs")),
        menuItem("Dynamic fitting", tabName = "dyna_fit", icon = icon("car-crash")),
        menuItem("Global fitting", tabName = "global_fit", icon = icon("pastafarianism")),
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

dashboardPage(header, sidebar, body, title = "biogrowth - web")









