# Generales
# install.packages("plotly")
# install.packages("tsibble")


# Gráficos
# install.packages("GGally")
# install.packages("urca")
# install.packages("fracdiff")
# install.packages("glue")
# install.packages("seasonalview")


# Librerias Generales
library(shiny)
library(tidyverse)
library(bslib)
library(future)
library(promises)
library(tsibble)
library(rJava)
library(dplyr)
library(tidyr)
library(stats)
library(bslib)

# Gráficos
library(plotly)
library(GGally)
library(urca)
library(fracdiff)
library(glue)
library(seasonalview)
library(ggplot2)


# Constants
AVAILABLE_DATASETS <- c(
  "Air Passengers" = "AirPassengers",
  "DAX Stock Index" = "DAX",
  "FTSE Stock Index" = "FTSE",
  "CAC Stock Index" = "CAC"
)

SERIES_TYPES <- c(
  "Original" = "y_values",
  "Seasonally Adjusted" = "sa_values",
  "Trend-Cycle" = "t_values",
  "Seasonal Component" = "s_values",
  "Irregular Residuals" = "i_values"
)

# UI with dark theme
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#375a7f"
  ),

  tags$head(
    tags$style(HTML("
      .plot-container { margin-bottom: 20px; }
      .group-section { margin-top: 20px; padding-top: 10px; border-top: 1px solid #444; }
      .well { background-color: #303030; }
    "))
  ),

  titlePanel("Time Series Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      selectInput("timeseries", "Select Dataset:",
                  choices = AVAILABLE_DATASETS),

      checkboxGroupInput("selected_series", "Select Analysis Types:",
                         choices = SERIES_TYPES,
                         selected = "original"),

      wellPanel(
        tags$h4("Advanced Options"),
        checkboxInput("show_statistics", "Show Summary Statistics", FALSE)
      ),

      div(class = "group-section",
          numericInput("num_groups", "Number of Comparison Groups:",
                       value = 0, min = 0, max = 4),
          uiOutput("group_checkboxes")
      )
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 uiOutput("time_series_plots")
        ),
        tabPanel("Statistics",
                 verbatimTextOutput("summary_stats")
        )
      )
    )
  )
)
