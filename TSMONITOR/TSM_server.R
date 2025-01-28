# Generales
# install.packages("plotly")
# install.packages("tsibble")

# TSA
# install.packages("seasonal")
# install.packages("feasts")
# install.packages("RJDemetra")
# install.packages("fpp3")
# install.packages("fable")

# Gráficos
# install.packages("GGally")
# install.packages("urca")
# install.packages("fracdiff")
# install.packages("glue")
# install.packages("seasonalview")


# Análisis Multivariante
# install.packages("broom")

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

# Librerias para TSA
library(seasonal)
library(feasts)
library(RJDemetra)
library(rjdqa)
# library(JcruncheR)
library(rjwsacruncher)
library(fpp3)
library(fable)
library(rjd3toolkit)
library(rjd3x13)
library(rjd3tramoseats)
library(rjd3providers)
library(rjd3workspace)
library(rjd3filters)
library(rjd3sts)
library(rjd3revisions)
library(ggdemetra3)


# Análisis Multivariante
library(broom) #PCA


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


# Server Logic

server <- function(input, output, session) {
  # Load and validate time series data
  time_series_data <- reactive({
    req(input$timeseries)

    data <- switch(input$timeseries,
                   "AirPassengers" = AirPassengers,
                   "DAX" = EuStockMarkets[, "DAX"],
                   "FTSE" = EuStockMarkets[, "FTSE"],
                   "CAC" = EuStockMarkets[, "CAC"]
    )

    validate(
      need(!is.null(data), "Selected dataset is not available")
    )

    return(data)
  })

  # Process time series with reactive window size
  processed_data <- reactive({
    req(time_series_data())
    process_timeseries(time_series_data())
  })

  # Dynamic group UI
  output$group_checkboxes <- renderUI({
    req(input$num_groups > 0)
    req(input$selected_series)

    lapply(1:input$num_groups, function(group_idx) {
      div(class = "well",
          checkboxGroupInput(
            inputId = paste0("group_", group_idx, "_series"),
            label = paste("Group", group_idx, "Series:"),
            choices = input$selected_series,
            selected = NULL
          )
      )
    })
  })

  # Generate interactive plots using plotly with light gray background
  generate_plot <- function(data, title, series_name = NULL) {
    df <- data.frame(
      time = seq_along(data),
      value = as.numeric(data)
    )

    p <- ggplot(df, aes(x = time, y = value)) +
      geom_line(color = "#375a7f", size = 0.25) +  # Darker blue line
      theme_minimal() +
      labs(title = title,
           x = "Time",
           y = "Value") +
      theme(
        plot.title = element_text(hjust = 0.5, color = "#ffffff"),
        panel.background = element_rect(fill = "#f5f5f5"),  # Light gray background
        plot.background = element_rect(fill = "#303030"),   # Keep dark outer background
        panel.grid.major = element_line(color = "#dddddd"), # Lighter grid lines
        panel.grid.minor = element_line(color = "#eeeeee"), # Lighter minor grid lines
        axis.text = element_text(color = "#ffffff"),        # White axis text
        axis.title = element_text(color = "#ffffff")        # White axis titles
      )

    # Convert to plotly with custom hover template
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        plot_bgcolor = "#f5f5f5",    # Light gray plot background
        paper_bgcolor = "#303030",    # Keep dark outer background
        font = list(color = "#ffffff"),
        xaxis = list(
          gridcolor = "#dddddd",
          zerolinecolor = "#dddddd"
        ),
        yaxis = list(
          gridcolor = "#dddddd",
          zerolinecolor = "#dddddd"
        )
      ) %>%
      config(displayModeBar = FALSE)
  }

  # Render individual and group plots
  output$time_series_plots <- renderUI({
    req(processed_data(), input$selected_series)

    plot_outputs <- list()

    # Individual plots
    for (series in input$selected_series) {
      plot_outputs[[length(plot_outputs) + 1]] <- div(class = "plot-container",
                                                      plotlyOutput(paste0("plot_", series))
      )
    }

    # Group plots
    if (input$num_groups > 0) {
      for (group_idx in 1:input$num_groups) {
        plot_outputs[[length(plot_outputs) + 1]] <- div(class = "plot-container",
                                                        plotlyOutput(paste0("plot_group_", group_idx))
        )
      }
    }

    do.call(tagList, plot_outputs)
  })

  # Generate individual plots
  observe({
    req(processed_data(), input$selected_series)

    for (series in input$selected_series) {
      local({
        local_series <- series
        output[[paste0("plot_", local_series)]] <- renderPlotly({
          generate_plot(processed_data()[[local_series]],
                        paste(gsub("_", " ", tools::toTitleCase(local_series)), "Series"))
        })
      })
    }
  })

  # Generate group plots
  observe({
    req(processed_data(), input$num_groups > 0)

    for (group_idx in 1:input$num_groups) {
      local({
        local_group_idx <- group_idx
        output[[paste0("plot_group_", local_group_idx)]] <- renderPlotly({
          group_input_id <- paste0("group_", local_group_idx, "_series")
          selected_series <- input[[group_input_id]]
          req(length(selected_series) > 0)

          # Combine data for plotting
          plot_data <- lapply(selected_series, function(series) {
            data.frame(
              time = seq_along(processed_data()[[series]]),
              value = as.numeric(processed_data()[[series]]),
              series = series
            )
          }) %>% bind_rows()

          p <- ggplot(plot_data, aes(x = time, y = value, color = series)) +
            geom_line(size = 0.25) +
            theme_minimal() +
            labs(title = paste("Group", local_group_idx, "Comparison"),
                 x = "Time",
                 y = "Value") +
            theme(
              plot.title = element_text(hjust = 0.5, color = "#ffffff"),
              panel.background = element_rect(fill = "#f5f5f5"),  # Light gray background
              plot.background = element_rect(fill = "#303030"),   # Keep dark outer background
              panel.grid.major = element_line(color = "#dddddd"), # Lighter grid lines
              panel.grid.minor = element_line(color = "#eeeeee"), # Lighter minor grid lines
              axis.text = element_text(color = "#ffffff"),        # White axis text
              axis.title = element_text(color = "#ffffff"),       # White axis titles
              legend.position = "bottom",
              legend.background = element_rect(fill = "#303030"),
              legend.text = element_text(color = "#ffffff")
            ) +
            scale_color_brewer(palette = "Set1")

          ggplotly(p, tooltip = c("x", "y", "color")) %>%
            layout(
              plot_bgcolor = "#f5f5f5",    # Light gray plot background
              paper_bgcolor = "#303030",    # Keep dark outer background
              font = list(color = "#ffffff"),
              xaxis = list(
                gridcolor = "#dddddd",
                zerolinecolor = "#dddddd"
              ),
              yaxis = list(
                gridcolor = "#dddddd",
                zerolinecolor = "#dddddd"
              ),
              legend = list(
                bgcolor = "#303030",
                font = list(color = "#ffffff")
              )
            ) %>%
            config(displayModeBar = FALSE)
        })
      })
    }
  })

  # Generate summary statistics
  output$summary_stats <- renderPrint({
    req(processed_data(), input$show_statistics)

    cat("Summary Statistics:\n\n")
    for (series in names(processed_data())) {
      cat(paste("\n", tools::toTitleCase(gsub("_", " ", series)), ":\n"))
      print(summary(processed_data()[[series]]))
      cat("\n")
    }
  })
}
