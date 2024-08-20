library(shiny)
library(ggplot2)
library(EnhancedVolcano)
library(dplyr)

# Define UI
ui <- fluidPage(
    titlePanel("Gene Expression Analysis"),
    sidebarLayout(
        sidebarPanel(
            fileInput("data_fc_file", "Upload Data File (data.fc)", accept = c(".csv")),
            fileInput("selected_genes_file", "Upload Selected Genes File (selectedgenes)", accept = c(".csv")),
            selectInput("plot_type", "Choose Plot Type:",
                        choices = c("Bar Plot", "Volcano Plot", "Heatmap")),
            conditionalPanel(
                condition = "input.plot_type == 'Bar Plot'",
                numericInput("p_threshold", "Significance Threshold (p-value)", value = 0.05, min = 0, max = 1, step = 0.01)
            ),
            conditionalPanel(
                condition = "input.plot_type == 'Volcano Plot'",
                selectInput("volcano_type", "Volcano Plot Type:",
                            choices = c("Fold Regulation", "Log2 Fold Change"))
            ),
            actionButton("update", "Update Plot")
        ),
        mainPanel(
            plotOutput(outputId = "main_plot", width = "100%")
        )
    )
)
