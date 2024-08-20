# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(EnhancedVolcano)

# Define server logic
server <- function(input, output, session) {
    # Observe the update button
    observeEvent(input$update, {
        # Read data.fc file
        req(input$data_fc_file)
        data.fc <- read.csv(input$data_fc_file$datapath)
        data.fc$Significance <- ifelse(data.fc$p.value < 0.05, "Significant", "NS")
        data.fc$Significance2 <- ifelse(data.fc$p.value < 0.05, "*", "")
        data.fc$log2FC <- log2(data.fc$FoldChange)
        
        if (input$plot_type == "Bar Plot") {
            output$main_plot <- renderPlot({
                data.sig <- subset(data.fc, data.fc$p.value < input$p_threshold)
                ggplot(data.sig, aes(x = Genes, y = FoldRegulation)) + 
                    geom_bar(stat = "identity", position = position_dodge(), fill = "#4292c6") +
                    labs(title = paste("Significant Genes \n p <", input$p_threshold)) +
                    ylab("Fold Regulation") + xlab("Genes") +
                    theme_minimal(base_size = 16) +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
            }, height = 600, width = 800)
        } else if (input$plot_type == "Volcano Plot") {
            if (input$volcano_type == "Fold Regulation") {
                output$main_plot <- renderPlot({
                    EnhancedVolcano(data.fc,
                                    lab = data.fc$Gene,
                                    x = 'FoldRegulation',
                                    y = 'p.value',
                                    FCcutoff = 1.4,
                                    pCutoff = 0.05,
                                    xlab = bquote("Fold Regulation"),
                                    ylim = c(0, 4),
                                    max.overlaps = 40,
                                    pointSize = 4,
                                    labSize = 4.5,
                                    drawConnectors = TRUE,
                                    legendLabels = c("NS", "Fold Regulation", "p-value", "p-value and Fold Regulation"),
                                    title = data.fc$Group,subtitle = "")
                }, height = 600, width = 800)
            } else {
                output$main_plot <- renderPlot({
                    EnhancedVolcano(data.fc,
                                    lab = data.fc$Gene,
                                    x = 'log2FC',
                                    y = 'p.value',
                                    FCcutoff = 0.5,
                                    pCutoff = 0.05,
                                    ylim = c(0, 4),
                                    max.overlaps = 40,
                                    pointSize = 4,
                                    labSize = 4.5,
                                    drawConnectors = TRUE,
                                    title = data.fc$Group,subtitle = "")
                }, height = 600, width = 800)
            }
        } else if (input$plot_type == "Heatmap") {
            req(input$selected_genes_file)
            selectedgenes <- read.csv(input$selected_genes_file$datapath)
            output$main_plot <- renderPlot({
                data <- data.fc %>% select(Genes, p.value, log2FC, FoldRegulation, Group, Significance2)
                data <- data %>% 
                    filter(Genes %in% selectedgenes$select) %>% 
                    arrange(match(Genes, selectedgenes$select))
                data$Genes <- factor(data$Genes, levels = rev(selectedgenes$select))
                
                ggplot(data, aes(Group, Genes)) +
                    geom_tile(aes(fill = FoldRegulation), color = "white") +
                    scale_fill_gradient2(low = "#3288bd", high = "#f03b20", mid = "white", midpoint = 0,
                                         limit = c(min(data$FoldRegulation), max(data$FoldRegulation)),
                                         space = "Lab", name = "FoldRegulation") +
                    geom_text(aes(label = Significance2), color = "black", size = 3) +
                    theme_minimal(base_size = 16) +
                    labs(title = "Heatmap of \nFoldRegulation")
            }, height = 1200, width = 300)
        }
    })
}

