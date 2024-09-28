library(devtools)
library(shiny)
library(ggplot2)
library(colourpicker)
library(showtext)
library(ggpubr)
library(rstatix)
library(curl)
library(dunn.test)
library(EnhancedVolcano)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(vegan)
library(tidyverse)
library(pairwiseAdonis)
library(RColorBrewer)
library(reshape2)
library(BiocManager)
options(repos = BiocManager::repositories())

# Load some fonts
font_add_google("Tinos", "Times New Roman")
showtext_auto()

# Box Plot: example dataset
boxplot_default_data <- data.frame(
  dose = factor(c(0.5, 0.5, 0.5, 1, 1, 1, 2, 2, 2, 0.5, 0.5, 0.5, 1, 1, 1, 2, 2, 2)),
  len = c(4.2, 11.5, 7.3, 16.5, 16.5, 15.2, 19.7, 23.3, 23.6, 15.2, 21.5, 17.6, 22.4, 25.8, 19.7, 28.5, 33.9, 30.9),
  supp = factor(c('VC', 'VC', 'VC', 'VC', 'VC', 'VC', 'VC', 'VC', 'VC', 'OJ', 'OJ', 'OJ', 'OJ', 'OJ', 'OJ', 'OJ', 'OJ', 'OJ'))
)

# Volcano Plot: example dataset
set.seed(123)
n_genes <- 1000
log2FC <- seq(-5, 5, length.out = n_genes)
pvalues <- 10^-(abs(log2FC)^1.5 + rnorm(n_genes, mean = 0, sd = 0.5))
example_volcano_data <- data.frame(
  gene = paste0("Gene_", 1:n_genes),
  log2FoldChange = log2FC,
  pvalue = pvalues,
  padj = p.adjust(pvalues, method = "BH")
)

# PCA Plot: example dataset
set.seed(123)
n_samples <- 100
n_features <- 20
n_groups <- 3

generate_group_data <- function(n, features, mean, sd) {
  matrix(rnorm(n * features, mean = mean, sd = sd), nrow = n)
}

group1 <- generate_group_data(n_samples %/% 3, n_features, mean = 0, sd = 1)
group2 <- generate_group_data(n_samples %/% 3, n_features, mean = 2, sd = 1.5)
group3 <- generate_group_data(n_samples - 2*(n_samples %/% 3), n_features, mean = -1, sd = 0.5)

data <- rbind(group1, group2, group3)
groups <- rep(paste0("Group", 1:n_groups), c(nrow(group1), nrow(group2), nrow(group3)))
colnames(data) <- paste0("Feature", 1:n_features)
sample_names <- paste0("Sample", 1:nrow(data))
example_pca_data <- data.frame(Sample = sample_names, Group = groups, data)

# Define UI for application
ui <- navbarPage(
  title = "SimpleViz",
  # Box Plot
  tabPanel("Box/Violin/Dot Plot",
           titlePanel("Box/Violin/Dot Plot with Jittered Points"),
           tags$head(
             tags$style(HTML("
              .button-space {
                margin-bottom: 20px;
              }
              .col-sm-4 {
                position: sticky;
                top: 60px;
                height: calc(100vh - 60px);
                overflow-y: auto;
              }
              .col-sm-8 {
                height: calc(100vh - 60px);
                overflow-y: auto;
              }
            "))
           ),
           sidebarLayout(
             position = "left",
             sidebarPanel(
               textAreaInput("matrix_input", "Paste your matrix data (tab-separated):", 
                             rows = 10, 
                             placeholder = "A\tB\tC\tD\n1\t5\t4\t1\n2\t6\t4\t2\n1\t5\t4\t3\n2\t6\t3\t1\n3\t5\t4\t2\n1\t5\t3\t3\n2\t5\t4\t1"),
               div(class = "button-space",
                   fluidRow(
                     column(5, actionButton("submit", "Submit Data")),
                     column(6, downloadButton("download_example", "Example Data"))
                   )
               ),
               selectInput("plot_type", "Select Plot Type:", 
                           choices = c("Box Plot", "Violin Plot", "Dot Plot"), 
                           selected = "Box Plot"),
               selectInput("x_var", "Select X-axis Variable:", choices = names(boxplot_default_data), selected = "dose"),
               selectInput("y_var", "Select Y-axis Variable:", choices = names(boxplot_default_data), selected = "len"),
               numericInput("ymin", "Y-axis minimum:", value = 0),
               numericInput("ymax", "Y-axis maximum:", value = 50),
               textInput("xlab", "X-axis Label:", value = "Dose"),
               textInput("ylab", "Y-axis Label:", value = "Length"),
               selectInput("stat_method", "Statistical Method:", 
                           choices = c("t-test" = "t.test", 
                                       "ANOVA" = "anova", 
                                       "Kruskal-Wallis" = "kruskal.test"),
                           selected = "t.test"),
               uiOutput("x_order_input"),
               uiOutput("color_inputs"),
               actionButton("apply_colors", "Apply Colors"),
               sliderInput("pointSize", "Data Point Size:", min = 0, max = 5, value = 2, step = 0.1),
               sliderInput("barWidth", "Bar Width:", min = 0.1, max = 1, value = 0.7, step = 0.05),
               sliderInput("lineThickness", "Box/Violin Line Thickness:", min = 0, max = 2, value = 1.0, step = 0.05),
               sliderInput("fontSize", "Font Size:", min = 6, max = 24, value = 12, step = 1),
               sliderInput("plotWidth", "Plot Width (pixels):", value = 400, min = 200, max = 2000, step = 10),
               sliderInput("plotHeight", "Plot Height (pixels):", value = 600, min = 200, max = 1500, step = 10)
             ),
             mainPanel(
               uiOutput("dynamic_output")
             )
           )
  ),
  # Volcano Plot
  tabPanel("Volcano Plot",
           titlePanel("Volcano Plot"),
           sidebarLayout(
             sidebarPanel(
               fileInput("volcano_file", "Upload your TSV file", 
                         accept = c("text/tab-separated-values", "text/plain", ".tsv", ".txt")),
               downloadButton("download_example", "Example Data"),
               conditionalPanel(
                 condition = "input.data_source == 'Upload file'",
                 fileInput("file", "Upload TSV file", 
                           accept = c("text/tab-separated-values",
                                      "text/plain",
                                      ".tsv",
                                      ".txt"))
               ),
               selectInput("x_col", "Select log2 Fold Change column", ""),
               selectInput("y_col", "Select p-value column", ""),
               selectInput("label_col", "Select label column", ""),
               numericInput("pCutoff", "p-value cutoff", value = 0.05, min = 0, max = 1),
               numericInput("FCcutoff", "Fold change cutoff", value = 1, min = 0),
               textInput("highlight_genes", "Highlight genes (comma-separated)", "Gene_300,Gene_301"),
               sliderInput("x.range", "X-axis range:",
                           min = -10, max = 10, value = c(-5, 5)),
               sliderInput("y.range", "Y-axis range:",
                           min = 0, max = 310, value = c(0, 15)),
               sliderInput("point_size", "Data point size:",
                           min = 0.1, max = 5, value = 1, step = 0.1),
               numericInput("plot_width", "Plot width (pixels):", 
                            value = 800, min = 400, max = 2000),
               numericInput("plot_height", "Plot height (pixels):", 
                            value = 800, min = 300, max = 2000),
               colourInput("col_ns", "Color for non-significant", value = "grey"),
               colourInput("col_log2fc", "Color for log2FC significant", value = "red"),
               colourInput("col_p", "Color for p-value significant", value = "blue"),
               colourInput("col_both", "Color for both significant", value = "green")
             ),
             mainPanel(
               plotOutput("volcano_plot")
             )
           )
  ),
  # PCA Plot
  tabPanel("PCA Plot",
           titlePanel("PCA Plot"),
           sidebarLayout(
             sidebarPanel(
               fileInput("pca_file", "Upload your TSV file",
                         accept = c("text/tab-separated-values",
                                    "text/plain",
                                    ".tsv", ".txt")),
               downloadButton("downloadPCAData", "Example Data"),
               selectInput("x_axis", "X-axis:", choices = paste0("Dim", 1:5), selected = "Dim1"),
               selectInput("y_axis", "Y-axis:", choices = paste0("Dim", 1:5), selected = "Dim2"),
               sliderInput("x_range", "X-axis range:", min = -10, max = 10, value = c(-10, 10), step = 1),
               sliderInput("y_range", "Y-axis range:", min = -10, max = 10, value = c(-10, 10), step = 1),
               sliderInput("point_size", "Point Size:", min = 1, max = 5, value = 2, step = 0.5),
               sliderInput("axis_font_size", "Axis Font Size:", min = 8, max = 20, value = 12, step = 1),
               checkboxInput("add_ellipse", "Add ellipses", value = TRUE),
               checkboxInput("show_points", "Show points without text", value = TRUE),
               sliderInput("plot_width", "Plot Width:", min = 400, max = 1200, value = 800, step = 50),
               sliderInput("plot_height", "Plot Height:", min = 300, max = 1000, value = 600, step = 50),
               uiOutput("color_pickers")
             ),
             mainPanel(
               plotOutput("pca_plot", width = "100%", height = "auto"),
               verbatimTextOutput("permanova_result"),
               verbatimTextOutput("pairwise_result")
             )
           )
  ),
  tabPanel("Citation", 
           h3("About this app"),
           p("SimpleViz allows you to create box/violin/dot plots, volcano plot and PCA plot with statistical analysis.")
  )
)

server <- function(input, output, session) {
  
  # Box plot: plotting
  data <- reactiveVal(boxplot_default_data)
  color_palette <- reactiveVal(NULL)
  
  output$dynamic_output <- renderUI({
    tagList(
      plotOutput("plot", width = "100%", height = paste0(input$plotHeight, "px")),
      htmlOutput("save_message"),
      verbatimTextOutput("ttest_results")
    )
  })
  
  # Box plot: Define default colors
  default_colors <- c("#F8766D", "#00BA38", "#619CFF", "#F564E3", "#00BFC4", "#B79F00")

  # Box plot: Initialize color palette
  observe({
    req(data())
    groups <- levels(as.factor(data()[[input$x_var]]))
    if (is.null(color_palette())) {
      color_palette(setNames(default_colors[1:length(groups)], groups))
    }
  })
  
  # Box plot: Generate boxplot example data
  example_data <- data.frame(
    A = rep(c(1, 2, 3), each = 100),
    B = c(rnorm(100, 5, 1), rnorm(100, 6, 1), rnorm(100, 5, 1)),
    C = c(rnorm(100, 4, 0.5), rnorm(100, 4, 0.5), rnorm(100, 3, 0.5)),
    D = c(sample(1:3, 100, replace = TRUE), 
          sample(1:3, 100, replace = TRUE), 
          sample(1:3, 100, replace = TRUE))
  )
  
  # Box plot: Create download handler for example file
  output$download_example <- downloadHandler(
    filename = function() {
      "boxplot_example_data.txt"
    },
    content = function(file) {
      write.table(example_data, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
  
  # Box plot: submit
  observeEvent(input$submit, {
    req(input$matrix_input)
    tryCatch({
      # Transform the data using melt()
      matrix_data <- read.table(text = input$matrix_input, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      melted_data <- melt(matrix_data, variable.name = "type", value.name = "value")
      data(melted_data)
      
      # Automatically set x and y variables
      x_var <- names(melted_data)[1]
      y_var <- names(melted_data)[2]
      
      # Update UI elements
      updateSelectInput(session, "x_var", choices = names(melted_data), selected = x_var)
      updateSelectInput(session, "y_var", choices = names(melted_data), selected = y_var)
      
      # Update x and y axis labels
      updateTextInput(session, "xlab", value = x_var)
      updateTextInput(session, "ylab", value = y_var)
      
      # Update y-axis range
      y_min <- min(melted_data[[y_var]], na.rm = TRUE) - 10
      y_max <- max(melted_data[[y_var]], na.rm = TRUE) + 10
      updateNumericInput(session, "ymin", value = floor(y_min))
      updateNumericInput(session, "ymax", value = ceiling(y_max))
      
      # Initialize color palette
      groups <- levels(as.factor(melted_data[[x_var]]))
      color_palette(setNames(default_colors[1:length(groups)], groups))
      
    }, error = function(e) {
      showNotification(paste("Error reading data:", e$message), type = "error")
    })
  })
  
  # Box plot: change XY variable
  observeEvent(input$x_var, {
    updateTextInput(session, "xlab", value = input$x_var)
  })
  observeEvent(input$y_var, {
    updateTextInput(session, "ylab", value = input$y_var)
  })
  
  # Box plot: change X order
  output$x_order_input <- renderUI({
    req(data(), input$x_var)
    x_levels <- levels(as.factor(data()[[input$x_var]]))
    tagList(
      tags$b("X-axis Order:"),
      sortable::rank_list(
        text = "Drag to reorder",
        labels = x_levels,
        input_id = "x_order"
      )
    )
  })
  
  # Box plot: change color
  output$color_inputs <- renderUI({
    req(data(), input$x_var)
    groups <- levels(as.factor(data()[[input$x_var]]))
    current_palette <- color_palette() %||% setNames(default_colors[1:length(groups)], groups)
    
    color_inputs <- lapply(seq_along(groups), function(i) {
      colourInput(
        inputId = paste0("color", i),
        label = paste("Color for", groups[i]),
        value = current_palette[groups[i]]
      )
    })
    
    do.call(tagList, color_inputs)
  })
  
  # Box plot: Color palette update
  observeEvent(input$apply_colors, {
    req(data(), input$x_var)
    groups <- levels(as.factor(data()[[input$x_var]]))
    new_palette <- sapply(seq_along(groups), function(i) input[[paste0("color", i)]])
    color_palette(setNames(new_palette, groups))
  })
  
  # Box Plot: Plot reactive expression
  plot_data <- reactive({
    req(data(), input$x_var, input$y_var, input$plot_type, input$x_order)
    
    data_plot <- data()
    data_plot[[input$x_var]] <- factor(data_plot[[input$x_var]], levels = input$x_order)
    
    groups <- levels(data_plot[[input$x_var]])
    
    current_palette <- color_palette()
    if (is.null(current_palette) || !all(groups %in% names(current_palette))) {
      current_palette <- setNames(default_colors[1:length(groups)], groups)
      color_palette(current_palette)
    }
    
    list(
      data = data_plot,
      x_var = input$x_var,
      y_var = input$y_var,
      plot_type = input$plot_type,
      groups = groups,
      colors = color_palette() %||% setNames(default_colors[1:length(groups)], groups)
    )
  })
  
  # Box Plot: Plot rendering
  output$plot <- renderPlot({
    plot_info <- plot_data()
    
    p <- ggplot(plot_info$data, aes_string(x = plot_info$x_var, y = plot_info$y_var, group = plot_info$x_var)) +
      labs(title = paste(plot_info$plot_type),
           x = input$xlab, y = input$ylab) +
      theme_pubr(base_size = input$fontSize) +
      theme(
        plot.title = element_text(hjust = 0.5, size = input$fontSize * 1.3),
        axis.title = element_text(size = input$fontSize * 1.2),
        axis.text = element_text(size = input$fontSize),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(size = 1, color = "black"), 
        axis.ticks = element_line(size = 1, color = "black"), 
        legend.title = element_text(size = input$fontSize),
        legend.text = element_text(size = input$fontSize * 0.8)
      ) +
      scale_x_discrete(limits = plot_info$groups) +
      scale_fill_manual(values = color_palette(), name = input$xlab) +
      coord_cartesian(ylim = c(input$ymin, input$ymax))
    
    if (plot_info$plot_type == "Box Plot") {
      p <- p + geom_boxplot(aes(fill = .data[[plot_info$x_var]]), 
                            width = input$barWidth, 
                            size = input$lineThickness, 
                            outlier.shape = NA)
    } else if (plot_info$plot_type == "Violin Plot") {
      p <- p + geom_violin(aes(fill = .data[[plot_info$x_var]]), 
                           width = input$barWidth, 
                           size = input$lineThickness, 
                           trim = FALSE)
    } else if (plot_info$plot_type == "Dot Plot") {
      p <- p + geom_dotplot(aes(fill = .data[[plot_info$x_var]]), 
                            binaxis = 'y', 
                            stackdir = 'center', 
                            dotsize = input$pointSize * 0.4,
                            binwidth = (input$ymax - input$ymin) / 50)
    }
    
    if (plot_info$plot_type %in% c("Box Plot", "Violin Plot")) {
      p <- p + geom_jitter(color = "black", width = 0.2, size = input$pointSize, alpha = 0.7)
    }
    
    p <- p + scale_fill_manual(values = plot_info$colors, name = input$xlab)
    
    # Box Plot: Add statistical test
    if (length(plot_info$groups) >= 2) {
      formula <- as.formula(paste(plot_info$y_var, "~", plot_info$x_var))
      
      if (input$stat_method == "t.test") {
        stat_test <- compare_means(formula, data = plot_info$data, method = "t.test")
        comparisons <- combn(plot_info$groups, 2, simplify = FALSE)
        
        y_max <- max(plot_info$data[[plot_info$y_var]])
        step <- (input$ymax - y_max) / (length(comparisons) + 1)
        y_positions <- seq(y_max + step, by = step, length.out = length(comparisons))
        
        p <- p + stat_compare_means(comparisons = comparisons,
                                    label = "p.signif", 
                                    method = "t.test",
                                    label.y = y_positions)
      } else if (input$stat_method %in% c("anova", "kruskal.test")) {
        stat_test <- compare_means(formula, data = plot_info$data, method = input$stat_method)
        p <- p + stat_compare_means(label.y = max(plot_info$data[[plot_info$y_var]]) * 1.4,
                                    method = input$stat_method)
      }
    }
    
    p
  }, width = function() input$plotWidth, height = function() input$plotHeight)
  
  output$save_message <- renderUI({
    HTML("<p style='text-align: center; color: #666; font-style: italic;'>Right-click on the plot and select 'Save image as...' to download the plot as an image file.</p>")
  })
  
  # Box Plot: show statistical result
  output$ttest_results <- renderPrint({
    req(data(), input$x_var, input$y_var, input$stat_method)
    data_test <- data()
    data_test[[input$x_var]] <- as.factor(data_test[[input$x_var]])
    data_test[[input$y_var]] <- as.numeric(as.character(data_test[[input$y_var]]))
    groups <- levels(data_test[[input$x_var]])
    
    if (length(groups) >= 2) {
      formula <- as.formula(paste(input$y_var, "~", input$x_var))
      
      if (input$stat_method == "t.test") {
        stat_test <- compare_means(formula, data = data_test, method = "t.test")
        
        cat("t-test Results (p-values):\n")
        for (i in 1:nrow(stat_test)) {
          cat(paste(stat_test$group1[i], "vs", stat_test$group2[i], ":", 
                    format(stat_test$p[i], scientific = TRUE, digits = 4), "\n"))
        }
      } else if (input$stat_method == "anova") {
        # ANOVA
        anova_result <- aov(formula, data = data_test)
        cat("ANOVA Results:\n")
        print(summary(anova_result))
        
        # Tukey's HSD for pairwise comparisons
        tukey_result <- TukeyHSD(anova_result)
        cat("\nTukey's HSD Pairwise Comparisons:\n")
        print(tukey_result[[1]])
        
      } else if (input$stat_method == "kruskal.test") {
        # Kruskal-Wallis test
        kruskal_result <- kruskal.test(formula, data = data_test)
        cat("Kruskal-Wallis Test Results:\n")
        print(kruskal_result)
        
        # Dunn's test for pairwise comparisons
        if (!requireNamespace("dunn.test", quietly = TRUE)) {
          install.packages("dunn.test")
        }
        library(dunn.test)
        dunn_result <- dunn.test(data_test[[input$y_var]], data_test[[input$x_var]], method = "bonferroni")
        cat("\nDunn's Test Pairwise Comparisons:\n")
        print(dunn_result)
      }
    } else {
      cat("Not enough groups to perform statistical tests.")
    }
  })
  
  # Volcano Plot: data selection
  volcano_data <- reactive({
    if (is.null(input$volcano_file)) {
      return(example_volcano_data)
    } else {
      read.delim(input$volcano_file$datapath, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    }
  })
  
  # Calculate axis limits based on data
  axis_limits <- reactive({
    data <- volcano_data()
    zero_present <- any(data$padj == 0, na.rm = TRUE)
    
    if (zero_present) {
      y_max <- 310
    } else {
      y_max <- -log10(max(data$padj[data$padj > 0], na.rm = TRUE))
    }
    x_min <- min(data$log2FoldChange, na.rm = TRUE)
    x_max <- max(data$log2FoldChange, na.rm = TRUE)
    list(y_max = y_max, x_min = x_min, x_max = x_max)
  })
  
  observe({
    limits <- axis_limits()
    
    updateSelectInput(session, "x_col", choices = "log2FoldChange")
    updateSelectInput(session, "y_col", choices = c("padj", "pvalue"))
    updateSelectInput(session, "label_col", choices = "gene")
    
    updateSliderInput(session, "y.range", 
                      min = 0, max = 310, 
                      value = c(0, max(15, ceiling(limits$y_max))))
    
    updateSliderInput(session, "x.range", 
                      min = -10, max = 10, 
                      value = c(floor(limits$x_min), ceiling(limits$x_max)))
    
    if (is.null(input$volcano_file)) {
      updateSelectInput(session, "x_col", selected = "log2FoldChange")
      updateSelectInput(session, "y_col", selected = "padj")
      updateSelectInput(session, "label_col", selected = "gene")
    }
  })
  
  # Volcano Plot: plotting
  output$volcano_plot <- renderPlot({
    req(input$x_col, input$y_col, input$label_col)
    
    highlight_genes <- unlist(strsplit(input$highlight_genes, ","))
    highlight_genes <- trimws(highlight_genes)
    
    EnhancedVolcano(volcano_data(),
                    lab = volcano_data()[[input$label_col]],
                    x = input$x_col,
                    y = input$y_col,
                    pCutoff = input$pCutoff,
                    FCcutoff = input$FCcutoff,
                    pointSize = input$point_size,
                    labSize = 4.0,
                    title = "Volcano Plot",
                    subtitle = "Created with EnhancedVolcano",
                    xlim = input$x.range,
                    ylim = input$y.range,
                    colAlpha = 1,
                    col = c(input$col_ns, input$col_log2fc, input$col_p, input$col_both),
                    selectLab = highlight_genes,
                    drawConnectors = TRUE,
                    boxedLabels = TRUE) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }, width = function() input$plot_width, height = function() input$plot_height)

  # Volcano Plot: download example data 
  output$download_example <- downloadHandler(
    filename = function() {
      "example_volcano_data.tsv"
    },
    content = function(file) {
      write.table(example_volcano_data, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
  
  # PCA Plot: data selection
  pca_dataset <- reactive({
    if (is.null(input$pca_file)) {
      return(example_pca_data)
    } else {
      df <- read.delim(input$pca_file$datapath, sep = "\t", header = TRUE, check.names = FALSE)
     
      df_long <- df %>%
        pivot_longer(cols = -Sample, names_to = "Variable", values_to = "Value") %>%
        pivot_wider(names_from = "Sample", values_from = "Value")
      
      df_long <- df_long %>%
        mutate(across(3:ncol(df_long), as.numeric))
      
      rownames(df_long) <- seq_len(nrow(df_long))
      return(df_long)
    }
  })
  
  # PCA Plot: color selection
  output$color_pickers <- renderUI({
    df <- pca_dataset()
    groups <- unique(df$Group)
    color_pickers <- lapply(seq_along(groups), function(i) {
      colourInput(
        inputId = paste0("color_", i),
        label = paste("Color for", groups[i]),
        value = brewer.pal(8, "Set2")[i %% 8 + 1]
      )
    })
    do.call(tagList, color_pickers)
  })
  
  selected_palette <- reactive({
    df <- pca_dataset()
    groups <- unique(df$Group)
    sapply(seq_along(groups), function(i) input[[paste0("color_", i)]])
  })
  
  pca_result <- reactive({
    df <- pca_dataset()
    PCA(df[, -(1:2)], graph = FALSE)
  })
  
  # PCA Plot: option change
  observe({
    res.pca <- pca_result()
    x_axis <- which(paste0("Dim", 1:5) == input$x_axis)
    y_axis <- which(paste0("Dim", 1:5) == input$y_axis)
    
    x_range <- range(res.pca$ind$coord[, x_axis])
    y_range <- range(res.pca$ind$coord[, y_axis])
    
    x_padding <- diff(x_range) * 0.2
    y_padding <- diff(y_range) * 0.2
    x_range_initial <- x_range + c(-x_padding, x_padding)
    y_range_initial <- y_range + c(-y_padding, y_padding)
    
    x_padding_wide <- diff(x_range) * 0.5
    y_padding_wide <- diff(y_range) * 0.5
    x_range_wide <- x_range + c(-x_padding_wide, x_padding_wide)
    y_range_wide <- y_range + c(-y_padding_wide, y_padding_wide)
    
    x_min <- floor(min(x_range_wide) * 3)
    x_max <- ceiling(max(x_range_wide) * 3)
    y_min <- floor(min(y_range_wide) * 3)
    y_max <- ceiling(max(y_range_wide) * 3)
    
    updateSliderInput(session, "x_range", 
                      min = x_min, 
                      max = x_max, 
                      value = c(floor(x_range_initial[1]), ceiling(x_range_initial[2])))
    updateSliderInput(session, "y_range", 
                      min = y_min, 
                      max = y_max, 
                      value = c(floor(y_range_initial[1]), ceiling(y_range_initial[2])))
  })
  
  # PCA Plot: plotting
  output$pca_plot <- renderPlot({
    res.pca <- pca_result()
    df <- pca_dataset()
    
    x_axis <- which(paste0("Dim", 1:5) == input$x_axis)
    y_axis <- which(paste0("Dim", 1:5) == input$y_axis)
    
    plot <- fviz_pca_ind(res.pca,
                         title = "PCA Plot",
                         axes = c(x_axis, y_axis),
                         geom.ind = if(input$show_points) "point" else c("point", "text"),
                         col.ind = df$Group,
                         palette = selected_palette(),
                         addEllipses = input$add_ellipse,
                         ellipse.level = 0.9,
                         legend.title = "Groups",
                         mean.point = FALSE,
                         pointsize = input$point_size) +
      theme(axis.line = element_line(color = "black"),
            panel.border = element_blank(),
            panel.background = element_blank(),
#            panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank(),
            axis.text = element_text(size = input$axis_font_size),
            axis.title = element_text(size = input$axis_font_size + 2)) +
      scale_x_continuous(position = "bottom", limits = input$x_range) +
      scale_y_continuous(position = "left", limits = input$y_range)
    
    plot
  }, width = function() input$plot_width, height = function() input$plot_height)

  # PCA Plot: statistical test
  output$permanova_result <- renderPrint({
    res.pca <- pca_result()
    df <- pca_dataset()
    pca_coords <- res.pca$ind$coord
    permanova_result <- adonis2(pca_coords ~ Group, data = df, permutations = 999)
    cat("PERMANOVA Results:\n")
    print(permanova_result)
    cat("\nOverall P-value:", permanova_result$`Pr(>F)`[1])
  })
  
  # PCA Plot: pairwise statistical test
  output$pairwise_result <- renderPrint({
    res.pca <- pca_result()
    df <- pca_dataset()
    pca_coords <- res.pca$ind$coord
    pairwise_result <- pairwise.adonis(pca_coords, df$Group, p.adjust.m = "bonferroni")
    cat("Pairwise PERMANOVA Results:\n")
    print(pairwise_result)
  })
  
  # PCA Plot: download example data
  output$downloadPCAData <- downloadHandler(
    filename = function() {
      "example_pca_data.tsv"
    },
    content = function(file) {
      #write.table(example_pca_data, file, row.names = FALSE, sep = "\t")
      file.copy("example_pca_data.tsv", file)
    }
  )
  
  # PCA Plot: submit user data
  observeEvent(input$pca_file, {
    if (!is.null(input$pca_file)) {
      df <- read.delim(input$pca_file$datapath, sep = "\t", header = TRUE, check.names = FALSE)
      
      df_long <- df %>%
        pivot_longer(cols = -Sample, names_to = "Variable", values_to = "Value") %>%
        pivot_wider(names_from = "Sample", values_from = "Value")
      
      df_long <- df_long %>%
        mutate(across(3:ncol(df_long), as.numeric))
      
      rownames(df_long) <- seq_len(nrow(df_long))
      num_cols <- ncol(df_long) - 2
      updateSelectInput(session, "x_axis", choices = paste0("Dim", 1:num_cols), selected = "Dim1")
      updateSelectInput(session, "y_axis", choices = paste0("Dim", 1:num_cols), selected = "Dim2")
    }
  })
  
}

shinyApp(ui = ui, server = server)
