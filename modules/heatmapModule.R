# modules/heatmapModule.R

# 0. example dataset

set.seed(123)  

genes   <- paste0("Gene", 1:5)
samples <- paste0("Sample", 1:3)

mat_data <- matrix(runif(5 * 3, min = 0, max = 10), nrow = 5, ncol = 3)
rownames(mat_data) <- genes
colnames(mat_data) <- samples

# 1. UI
heatmapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Heatmap"),
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
      sidebarPanel(
        position = "left",
        
        # (1) Tab-separated text input
        textAreaInput(ns("matrix_input"), "Paste your matrix data (tab-separated):",
                      rows = 10,
                      placeholder = "gene\tSample1\tSample2\tSample3\nGene1\t2.875\t0.455\t9.563\nGene2\t7.883\t5.281\t4.533\nGene3\t4.089\t8.924\t6.775\nGene4\t8.830\t5.514\t5.726\nGene5\t9.404\t4.566\t1.029"),
        
        div(class = "button-space",
            fluidRow(
              column(5, 
                     actionButton(ns("submit"), "Submit Data")),
              column(6, 
                     downloadButton(ns("download_example"), "Example Data"))
            )
        ),
        
        # (2,3) File upload
        fileInput(ns("heatmap_file"), "Upload your TSV file",
                  accept = c("text/tab-separated-values", 
                             "text/plain", ".tsv", ".txt")),
        hr(),
        
        # (4) Heatmap parameter settings
        selectInput(ns("scale_option"), "Scale:", 
                    choices = c("none", "row", "column"), 
                    selected = "none"),
        checkboxInput(ns("cluster_rows"), "Cluster Rows", value = TRUE),
        checkboxInput(ns("cluster_cols"), "Cluster Columns", value = TRUE),
        selectInput(ns("dist_method"), "Distance Method:",
                    choices = c("euclidean", "manhattan", "maximum", 
                                "canberra", "binary", "minkowski"),
                    selected = "euclidean"),
        selectInput(ns("hclust_method"), "Clustering Method:",
                    choices = c("complete", "ward.D", "ward.D2", 
                                "single", "average", "mcquitty", 
                                "median", "centroid"),
                    selected = "complete"),
        
        selectInput(ns("color_palette"), "Color Palette:", 
                    choices = c("RdBu", "Blues", "Greens", "Reds", 
                                "YlOrRd", "YlGnBu", "heat.colors"), 
                    selected = "RdBu"),
#        sliderInput(ns("num_colors"), "Number of Colors:", 
#                    min = 3, max = 100, value = 9),
        
        # **Font size settings (added)**
        sliderInput(ns("font_size"), "Font Size:",
                    min = 5, max = 20, value = 10, step = 1),
        
        # (5) Plot size settings
        sliderInput(ns("plot_width"), "Plot Width:", 
                    min = 400, max = 1200, value = 700, step = 50),
        sliderInput(ns("plot_height"), "Plot Height:", 
                    min = 300, max = 1000, value = 600, step = 50)
      ),
      mainPanel(
        plotOutput(ns("heatmap_plot"), width = "100%", height = "auto")
      )
    )
  )
}

# 2. Server

heatmapServer <- function(id, exampleHeatmapData = mat_data, exampleAnnotation = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # (A) reactiveVal to store data entered by user in text area
      parsed_text_data <- reactiveVal(NULL)
      
      # (B) Parse and store data from text area when Submit Data button is clicked
      observeEvent(input$submit, {
        req(input$matrix_input)
        
        # Read string from text area
        data_lines <- strsplit(input$matrix_input, "\n")[[1]]
        
        # Parse table with tab separator
        df <- read.table(
          textConnection(data_lines),
          sep = "\t", 
          header = TRUE,
          check.names = FALSE
        )
        
        # First column is gene names (row names), rest are sample values
        mat <- as.matrix(df[,-1])
        rownames(mat) <- df[[1]]
        
        # Assign to parsed_text_data
        parsed_text_data(mat)
      })
      
      
      # (C) reactive that returns data to be used in heatmap
      heatmap_data <- reactive({
        # 1. Use parsed data from text input if available
        if(!is.null(parsed_text_data())) {
          return(parsed_text_data())
        }
        # 2. If file is uploaded
        else if (!is.null(input$heatmap_file)) {
          df <- read.delim(
            input$heatmap_file$datapath, 
            sep = "\t", 
            header = TRUE, 
            check.names = FALSE
          )
          mat <- as.matrix(df[,-1])
          rownames(mat) <- df[[1]]
          return(mat)
        }
        # 3. Otherwise use example data
        else {
          return(exampleHeatmapData)
        }
      })
      
      # (D) Create Heatmap Plot
      output$heatmap_plot <- renderPlot({
        req(heatmap_data())
        mat <- heatmap_data()
        
        # Create color palette
        pal_name <- input$color_palette
        pal_size <- 100
        
        # Set palette based on whether it's RColorBrewer
        if (pal_name %in% rownames(brewer.pal.info)) {
          # RColorBrewer palette
          colors <- colorRampPalette(brewer.pal(min(pal_size, 9), pal_name))(pal_size)
        } else {
          # base R palettes like heat.colors
          if (pal_name == "heat.colors") {
            colors <- heat.colors(pal_size)
          } else {
            # Other exception handling (or fixed RdBu etc.)
            colors <- colorRampPalette(brewer.pal(9, "RdBu"))(pal_size)
          }
        }
        
        # annotation_col (optional)
        annotation_col <- exampleAnnotation
        if (!is.null(exampleAnnotation) && nrow(exampleAnnotation) == ncol(mat)) {
          rownames(annotation_col) <- colnames(mat)
        } else {
          annotation_col <- NA
        }
        
        # Run pheatmap
        pheatmap(
          mat,
          scale = input$scale_option,               
          cluster_rows = input$cluster_rows,
          cluster_cols = input$cluster_cols,
          color = colors,
          annotation_col = if (is.data.frame(annotation_col)) annotation_col else NULL,
          clustering_distance_rows = input$dist_method,
          clustering_distance_cols = input$dist_method,
          clustering_method = input$hclust_method,
          legend = TRUE,
          border_color = "grey80",
          main = "Heatmap",
          fontsize = input$font_size
        )
      }, width = function() input$plot_width, height = function() input$plot_height)
      
      
      # (E) Example data download
      output$download_example <- downloadHandler(
        filename = function() {
          "example_heatmap_data.tsv"
        },
        content = function(file) {
          mat <- exampleHeatmapData
          df_out <- data.frame(rownames(mat), mat, check.names = FALSE)
          colnames(df_out)[1] <- "Gene"
          write.table(df_out, file, sep = "\t", row.names = FALSE, quote = FALSE)
        }
      )
    }
  )
}
