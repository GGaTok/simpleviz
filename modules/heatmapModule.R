set.seed(123)  # 재현 가능한 랜덤 시드

# 1) Gene, Sample 이름 설정
genes   <- paste0("Gene", 1:5)
samples <- paste0("Sample", 1:3)

# 2) 행=유전자, 열=샘플 인 5x3 matrix 생성
mat_data <- matrix(runif(5 * 3, min = 0, max = 10), nrow = 5, ncol = 3)
rownames(mat_data) <- genes
colnames(mat_data) <- samples

# 1. UI 부분: heatmapUI()
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
        
        # 1) 텍스트 입력
        textAreaInput(ns("matrix_input"), "Paste your matrix data (tab-separated):",
                      rows = 10,
                      placeholder = "gene\tSample1\tSample2\tSample3\nGene1\t2.875\t0.455\t9.563\nGene2\t7.883\t5.281\t4.533\nGene3\t4.089\t8.924\t6.775\nGene4\t8.830\t5.514\t5.726\nGene5\t9.404\t4.566\t1.029"),
        
        
        # 2) 파일 업로드
        fileInput(ns("heatmap_file"), "Upload your TSV file",
                  accept = c("text/tab-separated-values", 
                             "text/plain", ".tsv", ".txt")),
        actionButton(ns("submit"), "Submit Data"),
        
        # 3) 예제 데이터 다운로드
        downloadButton(ns("downloadHeatmapData"), "Example Data"),
        
        hr(),
        
        # 4) 히트맵 파라미터 설정
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
        sliderInput(ns("num_colors"), "Number of Colors:", 
                    min = 3, max = 15, value = 9),
        
        # **폰트 크기 설정 (추가)**
        sliderInput(ns("font_size"), "Font Size:",
                    min = 5, max = 20, value = 10, step = 1),
        
        # 5) 플롯 크기 설정
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



# 2. Server 부분: heatmapServer()

heatmapServer <- function(id, exampleHeatmapData = mat_data, exampleAnnotation = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # 1 업로드 또는 예제 데이터 불러오기
      heatmap_data <- reactive({
        # 업로드 파일이 없으면 예제 데이터 사용
        if (is.null(input$heatmap_file)) {
          return(exampleHeatmapData)
        } else {
          df <- read.delim(input$heatmap_file$datapath, 
                           sep = "\t", header = TRUE, check.names = FALSE)
          
          # 첫 열이 유전자명, 나머지 열이 샘플값
          mat <- as.matrix(df[,-1])
          rownames(mat) <- df[[1]]
          return(mat)
        }
      })
      
      # 2 Heatmap Plot 생성
      output$heatmap_plot <- renderPlot({
        req(heatmap_data())
        mat <- heatmap_data()
        
        # 색상 팔레트 생성
        pal_name <- input$color_palette
        pal_size <- input$num_colors
        
        # 내장 팔레트들 중 선택된 이름에 따라 팔레트 구성
        if (pal_name %in% rownames(brewer.pal.info)) {
          # RColorBrewer 팔레트
          colors <- colorRampPalette(brewer.pal(min(pal_size, 9), pal_name))(pal_size)
        } else {
          # heat.colors() 등 base R 팔레트 지원
          if (pal_name == "heat.colors") {
            colors <- heat.colors(pal_size)
          } else {
            # 기타 예외처리 (RdBu, etc.)
            colors <- colorRampPalette(brewer.pal(9, "RdBu"))(pal_size)
          }
        }
        
        # annotation_col: (옵션) 열 주석
        annotation_col <- exampleAnnotation
        if (!is.null(exampleAnnotation) && nrow(exampleAnnotation) == ncol(mat)) {
          rownames(annotation_col) <- colnames(mat)
        } else {
          annotation_col <- NA
        }
        
        pheatmap(
          mat,
          scale = input$scale_option,               # "none", "row", or "column"
          cluster_rows = input$cluster_rows,
          cluster_cols = input$cluster_cols,
          color = colors,
          annotation_col = if (is.data.frame(annotation_col)) annotation_col else NULL,
          clustering_distance_rows = input$dist_method,
          clustering_distance_cols = input$dist_method,
          clustering_method = input$hclust_method,
          legend = TRUE,
          border_color = "grey80",
          main = "Heatmap Example",
          fontsize = input$font_size  
        )
      }, width = function() input$plot_width, height = function() input$plot_height)
      
      # (3) 예제 데이터 다운로드
      output$downloadHeatmapData <- downloadHandler(
        filename = function() {
          "example_heatmap_data.tsv"
        },
        content = function(file) {
          # 예제 데이터가 matrix 형태라면 TSV로 저장하기 위해 data.frame 변환
          mat <- exampleHeatmapData
          df_out <- data.frame(rownames(mat), mat, check.names = FALSE)
          colnames(df_out)[1] <- "Gene"
          write.table(df_out, file, sep = "\t", row.names = FALSE, quote = FALSE)
        }
      )
    }
  )
}

