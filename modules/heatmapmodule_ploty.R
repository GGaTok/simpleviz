set.seed(123)  # 재현 가능한 랜덤 시드

# 1) Gene, Sample 이름 설정
genes   <- paste0("Gene", 1:5)
samples <- paste0("Sample", 1:3)

# 2) 행=유전자, 열=샘플 인 5x3 matrix 생성
mat_data <- matrix(runif(5 * 3, min = 0, max = 10), nrow = 5, ncol = 3)
rownames(mat_data) <- genes
colnames(mat_data) <- samples

heatmaplyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Interactive Heatmap (heatmaply)"),
    sidebarLayout(
      sidebarPanel(
        position = "left",
        
        # 텍스트 입력: 매트릭스 형식 (tab-separated)
        textAreaInput(ns("matrix_input"), "Paste your matrix data (tab-separated):",
                      rows = 10,
                      placeholder = "gene\tSample1\tSample2\tSample3\nGene1\t2.8\t0.45\t9.56\nGene2\t7.88\t5.28\t4.53\nGene3\t4.08\t8.92\t6.77\nGene4\t8.83\t5.51\t5.72\nGene5\t9.40\t4.56\t1.02"),
        
        # 파일 업로드
        fileInput(ns("heatmap_file"), "Upload your TSV file",
                  accept = c("text/tab-separated-values", 
                             "text/plain", ".tsv", ".txt")),
        # 액션 버튼
        actionButton(ns("submit"), "Submit Data"),
        
        # 예제 데이터 다운로드 버튼 (선택사항)
        downloadButton(ns("downloadHeatmapData"), "Example Data"),# 스케일 옵션
        selectInput(ns("scale_option"), "Scale:", 
                    choices = c("none", "row", "column"), 
                    selected = "none"),
        
        # 클러스터링 옵션
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
        
        # 색상 팔레트
        selectInput(ns("color_palette"), "Color Palette:", 
                    choices = c("RdBu", "Blues", "Greens", "Reds", 
                                "YlOrRd", "YlGnBu", "heat.colors"), 
                    selected = "RdBu"),
        sliderInput(ns("num_colors"), "Number of Colors:", 
                    min = 3, max = 15, value = 9),
        
        # Plot 크기 (단, heatmaply 출력에 직접 반영 여부는 server 코드에서 설정)
        sliderInput(ns("plot_width"), "Plot Width:", 
                    min = 400, max = 1200, value = 700, step = 50),
        sliderInput(ns("plot_height"), "Plot Height:", 
                    min = 300, max = 1000, value = 600, step = 50)
      ),
      mainPanel(
        # heatmaply는 plotly 기반이므로, renderPlotly() 대신 renderUI/renderPlotly() 둘 다 가능
        plotlyOutput(ns("heatmaply_plot"), width = "100%", height = "auto")
      )
    )
  )
}


heatmaplyServer <- function(id, exampleHeatmapData = mat_data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # (1) 업로드 / 텍스트 입력 / 예제 데이터 불러오기
      heatmap_data <- reactive({
        # 1) 파일 업로드 우선
        if (!is.null(input$heatmap_file)) {
          df <- read.delim(input$heatmap_file$datapath, sep = "\t", header = TRUE, check.names = FALSE)
          mat <- as.matrix(df[,-1])
          rownames(mat) <- df[[1]]
          return(mat)
        }
        
        # 2) 텍스트 영역 입력
        txt <- input$matrix_input
        if (nzchar(txt)) {
          lines <- strsplit(txt, "\n")[[1]]
          split_lines <- strsplit(lines, "\t")
          # 첫 줄은 colnames, 첫 열은 rownames
          col_names <- split_lines[[1]][-1]
          mat_data <- do.call(rbind, lapply(split_lines[-1], function(x) as.numeric(x[-1])))
          row_names <- sapply(split_lines[-1], function(x) x[1])
          rownames(mat_data) <- row_names
          colnames(mat_data) <- col_names
          return(mat_data)
        }
        
        # 3) 예제 데이터
        if (!is.null(exampleHeatmapData)) {
          return(exampleHeatmapData)
        }
        
        # 4) NULL 반환 (아무 입력이 없을 때)
        NULL
      })
      
      # (2) `heatmaply()` 호출을 위한 renderPlotly
      output$heatmaply_plot <- renderPlotly({
        req(heatmap_data())
        mat <- heatmap_data()
        
        # (a) 스케일 옵션
        scale_opt <- input$scale_option
        # heatmaply 내부 scale 파라미터("row"/"column"/"none")와 동일하게 맞춤
        # 다만 heatmaply의 scale 파라미터는 "none", "row", "column"을 그대로 지원
        
        # (b) distance, hclust
        dist_m <- input$dist_method
        hclust_m <- input$hclust_method
        
        # (c) 클러스터링 할지 여부
        cluster_rows <- input$cluster_rows
        cluster_cols <- input$cluster_cols
        
        # (d) 색상 팔레트 생성
        pal_name <- input$color_palette
        pal_size <- input$num_colors
        
        if (pal_name %in% rownames(brewer.pal.info)) {
          colors <- colorRampPalette(brewer.pal(min(pal_size, 9), pal_name))(pal_size)
        } else {
          if (pal_name == "heat.colors") {
            colors <- heat.colors(pal_size)
          } else {
            # 예: default RdBu
            colors <- colorRampPalette(brewer.pal(9, "RdBu"))(pal_size)
          }
        }
        
        # (e) heatmaply 생성
        # heatmaply(
        #   x, scale = "none", dist_method = "euclidean", hclust_method = "complete",
        #   Rowv = TRUE, Colv = TRUE, colors = ...
        # )
        heatmaply(
          x              = mat,
          scale          = if (scale_opt == "none") "none" else scale_opt,  # "row"/"column"/"none"
          dist_method    = dist_m,
          hclust_method  = hclust_m,
          row_dend_left  = cluster_rows,  # 행 덴드로그램 표시
          col_dend_side  = if (cluster_cols) "top" else FALSE,  # 열 덴드로그램 표시 여부
          colors         = colors,
          # plotly 레이아웃
          width          = input$plot_width,
          height         = input$plot_height,
          # legend_title
          xlab           = "Samples",
          ylab           = "Genes"
        )
      })
      
      # (3) 예제 데이터 다운로드
      output$downloadHeatmapData <- downloadHandler(
        filename = function() {
          "example_heatmap_data.tsv"
        },
        content = function(file) {
          if (is.null(exampleHeatmapData)) return(NULL)
          mat <- exampleHeatmapData
          df_out <- data.frame(Gene = rownames(mat), mat, check.names = FALSE)
          write.table(df_out, file, sep = "\t", row.names = FALSE, quote = FALSE)
        }
      )
    }
  )
}

