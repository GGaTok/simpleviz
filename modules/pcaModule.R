# modules/pcaModule.R

# 0. example dataset
set.seed(123)
n_samples <- 100
n_features <- 20
n_groups <- 3
generate_group_data <- function(n, features, mean, sd) {
  matrix(rnorm(n * features, mean = mean, sd = sd), nrow = n)
}

group1 <- generate_group_data(n_samples %/% 3, n_features, mean = 0, sd = 1)
group2 <- generate_group_data(n_samples %/% 3, n_features, mean = 2, sd = 1.5)
group3 <- generate_group_data(n_samples - 2 * (n_samples %/% 3), n_features, mean = -1, sd = 0.5)

data <- rbind(group1, group2, group3)
groups <- rep(paste0("Group", 1:n_groups), c(nrow(group1), nrow(group2), nrow(group3)))
colnames(data) <- paste0("Feature", 1:n_features)
sample_names <- paste0("Sample", 1:nrow(data))
example_pca_data <- data.frame(Sample = sample_names, Group = groups, data)

# 1. UI
pcaUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titlePanel("PCA Plot"),
    sidebarLayout(
      sidebarPanel(
        # 파일 업로드
        fileInput(ns("pca_file"), "Upload your TSV file",
                  accept = c("text/tab-separated-values", "text/plain", ".tsv", ".txt")),
        
        # 예제 데이터 다운로드
        downloadButton(ns("downloadPCAData"), "Example Data"),
        
        # PCA 축 선택
        selectInput(ns("x_axis"), "X-axis:", choices = paste0("Dim", 1:5), selected = "Dim1"),
        selectInput(ns("y_axis"), "Y-axis:", choices = paste0("Dim", 1:5), selected = "Dim2"),
        
        # 축 범위 설정
        sliderInput(ns("x_range"), "X-axis range:", min = -10, max = 10, 
                    value = c(-10, 10), step = 1),
        sliderInput(ns("y_range"), "Y-axis range:", min = -10, max = 10, 
                    value = c(-10, 10), step = 1),
        
        # 포인트 크기 & 폰트 사이즈
        sliderInput(ns("point_size"), "Point Size:", min = 1, max = 5, value = 2, step = 0.5),
        sliderInput(ns("axis_font_size"), "Axis Font Size:", min = 8, max = 20, value = 12, step = 1),
        
        # 타원 표시 여부
        checkboxInput(ns("add_ellipse"), "Add ellipses", value = TRUE),
        selectInput(ns("ellipse_type"), "Ellipse Type:", 
                    choices = c("concentration", "convex"), selected = "concentration"),
        
        # 포인트만 표시 / 텍스트 표시
        checkboxInput(ns("show_points"), "Show points without text", value = TRUE),
        
        # Plot 크기
        sliderInput(ns("plot_width"), "Plot Width:", min = 400, max = 1200, value = 800, step = 50),
        sliderInput(ns("plot_height"), "Plot Height:", min = 300, max = 1000, value = 600, step = 50),
        
        # 그룹별 색깔
        uiOutput(ns("color_pickers"))
      ),
      mainPanel(
        plotOutput(ns("pca_plot"), width = "100%", height = "auto"),
        verbatimTextOutput(ns("permanova_result")),
        verbatimTextOutput(ns("pairwise_result"))
      )
    )
  )
}

# 2. Server
pcaServer <- function(id, examplePCAData=example_pca_data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # 업로드 / 예제 데이터 선택
      pca_dataset <- reactive({
        # 업로드된 파일이 없으면 예제 데이터 사용
        if (is.null(input$pca_file)) {
          return(examplePCAData)
        } else {
          df <- read.delim(input$pca_file$datapath, sep = "\t", header = TRUE, check.names = FALSE)
          
          # 'Sample'과 'Group' 등 컬럼이 어떻게 들어오는지에 따라 전처리 로직을 조정하세요.
          # 아래 예시는 pivot_longer -> pivot_wider 형태를 사용한 예시입니다.
          
          # 만약 업로드 파일이 이미 wide format(행=샘플, 열=Feature)이라면
          # 이 과정을 생략하거나 필요에 맞게 수정해야 합니다.
          if (!all(c("Sample", "Group") %in% colnames(df))) {
            # Sample/Group 컬럼이 없는 경우 처리 (단순 예시)
            return(df)
          }
          
          # long -> wide 변환
          df_long <- df %>%
            pivot_longer(cols = -c(Sample, Group), names_to = "Variable", values_to = "Value") %>%
            pivot_wider(names_from = "Sample", values_from = "Value")
          
          # 숫자 컬럼 변환
          df_long <- df_long %>%
            mutate(across(-(1:2), as.numeric))
          
          # rownames를 임시 세팅 (옵션)
          rownames(df_long) <- seq_len(nrow(df_long))
          return(df_long)
        }
      })
      
      # 그룹 이름 추출 후, 그룹별 색상 선택 UI
      output$color_pickers <- renderUI({
        df <- pca_dataset()
        if (!"Group" %in% colnames(df)) return(NULL)
        
        groups <- unique(df$Group)
        
        color_inputs <- lapply(seq_along(groups), function(i) {
          colourInput(
            inputId = ns(paste0("color_", i)),
            label = paste("Color for", groups[i]),
            value = brewer.pal(8, "Set2")[((i - 1) %% 8) + 1]
          )
        })
        
        do.call(tagList, color_inputs)
      })
      
      # 그룹별 팔레트 생성
      selected_palette <- reactive({
        df <- pca_dataset()
        if (!"Group" %in% colnames(df)) return(NULL)
        
        groups <- unique(df$Group)
        sapply(seq_along(groups), function(i) {
          input[[paste0("color_", i)]]
        })
      })
      
      # PCA 계산
      pca_result <- reactive({
        df <- pca_dataset()
        
        # Group, Variable 컬럼 제외한 나머지를 numeric 행렬로 사용
        # (예제 데이터 구조에 따라 조정)
        numeric_cols <- setdiff(colnames(df), c("Sample", "Group", "Variable"))
        if (length(numeric_cols) < 2) return(NULL)
        
        X <- df[, numeric_cols, drop = FALSE]
        X <- as.data.frame(lapply(X, as.numeric))  # 혹시 모를 문자형 변환
        
        if (nrow(X) <= 1) return(NULL)
        
        # PCA 계산 (FactoMineR)
        PCA(X, scale.unit = TRUE, graph = FALSE)
      })
      
      # 축 범위를 파일 업로드/축 변경 시 동적으로 업데이트
      observe({
        res.pca <- pca_result()
        if (is.null(res.pca)) return()
        
        x_axis <- which(paste0("Dim", 1:10) == input$x_axis)  # 최대 10차원 가정
        y_axis <- which(paste0("Dim", 1:10) == input$y_axis)
        
        # PCA 좌표
        coords <- res.pca$ind$coord
        if (ncol(coords) < max(x_axis, y_axis)) return()
        
        x_range <- range(coords[, x_axis])
        y_range <- range(coords[, y_axis])
        
        x_padding <- diff(x_range) * 0.2
        y_padding <- diff(y_range) * 0.2
        x_range_initial <- x_range + c(-x_padding, x_padding)
        y_range_initial <- y_range + c(-y_padding, y_padding)
        
        # 적절한 슬라이더 범위 설정
        x_min <- floor(x_range_initial[1])
        x_max <- ceiling(x_range_initial[2])
        y_min <- floor(y_range_initial[1])
        y_max <- ceiling(y_range_initial[2])
        
        updateSliderInput(session, "x_range", 
                          min = x_min - 5, max = x_max + 5, 
                          value = c(x_min, x_max))
        updateSliderInput(session, "y_range", 
                          min = y_min - 5, max = y_max + 5, 
                          value = c(y_min, y_max))
      })
      
      # PCA Plot
      output$pca_plot <- renderPlot({
        res.pca <- pca_result()
        df <- pca_dataset()
        if (is.null(res.pca) || !"Group" %in% colnames(df)) return()
        
        x_axis <- which(paste0("Dim", 1:10) == input$x_axis)
        y_axis <- which(paste0("Dim", 1:10) == input$y_axis)
        
        # fviz_pca_ind()에 전달할 인수 구성
        p <- fviz_pca_ind(
          res.pca,
          title = "PCA Plot",
          repel = TRUE,
          axes = c(x_axis, y_axis),
          geom.ind = if (input$show_points) "point" else c("point", "text"),
          col.ind = df$Group,          # 색상은 그룹별
          palette = selected_palette(), # 사용자가 지정한 팔레트
          addEllipses = input$add_ellipse,
          ellipse.level = 0.9,
          legend.title = "Groups",
          mean.point = FALSE,
          pointsize = input$point_size
        )
        
        # ellipse 타입 지정 (concentration vs convex)
        if (input$add_ellipse && input$ellipse_type == "convex") {
          p$layers[[2]]$aes_params$linetype <- 2  # 예시: 타원 모양 바꾸기 등
        }
        
        # x_range, y_range 설정
        p <- p +
          theme(axis.line = element_line(color = "black"),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text = element_text(size = input$axis_font_size),
                axis.title = element_text(size = input$axis_font_size + 2)) +
          scale_x_continuous(limits = input$x_range) +
          scale_y_continuous(limits = input$y_range)
        
        p
      }, width = function() input$plot_width, height = function() input$plot_height)
      
      # PERMANOVA 결과
      output$permanova_result <- renderPrint({
        res.pca <- pca_result()
        df <- pca_dataset()
        if (is.null(res.pca) || !"Group" %in% colnames(df)) {
          cat("No valid data for PERMANOVA.\n")
          return()
        }
        
        pca_coords <- res.pca$ind$coord
        if (nrow(pca_coords) != nrow(df)) {
          cat("Data size mismatch. Cannot run PERMANOVA.\n")
          return()
        }
        
        permanova_result <- adonis2(pca_coords ~ Group, data = df, permutations = 999)
        cat("PERMANOVA Results:\n")
        print(permanova_result)
        cat("\nOverall P-value:", permanova_result$`Pr(>F)`[1], "\n")
      })
      
      # Pairwise PERMANOVA
      output$pairwise_result <- renderPrint({
        res.pca <- pca_result()
        df <- pca_dataset()
        if (is.null(res.pca) || !"Group" %in% colnames(df)) {
          cat("No valid data for Pairwise PERMANOVA.\n")
          return()
        }
        
        pca_coords <- res.pca$ind$coord
        if (nrow(pca_coords) != nrow(df)) {
          cat("Data size mismatch. Cannot run pairwiseAdonis.\n")
          return()
        }
        
        pairwise_result <- pairwise.adonis(pca_coords, df$Group, p.adjust.m = "bonferroni")
        cat("Pairwise PERMANOVA Results:\n")
        print(pairwise_result)
      })
      
      # 예제 데이터 다운로드
      output$downloadPCAData <- downloadHandler(
        filename = function() {
          "example_pca_data.tsv"
        },
        content = function(file) {
          write.table(examplePCAData, file, row.names = FALSE, sep = "\t", quote = FALSE)
        }
      )
      
    }
  )
}
