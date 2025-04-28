# modules/deseqModule.R

# 0. example dataset
set.seed(123)

# 예시 count matrix
ex_counts <- data.frame(
  Sample1 = c(50, 200, 0, 100, 15),
  Sample2 = c(40, 220, 5, 80, 10),
  Sample3 = c(55, 180, 10, 130, 20),
  Sample4 = c(700, 250, 2, 150, 5),
  Sample5 = c(600, 270, 7, 140, 12),
  Sample6 = c(550, 300, 3, 120, 18)
)
rownames(ex_counts) <- c("GeneA", "GeneB", "GeneC", "GeneD", "GeneE")

# example metadata
ex_meta <- data.frame(
  condition = c("Control", "Control", "Control", "Treatment", "Treatment", "Treatment"),
  row.names = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5", "Sample6")
)

# 1. UI
deseqUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("DESeq2"),
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
      # (A) 왼쪽 사이드바
      sidebarPanel(
        h4("샘플 및 그룹 설정"),
        
        # (1) Count Data 업로드
        fileInput(
          ns("count_file"),
          label = "Count Data 업로드 (TSV)",
          accept = c(".csv", ".tsv", ".txt")
        ),
        helpText("업로드하지 않으면 예시 데이터를 사용합니다."),
        br(),
        
        # (1-2) 예시 데이터 다운로드 버튼
        downloadButton(ns("download_ex_counts"), "Download Example data"),
        br(), br(),
        
        # (2) Available Samples (클릭으로 선택)
        selectInput(
          ns("available_samples"),
          label = "Available Samples",
          choices = character(0),
          selected = NULL,
          multiple = TRUE,
          selectize = FALSE,  # size 옵션 사용 위해 selectize 꺼둠
          size = 6
        ),
        
        # (3) Add 버튼: 선택된 샘플 → Group1, Group2
        fluidRow(
          column(6, actionButton(ns("add_group1"), "-> G1")),
          column(6, actionButton(ns("add_group2"), "-> G2"))
        ),
        br(),
        
        # (4) Group 1
        textInput(ns("group1_name"), "Group 1 Name:", value = "Control"),
        selectInput(
          ns("group1_samples"),
          label = "Group 1",
          choices = character(0),
          selected = NULL,
          multiple = TRUE,
          selectize = FALSE,
          size = 6
        ),
        actionButton(ns("remove_group1"), "<- Remove"),
        br(), br(),
        
        # (5) Group 2
        textInput(ns("group2_name"), "Group 2 Name:", value = "Treatment"),
        selectInput(
          ns("group2_samples"),
          label = "Group 2",
          choices = character(0),
          selected = NULL,
          multiple = TRUE,
          selectize = FALSE,
          size = 6
        ),
        actionButton(ns("remove_group2"), "<- Remove"),
        br(), br(),
        
        # (6) DESeq2 실행 / 다운로드 버튼
        actionButton(ns("run_deseq"), "DESeq2 실행", class = "btn-primary"),
        br(), br(),
        downloadButton(ns("download_deseq_res"), "Download DESeq2 Result"),
        
        width = 4  # 사이드바 폭 조절
      ),
      
      # (B) 오른쪽 메인 영역
      mainPanel(
        h4("DESeq2 analysis result "),
        DTOutput(ns("deseq_table"))
      )
    )
  )
}

# 2. Server
deseqServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # (A) Count Data 가져오기 (업로드 or 예시)
    countData <- reactive({
      if (!is.null(input$count_file)) {
        ext <- tools::file_ext(input$count_file$name)
        if (ext %in% c("csv")) {
          read.csv(input$count_file$datapath, row.names = 1, header = TRUE, check.names = FALSE)
        } else {
          read.delim(input$count_file$datapath, row.names = 1, header = TRUE, check.names = FALSE)
        }
      } else {
        ex_counts
      }
    })
    
    # (B) 그룹 관리용 reactiveValues
    rvals <- reactiveValues(
      available = character(0),
      group1 = character(0),
      group2 = character(0)
    )
    
    # (C) countData() 변경 시 샘플 초기화
    observeEvent(countData(), {
      samples <- colnames(countData())
      n <- length(samples)
      if (n >= 6) {
        rvals$group1    <- samples[1:3]
        rvals$group2    <- samples[(n-2):n]
        if (n > 6) {
          rvals$available <- samples[4:(n-3)]
        } else {
          rvals$available <- character(0)
        }
      } else if (n >= 3) {
        rvals$group1    <- samples[1:3]
        rvals$group2    <- setdiff(samples, rvals$group1)
        rvals$available <- character(0)
      } else {
        rvals$group1    <- character(0)
        rvals$group2    <- character(0)
        rvals$available <- samples
      }
    })
    
    # (D) Add/Remove 버튼 로직
    observeEvent(input$add_group1, {
      selected <- input$available_samples
      rvals$group1 <- unique(c(rvals$group1, selected))
      rvals$available <- setdiff(rvals$available, selected)
    })
    
    observeEvent(input$add_group2, {
      selected <- input$available_samples
      rvals$group2 <- unique(c(rvals$group2, selected))
      rvals$available <- setdiff(rvals$available, selected)
    })
    
    observeEvent(input$remove_group1, {
      selected <- input$group1_samples
      rvals$available <- unique(c(rvals$available, selected))
      rvals$group1 <- setdiff(rvals$group1, selected)
    })
    
    observeEvent(input$remove_group2, {
      selected <- input$group2_samples
      rvals$available <- unique(c(rvals$available, selected))
      rvals$group2 <- setdiff(rvals$group2, selected)
    })
    
    # (E) selectInput와 reactiveValues 동기화
    observe({
      updateSelectInput(session, "available_samples",
                        choices  = rvals$available,
                        selected = intersect(input$available_samples, rvals$available)
      )
      updateSelectInput(session, "group1_samples",
                        choices  = rvals$group1,
                        selected = intersect(input$group1_samples, rvals$group1)
      )
      updateSelectInput(session, "group2_samples",
                        choices  = rvals$group2,
                        selected = intersect(input$group2_samples, rvals$group2)
      )
    })
    
    # (F) metaData 생성
    makeMetaData <- reactive({
      req(countData())
      allSamples <- colnames(countData())
      cond <- rep("Unassigned", length(allSamples))
      
      cond[allSamples %in% rvals$group1] <- input$group1_name
      cond[allSamples %in% rvals$group2] <- input$group2_name
      
      data.frame(condition = cond, row.names = allSamples)
    })
    
    # (G) DESeq2 실행
    dds <- eventReactive(input$run_deseq, {
      cData <- countData()
      mData <- makeMetaData()
      
      dds_obj <- DESeqDataSetFromMatrix(
        countData = cData,
        colData   = mData,
        design    = ~ condition
      )
      dds_obj <- DESeq(dds_obj)
      dds_obj
    })
    
    # (H) 결과 테이블 & 다운로드
    # --- (1) DESeq2 결과와 정규화 카운트 병합 ---
    deseq_results_with_norm <- reactive({
      req(dds())
      # 1) DEG 결과
      res  <- results(dds())
      resdf <- as.data.frame(res)
      resdf$gene <- rownames(resdf)
      
      # 2) 정규화 카운트
      normCounts <- counts(dds(), normalized=TRUE)
      normCountsDF <- as.data.frame(normCounts)
      normCountsDF$gene <- rownames(normCountsDF)
      
      # 3) cbind를 위해 row이름 기준 정렬 (혹은 merge)
      #    여기서는 gene 컬럼 기준 merge 사용
      merged_df <- merge(resdf, normCountsDF, by = "gene", all.x = TRUE)
      
      # 4) gene 열을 맨 앞으로 빼고 나머지 순서 조정
      #    DESeq2 결과(default column들) + 샘플별 정규화 카운트 순
      #    resdf 기본 컬럼: "gene", "baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj"
      #    normCountsDF: gene 제외하고 Sample 열들
      deg_cols <- c("gene","baseMean","log2FoldChange","lfcSE","stat","pvalue","padj")
      sample_cols <- setdiff(colnames(normCountsDF), "gene")
      
      # colnames(merged_df)은 ["gene","baseMean","log2FoldChange","lfcSE","stat","pvalue","padj",
      #                        "Sample1","Sample2","..."] (순서 보장 안 될 수 있음)
      # 따라서 deg_cols + sample_cols 순으로 재정렬
      merged_df <- merged_df[, c(deg_cols, sample_cols)]
      
      merged_df
    })
    
    # --- (2) DT 출력 ---
    output$deseq_table <- renderDT({
      req(deseq_results_with_norm())
      datatable(
        deseq_results_with_norm(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    # --- (3) 다운로드 ---
    output$download_deseq_res <- downloadHandler(
      filename = function() {
        paste0("deseq_results_with_norm_", Sys.Date(), ".tsv")
      },
      content = function(file) {
        df_out <- deseq_results_with_norm()
        write.table(
          df_out,
          file,
          sep = "\t",
          row.names = FALSE,
          quote = FALSE
        )
      }
    )
    
    # (I) 예시 데이터 다운로드 추가
    output$download_ex_counts <- downloadHandler(
      filename = function() {
        paste0("example_counts_", Sys.Date(), ".tsv")
      },
      content = function(file) {
        # (1) 행 이름을 새로운 열 'Geneid'로 변환
        df_out <- data.frame(Geneid = rownames(ex_counts), ex_counts)
        
        # (2) 파일에 저장(맨 앞 열이 "Geneid"가 됨)
        write.table(
          df_out,
          file,
          sep = "\t",
          row.names = FALSE,  # 별도의 행 이름을 쓰지 않음
          quote = FALSE
        )
      }
    )
    
  })
}
