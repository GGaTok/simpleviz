example_counts <- data.frame(
  Taxon = c("s__Candida_albicans", "s__Saccharomyces_cerevisiae",
            "s__Kazachstania_slooffiae", "s__Malassezia_restricta",
            "s__Aspergillus_fumigatus"),
  S1 = c(123, 67, 10, 0, 5),
  S2 = c(0, 89, 34, 12, 1),
  S3 = c(45, 10, 5, 0, 0),
  S4 = c(99, 3, 2, 25, 0),
  S5 = c(3, 150, 17, 5, 8),
  S6 = c(11, 0, 0, 0, 0),
  check.names = FALSE
)
# ────────────────────────────────────────────────────────────────
#  ANCOM-BC2 모듈 – 고정 FDR(0.05) · 고정 Group 이름
# ────────────────────────────────────────────────────────────────
ancombc2UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        ## 데이터 업로드 & 예시
        fileInput(ns("counts_tsv"), "Counts TSV (row = Taxon, col = SampleID)",
                  accept = c(".tsv", ".txt")),
        downloadButton(ns("dl_example"), "example OTU TSV"),
        hr(),
        
        ## 샘플 → 그룹 이동 위젯
        h4("Sample grouping"),
        selectInput(ns("available_samples"), "Available Samples",
                    multiple = TRUE, choices = character(0),
                    selectize = FALSE, size = 6),
        fluidRow(
          column(6, actionButton(ns("add_group1"), "→ G1")),
          column(6, actionButton(ns("add_group2"), "→ G2"))
        ), br(),
        
        tags$p(tags$strong("Group 1 Name : Control")),
        selectInput(ns("group1_samples"), "Group 1 (Control)",
                    multiple = TRUE, choices = character(0),
                    selectize = FALSE, size = 6),
        actionButton(ns("remove_group1"), "← Remove"), br(), br(),
        
        tags$p(tags$strong("Group 2 Name : Treatment")),
        selectInput(ns("group2_samples"), "Group 2 (Treatment)",
                    multiple = TRUE, choices = character(0),
                    selectize = FALSE, size = 6),
        actionButton(ns("remove_group2"), "← Remove"),
        hr(),
        
        checkboxInput(ns("pair"), "Pairwise directional test", TRUE),
        actionButton(ns("run"), "ANCOM-BC2 run",
                     class = "btn-primary", width = "100%"),
        hr(),
        
        downloadButton(ns("dl"),    "Summary result TSV"),
        downloadButton(ns("dl_de"), "DEG-like TSV")
      ),
      
      mainPanel(
        style = "padding-top:60px;",
        DTOutput(ns("tbl")),
        br(),
        tags$div(
          style = "font-size:0.9em; line-height:1.4;",
          tags$strong("row column"),
          tags$ul(
            tags$li(tags$code("Contrast"), " : Comparison group name (e.g., GroupTreatment)"),
            tags$li(tags$code("Beta"),     " : Log2 fold-change estimate"),
            tags$li(tags$code("P"),        " : P-value"),
            tags$li(tags$code("Q"),        " : FDR-corrected P-value (P if not available)"),
            tags$li(tags$code("diff_abn"), " : Whether there is a significant difference (β, W, and Q all meet the criteria) Only cases where β (log fold-change), W (test statistic), and Q (FDR)* all meet the statistical criteria and are concluded to be “significant” are marked as TRUE. 
This flag provides a quick overview of whether there is a statistically verified increase or decrease."),
            tags$li(tags$code("passed_ss")," : Sensitivity and sample size conditions ANCOM-BC2 repeats the analysis three more times, changing the pseudo-count (0.1 / 0.5 / 1) to solve the 0 count problem.
If the q-value for the same taxon remains above α in all three analyses, it is considered “stable” and TRUE is assigned.
Conversely, if the significance/non-significance is reversed depending on the pseudo-count value, FALSE is assigned, meaning “the results may be vulnerable, so caution is advised.")
          )
        )
      )
    )
  )
}

ancombc2Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ## 예시 OTU 다운로드 ------------------------------------------------
    output$dl_example <- downloadHandler(
      filename = function() "example_otu_counts.tsv",
      content  = function(f) readr::write_tsv(example_counts, f)
    )
    
    ## 업로드 OTU & 상태 저장 ----------------------------------------
    counts_mat <- reactiveVal(NULL)
    rv <- reactiveValues(avail = character(0), g1 = character(0), g2 = character(0))
    
    observeEvent(input$counts_tsv, {
      df  <- readr::read_tsv(input$counts_tsv$datapath, col_types = readr::cols())
      mat <- as.matrix(df[ , -1]); rownames(mat) <- df[[1]]
      counts_mat(mat)
      
      rv$avail <- colnames(mat); rv$g1 <- rv$g2 <- character(0)
      refresh_lists()
    })
    
    refresh_lists <- function() {
      updateSelectInput(session, "available_samples",
                        choices = rv$avail, selected = character(0))
      updateSelectInput(session, "group1_samples",
                        choices = rv$g1,    selected = character(0))
      updateSelectInput(session, "group2_samples",
                        choices = rv$g2,    selected = character(0))
    }
    
    ## 샘플 이동 ------------------------------------------------------
    observeEvent(input$add_group1, {
      sel <- intersect(isolate(input$available_samples), rv$avail)
      if (length(sel)) { rv$avail <- setdiff(rv$avail, sel); rv$g1 <- union(rv$g1, sel); refresh_lists() }
    })
    observeEvent(input$add_group2, {
      sel <- intersect(isolate(input$available_samples), rv$avail)
      if (length(sel)) { rv$avail <- setdiff(rv$avail, sel); rv$g2 <- union(rv$g2, sel); refresh_lists() }
    })
    observeEvent(input$remove_group1, {
      sel <- intersect(isolate(input$group1_samples), rv$g1)
      if (length(sel)) { rv$g1 <- setdiff(rv$g1, sel); rv$avail <- union(rv$avail, sel); refresh_lists() }
    })
    observeEvent(input$remove_group2, {
      sel <- intersect(isolate(input$group2_samples), rv$g2)
      if (length(sel)) { rv$g2 <- setdiff(rv$g2, sel); rv$avail <- union(rv$avail, sel); refresh_lists() }
    })
    
    ## ANCOM-BC2 실행 -------------------------------------------------
    res_raw <- eventReactive(input$run, {
      validate(
        need(!is.null(counts_mat()), "❗Please upload the OTU TSV first."),
        need(length(rv$g1) > 0 && length(rv$g2) > 0,
             "❗ Assign ≥1 sample to each of the two groups.")
      )
      meta_df <- data.frame(
        SampleID = c(rv$g1, rv$g2),
        Group    = factor(c(rep("Control",   length(rv$g1)),
                            rep("Treatment", length(rv$g2)))),
        row.names = c(rv$g1, rv$g2)
      )
      mat <- counts_mat()[ , meta_df$SampleID, drop = FALSE]
      
      ANCOMBC::ancombc2(
        data          = mat,
        taxa_are_rows = TRUE,
        meta_data     = meta_df,
        fix_formula   = "Group",
        group         = "Group",
        p_adj_method  = "fdr",
        alpha         = 0.05,            # ▲ FDR 고정
        pairwise      = input$pair
      )
    })
    
    ## primary results → 요약 테이블 ---------------------------------
    get_first_contrast <- function(df) {
      lfc_cols <- grep("^lfc_", names(df), value = TRUE)
      sub("^lfc_", "", setdiff(lfc_cols, "lfc_(Intercept)")[1])
    }
    
    res_df <- reactive({
      req(res_raw())
      df <- res_raw()$res
      contrast <- get_first_contrast(df)
      tibble::tibble(
        Taxon     = df$taxon,
        Contrast  = contrast,
        Beta      = df[[paste0("lfc_",  contrast)]],
        P         = df[[paste0("p_",    contrast)]],
        Q         = df[[paste0("q_",    contrast)]],
        diff_abn  = df[[paste0("diff_", contrast)]],
        passed_ss = df[[paste0("passed_ss_", contrast)]]
      ) |>
        dplyr::mutate(Q = dplyr::coalesce(Q, P)) |>
        dplyr::arrange(Q)
    })
    
    output$tbl <- DT::renderDT({
      DT::datatable(res_df(), filter = "top",
                    options = list(dom = 'ft'))   # Buttons 확장 제거
    })
    
    output$dl <- downloadHandler(
      filename = function() paste0("ancombc2_summary_", Sys.Date(), ".tsv"),
      content  = function(f) readr::write_tsv(res_df(), f)
    )
    
    ## DEG-스타일 TSV ----------------------------------------------
    de_df <- reactive({
      req(res_raw())
      df <- res_raw()$res
      cols <- grep("^lfc_", names(df), value = TRUE)
      purrr::map_dfr(cols, \(cname) {
        con <- sub("^lfc_", "", cname)
        tibble::tibble(
          taxon           = df$taxon,
          log2FoldChange  = df[[cname]],
          pvalue          = df[[paste0("p_", con)]],
          padj            = df[[paste0("q_", con)]]
        )
      }) |> dplyr::arrange(padj)
    })
    
    output$dl_de <- downloadHandler(
      filename = function() paste0("ancombc2_summary_result_", Sys.Date(), ".tsv"),
      content  = function(f) readr::write_tsv(de_df(), f)
    )
  })
}
