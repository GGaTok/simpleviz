# ─────────────────────────────────────────────────────────────
# SNP Finder  •  Upload ref + query FASTA ▶ minimap2 ▶ bcftools
# Dependencies: minimap2, samtools, bcftools (PATH), VariantAnnotation, ggplot2, DT
# ─────────────────────────────────────────────────────────────
snpUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h3(icon("dna"), "Reference vs Query SNP Finder"),
    fluidRow(
      column(6, fileInput(ns("ref"),  "Reference genome (FASTA)",  accept = c(".fa",".fasta"))),
      column(6, fileInput(ns("qry"),  "Query genome (FASTA)",      accept = c(".fa",".fasta")))
    ),
    actionButton(ns("run"), "Run SNP scan", class = "btn-primary"),
    br(), br(),
    downloadButton(ns("dlVCF"),   "Download VCF"),
    downloadButton(ns("dlTable"), "Download SNP table (TSV)"),
    hr(),
    plotOutput(ns("density"), height = 220),
    DT::DTOutput(ns("snps"))
  )
}

snpServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns  <- session$ns
    dir <- reactiveVal()             # 작업 디렉터리 경로
    snp <- reactiveVal(data.frame()) # 최종 표
    
    # ── 메인 파이프라인 ──────────────────────────
    observeEvent(input$run, {
      req(input$ref, input$qry)
      work <- tempfile("snpJob_")
      dir.create(work); dir(work)
      
      file.copy(input$ref$datapath, file.path(work,"ref.fa"))
      file.copy(input$qry$datapath, file.path(work,"qry.fa"))
      dir(work)
      
      withProgress(message = "Mapping & SNP calling …", value = 0, {
        incProgress(.15, "Indexing ref")
        system2("minimap2", c("-d","ref.mmi","ref.fa"), wd = work)
        
        incProgress(.45, "Alignment")
        system2("minimap2", c("-ax","asm5","ref.mmi","qry.fa"), stdout = "aln.sam", wd = work)
        
        incProgress(.60, "SAM→BAM")
        system2("samtools", c("sort","-o","aln.bam","aln.sam"), wd = work)
        system2("samtools", c("index","aln.bam"),               wd = work)
        
        incProgress(.80, "Variant calling")
        cmd <- paste(
          "bcftools mpileup -f ref.fa aln.bam |",
          "bcftools call -mv -Ov -o snp.raw.vcf"
        )
        system(cmd, intern = FALSE, ignore.stdout = TRUE, wd = work)
        
        incProgress(.90, "Filtering")
        system2("bcftools",
                c("filter","-e","QUAL<30 || DP<5","snp.raw.vcf","-o","snp.filt.vcf"),
                wd = work)
        incProgress(1)
      })
      
      dir(work); dir(work, pattern = "vcf$")
      
      vcf   <- VariantAnnotation::readVcf(file.path(work,"snp.filt.vcf"))
      tbl   <- data.frame(
        Chr  = as.character(GenomeInfoDb::seqnames(vcf)),
        Pos  = GenomicRanges::start(vcf),
        REF  = as.character(VariantAnnotation::ref(vcf)),
        ALT  = as.character(unlist(VariantAnnotation::alt(vcf))),
        QUAL = round(VariantAnnotation::qual(vcf), 1),
        DP   = VariantAnnotation::info(vcf)$DP
      )
      snp(tbl); dir(work); dir(work, pattern="bam$"); dir(work, pattern="mmi$")
      
      # ── 출력 ────────────────────────────────────
      output$snps <- DT::renderDT({
        DT::datatable(tbl, options = list(pageLength = 15, scrollX = TRUE))
      })
      
      output$density <- renderPlot({
        ggplot2::ggplot(tbl, ggplot2::aes(Pos)) +
          ggplot2::geom_histogram(binwidth = 10000) +
          ggplot2::facet_wrap(~Chr, scales = "free_x") +
          ggplot2::labs(x = NULL, y = "SNPs / 10 kb") +
          ggplot2::theme_minimal(base_size = 11)
      })
      
      # ── 다운로드 ───────────────────────────────
      output$dlVCF <- downloadHandler(
        filename = "snp_filtered.vcf",
        content  = function(f) file.copy(file.path(work,"snp.filt.vcf"), f, overwrite = TRUE)
      )
      output$dlTable <- downloadHandler(
        filename = "snp_table.tsv",
        content  = function(f) utils::write.table(snp(), f, sep = "\t",
                                                  row.names = FALSE, quote = FALSE)
      )
      
      dir(work); message("▶ SNP module finished.")
      dir(work); dir(work, pattern="filt")
    })
  })
}
