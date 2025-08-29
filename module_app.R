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
library(pheatmap)
library(tidyr)
library(DESeq2)
library(DT)
library(sortable)
library(gggenes)
library(rtracklayer)
library(tools)
library(svglite)
library(ANCOMBC)
library(phyloseq)
library(DirichletMultinomial)
library(tidytext)

# Load configuration and modules
source("modules/boxplotModule.R")
source("modules/dmmModule.R")
source("modules/pcaModule.R")
source("modules/volcanoModule.R")
source("modules/heatmapModule.R")
source("modules/deseqModule.R")
source("modules/citationModule.R")
source("modules/correlationModule.R")
source("modules/ancombc2Module.R")
# source("modules/snpModule.R")
# source("modules/GenesyntenyModule.R")

# Load some fonts
font_add_google("Tinos", "Times New Roman")
showtext_auto()
addResourcePath("modules", "modules")

# ---- Simple style for the overview page ----
overview_css <- "
.overview-wrap { max-width: 1100px; margin: 0 auto; padding: 24px 18px 42px; }
.overview-card { background: #ffffff; border: 1px solid #e6e6e6; border-radius: 10px; padding: 18px 20px; margin-bottom: 14px; }
.overview-h1 { font-size: 26px; font-weight: 700; margin: 6px 0 14px; }
.overview-h2 { font-size: 18px; font-weight: 700; margin: 12px 0 8px; }
.overview-muted { color: #5f6b7a; }
.overview-kv { display: grid; grid-template-columns: 160px 1fr; gap: 10px 16px; }
.overview-badge { display: inline-block; background: #f0f3f7; border: 1px solid #e2e8f0; border-radius: 8px; padding: 2px 8px; margin: 2px 6px 2px 0; }
"

ui <- navbarPage(
  title = "SimpleViz",
  
  # ---------------------- Overview tab (first) ----------------------
  tabPanel(
    "Overview",
    tags$style(HTML(overview_css)),
    div(class = "overview-wrap",
        tags$h1(class = "overview-h1", "SimpleViz — Overview"),
        div(class = "overview-card",
            tags$p("SimpleViz is a Shiny application for exploring tabular omics/microbiome data (counts and continuous values). ",
                   "It streamlines statistical testing, visualization, ordination, clustering, and differential analysis in one place."),
            tags$p(class = "overview-muted",
                   "• Modules: Box/Violin/Dot/Bar, Ordination (PCA/PCoA/NMDS), Volcano, Heatmap, DESeq2, Correlation Matrix, ANCOM-BC2, DMM Clustering, Citation"),
            
        ),
        
        div(class="overview-card",
            tags$h2(class="overview-h2","Quick start (3 steps)"),
            tags$ol(
              tags$li("Pick a module from the top tabs (e.g., Box/Violin/Dot)."),
              tags$li("Upload data and choose variables/options in the left (or top) control panel."),
              tags$li("Click ‘Run’, ‘Plot’, ‘Update’, or ‘Analyze’ to render results, then use ‘Download’ if needed.")
            ),
            tags$p(class="overview-muted",
                   "※ Each module can accept files independently. If you have a phyloseq object, ANCOM-BC2 and DMM can consume it directly.")
        ),
        
        div(class="overview-card",
            tags$h2(class="overview-h2","What each tab does"),
            tags$ul(
              tags$li(tags$b("Box/Violin/Dot Plot:"), " Compare distributions across groups. Means/medians, jitter dots, ",
                      "significance annotations (ggpubr/rstatix, Dunn post-hoc), palettes/themes, image export."),
              tags$li(tags$b("Ordination Plot:"), " PCA and, depending on data, PCoA/NMDS. ",
                      "Group ellipses/convex hulls, variance contributions, pairwise Adonis for group separation tests."),
              tags$li(tags$b("Volcano Plot:"), " EnhancedVolcano to visualize log2FC vs −log10(p). ",
                      "Flexible cutoffs, labeling/highlighting, auto labeling of top features."),
              tags$li(tags$b("Heatmap:"), " pheatmap-based scaling/clustering with row/column annotations, ",
                      "custom distance/linkage, export values as tables."),
              tags$li(tags$b("DESeq2:"), " Differential abundance/expression from counts + metadata. ",
                      "Design/contrast selection, normalization/dispersion/shrinkage, tables/summaries/exports."),
              tags$li(tags$b("Correlation matrix:"), " Pearson/Spearman correlation heatmaps, hierarchical clustering, ",
                      "paired interactive table (DT) and figure."),
              tags$li(tags$b("ANCOM-BC2:"), " Compositional DA suited for microbiome data; ",
                      "covariates/confounders, FDR control, synchronized volcano + table view."),
              tags$li(tags$b("DMM Clustering:"), " Dirichlet–Multinomial mixture; choose K by Laplace (or other) criterion, ",
                      "sample clusters and top driving taxa."),
              tags$li(tags$b("Citation:"), " Generate and copy citations for packages and methods used.")
            )
        ),
        
        div(class="overview-card",
            tags$h2(class="overview-h2","Input format (summary)"),
            tags$ul(
              tags$li(tags$b("Count matrix:"), " rows = features (ASV/OTU/Gene), columns = samples; integer counts are recommended."),
              tags$li(tags$b("Metadata:"), " rows = samples, columns = group/covariates; sample IDs must match count-matrix columns."),
              tags$li(tags$b("Long format (for some plots):"), " columns like ",
                      tags$code("Sample, Group, Feature, Value"))
              # intentionally no bullet for "phyloseq RDS: phyloseq::phyloseq ..." per request
            )
        ),
        
        div(class="overview-muted",
            "Last updated: ", format(Sys.Date(), "%Y-%m-%d")
        )
    )
  ),
  
  # ---------------------- existing tabs ----------------------
  tabPanel("Box/Violin/Bar/Dot Plot", boxplotUI("boxplot")),
  tabPanel("Ordination Plot", pcaUI("pca")),
  tabPanel("Volcano Plot", volcanoUI("volcano")),
  tabPanel("Heatmap", heatmapUI("heatmap")),
  tabPanel("DESeq2", deseqUI("DESeq2")),
  tabPanel("Correlation matrix", correlationUI("correlation")),
  # tabPanel("Genesynteny", GenesyntenyUI("Genesynteny")),
  tabPanel("Ancombc2", ancombc2UI("ancombc2")),
  tabPanel("DMM Clustering", dmmUI("dmm")),
  # tabPanel("SNP Finder", snpUI("snp")),
  tabPanel("Citation", citationUI("citation")),
  
  # keep your global header logo as in your original app
  header = tags$div(
    style = "position: absolute;right:-100px; top: -50px; padding: 100px;",
    tags$img(src = "modules/KNU.png", style = "height: 40px;")
  )
)


# Define Server
server <- function(input, output, session) {
  boxplotServer("boxplot")
  pcaServer("pca")
  volcanoServer("volcano")
  heatmapServer("heatmap")
  deseqServer("DESeq2")
  correlationServer("correlation")
  ancombc2Server("ancombc2")
  dmmServer("dmm")
  # snpServer("snp")
  # GenesyntenyServer("Genesynteny")
  citationServer("citation")
}

options(shiny.maxRequestSize = 100 * 1024^2)

# Run the app
shinyApp(ui, server)
