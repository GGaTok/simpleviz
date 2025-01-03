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
library(reactwidgets)
#library(plotly)
#library(heatmaply)

# Load configuration and modules
source("modules/boxplotModule.R")
source("modules/pcaModule.R")
source("modules/volcanoModule.R")
source("modules/heatmapModule.R")
source("modules/deseqModule.R")
source("modules/citationModule.R")

# Load some fonts
font_add_google("Tinos", "Times New Roman")
showtext_auto()
addResourcePath("modules", "modules")
# Define UI
ui <- navbarPage(
  title = "SimpleViz",
  
  # 탭들
  tabPanel("Box/Violin/Dot Plot", boxplotUI("boxplot")),
  tabPanel("PCA Plot", pcaUI("pca")),
  tabPanel("Volcano Plot", volcanoUI("volcano")),
  tabPanel("Heatmap", heatmapUI("heatmap")),
  tabPanel("DeSeq2", deseqUI("DeSeq2")),
  tabPanel("Citation", citationUI("citation")),
  header = tags$div(
    style = "position: absolute;right:-100px; top: -50px; padding: 100px;",
    tags$img(
      src = "modules/KNU.png",
      style = "height: 40px;"
    )
  )
)
# Define Server
server <- function(input, output, session) {
  boxplotServer("boxplot")
  pcaServer("pca")
  volcanoServer("volcano")
  heatmapServer("heatmap")
  deseqServer("DeSeq2")
  citationServer("citation")
}

# Run the app
shinyApp(ui, server)
