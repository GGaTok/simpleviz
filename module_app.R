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


 # 모듈 파일 로드
#setwd("C:/R/SimpleViz/simpleviz")
# Load configuration and modules
source("modules/boxplotModule.R")
source("modules/pcaModule.R")
source("modules/volcanoModule.R")
source("modules/heatmapModule.R")
source("modules/heatmapmodule_ploty.R")
# Load some fonts
font_add_google("Tinos", "Times New Roman")
showtext_auto()

# Define UI
ui <- navbarPage(
  title = "SimpleViz",
  tabPanel("Box/Violin/Dot Plot", boxplotUI("boxplot")),
  tabPanel("PCA Plot", pcaUI("pca")),
  tabPanel("Volcano Plot", volcanoUI("volcano")),
  tabPanel("Heatmap",heatmapUI("heatmap")),
#  tabPanel("Heatmap1",heatmaplyUI("heatmap")),
  tabPanel("Citation", 
           h3("About this app"),
           p("SimpleViz allows you to create box/violin/dot plots, PCA plot, volcano plot and Heatmap with statistical analysis.")
  )
)

# Define Server
server <- function(input, output, session) {
  boxplotServer("boxplot")
  pcaServer("pca")
  volcanoServer("volcano")
  heatmapServer("heatmap")
#  heatmaplyServer("heatmap")
}

# Run the app
shinyApp(ui, server)


