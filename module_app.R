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
<<<<<<< HEAD

 # 모듈 파일 로드
#setwd("C:/R/SimpleViz/simpleviz")
=======
library(plotly)
library(heatmaply)

 # 모듈 파일 로드
setwd("C:/R/SimpleViz")
>>>>>>> 683bdece440bf3b61f32ad6fff05903138e6721e
# Load configuration and modules
source("modules/boxplotModule.R")
source("modules/pcaModule.R")
source("modules/volcanoModule.R")
source("modules/heatmapModule.R")
<<<<<<< HEAD
#source("modules/heatmapmodule_ploty.R")
=======
source("modules/heatmapmodule_ploty.R")
>>>>>>> 683bdece440bf3b61f32ad6fff05903138e6721e
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
<<<<<<< HEAD
#  tabPanel("Heatmap1",heatmaplyUI("heatmap")),
  tabPanel("Citation", 
           h3("About this app"),
           p("SimpleViz allows you to create box/violin/dot plots, PCA plot, volcano plot and Heatmap with statistical analysis.")
=======
  tabPanel("Heatmap1",heatmaplyUI("heatmap")),
  tabPanel("Citation", 
           h3("About this app"),
           p("SimpleViz allows you to create box/violin/dot plots, volcano plot and PCA plot with statistical analysis.")
>>>>>>> 683bdece440bf3b61f32ad6fff05903138e6721e
  )
)

# Define Server
server <- function(input, output, session) {
  boxplotServer("boxplot")
  pcaServer("pca")
  volcanoServer("volcano")
  heatmapServer("heatmap")
<<<<<<< HEAD
#  heatmaplyServer("heatmap")
=======
  heatmaplyServer("heatmap")
>>>>>>> 683bdece440bf3b61f32ad6fff05903138e6721e
}

# Run the app
shinyApp(ui, server)


