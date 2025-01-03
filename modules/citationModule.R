# modules/citation_Module.R

#UI
citationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "citation-section",
      h4("Citation"),
      tags$code("Author: Juhee Kim"),
      tags$p("Year: 2025"),
      p("SimpleViz allows you to create box/violin/dot plots, volcano plot, PCA plot, Heatmap and DEseq2 with statistical analysis.")
    ),
      div(
      class = "code-fence",
      h4("Latest Updates"),
      tags$pre(
        class = "r-code",
        verbatimTextOutput(ns("updateContents"))
      )
    )
  )
}

# Server
citationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    filePath <- file.path("update.txt")
    
    contents <- if (file.exists(filePath)) {
      paste(readLines(filePath, warn = FALSE), collapse = "\n")
    } else {
      "No updates available or 'update.txt' not found in modules folder."
    }
    
    output$updateContents <- renderText({
      contents
    })
  })
}
