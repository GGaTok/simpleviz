# modules/pcaModule.R (v2.2 – full & tested)
# ---------------------------------------------------------------
# * Completes previous truncation at Plot section
# * Includes PERMANOVA, pairwiseAdonis, download handler
# * Minor refactor for clarity
# ---------------------------------------------------------------

# ---- Example data -----------------------------------------------------
set.seed(123)
make_group <- function(n, p, mu, sd) matrix(rnorm(n * p, mu, sd), n)
ex <- rbind(make_group(10, 10, 0, 1),
            make_group(10, 10, 2, 1.5),
            make_group(10, 10, -1, 0.5))
example_pca_data <- data.frame(
  Sample  = paste0("Sample", 1:nrow(ex)),
  Cluster = rep(paste0("Group", 1:3), each = 10),
  ex,
  check.names = FALSE
)

# ---- UI ---------------------------------------------------------------
pcaUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Ordination Plot"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("pca_file"), "Upload TSV", accept = c(".tsv", ".txt")),
        downloadButton(ns("downloadPCAData"), "Example Data"),
        uiOutput(ns("group_var_ui")),
        radioButtons(ns("analysis_method"), "Analysis", c("PCA", "NMDS"), "PCA"),
        selectInput(ns("x_axis"), "X-axis",  paste0("Dim", 1:5), "Dim1"),
        selectInput(ns("y_axis"), "Y-axis",  paste0("Dim", 1:5), "Dim2"),
        sliderInput(ns("x_range"), "X-range", -10, 10, c(-10, 10)),
        sliderInput(ns("y_range"), "Y-range", -10, 10, c(-10, 10)),
        sliderInput(ns("point_size"),     "Point size", 1, 5, 2, 0.5),
        sliderInput(ns("axis_font_size"), "Font size", 8, 20, 12, 1),
        checkboxInput(ns("add_ellipse"), "Add ellipses", TRUE),
        selectInput(ns("ellipse_type"), "Ellipse type", c("concentration", "convex")),
        checkboxInput(ns("show_points"), "Points only", TRUE),
        sliderInput(ns("plot_width"),  "Width",  400, 1200, 800, 50),
        sliderInput(ns("plot_height"), "Height", 300, 1000, 600, 50),
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

# ---- Server -----------------------------------------------------------
pcaServer <- function(id, examplePCAData = example_pca_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------- Data load ---------------------------------------------
    pca_dataset <- reactive({
      if (is.null(input$pca_file)) {
        examplePCAData
      } else {
        df <- read.delim(input$pca_file$datapath, sep = "\t", header = TRUE, check.names = FALSE)
        if ("Cluster" %in% names(df) && !"Group" %in% names(df))
          names(df)[names(df) == "Cluster"] <- "Group"
        df
      }
    })
    
    # ---------- Metadata columns --------------------------------------
    metadata_cols <- reactive({
      df <- pca_dataset()
      non_num <- names(df)[!vapply(df, is.numeric, logical(1))]
      setdiff(non_num, "Sample")
    })
    
    # ---------- Group selector ----------------------------------------
    output$group_var_ui <- renderUI({
      choices <- metadata_cols()
      if (length(choices) == 0) return(helpText("No categorical metadata!"))
      selectInput(ns("group_var"), "Grouping variable", choices,
                  selected = ifelse("Group" %in% choices, "Group", choices[1]))
    })
    
    # ---------- Colour pickers ----------------------------------------
    output$color_pickers <- renderUI({
      req(input$group_var)
      grps <- unique(pca_dataset()[[input$group_var]])
      picks <- lapply(seq_along(grps), function(i) {
        colourpicker::colourInput(ns(paste0("col_", i)), paste("Colour", grps[i]),
                                  RColorBrewer::brewer.pal(8, "Set2")[[(i - 1) %% 8 + 1]])
      })
      do.call(tagList, picks)
    })
    
    palette_sel <- reactive({
      req(input$group_var)
      grps <- unique(pca_dataset()[[input$group_var]])
      cols <- sapply(seq_along(grps), function(i) input[[paste0("col_", i)]])
      names(cols) <- grps; cols
    })
    
    # ---------- Analysis ---------------------------------------------
    analysis_result <- reactive({
      df <- pca_dataset()
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      X <- df[, num_cols, drop = FALSE]
      if (any(X < 0, na.rm = TRUE)) X[X < 0] <- 0
      X <- X[, colSums(X, na.rm = TRUE) > 0, drop = FALSE]
      if (input$analysis_method == "PCA") {
        list(type = "PCA", result = FactoMineR::PCA(X, scale.unit = TRUE, graph = FALSE), X = X)
      } else {
        list(type = "NMDS", result = tryCatch(vegan::metaMDS(X, distance = "bray", k = 2, trymax = 300),
                                              error = function(e) NULL), X = X)
      }
    })
    
    # ---------- Axis range update -------------------------------------
    observe({
      res <- analysis_result(); if (is.null(res$result)) return()
      coords <- if (res$type == "PCA") res$result$ind$coord else res$result$points
      if (is.null(coords)) return()
      xa <- as.integer(sub("Dim", "", input$x_axis)); ya <- as.integer(sub("Dim", "", input$y_axis))
      if (ncol(coords) < max(xa, ya)) return()
      xr <- range(coords[, xa]); yr <- range(coords[, ya])
      pad <- function(r, f) r + c(-f, f)
      xr_ext <- pad(xr, diff(xr) * 0.3); yr_ext <- pad(yr, diff(yr) * 0.4)
      updateSliderInput(session, "x_range", min = floor(xr_ext[1]) - 5, max = ceiling(xr_ext[2]) + 5,
                        value = c(floor(xr_ext[1]) - 1, ceiling(xr_ext[2]) + 1))
      updateSliderInput(session, "y_range", min = floor(yr_ext[1]) - 5, max = ceiling(yr_ext[2]) + 5,
                        value = c(floor(yr_ext[1]) - 1, ceiling(yr_ext[2]) + 1))
    })
    
    # ---------- Plot ---------------------------------------------------
    output$pca_plot <- renderPlot({
      req(input$group_var)
      res <- analysis_result(); df <- pca_dataset(); grp <- df[[input$group_var]]
      
      if (res$type == "PCA") {
        xa <- as.integer(sub("Dim", "", input$x_axis)); ya <- as.integer(sub("Dim", "", input$y_axis))
        p <- factoextra::fviz_pca_ind(
          res$result, axes = c(xa, ya), repel = TRUE,
          geom.ind   = if (input$show_points) "point" else c("point", "text"),
          col.ind    = grp,
          palette    = palette_sel(),
          addEllipses = input$add_ellipse,
          ellipse.level = 0.9,
          legend.title = input$group_var,
          mean.point   = FALSE,
          pointsize    = input$point_size,
          title        = "PCA Plot"
        )
        if (input$add_ellipse && input$ellipse_type == "convex")
          p$layers[[2]]$aes_params$linetype <- 2
        p <- p +
          theme(axis.text = element_text(size = input$axis_font_size),
                axis.title = element_text(size = input$axis_font_size + 2)) +
          scale_x_continuous(limits = input$x_range) +
          scale_y_continuous(limits = input$y_range)
      } else {
        validate(need(!is.null(res$result$points), "NMDS failed"))
        pts <- as.data.frame(res$result$points)
        colnames(pts) <- c("NMDS1", "NMDS2")
        pts$grp <- grp
        p <- ggplot(pts, aes(NMDS1, NMDS2, colour = grp)) +
          geom_point(size = input$point_size) +
          scale_colour_manual(values = palette_sel()) +
          theme_minimal(base_size = input$axis_font_size) +
          labs(color = input$group_var, title = "NMDS Plot")
        if (input$add_ellipse) {
          if (input$ellipse_type == "concentration") {
            p <- p + stat_ellipse(aes(group = grp), level = 0.9)
          } else {
            hulls <- dplyr::group_by(pts, grp) |> dplyr::slice(chull(NMDS1, NMDS2))
            p <- p + geom_polygon(data = hulls, aes(fill = grp), alpha = 0.2, colour = NA) + guides(fill = "none")
          }
        }
        p <- p + coord_cartesian(xlim = input$x_range, ylim = input$y_range)
      }
      p
    }, width = function() input$plot_width, height = function() input$plot_height)
    
    # ---------- PERMANOVA ---------------------------------------------------
    output$permanova_result <- renderPrint({
      req(input$group_var)
      res <- analysis_result(); grp <- pca_dataset()[[input$group_var]]
      validate(need(nrow(res$X) == length(grp), "Data dimension mismatch"))
      perm <- vegan::adonis2(res$X ~ grp, permutations = 999)
      cat("PERMANOVA (grouping by", input$group_var, ")\n")
      print(perm); cat("\nP-value:", perm$`Pr(>F)`[1], "\n")
    })
    
    # ---------- Pairwise PERMANOVA -------------------------------------
    output$pairwise_result <- renderPrint({
      req(input$group_var)
      res <- analysis_result(); grp <- pca_dataset()[[input$group_var]]
      validate(need(nrow(res$X) == length(grp), "Data dimension mismatch"))
      pw <- pairwiseAdonis::pairwise.adonis(res$X, factor(grp), p.adjust.m = "bonferroni")
      cat("Pairwise PERMANOVA (", input$group_var, ")\n", sep = "")
      print(pw)
    })
    
    # ---------- Example data download ----------------------------------
    output$downloadPCAData <- downloadHandler(
      filename = function() "example_pca_data.tsv",
      content  = function(file) write.table(example_pca_data, file, row.names = FALSE, sep = "\t", quote = FALSE)
    )
  })
}
