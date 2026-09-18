library(shiny)
library(data.table)
library(plotly)
library(VizModules)

source("KIDS26-Team12/R/adapters/adapt_ddr_scores.R")

ddr_data <- adapt_ddr_scores()

sample_data <- unique(ddr_data[, .(
  sample_id,
  cancer_type,
  HRDsum,
  HRD_LOH,
  LST,
  TAI,
  purity,
  ploidy
)])

set.seed(1)
sample_data$epi_HRD <- sample_data$HRDsum +
  rnorm(nrow(sample_data), 0, 2)

set.seed(42)
random_subset <- sample_data[
  sample.int(
    nrow(sample_data),
    size = min(50, nrow(sample_data))
  )
]

sample_choices <- unique(random_subset$sample_id)
sample_choices <- sample_choices[!is.na(sample_choices)]

scatter_defaults <- list(
  x.by = "HRDsum",
  y.by = "epi_HRD",
  color.by = "cancer_type"
)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Resolves this script's own directory, independent of the current working directory
app_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    doc_path <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(doc_path)) {
      return(dirname(normalizePath(doc_path)))
    }
  }
  getwd()
}

# shinyApp(ui, server) doesn't auto-mount www/ the way a directory-based app does
www_dir <- file.path(app_dir(), "www")
if (dir.exists(www_dir)) {
  addResourcePath("www", www_dir)
} else {
  warning("www/ directory not found at: ", www_dir)
}

# Figures shown as cards on the Welcome landing page
welcome_figures <- list(
  list(title = "Pipeline Workflow", img = "fig01_workflow.png"),
  list(title = "Predicted vs Observed HRDsum", img = "fig02_pred_vs_obs.png"),
  list(title = "Within-Tissue Correlation by Cancer Type", img = "fig03_per_tissue_correlation.png"),
  list(title = "Per-Tissue Calibration Bias", img = "fig04_tissue_bias.png"),
  list(title = "Lineage Imprinting", img = "fig05_lineage_imprinting.png"),
  list(title = "Few-Shot Calibration", img = "fig06_fewshot_calibration.png"),
  list(title = "HRD Component Decomposition", img = "fig07_components.png")
)

# Renders a single figure card for the Welcome page
figure_card <- function(figure) {
  div(
    class = "scars-card",
    h4(class = "scars-card-title", figure$title),
    tags$img(class = "scars-card-img", src = file.path("www", figure$img))
  )
}

# Sidebar navigation entries: id, label, icon
nav_items <- list(
  list(id = "welcome", label = "Welcome", icon = "house"),
  list(id = "hrd_genome", label = "HRD (Genome)", icon = "dna"),
  list(id = "methylation", label = "Methylation", icon = "flask"),
  list(id = "transcriptomics", label = "Transcriptomics", icon = "chart-line")
)

app_css <- "
  body { background-color: #f2f3f5; }
  .app-header {
    background-color: #16233f;
    color: #fff;
    padding: 14px 24px;
    font-size: 20px;
    display: flex;
    align-items: center;
    gap: 14px;
  }
  .app-layout { display: flex; align-items: stretch; }
  .app-sidebar {
    width: 220px;
    flex-shrink: 0;
    background-color: #16233f;
    min-height: calc(100vh - 50px);
    padding-top: 10px;
  }
  .app-sidebar .nav-link {
    display: block;
    padding: 14px 22px;
    color: #cbd3e1;
    text-decoration: none;
    cursor: pointer;
  }
  .app-sidebar .nav-link:hover { background-color: #22304f; color: #fff; }
  .app-sidebar .nav-link.active {
    background-color: #b81f34;
    color: #fff;
    font-weight: 600;
  }
  .app-sidebar .nav-link i, .app-sidebar .nav-link svg { margin-right: 10px; }
  .app-content { flex: 1; padding: 30px 40px; }
  .welcome-title { color: #16233f; font-weight: 700; text-align: center; }
  .welcome-subtitle { text-align: center; color: #6b7280; margin-bottom: 20px; }
  .welcome-description { max-width: 900px; margin: 0 auto 30px auto; }
  .scars-card {
    background-color: #fff;
    border-radius: 6px;
    box-shadow: 0 1px 4px rgba(0, 0, 0, 0.15);
    padding: 20px;
    margin-bottom: 24px;
  }
  .scars-card-title { color: #16233f; font-weight: 600; margin-bottom: 14px; }
  .scars-card-img { width: 100%; height: auto; border-radius: 4px; }
  .placeholder-panel { text-align: center; color: #6b7280; padding: 60px 20px; }
"

# Left navigation menu, re-rendered so the active tab is highlighted
sidebar_menu_ui <- function(active) {
  tagList(lapply(nav_items, function(item) {
    actionLink(
      inputId = paste0("nav_", item$id),
      label = tagList(icon(item$icon), item$label),
      class = paste("nav-link", if (identical(active, item$id)) "active" else "")
    )
  }))
}

# UI for the existing "HRD (Genome)" scatter/bar/table tool
hrd_genome_ui <- sidebarLayout(
  sidebarPanel(
    radioButtons(
      "sample_mode",
      "Samples to display",
      choices = c(
        "All samples" = "all",
        "Choose samples" = "selected"
      ),
      selected = "all"
    ),

    conditionalPanel(
      condition = "input.sample_mode == 'selected'",

      selectizeInput(
        "selected_samples",
        "Choose samples",
        choices = sample_choices,
        selected = sample_choices[
          seq_len(min(10, length(sample_choices)))
        ],
        multiple = TRUE,
        options = list(
          placeholder = "Select samples"
        )
      )
    ),

    VizModules::dittoViz_scatterPlotInputsUI(
      "samples",
      data = as.data.frame(random_subset),
      defaults = scatter_defaults
    )
  ),

  mainPanel(
    tabsetPanel(
      tabPanel(
        "Scatter plot",
        VizModules::dittoViz_scatterPlotOutputUI("samples")
      ),
      tabPanel(
        "Stacked bar chart",
        plotlyOutput("hrd_bars", height = "700px")
      ),
      tabPanel(
        "Data table",
        DT::DTOutput("sample_table")
      )
    )
  )
)

ui <- fluidPage(
  title = "Genomic SCARS",
  tags$head(tags$style(HTML(app_css))),

  div(
    class = "app-header",
    icon("bars"),
    span("Genomic SCARS")
  ),

  div(
    class = "app-layout",
    div(class = "app-sidebar", uiOutput("sidebar_menu")),
    div(
      class = "app-content",
      tabsetPanel(
        id = "page",
        type = "hidden",
        tabPanelBody(
          "welcome",
          h1(class = "welcome-title", "Genomic SCARS"),
          h4(class = "welcome-subtitle", "Predicting HRD from Methylation Across Cancer Types"),
          p(
            class = "welcome-description",
            "Can methylation patterns predict HRDsum, the genomic scar burden used as a surrogate for homologous ",
            "recombination deficiency? This pipeline uses TCGA PanCanAtlas data (7,065 specimens, 30 cancer types) ",
            "with a v3 elastic-net model and leave-one-cancer-out cross-validation. The figures below summarize the ",
            "current results."
          ),
          fluidRow(
            column(6, figure_card(welcome_figures[[1]])),
            column(6, figure_card(welcome_figures[[2]]))
          ),
          fluidRow(
            column(6, figure_card(welcome_figures[[3]])),
            column(6, figure_card(welcome_figures[[4]]))
          ),
          fluidRow(
            column(12, figure_card(welcome_figures[[5]]))
          ),
          fluidRow(
            column(6, figure_card(welcome_figures[[6]])),
            column(6, figure_card(welcome_figures[[7]]))
          )
        ),
        tabPanelBody(
          "hrd_genome",
          titlePanel("HRD Scores"),
          hrd_genome_ui
        ),
        tabPanelBody(
          "methylation",
          div(class = "placeholder-panel", h3("Methylation"), p("Coming soon."))
        ),
        tabPanelBody(
          "transcriptomics",
          div(class = "placeholder-panel", h3("Transcriptomics"), p("Coming soon."))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$sidebar_menu <- renderUI({
    sidebar_menu_ui(input$page %||% "welcome")
  })

  lapply(nav_items, function(item) {
    observeEvent(input[[paste0("nav_", item$id)]], {
      updateTabsetPanel(session, "page", selected = item$id)
    })
  })

  selected_data <- reactive({
    if (input$sample_mode == "all") {
      return(random_subset)
    }
    
    req(input$selected_samples)
    
    selected <- random_subset[
      sample_id %in% input$selected_samples
    ]
    
    selected[
      order(match(sample_id, input$selected_samples))
    ]
  })
  
  VizModules::dittoViz_scatterPlotServer(
    "samples",
    data = reactive(as.data.frame(selected_data())),
    defaults = scatter_defaults
  )
  
  output$hrd_bars <- renderPlotly({
    displayed_data <- selected_data()
    
    selected_bar_data <- melt(
      displayed_data,
      id.vars = c("sample_id", "cancer_type"),
      measure.vars = c("HRD_LOH", "LST", "TAI"),
      variable.name = "HRD_component",
      value.name = "score"
    )
    
    displayed_sample_ids <- unique(displayed_data$sample_id)
    
    selected_bar_data[, sample_id := factor(
      sample_id,
      levels = displayed_sample_ids
    )]
    
    bar_plot <- plotthis::BarPlot(
      data = as.data.frame(selected_bar_data),
      x = "sample_id",
      y = "score",
      group_by = "HRD_component",
      position = "stack",
      x_text_angle = 90,
      palcolor = c(
        HRD_LOH = "#0072B2",
        LST = "#D55E00",
        TAI = "#009E73"
      )
    )
    
    ggplotly(bar_plot) |>
      config(displaylogo = FALSE)
  })
  
  output$sample_table <- DT::renderDT({
    DT::datatable(
      as.data.frame(selected_data()),
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })
}

app <- shinyApp(ui, server)

shiny::runApp(
  app,
  host = "127.0.0.1",
  port = 3838,
  launch.browser = TRUE
)