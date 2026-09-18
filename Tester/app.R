# Load packages ----
library(shiny)
library(maps)
library(mapproj)



# Development version
library(VizModules)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      dittoViz_scatterPlotInputsUI(
        "cars",
        mtcars,
        defaults = list(
          x.by = "wt",
          y.by = "mpg",
          color.by = "cyl"
        )
      )
    ),
    mainPanel(dittoViz_scatterPlotOutputUI("cars"))
  )
)

server <- function(input, output, session) {
  dittoViz_scatterPlotServer(
    "cars",
    data = reactive(mtcars)
  )
}

shinyApp(ui, server)


library(VizModules)
# Using built-in example data (or upload your own file in the app)
plotthis_BarPlotApp()

# Providing your own data
df <- data.frame(
  category = c("A", "B", "C"),
  value = c(10, 20, 15),
  group = c("X", "Y", "X")
)

plotthis_BarPlotApp(data = df)


library(VizModules)

app <- createModuleApp(
  inputs_ui_fn = plotthis_BarPlotInputsUI,
  output_ui_fn = plotthis_BarPlotOutputUI,
  server_fn    = plotthis_BarPlotServer,
  data_list    = list("cars" = mtcars),
  title        = "My Bar Plot"
)

runApp(app)



figureBuilderApp(data_list = list("iris" = iris, "mtcars" = mtcars))

library(VizModules)

ui <- fluidPage(
  figureBuilderUI("figure_builder")
)

server <- function(input, output, session) {
  figureBuilderServer("figure_builder")
}

shinyApp(ui, server)
