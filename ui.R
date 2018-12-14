library(shiny)
# Define UI for application that draws a histogram
shinyUI( fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 2px solid #000000;}"))
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("categories","Select Categories", choices = NULL, multiple = TRUE, selected = NULL),
      sliderInput('nterms',  "Number of terms to plot", min = 1, max = 40, value = 40, step = 1),
      hr(),
      textInput("outfile", "Output file name", value="cellplots"),
      submitButton('submit')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "cellplot",
          plotOutput("cellplot", height = "850px", width = "900px"),
          downloadButton('downloadcellPlot', 'Download Plot')
          ),
        tabPanel(
          "symplot",
          plotOutput("symplot", height = "1000px", width = "1000px"),
          downloadButton('downloadsymPlot', 'Download Plot')
        ),
        tabPanel(
          "arcplot",
          plotOutput("arcplot", height = "850px", width = "900px"),
          downloadButton('downloadarcPlot', 'Download Plot')
        )),
        #tabPanel(
        #  "histogram",
        #  plotOutput("histogram", height = "1000px", width = "1000px")
        #)
        #),
      br(), br(),
      p("This App uses the", code('cellplot'), " package. For more information read the respective documentation in ",
        a("github", href = "http://htmlpreview.github.io/?https://github.com/dieterich-lab/CellPlot/blob/master/vignettes/CellPlotManual.html"),
        "."
      ),
      p("Please keep the version tag on all downloaded files."),
      htmlOutput('appversion')
    )
  )
))

