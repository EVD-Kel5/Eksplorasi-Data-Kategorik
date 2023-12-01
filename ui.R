library(shiny)
library(plotly)
server <- function(input, output) {}

ui <- fluidPage(
  titlePanel("Eksplorasi Data Kategorik"),
  tabsetPanel(
    tabPanel(title = "Satu Peubah Kategorik",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "datapilihan", label = "Pilih Jenis Data", choices = c("Dataset", "Tabel Frekuensi"),
                             selected = "Dataset"),
                 numericInput(inputId = "jum_kategori", label = "Banyaknya Kategori", value = 2, min = 2, max = 30),
               ),
               mainPanel(
               )
             )),
    tabPanel(title = "Dua Peubah Kategorik",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "datapilihan", label = "Pilih Jenis Data", choices = c("Dataset", "Tabel Kontingensi"),
                             selected = "Dataset"),
               ),
               mainPanel(
               )
             ))
  ),
)

shinyApp(ui = ui, server = server)
