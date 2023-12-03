library(shiny)
library(data.table)
library(plotly)


ui <- navbarPage(
  title=a(tags$b("Eksplorasi Data Kategorik")) ,
  windowTitle="Eksplorasi Data Kategorik",
    tabPanel(title = "Satu Peubah Kategorik",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "jenisdata", label = "Pilih Jenis Data :", 
                             choices = list('Dataset' = "datatersedia", 'Tabel Frekuensi' = "tabel_frek", 'Upload File csv' = "upload_file")),
                 conditionalPanel(
                   condition = "input.jenisdata == 'datatersedia'",
                   selectInput(inputId = "pilih_dataset", label = "Dataset Tersedia :", choices = list('A' = "dataA", 'B' = "dataB"))
                 ),
                 conditionalPanel(
                   condition = "input.jenisdata == 'tabel_frek'",
                   numericInput(inputId = "jum_kategori", label = "Banyaknya Kategori :", value = 2, min = 2, max = 30)
                 ),
                 conditionalPanel(
                   condition = "input.jenisdata == 'upload_file'",
                   fileInput("csv_input", "Pilih CSV File", accept = ".csv"),
                   selectInput("var_kategorik", "Variable Kategorik :", choices = c("Not Selected"))
                 ),
                 br(),
                 tags$b("Pilihan :"),
                 checkboxInput("show_persen", "Tampilkan Persentase", value = FALSE),
                 checkboxInput("show_inbars", "Tampilkan Jumlah/Persentase di Bar", value = FALSE),
                 checkboxInput("show_urutan", "Urutkan", value = FALSE),
                 tags$b("Modifikasi :"),
                 checkboxInput("modif_judul", "Judul", value = FALSE),
                 checkboxInput("modif_warna", "Warna", value = FALSE),
                 radioButtons(inputId = "tambahplot", label = "Plot Tambahan :", choices = c("Tidak", "Pie Chart"), selected = "Tidak")
               ),
               mainPanel(
                 uiOutput("selection_text"),
                 plotlyOutput(outputId = "barplot")
               )
             )),
    tabPanel(title = "Dua Peubah Kategorik",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "datapilihan", label = "Pilih Jenis Data", choices = c("Dataset", "Tabel Kontingensi", "Pengamatan Individu"),
                             selected = "Dataset"),
                 numericInput(inputId = "jum_kategori", label = "Banyaknya Kategori", value = 2, min = 2, max = 30)
               ),
               mainPanel(
                 
               )
             )),
)


server <- function(input, output) {
  options(shiny.maxRequestSize=10*1024^2) 
  data_input <- reactive({
    req(input$csv_input)
    fread(input$csv_input$datapath)
  })
  
  observeEvent(data_input(),{
    choices <- c("Not Selected",names(data_input()))
    updateSelectInput(inputId = "var_kategorik", choices = choices)
  })

  output$barplot <-renderPlotly({
    req(input$var_kategorik)
    datax <- table(data_input()[[input$var_kategorik]])
    plot_ly(
      x = names(datax),
      y = datax,
      type = "bar"
    )
  })
  
}

shinyApp(ui = ui, server = server)
