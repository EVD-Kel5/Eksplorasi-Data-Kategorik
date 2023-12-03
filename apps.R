library(shiny)
library(data.table)
library(plotly)

#- - - - - UI - - - - - 
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
                   selectInput(inputId = "var_kategorik", label = "Dataset Tersedia :", choices = list('Sumber Air Minum' = "Sumber Air Minum", 'Tempat Pembuangan Sampah' = "Tempat Pembuangan Sampah", 'Fasilitas BAB' = "Fasilitas Buang Air Besar"))
                 ),
                 conditionalPanel(
                   condition = "input.jenisdata == 'tabel_frek'",
                   numericInput(inputId = "jum_kategori", label = "Banyaknya Kategori :", value = 2, min = 2, max = 30)
                 ),
                 conditionalPanel(
                   condition = "input.jenisdata == 'upload_file'",
                   fileInput("csv_input", "Pilih File CSV", accept = ".csv"),
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
                 tableOutput(outputId = "table1"),
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


#- - - - - Server - - - - -
server <- function(input, output) {
  options(shiny.maxRequestSize=10*1024^2) 
  
  data_input <- reactive({
    if(input$jenisdata == "datatersedia"){
      url_data <- "https://raw.githubusercontent.com/EVD-Kel5/Eksplorasi-Data-Kategorik/main/dataset.csv"
      fread(url_data)
    } else if (input$jenisdata == "upload_file"){
      req(input$csv_input)
      fread(input$csv_input$datapath)
    }
  })
  
  observeEvent(data_input(),{
    choices <- c(names(data_input()))
    updateSelectInput(inputId = "var_kategorik", choices = choices)
  })
  
  output$table1 <- renderTable({
    req(input$var_kategorik)
    table(data_input()[[input$var_kategorik]])
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
