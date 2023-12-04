library(shiny)
library(data.table)
library(plotly)
library(tidyverse)

#- - - - - - - - UI - - - - - - - -

ui <- navbarPage(
  title=a(tags$b("Eksplorasi Data Kategorik")) ,
  windowTitle="Eksplorasi Data Kategorik",
  
  ##- - - - - - - - Satu Peubah - - - - - - - -
  
  tabPanel(title = "Satu Peubah Kategorik",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "jenisdata", label = "Pilih Jenis Data :", 
                           choices = list('Dataset' = "datatersedia", 'Upload File csv' = "upload_file")),
               conditionalPanel(
                 condition = "input.jenisdata == 'datatersedia'",
                 selectInput(inputId = "var_kategorik1", label = "Dataset Jawa Barat :", choices = c("Not Selected"))
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
               checkboxInput("modif_judul", "Modifikasi Judul", value = FALSE),
               conditionalPanel(
                 condition = "input.modif_judul == true",
                 textInput(inputId = "judul_baru", label = "Modifikasi Judul :", value ="")
               ),
               radioButtons(inputId = "tambahplot", label = "Plot Tambahan :", choices = c( "Tidak" = 'tanpa_piechart', "Pie Chart" = 'ada_piechart'), selected = 'tanpa_piechart')
             ),
             mainPanel(
               uiOutput("selection_text"),
               textOutput("text"),
               tableOutput(outputId = "table1"),
               plotlyOutput(outputId = "barplot"),
               plotlyOutput(outputId = "piechart")
             )
           )),
  
  ##- - - - - - - - Dua Peubah - - - - - - - -
  
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

#- - - - - - - - Server - - - - - - - -

server <- function(input, output, session) {
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
    choices <- c("Not Selected", names(data_input()))
    if(input$jenisdata == "datatersedia"){
      updateSelectInput(inputId = "var_kategorik1", choices = choices)
    } else if(input$jenisdata == "upload_file"){
      updateSelectInput(inputId = "var_kategorik", choices = choices)
    }
  })
  
  ##- - - - - - - - Tabel Frekuensi - - - - - - - -
  
  output$text <- renderText({ 
    "Tabel Frekuensi"
  })
  
  output$table1 <- renderTable({
    req(input$jenisdata)
    req(input$var_kategorik1, input$var_kategorik)
    table(data_input()[[ifelse(input$jenisdata == "datatersedia", input$var_kategorik1, input$var_kategorik)]])
  })
  
  ##- - - - - - - - Barplot - - - - - - - -
  
  output$barplot <-renderPlotly({
    req(input$jenisdata)
    req(input$var_kategorik1, input$var_kategorik)
    if(input$jenisdata == "datatersedia"){
      judul_plot <- paste(input$var_kategorik1)
    } else if(input$jenisdata == "upload_file"){
      judul_plot <- paste(input$var_kategorik)
    }
    
    if(input$modif_judul){
      judul_plot <- input$judul_baru 
    }
    
    datax <- table(data_input()[[ifelse(input$jenisdata == "datatersedia", input$var_kategorik1, input$var_kategorik)]])
    
    if(input$show_persen){
      datax_persen <- prop.table(datax)*100
    } else {
      datax_persen <- datax
    }
    
    plot_ly(
      x = names(datax_persen),
      y = datax_persen,
      type = "bar",
      color = names(datax_persen),
      hoverinfo = "y+name",
      text = if(input$show_inbars) ~paste0(round(datax_persen,2)),
      textposition = if(input$show_inbars) "outside" else "none"
    ) %>% 
      layout(title = judul_plot, 
             yaxis = list(title = if(input$show_persen) "Persentase (%)" else "Count"),
             showlegend = FALSE
      )
    
  })
  
  
  ##- - - - - - - - Pie Chart - - - - - - - -
  
  observe({
    if(input$tambahplot == "ada_piechart"){
      output$piechart <-renderPlotly({
        req(input$jenisdata)
        req(input$var_kategorik1, input$var_kategorik)
        if(input$jenisdata == "datatersedia"){
          judul_plot <- paste(input$var_kategorik1)
        } else if(input$jenisdata == "upload_file"){
          judul_plot <- paste(input$var_kategorik)
        }
        
        datax <- table(data_input()[[ifelse(input$jenisdata == "datatersedia", input$var_kategorik1, input$var_kategorik)]])
        
        plot_ly(
          labels = names(datax),
          values = datax,
          type = "pie",
          textinfo = "percent+labels",
          insidetextorientation = "radial"
        ) %>%
          layout(title = judul_plot)
      })
    }
  })

}


shinyApp(ui = ui, server = server)
