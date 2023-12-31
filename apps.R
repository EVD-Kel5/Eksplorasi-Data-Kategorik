library(shiny)
library(data.table)
library(plotly)
library(readxl)

#- - - - - - - - UI - - - - - - - -

ui <- navbarPage(
  title=a(tags$b("Eksplorasi Data Kategorik")) ,
  windowTitle="Eksplorasi Data Kategorik",
  
  ##- - - - - - - - Satu Peubah - - - - - - - -
  
  tabPanel(title = "Satu Peubah Kategorik",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "jenisdata", label = "Pilih Jenis Data :", 
                           choices = list('Dataset' = "datatersedia", 'Upload File csv' = "upload_file")
               ),
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
                 textInput(inputId = "judul_baru", label = "Judul Baru :", value ="")
               ),
               radioButtons(inputId = "tambahplot", label = "Tampilan Chart :", choices = c( "Bar Chart" = 'tanpa_piechart', "Pie Chart" = 'ada_piechart'), selected = 'tanpa_piechart')
             ),
             mainPanel(
               uiOutput("selection_text"),
               uiOutput("text"),
               tableOutput(outputId = "table1"),
               plotlyOutput(outputId = "chart")
             )
           )),
  
  ##- - - - - - - - Dua Peubah - - - - - - - -
  
  tabPanel(title = "Dua Peubah Kategorik",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "datapilihan", label = "Pilih Jenis Data", 
                           choices = list('Dataset' = "datatersedia2", 'Upload File csv' = "upload_file2")),
               conditionalPanel(
                 condition = "input.datapilihan == 'datatersedia2'",
                 tags$b("Dataset Jawa Barat :"),
                 br(),
                 selectInput(inputId = "var_cat11", label = "Variable Kategorik 1:", choices = c("Not Selected")),
                 selectInput(inputId = "var_cat12", label = "Variable Kategorik 2:", choices = c("Not Selected"))
               ),
               conditionalPanel(
                 condition = "input.datapilihan == 'upload_file2'",
                 fileInput("csv_input2", "Pilih File CSV", accept = ".csv"),
                 selectInput("var_cat21", "Variable Kategorik 1:", choices = c("Not Selected")),
                 selectInput("var_cat22", "Variable Kategorik 2:", choices = c("Not Selected"))
               ),
               br(),
               tags$b("Pilihan :"),
               checkboxInput("show_persen2", "Tampilkan Persentase", value = FALSE),
               checkboxInput("modif_judul2", "Modifikasi Judul", value = FALSE),
               conditionalPanel(
                 condition = "input.modif_judul2 == true",
                 textInput(inputId = "judul_baru2", label = "Judul Baru :", value ="")
               )
             ),
             mainPanel(
               uiOutput("selection_text2"),
               uiOutput("text2"),
               tableOutput(outputId = "table2"),
               plotlyOutput(outputId = "barplot2")
             )
           )),
  ##- - - - - - - - Tentang - - - - - - - -
  
  tabPanel(
    title = "Tentang",
    titlePanel("Tentang"),
    br(),
    "Data kategorik adalah jenis data yang variabel-variabelnya dapat dikelompokkan menjadi beberapa kelompok atau kategorik. Data kategorik yang tersedia dalam dashboard ini adalah data tentang penduduk di provinsi Jawa Barat yang bersumber dari Data Podes 2021. ",
    br(),
    "Eksplorasi data kategorik yang disajikan pada dashboard ini adalah eksplorasi satu peubah kategorik dengan menggunakan tabel frekuensi, barchart dan piechart; serta eksplorasi dua peubah kategorik dengan menggunakan tabel kontingensi dan barchart."
  )
)

#- - - - - - - - Server - - - - - - - -

server <- function(input, output, session) {
  
  ##- - - - - - - - Satu Peubah - - - - - - - -
  
  data_input <- reactive({
    if(input$jenisdata == "datatersedia"){
      excel_data1 <- read.csv(url("https://raw.githubusercontent.com/EVD-Kel5/Eksplorasi-Data-Kategorik/main/dataset.csv"))
      colnames(excel_data1)<-c("Keberadaan Diare","Sumber Air Minum","Tempat Pembuangan Sampah","Fasilitas Buang Air Besar")
      return(excel_data1)
    } else if (input$jenisdata == "upload_file"){
      req(input$csv_input)
      uploaded_data1 <- fread(input$csv_input$datapath)
      return(uploaded_data1)
    }
  })
  
  observeEvent(data_input(),{
    choices <- c(names(data_input()))
    if(input$jenisdata == "datatersedia"){
      updateSelectInput(inputId = "var_kategorik1", choices = choices)
    } else if(input$jenisdata == "upload_file"){
      updateSelectInput(inputId = "var_kategorik", choices = choices)
    }
  })
  
  ###= = = = = = = = Tabel Frekuensi = = = = = = = =
  
  output$text <- renderUI({
    HTML("<b><u>Tabel Frekuensi</u></b>")
  })
  
  output$table1 <- renderTable({
    req(input$jenisdata)
    req(input$var_kategorik1, input$var_kategorik)
    tabel_hasil1 <- table(data_input()[[ifelse(input$jenisdata == "datatersedia", input$var_kategorik1, input$var_kategorik)]])
    tabel_hasil <- as.data.frame(tabel_hasil1)
    proporsi <- prop.table(tabel_hasil$Freq)
    persentase <- proporsi * 100
    
    # Combine the results into a data frame
    result_df <- data.frame(
      Faktor = as.character(tabel_hasil$Var1),
      Frekuensi = as.numeric(tabel_hasil$Freq),
      Proporsi = proporsi,
      Persentase = persentase,
      row.names = NULL
    )
    result_df
  }, include.rownames = FALSE)
  
  observe({
    
    ###= = = = = = = = Barplot = = = = = = = =
    
    if(input$tambahplot == "tanpa_piechart"){
      output$chart <-renderPlotly({
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
        
        selected_var <- ifelse(input$jenisdata == "datatersedia", input$var_kategorik1, input$var_kategorik)
        
        if (is.numeric(data_input()[[selected_var]])) {
          # Memberikan pemberitahuan jika variabel numerik
          showModal(
            modalDialog(
              title = "Pemberitahuan",
              "Data yang Anda pilih adalah variabel numerik. Silakan pilih variabel kategorik.",
              easyClose = TRUE,
              footer = NULL
            )
          )
          return(NULL)
        }
        
        datax <- table(data_input()[[selected_var]])
        
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
    }
    
    ###= = = = = = = = Pie Chart = = = = = = = =    
    
    if(input$tambahplot == "ada_piechart"){
      output$chart <-renderPlotly({
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
        
        selected_var <- ifelse(input$jenisdata == "datatersedia", input$var_kategorik1, input$var_kategorik)
        
        if (is.numeric(data_input()[[selected_var]])) {
          # Memberikan pemberitahuan jika variabel numerik
          showModal(
            modalDialog(
              title = "Pemberitahuan",
              "Variabel yang anda pilih adalah variabel numerik. Silakan pilih variabel kategorik.",
              easyClose = TRUE,
              footer = NULL
            )
          )
          return(NULL)
        }
        
        datax <- table(data_input()[[selected_var]])
        
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
  
  ##- - - - - - - - Dua Peubah - - - - - - - -
  
  data_input2 <- reactive({
    if(input$datapilihan == "datatersedia2"){
      excel_data2 <- read.csv(url("https://raw.githubusercontent.com/EVD-Kel5/Eksplorasi-Data-Kategorik/main/dataset.csv"))
      colnames(excel_data2)<-c("Keberadaan Diare","Sumber Air Minum","Tempat Pembuangan Sampah","Fasilitas Buang Air Besar")
      return(excel_data2)
    } else if (input$datapilihan == "upload_file2"){
      req(input$csv_input2)
      uploaded_data2 <- fread(input$csv_input2$datapath)
      return(uploaded_data2)
    }
  })
  
  observeEvent(data_input2(),{
    choices <- c(names(data_input2()))
    if(input$datapilihan == "datatersedia2"){
      updateSelectInput(inputId = "var_cat11", choices = choices)
      updateSelectInput(inputId = "var_cat12", choices = choices)
    } else if(input$datapilihan == "upload_file2"){
      updateSelectInput(inputId = "var_cat21", choices = choices)
      updateSelectInput(inputId = "var_cat22", choices = choices)
    }
  })
  
  ###= = = = = = = = Tabel Kontingensi = = = = = = = =
  
  output$text2 <- renderUI({
    HTML("<b><u>Tabel Kontingensi</u></b>")
  })
  
  output$table2 <- renderTable({
    req(input$datapilihan)
    if(input$datapilihan == "datatersedia2"){
      req(input$var_cat11, input$var_cat12)
      data_contingency <- table(data_input2()[, c(req(input$var_cat11), req(input$var_cat12))])
    } else if (input$datapilihan == "upload_file2"){
      req(input$var_cat21, input$var_cat22)
      data_contingency <- table(data_input2()[, c(req(input$var_cat21), req(input$var_cat22))])
    }
    as.data.frame.matrix(data_contingency)
  }, include.rownames = TRUE)
  
  ###= = = = = = = = Barplot = = = = = = = =
  
  output$barplot2 <-renderPlotly({
    req(input$datapilihan)
    
    if(input$modif_judul2){
      judul_plot2 <- input$judul_baru2 
    } else {
      judul_plot2 <- " "
    }
    
    if(input$datapilihan == "datatersedia2"){
      req(input$var_cat11, input$var_cat12)
      datax2 <- table(data_input2()[, c(req(input$var_cat11), req(input$var_cat12))])
    } else if (input$datapilihan == "upload_file2"){
      req(input$var_cat21, input$var_cat22)
      datax2 <- table(data_input2()[, c(req(input$var_cat21), req(input$var_cat22))])
    }
    
    if(input$show_persen2){
      datax_persen2 <- prop.table(datax2)*100
    } else {
      datax_persen2 <- datax2
    }
    
    datax_persen2 <- as.data.frame(datax_persen2)
    
    if(input$datapilihan == "datatersedia2"){
      nama_sumbux <- paste(input$var_cat11)
    } else if(input$datapilihan == "upload_file2"){
      nama_sumbux <- paste(input$var_cat21)
    }
    
    plot_ly(
      x=datax_persen2[,1],
      y = datax_persen2[,3],
      type = "bar",
      color = datax_persen2[,2],
      barmode = "group"
    ) %>% 
      layout(title = judul_plot2, 
             yaxis = list(title = if(input$show_persen2) "Persentase (%)" else "Count"),
             xaxis = list(title = nama_sumbux),
             showlegend = TRUE
      )
    
  })
  
}


shinyApp(ui = ui, server = server)
