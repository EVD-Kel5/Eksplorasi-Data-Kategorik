library(shiny)

server <- function(input, output) {}

ui <- navbarPage(
  title=a(tags$b("Eksplorasi Data Kategorik")) ,
  windowTitle="Eksplorasi Data Kategorik",
    tabPanel(title = "Satu Peubah Kategorik",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "jenisdata", label = "Pilih Jenis Data", 
                             choices = list('Dataset' = "datatersedia", 'Tabel Frekuensi' = "tabel_frek")),
                 conditionalPanel(
                   condition = "input.jenisdata == 'datatersedia'",
                   selectInput(inputId = "pilih_dataset", label = "Dataset Tersedia", choices = list('A' = "dataA", 'B' = "dataB"))
                 ),
                 conditionalPanel(
                   condition = "input.jenisdata == 'tabel_frek'",
                   numericInput(inputId = "jum_kategori", label = "Banyaknya Kategori", value = 2, min = 2, max = 30)
                 ),
                 "Pilihan :",
                 checkboxInput("show_persen", "Tampilkan Persentase", value = FALSE),
                 checkboxInput("show_inbars", "Tampilkan Jumlah/Persentase di Bar", value = FALSE),
                 checkboxInput("show_urutan", "Urutkan", value = FALSE),
                 "Modifikasi :",
                 checkboxInput("showSmoothLine", "Judul", value = FALSE),
                 checkboxInput("showResiduals", "Warna", value = FALSE),
                 radioButtons(inputId = "tambahplot", label = "Plot Tambahan", choices = c("Tidak", "Pie Chart"), selected = "Tidak")
               ),
               mainPanel(
                 uiOutput("selection_text")
               )
             )),
    tabPanel(title = "Dua Peubah Kategorik",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "datapilihan", label = "Pilih Jenis Data", choices = c("Dataset", "Tabel Frekuensi"),
                             selected = "Dataset"),
                 numericInput(inputId = "jum_kategori", label = "Banyaknya Kategori", value = 2, min = 2, max = 30)
               ),
               mainPanel(
                 
               )
             )),
)

shinyApp(ui = ui, server = server)
