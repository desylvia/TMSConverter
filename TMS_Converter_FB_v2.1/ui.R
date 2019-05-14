#Buat error message
library(shiny)
library(shinyjs)

#UI Shiny
ui = fluidPage(
  useShinyjs(),
  titlePanel("CSV Converter"),
  
  #sidebar-layout
  sidebarLayout(
    #ui-panel
    sidebarPanel(
      
      #input
      fileInput("file1", "CSV Only!",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      h5(strong('Download New Data')),
      downloadButton('new_data_txt', 'GET TXT'),
      downloadButton('new_data_ctl', 'GET CTL'),
      br(),br(),
      a(id = "toggleAdvanced", "Download Saved Data"),
      hidden(
        div(id = "advanced",
            downloadButton('saved_data_cln', 'CLEAN'),
            downloadButton('saved_data_ori', 'ORIGINAL')
        )
      )
    ),
    
    #ui-preview holder
    mainPanel(
      tableOutput('contents'))
  )
)