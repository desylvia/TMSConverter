library(shiny)
library(stringr)
library(data.table)
library(excel.link)


#UI Shiny
ui = fluidPage(
  titlePanel("AturDuit Converter"),
  
  #sidebar-layout
  sidebarLayout(
    #ui-panel
    sidebarPanel(
      
      #input
      fileInput("file1", "Only XLSX!",
                multiple = TRUE,
                accept = c(".xlsx")),
      
      #download-modified : add ctl
      downloadButton('downloadData', 'GET TXT'),
      downloadButton('downloadData2', 'GET CTL')
    ),
    
    #ui-preview holder
    mainPanel(
      tableOutput('contents'))
  )
)

#R Server
server <- function(input, output) {
  getData <- reactive({
    inFile <- input$file1
    
    if(is.null(inFile)){
      return(NULL)
    }else{
      tmp <- list()
      for(idx in 1:nrow(inFile)){
        dat <- xl.read.file(inFile[[idx, 'datapath']], password = "adpfi19", write.res.password="adpfi19")
        
        tmp[[idx]] <- dat
      }
      df <- do.call(rbind, tmp)
    }
    
    #========================================= DATA INJECT
    df <- as.data.table(df) #Convert to data.table
    df$Name <- str_to_title(df$Name) #Change case to title case
    
    #Remove: 1) Match name, email, phone. 2) Match name, email. 3) Match name, phone.
    df <- unique(df)
    df <- unique(setDT(df), by = c("Email Address","Name","Phone Numbers"))
    df <- unique(setDT(df), by = c("Email Address","Name"))
    df <- unique(setDT(df), by = c("Name","Phone Numbers"))
    
    for(idx in 1:nrow(df)){
      if(substring(df$`Phone Numbers`[idx],1,1) == "0"){
        df$`Phone Numbers`[idx] <- paste("62",substring(df$`Phone Numbers`[idx],2))
      }else if(substring(df$`Phone Numbers`[idx],1,2) == "62"){
        df$`Phone Numbers`[idx] <-  paste(substring(df$`Phone Numbers`[idx],1,2),substring(df$`Phone Numbers`[idx],3))
      }
    }
    
    #Convert input file to defined template
    converted_data <- data.table(
      v1 = df$Name[1],
      v2 = df$`Email Address`[1],
      v3 = df$`Phone Numbers`[1],
      v4 = df$City[1],
      v5 = "TMS",
      v6 = "Survei",
      v7 = paste("TMS-PFI-SRVSKRIP01-",toupper(format(Sys.time(), "%b")),format(Sys.time(), "%Y"),sep = ""),
      V8 = paste("STATUS",df$`Code Bank`[1],sep = "-")
    )
    
    if(nrow(df) > 1){
      for(idx in 2:nrow(df)){
        tmp <- list(df$Name[idx],df$`Email Address`[idx],df$`Phone Numbers`[idx],df$City[idx],"TMS",
                    "Survei",paste("TMS-PFI-SRVSKRIP01-",toupper(format(Sys.time(), "%b")),format(Sys.time(), "%Y"),sep = ""),
                    paste("STATUS",df$`Code Bank`[idx],sep = "-"))
        converted_data <- rbind(converted_data,tmp)
      }
    }
    
    colnames(converted_data) <- c("Nama Lengkap","Email","No Handphone","Kota","Utm Source","Utm Medium",
                                  "Utm Campaign","Utm Content")
    
    return(converted_data)
    #==========================================
  })
  
  #Render data to view
  output$contents <- renderTable(
    getData()
  )
  
  #Download data as file TXT
  output$downloadData <- downloadHandler(
    filename = function() { #Format:  lead_tmsq2aprsurv_yyyymmddxx.txt
      str_q <- paste("q",quarter(Sys.time()),sep = "")
      str_mnt <- paste(tolower(format(Sys.time(), "%b")))
      str_time <- tolower(format(Sys.time(), "%Y%m%d%H"))
      
      str1 <- paste("lead_tms",str_q,str_mnt,"surv",sep = "")
      paste(paste(str1,str_time,sep = "_"),"txt",sep = ".")
    },
    
    content = function(file) {
      write.table(getData(), file, quote = FALSE, row.names = FALSE, sep = "|")
    })
  
  #Download data as file CTL
  output$downloadData2 <- downloadHandler(
    filename = function() {
      str_q <- paste("q",quarter(Sys.time()),sep = "")
      str_mnt <- paste(tolower(format(Sys.time(), "%b")))
      str_time <- tolower(format(Sys.time(), "%Y%m%d%H"))
      
      str1 <- paste("lead_tms",str_q,str_mnt,"surv",sep = "")
      paste(paste(str1,str_time,sep = "_"),"ctl",sep = ".")
    },
    
    content = function(file) {
      file.create(file)
    })
}


# Create Shiny app ----
shinyApp(ui, server)