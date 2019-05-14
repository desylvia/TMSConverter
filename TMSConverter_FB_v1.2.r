library(shiny)
library(tidyr)
library(stringr)
library(data.table)


#UI Shiny
ui = fluidPage(
  titlePanel("CSV Converter"),
  
  #sidebar-layout
  sidebarLayout(
    #ui-panel
    sidebarPanel(
      
      #input
      fileInput("file1", "Only CSV!",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
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
        dat <- read.table(inFile[[idx, 'datapath']], comment.char = "", header = TRUE, sep = "\t", quote="\"", skipNul = TRUE, check.names = F)
        colnames(dat) <- iconv(names(dat), to = "ASCII", sub = "")
        tmp[[idx]] <- dat
      }
      df <- do.call(rbind, tmp)
    }
    
    #========================================= DATA INJECT
    df <- as.data.table(df) #Convert to data.table
    df$full_name <- str_to_title(df$full_name) #Change case to title case
    
    #Remove: 1) Match name, email, phone. 2) Match name, email. 3) Match name, phone.
    df <- unique(df)
    df <- unique(setDT(df), by = c("email","full_name","phone_number"))
    df <- unique(setDT(df), by = c("email","full_name"))
    df <- unique(setDT(df), by = c("full_name","phone_number"))
    
    #Convert format kapan_anda_bersedia_dihubungi_oleh_pfi_mega_life?, email, 
    #full_name, phone_number to character
    col <- names(df)
    strTmp = c(col[13],col[14],col[15],col[16])
    df[, (strTmp) := lapply(.SD, as.character), .SDcols = strTmp]
    
    #Change phone number format
    for(idx in 1:nrow(df)){
      if(substring(df$phone_number[idx],1,3) == "p:+"){
        df$phone_number[idx] <- substring(df$phone_number[idx],4)
      }
    }
    for(idx in 1:nrow(df)){
      if(substring(df$phone_number[idx],1,1) == "0"){
        substring(df$phone_number[idx],2) = "62"
      }else if(substring(df$phone_number[idx],1,3) == "620"){
        df$phone_number[idx] <- paste(substring(df$phone_number[idx],1,2),
                                      substring(df$phone_number[idx],4),sep = "")
      }else if(substring(df$phone_number[idx],1,3) == "p:0"){
        df$phone_number[idx] <- paste("62",substring(df$phone_number[idx],4),sep = "")
      }
    }
    
    #Remove non alphanumeric and space
    for(idx in 1:nrow(df)){
      df$phone_number[idx] <- gsub("[^[:alnum:]\\n]", "", df$phone_number[idx])
    }
    
    #Extract FirstName and LastName
    ori <- df$full_name #Save original full_name
    df <- extract(df, full_name, c("FirstName", "LastName"), "([^ ]+) (.*)")
    
    na_list <- which(is.na(df$FirstName))
    for(idx in na_list){
      df$FirstName[idx] <- ori[idx]
    }
    df[is.na(df)] <- ""
    
    #Convert input file to defined template
    new_lead <- data.table(v1 = "MHID2C",
                           v2 = paste(df$FirstName[1],df$LastName[1],sep = " "),
                           v3 = df$FirstName[1],
                           v4 = df$LastName[1],
                           v5 = "",
                           v6 = "",
                           v7 = "",
                           v8 = "",
                           v9 = "",
                           v10 = df$email[1],
                           v11 = df$phone_number[1],
                           v12 = df$`kapan_anda_bersedia_dihubungi_oleh_pfi_mega_life?`[1],
                           v13 = "",
                           v14 = "",
                           v15 = "",
                           v16 = "",
                           v17 = "",
                           v18 = df$created_time[1])
    if(nrow(df) > 1){
      for(idx in 2:nrow(df)){
        tmp <- list("MHID2C",paste(df$FirstName[idx],df$LastName[idx],sep = " "),
                    df$FirstName[idx],df$LastName[idx],"","","","","",df$email[idx],
                    df$phone_number[idx],
                    df$`kapan_anda_bersedia_dihubungi_oleh_pfi_mega_life?`[idx],"","","","","",
                    df$created_time[idx])
        new_lead <- rbind(new_lead,tmp)
      }
    }
    
    colnames(new_lead) <- c("Produk","Nama Lengkap","Nama Depan","Nama Belakang","Nama Panggilan",
                            "Umur","Tertanggung Lainya","Cakupan","Premi Per Tahun","Email",
                            "No Handphone","Waktu Dihubungi","Lokasi","Utm Source","Utm Medium",
                            "Utm Campaign","Utm Content","waktu submit")
    
    return(new_lead)
    #==========================================
  })
  
  #Render data to view
  output$contents <- renderTable(
    getData()
  )
  
  #Download data as file TXT
  output$downloadData <- downloadHandler(
    filename = function() { #Format: lead_tmsq1mar2019031200 (00 = jam 00)
      str_q <- paste("q",quarter(Sys.time()),sep = "")
      str_time <- tolower(format(Sys.time(), "%b%Y%m%d%H"))
      
      paste(paste("lead_tms",str_q,str_time,sep = ""),"txt",sep = ".")
    },
    
    content = function(file) {
      write.table(getData(), file, quote = FALSE, row.names = FALSE, sep = "|")
    })
  
  #Download data as file CTL
  output$downloadData2 <- downloadHandler(
    filename = function() {
      str_q <- paste("q",quarter(Sys.time()),sep = "")
      str_time <- tolower(format(Sys.time(), "%b%Y%m%d%H"))
      
      paste(paste("lead_tms",str_q,str_time,sep = ""),"ctl",sep = ".")
    },
    
    content = function(file) {
      file.create(file)
    })
}


#Create Shiny app
shinyApp(ui, server)
