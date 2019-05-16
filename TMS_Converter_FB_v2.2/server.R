library(tidyr)
library(dplyr)
library(shinyjs)
library(hashids)
library(stringr)
library(data.table)

server <- function(input, output) {
  getData <- reactive({
    inFile <- input$file1
    
    if(is.null(inFile)){
      return(NULL)
    }else{
      tmp <- list()
      for(idx in 1:nrow(inFile)){
        dat <- read.table(inFile[[idx, 'datapath']], comment.char = "", header = TRUE, sep = "\t", skipNul = TRUE, check.names = F)
        colnames(dat) <- iconv(names(dat), to = "ASCII", sub = "")
        tmp[[idx]] <- dat
      }
      dat_ori_new <- do.call(rbind, tmp)
      dat_ori_new$full_name <- str_to_title(dat_ori_new$full_name) #Change case to title case
    }
    
    #========================================= PROCESSING
    df <- as.data.table(dat_ori_new) #Convert to data.table
    #df$full_name <- str_to_title(df$full_name) #Change case to title case
    
    #------------- Load saved data if existed
    setwd("~/My Data/My TMS Data/PFI/Shiny/Saved Data")
    
    tryCatch({  
      assign("data_ori", readRDS("data_ori.rds"), envir=.GlobalEnv)
      assign("data_edt", readRDS("data_edt.rds"), envir=.GlobalEnv)
    },
    error = function( err ) 
    {
      assign("data_ori",0,envir=.GlobalEnv)
      assign("data_edt",0,envir=.GlobalEnv)
    },
    warning = function( w )
    {
      assign("data_ori",0,envir=.GlobalEnv)
      assign("data_edt",0,envir=.GlobalEnv)
    })
    
    #------------- Convert file structure
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
    
    #Remove non alphanumeric and space from phone number and keep only a-z and A-Z from name
    for(idx in 1:nrow(df)){
      df$phone_number[idx] <- gsub("[^[:alnum:]\\n]", "", df$phone_number[idx])
      df$full_name[idx] <- gsub("[^a-zA-Z ]", "", df$full_name[idx])
    }
    
    #Remove duplicates routine from inputed data 
    #Remove: 1) Match name, email, phone. 2) Match name, email. 3) Match name, phone.
    df <- unique(df)
    df <- unique(setDT(df), by = c("email","full_name","phone_number"))
    df <- unique(setDT(df), by = c("email","full_name"))
    df <- unique(setDT(df), by = c("full_name","phone_number"))
    
    #Extract FirstName and LastName
    ori <- df$full_name #Save original full_name
    df <- extract(df, full_name, c("FirstName", "LastName"), "([^ ]+) (.*)")
    
    na_list <- which(is.na(df$FirstName))
    for(idx in na_list){
      df$FirstName[idx] <- ori[idx]
    }
    df[is.na(df)] <- ""
    
    #=== Convert input file to defined template
    #Convert date to Indonesian format
    m <- format(Sys.time(), "%m")
    if(m == "01"){
      m <- "JAN"
    }else if(m == "02"){
      m <- "FEB"
    }else if(m == "03"){
      m <- "MAR"
    }else if(m == "04"){
      m <- "APR"
    }else if(m == "05"){
      m <- "MEI"
    }else if(m == "06"){
      m <- "JUN"
    }else if(m == "07"){
      m <- "JUL"
    }else if(m == "08"){
      m <- "AGU"
    }else if(m == "09"){
      m <- "SEP"
    }else if(m == "10"){
      m <- "OKT"
    }else if(m == "11"){
      m <- "NOV"
    }else if(m == "12"){
      m <- "DES"
    }
    #Concatenate month with year
    m <- paste(m,format(Sys.time(), "%Y"),sep = "")
    
    #--- First index
    #Check FB or IG
    if(grepl("FB", df$adset_name[1], fixed=TRUE)){
      str <- "FB"
    }else{
      str <- "IG"
    }
    
    #Get Creative number from form_name
    ch <- sub(".*Creative","",df$form_name[1])
    if(nchar(ch) == 1){ ch <- paste("0",ch,sep = "") }
    
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
                           v14 = "TMS", #UTM Source
                           v15 = "cpl", #UTM Medium
                           v16 = paste("TMS",str,"LEADGEN",m, sep = "-"), #UTM Campaign Name
                           v17 = paste(str,"ADS-CREATIVE",ch,sep = "-"), #UTM Content
                           v18 = df$created_time[1])
    
    if(nrow(df) > 1){
      for(idx in 2:nrow(df)){
        #Check FB or IG
        if(grepl("FB", df$adset_name[idx], fixed=TRUE)){
          str <- "FB"
        }else{
          str <- "IG"
        }
        
        #Get Creative number from form_name
        ch <- sub(".*Creative","",df$form_name[idx])
        if(nchar(ch) == 1){ ch <- paste("0",ch,sep = "") }
        
        tmp <- list("MHID2C",
                    paste(df$FirstName[idx],df$LastName[idx],sep = " "),
                    df$FirstName[idx],
                    df$LastName[idx],
                    "",
                    "",
                    "",
                    "",
                    "",
                    df$email[idx],
                    df$phone_number[idx],
                    df$`kapan_anda_bersedia_dihubungi_oleh_pfi_mega_life?`[idx],
                    "",
                    "TMS", #UTM Source
                    "cpl", #UTM Medium
                    paste("TMS",str,"LEADGEN",m, sep = "-"), #UTM Campaign Name
                    paste(str,"ADS-CREATIVE",ch,sep = "-"), #UTM Content
                    df$created_time[idx])
        new_lead <- rbind(new_lead,tmp)
      }
    }
    colnames(new_lead) <- c("Produk","Nama Lengkap","Nama Depan","Nama Belakang","Nama Panggilan",
                            "Umur","Tertanggung Lainnya","Cakupan","Premi Per Tahun","Email",
                            "No Handphone","Waktu Dihubungi","Lokasi","UTM Source","UTM Medium",
                            "UTM Campaign Name","UTM Content","Waktu Submit")
    
    #------------- Compare edited data with loaded data (if existed) to remove duplicate
    if(!is.null(nrow(data_edt))){
      new_lead_edt <- new_lead
      
      h = hashid_settings(salt = 'pfimegalife')
      t <- as.numeric(as.character(new_lead_edt$'No Handphone'))
      newcode <- sapply(t, function(x) encode(x,h))
      new_lead_edt <- new_lead_edt[,KEY:=newcode]
      
      tmp <- rbind(data_edt,new_lead_edt)
      tmp <- unique(tmp)
      tmp <- unique(setDT(tmp), by = c("Email","Nama Lengkap","No Handphone"))
      tmp <- unique(setDT(tmp), by = c("Email","Nama Lengkap"))
      tmp <- unique(setDT(tmp), by = c("Nama Lengkap","No Handphone"))
      
      tmp$`Waktu Submit` <- as.character(tmp$`Waktu Submit`)
      new_lead_edt$`Waktu Submit` <- as.character(new_lead_edt$`Waktu Submit`)
      
      dat_cln <- inner_join(new_lead_edt,tmp)
    }else{
      h = hashid_settings(salt = 'pfimegalife')
      t <- as.numeric(as.character(new_lead$'No Handphone'))
      newcode <- sapply(t, function(x) encode(x,h))
      dat_cln <- new_lead
      dat_cln <- dat_cln[,KEY:=newcode]
    }
    
    #------------- Save original data and edited data (.rds format)
    if(!is.null(nrow(data_edt))){
      dat_ori_new <- as.data.table(dat_ori_new)
      dat_ori_new[,':='(Duplicate = 0)]
      
      data_ori <- rbind(data_ori,dat_ori_new)
      data_ori <- as.data.table(data_ori)
      data_ori[,':='(Duplicate = 0)]
      
      idx <- list()
      idx[[1]] <- which(duplicated(data_ori[,c("email","full_name","phone_number")]))
      idx[[2]] <- which(duplicated(data_ori[,c("email","full_name")]))
      idx[[3]] <- which(duplicated(data_ori[,c("full_name","phone_number")]))
      
      for(i in 1:3){
        for(k in idx[[i]]){
          data_ori$Duplicate[k] <- 1
        } 
      }
      data_ori <- data_ori[order(-Duplicate)]
      
      saveRDS(data_ori, "data_ori.rds")
      
      data_edt <- rbind(data_edt,dat_cln)
      saveRDS(data_edt, "data_edt.rds")
    }else{
      dat_ori_new <- as.data.table(dat_ori_new)
      dat_ori_new[,':='(Duplicate = 0)]
      
      idx <- list()
      idx[[1]] <- which(duplicated(dat_ori_new[,c("email","full_name","phone_number")]))
      idx[[2]] <- which(duplicated(dat_ori_new[,c("email","full_name")]))
      idx[[3]] <- which(duplicated(dat_ori_new[,c("full_name","phone_number")]))
      
      for(i in 1:3){
        for(k in idx[[i]]){
          dat_ori_new$Duplicate[k] <- 1
        } 
      }
      dat_ori_new <- dat_ori_new[order(-Duplicate)]
      
      saveRDS(dat_ori_new, "data_ori.rds")
      saveRDS(dat_cln, "data_edt.rds")
    }
    
    return(dat_cln) #Return edited data
    #==========================================
  })
  
  onclick("toggleAdvanced", toggle(id = "advanced", anim = TRUE))
  
  #Render data to view
  output$contents <- renderTable(
    getData()
  )
  
  #Download data as file TXT
  output$new_data_txt <- downloadHandler(
    filename = function() { #Format: lead_tmsq1mar2019031200 (00 = jam 00)
      str_q <- paste("q",quarter(Sys.time()),sep = "")
      str_time <- tolower(format(Sys.time(), "%b%Y%m%d%H"))
      
      paste(paste("lead_tms",str_q,str_time,sep = ""),"txt",sep = ".")
    },
    
    content = function(file) {
      tmp <- getData()
      tmp$KEY <- NULL
      
      write.table(tmp, file, quote = FALSE, row.names = FALSE, sep = "|")
    })
  
  #Download data as file CTL
  output$new_data_ctl <- downloadHandler(
    filename = function() {
      str_q <- paste("q",quarter(Sys.time()),sep = "")
      str_time <- tolower(format(Sys.time(), "%b%Y%m%d%H"))
      
      paste(paste("lead_tms",str_q,str_time,sep = ""),"ctl",sep = ".")
    },
    
    content = function(file) {
      file.create(file)
    })
  
  #------------- Button to save original data and edited data (.rds format)
  output$saved_data_cln <- downloadHandler(
    filename = function(){
      str_time <- tolower(format(Sys.time(), "%b%Y%m%d%H"))
      
      paste(paste("saved_clean",str_time,sep = ""),"txt",sep = ".")
    },
    
    content = function(file){
      setwd("~/My Data/My TMS Data/PFI/Shiny/Saved Data")
      data_edt <- readRDS("data_edt.rds")
      data_edt$KEY <- NULL
      
      
      write.table(data_edt, file, quote = FALSE, row.names = FALSE, sep = "|")
    })
  
  output$saved_data_ori <- downloadHandler(
    filename = function(){
      str_time <- tolower(format(Sys.time(), "%b%Y%m%d%H"))
      
      paste(paste("saved_ori",str_time,sep = ""),"txt",sep = ".")
    },
    
    content = function(file) {
      setwd("~/My Data/My TMS Data/PFI/Shiny/Saved Data")
      data_ori <- readRDS("data_ori.rds")
      
      write.table(data_ori, file, quote = FALSE, row.names = FALSE, sep = "|")
    })
}