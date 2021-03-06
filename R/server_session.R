#'
#' Session management for HIDAP-AGROFIMS
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
## @param dom target dom element name
#' @param values reactive values
#' @author Ivan Perez / Omar Benites
#' @export
#'

server_session <- function(input, output, session, values){
  
  # path global para lectura de RDS's
  globalpath <- "/home/obenites/AGROFIMS/hagrofims/inst/hidap_agrofims/www/internal_files/"
  
  # path global donde se aloja las sessiones y backups
  sessionpath <- "/home/obenites/AGROFIMS/savesession/"
  sessionpathbk <- "/home/obenites/AGROFIMS/savesession_bk/"
  
  ##### Start Modulo: Refresh list #####
  my_files <- function() {
    lf <- list.files(sessionpath)
    lf
  }
  
  sessionVals <- reactiveValues()
  sessionVals$aux <- data.frame()
  
  refreshDT <- function() {
    df <- data.frame()
    a <- b <- c <- d <- e <- f <- c()
    
    if (length(my_files()) >= 1) {
      for (i in 1:length(my_files())) {
        # Experiment ID
        mf <- my_files()[i]
        mf <- unlist(strsplit(mf, "[.]"))
        a[i] <- mf[1]
        
        # Experiment name
        fl <- read.csv(paste0(sessionpath, my_files()[i]))
        b[i] <- as.character(fl[5, 4])
        
        # Experiment project name
        c[i] <- as.character(fl[6, 4])
        
        # Date created 
        d[i] <- as.character(fl[2, 4])
        
        # Date modified
        #e[i] <- as.character(file.info(paste0(sessionpath, my_files()[i]))$mtime)
        e[i] <- as.character(fl[3, 4])
        
        # User
        f[i] <- as.character(fl[1, 4])
      }
      
      userM <- session$userData$userMail
      
      df <- data.frame(a, b, c, d, e, f, stringsAsFactors = F)
      df <- dplyr::filter(as.data.frame(df), f == userM)
      df <- df %>% dplyr::arrange(desc(e))
      #colnames(df) <- c("Experiment.ID", "Experiment.name", "Experiment project name", "Date.created", "Date.modified", "User")
      
      sessionVals$aux <- data.frame(df)
      colnames(sessionVals$aux) <- c("Experiment ID", "Experiment name", "Experiment project name", "Date created", "Date modified", "User")
    } else {
      
      sessionVals$aux <- data.frame()
    }
  }
  
  observeEvent(input$refreshsession, {
    refreshDT()
  })
  ##### End Modulo: Refresh list ######
  
  ##### Start Modulo: Render session list in DT #####
  output$dtsession <- DT::renderDataTable({
    DT::datatable(
      sessionVals$aux, 
      selection = 'single',
      options = list(
        pageLength = 5,
        columnDefs = list(list(visible=FALSE, targets=c(6)))
        #list(width = '30%', targets = c(1)),
        #list(className = 'dt-center', targets = c(7,8))
      )
    )
  })
  ##### End Modulo: Render session list in DT ######
  
  ##### Start Modulo: Load fieldbook #####
  # Obtiene el id del row del DT
  selectedRow <- eventReactive(input$load_inputs, {
    id <- input$dtsession_rows_selected
    sessionVals$aux[id, 1]
  })
  
  # Load session
  loadsession <- function() {
    if (length(selectedRow() != 0)) {
      if (file.exists(isolate(paste0(sessionpath, selectedRow(), ".csv")))){
        uploaded_inputs <- read.csv(paste0(sessionpath, selectedRow(), ".csv"))
        
        for(i in 1:nrow(uploaded_inputs)) {
          type <- as.character(uploaded_inputs[i, 2])
          create <- as.character(uploaded_inputs[i, 3])
          
          if (type == "textInput") {
            updateTextInput(session,
                            inputId = uploaded_inputs$inputId[i],
                            value = uploaded_inputs$value[i])
          }
          
          if (type == "dateRangeInput") {
            updateDateRangeInput(session,
                                 inputId = uploaded_inputs$inputId[i],
                                 start =  getInputs(uploaded_inputs[i, 4], "start"),
                                 end =  getInputs(uploaded_inputs[i, 4], "end"))
          }
          
          if (type == "selectizeInput" && create == "n") {
            updateSelectizeInput(session,
                                 inputId = uploaded_inputs$inputId[i],
                                 selected = getInputs(uploaded_inputs[i, 4], ""))
          }
          
          if (type == "selectizeInput" && create == "y") {
            updateSelectizeInput(session,
                                 inputId = uploaded_inputs$inputId[i],
                                 selected = getInputs(uploaded_inputs[i, 4], ""),
                                 choices = getInputs(uploaded_inputs[i, 4], ""),
                                 options = list('create' = TRUE))
          }
          
          if (type == "textAreaInput") {
            updateTextAreaInput(session,
                                inputId = uploaded_inputs$inputId[i],
                                value = uploaded_inputs$value[i])
          }
          
          if (type == "numericInput") {
            updateNumericInput(session,
                               inputId = uploaded_inputs$inputId[i],
                               value = uploaded_inputs$value[i])
          }
          
          if (type == "checkboxInput") {
            if (uploaded_inputs$value[i] == "FALSE") {
              x <- FALSE
            } else {
              x <- TRUE
            }
            
            updateCheckboxInput(session,
                                inputId = uploaded_inputs$inputId[i],
                                value = x)
          }
        }
        
        delay(
          1500,
          for(i in 1:nrow(uploaded_inputs)) {
            type <- as.character(uploaded_inputs[i, 2])
            create <- as.character(uploaded_inputs[i, 3])
            
            if (type == "textInput") {
              updateTextInput(session,
                              inputId = uploaded_inputs$inputId[i],
                              value = uploaded_inputs$value[i])
            }
            
            if (type == "dateRangeInput") {
              updateDateRangeInput(session,
                                   inputId = uploaded_inputs$inputId[i],
                                   start =  getInputs(uploaded_inputs[i, 4], "start"),
                                   end =  getInputs(uploaded_inputs[i, 4], "end"))
            }
            
            if (type == "selectizeInput" && create == "n") {
              updateSelectizeInput(session,
                                   inputId = uploaded_inputs$inputId[i],
                                   selected = getInputs(uploaded_inputs[i, 4], ""))
            }
            
            if (type == "selectizeInput" && create == "y") {
              updateSelectizeInput(session,
                                   inputId = uploaded_inputs$inputId[i],
                                   selected = getInputs(uploaded_inputs[i, 4], ""),
                                   choices = getInputs(uploaded_inputs[i, 4], ""),
                                   options = list('create' = TRUE))
            }
            
            if (type == "textAreaInput") {
              updateTextAreaInput(session,
                                  inputId = uploaded_inputs$inputId[i],
                                  value = uploaded_inputs$value[i])
            }
            
            if (type == "numericInput") {
              updateNumericInput(session,
                                 inputId = uploaded_inputs$inputId[i],
                                 value = uploaded_inputs$value[i])
            }
            
            if (type == "checkboxInput") {
              if (uploaded_inputs$value[i] == "FALSE") {
                x <- FALSE
              } else {
                x <- TRUE
              }
              
              updateCheckboxInput(session,
                                  inputId = uploaded_inputs$inputId[i],
                                  value = x)
            }
          }
        )
        
        #output$text2 <- renderText({"Loaded successfully"})
        shinyalert("Loaded successfully", type = "success", timer = 1500, showConfirmButton = F)
      }
      else{
        #output$text <- renderText({"The session file does not exist"})
        shinyalert("Oops!", "The session file does not exist", type = "error", timer = 1500, showConfirmButton = F)
      }
    }
  }
  
  # Funcion que devuelve valores de un array para la funcion Load session
  getInputs<- function(valor, q){
    valor <- sapply(valor, as.character)
    valor[is.na(valor)] <- " "
    valor
    
    if (stringr::str_detect(valor, "&")) {
      if (q == "start") {
        valor <- unlist(strsplit(valor, "&"))
        valor <- valor[[1]]
      }
      
      if (q == "end") {
        valor <- unlist(strsplit(valor, "&"))
        valor <- valor[[2]]
      }
    }
    
    if(stringr::str_detect(valor,"&")){
      valor<-unlist(strsplit(valor, "&"))
    } else {
      valor<-valor
    }
    
    valor
  }
  
  #Load session
  observeEvent(input$load_inputs, {
    loadsession()
  })
  ##### End Modulo: Load fieldbook ######
  
  ##### Start Modulo: Delete fieldbook ######
  observeEvent(input$delete_file, {
    id <- input$dtsession_rows_selected
    i <- sessionVals$aux[id,1]
    r <- paste0(sessionpath, i, ".csv")
    
    if (file.exists(r)) {
      unlink(r)
      #onclick("refreshsession")
      shinyalert("Delete successfully", type = "success", timer = 1500, showConfirmButton = F)
      refreshDT()
    }
  })
  ##### End Modulo: Delete fieldbook ######
  
  ##### Start Modulo: Duplicate fieldbook ######
  # funcion que genera el ID del fieldbook
  idgenerator <- function() {
    id <- stri_rand_strings(1, 8,  '[A-Z0-9]')
    id
  }
  
  # Duplica el fieldbook
  observeEvent(input$duplicate_file, {
    id <- input$dtsession_rows_selected
    i <- sessionVals$aux[id,1]
    r <- paste0(sessionpath, i, ".csv")
    
    if (file.exists(r)) {
      newId <- idgenerator()
      newFile <- file.copy(r, paste0(sessionpath, newId, ".csv"))
      
      df <- as.data.frame(read.csv(paste0(sessionpath, newId, ".csv"), stringsAsFactors = F))
      df[2,4] <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
      df[3,4] <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
      df[4,4] <- newId
      value <- df[5,4]
      df[5,4] <- paste0(value, " - copy")
      
      write.csv(df, paste0(sessionpath, newId, ".csv"), row.names = FALSE)
      write.csv(df, paste0(sessionpathbk, newId, ".csv"), row.names = FALSE)
      
      shinyalert("Duplicate successfully", type = "success", timer = 1500, showConfirmButton = F)
      refreshDT()
    }
  })
  ##### End Modulo: Duplicate fieldbook ######
  
  ##### Start Modulo: Download fieldbook ######
  # funcion que genera el ID del fieldbook
  # idgenerator <- function() {
  #   id <- stri_rand_strings(1, 8,  '[A-Z0-9]')
  #   id
  # }
  
  # Download fieldbook
  observeEvent(input$download_file, {
    # id <- input$dtsession_rows_selected
    # i <- sessionVals$aux[id,1]
    # r <- paste0(sessionpath, i, ".csv")
    # 
    # if (file.exists(r)) {
    #   download.file(r, "filedescarga.csv")
    #   # newId <- idgenerator()
    #   # newFile <- file.copy(r, paste0(sessionpath, newId, ".csv"))
    #   # 
    #   # df <- as.data.frame(read.csv(paste0(sessionpath, newId, ".csv"), stringsAsFactors = F))
    #   # df[2,4] <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
    #   # df[3,4] <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
    #   # df[4,4] <- newId
    #   # value <- df[5,4]
    #   # df[5,4] <- paste0(value, " - copy")
    #   # 
    #   # write.csv(df, paste0(sessionpath, newId, ".csv"), row.names = FALSE)
    #   # write.csv(df, paste0(sessionpathbk, newId, ".csv"), row.names = FALSE)
    #   # 
    #   # shinyalert("Duplicate successfully", type = "success", timer = 1500, showConfirmButton = F)
    #   # refreshDT()
    # }
  })
  ##### End Modulo: Download fieldbook ######
  
}