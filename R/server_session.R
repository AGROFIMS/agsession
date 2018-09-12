#'
#' Session management for HIDAP-AGROFIMS
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
## @param dom target dom element name
#' @param values reactive values
#' @author Omar Benites Ivan Perez
#' @export
#'

server_session <- function(input, output, session, values){
  
  # path global para guardar las sesiones de fieldbook
  globalpath <- "/home/obenites/HIDAP_SB_1.0.0/hidap/inst/hidap_agrofims/www/internal_files/"
  
  ##### Start Modulo: Refresh list #####
  my_files <- function() {
    lf <- list.files(paste0(globalpath, "savesession/"))
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
        fl <- read.csv(paste0(globalpath, "savesession/", my_files()[i]))
        b[i] <- as.character(fl[4,2])
        
        # Experiment project name
        c[i] <- as.character(fl[5,2])
        
        # Date created
        d[i] <- as.character(fl[2,2])
        
        # Date modified
        e[i] <- as.character(file.info(paste0(globalpath, "savesession/", my_files()[i]))$mtime)
        
        # User
        f[i] <- as.character(fl[1,2])
      }
      
      userM <- session$userData$userMail
      
      df <- data.frame(a, b, c, d, e, f, stringsAsFactors = F)
      df <- dplyr::filter(as.data.frame(df), f == userM)
      df <- df %>% dplyr::arrange(desc(d))
      colnames(df) <- c("Experiment.ID", "Experiment.name", "Experiment project name", "Date.created", "Date.modified", "User")
      
      sessionVals$aux <- data.frame(df)
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
  selectedRow <- eventReactive(input$load_inputs, {
    id <- input$dtsession_rows_selected
    sessionVals$aux[id, 1]
  })
  
  checktype <- function(up) {
    
    # list1: updateTextInput
    list1 <- c("user",
               "datec",
               "experimentId",
               "experimentName",
               "experimentProjectName",
               "fundName_1",
               "fundName_2",
               "fundName_3",
               "fundName_4",
               "fundName_5",
               "fundName_6",
               "fundName_7",
               "fundName_8",
               "fundName_9",
               "projEntity_1_other",
               "projEntity_2_other",
               "projEntity_3_other",
               "projEntity_4_other",
               "projEntity_5_other",
               "projEntity_6_other",
               "projEntity_7_other",
               "projEntity_8_other",
               "projEntity_9_other",
               "projEntity_10_other",
               "lead_org_type_1_1_other",
               "leadNameOther_1",
               "expLead_1",
               "lead_org_type_1_2_other",
               "leadNameOther_2",
               "expLead_2",
               "lead_org_type_1_3_other",
               "leadNameOther_3",
               "expLead_3",
               "lead_org_type_1_4_other",
               "leadNameOther_4",
               "expLead_4",
               "lead_org_type_1_5_other",
               "leadNameOther_5",
               "expLead_5",
               "lead_org_type_1_6_other",
               "leadNameOther_6",
               "expLead_6",
               "lead_org_type_1_7_other",
               "leadNameOther_7",
               "expLead_7",
               "lead_org_type_1_8_other",
               "leadNameOther_8",
               "expLead_8",
               "lead_org_type_1_9_other",
               "leadNameOther_9",
               "expLead_9",
               "lead_org_type_1_10_other",
               "leadNameOther_10",
               "expLead_10",
               "personnel1Type_other",
               "person1FirstName",
               "person1LastName",
               "person1Email",
               "person1Affiliation_other",
               "person1ORCID",
               "personnel2Type_other",
               "person2FirstName",
               "person2LastName",
               "person2Email",
               "person2Affiliation_other",
               "person2ORCID",
               "personnel3Type_other",
               "person3FirstName",
               "person3LastName",
               "person3Email",
               "person3Affiliation_other",
               "person3ORCID",
               "personnel4Type_other",
               "person4FirstName",
               "person4LastName",
               "person4Email",
               "person4Affiliation_other",
               "person4ORCID",
               "personnel5Type_other",
               "person5FirstName",
               "person5LastName",
               "person5Email",
               "person5Affiliation_other",
               "person5ORCID",
               "cropCommonNameMono_other",
               "prevCropName_other",
               "cropCommonName1",
               "cropCommonName2",
               "cropCommonName3",
               "cropCommonName4",
               "cropCommonName5",
               "cropCommonName6",
               "cropCommonName7",
               "intercropValue_row_crop_1",
               "intercropValue_row_crop_2",
               "intercropValue_row_crop_3",
               "intercropValue_row_crop_4",
               "intercropValue_row_crop_5",
               "intercropValue_row_crop_6",
               "intercropValue_row_crop_7")
    
    # list2: updateDateRangeInput
    list2 <- c("fbDesign_project_time_line")
    
    # list3: updateSelectizeInput && updateSelectInput --> Tipo1
    list3 <- c("designFieldbook_typeExperiment",
               "designFieldbook_fundAgencyType",
               "projEntity_1",
               "contCenter_1",
               "contCRP_1",
               "projEntity_2",
               "contCenter_2",
               "contCRP_2",
               "projEntity_3",
               "contCenter_3",
               "contCRP_3",
               "projEntity_4",
               "contCenter_4",
               "contCRP_4",
               "projEntity_5",
               "contCenter_5",
               "contCRP_5",
               "projEntity_6",
               "contCenter_6",
               "contCRP_6",
               "projEntity_7",
               "contCenter_7",
               "contCRP_7",
               "projEntity_8",
               "contCenter_8",
               "contCRP_8",
               "projEntity_9",
               "contCenter_9",
               "contCRP_9",
               "projEntity_10",
               "contCenter_10",
               "contCRP_10",
               "projLeadEnt_1",
               "tLeadCenter_1",
               "lead_org_type_1_1",
               "projLeadEnt_2",
               "tLeadCenter_2",
               "lead_org_type_1_2",
               "projLeadEnt_3",
               "tLeadCenter_3",
               "lead_org_type_1_3",
               "projLeadEnt_4",
               "tLeadCenter_4",
               "lead_org_type_1_4",
               "projLeadEnt_5",
               "tLeadCenter_5",
               "lead_org_type_1_5",
               "projLeadEnt_6",
               "tLeadCenter_6",
               "lead_org_type_1_6",
               "projLeadEnt_7",
               "tLeadCenter_7",
               "lead_org_type_1_7",
               "projLeadEnt_8",
               "tLeadCenter_8",
               "lead_org_type_1_8",
               "projLeadEnt_9",
               "tLeadCenter_9",
               "lead_org_type_1_9",
               "projLeadEnt_10",
               "tLeadCenter_10",
               "lead_org_type_1_10",
               "npersons",
               "personnel1Type",
               "person1Affiliation",
               "person1Center",
               "personnel2Type",
               "person2Affiliation",
               "person2Center",
               "personnel3Type",
               "person3Affiliation",
               "person3Center",
               "personnel4Type",
               "person4Affiliation",
               "person4Center",
               "personnel5Type",
               "person5Affiliation",
               "person5Center",
               "fbDesign_countryTrial",
               "designFieldbook_sites",
               "fbDesign_inHighLevel",
               "fbDesign_inSiteVegetation",
               "croppingType",
               "cropCommonNameMono",
               "prevCropName",
               "cropsSelected")
    
    # List3_1: updateSelectizeInput && updateSelectInput --> Tipo2
    List3_1 <- c("cultivarNameMono",
                 "cropVarietyName1",
                 "cropVarietyName2",
                 "cropVarietyName3",
                 "cropVarietyName4",
                 "cropVarietyName5",
                 "cropVarietyName6",
                 "cropVarietyName7")
    
    # list4: updateTextAreaInput
    list4 <- c("experimentObj", "inSiteDescNotes")
    
    # list5: updateNumericInput
    list5 <- c("numProjEntity",
               "numLeads")
    
    
    # # Text input
    # list1 <- c("user" ,"experimentId", "experimentName", "experimentProjectName")
    # # Text input dinamicos
    # list1_din <- c("fundName_1", "fundName_2", "fundName_3", "fundName_4", "fundName_5", "fundName_6", "fundName_7", "fundName_8", "fundName_9")
    # # Date range input
    # list2 <- c("fbDesign_project_time_line")
    # # Selectize input
    # list3 <- c("designFieldbook_typeExperiment", "designFieldbook_fundAgencyType")
    # # Text area
    # list4 <- c("experimentObj")
    # # updateNumericInput
    # list5 <- c()
    
    if (up %in% list1) {
      return("updateTextInput")
    }

    # if (up %in% list1_din) {
    #   return("updateTextInput_din")
    # }

    if (up %in% list2) {
      return("updateDateRangeInput")
    }

    if (up %in% list3) {
      return("updateSelectizeInput")
    }
    
    if (up %in% List3_1) {
      return("updateSelectizeInput_t2")
    }

    if (up %in% list4) {
      return("updateTextAreaInput")
    }
    
    if (up %in% list5) {
      return("updateNumericInput")
    }
  }
  
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
    if (length(selectedRow() != 0)) {
      if (file.exists(isolate(paste0(globalpath, "savesession/", selectedRow(), ".csv")) ) ){
        uploaded_inputs <- read.csv(paste0(globalpath, "savesession/", selectedRow(), ".csv"))
        
        for(i in 1:nrow(uploaded_inputs)) {
          
          a <- checktype(uploaded_inputs$inputId[i])
          
          if (a == "updateTextInput") {
            updateTextInput(session,
                            inputId = uploaded_inputs$inputId[i],
                            value = uploaded_inputs$value[i])
          }
          
          if (a == "updateDateRangeInput") {
            updateDateRangeInput(session,
                                 inputId = uploaded_inputs$inputId[i],
                                 start =  getInputs(uploaded_inputs[i,2], "start"),
                                 end =  getInputs(uploaded_inputs[i,2], "end"))
          }
          
          if (a == "updateSelectizeInput") {
            updateSelectizeInput(session,
                                 inputId = uploaded_inputs$inputId[i],
                                 selected = getInputs(uploaded_inputs[i,2], ""))
          }
          
          if (a == "updateSelectizeInput_t2") {
            updateSelectizeInput(session,
                                 inputId = uploaded_inputs$inputId[i],
                                 selected = getInputs(uploaded_inputs[i,2], ""),
                                 choices = getInputs(uploaded_inputs[i,2], ""),
                                 options = list('create' = TRUE))
          }
          
          if (a == "updateTextAreaInput") {
            updateTextAreaInput(session,
                                inputId = uploaded_inputs$inputId[i],
                                value = uploaded_inputs$value[i])
          }
          
          if (a == "updateNumericInput") {
            updateNumericInput(session,
                               inputId = uploaded_inputs$inputId[i],
                               value = uploaded_inputs$value[i])
          }
        }
        
        delay(
          500,
          for(i in 1:nrow(uploaded_inputs)) {
            a <- checktype(uploaded_inputs$inputId[i])
            
            if (a == "updateTextInput") {
              updateTextInput(session,
                              inputId = uploaded_inputs$inputId[i],
                              value = uploaded_inputs$value[i])
            }
            
            if (a == "updateDateRangeInput") {
              updateDateRangeInput(session,
                                   inputId = uploaded_inputs$inputId[i],
                                   start =  getInputs(uploaded_inputs[i,2], "start"),
                                   end =  getInputs(uploaded_inputs[i,2], "end"))
            }
            
            if (a == "updateSelectizeInput") {
              updateSelectizeInput(session,
                                   inputId = uploaded_inputs$inputId[i],
                                   selected =  getInputs(uploaded_inputs[i,2], ""))
            }
            
            if (a == "updateTextAreaInput") {
              updateTextAreaInput(session,
                                  inputId = uploaded_inputs$inputId[i],
                                  value = uploaded_inputs$value[i])
            }
            if (a == "updateNumericInput") {
              updateNumericInput(session,
                                 inputId = uploaded_inputs$inputId[i],
                                 value = uploaded_inputs$value[i])
            }
          }
        )
        
        #output$text2 <- renderText({"Loaded successfully"})
        shinyalert("Success", "Loaded successfully", type = "success", timer = 1500, showConfirmButton = F)
      }
      else{
        #output$text <- renderText({"The session file does not exist"})
        shinyalert("Oops!", "The session file does not exist", type = "error", timer = 1500, showConfirmButton = F)
      }
    }
  })
  ##### End Modulo: Load fieldbook ######
  
  ##### Start Modulo: Delete fieldbook ######
  observeEvent(input$delete_file, {
    id <- input$dtsession_rows_selected
    i <- sessionVals$aux[id,1]
    r <- paste0(globalpath,i,".csv")
    
    if (file.exists(r)) {
      unlink(r)
      onclick("refreshsession")
    }
    
    shinyalert("Success", "Delete successfully", type = "success", timer = 1500, showConfirmButton = F)
    resertDT()
    
  })
  ##### End Modulo: Delete fieldbook ######
  
}