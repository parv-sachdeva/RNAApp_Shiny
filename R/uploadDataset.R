uploadUI <- function(id) {
    ns <- NS(id)

    tabItem(
        tabName = "Upload",
        actionLink(ns("infoModal"), span("Walkthrough", icon("question-circle"))),
        p("Note: Use the i icons to download test datasets.", style="color:red;"),
        p("Note: Click on Next button to proceed after data upload.", style="color:red;"),
        fluidRow(    
            createUploader(id, "countData", "Read Count", multipleFileInput=TRUE),
            createUploader(id, "metaData", "Meta Data")
        ),
        fluidRow(
            box(
                title = "Count Data Preview", 
                closable = TRUE,
                collapsible = TRUE,
                solidHeader = FALSE, 
                width = 6,                
                DT::DTOutput(ns("countData")),
                uiOutput(ns('convertToCountsMatrix'))
            ),
            box(
                title = "Sample Metadata", 
                closable = FALSE,
                collapsible = TRUE,
                solidHeader = FALSE, 
                width = 6,                
                DT::DTOutput(ns("metaData"))
            )
        ),
        fluidRow(
            column(12,
                actionButton(inputId=ns("btnUploadNext"),label = div("Next",icon("angle-right")), status = "secondary",class="float-right")%>%tooltip(
                    title = "This triggers the data ingestion. And the page with switch to the next module."
                )
            )       
        )
    )
}

createUploader <- function(id, inputId, label, withType = F, width_ =6, multipleFileInput=FALSE) {
    ns = NS(id)
    box(
        width = width_,
        maximizable = TRUE,
        title =  span(paste(label, "File"),actionButton(ns(paste0(inputId, "Info")), icon("info"), size ="xs")),
        sidebar = boxSidebar(
            startOpen = F,
            id = ns(paste0(inputId,"SideBar")),
            background = "#7f7f7f",
            width =25,
            selectInput(
                ns(paste0(inputId, "Quote")), 
                label="Field wrapping", 
                choices=c(
                    "Double quote" = "\"",
                    "No quote" = " ",
                    "Single quote" = "'"
                )
            ),
            if(withType){
                selectInput(
                    inputId = ns("countType"),
                    label = "Count type",
                    choices= c(
                        "Raw Count" = "Count",
                        "TPM" = "TPM"
                    )
                )
            }
        ),
        fluidRow(
            column(6,
                radioButtons(
                    ns(paste0(inputId, "Type")),
                    label = "Data source:",
                    choices = list(
                        "Local" = "local",
                        "Cloud" = "Cloud"),
                    selected = "local",
                    inline = TRUE
                )
            )
        ),
        conditionalPanel(
            paste0("input.",inputId,"Type=='Cloud'"),
            ns = ns,
            h5("Cloud data support coming soon!")
        ),
        conditionalPanel(
            paste0("input.",inputId,"Type=='local'"),
            ns = ns,
            fileInput(
                inputId = ns(inputId),
                label = NULL,
                multiple = multipleFileInput
            )
        )
    )
}


uploadDataset <- function(id, parentSession,data) {
    moduleServer(
        id,
        function(input,output,session) {
            fileCacheData <- reactiveValues()

            observe({
                # Only load if not already populated (e.g., after file upload)
                if (Sys.getenv("DISPLAY_MODE") == "DEMO" && is.null(data$counts) && is.null(data$samples)) {
                    tryCatch({
                        message("Loading demo data...")
                        data$counts <- read.csv("sample-data/GSE148505_counts.csv", row.names = 1) %>%
                            dplyr::filter(dplyr::if_all(.fns = ~ . != 0))
                        
                        print(data$counts %>% head)
                        
                        data$samples <- parseSampleSheet("sample-data/GSE148505_SampleSheet.csv")
                        data$genome <- data$samples$genome[1]
                        print(data$samples %>% head)
                        runDESeqFromUpload(data, parentSession=parentSession)
                        updateTabItems(session=parentSession, inputId= "sidebarTabs", "Sample")
                    }, 
                    interrupt = function(x){ getFileErrorModal(); print(x);},
                    error = function(x){ getFileErrorModal(); print(x);}
                    )
                }
            })
            
            # Get local countData 
            observeEvent(input$countData,{
                if(input$countDataType =="local"){
                    tryCatch({
                        if(grepl("csv|tsv", input$countData$name[1])){
                            df <- read.csv(input$countData$datapath, quote = input$countDataQuote, row.names=1)
                            data$counts = df %>% dplyr::filter(dplyr::if_all(.fns= ~ . != 0))
                        }else{
                            stop("Invalid format")
                        }
                    }, 
                    interrupt = function(x){ getFileErrorModal(); print(x);},
                    error = function(x){ getFileErrorModal(); print(x);}
                    )
                }
            })


            output$countData <- DT::renderDataTable({
                req(nrow(data$counts)>1)
                DT::datatable(
                    data$counts,
                    class = "compact cell-border stripe",
                    options = list(
                        scrollX = TRUE
                        )
                )
            })

    
            # Get and process the metadata file
            observeEvent(input$metaData,{
                #Adding logic to parse 
                tryCatch({
                    if(! grepl("csv", input$metaData$name)){
                        stop("Invalid format")
                    }
                    data$samples = parseSampleSheet(input$metaData$datapath)
                    data$genome = data$samples$genome[1]
                }, 
                interrupt = getFileErrorModal,
                error = getFileErrorModal
                )                           
            })
            
            output$metaData <- DT::renderDataTable({
                req(nrow(data$samples)>1)
                DT::datatable(data$samples, class = "compact cell-border stripe",
                            options = list(scrollX = TRUE),editable=TRUE
                )
            })

            #udate group values for sample sheet
            observeEvent(input$metaData_cell_edit, {
                row  <- input$metaData_cell_edit$row
                clmn <- input$metaData_cell_edit$col
                data$samples[row, clmn] <- input$metaData_cell_edit$value
            })

            # next button 
            observeEvent(input$btnUploadNext,{
                ns <- session$ns
                req(data$counts, data$samples)
                tryCatch({
                    if(any(data$samples$group=="")){
                        stop("Sample mismatch!")
                    }else{
                        runDESeqFromUpload(data, parentSession=parentSession)
                    }
                }, 
                interrupt = function(x){ getFileErrorModal; print(x);},
                error = function(x){ getFileErrorModal; print(x);}
                )
            })
            # Modal for counts data
            observeEvent(input$countDataInfo,{
                ns <- session$ns
                
                showModal(modalDialog(
                    title = "Counts Data Upload",
                    tagList(
                        HTML(data$descriptions[['count_data_info']]),
                        downloadButton(ns('downloadExampleCountsData'), 'Download Sample Counts')
                    ),
                    size = "l",
                    easyClose = TRUE,
                    footer = NULL
                ))
            })

            # Demo counts data download 
            output$downloadExampleCountsData <- downloadHandler(
                filename = function() {
                    paste('GSE148505_counts', '.csv', sep='')
                },
                content = function(file) {
                    tmp_data = read.csv("sample-data/GSE148505_counts.csv")
                    write.csv(tmp_data, file,  row.names=FALSE)
                }
            )

            # Modal for meta data
            observeEvent(input$metaDataInfo,{
                ns <- session$ns
                
                showModal(modalDialog(
                    title = "Meta Data Upload",
                    tagList(
                        HTML(data$descriptions[['meta_data_info']]),
                        downloadButton(ns('downloadMetaData'), 'Download Sample Metadata')
                    ),
                    size = "l",
                    easyClose = TRUE,
                    footer = NULL
                ))
            })
            # Demo counts data download 
            output$downloadMetaData <- downloadHandler(
                filename = function() {
                    paste('GSE148505_SampleSheet', '.csv', sep='')
                },
                content = function(file) {
                    tmp_data = readLines("sample-data/GSE148505_SampleSheet.csv")
                    writeLines(tmp_data, file)
                }
            )    

            # Downloadable counts matrix
            output$downloadCountData <- downloadHandler(
                filename = function() {
                    paste('counts', '.csv', sep='')
                },
                content = function(file) {
                    # handle the counts generating if user directly click the download button
                    if(is.null(data$counts)){
                        # convert to counts table
                        data$counts <- convertQuantfilesToCounts(data)
                    }
                    write.csv(data$counts, file,  row.names=TRUE)
                }
            )            
            observeEvent(input$infoModal,{
                req(data$descriptions)
                getRnaAppUploadInfoModal(data)
            })
        }
    )
}