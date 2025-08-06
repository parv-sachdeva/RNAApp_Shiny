# Load app data from RDS
loadBookmarkData <- function(sessionID, appDataObject){
    filePath = paste0(getBookmarkDirectory(), "/", sessionID, ".rds")
	tryCatch({
        # Avoid reload for same session
        if("sessionID" %in% names(appDataObject) && sessionID==appDataObject$sessionID) return(appDataObject)
		if (file.exists(filePath)){
			tmp = readRDS(filePath)
			for(item in names(tmp)){
				appDataObject[[item]] = tmp[[item]]
			}
            appDataObject$sessionID <- sessionID
		}else{
            getBookmarkErrorModal()
        }
		return(appDataObject)
	}, 
	interrupt = function(x){print(x);getBookmarkErrorModal()},
	error = function(x){print(x);getBookmarkErrorModal()},
	warning = function(x){print(x);getBookmarkErrorModal()}
	)
}

# Save app data from RDS in server
saveBookmark <- function(sessionID, appDataObject){
	tryCatch({
        if(!dir.exists(getBookmarkDirectory())){
            dir.create(getBookmarkDirectory(), recursive=TRUE)
        }        
        file = paste0(getBookmarkDirectory(), "/", sessionID, ".rds")
        if( !"appName" %in% appDataObject) appDataObject$appName = tolower(getAppName())
        if(appDataObject$appName == "scrnaapp"){
            tryCatch({
                appDataObject$seuratObject@assays$RNA@scale.data <-  new(Class = 'matrix')
            }, 
            interrupt = function(x){},
            error = function(x){},
            warning = function(x){}
            )            
        }
        saveRDS(file=file, reactiveValuesToList(appDataObject))
	}, 
	interrupt = getSaveSessionErrorModal,
	error = getSaveSessionErrorModal,
	warning = getSaveSessionErrorModal
	)          
}

# Delete app data from RDS in server
deleteBookmarkData <- function(sessionID){
    filePath = paste0(getBookmarkDirectory(), "/", sessionID, ".rds")
	tryCatch({
		if (file.exists(filePath)){
            file.remove(filePath)
            deleteBookmarkUserLabels(sessionID)
		}else{
            getBookmarkErrorModal()
        }
        deleteBookmarkUserLabels(sessionID)
	}, 
	interrupt = function(x){print(x);getBookmarkErrorModal()},
	error = function(x){print(x);getBookmarkErrorModal()},
	warning = function(x){print(x);getBookmarkErrorModal()}
	)
}

# Bookmark Session error popup
getBookmarkErrorModal <- function() {
    return(getPopupWindow("Error!", "Could not find session bookmark!"))
}

# Save Session error popup
getSaveSessionErrorModal <- function() {
    return(getPopupWindow("Error!", "Could not save session bookmark!"))
}

# Get Bookmark info modal
getBookmarkInfoModal <- function(session, id, sessionID=NULL){
    ns = NS(id)
    shiny::showModal(
        modalDialog(
            title = "Bookmark Current Analysis",
            textInput(ns("sessionUserLabel"), "Name:", value = "", width = NULL, placeholder = "Note to label current analysis"),
            size = "l",
            easyClose = TRUE,
            footer = fluidRow(
                tooltip(title="Save session bookmark", placement="bottom", actionButton(ns("saveSessionLabel"),"Save",status = "secondary")),
                tooltip(title="Cancel", placement="bottom", actionButton(ns("cancelSessionLabel"),"Cancel", style= "fill:gray;"))
                )
        )
    )
}

# Get Session ID
## If session ID already present in URL it is used and rds file will be updated
## to prevent too many session folders
## Otherwise the url is updated with the current session ID
getSessionID <- function(session=NULL, updateURL=FALSE){
    sessionID <- ids::random_id(n=1, bytes=16)
    if(updateURL){
        updateQueryString(paste0("?session=",sessionID))
    }    
    return(sessionID)
}

# Get Bookmark names, sizes and modified timestamp
getAllSessionInfo <- function(filterEmpty=TRUE, sessionFiles=NULL){
    if(is.null(sessionFiles)){
        sessionFiles <- list.files(getBookmarkDirectory(), full.names=T)
    }
    nonEmptyFiles <- c()
    fileSizes <- c()
    if(length(sessionFiles)<1){
        return(list())
    }
    for (file in sessionFiles){
        tmpFileSize <- utils:::format.object_size(file.size(file), "Mb")
        if(filterEmpty){
            if(as.numeric(strsplit(tmpFileSize, " Mb")[[1]][1]) > 0.5){
                nonEmptyFiles <- c(nonEmptyFiles, file)
                fileSizes <- c(fileSizes, tmpFileSize)
            }
        }else{
            nonEmptyFiles <- c(nonEmptyFiles, file)
            fileSizes <- c(fileSizes, tmpFileSize)            
        }
    }
    if(length(nonEmptyFiles)<1){
        return(list())
    }
    fileModified <- file.mtime(nonEmptyFiles)
    sessionNames <- c()
    for (i in seq(length(nonEmptyFiles))){
        tmpSplit <- strsplit(nonEmptyFiles[i], "/")[[1]]
        sessionNames  = c(sessionNames, stringr::str_replace_all(tmpSplit[length(tmpSplit)], "\\.rds", ""))
    }
    sessionInfo = list(sessionNames=sessionNames, fileSizes=fileSizes, fileModified=fileModified)
    return(sessionInfo)
}

# Get Bookmark settings modal
getBookmarkSettingsModal <- function(session, id, updateURL=FALSE){
    ns = NS(id)
    sessionInfo <- getAllSessionInfo()
    if(length(sessionInfo)<1){
        return(
            getPopupWindow("Error!", "No valid bookmarks exist! Bookmark a session with progress and try again!")
        )
    }

    shiny::showModal(
        modalDialog(
            title = "Saved Bookmarks",
                tags$table(
                class="table table-bordered table-hover",
                style="margin-left:auto; margin-right:auto; width:80%; table-layout: fixed;",
                tags$thead(
                    tags$tr(
                        tags$th(scope="col", style="width:33%;", "Session"),
                        tags$th(scope="col", style="width:8%;", "Size"),
                        tags$th(scope="col", style="width:12%;", "Last Modified"),
                        tags$th(scope="col", style="width:11%;", "Label"),
                        tags$th(scope="col", style="width:35%;", "Options")
                    ),
                ),
                tags$tbody(
                    apply(
                        data.frame(
                            sessionNames = sessionInfo$sessionNames,
                            fileSizes = sessionInfo$fileSizes,
                            fileModified = sessionInfo$fileModified
                        ), 1, function(x){
                            tags$tr(
                                tags$td(x['sessionNames']),
                                tags$td(x['fileSizes']),
                                tags$td(x['fileModified']),
                                tags$td(
                                    tags$input(
                                        type="text",
                                        style="width:100%; height: 100%;",
                                        name=paste0(paste0("Bookmark-",x['sessionNames']),"sessionLab"),
                                        value=getBookmarkUserLabels(x['sessionNames'])
                                    )
                                ),
                                tags$td(
                                    tagList(
                                        actionButton(
                                            inputId=ns(paste0(x['sessionNames'], "Load")),
                                            label="Load",
                                            class="btn btn-default",
                                            icon("check-circle", style="color:green;")
                                        ), 
                                        actionButton(
                                            inputId=ns(paste0(x['sessionNames'], "Update")),
                                            label=NULL,
                                            class="btn btn-default",
                                            icon("user-edit", style="color:solid;")
                                        ),                                       
                                        actionButton(
                                            inputId=ns(paste0(x['sessionNames'], "Delete")),
                                            label=NULL,
                                            class="btn btn-default",
                                            icon("trash-alt", style="color:red;")
                                        ),
                                        downloadButton(
                                            outputId=ns(paste0(x['sessionNames'], "Download")), 
                                            label=NULL,
                                            icon("download", style="color:blue;")
                                        )
                                    )
                                )
                            )
                        })%>%tagList
                )
            ),
            size = "xl",
            easyClose = TRUE
        )
    )
}

# Load bookmark user data labels
getBookmarkUserLabels <- function(sessionID=NULL){
    bookmarkLabelFile <- paste0(getBookmarkDirectory(), "/bookmarkLabels.yaml")
    if(file.exists(bookmarkLabelFile)){
        sessionUserLabels <- yaml::read_yaml(bookmarkLabelFile)
    }else{
        sessionUserLabels <- list()
    }
    if(!is.null(sessionID)){
        if(sessionID %in% names(sessionUserLabels)){
            return(sessionUserLabels[sessionID])
        }else{
            return("")
        }
    }
    return(sessionUserLabels)
}

# Write bookmark user data labels
writeBookmarkUserLabels <- function(bookmarkLabelList){
    bookmarkLabelFile <- paste0(getBookmarkDirectory(), "/bookmarkLabels.yaml")
    tryCatch({
        yaml::write_yaml(bookmarkLabelList, bookmarkLabelFile)
	}, 
	interrupt = function(x){print(x);getFileErrorPopup()},
	error = function(x){print(x);getFileErrorPopup()},
	warning = function(x){print(x);getFileErrorPopup()}
	)    
}

# Delete bookmark user data labels
# @noRd
#'
# @examples
#  \donttest{
#'}
deleteBookmarkUserLabels <- function(sessionID){
    bookmarkLabels <- getBookmarkUserLabels()
    if(sessionID %in% names(bookmarkLabels)){
        bookmarkLabels <- bookmarkLabels[names(bookmarkLabels) != sessionID]
    }
    writeBookmarkUserLabels(bookmarkLabels)
}

# Update bookmark user data labels
# @noRd
#'
# @examples
#  \donttest{
#'}
updateBookmarkUserLabels <- function(sessionName, sessionID){
    bookmarkLabels <- getBookmarkUserLabels()
    if(sessionID %in% names(bookmarkLabels) && sessionName != bookmarkLabels[sessionID] ){
        print(sessionName)
        print(bookmarkLabels[sessionID])
        bookmarkLabels[sessionID] <- sessionName
        writeBookmarkUserLabels(bookmarkLabels)
    }
}

# Bookmark UI function in Bookmark shiny module
bookmarkUI<- function(id){
    ns = NS(id)
      shiny::tagList(
      shiny::tags$li(hr()),
      tags$li(class = "nav-item", 
        actionLink(
          inputId=ns("bookMarkSession"), 
          class="nav-link",
          label=NULL,
          tagList(
            tagAppendAttributes(icon("bookmark"),class = "nav-icon"),
            p("Add Bookmark")
          ),
          icon=NULL,
          style="margin:0;"
        )
      ),
      tags$li(class = "nav-item", 
        actionLink(
          inputId=ns("launchBookmarkModal"), 
          class="nav-link",
          label=NULL,
          tagList(
            tagAppendAttributes(icon("book"),class = "nav-icon"),
            p("Load Bookmark")
          ),          
          icon=NULL,
          style="margin:0;"
        )
      )
      )    
}

# Get Bookmark Server
bookmarkServer <- function(id,appDataObject) {
    moduleServer(
        id,
        function(input,output,session) {
            observeEvent(input$bookMarkSession,{
                getBookmarkInfoModal(session, id)
            })

            observeEvent(input$cancelSessionLabel,{
                removeModal()
            })

            observeEvent(input$saveSessionLabel,{
                req(input$sessionUserLabel)
                sessionUserLabels <- getBookmarkUserLabels()
                if(input$sessionUserLabel %in% unlist(sessionUserLabels, use.names=FALSE)){
                    showNotification("Session label already exists!", duration = 10, type="warning")
                }else{
                    tryCatch({
                        withProgress( 
                            message = paste("Saving bookmark"), value=0,{
                                incProgress(0.3, detail = "saving session info...")
                                appDataObject$sessionID <- getSessionID(updateURL=FALSE)
                                sessionUserLabels[appDataObject$sessionID] <- input$sessionUserLabel
                                saveBookmark(sessionID=appDataObject$sessionID, appDataObject=appDataObject)
                                incProgress(0.9, detail = "writing bookmark label...")
                                writeBookmarkUserLabels(sessionUserLabels)
                            setProgress(1)
                        removeModal()
                        })
                    }, 
                    interrupt = function(x){print(x);getSaveSessionErrorModal()},
                    error = function(x){print(x);getSaveSessionErrorModal()},
                    warning = function(x){print(x);getSaveSessionErrorModal()}
                    )
                }
            })

            ## Whenever URL is refereshed the app tries to load session if it exists 
            observeEvent(session$clientData$url_search,{
                if("session" %in% names(getQueryString(session))){
                    withProgress(
                        message = paste("Loading bookmark"), value=0,{
                            incProgress(0.3, detail = "loading bookmarked session...")
                                sessionID = getQueryString()$session
                                loadBookmarkData(sessionID, appDataObject=appDataObject)
                        setProgress(1)
                    })
                }
            })

            ## Session loading from input box and button
            observeEvent(input$launchBookmarkModal,{
                getBookmarkSettingsModal(session, id)
            })

            # Reactive Poll to monitor last modified time of the bookmark folder
            # The bookmark settings modal server side is updated if folder is modified 
            reactiveSessionInfo <- reactivePoll(
                intervalMillis=1000, 
                session=session,
                checkFunc=function(){
                    if(file.exists(getBookmarkDirectory())){
                        return(file.info(getBookmarkDirectory())$mtime[1])
                    }else{
                        return("")
                    }
                },
                valueFunc=getAllSessionInfo
                )
            observeEvent(reactiveSessionInfo(),{

                ## Bookmark options in a loop
                if(length(getAllSessionInfo()) > 0){
                    lapply(
                        X = getAllSessionInfo()$sessionNames,
                        FUN = function(x){
                            observeEvent(input[[paste0(x,"Load")]], {
                                withProgress(
                                    message = paste("Loading bookmark"), value=0,{
                                        incProgress(0.3, detail = "loading bookmarked session...")
                                            removeModal()
                                            loadBookmarkData(x, appDataObject=appDataObject)
                                    setProgress(1)
                                })
                            })
                            observeEvent(input[[paste0(x,"Update")]], {
                                sessionLab <- input[[paste0(x,"sessionLab")]]
                                updateBookmarkUserLabels(sessionLab, x)
                                getBookmarkSettingsModal(session, id)
                            })
                            observeEvent(input[[paste0(x,"Delete")]], {
                                deleteBookmarkData(x)
                                getBookmarkSettingsModal(session, id)
                            })
                            output[[paste0(x,"Download")]] <- downloadHandler(
                                filename = function() {
                                    ## Fetching app name from working directory
                                    paste0(strsplit(getwd(), "/")[[1]][length(strsplit(getwd(), "/")[[1]])], ".rds")
                                },
                                content = function(file) {
                                    file.copy(file.path(paste0(file.path(getWritableDir(), "bookmarkSession"), "/", x, ".rds")), file)
                                }
                            )
                        }
                    )
                }
            })
        }
    )
}

# Get bookmark directory
getBookmarkDirectory <- function(){
    # bookmarkDir <- file.path(getWritableDir(), "bookmarkSession")
    bookmarkDir <- file.path(getWritableDir(), paste0(getAppName(), "bookmarkSession"))
    return(bookmarkDir)
}

# Get a directory with write permission
getWritableDir <- function(dirs=c('/tmp/', '~', '.')){
    # Returns the first writable path from the dirs vector

    # Check permission to write
    writableStatus <- file.access(dirs, 2)
    # Filter only writable directories
    writableDirs <- writableStatus[lapply(writableStatus, as.numeric)==0]
    # Return the first one
    return(normalizePath(names(writableDirs)[1]))
}