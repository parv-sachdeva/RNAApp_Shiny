#' Get App Name
#'
#' @return App name
#' @noRd
#'
#' @examples
#'  \donttest{
#'}
getAppName <- function(){
    # return(tail(stringr::str_split(getwd(),"/")[[1]],1))
    return("rnaApp")
}

#' File error popup 
#'
#' @param cond condition from trycatch
#'
#' @return A popup window. 
#' @export
#'
#' @examples
#'  \donttest{
#'}

getFileErrorModal <- function(cond=NULL) {
    # message(cond)
    # shiny::showNotification(base::.makeMessage(cond))
    return(getModal("Error!", "Uploaded file is invalid, please check correct format!"))
}

#' Parse the SampleSheet csv file.
#'
#' @param samplesheet_file_path THe samplesheet's local path.
#'
#' @return A data.frame of the content of the samplesheet.
#' @export
#'
#' @examples
#'  \donttest{
#'}

parseSampleSheet = function(samplesheet_file_path){
    # load sample data 
    samplesheet = readLines(samplesheet_file_path)
    samplesheet=gsub(" ", "", samplesheet, fixed = TRUE)
    i=0
    samples = c()
    groups = c()
    file=c()
    treatment_list = c()
    genome = c()
    for (line in samplesheet){
        if (i==1){
            sample = stringr::str_split(line, ",")[[1]][1]
            file = stringr::str_split(line, ",")[[1]][2]
            genome = stringr::str_split(file, "/")[[1]][length(stringr::str_split(file, "/")[[1]])]
            group = stringr::str_split(line, ",")[[1]][3]
            samples = c(samples, sample)
            groups = c(groups, group)
        }  
        if (grepl(toupper("name,file,group"), toupper(line)) || grepl(toupper("name,genome,group"), toupper(line))){
            i=1
        }
     
    }
    runName = paste0(unique(groups)[1], "_vs_", unique(groups)[2])
    i=1
    out = data.frame(name=samples, genome=genome, group=groups)
    if(nrow(out)==0){
        stop("Invalid format")
    }
    return(out)
}
#' Get value box
#'
#' @param title Value box title
#' @param value Value box value
#' @param color Value box color, refer bs4Dash::valueBox
#' @param icon Value box icon
#' @noRd
#'
#' @return 
#' @examples
#'  \donttest{
#'}
get_value_box = function(title, value, color, icon_name, href=NULL,linkSource=NULL, value_size="H4",...){
    #Valid colors
    #primary, secondary, info, success, warning, danger, gray-dark, 
    #gray, white, indigo, lightblue, navy, purple, fuchsia, pink, maroon, orange, lime, teal, olive.
    value = HTML(paste0("<", value_size, ">", value, "</", value_size, ">"))
    if(!is.null(href) & !is.null(linkSource)){
        footerLink = shiny::tags$a(href = href, target = "_blank", class = "small-box-footer",
            paste0("More info [",linkSource,"]"), shiny::icon("arrow-circle-right"))
    }else{footerLink=NULL}
    
    # value = h4(value)
    return(
        bs4Dash::valueBox(
            value,
            title,
            color=color,
            icon = icon(icon_name),
            footer = footerLink,
            ...
        )
    )        
}

#' Prepare GSEA annotation DB
#'
#' @param data Server data
#' @return 
#' @noRd
#' @examples
#'  \donttest{
#'}

prepareAnnotationDb = function(data){
    # load org.xx.eg.db and assign kegg organism string
    
    # return if already existed
    # if ("organism" %in% names(data) & "kegg_organism" %in% names(data)){
    #     return()
    # }

    if (grepl("mm10", data$genome)){
        data$organism = org.Mm.eg.db::org.Mm.eg.db
        data$kegg_organism = "mmu"
    }else if (grepl("hg19", data$genome)){
        data$organism = org.Hs.eg.db::org.Hs.eg.db
        data$kegg_organism = "hsa"
    } else if (grepl("rn6", data$genome)){
        data$organism = org.Rn.eg.db::org.Rn.eg.db
        data$kegg_organism = "rno" #https://github.com/YuLab-SMU/clusterProfiler/blob/master/R/enrichKEGG.R
    }else{
        data$organism = NULL
    }
    return(data$organism)
}

#' Draw PIE chart
#'
#' @param serverData Server data
#' @param group Comparison name
#' @return 
#' @noRd
#' @examples
#'  \donttest{
#'}

drawPieChart <- function(serverData, group) {
    return(
        serverData$comparisons[[group]]$DE %>%
                dplyr::count(type, name = '#Genes')%>%
                plot_ly(., 
                labels = ~type, 
                values = ~`#Genes`, 
                type = 'pie',
                textinfo = 'label',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'label+text+percent',
                text = ~`#Genes`,
                marker = list(colors = serverData$deCatColors,
                                line = list(color = '#FFFFFF', width = 1)),
                showlegend = TRUE) %>% 
                layout(
                title = '',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                )
    )
}

#' Draw Volcano + MA
#'
#' @param serverData Server data
#' @param group Comparison name
#' @return 
#' @noRd
#' @examples
#'  \donttest{
#'}
drawVolcanoMA <- function(serverData, group, mode='html') {
    d <- highlight_key(serverData$comparisons[[group]]$DE)

    p1 <- (ggplot(d, aes(baseMean, log2FoldChange, color = type)) + geom_point(size = 0.5, aes(text = paste0(
            'Gene: ',geneID,
            '\ntype: ', type,
            '\nbaseMean: ',baseMean%>%round(1),
            '\nlog2FoldChange: ',log2FoldChange%>%round(1)
            ))) +
            theme_bw() + scale_x_log10() + geom_hline(yintercept = 0, color = 'black') +
            geom_hline(yintercept = c(-log2(serverData$comparisons[[group]]$fcThreshold), log2(serverData$comparisons[[group]]$fcThreshold)), linetype = 2) +                    
            scale_color_manual(values = serverData$deCatColors) + theme(legend.position = 'none')) 
    p2 <- (ggplot(d, aes(log2FoldChange, -log10(padj))) + geom_point(size = 0.5, aes(color = type, text = paste0(
            'Gene: ',geneID,
            '\ntype: ', type,
            '\nlog2FoldChange: ',log2FoldChange %>%round(1),
            '\n-log10(padj): ',(-log10(padj))%>%round(1)
            ))) +
            theme_bw() + geom_vline(xintercept = 0, color = 'black') + 
            geom_vline(xintercept = c(-log2(serverData$comparisons[[group]]$fcThreshold), log2(serverData$comparisons[[group]]$fcThreshold)), linetype = 2) + geom_hline(yintercept = -log10(serverData$comparisons[[group]]$padjThreshold), linetype = 2)+
            scale_color_manual(values = serverData$deCatColors) +
            theme(legend.title = element_blank()))
    ## Return ggplot if mode is not html            
    if(mode!='html'){
        return(gridExtra::grid.arrange(p1, p2, ncol=2))         
    }
    ## Continue to build plotly plot if mode is html
    p1<- p1 %>% ggplotly(.,tooltip='text') %>% toWebGL
    p2<- p2 %>% ggplotly(.,tooltip='text') %>% toWebGL

    subplot(p1 %>%
            style(showlegend = F), p2 %>%
            style(showlegend = T, traces = 1:5), nrows = 1, titleX = TRUE, titleY = TRUE, margin = 0.06) %>%
            highlight('plotly_selected') %>%
            layout(annotations = list(list(x = 0.2, y = 1.08, text = 'MAplot', showarrow = F,
            xref = 'paper', yref = 'paper'), list(x = 0.8, y = 1.08, text = 'Volcano plot',
            showarrow = F, xref = 'paper', yref = 'paper')))  
}

#' Draw Heatmap
#'
#' @param serverData Server data
#' @param group Comparison name
#' @param gentType 'Up', 'Down' or c('Up', 'Down')
#' @param gentName Vector of gene names
#' @return 
#' @noRd
#' @examples
#'  \donttest{
#'}
drawHeatmap <- function(serverData, group, geneType=c('Up', 'Down'), geneName=NULL, show_rownames=FALSE) {
    treatment = strsplit(group, "_vs_")[[1]][1]
    control = strsplit(group, "_vs_")[[1]][2]
    pd <- assay(serverData$vsd)%>%as.data.frame
    # geneID    baseMean log2FoldChange     lfcSE       stat       pvalue   padj       type
    if(!is.null(geneType)) pd <- pd[serverData$comparisons[[group]]$DE$geneID[serverData$comparisons[[group]]$DE$type %in% geneType],]
    if(!is.null(geneName)) pd <- pd[serverData$comparisons[[group]]$DE$geneID[serverData$comparisons[[group]]$DE$geneID %in% geneName],]
    pd <- t(scale(t(pd)))
    annoCol <- serverData$pca[,c('name','group')] %>%
    column_to_rownames('name')
    selectedSamples <- (rownames(annoCol))[annoCol$group %in% c(treatment,control)]
    pheatmap::pheatmap(pd[, selectedSamples], scale = 'none', annotation_col = annoCol, show_rownames = show_rownames) 
}

#' Draw Sample Correlation
#'
#' @param serverData Server data
#' @return 
#' @noRd
#' @examples
#'  \donttest{
#'}
drawSampleCorrelation <- function(serverData) {
    pd <-cor(assay(serverData$vsd))
    heatmaply::heatmaply(
        pd %>% round(3),
        label_names = c("row", "column", "correlation")
    ) %>%
    htmlwidgets::onRender("
    function(el) { 
        el.on('plotly_click', function(d) { 
        // Parsing data from plotly click
        // Setting the [row,column,correlation] value
        // as a shiny object, this is a simple array and names
        // are not sent
        // row: GSM4472578<br>column: GSM4472578<br>correlation: 1.000
        var out = d['points'][0]['text']
        out = out.replace('row: ', '').replace('column: ', '').replace('correlation: ', '')
        var out_arr = out.split('<br>')
        Shiny.setInputValue('Sample-correlation_on_click', out_arr);
        });
    }
    ")
}

#' Draw PCA
#'
#' @param serverData Server data
#' @return 
#' @noRd
#' @examples
#'  \donttest{
#'}
drawPCA <- function(serverData, mode='html') {
    ## If mode is not html build and return 2D PCA ggplot
    if(mode!='html'){
        colors <- RColorBrewer::brewer.pal(length(unique(serverData$pca$group)), 'Set1')
        tmpList <- list()
        tmpList[unique(serverData$pca$group)] <- colors
        expandedColors <- unlist(tmpList[serverData$pca$group], use.names=FALSE)
        serverData$pca$color <- expandedColors
        s3dPlot <- scatterplot3d::scatterplot3d(as.matrix(serverData$pca[,c('PC1', 'PC2', 'PC3')]), color=expandedColors, grid=TRUE, box=FALSE, pch=16)
        legend("topright", legend = unique(serverData$pca$group),
            col =  colors, pch=16)
    }else{
        ## IF mode is html continue to build 3D PCA
        fig <- plot_ly(serverData$pca, x = ~PC1, 
                    y = ~PC2, z = ~PC3, 
                    color = ~group, 
                    text = ~name,
                    marker = list(size=8)
                )# %>% add_markers(size = 20)
        fig
    }
}

#' Draw Sample Scatter
#'
#' @param serverData Server data
#' @param value1 Sample 1 ID
#' @param value2 Sample 2 ID
#' @noRd
#' @return 
#' @examples
#'  \donttest{
#'}
drawSampleScatter <- function(serverData, value1, value2) {
    x = value1
    y = value2
    pd <- assay(serverData$vsd)%>%as.data.frame
    p<- ggplot(pd %>% round(3)%>% rownames_to_column("gene")) + geom_point(size = 0.25, aes_string(x, y, label="gene")) +
    geom_abline(slope = 1, intercept = 0, color = "red") + theme_bw()
    p<- ggplotly(p,tooltip=c("label","x","y"))%>% toWebGL()
    p    
}

#' Draw Gene Expression
#'
#' @param serverData Server data
#' @param geneName gene name
#' @noRd
#' @return 
#' @examples
#'  \donttest{
#'}
drawGeneExpression <- function(serverData, geneName) {
    pd <- (assay(serverData$vsd))%>%as.data.frame
    pd[geneName, ] %>%
        tidyr::pivot_longer(everything(), names_to = "name", values_to = "normalizedCount") %>%
        dplyr::left_join(serverData$pca[, c("name", "group")]) %>%
        group_by(group) %>%
        e_charts() %>%
        e_boxplot(normalizedCount, outliers = TRUE) %>%
        e_y_axis(scale = TRUE) %>%
        e_axis_labels(y="normalizedCount", x="Group")    
}

#' Download item link
#'
#' @param text Text
#' @noRd
#' @return 
#' @examples
#'  \donttest{
#'}
downloadItem<- function (text, icon = shiny::icon("warning"), status = "success",
    href = NULL, inputId = NULL,itemCl="dropdown-item"){
  if (is.null(href))
      href <- "#"

  if (!is.null(inputId))
      itemCl <- paste0(itemCl, " shiny-download-link")
  if (!is.null(status)) {
      icon <- shiny::tagAppendAttributes(icon, class = paste0("text-",
          status))
  }
  shiny::tags$a(
    class = itemCl, 
    disabled = if (is.null(inputId))
      NA
    else NULL, 
    href = if (is.null(inputId)) {
      "#"
    }else {
      href
    },
    target = if (is.null(inputId)) {
      if (!is.null(href))
          "_blank"
    }, 
    id = inputId, 
    shiny::tagAppendAttributes(icon, class = "mr-2"),
      text
    )
}

#' Get gene List
#'
#' @param df Dataframe
#' @noRd
#' @return 
#' @examples
#'  \donttest{
#'}
getGList <- function(df, deGroup=NULL ){
    df = df %>%
        mutate(geneID = gsub("\\..*", "", geneID)) %>%
        arrange(-log2FoldChange) 
    
    if(!is.null(deGroup)){df= df %>%dplyr::filter(type==deGroup)}
    
    glist <- df$log2FoldChange
    names(glist) <- df$geneID
    return(glist)
}

#' Get species name from Transcripts
#'
#' @param filePath Path to gene file
#' @param header boolean to indicate file header
#' @noRd
#' @return 
#' @examples
#'  \donttest{
#'}
getGenomeFromTranscriptFile <- function(filePath, header=TRUE){
    species = "NA"
    tryCatch({
        genes <- read.table(file = filePath, header = as.logical(header),nrow=1, row.names=NULL)
        if(grepl("^ENSMUSG",genes[1,1])){species="mm10"}
        else if(grepl("^ENSG",genes[1,1])){species="hg19"}
        else if(grepl("^ENSR",genes[1,1])){species="rn6"}
        else{species="NA";stop()}
        }, 
        interrupt = function(x){print(x)},
        error = function(x){print(x)}
        )
    return(species)
}

#' Run DeSEQ from uploaded data
#'
#' @param data serverData object
#' @param parentSession parentSession
#' @noRd
#' @return 
#' @examples
#'  \donttest{
#'}
runDESeqFromUpload <- function(data, parentSession){
        withProgress(
            message = "processing uploaded data",
            value = 0,{    
            incProgress(0.1, detail = "creating dds...")

            data$counts=data$counts[,sort(names(data$counts))]
            # Adding rownames
            data$samples <- data$samples[order(data$samples$name), ]
            
            # create dds
            rownames(data$samples) <- NULL
            data$dds <- DESeqDataSetFromMatrix(
                countData = round(data$counts),
                colData = data$samples %>% column_to_rownames("name"),
                design = ~1 + group)
            incProgress(0.3, detail = "calculating normalized counts ...")                            
            data$vsd <- vst(data$dds, blind=FALSE)
            incProgress(0.5, detail = "calculating pca  ...")                                                    
            data$pca <- prcomp(assay(data$vsd)%>%
                t, scale. = T, center = T)$x %>%
                as.data.frame %>%
                round(2) %>%
                rownames_to_column("name") %>%
                left_join(data$samples)
            incProgress(0.7, detail = "preparing annotation db  ...")
            prepareAnnotationDb(data)                            
            incProgress(0.9, detail = "adding to report ...")
            data$rmd <- updateRmd(session=parentSession, boxId='sample-header', serverData=data)
            data$rmd <- updateRmd(session=parentSession, boxId='sample-table', serverData=data)
            data$rmd <- updateRmd(session=parentSession, boxId='pca', serverData=data)
            data$rmd <- updateRmd(session=parentSession, boxId='sample-correlation', serverData=data)
            setProgress(1)
            # update main tab 
            updateTabItems(session=parentSession, inputId= "mainTabs", "Sample")
            }
        )
}

#' Get prepare sample sheet modal
#'
#' @param samplesFromQuant Data Frame prepared by rnaApp:::prepareCountsFromQuants
#' @noRd
#' @return 
#' @examples
#'  \donttest{
#'}
createSampleSheetModal <- function(data, session, module="Upload"){
    # name genome group
    ns <- session$ns
    samplesFromQuant <- data$samples %>% as.data.frame
    clusterNames <- c()              
        sampleLabelTable <- tags$table(
        class="table table-bordered table-hover",
        style="margin-left:auto; margin-right:auto; width:50%; table-layout: fixed;",
        tags$thead(
            tags$tr(
                tags$th(scope="col", style="width:12%;", "Name"),
                tags$th(scope="col", style="width:12%", "Genome"),
                tags$th(scope="col", style="width:12%", "Group")
            ),
        ),
        tags$tbody(
            apply(samplesFromQuant, 1, function(x){
                    tags$tr(
                        tags$td(x['name']),
                        tags$td(x['genome']),
                        tags$td(
                        tags$input(
                        type="text",
                        style="width:100%; height: 100%;",
                        name=paste0(paste0(module, '-inputGroup-'), x['name']),
                        value=x['group']
                        )
                    )
                    )
                })%>%tagList
        )
    )

    showModal(
        modalDialog(
            title = "Prepare Sample Sheet",
            easyClose = TRUE,
            size="l",
            sampleLabelTable,
            footer=tagList(
                modalButton("Cancel"),
                actionButton(ns("saveSampleSheet"),"Save",status = "secondary",class = "float-right")
                )
        )
    )
}

#' Save sample groups from modal
#'
#' @param data serverData object
#' @param session session
#' @param input input
#' @noRd
#' @return 
#' @examples
#'  \donttest{
#'}
saveSampleGroups <- function(data, session, input){
    ns <- session$ns
    removeModal()
    for(i in seq(nrow(data$samples))){
        data$samples$group[i] <- input[[paste0('inputGroup-', data$samples$name[i])]]
        print(paste0(data$samples$group[i], "-->", data$samples$name[i]))
    }

    rownames(data$samples) <- NULL
}

#' Prepare Counts from DRAGEN Gene Quants files
#'
#' @param quantDataFileMatrix fileInput Data Frame
#' @param data (optional) serverData object
#' @noRd
#' @return 
#' @examples
#'  \donttest{
#'}
# prepareCountsFromQuants <- function(quantDataFileMatrix, data=NULL){
#     sampleNames <- stringr::str_replace(quantDataFileMatrix$name, ".quant.genes.sf", "")
#     sampleGenome <- rnaApp:::getGenomeFromTranscriptFile(quantDataFileMatrix$datapath[1])
#     dbObject <- prepareAnnotationDb(list(genome=sampleGenome))
#     i=1
#     countsDf <- data.frame()
#     for (quantFile in quantDataFileMatrix$datapath){
#         tmpDf <- read.table(file = quantFile, header = TRUE, sep = '\t')
#         tmpDf$GeneSymbol <- tmpDf$Name
#         tmpDf <- tmpDf %>% select(GeneSymbol, NumReads) %>% mutate(NumReads = round(NumReads))
#         colnames(tmpDf) <- c("GeneSymbol", sampleNames[i])
#         if(i==1){
#             countsDf <- tmpDf
#         }else{
#             countsDf <- left_join(countsDf, tmpDf, by="GeneSymbol")
#         }
#         i=i+1
#     }
#     countsDf$GeneSymbol <- as.vector(AnnotationDbi::mapIds(dbObject, keys=gsub("\\.[0-9]*", "", countsDf$GeneSymbol), column="SYMBOL", keytype="ENSEMBL", multiVals="first"))
#     countsDf <- countsDf %>% filter(!is.na(GeneSymbol))
#     print(countsDf)
#     countsDf <- countsDf %>% group_by(GeneSymbol) %>% summarise_if(is.numeric, max, na.rm=TRUE) %>% as.data.frame
#     rownames(countsDf) <- countsDf$GeneSymbol
#     countsDf <- countsDf %>% select(-GeneSymbol)
#     countsDf <- countsDf[rowSums(countsDf[])>0,]
#     if(!is.null(data)){
#         data$samples <- data.frame(
#             name=sampleNames,
#             genome=rep(sampleGenome, nrow(quantDataFileMatrix)),
#             group=rep("", nrow(quantDataFileMatrix))
#         )
#         data$samplesFromQuant <- TRUE
#         data$genome <- sampleGenome
#     }
#     return(countsDf)
# }

#' Prepare resDE summary
#'
#' @noRd
#' @export
#' @examples
#'  \donttest{
#'}
prepare_resDE_summary = function(resDE, padjThreshold, foldChange, mean_count) {
    ## Given the resDE df the this function 
    ## prepares a string to the mimic the summary 
    ## function of DESeq2
    ## Find out: mean count
    ## Reference: https://rdrr.io/bioc/DESeq2/src/R/methods.R

    counts = table(resDE$type)
    total = nrow(resDE)
    notallzero = sum(resDE$baseMean > 0)
    up = ifelse("Up" %in% names(counts), counts[["Up"]], 0)
    down = ifelse("Down" %in% names(counts), counts[["Down"]], 0)
    outliers = ifelse("Outlier" %in% names(counts), counts[["Outlier"]], 0)
    low_counts = ifelse("Low counts" %in% names(counts), counts[["Low counts"]], 0)
    summary_str = ""
    summary_str = paste0(summary_str, "Out of ", notallzero, " with nonzero total read count \n")
    summary_str = paste0(summary_str, "adjusted p-value < ", padjThreshold, " \n")
    summary_str = paste0(summary_str, "Fold change > ", foldChange, " (up)       :", up, ", ", round(up*100/total, 1), "% \n")
    summary_str = paste0(summary_str, "Fold change < -", foldChange, " (down)     :", down, ", ", round(down*100/total, 1), "% \n")
    summary_str = paste0(summary_str, "outliers [1]       :", outliers, ", ", round(outliers*100/total, 1), "% \n")
    summary_str = paste0(summary_str, "low counts [2]     :", low_counts, ", ", round(low_counts*100/total, 1), "% \n")
    summary_str = paste0(summary_str, "(mean count < ", mean_count,") \n")
    summary_str = paste0(summary_str, "[1] see 'cooksCutoff' argument of ?results \n")
    summary_str = paste0(summary_str, "[1] see 'independentFiltering' argument of ?results \n")
    return(summary_str)
}


#' Prepare resDE
#'
#' @noRd
#' @export
#' @examples
#'  \donttest{
#'}
prepare_resDE = function(resDE, padjThreshold, fcThreshold) {
    # Separatin geneID column to row operation for refilter usage
    if(!"geneID" %in% colnames(resDE%>%as.data.frame)){
        resDE = resDE %>% as.data.frame %>%
                rownames_to_column("geneID")
    }
    if("type" %in% colnames(resDE%>%as.data.frame)){
        resDE = resDE %>% as.data.frame %>%
                dplyr::select(-type)
    }    
    resDE.df = resDE %>%
        as.data.frame %>%
        mutate(
            notallzero = baseMean > 0, 
            up = padj < padjThreshold & log2FoldChange > log2(fcThreshold), 
            down = padj < padjThreshold & log2FoldChange < - log2(fcThreshold), 
            outlier = baseMean > 0 & is.na(pvalue), 
            filt = !is.na(pvalue) & (is.na(padj) | padj==1)) %>%
        mutate(type = ifelse(up %>%
            tidyr::replace_na(F), "Up", ifelse(down %>%
            tidyr::replace_na(F), "Down", ifelse(outlier %>%
            tidyr::replace_na(F), "Outlier", ifelse(filt %>%
            tidyr::replace_na(F), "Low counts", "nonDE"))))) %>%
            dplyr::select(-up, -down, -outlier, -filt, -notallzero)%>%
        mutate_at("type", as.factor) %>% 
        mutate(padj = ifelse(is.na(padj), 1, padj))
    return(resDE.df)
}

# Add dependencies to a tag object
#' add js dependency
#'
#' @return A jwt token string.
#' @noRd
#'
#' @examples
#'  \donttest{
#'}

addDeps<- function(){
    # put all necessary ressources here
    # ref: https://shiny.rstudio.com/articles/packaging-javascript.html#javascript-file-under-www
    # incl: https://book.javascript-for-r.com/shiny-cookies.html#shiny-cookies-r-code

    tagList(
        includeScript(system.file("customizedJs/icaScript.js", package = "rnaApp")),
        tags$script(
            src = paste0(
            "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
            "dist/js.cookie.min.js"
            )
        )
    )
}

#' Helper function to create a modal with title and description
#'
#' @param modal_title Title of the popup window.
#' @param description Content in the popup window.
#'
#' @export
#'
#' @examples
#'  \donttest{
#'}

getModal = function(modal_title, description, size="l"){
    return(
                shiny::showModal(modalDialog(
                    title = modal_title,
                    HTML(description),
                    size = size,
                    easyClose = TRUE,
                    footer = NULL
                ))        
    )
}