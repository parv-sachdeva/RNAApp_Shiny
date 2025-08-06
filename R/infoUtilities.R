# Get rnaApp Upload Info Modal
getRnaAppUploadInfoModal <- function(data, gif="upload"){
    shiny::showModal(
        modalDialog(
            title = "Upload Module Help",
            h3("Upload Data"),
            br(),
            HTML(data$descriptions$count_data_info),
            br(),
            HTML(data$descriptions$meta_data_info),
            br(),
            size = "xl",
            easyClose = TRUE
        )
    )
}

# Get rnaApp Sample Info Modal
getRnaAppSampleInfoModal <- function(data, gif="vials"){
    shiny::showModal(
        modalDialog(
            title = "Sample Explorer Help",
            h3("Samples"),
            br(),
            "Explore samples using the following tools:",
            br(),
            br(),
            h5("PCA"),
            HTML(data$descriptions$sample_pca),
            br(),
            br(),
            h5("Sample Correlation Matrix"),
            HTML(data$descriptions$sample_correlation_matrix),
            br(),
            br(),
            h5("Sample Scatter"),
            HTML(data$descriptions$sample_scatter),
            br(),
            br(),
            size = "xl",
            easyClose = TRUE
        )
    )
}


# Get rnaApp DEG Info Modal
getRnaAppDEGInfoModal <- function(data, gif="balance-scale-right"){
    shiny::showModal(
        modalDialog(
            title = "Differential Expression Help",
            h3("Differential Expression"),
            br(),
            HTML(data$descriptions$de_select_design),
            br(),
            br(),
            h5("Plots"),
            HTML(data$descriptions$de_plots),
            br(),
            br(),
            h5("Results"),
            HTML(data$descriptions$de_result_table),
            br(),
            br(),
            h5("Heatmap"),
            HTML(data$descriptions$de_heatmap),
            br(),
            br(),
            size = "xl",
            easyClose = TRUE
        )
    )
}

# Get rnaApp Gene Info Modal
getRnaAppGeneInfoModal <- function(data, gif="dna"){
    shiny::showModal(
        modalDialog(
            title = "Gene Exporer Help",
            h3("Genes"),
            br(),
            HTML(data$descriptions$de_select_design),
            br(),
            br(),
            size = "xl",
            easyClose = TRUE
        )
    )
}