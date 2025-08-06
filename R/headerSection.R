# Header nav UI for the shiny app
headerUI<- function(id){
  ns = NS(id)
  bs4Dash::dashboardHeader(
    title = NULL,
    disable =FALSE,
    skin = "dark",
    # border = TRUE,
    fixed = TRUE,
    rightUi=  tagList(
      uiOutput(ns("LinkedinLink"))
      # uiOutput(ns("GithubLink"))
    )
  )
}

# Header nav server for the shiny app

headerServer <- function(id, data){
  moduleServer(
    id,
    function(input,output,session){
      ns <- session$ns
      # linkedin link
      output$LinkedinLink <- renderUI({
        tooltip(
          title  = "Open LinkedIn profile in a new tab",
          shiny::tags$a(
            href = "https://www.linkedin.com/in/parv-sachdeva/",
            icon("linkedin"),
            target="_blank",
            class = "nav-link"
          )
        )
	    })
      # github link
      output$GithubLink <- renderUI({
        tooltip(
          title  = "Open GitHub profile in a new tab",
          shiny::tags$a(
            href = "https://github.com/parv-sachdeva",
            icon("github"),
            target="_blank",
            class = "nav-link"
          )
        )
	    })
    }
  )
}
