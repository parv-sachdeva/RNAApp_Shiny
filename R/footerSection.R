# Foot UI for the shiny app
footerUI<- function(){
  appVersion = "0.0.1"
  bs4Dash::dashboardFooter(
    left = tooltip(
            title  = "Contact the author!",
            div(
            shiny::tags$a(
            href = "https://www.linkedin.com/in/parv-sachdeva/",
            shiny::tags$span(paste0("Version: ", appVersion)),
            target = "_blank",
            style = "color:#4E60EE; border-bottom:1px solid #0000EE;"
            )
            )
          ),
    right = "Parv Sachdeva",
    fixed = T
  )}