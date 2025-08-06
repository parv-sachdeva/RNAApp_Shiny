# Add UI component of app using bs4Dash
ui = dashboardPage(
  title ="RNA Analysis App",
  fullscreen = TRUE,
  help = FALSE,
  dark = NULL, 
  scrollToTop = TRUE,
  header = headerUI("Header"),
  preloader = list(html = tagList(waiter::spin_1(), "Loading ..."), color = "#3c8dbc"),
  sidebar = dashboardSidebar(
    status = "primary",
    elevation = 3,
    disable = FALSE,
    fixed = TRUE,
    minified=TRUE,
    collapsed = TRUE,
    sidebarMenu(
      id = "sidebarTabs",
      menuItem(
        "Data Upload",
        tabName = "Upload",
        icon = icon("upload")
      ),
      menuItem(
        "Review Samples",
        tabName = "Sample",
        icon = icon("vials")
      ),
      menuItem(
        "View Gene Expression",
        tabName = "Gene",
        icon = icon("dna")
      ),
      menuItem(
        "Differential Expression",
        tabName = "DE",
        icon = icon("balance-scale-right")
      ),
      bookmarkUI("Bookmark")
    )
  ),
  body = dashboardBody(
    tabItems(
      uploadUI("Upload"),
      sampleUI("Sample"),
      geneUI("Gene"),
      degUI("DE")
    )
  ),
  footer = footerUI()
)