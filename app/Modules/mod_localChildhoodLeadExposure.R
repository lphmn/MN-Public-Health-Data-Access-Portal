
source("./Data/df_localChildhoodLeadExposure.R")


mod_localChildhoodLeadExposureUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(
      column(
        width = 4,
        selectInput(
          ns("parLocal_leadIndicator"),
          label= "Select Indicator",
          choices= sort(unique(df_leadRaw$indicator)),
          selected= df_leadRaw$indicator[1],
          multiple= FALSE
        )
      ),
      column(
        width = 4,
        selectInput(
          ns("parLocal_leadIndicatorType"),
          label= "Select Indicator Type",
          choices= sort(unique(df_leadRaw$indicator_type)),
          selected= NULL,
          multiple= FALSE
        )
      ),
      column(
        width = 4,
        selectInput(
          ns("parLocal_leadYear"),
          label= "Select Year",
          choices= NULL,
          selected= NULL,
          multiple= FALSE
        )
      )
    ),
    fluidRow(
        # Narrative section explaining the purpose of the dashboard
        column(
          width = 12,
          id = "cID_leadNarrative",
          tags$h4(htmlOutput("lead_narrative")),
          tags$h3("For information about why this data is important please visit ",
                  tags$a(href="https://data.web.health.state.mn.us/lead", "here!", target= "_blank")
          ),
          tags$h3("Data for this project can be found ",
                  tags$a(href="https://data.web.health.state.mn.us/lead_query", "here!", target= "_blank")
          ),
          tags$h3("Data for this project is stored ",
                  tags$a(href="https://raw.githubusercontent.com/lphmn/data_publicState/refs/heads/main/MN_Regions/SchsacRegionChbCounty.csv",
                         "here!", target= "_blank")
          )
        )
      )
  )
}

mod_localChildhoodLeadExposureServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
      
    observeEvent(input$parLocal_leadIndicatorType, {
      if(input$parLocal_leadIndicatorType %in% "Birth year (cohort method)") {
        updateSelectInput(
          session, "parLocal_leadYear",
          choices =  sort(unique(df_leadRaw$year[input$parLocal_leadIndicatorType== df_leadRaw$indicator_type]), decreasing = TRUE),
          selected = sort(unique(df_leadRaw$year[input$parLocal_leadIndicatorType== df_leadRaw$indicator_type]), decreasing = TRUE)[1]
        )
      }
      else {
        updateSelectInput(
          session, "parLocal_leadYear",
          choices =  sort(unique(df_leadRaw$year[input$parLocal_leadIndicatorType== df_leadRaw$indicator_type]), decreasing = TRUE),
          selected = sort(unique(df_leadRaw$year[input$parLocal_leadIndicatorType== df_leadRaw$indicator_type]), decreasing = TRUE)[1]
        )
      }
    })
    }
  )
}



# Below is how I can test the application
# Standard Shiny UI
#  library(shiny)
#  ui <- fluidPage(titlePanel("Test"),sidebarLayout(sidebarPanel(mod_localChildhoodLeadExposureUI("x")),mainPanel()))
# # Main App Server
#  server <- function(input, output, session) {mod_localChildhoodLeadExposureServer("x")}
#  shinyApp(ui, server)
