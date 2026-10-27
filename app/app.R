# Shiny Dashboard
# Hastag (# ---------------------------------------------------------------) means Primary Header
# Hastag (### *** Definition ###) means Secondary Header
# Hold shift alt o to collapse all
# Hold alt o to expand all

# Load Packages ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyjs)
#library(dplyr)
#Loading shinyjs really slowed down my application load time. By loading just the functions I required such as shnyjs::useShinyjs(), my application ran way faster
#library(shinyjs)



# Load Data ---------------------------------------------------------------

# State Community Health Services Advisory Committee as of 1_17_2024
df_schsacRaw <- read.csv("https://raw.githubusercontent.com/lphmn/data_publicState/refs/heads/main/MN_Regions/SchsacRegionChbCounty.csv")
# Community Health Board as of 1_17_2024
df_chbRaw <- read.csv("https://raw.githubusercontent.com/lphmn/data_publicState/refs/heads/main/MN_Regions/SchsacRegionChbCounty.csv")

#Healthy Homes
#* Childhood Lead Exposure
df_leadRaw <-  read.csv("https://raw.githubusercontent.com/lphmn/data_publicState/refs/heads/main/MN_PHDAP/healthyHomes/childhoodLeadExposure.csv")
#Radon

# User Interface ----------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span("MN Public Health Data Access Portal", style = "color: #ffffff; font-weight: 700;"),
    titleWidth = 400
    #disable = TRUE #uncomment if the header should be hid
  ),
  dashboardSidebar(
    width = 350, #This makes the sidebar wider. However, the input boxes seem to have a set dimension resulting in long names still wrapping
    tags$div(class = "sr-only", "Main navigation and filters for the Minnesota Public Health Data Access Portal"),
    # The next items require
    #The input parameters are global parameters
    checkboxInput(
      inputId = "parGlobal_hideNarrative",
      label = tags$span("HIDE NARRATIVE", style = "font-weight: 600; color: #111111;"),
      value = FALSE
    ),
    # Other input elements...
    selectInput(
      "parGlobal_county",
      label = tags$span("Select County of Interest", style = "font-weight: 600; color: #111111;"),
      choices = sort(unique(df_schsacRaw$County)),
      selected = "Kittson",
      multiple = FALSE,
      width = 350
    ),
    selectInput(
      "parGlobal_region",
      label = tags$span("Select SCHSAC Region", style = "font-weight: 600; color: #111111;"),
      choices = NULL,
      selected = NULL,
      multiple = FALSE,
      width = 350
    ),
    selectInput(
      "parGlobal_chb",
      label = tags$span("Select Community Health Board", style = "font-weight: 600; color: #111111;"),
      choices = NULL,
      selected = NULL,
      multiple = FALSE,
      width = 350
    ),
    #Sidebar is required to have sub menus because it requires the tabName to reference
    #By having sidebarMenu id, you can reference it and hide other filters with shinyjs 
    # The CHB and Region Filters are greyed out when onthe Region & CHB Defintions
    sidebarMenu(
      id = "smID",
      menuItem("Home", tabName = "tn_homePage"),
      menuItem("Region & CHB Defintions", tabName = "tn_regionChbDefinations"),
      menuItem("Child Health", tabName = "tn_childHealth"),
      menuItem("Climate", tabName = "tb_climate"),
      menuItem("Diseases & Conditions", tabName = "tn_diseasesConditions"),
      menuItem("Environmental Health", tabName = "tn_environmentalHealth"),
      menuItem("Health Behaviors/Risk Factors", tabName = "tn_healthBehaviorsRiskFactors"),
      menuItem("Health Equity", tabName = "tn_HealthEquity"),
      menuItem("Healthy Homes", tabName = "tn_healthyHomes")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(" 
        :root {
          color-scheme: light;
        }
        body {
          background-color: #ffffff;
          color: #111111;
          line-height: 1.5;
        }
        .content-wrapper,
        .right-side,
        .main-sidebar {
          background-color: #f7f7f7;
        }
        .main-sidebar .sidebar-menu > li > a,
        .main-header .logo,
        .main-header .navbar,
        .main-header .navbar-static-top,
        .main-header .navbar-nav > li > a {
          color: #111111;
        }
        .main-header {
          background-color: #0d4f8b;
          border-bottom: 3px solid #083f6b;
        }
        .box,
        .box-body,
        .box-header {
          border: 1px solid #d0d0d0;
          color: #111111;
        }
        .nav-tabs > li > a,
        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:hover,
        .nav-tabs > li.active > a:focus {
          color: #111111;
        }
        .form-control,
        .selectize-input,
        .selectize-dropdown {
          border: 2px solid #4d4d4d;
          min-height: 38px;
          color: #111111;
          background-color: #ffffff;
        }
        .form-control:focus,
        .selectize-input:focus,
        .selectize-input.focus,
        .sidebar-menu > li > a:focus,
        .nav-tabs > li > a:focus,
        .btn:focus,
        input:focus,
        select:focus,
        button:focus,
        a:focus {
          outline: 3px solid #005fcc !important;
          outline-offset: 3px;
          box-shadow: 0 0 0 3px rgba(0, 95, 204, 0.25) !important;
        }
        .checkbox,
        .radio,
        label,
        .shiny-input-container {
          color: #111111;
        }
        h1, h2, h3, h4, h5, h6, p, li, td, th {
          color: #111111;
        }
        a {
          color: #0b57a6;
        }
        .sr-only {
          position: absolute;
          width: 1px;
          height: 1px;
          padding: 0;
          margin: -1px;
          overflow: hidden;
          clip: rect(0, 0, 0, 0);
          white-space: nowrap;
          border: 0;
        }
      ")),
      tags$script(HTML(" 
        $(document).ready(function() {
          $('.sidebar-menu a').attr('aria-label', function() {
            return $(this).text().trim();
          });
          $('.nav-tabs a').attr('aria-label', function() {
            return $(this).text().trim();
          });
        });
      "))
    ),
    shinyjs::useShinyjs(),
    fluidRow(
      column(12,
             tabItems(
               tabItem(
                 tabName = "tn_homePage", #tabName is what ties the menuItem to the tabItem
                 tabsetPanel(
                   id = "tpId_home",
                   tabPanel(
                     "Home Page",
                     fluidRow(
                       # Narrative section explaining the purpose of the dashboard
                       column(
                         width = 12,
                         tags$h1("Welcome to the Landing Page for This Awesome Dashboard...Subjective of Course :)"),
                         tags$h4("This shinyLive application is designed to minor the work represeted ",
                                 tags$a(href="https://data.web.health.state.mn.us/web/mndata/", "here!", target = "_blank", tabindex = "0")),

                         tags$h4("The features displayed on the left are called sidebar menus."),
                         tags$h3("Currently, only two sidebar menus are working"),
                         tags$h4("The Region & CHB Defintions sidebar menu has both tabs (Region and CHB) working"),
                         tags$h4("The Healthy Homes sidebar menu only has the Childhood Lead Exposure tab working"),
                         tags$h4("Limitations to the Childhood Lead Exposure data can be found ",
                                 tags$a(href="https://data.web.health.state.mn.us/lead_metadata", "here!", target = "_blank", tabindex = "0"))
                       )
                     )
                   )
                 )
               ),
               tabItem(
                 tabName = "tn_regionChbDefinations",
                 tabsetPanel(
                   id = "tpId_RegionChb",
                   tabPanel(
                     "Region",
                     fluidRow(
                       # Narrative section explaining the dashboard purpose
                       column(
                         width = 12,
                         h3(HTML("Updating the Select County of Interest filter, wll highlight the county in <font color=red>red</font> while the Regions will remain in <b>bold</b>.")),
                         h3("For this tab, the Select SCHSAC Region and Select Community Health Board filters are greyed out because they do not execute any function on this tab."),
                         h3("The purpose for this tab is to provide a quick reference for what counties fall under which region."),
                         #The next line inserts a line between the narrative and the data
                         tags$hr(style = "border-top: 2px solid #4d4d4d; margin-top: 20px; margin-bottom: 20px;")
                       )
                     ),
                     fluidRow(
                       uiOutput("region_narrative", style = "font-size: 20px;")
                     )
                   ),
                   tabPanel(
                     "CHB",
                     fluidRow(
                       # Narrative section explaining the purpose of the dashboard
                       column(
                         width = 12,
                         h3(HTML("Updating the Select County of Interest filter, wll highlight the county in <font color=red>red</font> while the Community Health Boards will remain in <b>bold</b>.")),
                         h3("For this tab, the Select SCHSAC Region and Select Community Health Board filters are greyed out because they don't have any functions on this page."),
                         h3("The purpose for this tab is to provide a quick reference for what counties fall under which Community Health Board."),
                         #The next line inserts a line between the narrative and the data
                         tags$hr(style = "border-top: 2px solid #4d4d4d; margin-top: 20px; margin-bottom: 20px;")
                       )
                     ),
                     fluidRow(
                       column(6, uiOutput("chb_narrative_01", style = "font-size: 20px;")),
                       column(6, uiOutput("chb_narrative_02", style = "font-size: 20px;"))
                     )
                   )
                 )
               ),
               tabItem(
                 tabName = "tn_childHealth",
                 tabsetPanel(
                   tabPanel("Asthma"),
                   tabPanel("Health Inequities in Childhood Asthma"),
                   tabPanel("Birth Defects"),
                   tabPanel("Childhood Lead Exposure"),
                   tabPanel("Free/Reduced Price Lunch Eligibility"),
                   tabPanel("Immunizations"),
                   tabPanel("Oral Health")
                 )
               ),
               tabItem(
                 tabName = "tb_climate",
                 tabsetPanel(
                   tabPanel("Air Quality"),
                   tabPanel("Cold-related Illness"),
                   tabPanel("tbClimate-related Environmental Health Concerns"),
                   tabPanel("Heat-related Illness"),
                   tabPanel("Hot Weather"),
                   tabPanel("Pollen")
                 )
               ),
               tabItem(
                 tabName = "tn_diseasesConditions",
                 tabsetPanel(
                   tabPanel("Asthma"),
                   tabPanel("Birth Defects"),
                   tabPanel("Cancer"),
                   tabPanel("Carbon Monoxide (CO) Poisoning"),
                   tabPanel("Chronic Obstructive Pulmonary Disease (COPD)"),
                   tabPanel("Diabetes"),
                   tabPanel("Heart Attacks"),
                   tabPanel("Heat-related Illness"),
                   tabPanel("Immunizations"),
                   tabPanel("Oral Health")
                 )
               ),
               tabItem(
                 tabName = "tn_environmentalHealth",
                 tabsetPanel(
                   tabPanel("Air Quality"),
                   tabPanel("Biomonitoring: Chemicals in people"),
                   tabPanel("Cold-related Illness"),
                   tabPanel("Drinking Water Quality"),
                   tabPanel("Environmental Justice"),
                   tabPanel("Heat-related Illness"),
                   tabPanel("Pesticide Poisoning"),
                   tabPanel("Traffic")
                 )
               ),
               tabItem(
                 tabName = "tn_healthBehaviorsRiskFactors",
                 tabsetPanel(
                   tabPanel("Health Insurance"),
                   tabPanel("Immunizations"),
                   tabPanel("Obesity"),
                   tabPanel("Oral Health"),
                   tabPanel("Poverty & Income"),
                   tabPanel("Smoking")
                 )
               ),
               tabItem(
                 tabName = "tn_HealthEquity",
                 tabsetPanel(
                   tabPanel("Health Equity"),
                   tabPanel("Health Inequities in Childhood Lead Exposure"),
                   tabPanel("Health Inequities in Childhood Asthma")
                 )
               ),
               tabItem(
                 tabName = "tn_healthyHomes",
                 tabsetPanel(
                   id = "tpId_healthyHomes",
                   tabPanel("Carbon Monoxide (CO) Poisoning"),
                   tabPanel("Childhood Lead Exposure",
                            fluidRow(
                              # Narrative section explaining the purpose of the dashboard
                              column(
                                width = 1,
                                selectInput(
                                  "parLocal_leadYear",
                                  label = tags$span("Select Year", style = "font-weight: 600; color: #111111;"),
                                  choices = sort(unique(df_leadRaw$year), decreasing = TRUE),
                                  selected = max(unique(df_leadRaw$year)),
                                  multiple = FALSE
                                ),
                                selectInput(
                                  "par_leadStateRegionChb",
                                  label = tags$span("Select Comparison", style = "font-weight: 600; color: #111111;"),
                                  choices = c("All", "CHB", "Region", "State"),
                                  selected = "All",
                                  multiple = FALSE
                                )
                              ),
                              # Narrative section explaining the purpose of the dashboard
                              column(
                                width = 11,
                                id = "cID_leadNarrative",
                                tags$h4(htmlOutput("lead_narrative")),
                                tags$h3("For information about why this data is important please visit ",
                                        tags$a(href = "https://data.web.health.state.mn.us/lead", "here!", target = "_blank", tabindex = "0")),
                                tags$h3("Data for this project can be found ",
                                        tags$a(href = "https://data.web.health.state.mn.us/lead_query", "here!", target = "_blank", tabindex = "0")),
                                tags$h3("Data for this project is stored ",
                                        tags$a(href = "https://raw.githubusercontent.com/quinCHB/Public-Data-Sources/main/MN%20Public%20Health%20Data%20Access%20Portal/Healthy%20Homes/Childhood%20Lead%20Exposure.csv",
                                               "here!", target = "_blank", tabindex = "0"))
                              )
                            ),
                            fluidRow(
                              #The next line inserts a line between the narrative and the data.
                              #It is added here instead of in the narrative since the narrative can have multiple columns.
                              #If there are multiple narrative columns and the line is added there than the line consists of multiple breaks
                              tags$hr(style = "border-top: 2px solid #4d4d4d; margin-top: 20px; margin-bottom: 20px;"),
                              box(plotOutput("lead_state")),
                              box(plotOutput("lead_region"))
                            ),
                            fluidRow(
                              box(plotOutput("lead_chb")),
                              box(plotOutput("lead_county"))
                            )
                   ),
                   tabPanel("Drinking Water Quality"),
                   tabPanel("Pesticide Poisoning"),
                   tabPanel("Radon")
                 )
               )
             )
      )
    )
  )
)

# Preload Data prior to Server -----------------------------
#To improve performance load these once and don't have them run every time the server runs


#* Region & County Definitions ---------------------------------------------
# Create region data frame for global narrative reference
schsac_raw <- df_schsacRaw
schsac_raw$Region <-  paste("<b>", schsac_raw$Region, "</b>") #Bold Regions so it is easier to understand narrative

# Create chb data frame for global narrative reference
# CHB Narrative 01 (It is split in half so it displays in two nice columns on the UI
chb_raw_01 <- df_chbRaw[1:33,]
chb_raw_01$CHB <-  paste("<b>", chb_raw_01$CHB, "</b>") #Bold CHBs so it is easier to understand narrative

# Create chb data frame for global narrative reference
# CHB Narrative 02 (It is split in half so it displays in two nice columns on the UI
chb_raw_02 <- df_chbRaw[34:nrow(df_chbRaw),] # :nrow means it goes to the end of the data frame
chb_raw_02$CHB <-  paste("<b>", chb_raw_02$CHB, "</b>") #Bold CHBs so it is easier to understand narrative

#* Healthy Homes -----------------------------

#Childhood Lead Exposure
#Region
# Combine Region
# A lot of sources online say to use all= TRUE after the by condition for an inner. 
# This does not appear to be correct. It seems like it is executing an outer join not an inner join
lead_Region <- merge(x= df_leadRaw, y= df_schsacRaw, by.x= "location", by.y= "County") #, all = TRUE) Don't include the all = TRUE 

# #https://www.youtube.com/watch?v=zmiC7X9fUmo
# First sum number tested
# Next sum denominator
#Combine in a completed data set

#Sum will be applied to numTested and it will (~) be subsetted by everything after
lead_RegionGrpTested <-  aggregate(numTested~ ageGroup+ Region+ year+ indicator+ indicator.type+ ebllDescription, lead_Region, FUN=sum)
#Rename grouped field
colnames(lead_RegionGrpTested)[colnames(lead_RegionGrpTested) == 'numTested'] <- 'regionNumTested'

lead_RegionGrpDenominator <-  aggregate(denominator~ ageGroup+ Region+ year+ indicator+ indicator.type+ ebllDescription, lead_Region, FUN=sum)
#Rename grouped field
colnames(lead_RegionGrpDenominator)[colnames(lead_RegionGrpDenominator) == 'denominator'] <- 'regionDenominator'

lead_RegionWithTestGrp <- merge(x= lead_Region, 
                                y= lead_RegionGrpTested, 
                                by= c("ageGroup", "Region", "year", "indicator", "indicator.type", "ebllDescription"), 
                                all.x = TRUE) #Left join so counties may be displayed moving forward
lead_RegionComplete <- merge(x= lead_RegionWithTestGrp, 
                             y= lead_RegionGrpDenominator, 
                             by= c("ageGroup", "Region", "year", "indicator", "indicator.type", "ebllDescription"), 
                             all.x = TRUE) #Left join so counties may be displayed moving forward
lead_RegionComplete$regionTestPct <- round(lead_RegionComplete$regionNumTested/lead_RegionComplete$regionDenominator*100,2)

# Combine CHB
# This does not appear to be correct when including the all = TRUE. It seems like it is executing an outer join not an inner join
lead_CHB <- merge(x= df_leadRaw, y= df_chbRaw, by.x= "location", by.y= "County") #, all = TRUE) Don't include the all = TRUE

# #https://www.youtube.com/watch?v=zmiC7X9fUmo
# First sum number tested
# Next sum denominator
#Combine in a completed data set

#Sum will be applied to numTested and it will (~) be subset by everything after
lead_CHBGrpTested <-  aggregate(numTested~ ageGroup+ CHB+ year+ indicator+ indicator.type+ ebllDescription, lead_CHB, FUN=sum)
#Rename grouped field
colnames(lead_CHBGrpTested)[colnames(lead_CHBGrpTested) == 'numTested'] <- 'chbNumTested'

lead_CHBGrpDenominator <-  aggregate(denominator~ ageGroup+ CHB+ year+ indicator+ indicator.type+ ebllDescription, lead_CHB, FUN=sum)
#Rename grouped field
colnames(lead_CHBGrpDenominator)[colnames(lead_CHBGrpDenominator) == 'denominator'] <- 'chbDenominator'

lead_CHBWithTestGrp <- merge(x= lead_CHB, 
                             y= lead_CHBGrpTested, 
                             by= c("ageGroup", "CHB", "year", "indicator", "indicator.type", "ebllDescription"), 
                             all.x = TRUE) #Left join so counties may be displayed moving forward

lead_CHBComplete <- merge(x= lead_CHBWithTestGrp, 
                          y= lead_CHBGrpDenominator, 
                          by= c("ageGroup", "CHB", "year", "indicator", "indicator.type", "ebllDescription"), 
                          all.x = TRUE) #Left join so counties may be displayed moving forward

lead_CHBComplete$CHBTestPct <- round(lead_CHBComplete$chbNumTested/lead_CHBComplete$chbDenominator*100,2)



server <- function(input, output, session) {
  

# Region & County ---------------------------------------------------------

  output$region_narrative <- renderUI({
    # Replace the values that are equal to input county by adding the font tag
    schsac_raw$County[schsac_raw$County == input$parGlobal_county] <-  paste("<font color=red>", schsac_raw$County[schsac_raw$County == input$parGlobal_county], "</font>")
    
    # Split the data frame by the group column
    schsac_split <- split(schsac_raw , schsac_raw$Region)
    
    # Apply a function to each subset to create a new column with comma-separated values
    schsac_character <- sapply(schsac_split, function(x) paste(x$County, collapse = ", "))
    
    # Convert the result to a data frame
    schsac_df <- as.data.frame(schsac_character)
    
    # Create an empty character vector to store the results
    schsac_result <- character()
    
    # Loop through each row of schsac_df
    for (i in 1:nrow(schsac_df)) {
      # Get the row name and schsac_character_placeholder as strings
      rowname <- as.character(rownames(schsac_df)[i])
      schsac_character_placeholder <- as.character(schsac_df$schsac_character[i])
      
      # Concatenate them with a separator
      concat <- paste0(rowname, ":: ", schsac_character_placeholder)
      
      # Append the result to the vector
      schsac_result <- c(schsac_result, concat)
    }
    
    # Paste the result vector and create a new line after each Region
    HTML(paste(schsac_result, collapse=  "<br/>"))
    
  })
  
  output$chb_narrative_01 <- renderUI({
    # Replace the values that are equal to input county by adding the font tab
    chb_raw_01$County[chb_raw_01$County == input$parGlobal_county] <-  paste("<font color=red>", chb_raw_01$County[chb_raw_01$County == input$parGlobal_county], "</font>")
    
    # Split the data frame by the chb
    chb_split <- split(chb_raw_01 , chb_raw_01$CHB)
    
    # Apply a function to each subset to create a new column with comma-separated values
    chb_character <- sapply(chb_split, function(x) paste(x$County, collapse = ", "))
    
    # Convert the result to a data frame
    chb_df <- as.data.frame(chb_character)
    
    # Create an empty character vector to store the results
    chb_result <- character()
    
    # Loop through each row of chb_df
    for (i in 1:nrow(chb_df)) {
      # Get the row name and chb_character_placeholder as strings
      rowname <- as.character(rownames(chb_df)[i])
      chb_character_placeholder <- as.character(chb_df$chb_character[i])
      
      # Concatenate them with a separator
      concat <- paste0(rowname, ":: ", chb_character_placeholder)
      
      # Append the result to the vector
      chb_result <- c(chb_result, concat)
    }
    
    # Paste the result vector and create a new line after each CHB
    HTML(paste(chb_result, collapse =  "<br/>"))
  })
  
  output$chb_narrative_02 <- renderUI({
    # Replace the values that are equal to input county by adding the font tab
    chb_raw_02$County[chb_raw_02$County == input$parGlobal_county] <-  paste("<font color=red>", chb_raw_02$County[chb_raw_02$County == input$parGlobal_county], "</font>")
    
    # Split the data frame by the chb
    chb_split <- split(chb_raw_02 , chb_raw_02$CHB)
    
    # Apply a function to each subset to create a new column with comma-separated values
    chb_character <- sapply(chb_split, function(x) paste(x$County, collapse = ", "))
    
    # Convert the result to a data frame
    chb_df <- as.data.frame(chb_character)
    
    # Create an empty character vector to store the results
    chb_result <- character()
    
    # Loop through each row of chb_df
    for (i in 1:nrow(chb_df)) {
      # Get the row name and chb_character_placeholder as strings
      rowname <- as.character(rownames(chb_df)[i])
      chb_character_placeholder <- as.character(chb_df$chb_character[i])
      
      # Concatenate them with a separator
      concat <- paste0(rowname, ":: ", chb_character_placeholder)
      
      # Append the result to the vector
      chb_result <- c(chb_result, concat)
    }
    
    # Paste the result vector and create a new line after each CHB
    HTML(paste(chb_result, collapse =  "<br/>"))
  })
  
  # Update the region input based on the county input
  # In case data source doesn't have the same number of counties Null, Blank, Minnesota are examples. 
  # Currently, the county source is from the region data source but if it ever changes this should help capture it
  observeEvent(input$parGlobal_county, {
    if(input$parGlobal_county %in% df_schsacRaw$County) {
      updateSelectInput(
        session, "parGlobal_region",
        choices =  unique(df_schsacRaw$Region[order(df_schsacRaw$County == input$parGlobal_county, decreasing = TRUE)]),
        selected = unique(df_schsacRaw$Region[order(df_schsacRaw$County == input$parGlobal_county, decreasing = TRUE)])[1] #Default to the first choice, which will automatically change the cascading results when the parameter changes
      )
    }
    else {
      updateSelectInput(
        session, "parGlobal_region",
        choices = "",
        selected = ""
      )
    }
  }
  )
  
  # Update the chb input based on the county input
  # In case data source doesn't have the same number of counties Null, Blank, Minnesota are examples. 
  # Currently, the county source is from the chb data source but if it ever changes this should help capture it
  observeEvent(input$parGlobal_county, {
    if(input$parGlobal_county %in% df_chbRaw$County) {
      updateSelectInput(
        session, "parGlobal_chb",
        choices =  unique(df_chbRaw$CHB[order(df_chbRaw$County == input$parGlobal_county, decreasing = TRUE)]),
        selected = unique(df_chbRaw$CHB[order(df_chbRaw$County == input$parGlobal_county, decreasing = TRUE)])[1] #Default to the first choice, which will automatically change the cascading results when the parameter changes
      )
    }
    else {
      updateSelectInput(
        session, "parGlobal_chb",
        choices = "",
        selected = ""
      )
    }
  }
  )
  
  # Observe what sidebar the user is on and only allow for read only access to global parameters
  observe({
    if(input$smID == "tn_homePage") # it requires an ID of sidebarMenu
      {
        shinyjs::hide("parGlobal_hideNarrative")
        shinyjs::hide("parGlobal_county")
        shinyjs::hide("parGlobal_region")
        shinyjs::hide("parGlobal_chb")
      }
    else if(input$smID == "tn_regionChbDefinations") # it requires an ID of sidebarMenu
        {
          shinyjs::hide("parGlobal_hideNarrative")
          shinyjs::show("parGlobal_county")
          shinyjs::hide("parGlobal_region")
          shinyjs::hide("parGlobal_chb")
    }
    else if (input$smID == "tn_healthyHomes" & input$tpId_healthyHomes == "Childhood Lead Exposure" & isTRUE(input$parGlobal_hideNarrative))
        {
    #Toggle does not seem to work. It works for one interation and then it doesn't function correcly with the checkbox which is why there are two if statements
    shinyjs::hide("parLocal_leadYear")
    shinyjs::hide("par_leadStateRegionChb")
    shinyjs::hide("cID_leadNarrative")
  }
    else
      {
        shinyjs::show("parGlobal_hideNarrative")
        shinyjs::show("parGlobal_county")
        shinyjs::show("parGlobal_region")
        shinyjs::show("parGlobal_chb")
        
        shinyjs::show("parLocal_leadYear")
        shinyjs::show("par_leadStateRegionChb")
        shinyjs::show("cID_leadNarrative") 
      }
  })
  
  # # Observe the input value of the checkbox
  # observe({
  #   # Submenu --> tab name dependent on the parGlbal_hideNarrative
  #   #This is done so it isn't doing it in the background when tab panels or submenus are changes
  #   # Shiny may already do this but my thought process is since I don't know for sure that I should include it in the application until I learn more about shiny 4/9/2024
  #   if(input$smID == "tn_healthyHomes" & input$tpId_healthyHomes == "Childhood Lead Exposure" & isTRUE(input$parGlobal_hideNarrative))
  #   {
  #     #Toggle does not seem to work. It works for one interation and then it doesn't function correcly with the checkbox which is why there are two if statements
  #     shinyjs::hide("parLocal_leadYear")
  #     shinyjs::hide("par_leadStateRegionChb")
  #     shinyjs::hide("cID_leadNarrative")
  #   }
  #   if(input$smID == "tn_healthyHomes" & input$tpId_healthyHomes == "Childhood Lead Exposure" & isFALSE(input$parGlobal_hideNarrative))
  #   {
  #     shinyjs::show("parLocal_leadYear")
  #     shinyjs::show("par_leadStateRegionChb")
  #     shinyjs::show("cID_leadNarrative")
  #   }
  # })
  
  #Healthy Homes
  
  #Display every other label
  # Get the unique values of x
  xlabels <- unique(df_leadRaw$year)
  # Remove every other value by subsetting with a logical vector
  xlabels [c (FALSE, TRUE)] <- ""
  
  #Lead
  #State
  output$lead_state <-  renderPlot({
    df_leadRaw[df_leadRaw$location =="Minnesota" & 
                 df_leadRaw$indicator== "Blood lead testing"&
                 df_leadRaw$indicator.type == "Test year (annual method)"
               #Requires a comma to work
               ,] |>
      ggplot(aes(x= year, y= pctTested, color= ageGroup)) +
      geom_line()+
      geom_point()+
      theme(
        legend.position="bottom", # move legend to bottom rather than have it on the right
        text= element_text(size= 21), # increase font size 
        plot.title= element_text(hjust = 0.5) # Center the title
      )+
      guides(color = guide_legend(title = "Age Group"))+
      scale_color_discrete(breaks= c('<3 years', '3-<6 years', '<6 years'))+
      scale_x_discrete(limit= df_leadRaw$year, breaks = seq(min(df_leadRaw$year), max(df_leadRaw$year), 2))+ # add breaks argument here Microsoft Copilot AI generated
      #Sets y axis for the same of all the graphs
      scale_y_continuous(limits= c(0, 60)) +
      labs(
        title = "Blood Lead Testing (Test Year) for all of \nMinnesota", #\n means a new line 
        x = NULL,
        y = "Pct Tested"
        #caption = "Data last updated, 1/15/2024"
      )
  })
  
  
  # #  
  #Reactive Data
  lead_region_sub <- reactive({lead_RegionComplete[input$parGlobal_region == lead_RegionComplete$Region & #Changed variable so no longer need to use gsub
                                                     lead_RegionComplete$indicator== "Blood lead testing"&
                                                     lead_RegionComplete$indicator.type == "Test year (annual method)",] })
  
  output$lead_region <-  renderPlot({
    #Open parenthesis since it is dynamic
    lead_region_sub() |>
      ggplot(aes(x= year, y= regionTestPct, color= ageGroup)) +
      geom_line()+
      geom_point()+
      theme(
        legend.position="bottom", # move legend to bottom rather than have it on the right
        text= element_text(size= 21), # increase font size 
        plot.title= element_text(hjust = 0.5) # Center the title
      )+
      guides(color = guide_legend(title = "Age Group"))+
      scale_color_discrete(breaks=c('<3 years', '3-<6 years', '<6 years'))+
      scale_x_discrete(limit= df_leadRaw$year, breaks = seq(min(df_leadRaw$year), max(df_leadRaw$year), 2))+ # add breaks argument here Microsoft Copilot AI generated
      #Sets y axis for the same of all the graphs
      scale_y_continuous(limits= c(0, 60)) +
      labs(
        title = paste("Blood Lead Testing (Test Year) for \n", input$parGlobal_region, "Region"), #\n means a new line 
        x = NULL,
        y = "Pct Tested"
        #caption = "Data last updated, 1/15/2024"
      )
  })
  
  #CHB
  
  #Reactive Data
  validateChb <- reactive({
    validate(
      need(
        input$parGlobal_chb %in% lead_CHBComplete$CHB, paste("There is no data for ", input$parGlobal_chb)
      )
    )
  })
  
  #Reactive Data
  # validateChb <- reactive({
  #                         if(input$parGlobal_chb == lead_CHBComplete$CHB
  #                            ) 
  #                             {
  #                             validate(
  #                                     paste("There is no data for ", input$parGlobal_chb)
  #                                     )
  #                             }
  #                       })
  
  #Reactive Data
  lead_CHB_sub <- reactive({
    lead_CHBComplete[
      input$parGlobal_chb == lead_CHBComplete$CHB  &
      lead_CHBComplete$indicator== "Blood lead testing"&
      lead_CHBComplete$indicator.type == "Test year (annual method)",] 
  })
  
  
  output$lead_chb <-  renderPlot({
    validateChb() #Won't return red error meassage. It will display the validateChb error message 
    #Open parenthesis since it is dynamic
    lead_CHB_sub() |>
      ggplot(aes(x= year, y= CHBTestPct, color= ageGroup))  +
      geom_line()+
      geom_point()+
      theme(
        legend.position="bottom", # move legend to bottom rather than have it on the right
        text= element_text(size= 21), # increase font size 
        plot.title= element_text(hjust = 0.5) # Center the title
      )+
      guides(color = guide_legend(title = "Age Group"))+
      scale_color_discrete(breaks=c('<3 years', '3-<6 years', '<6 years'))+
      #Had this
      # scale_x_discrete(limits = df_leadRaw$year, guide = guide_axis(n.dodge = 2))+ 
      scale_x_discrete(limit= df_leadRaw$year, breaks = seq(min(df_leadRaw$year), max(df_leadRaw$year), 2))+ # add breaks argument here Microsoft Copilot AI generated
      #Sets y axis for the same of all the graphs
      scale_y_continuous(limits= c(0, 60)) +
      labs(
        title = paste("Blood Lead Testing (Test Year) for \n", input$parGlobal_chb, "CHB"), #\n means a new line
        x = NULL,
        y = "Pct Tested",
        caption = "Data last updated, 1/15/2024"
      )
  })
  
  
  
  
  # Get county data subset
  lead_county_sub <- reactive({df_leadRaw[df_leadRaw$location == input$parGlobal_county & 
                                            df_leadRaw$indicator== "Blood lead testing" &
                                            df_leadRaw$indicator.type == "Test year (annual method)",] 
  })
  
  output$lead_county <-  renderPlot({
    #Open parenthesis since it is dynamic
    lead_county_sub() |>
    
      ggplot(aes(x= year, y= pctTested, color= ageGroup)) +
      geom_line()+
      geom_point()+
      theme(
        legend.position="bottom", # move legend to bottom rather than have it on the right
        text= element_text(size= 21), # increase font size
        plot.title= element_text(hjust = 0.5) # Center the title
      )+
      guides(color = guide_legend(title = "Age Group"))+
      scale_color_discrete(breaks=c('<3 years', '3-<6 years', '<6 years'))+
      scale_x_discrete(limit= df_leadRaw$year, breaks = seq(min(df_leadRaw$year), max(df_leadRaw$year), 2))+ # add breaks argument here Microsoft Copilot AI generated
      #Sets y axis for the same of all the graphs
      scale_y_continuous(limits= c(0, 60))+
      labs(
        title = paste("Blood Lead Testing (Test Year) for \n", input$parGlobal_county, "County"), #\n means a new line
        x = NULL,
        y = "Pct Tested"
        # caption = "Data last updated, 1/15/2024"
      )
  })
  
  # https://stackoverflow.com/questions/23233497/outputting-multiple-lines-of-text-with-rendertext-in-r-shiny
  output$lead_narrative <- renderUI({
    ####################################################################################################################################################################################
    ## LESS then 3 YEARS OF AGE
    
    ##### Less than 3 years of age State      
    strAgeLessThanThreeState <- if(
      #ifelse is required for the times when a value is not provided
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested),
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested,
        0
      )
      <
      #ifelse isn't required for state since state will always return a value
      #Unique is required because there is a duplicate for Minnesota in 2019. This was brought up to MDH and since there tool is changing in 2024 they are not going to fix error.
      unique(
        df_leadRaw[df_leadRaw$year == input$parLocal_leadYear &
                   df_leadRaw$ageGroup == "<3 years" &
                   df_leadRaw$location == "Minnesota" &
                   df_leadRaw$indicator== "Blood lead testing" &
                   df_leadRaw$indicator.type == "Test year (annual method)", ]$pctTested
      )
    )
    {" <b>less</b> than <font color=red>"}
    else  {" <b>greater</b> than <font color=red>"}
    
    strAgeLessThanThreeState <- paste0(
      strAgeLessThanThreeState,
      "Minnesota </font> ",
      unique(
        df_leadRaw[ df_leadRaw$year == input$parLocal_leadYear &
                      df_leadRaw$ageGroup == "<3 years" &
                      df_leadRaw$location == "Minnesota" &
                      df_leadRaw$indicator== "Blood lead testing" &
                      df_leadRaw$indicator.type == "Test year (annual method)", ]$pctTested
      ),
      "% (",
      unique(
        df_leadRaw[ df_leadRaw$year == input$parLocal_leadYear &
                      df_leadRaw$ageGroup == "<3 years" &
                      df_leadRaw$location == "Minnesota" &
                      df_leadRaw$indicator== "Blood lead testing" &
                      df_leadRaw$indicator.type == "Test year (annual method)", ]$numTested
      ),
      "/",
      unique(
        df_leadRaw[ df_leadRaw$year == input$parLocal_leadYear &
                      df_leadRaw$ageGroup == "<3 years" &
                      df_leadRaw$location == "Minnesota" &
                      df_leadRaw$indicator== "Blood lead testing" &
                      df_leadRaw$indicator.type == "Test year (annual method)", ]$denominator
      ),
      ")"
    )
    
    ##### Less than 3 years of age Region      
    strAgeLessThanThreeRegion <- if(
      #ifelse is required for the times when a value is not provided
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested,
        0
      )
      <
      #ifelse isn't required for regions since regions will always return a value
      unique(lead_region_sub()[lead_region_sub()$year == input$parLocal_leadYear & lead_region_sub()$ageGroup == "<3 years", ]$regionTestPct)
    )
    {" <b>less</b> than the <font color=red>"}
    else  {" <b>greater</b> than the <font color=red>"}
    
    strAgeLessThanThreeRegion <- paste0(
      strAgeLessThanThreeRegion,
      input$parGlobal_region, 
      "</font> region ",
      unique(lead_region_sub()[lead_region_sub()$year == input$parLocal_leadYear & lead_region_sub()$ageGroup == "<3 years", ]$regionTestPct),
      "% (",
      unique(lead_region_sub()[lead_region_sub()$year == input$parLocal_leadYear & lead_region_sub()$ageGroup == "<3 years", ]$regionNumTested),
      "/",
      unique(lead_region_sub()[lead_region_sub()$year == input$parLocal_leadYear & lead_region_sub()$ageGroup == "<3 years", ]$regionDenominator),
      ")"
    )
    
    ##### Less than 3 years of age CHB      
    strAgeLessThanThreeChb <- if(
      #ifelse is required for the times when a value is not provided
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested,
        0
      )
      <
      #ifelse is required for the times when a value is not provided
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$CHBTestPct)), 
        #If meets criteria and is not null then it will return valid value
        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$CHBTestPct),
        0
      )
    )
    {" <b>less</b> than <font color=red>"}
    else  { " <b>greater</b> than <font color=red>"}
    
    strAgeLessThanThreeChb <- paste0(
      strAgeLessThanThreeChb,
      input$parGlobal_chb, 
      "</font> ",
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$CHBTestPct)), 
        #If meets criteria and is not null then it will return valid value
        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$CHBTestPct),
        0
      ),
      "% (",
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$chbNumTested)), 
        #If meets criteria and is not null then it will return valid value
        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$chbNumTested),
        0
      ),
      "/",
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$chbDenominator)), 
        #If meets criteria and is not null then it will return valid value
        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<3 years", ]$chbDenominator),
        0
      ),
      ")"
    )
    
    #######################Final Narrative snippet     
    strAgeLessThanThree <- paste0(
      "In <font color=red>",
      input$parLocal_leadYear,
      "</font>, ",
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$pctTested,
        0
      ),
      "% (",
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$numTested), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$numTested,
        0
      ),
      "/",
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$denominator), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<3 years", ]$denominator,
        0
      ),
      ") of <font color=red>",
      input$parGlobal_county, 
      "</font> residents <b>under the age of 3 </b>had their blood lead levels tested which is",
      if(input$par_leadStateRegionChb %in% c("All","CHB")){strAgeLessThanThreeChb},
      if(input$par_leadStateRegionChb %in% c("All","Region")){strAgeLessThanThreeRegion},
      if(input$par_leadStateRegionChb %in% c("All","State")){strAgeLessThanThreeState}
    )
    
    ####################################################################################################################################################################################
    ## 3 to 6 YEARS OF AGE
    
    ##### 3 to 6 years of age State      
    strAgeThreeToSixState <- if(
      #ifelse is required for the times when a value is not provided
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested),
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested,
        0
      )
      <
      #ifelse isn't required for state since state will always return a value
      #Unique is required because there is a duplicate for Minnesota in 2019. This was brought up to MDH and since there tool is changing in 2024 they are not going to fix error.
      unique(
        df_leadRaw[df_leadRaw$year == input$parLocal_leadYear &
                   df_leadRaw$ageGroup == "3-<6 years" &
                   df_leadRaw$location == "Minnesota" &
                   df_leadRaw$indicator== "Blood lead testing" &
                   df_leadRaw$indicator.type == "Test year (annual method)", ]$pctTested
      )
    )
    {" <b>less</b> than <font color=red>"}
    else  {" <b>greater</b> than <font color=red>"}
    
    strAgeThreeToSixState <- paste0(
      strAgeThreeToSixState,
      "Minnesota </font> ",
      unique(
        df_leadRaw[ df_leadRaw$year == input$parLocal_leadYear &
                      df_leadRaw$ageGroup == "3-<6 years" &
                      df_leadRaw$location == "Minnesota" &
                      df_leadRaw$indicator== "Blood lead testing" &
                      df_leadRaw$indicator.type == "Test year (annual method)", ]$pctTested
      ),
      "% (",
      unique(
        df_leadRaw[ df_leadRaw$year == input$parLocal_leadYear &
                      df_leadRaw$ageGroup == "3-<6 years" &
                      df_leadRaw$location == "Minnesota" &
                      df_leadRaw$indicator== "Blood lead testing" &
                      df_leadRaw$indicator.type == "Test year (annual method)", ]$numTested
      ),
      "/",
      unique(
        df_leadRaw[ df_leadRaw$year == input$parLocal_leadYear &
                      df_leadRaw$ageGroup == "3-<6 years" &
                      df_leadRaw$location == "Minnesota" &
                      df_leadRaw$indicator== "Blood lead testing" &
                      df_leadRaw$indicator.type == "Test year (annual method)", ]$denominator
      ),
      ")"
    )
    
    
    ##### 3 to 6 years of age Region      
    strAgeThreeToSixRegion <- if(
      #ifelse is required for the times when a value is not provided
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested,
        0
      )
      <
      unique(lead_region_sub()[lead_region_sub()$year == input$parLocal_leadYear & lead_region_sub()$ageGroup == "3-<6 years", ]$regionTestPct)
    )
    {" <b>less</b> than <font color=red>"}
    else  {" <b>greater</b> than <font color=red>"}
    
    strAgeThreeToSixRegion <- paste0(
      strAgeThreeToSixRegion,
      input$parGlobal_region, 
      "</font> ",
      unique(lead_region_sub()[lead_region_sub()$year == input$parLocal_leadYear & lead_region_sub()$ageGroup == "3-<6 years", ]$regionTestPct),
      "% (",
      unique(lead_region_sub()[lead_region_sub()$year == input$parLocal_leadYear & lead_region_sub()$ageGroup == "3-<6 years", ]$regionNumTested),
      "/",
      unique(lead_region_sub()[lead_region_sub()$year == input$parLocal_leadYear & lead_region_sub()$ageGroup == "3-<6 years", ]$regionDenominator),
      ")"
    )
    
    ##### 3 to 6 years of age CHB      
    strAgeThreeToSixChb <- if(
      #ifelse is required for the times when a value is not provided
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested,
        0
      )
      <
      #ifelse is required for the times when a value is not provided
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$CHBTestPct)), 
        #If meets criteria and is not null then it will return valid value
        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$CHBTestPct),
        0
      )
    )
    {" <b>less</b> than <font color=red>"}
    else  {" <b>greater</b> than <font color=red>"}
    
    strAgeThreeToSixChb <- paste0(
      strAgeThreeToSixChb,
      input$parGlobal_chb,
      "</font> ",
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$CHBTestPct)), 
        #If meets criteria and is not null then it will return valid value
        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$CHBTestPct),
        0
      ),
      "% (",
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$chbNumTested)), 
        #If meets criteria and is not null then it will return valid value
        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$chbNumTested),
        0
      ),
      "/",
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$chbDenominator)), 
        #If meets criteria and is not null then it will return valid value
        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "3-<6 years", ]$chbDenominator),
        0
      ),
      ")"
    )
    #######################Final Narrative snippet     
    strAgeThreeToSix <- paste0(
      "In <font color=red>",
      input$parLocal_leadYear,
      "</font>, ",
      ifelse(
        #Any checks if any true values exist. is.na did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$pctTested,
        0
      ),
      "% (",
      ifelse(
        #Any checks if any true values exist. is.na did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$numTested), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$numTested,
        0
      ),
      "/",
      ifelse(
        #Any checks if any true values exist. is.na did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$denominator), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "3-<6 years", ]$denominator,
        0
      ),
      ") of <font color=red>",
      input$parGlobal_county, 
      "</font> residents ages <b>3 to 6 </b>had their blood lead levels tested which is",
      if(input$par_leadStateRegionChb %in% c("All","CHB")){strAgeThreeToSixChb},
      if(input$par_leadStateRegionChb %in% c("All","Region")){strAgeThreeToSixRegion},
      if(input$par_leadStateRegionChb %in% c("All","State")){strAgeThreeToSixState}
    )
    ####################################################################################################################################################################################
    ## Less Than 6 YEARS OF AGE
    
    ##### < 6 years of age State      
    strAgeLessThanSixState <- if(
      #ifelse is required for the times when a value is not provided
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested),
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested,
        0
      )
      <
      #ifelse isn't required for state since state will always return a value
      #Unique is required because there is a duplicate for Minnesota in 2019. This was brought up to MDH and since there tool is changing in 2024 they are not going to fix error.
      unique(
        df_leadRaw[df_leadRaw$year == input$parLocal_leadYear &
                   df_leadRaw$ageGroup == "<6 years" &
                   df_leadRaw$location == "Minnesota" &
                   df_leadRaw$indicator== "Blood lead testing" &
                   df_leadRaw$indicator.type == "Test year (annual method)", ]$pctTested
      )
    )
    {" <b>less</b> than <font color=red>"}
    else  {" <b>greater</b> than <font color=red>"}
    
    strAgeLessThanSixState <- paste0(
      strAgeLessThanSixState,
      "Minnesota </font> ",
      unique(
        df_leadRaw[ df_leadRaw$year == input$parLocal_leadYear &
                      df_leadRaw$ageGroup == "<6 years" &
                      df_leadRaw$location == "Minnesota" &
                      df_leadRaw$indicator== "Blood lead testing" &
                      df_leadRaw$indicator.type == "Test year (annual method)", ]$pctTested
      ),
      "% (",
      unique(
        df_leadRaw[ df_leadRaw$year == input$parLocal_leadYear &
                      df_leadRaw$ageGroup == "<6 years" &
                      df_leadRaw$location == "Minnesota" &
                      df_leadRaw$indicator== "Blood lead testing" &
                      df_leadRaw$indicator.type == "Test year (annual method)", ]$numTested
      ),
      "/",
      unique(
        df_leadRaw[ df_leadRaw$year == input$parLocal_leadYear &
                      df_leadRaw$ageGroup == "<6 years" &
                      df_leadRaw$location == "Minnesota" &
                      df_leadRaw$indicator== "Blood lead testing" &
                      df_leadRaw$indicator.type == "Test year (annual method)", ]$denominator
      ),
      ")"
    )
    
    ##### < 6 years of age Region      
    strAgeLessThanSixRegion <- if(
      #ifelse is required for the times when a value is not provided
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested,
        0
      )
      <
      unique(lead_region_sub()[lead_region_sub()$year == input$parLocal_leadYear & lead_region_sub()$ageGroup == "<6 years", ]$regionTestPct)
    )
    {" <b>less</b> than <font color=red>"}
    else  {" <b>greater</b> than <font color=red>"}
    
    strAgeLessThanSixRegion <- paste0(
      strAgeLessThanSixRegion,
      input$parGlobal_region, 
      "</font> ",
      unique(lead_region_sub()[lead_region_sub()$year == input$parLocal_leadYear & lead_region_sub()$ageGroup == "<6 years", ]$regionTestPct),
      "% (",
      unique(lead_region_sub()[lead_region_sub()$year == input$parLocal_leadYear & lead_region_sub()$ageGroup == "<6 years", ]$regionNumTested),
      "/",
      unique(lead_region_sub()[lead_region_sub()$year == input$parLocal_leadYear & lead_region_sub()$ageGroup == "<6 years", ]$regionDenominator),
      ")"
    )
    
    ##### < 6 years of age CHB      
    strAgeLessThanSixChb <- if(
      #ifelse is required for the times when a value is not provided
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested,
        0
      )
      <
      #ifelse is required for the times when a value is not provided
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$CHBTestPct)), 
        #If meets criteria and is not null then it will return valid value
        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$CHBTestPct),
        0
      )
    )
    {" <b>less</b> than <font color=red>"}
    else  {" <b>greater</b> than <font color=red>"}
    
    strAgeLessThanSixChb <- paste0(
      strAgeLessThanSixChb,
      input$parGlobal_chb,
      "</font> ",
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$CHBTestPct)), 
        #If meets criteria and is not null then it will return valid value
        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$CHBTestPct),
        0
      ),
      "% (",
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$chbNumTested)), 
        #If meets criteria and is not null then it will return valid value
        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$chbNumTested),
        0
      ),
      "/",
      ifelse(
        #Any checks if any true values exist. is.na() did not work correctly
        any(unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$chbDenominator)), 
        #If meets criteria and is not null then it will return valid value
        unique(lead_CHB_sub()[lead_CHB_sub()$year == input$parLocal_leadYear & lead_CHB_sub()$ageGroup == "<6 years", ]$chbDenominator),
        0
      ),
      ")"
    )
    #######################Final Narrative snippet     
    strAgeLessThanSix <- paste0(
      "In <font color=red>",
      input$parLocal_leadYear,
      "</font>, ",
      ifelse(
        #Any checks if any true values exist. is.na did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$pctTested,
        0
      ),
      "% (",
      ifelse(
        #Any checks if any true values exist. is.na did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$numTested), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$numTested,
        0
      ),
      "/",
      ifelse(
        #Any checks if any true values exist. is.na did not work correctly
        any(lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$denominator), 
        #If meets criteria and is not null then it will return valid value
        lead_county_sub()[lead_county_sub()$year == input$parLocal_leadYear & lead_county_sub()$ageGroup == "<6 years", ]$denominator,
        0
      ),
      ") of <font color=red>",
      input$parGlobal_county, 
      "</font> residents <b>under the age of 6 </b> had their blood lead levels tested which is",
      if(input$par_leadStateRegionChb %in% c("All","CHB")){strAgeLessThanSixChb},
      if(input$par_leadStateRegionChb %in% c("All","Region")){strAgeLessThanSixRegion},
      if(input$par_leadStateRegionChb %in% c("All","State")){strAgeLessThanSixState}
    )
    HTML(paste
         (
           strAgeLessThanThree,
           strAgeThreeToSix,
           strAgeLessThanSix,
           sep= '<br/><br/>' #I wanted two line breaks between each string
         )
    )
    
  })
}

shinyApp(ui=ui, server=server)
