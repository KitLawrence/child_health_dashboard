

#Setup ----
##libraries ----
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(ggplot2)
library(readr)

library(labelled)
library(data.table)
library(DT)
library(plotly)
library(openxlsx)

library(shiny)
library(shinymanager)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(fresh)
library(shinya11y)

library(phsstyles)
library(phsmethods)
library(janitor)


source("functions.R")






##read in files ----

#date must match the file name to read in
extract_date <- "28thAugust2023"

#create the file names using the extract date
files <- paste0(extract_date,
                "Dashboard - ",
                c("firstvisit", "6-8 week",
                  "13-15m", "27-30m",
                  "13-15m_Domains", "27-30m_Domains",
                  "13-15m_SIMD", "27-30m_SIMD"),
                ".xlsx")

#read in each file and put them in a list
dashboard_data <- map(files, \(x) {
  read.xlsx(
    paste0("/PHI_conf/ChildHealthSurveillance/Topics/ChildHealth/Projects/20200707-Covid19-Breastfeeding-and-ChildDevelopment/Output/",
           x)
  ) |> 
    
    #make the data we read in tidier
    tibble() |> 
    clean_names() |>
    select( -any_of("hscp2019_code")) |>
    filter(!(geography %in% c("Argyll & Clyde", "Unknown HSCP"))) |>
    mutate(geography = case_when(geography == "Scotland" ~ "All Scotland",
                                 str_ends(geography, "HSCP", negate = TRUE) ~ paste0("NHS ", geography),
                                 TRUE ~ geography) |> factor(),
           month_review = as.Date(month_review, origin = ymd("1900-01-01")-2)
    )
})

#put names on each of the tibbles
names(dashboard_data) <- c("feeding_first_visit", "feeding_6_8_week_review",
                           "development_13_15_month_review", "development_27_30_month_review",
                           "development_13_15_month_review_domains", "development_27_30_month_review_domains",
                           "development_13_15_month_review_simd", "development_27_30_month_review_simd")








##reference lists ----
HBnames <- c("NHS Ayrshire & Arran", "NHS Borders", "NHS Dumfries & Galloway",
             "NHS Fife", "NHS Forth Valley", "NHS Grampian", "NHS Greater Glasgow & Clyde",
             "NHS Highland", "NHS Lanarkshire", "NHS Lothian", "NHS Tayside", "NHS Orkney",
             "NHS Shetland", "NHS Western Isles")

HSCPnames <- c("Aberdeen City HSCP", "Aberdeenshire HSCP", "Angus HSCP", "Argyll and Bute HSCP",
               "Clackmannanshire and Stirling HSCP", "Dumfries and Galloway HSCP",
               "Dundee City HSCP", "East Ayrshire HSCP", "East Dunbartonshire HSCP",
               "East Lothian HSCP", "East Renfrewshire HSCP", "Edinburgh HSCP",
               "Falkirk HSCP", "Fife HSCP", "Glasgow City HSCP", "Highland HSCP",
               "Inverclyde HSCP", "Midlothian HSCP", "Moray HSCP", "North Ayrshire HSCP",
               "North Lanarkshire HSCP", "Orkney Islands HSCP", "Perth and Kinross HSCP", 
               "Renfrewshire HSCP", "Scottish Borders HSCP", "Shetland Islands HSCP",
               "South Ayrshire HSCP", "South Lanarkshire HSCP", "West Dunbartonshire HSCP",
               "West Lothian HSCP", "Western Isles HSCP")


domains <- read_csv("variable, title
slc_perc, Speech, language & communication
             prob_solv_perc, Problem solving
             gross_motor_perc, Gross motor
             per_soc_perc, Personal/Social
             fine_motor_perc, Fine motor
             emot_beh_perc, Emotional/Behavioural
             vision_perc, Vision
             hearing_perc, Hearing")









#.----
#header ----

#makes the logo that goes in the top left corner of the dashboard
dashboardtitle <- tags$a(href = "https://www.publichealthscotland.scot/",
                         target="_blank",
                         tags$imag(src = "phs-logo.png",
                                   alt = "Public Health Scotland logo",
                                   width = 120
                         )
)

#makes the header bar, which includes the logo on the top left and title on the right
header <- dashboardHeader(
  title = dashboardtitle,
  titleWidth = 290,
  tags$li(class = "dropdown",
          tags$p("Child Health Dashboard v0.1")
  )
)






#sidebar ----
#creates the sidebar menu for the dashboard
sidebar <- dashboardSidebar(
  width = 280,
  
  #adds in the options for each page of the dashboard
  accessible_menu(    #function to please screen readers
    sidebarMenu(
      id = "menu",
      menuItem("Home",
               tabName = "home",
               icon = icon("info-circle", verify_fa = FALSE) |> rem_aria_label(),
               selected = TRUE),
      menuItem("Infant Feeding",
               tabName = "infant_feeding",
               icon = icon("person-breastfeeding", verify_fa = FALSE) |> rem_aria_label()),
      menuItem("Child Development",
               tabName = "child_development",
               icon = icon("children", verify_fa = FALSE) |> rem_aria_label())
    )    #sidebarMenu
  ),    #accessible_menu
  
  useShinyjs(),    #must be included to make javascript stuff happen
  
  #widgets that allow control of outputs, in sidebar to make them global
  radioButtons(
    inputId = "geog_level",
    label = "Select Geography Level to Display:",
    choices = c("All Scotland", "Health Board", "HSCP"),
    selected = "All Scotland"
  ),
  uiOutput("geog_select") # Board/HSCP name
)






#.----
#body ----

##mytheme ----

#must create a theme to make the colours match PHS colours
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#9B4393" # header bar = PHS-magenta
  ),
  adminlte_sidebar( # sidebar colours
    width = "290px",
    dark_bg = "#9B4393", # background colour (not selected) = PHS-magenta
    dark_hover_bg = "#3F3685", # background colour (when hovering) = PHS-purple
    dark_color = "#F5ECF4", # text colour (not selected) = white
    dark_hover_color = "#FFFFFF", #hover colour = white
    dark_submenu_bg = "#9B4393" #sub-menu background colour = PHS-magenta
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#FFF",
    info_box_bg = "#FFF"
  )
  #adminlte_vars(
  #box_border_color = "#FFF"
  #)
)





##Home ----
home <- tabItem(
  tabName = "home",
  fluidRow(
    tabBox(title = "Home",
           # The id lets us use input$tabset00 on the server to find the current tab
           id = "tabset00",
           width = 12,
           height = "25px",
           
           ### Welcome ----
           tabPanel(title = "Welcome",
                    fluidRow(box(width = 11,
                                 solidHeader = TRUE,
                                 h1("Welcome to the Child Health Dashboard"),
                                 p("This first page gives general detail. We have two indicators, listed in
                                 the two tabs on the left hand side."),
                                 p("A bunch of info will go on this page but to fill this space to see how
                                   it looks I will just type a bunch to see what it looks like. I can also
                                   include a link like ",
                                   tags$a(
                                     href = "https://scotland.shinyapps.io/phs-covid-wider-impact/",
                                     tags$u("this one to the wider impact dashboard"),
                                     class = "externallink", target = "_blank"),
                                   "from which our data will be taken."),
                                 p("Filler text filler text filler text filler
                                   text filler text filler text filler text filler 
                                   text filler text filler text filler text filler
                                   text filler text filler text filler text filler 
                                   text filler text filler text filler text filler 
                                   text filler text filler text filler text filler
                                   text filler text filler text filler text.")
                    ))
           ),  #tabPanel ("Welcome")
           
           ###Identifying Patterns ----
           tabPanel(title = "Identifying Patterns",
                    fluidRow(box(width = 11,
                                 solidHeader = TRUE,
                                 h1("How we identify patterns in the data"),
                                 p(strong("Run charts"), "have been used to show time series data for many of the measures
             in this dashboard. Run charts use a series of rules to help identify important change
             in the data. These are the ones we used for these charts:"
                                 ),
                                 
                                 tags$div(HTML("<ul>
                <li><strong>Shifts:</strong> Six or more consecutive data points above or below
                            the centreline. Points on the centreline neither break nor contribute
                            to a shift (marked on chart). </li>

                <li><strong>Trends:</strong> Five or more consecutive data points which are
                increasing or decreasing. An observation that is the same as the preceding value
                does not count towards a trend (marked on chart). </li>

                <li><strong>Too many or too few runs:</strong> A run is a sequence of one or more
                consecutive observations on the same side of the centreline. Any observations
                falling directly on the centreline can be ignored. If there are too many or too
                few runs (i.e. the median is crossed too many or too few times) that is a sign of
                something more than random chance. </li>

                <li><strong>Astronomical data point:</strong> A data point which is distinctly
                different from the rest. Different people looking at the same graph would be
                expected to recognise the same data point as astronomical (or not). </li>

                </ul>")),
                                 
                                 p("Further information on these methods of presenting data can be found in this ",
                                   
                                   tags$a(
                                     href =  "https://www.isdscotland.org/health-topics/quality-indicators/statistical-process-control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf",
                                     tags$u("guide to statistical process control charts."),
                                     class = "externallink", target = "_blank"
                                   )
                                 )
                    ))
           ) #tabPanel("How we identify patterns")
    )# tabBox ("Home")
  ) #fluidRow
)



##Infant Feeding ----
infant_feeding <- tabItem(
  tabName = "infant_feeding",
  fluidRow(
    tabBox(title = "Infant Feeding",
           # The id lets us use input$tabset00 on the server to find the current tab
           id = "tabset01",
           width = 12,
           height = "25px",
           
           ###Charts ----
           tabPanel(title = "Charts",
                    fluidRow(
                      box(width = 6,
                          radioGroupButtons(
                            inputId = "feeding_data",
                            label = "Select the data you want to explore:",
                            choiceNames = list("Health Visitor first visit", "6-8 week review"),
                            choiceValues = list("feeding_first_visit", "feeding_6_8_week_review"),
                            selected = "feeding_first_visit",
                            direction = "vertical",
                            justified = TRUE,
                            status = "primary",
                            checkIcon = list(
                              yes = icon("circle-check") |> rem_aria_label(),
                              no = icon("circle") |> rem_aria_label())
                          ) #radioGroupButtons
                      ), #box
                      box(width = 4, solidHeader = TRUE),
                      box(width = 2, solidHeader = TRUE,
                          downloadButton("", 
                                         "Download data",
                                         icon = shiny::icon("download") |> rem_aria_label()
                          ) 
                      ),
                    ), #fluidRow
                    
                    fluidRow(
                      box(width = 4, solidHeader = TRUE,
                          textOutput("perc_exclusive_breastfed_title"),
                          plotOutput("perc_exclusive_breastfed_plot", height = "200px"),
                          plotlyOutput("perc_exclusive_breastfed_plotly", height = "200px")
                      ),
                      box(width = 4, solidHeader = TRUE,
                          textOutput("perc_overall_breastfed_title"),
                          plotOutput("perc_overall_breastfed_plot", height = "200px"),
                          plotlyOutput("perc_overall_breastfed_plotly", height = "200px")
                      ),
                      box(width = 4, solidHeader = TRUE,
                          textOutput("perc_ever_breastfed_title"),
                          plotOutput("perc_ever_breastfed_plot", height = "200px"),
                          plotlyOutput("perc_ever_breastfed_plotly", height = "200px")
                      ),
                      
                      
                      box(width = 12, solidHeader = TRUE,
                          p("We have used ‘run charts’ to present the data above. 
                          Run charts use a series of rules to help identify unusual 
                          behaviour in data and indicate patterns that merit further 
                          investigation. Read more about the rules used in the charts
                          in the \"Identifying Data\" section of the Home page"),
                          p("To provide a basis for identifying patterns in the data,
                          the charts above use a blue line to show the average (median)
                          percentage of children who are recorded as breastfed over
                          the time period specified in the legend of each chart.
                          The blue line is dashed where the average is projected 
                          outside that time range. A black line shows the percentage
                          of children receiving a child health review who were recorded 
                          as being breastfed on their review record. Data is shown 
                          for each month from January 2019 onwards. The line becomes
                          yellow where there are 6 or more consecutive points above
                          or below the average, and is highlighted in green where 
                          there are 5 or more consecutively increasing or decreasing 
                          points.")),
                      
                      
                      box(width = 12,
                          textOutput("feeding_numbers_title"),
                          plotOutput("feeding_numbers_plot", height = "300px"),
                          plotlyOutput("feeding_numbers_plotly", height = "300px")
                      )
                    )
           ), #tabPanel ("Charts")
           
           
           ###About this indicator ----
           tabPanel(title = "About this indicator",
                    
                    fluidRow(
                      box(title = "Why measure this?",
                          width = 5,
                          p("This is the part where we add a description of the
                            indicator and why it is useful."),
                          p("Filler text Filler text Filler text Filler text 
                            Filler text Filler text Filler text Filler text 
                            Filler text Filler text Filler text Filler text 
                            Filler text Filler text Filler text Filler text.")
                      ),
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      box(title = "Data source and definitions",
                          width = 5,
                          p("Data Source: CHSP Pre-School"),
                          h3("Definitions"),
                          tags$div(HTML("<ul>
                <li><strong>Exclusively breastfed:</strong> children recorded as
                only being fed breastmilk in the previous 24 hour period </li>

                <li><strong>Overall breastfed:</strong> children recorded as being
                fed breast and formula milk in the previous 24 hour period </li>

                <li><strong>Ever breastfed:</strong> Has the child ever been breastfed?
                This is recorded at the Health Visitor First Visit. </li>


                </ul>")),
                          h3("Denominators used in calculations"),
                          p("The denominator for the breastfeeding indicators is the
                           number of reviews with valid data recorded (i.e. not ‘missing’
                           or ‘unknown’) for infant feeding status in the previous
                           24 hour period. Analysis is based on NHS Board of Residence."),
                          p("The average is calculated as the median value of the
                           period specified.")
                      )
                    )
           ) #tabPanel ("About this Measure")
    ) # tabBox ("Infant Feeding")
  )
)




##Child Development ----
child_development <- tabItem(
  tabName = "child_development",
  fluidRow(
    tabBox(title = "Child Development",
           # The id lets us use input$tabset00 on the server to find the current tab
           id = "tabset02",
           width = 12,
           height = "25px",
           
           ###Charts ----
           tabPanel(title = "Charts",
                    fluidRow(
                      box(width = 6,
                          radioGroupButtons(
                            inputId = "development_data",
                            label = "Select the data you want to explore:",
                            choiceNames = list("13-15 month review", "27-30 month review"),
                            choiceValues = list("development_13_15_month_review", "development_27_30_month_review"),
                            selected = "development_13_15_month_review",
                            direction = "vertical",
                            justified = TRUE,
                            status = "primary",
                            checkIcon = list(
                              yes = icon("circle-check") |> rem_aria_label(),
                              no = icon("circle") |> rem_aria_label())
                          ) #radioGroupButtons
                      ), #box
                      box(width = 4, solidHeader = TRUE),
                      box(width = 2, solidHeader = TRUE,
                          downloadButton("", 
                                         "Download data",
                                         icon = shiny::icon("download") |> rem_aria_label()
                          ) 
                      )
                    ), #fluidRow
                    
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          textOutput("development_percentage_concern_title"),
                          plotOutput("development_percentage_concern_plot", height = "300px"),
                          plotlyOutput("development_percentage_concern_plotly", height = "300px"),
                          
                          p("We have used ‘run charts’ to present the data above. 
                          Run charts use a series of rules to help identify unusual 
                          behaviour in data and indicate patterns that merit further 
                          investigation. Read more about the rules used in the charts
                          in the \"Identifying Data\" section of the Home page"),
                          p("To provide a basis for identifying patterns in the 
                            data, the chart above uses a blue line to show the average 
                            (median) percentage of children who are recorded as having 
                            1 or more developmental concern over the time period 
                            specified in the legend of the chart. The blue line is 
                            dashed where the average is projected outside that time
                            range. A black line shows the percentage of children 
                            receiving a child health review who had 1 or more developmental 
                            concern recorded on their review record. Data is shown 
                            for each month from January 2019 onwards. The line becomes
                            yellow where there are 6 or more consecutive points above 
                            or below the average, and is highlighted in green where
                            there are 5 or more consecutively increasing or decreasing
                            points.")
                      ),
                      
                      box(width = 12,
                          textOutput("development_numbers_title"),
                          plotOutput("development_numbers_plot", height = "300px"),
                          plotlyOutput("development_numbers_plotly", height = "300px")
                      )
                    ),
                    
                    uiOutput("scotland_only")
                    # box(width = 12,
                    #     textOutput("development_concerns_by_domain_title"),
                    #     plotOutput("development_concerns_by_domain_plot", height = "300px")
                    # ),
                    # 
                    # box(width = 12,
                    #     checkboxGroupButtons(
                    #       inputId = "simd_levels",
                    #       label = "Select which SIMD quintiles to show. \n
                    #       This scale ranges from 1 being the most deprived to 5 being the least deprived.",
                    #       choices = c(1:5),
                    #       selected = c(1),
                    #       status = "primary",
                    #       checkIcon = list(
                    #         yes = icon("square-check") |> rem_aria_label(),
                    #         no = icon("square") |> rem_aria_label())
                    #     ),
                    #     textOutput("development_concerns_by_simd_title"),
                    #     plotOutput("development_concerns_by_simd_plot", height = "300px")
                    # )
                    
                    
                    
                    
           ), #tabPanel ("Charts")
           
           ###About this indicator ----
           tabPanel(title = "About this indicator",
                    
                    fluidRow(
                      box(title = "Why measure this?",
                          width = 5, 
                          p("This is the part where we add a description of the
                            indicator and why it is useful."),
                          p("Filler text Filler text Filler text Filler text 
                            Filler text Filler text Filler text Filler text 
                            Filler text Filler text Filler text Filler text 
                            Filler text Filler text Filler text Filler text.")
                      ),
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          width = 5,
                          p("Data source: CHSP Pre-School"),
                          
                          h3("Meaningful Data"),
                          p("This refers to records where a value of N (no concerns),
                            C (concern newly suspected), or P (concern previously
                            identified) has been recorded for all eight developmental
                            domains assessed as part of the 13-15 month and 27-30
                            month child health reviews. See the ",
                            tags$a(
                              href = "https://beta.isdscotland.org/find-publications-and-data/population-health/child-health/early-child-development/15-september-2020/dashboard/",
                              tags$u("Early Child Development publication"),
                              class = "externallink", target = "_blank"),
                            " for further details."),
                          
                          h3("Denominators used in calculations"),
                          p("The denominator used in the child development indicators
                            is the total number of reviews, rather than the number
                            of reviews with meaningful data recorded. This is because
                            it is possible for children to have a developmental concern
                            identified against one or more developmental domain without
                            having meaningful data recorded for all domains. Analysis is
                            based on NHS Board of Residence."),
                          p("The 13-15 month review has only been delivered in NHS
                            Greater Glasgow & Clyde (NHS GG&C) from May 2019 onwards,
                            hence no data are shown for this review for NHS GG&C
                            for the period January to April 2019."),
                          p("For this reason, the pre-pandemic average for Scotland
                            and NHS GG&C (shown as the centreline in the charts)
                            is based on reviews provided in May 2019 to February
                            2020. The pre-pandemic average for all other Boards is
                            based on reviews provided in January 2019 to February
                            2020. The average is calculated as the median value of
                            the period specified.")
                      )
                    )
           ) #tabPanel ("About this Measure")
    ) # tabBox ("Child Development")
  )
)


##actual body ----
#put together each of the tabItems created above
body <-
  dashboardBody(
    use_theme(mytheme), # <-- use the theme to change colours
    tags$head(includeCSS("www/styles.css")),
    tags$head(
      tags$style(HTML(".sidebar {height: 90vh; overflow-y: auto;}",
                      ".box {-webkit-box-shadow: none;
                           -moz-box-shadow: none;
                           box-shadow: none;}",
                      ".nav-tabs-custom {box-shadow: none;}",
                      ".content-wrapper {overflow-y: hidden;}"
      ))),    #just a bunch of style stuff lifted from SPBaND
    
    #add dashboard pages to this list if we make more
    tabItems(home,
             infant_feeding,
             child_development)
  )





#.----
#ui ----
#pulls together all the elements made so far
ui <-   tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  tags$style("@import url(https://use.fontawesome.com/releases/v6.0/css/all.css);"),
  tags$head(HTML("<html lang='en'>"),
            tags$link(rel="shortcut icon",
                      href="favicon_phs.ico"), # Icon for browser tab
            tags$title("Child Health Dashboard")
  ),
  # Including Google analytics
  # includeScript("google-analytics.js")),
  
  #tool for assessing accessibility, uncomment to use
  #    use_tota11y(),
  
  #pull together the elements created above
  dashboardPage(
    header,
    sidebar,
    body
  ) # dashboardPage
)  # tagList









server <- function(input, output, session) {
  
  #Geography Control ----
  #reactive variable to decide what to show on the drop-down list
  geog_choices <- reactive({
    switch(input$geog_level,
           "All Scotland" = "Scotland",
           "Health Board" = HBnames,
           "HSCP" = HSCPnames)
  })
  
  output$geog_select <- renderUI({
    if (input$geog_level != "All Scotland") {
      pickerInput(
        inputId = "geog_chosen",
        label = paste0("Select ", input$geog_level),
        choices = geog_choices(),
        selected = geog_choices()[1],
        choicesOpt = list(
          style = rep("color: #3F3685;", # PHS-purple text
                      length(geog_choices()))
        ),
        options = list(
          size = 10)
      )
    }
  })
  
  geog_final <- reactive({
    switch(input$geog_level,
           "All Scotland" = "All Scotland",
           "Health Board" = input$geog_chosen,
           "HSCP" = input$geog_chosen)
  })
  
  
  
  
  
  
  
  #Versions of Names ----
  feeding_data_name <- reactive({
    switch(input$feeding_data,
           "feeding_first_visit" = "health visitor first visit",
           "feeding_6_8_week_review" = "6-8 week review")
  })
  
  development_data_name <- reactive({
    switch(input$development_data,
           "development_13_15_month_review" = "13-15 month review",
           "development_27_30_month_review" = "27-30 month review")
  })
  
  
  #.----
  
  
  #Infant Feeding Plots ----
  output$perc_exclusive_breastfed_title <- renderText({
    paste0("Percentage of children recorded as exclusively breastfed at ",
           feeding_data_name())
  })
  
  output$perc_overall_breastfed_title <- renderText({
    paste0("Percentage of children recorded as overall breastfed at ",
           feeding_data_name())
  })
  
  output$perc_ever_breastfed_title <- renderText({
    paste0("Percentage of children recorded as ever breastfed at ",
           feeding_data_name())
  })
  
  output$perc_exclusive_breastfed_plot <- renderPlot({
    create_runchart(dashboard_data[[input$feeding_data]], geog_final(), pc_excl) +
      ylim(0,100)
  })
  
  output$perc_overall_breastfed_plot <- renderPlot({
    create_runchart(dashboard_data[[input$feeding_data]], geog_final(), pc_overall) +
      ylim(0,100)
  })
  
  output$perc_ever_breastfed_plot <- renderPlot({
    create_runchart(dashboard_data[[input$feeding_data]], geog_final(), pc_ever) +
      ylim(0,100)
  })
  
  output$perc_exclusive_breastfed_plotly <- renderPlotly({
    create_runchart_plotly(dashboard_data[[input$feeding_data]], geog_final(), "pc_excl")
  })
  
  output$perc_overall_breastfed_plotly <- renderPlotly({
    create_runchart_plotly(dashboard_data[[input$feeding_data]], geog_final(), "pc_overall")
  })
  
  output$perc_ever_breastfed_plotly <- renderPlotly({
    create_runchart_plotly(dashboard_data[[input$feeding_data]], geog_final(), "pc_ever")
  })
  
  
  
  #Infant Feeding Numbers Plot ----
  output$feeding_numbers_title <- renderText({
    paste0("Number of ",
           feeding_data_name(),
           "s; ",
           str_extract(feeding_data_name(), " \\w*$"),
           "s with data on infant feeding recorded; and children recorded as being breastfed")
  })
  
  output$feeding_numbers_plot <- renderPlot({
    dashboard_data[[input$feeding_data]] |>
      filter(geography %in% geog_final()) |>
      pivot_longer(cols = no_reviews:ever_bf,
                   names_to = "category",
                   values_to = "number") |>
      ggplot(aes(x = month_review, y = number, color = category)) +
      geom_line()
  })
  
  
  output$feeding_numbers_plotly <- renderPlotly({
    data <- dashboard_data[[input$feeding_data]] |>
      filter(geography %in% geog_final()) |>
      pivot_longer(cols = no_reviews:ever_bf,
                   names_to = "category",
                   values_to = "number")
    
    plot_ly(data = data,
            x = ~ month_review,
            y = ~ number,
            type = "scatter",
            mode = "lines",
            linetype = ~ category)
  })
  
  
  #.----
  
  
  
  
  
  
  #Child Development Percentage Concern ----
  output$development_percentage_concern_title <- renderText({
    paste0("Percentage of children with one or more developmental concerns ",
           "recorded at the ", 
           development_data_name())
  })
  
  output$development_percentage_concern_plot <- renderPlot({
    create_runchart(dashboard_data[[input$development_data]], geog_final(), pc_1_plus) +
      ylim(0,40)
  })
  
  output$development_percentage_concern_plotly <- renderPlotly({
    create_runchart_plotly(dashboard_data[[input$development_data]], geog_final(), "pc_1_plus")
  })
  
  
  
  #Child Development Number of Reviews ----
  output$development_numbers_title <- renderText({
    paste0("Number of ",
           development_data_name(),
           "s; reviews with full meaningful data on child development recorded; ",
           "and children with 1 or more developmental concerns recorded")
  })
  
  output$development_numbers_plot <- renderPlot({
    dashboard_data[[input$development_data]] |>
      filter(geography %in% geog_final()) |>
      pivot_longer(cols = no_reviews:concerns_1_plus,
                   names_to = "category",
                   values_to = "number") |>
      ggplot(aes(x = month_review, y = number, color = category)) +
      geom_line()
  })
  
  output$development_numbers_plotly <- renderPlotly({
    dashboard_data[[input$development_data]] |>
      filter(geography %in% geog_final()) |>
      pivot_longer(cols = no_reviews:concerns_1_plus,
                   names_to = "category",
                   values_to = "number") |>
      plot_ly(x = ~ month_review,
              y = ~ number,
              type = "scatter",
              mode = "lines",
              linetype = ~ category)
  })
  
  
  
  
  #Child Development Concerns by Domain ----
  output$development_concerns_by_domain_title <- renderText({
    paste0("Percentage of ",
           development_data_name(),
           "s with a new or previous concern recorded by developmental domain")
  })
  
  output$development_concerns_by_domain_plot <- renderPlot({
    dashboard_data[[paste0(input$development_data, "_domains")]] |>
      filter(geography %in% geog_final()) |>
      pivot_longer(cols = slc_perc:hearing_perc,
                   names_to = "category",
                   values_to = "number") |>
      filter(category %in% input$domains_selected) |>
      ggplot(aes(x = month_review, y = number, color = category)) +
      geom_line()
  })
  
  output$development_concerns_by_domain_plotly <- renderPlotly({
    dashboard_data[[paste0(input$development_data, "_domains")]] |>
      filter(geography %in% geog_final()) |>
      pivot_longer(cols = slc_perc:hearing_perc,
                   names_to = "category",
                   values_to = "number") |>
      filter(category %in% input$domains_selected) |>
      plot_ly(x = ~ month_review,
              y = ~ number,
              type = "scatter",
              mode = "lines",
              linetype = ~ category)
  })
  
  
  
  #Child Develpments Concerns by SIMD Quintile ----
  output$development_concerns_by_simd_title <- renderText({
    paste0("Percentage of children with 1 or more developmental concerns recorded at the ",
           development_data_name(),
           " by SIMD deprivation quintile")
  })
  
  output$development_concerns_by_simd_plot <- renderPlot({
    dashboard_data[[paste0(input$development_data, "_simd")]] |>
      filter(geography %in% geog_final() & simd %in% input$simd_levels) |>
      ggplot(aes(x = month_review, y = pc_1_plus, color = simd)) +
      ylim(0,30) +
      geom_line()
  })
  
  output$development_concerns_by_simd_plotly <- renderPlotly({
    dashboard_data[[paste0(input$development_data, "_simd")]] |>
      filter(geography %in% geog_final() & simd %in% input$simd_levels) |>
      plot_ly(x = ~ month_review,
              y = ~ pc_1_plus,
              type = "scatter",
              mode = "lines",
              linetype = ~ simd)
  })
  
  
  
  #Scotland Only Section of UI ----
  output$scotland_only <- renderUI({
    if (input$geog_level == "All Scotland") {
      fluidRow(
        box(width = 12,
            checkboxGroupButtons(
              inputId = "domains_selected",
              label = "Select which domains to include in the plot:",
              choiceNames = domains$title,
              choiceValues = domains$variable,
              selected = domains$variable,
              status = "primary",
              checkIcon = list(
                yes = icon("square-check") |> rem_aria_label(),
                no = icon("square") |> rem_aria_label())
            ),
            textOutput("development_concerns_by_domain_title"),
            plotOutput("development_concerns_by_domain_plot", height = "300px"),
            plotlyOutput("development_concerns_by_domain_plotly", height = "300px")
        ),
        
        box(width = 12,
            checkboxGroupButtons(
              inputId = "simd_levels",
              label = "Select which SIMD quintiles to show.
                      This scale ranges from 1 being the most deprived to 5 being the least deprived.",
              choices = c(1:5),
              selected = c(1, 5),
              status = "primary",
              checkIcon = list(
                yes = icon("square-check") |> rem_aria_label(),
                no = icon("square") |> rem_aria_label())
            ),
            textOutput("development_concerns_by_simd_title"),
            plotOutput("development_concerns_by_simd_plot", height = "300px"),
            plotlyOutput("development_concerns_by_simd_plotly", height = "300px")
        )
      )
    }
  })
  
  
}





shinyApp(ui, server)
