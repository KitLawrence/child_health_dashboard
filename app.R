

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






##read in files ----

#date must match the file name to read in
extract_date <- "25thSeptember2023"

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
                                 TRUE ~ geography) |> factor(levels = c("All Scotland", HBnames, HSCPnames)),
           month_review = as.Date(month_review, origin = ymd("1900-01-01")-2)
    )
})

#put names on each of the tibbles
names(dashboard_data) <- c("feeding_first_visit", "feeding_6_8_week_review",
                           "development_13_15_month_review", "development_27_30_month_review",
                           "development_13_15_month_review_domains", "development_27_30_month_review_domains",
                           "development_13_15_month_review_simd", "development_27_30_month_review_simd")







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
               icon = icon("person-breastfeeding", verify_fa = FALSE) |> rem_aria_label(),
               menuSubItem("Charts",
                           tabName = "feeding_charts",
                           icon = shiny::icon("angle-double-right") |> rem_aria_label()),
               menuSubItem("About this indicator",
                           tabName = "feeding_about",
                           icon = shiny::icon("angle-double-right") |> rem_aria_label())
      ) |> rem_menu_aria_label(),
      
      menuItem("Child Development",
               icon = icon("children", verify_fa = FALSE) |> rem_aria_label(),
               menuSubItem("Charts",
                           tabName = "development_charts",
                           icon = shiny::icon("angle-double-right") |> rem_aria_label()),
               menuSubItem("About this indicator",
                           tabName = "development_about",
                           icon = shiny::icon("angle-double-right") |> rem_aria_label())
      ) |> rem_menu_aria_label()
      
    )    #sidebarMenu
  ),    #accessible_menu
  
  useShinyjs(),    #must be included to make javascript stuff happen
  
  #dev panel to track variables
  verbatimTextOutput("testing")
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
    dark_color = "#FFFFFF", # text colour (not selected) = white
    dark_hover_color = "#FFFFFF", #hover colour = white
    dark_submenu_bg = "#1E7F84", #sub-menu background colour = PHS-magenta-80
    dark_submenu_color = "#FFFFFF" #sub-menu text color = white
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
                                 uiOutput("test_home"),
                                 uiOutput("test_home2"),
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
feeding_charts <- tabItem(
  tabName = "feeding_charts",
  fluidRow(
    tabBox(title = "Infant Feeding",
           # The id lets us use input$tabset00 on the server to find the current tab
           id = "tabset01",
           width = 12,
           height = "25px",
           
           
           fluidRow(
             box(width = 5,
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
             box(width = 5,
                 checkboxGroupButtons(
                   inputId = "feeding_type",
                   label = "Select which types of breastfeeding statistics to show:",
                   choiceNames = c("Exclusively breastfed", "Overall breastfed", "Ever breastfed"),
                   choiceValues = c("pc_excl", "pc_overall", "pc_ever"),
                   selected = c("pc_excl", "pc_overall", "pc_ever"),
                   status = "primary",
                   direction = "vertical",
                   justified = TRUE,
                   checkIcon = list(
                     yes = icon("square-check") |> rem_aria_label(),
                     no = icon("square") |> rem_aria_label())
                 )
             ),
             box(width = 2, solidHeader = TRUE,
                 downloadButton("", 
                                "Download data",
                                icon = shiny::icon("download") |> rem_aria_label()
                 ) 
             )
           ),
           
           
           ###Runcharts ----
           tabPanel(title = "Runcharts",
                    fluidRow(
                      box(width = 5,
                          uiOutput("geog_level_select_feeding")
                      ),
                      box(width = 4, solidHeader = TRUE,
                          uiOutput("geog_select_feeding")
                      ),
                      box(width = 3, solidHeader = TRUE
                      )
                    ), #fluidRow
                    
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          textOutput("feeding_perc_title"),
                          plotlyOutput("feeding_perc_plotly", height = "300px")
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
                          plotlyOutput("feeding_numbers_plotly", height = "300px")
                      )
                    )
           ), #tabPanel ("Runcharts")
           
           
           
           ###Comparisons ----
           tabPanel(title = "Comparisons",
                    fluidRow(
                      box(width = 4,
                          uiOutput("geog_comparison_level_select_feeding")
                          # radioGroupButtons(
                          #   inputId = "geog_comparison_level_feeding",
                          #   label = "Select geography level to compare:",
                          #   choiceNames = list("Health Board", "Council Area"),
                          #   choiceValues = list("NHS", "HSCP"),
                          #   selected = "NHS",
                          #   status = "primary",
                          #   justified = TRUE,
                          #   checkIcon = list(
                          #     yes = icon("circle-check") |> rem_aria_label(),
                          #     no = icon("circle") |> rem_aria_label())
                          # )
                      ),
                      box(width = 8, solidHeader = TRUE,
                          uiOutput("geog_comparison_select_feeding")
                      )
                    ),
                    
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          textOutput("feeding_comparison_title"),
                          plotOutput("feeding_comparison_plot", height = "600px"))
                    )
           )
           
           
    ) # tabBox ("Infant Feeding")
  )
)


###About this indicator ----
feeding_about <- tabItem(
  tabName = "feeding_about",
  fluidRow(
    tabBox(title = "About this indicator",
           # The id lets us use input$tabset00 on the server to find the current tab
           id = "tabset01",
           width = 12,
           height = "25px",
           tabPanel(title = "About this indicator",
                    
                    fluidRow(
                      box(title = "Why measure this?",
                          width = 5,
                          uiOutput("test_feeding"),
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
           ) #tabPanel ("About this indicator")
    ) #tabBox
  ) #fluidRow
) #tabItem




##Child Development ----
development_charts <- tabItem(
  tabName = "development_charts",
  fluidRow(
    tabBox(title = "Child Development",
           # The id lets us use input$tabset00 on the server to find the current tab
           id = "tabset02",
           width = 12,
           height = "25px",
           
           fluidRow(
             box(width = 5,
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
             box(width = 5, solidHeader = TRUE
             ),
             box(width = 2, solidHeader = TRUE,
                 downloadButton("", 
                                "Download data",
                                icon = shiny::icon("download") |> rem_aria_label()
                 ) 
             )
           ), #fluidRow
           
           
           ###Runcharts ----
           tabPanel(title = "Runcharts",
                    fluidRow(
                      box(width = 5,
                          uiOutput("geog_level_select_development")
                      ),
                      box(width = 5, solidHeader = TRUE,
                          uiOutput("geog_select_development")
                      ),
                      box(width = 2, solidHeader = TRUE
                      ) 
                    ), #fluidRow
                    
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          textOutput("development_percentage_concern_title"),
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
                          plotlyOutput("development_numbers_plotly", height = "300px")
                      )
                    ) #fluidRow
           ), #tabPanel ("Runcharts")
           
           
           
           ###Comparisons ----
           tabPanel(title = "Comparisons",
                    fluidRow(
                      box(width = 4,
                          uiOutput("geog_comparison_level_select_development")
                          # radioGroupButtons(
                          #   inputId = "geog_comparison_level_development",
                          #   label = "Select geography level to compare:",
                          #   choiceNames = list("Health Board", "Council Area"),
                          #   choiceValues = list("NHS", "HSCP"),
                          #   selected = "NHS",
                          #   status = "primary",
                          #   justified = TRUE,
                          #   checkIcon = list(
                          #     yes = icon("circle-check") |> rem_aria_label(),
                          #     no = icon("circle") |> rem_aria_label())
                          # )
                      ),
                      box(width = 8, solidHeader = TRUE,
                          uiOutput("geog_comparison_select_development")
                      )
                    ),
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          textOutput("development_comparison_title"),
                          plotOutput("development_comparison_plot", height = "600px"))
                    )
           ),
           
           ###Domains ----
           tabPanel(title = "Domains",
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
                          plotlyOutput("development_concerns_by_domain_plotly", height = "300px")
                      )
                    )
           ),
           
           
           ###SIMD ----
           tabPanel(title = "SIMD",
                    fluidRow(
                      box(width = 12,
                          checkboxGroupButtons(
                            inputId = "simd_levels",
                            label = "Select which SIMD quintiles to show. This 
                            scale ranges from 1 being the most deprived to 5 
                            being the least deprived.",
                            choices = c(1:5),
                            selected = c(1, 5),
                            status = "primary",
                            checkIcon = list(
                              yes = icon("square-check") |> rem_aria_label(),
                              no = icon("square") |> rem_aria_label())
                          ),
                          textOutput("development_concerns_by_simd_title"),
                          plotlyOutput("development_concerns_by_simd_plotly", height = "300px")
                      )
                    )
           )
           
    ) # tabBox ("Child Development")
  )
)

###About this indicator ----
development_about <- tabItem(
  tabName = "development_about",
  fluidRow(
    tabBox(title = "About this indicator",
           # The id lets us use input$tabset00 on the server to find the current tab
           id = "tabset01",
           width = 12,
           height = "25px",
           tabPanel(title = "About this indicator",
                    
                    fluidRow(
                      box(title = "Why measure this?",
                          width = 5, 
                          uiOutput("test_dev"),
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
    ) #tabBox
  ) #fluidRow
) #tabItem



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
             feeding_charts,
             feeding_about,
             development_charts,
             development_about)
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
  output$geog_level_select_feeding <- renderUI({
    radioGroupButtons(
      inputId = "geog_level_chosen_feeding",
      label = "Select geography level to display:",
      choices = c("All Scotland", "Health Board", "Council Area"),
      selected = selected$geog_level,
      justified = TRUE,
      status = "primary",
      checkIcon = list(
        yes = icon("circle-check") |> rem_aria_label(),
        no = icon("circle") |> rem_aria_label())
    )
  })
  
  output$geog_level_select_development <- renderUI({
    radioGroupButtons(
      inputId = "geog_level_chosen_development",
      label = "Select geography level to display:",
      choices = c("All Scotland", "Health Board", "Council Area"),
      selected = selected$geog_level,
      justified = TRUE,
      status = "primary",
      checkIcon = list(
        yes = icon("circle-check") |> rem_aria_label(),
        no = icon("circle") |> rem_aria_label())
    )
  })
  
  
  
  
  output$geog_select_feeding <- renderUI({
    if (!("All Scotland" %in% selected$geog_level)) {
      pickerInput(
        inputId = "geog_chosen_feeding",
        label = paste0("Select ", selected$geog_level, ":"),
        choices = geog_choices(),
        selected = selected$geog,
        choicesOpt = list(
          style = rep("color: #3F3685;", # PHS-purple text
                      length(geog_choices()))
        ),
        options = list(
          size = 10)
      )
    }
  })
  
  output$geog_select_development <- renderUI({
    if (!("All Scotland" %in% selected$geog_level)) {
      pickerInput(
        inputId = "geog_chosen_development",
        label = paste0("Select ", selected$geog_level, ":"),
        choices = geog_choices(),
        selected = selected$geog,
        choicesOpt = list(
          style = rep("color: #3F3685;", # PHS-purple text
                      length(geog_choices()))
        ),
        options = list(
          size = 10)
      )
    }
  })
  
  
  
  
  
  
  #Selected Values ----
  selected <- reactiveValues(geog_level = "All Scotland",
                             geog_comparison_level = "NHS")
  
  
  observeEvent(input$geog_level_chosen_feeding, 
               selected$geog_level <- input$geog_level_chosen_feeding)
  
  observeEvent(input$geog_level_chosen_development, 
               selected$geog_level <- input$geog_level_chosen_development)
  
  geog_choices <- reactive({
    if(length(selected$geog_level) == 1) {
      switch(selected$geog_level,
             "All Scotland" = "Scotland",
             "Health Board" = HBnames,
             "Council Area" = HSCPnames)
    }
  })
  
  
  observeEvent(input$geog_chosen_feeding,
               selected$geog <- input$geog_chosen_feeding)
  
  observeEvent(input$geog_chosen_development,
               selected$geog <- input$geog_chosen_development)
  
  geog_final <- reactive({
    if (length(selected$geog_level) == 1) {
      switch(selected$geog_level,
             "All Scotland" = "All Scotland",
             "Health Board" = selected$geog,
             "Council Area" = selected$geog)
    }
  })
  
  
  
  
  observeEvent(input$geog_comparison_level_feeding,
               selected$geog_comparison_level <- input$geog_comparison_level_feeding)
  
  observeEvent(input$geog_comparison_level_development,
               selected$geog_comparison_level <- input$geog_comparison_level_development)
  
  comparison_choices <- reactive({
    if(length(selected$geog_comparison_level) == 1) {
      c("All Scotland", 
        switch(selected$geog_comparison_level,
               "NHS" = HBnames,
               "HSCP" = HSCPnames))
    }
  })
  
  
  
  
  
  observeEvent(input$geog_comparison_list_feeding,
               selected$geog_comparison_list <- input$geog_comparison_list_feeding)
  
  observeEvent(input$geog_comparison_list_development,
               selected$geog_comparison_list <- input$geog_comparison_list_development)
  
  observeEvent(input$geog_comparison_level_feeding,
               selected$geog_comparison_list <- comparison_choices())
  
  observeEvent(input$geog_comparison_level_development,
               selected$geog_comparison_list <- comparison_choices())
  
  
  
  
  
  
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
  
  
 
  
  feeding_type_name <- reactive({
    if(length(input$feeding_type) >= 1) {
      c(
        switch(input$feeding_type[1],
               "pc_excl" = "exclusively breastfed",
               "pc_overall" = "overall breastfed",
               "pc_ever" = "ever breastfed"),
        switch(input$feeding_type[2],
               "pc_excl" = "exclusively breastfed",
               "pc_overall" = "overall breastfed",
               "pc_ever" = "ever breastfed"),
        switch(input$feeding_type[3],
               "pc_excl" = "exclusively breastfed",
               "pc_overall" = "overall breastfed",
               "pc_ever" = "ever breastfed")
      )
    }
  })
  
  feeding_type_numbers <- reactive({
    if(length(input$feeding_type) >= 1) {
      c(
        switch(input$feeding_type[1],
               "pc_excl" = "exclusive_bf",
               "pc_overall" = "overall_bf",
               "pc_ever" = "ever_bf"),
        switch(input$feeding_type[2],
               "pc_excl" = "exclusive_bf",
               "pc_overall" = "overall_bf",
               "pc_ever" = "ever_bf"),
        switch(input$feeding_type[3],
               "pc_excl" = "exclusive_bf",
               "pc_overall" = "overall_bf",
               "pc_ever" = "ever_bf")
      )
    }
  })
  
  
  
  
  
  #.----
  #Infant Feeding ----
  
  
  ##Percentage Plot ----
  output$feeding_perc_title <- renderText({
    paste0("Percentage of children recorded as ",
           str_flatten_comma(feeding_type_name(), last = ", and "),
           " at ",
           feeding_data_name())
  })
  
  output$feeding_perc_plotly <- renderPlotly({
    dashboard_data[[input$feeding_data]] |>
      filter(geography %in% geog_final()) |>
      pivot_longer(cols = pc_excl:pc_ever,
                   names_to = "category",
                   values_to = "number") |>
      filter(category %in% input$feeding_type) |>
      plot_ly(x = ~ month_review,
              y = ~ number,
              type = "scatter",
              mode = "lines",
              linetype = ~ category) |>
      config(displayModeBar = FALSE) |>
      layout(yaxis = list(range = c(0,100), 
                          title = list(text = "% of reviews with feeding data")),
             xaxis = list(title = list(text = "Month of review")))
  })
  
  
  
  
  
  
  
  
  ##Numbers Plot ----
  output$feeding_numbers_title <- renderText({
    paste0("Number of ",
           feeding_data_name(),
           "s; ",
           str_extract(feeding_data_name(), " \\w*$"),
           "s with data on infant feeding recorded; and children recorded as being breastfed")
  })
  
  
  output$feeding_numbers_plotly <- renderPlotly({
    dashboard_data[[input$feeding_data]] |>
      filter(geography %in% geog_final()) |>
      pivot_longer(cols = no_reviews:ever_bf,
                   names_to = "category",
                   values_to = "number") |>
      filter(category %in% c("no_reviews", "no_valid_reviews", feeding_type_numbers())) |>
      plot_ly(x = ~ month_review,
              y = ~ number,
              type = "scatter",
              mode = "lines",
              linetype = ~ category) |>
      config(displayModeBar = FALSE)
  })
  
  
  
  
  
  
  
  
  ##Comparisons ----
  output$feeding_comparison_title <- renderText({
    paste0("Comparing Health Boards for percentage exclusively/overall/ever breastfed at ",
           feeding_data_name())
  })
  
  output$feeding_comparison_plot <- renderPlot({
    if(length(input$geog_comparison_list_feeding) >= 1) {
      dashboard_data[[input$feeding_data]] |>
        filter(geography %in% input$geog_comparison_list_feeding) |>
        pivot_longer(cols = pc_excl:pc_ever,
                     names_to = "category",
                     values_to = "number") |>
        filter(category %in% input$feeding_type) |>
        ggplot(aes(x = month_review, y = number, color = category)) +
        geom_line() +
        facet_wrap(~ geography)
    }
  })
  
  
  output$geog_comparison_level_select_feeding <- renderUI({
    radioGroupButtons(
      inputId = "geog_comparison_level_feeding",
      label = "Select geography level to compare:",
      choiceNames = list("Health Board", "Council Area"),
      choiceValues = list("NHS", "HSCP"),
      selected = selected$geog_comparison_level,
      status = "primary",
      justified = TRUE,
      checkIcon = list(
        yes = icon("circle-check") |> rem_aria_label(),
        no = icon("circle") |> rem_aria_label())
    )
  })
  
  output$geog_comparison_select_feeding <- renderUI({
    pickerInput(
      inputId = "geog_comparison_list_feeding",
      label = paste0("Select ", 
                     switch(selected$geog_comparison_level, "NHS" = "Health Board", "HSCP" = "Council Area"),
                     "s to display:"),
      choices = comparison_choices(),
      selected = selected$geog_comparison_list,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  
  
  
  # Child Development ----
  
  
  
  
  
  
  
  ##Percentage Concern ----
  output$development_percentage_concern_title <- renderText({
    paste0("Percentage of children with one or more developmental concerns ",
           "recorded at the ", 
           development_data_name())
  })
  
  
  output$development_percentage_concern_plotly <- renderPlotly({
    create_runchart_plotly(dashboard_data[[input$development_data]], geog_final(), "pc_1_plus") |>
      layout(yaxis = list(range = c(0,40)))
  })
  
  
  
  ##Number of Reviews ----
  output$development_numbers_title <- renderText({
    paste0("Number of ",
           development_data_name(),
           "s; reviews with full meaningful data on child development recorded; ",
           "and children with 1 or more developmental concerns recorded")
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
              linetype = ~ category) |>
      config(displayModeBar = FALSE)
  })
  
  
  
  
  ##Concerns by Domain ----
  output$development_concerns_by_domain_title <- renderText({
    paste0("Percentage of ",
           development_data_name(),
           "s with a new or previous concern recorded by developmental domain")
  })
  
  output$development_concerns_by_domain_plotly <- renderPlotly({
    dashboard_data[[paste0(input$development_data, "_domains")]] |>
      pivot_longer(cols = slc_perc:hearing_perc,
                   names_to = "category",
                   values_to = "number") |>
      filter(category %in% input$domains_selected) |>
      plot_ly(x = ~ month_review,
              y = ~ number,
              type = "scatter",
              mode = "lines",
              linetype = ~ category) |>
      config(displayModeBar = FALSE)
  })
  
  
  
  ##Concerns by SIMD Quintile ----
  output$development_concerns_by_simd_title <- renderText({
    paste0("Percentage of children with 1 or more developmental concerns recorded at the ",
           development_data_name(),
           " by SIMD deprivation quintile")
  })
  
  output$development_concerns_by_simd_plotly <- renderPlotly({
    dashboard_data[[paste0(input$development_data, "_simd")]] |>
      filter(simd %in% input$simd_levels) |>
      plot_ly(x = ~ month_review,
              y = ~ pc_1_plus,
              type = "scatter",
              mode = "lines",
              linetype = ~ simd) |>
      config(displayModeBar = FALSE)  |>
      layout(yaxis = list(range = c(0,30)))
  })
  
  
  
  
  
  
  ##Comparisons ----
  output$development_comparison_title <- renderText({
    paste0("Comparing Health Boards for percentage exclusively/overall/ever breastfed at ",
           development_data_name())
  })
  
  output$development_comparison_plot <- renderPlot({
    if(length(input$geog_comparison_list_development) >= 1) {
      dashboard_data[[input$development_data]] |>
        filter(geography %in% input$geog_comparison_list_development) |>
        ggplot(aes(x = month_review, y = pc_1_plus)) +
        geom_line() +
        facet_wrap(~ geography)
    }
  })
  
  
  output$geog_comparison_level_select_development <- renderUI({
    radioGroupButtons(
      inputId = "geog_comparison_level_development",
      label = "Select geography level to compare:",
      choiceNames = list("Health Board", "Council Area"),
      choiceValues = list("NHS", "HSCP"),
      selected = selected$geog_comparison_level,
      status = "primary",
      justified = TRUE,
      checkIcon = list(
        yes = icon("circle-check") |> rem_aria_label(),
        no = icon("circle") |> rem_aria_label())
    )
  })
  
  output$geog_comparison_select_development <- renderUI({
    pickerInput(
      inputId = "geog_comparison_list_development",
      label = paste0("Select ", 
                     switch(selected$geog_comparison_level, "NHS" = "Health Board", "HSCP" = "Council Area"),
                     "s to display:"),
      choices = comparison_choices(),
      selected =  selected$geog_comparison_list,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  
  
  
  
  
  
  #.----
  #Testing! ----
  output$testing <- renderPrint({
    str_view(c(paste0("A ", input$geog_level_chosen_feeding),
               paste0("B ", input$geog_level_chosen_development),
               paste0("C ", input$geog_chosen_feeding),
               paste0("D ", input$geog_chosen_development),
               paste0("H ", selected$geog_level),
               paste0("I ", selected$geog),
               paste0("J ", geog_final()),
               paste0("G ", input$geog_comparison_level_feeding),
               paste0("H ", input$geog_comparison_level_development),
               paste0("I ", input$feeding_type),
               paste0("J ", feeding_type_numbers()),
               selected$geog_comparison_list
    ))
  })
  
  
}





shinyApp(ui, server)
