

#Setup ----
##libraries ----
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(ggplot2)
library(readr)
library(forcats)

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
#library(shinya11y)

library(phsstyles)
library(phsmethods)


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


# paired list of developmental domains and their variable names
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
                  "13-15m", "27-30m", "4-5y",
                  "13-15m_Domains", "27-30m_Domains", "4-5y_Domains",
                  "13-15m_SIMD", "27-30m_SIMD", "4-5y_SIMD"),
                ".xlsx")

#read in each file and put them in a list
dashboard_data <- map(files, \(x) {
  data <- read.xlsx(
    paste0("/PHI_conf/ChildHealthSurveillance/Topics/ChildHealth/Projects/20200707-Covid19-Breastfeeding-and-ChildDevelopment/Output/",
           x)
  ) |> 
    
    #make the data we read in tidier
    tibble() |> 
    janitor::clean_names() |>
    
    #get rid of the columns and rows that aren't being used
    select( -any_of(c("hscp2019_code", "pc_valid", "pc_meaningful",
                      paste0("no_", str_remove(domains$variable, "_perc"))))) |>
    filter(!(geography %in% c("Argyll & Clyde", "Unknown HSCP"))) |>
    
    #properly format the geography and month_review varibales
    mutate(geography = case_when(geography == "Scotland" ~ "All Scotland",
                                 str_ends(geography, "HSCP", negate = TRUE) ~ paste0("NHS ", geography),
                                 TRUE ~ geography) |> factor(levels = c("All Scotland", HBnames, HSCPnames)),
           month_review = as.Date(month_review, origin = ymd("1900-01-01")-2)
    )
  
  #inside the SIMD files, get rid of the rows that don't have SIMD data
  if(str_ends(x, "SIMD.xlsx")) {
    data <- data |>
      filter(simd %in% as.character(1:5)) |>
      mutate(simd = fct(simd))
  }
  
  return(data)
})

#name each of the files being read in
names(dashboard_data) <- c("feeding_first_visit", "feeding_6_8_week_review",
                           "development_13_15_month_review", "development_27_30_month_review", "development_4_5_year_review",
                           "development_13_15_month_review_domains", "development_27_30_month_review_domains", "development_4_5_year_review_domains",
                           "development_13_15_month_review_simd", "development_27_30_month_review_simd", "development_4_5_year_review_simd")




#list all columns that contain measures that are numbers
no_cats <- map(dashboard_data, \(x){
  names(x)[str_ends(names(x), "bf") |
             str_starts(names(x), "no") |
             names(x) == "concerns_1_plus"]
}) |>
  list_c()

#list all columns that contain measures that are percentages
perc_cats <- map(dashboard_data, \(x){
  names(x)[str_ends(names(x), "perc") |
             str_starts(names(x), "pc")]
}) |>
  list_c()




#aggregate data to be by quarter rather than by month for the child development datasets
dashboard_data <- map(names(dashboard_data), \(x) {
  data <- dashboard_data
  if(str_starts(x, "development")) {
    data[[x]] <- data[[x]] |>
      mutate(quarter = fct_reorder(paste(str_extract(qtr(month_review), "^\\w*"),
                                         str_extract(qtr(month_review), "\\d*$")), 
                                   month_review)) |>
      filter((quarter != last(levels(quarter))) &
               !(geography == "NHS Greater Glasgow & Clyde" & quarter %in% c("January 2019", "April 2019"))) |>
      group_by(pick(any_of(c("geography", "quarter", "simd")))) |>
      summarise(across(any_of(perc_cats),
                       \(x) {sum(x * no_reviews) / sum(no_reviews)}),
                across(any_of(no_cats),
                       sum),
                .groups = "drop")
  }
  return(data[[x]])
})

#need to reestablish the names of the datasets after making the above aggregation
names(dashboard_data) <- c("feeding_first_visit", "feeding_6_8_week_review",
                           "development_13_15_month_review", "development_27_30_month_review", "development_4_5_year_review",
                           "development_13_15_month_review_domains", "development_27_30_month_review_domains", "development_4_5_year_review_domains",
                           "development_13_15_month_review_simd", "development_27_30_month_review_simd", "development_4_5_year_review_simd")





#the data was being pivoted within most of the plots in the dashboard, so this
#does the pivoting in a generalised way beforehand to reduce processing time
pivoted_data <- map(dashboard_data, \(x) {
  x |>
    pivot_longer(cols = any_of(c(no_cats, perc_cats)),
                 names_to = "category",
                 values_to = "measure") |>
    mutate(category = fct(category))
})








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








#mytheme ----

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



#.----
#body ----

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
                                 
                                 p("This dashboard has been developed by Public Health Scotland to 
                                   display data on child health throughout Scotland. It follows on
                                   from the ",
                                   tags$a(
                                     href = "https://scotland.shinyapps.io/phs-covid-wider-impact/",
                                     tags$u("COVID Wider Impacts dashboard"),
                                     class = "externallink", target = "_blank"),
                                   ", which updated for the last time in September 2023. It also exists
                                   as a sister dashboard to the ",
                                   tags$a(
                                     href = "https://scotland.shinyapps.io/phs-pregnancy-births-neonatal/",
                                     tags$u("Scottish Pregnancy, Births and Neonatal Data (SPBaND)"),
                                     class = "externallink", target = "_blank"),
                                   " dashboard."
                                 ),
                                 
                                 p("This dashboard contains two core child health indicators - infant feeding
                                   and child development. Each of these indicators can be accessed via the
                                   sidebar on the left-hand side of this dashboard. Each indicator has two 
                                   pages listed in drop-down lists within the sidebar; one with charts 
                                   displaying the data on that indicator, and one \"About this indicator\"
                                   tab which gives specific information and context for that indicator."),
                                 
                                 p("The \"Charts\" page for each indicator has several tabs displaying the 
                                   data in different ways. Firstly, the \"Runcharts\" tab allows users
                                   to see data displays for a singular region. Secondly, the \"Comparisons\" 
                                   tab allows users to see data plots for different regions alongside each 
                                   other. Users can pick what region(s) to display in each of these tabs,
                                   with the data being available to display by Council Area, by Health Board,
                                   or for all Scotland."),
                                 
                                 p("The Child Development charts include two more tabs in which the data 
                                   are broken down by developmental domain and by SIMD quintile. The data 
                                   for these breakdowns are aggregated for all of Scotland, so no geographical 
                                   options are provided on these tabs."),
                                 
                                 p("The top of each tab also gives several options for users to choose what 
                                   data is being displayed. The afformentioned geographical options are found
                                   here, as well as some options specific to each indicator. These specific
                                   options are described in those indicator's \"About this indicator\" pages."),
                                 
                                 p("Lastly, each indicator contains a download button in the top left corner
                                   that looks like this: ",
                                   tags$imag(src = "download_button.png",
                                             alt = "Download button image"), 
                                   ". Clicking this button will download an excel spreadsheet containing the 
                                   data used to make the plots for that indicator. The download buttons on
                                   each tab within an indicator will provide the same data download file, as
                                   this singular file contains all the data for that indicator.")
                                 
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
           id = "feeding_charts_tab",
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
                   justified = TRUE,
                   status = "primary",
                   checkIcon = list(
                     yes = icon("circle-check") |> rem_aria_label(),
                     no = icon("circle") |> rem_aria_label())
                 ) #radioGroupButtons
             ), #box
             box(width = 5,
                 uiOutput("feeding_type_select")
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
                          h4(textOutput("feeding_perc_title"), style = "text-align: center;"),
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
                          h4(textOutput("feeding_numbers_title"), style = "text-align: center;"),
                          plotlyOutput("feeding_numbers_plotly", height = "300px")
                      )
                    )
           ), #tabPanel ("Runcharts")
           
           
           
           ###Comparisons ----
           tabPanel(title = "Comparisons",
                    fluidRow(
                      box(width = 4,
                          uiOutput("geog_comparison_level_select_feeding")
                      ),
                      box(width = 8, solidHeader = TRUE,
                          uiOutput("geog_comparison_select_feeding")
                      )
                    ),
                    
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          h4(textOutput("feeding_comparison_title"), style = "text-align: center;"),
                          plotlyOutput("feeding_comparison_plotly", height = "600px"))
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
           id = "feeding_about_tab",
           width = 12,
           height = "25px",
           tabPanel(title = "About this indicator",
                    
                    fluidRow(
                      box(title = "Why measure this?",
                          width = 5,
                          p("Encouraging and supporting breastfeeding is an important 
                            public health activity. There is strong evidence that 
                            breastfeeding protects the health of children and mothers.  
                            The information is collected at Health Visitor reviews 
                            of children at around 10-14 days (First Visit) and at 
                            6-8 weeks."),
                          
                          p("Health visitors gather this information via three different
                            metrics; whether the baby was exclusively breastfed, 
                            overall breastfed, and ever breastfed. These are defined 
                            in the \"Data source and definitions\" column. The \"Charts\" tab
                            gives options to toggle on and off each of these three 
                            metrics to allow users to compare them against each other.")
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
           id = "development_charts_tab",
           width = 12,
           height = "25px",
           
           fluidRow(
             box(width = 7,
                 radioGroupButtons(
                   inputId = "development_data",
                   label = "Select the data you want to explore:",
                   choiceNames = list("13-15 month review", "27-30 month review", "4-5 year review"),
                   choiceValues = list("development_13_15_month_review", "development_27_30_month_review", "development_4_5_year_review"),
                   selected = "development_13_15_month_review",
                   justified = TRUE,
                   status = "primary",
                   checkIcon = list(
                     yes = icon("circle-check") |> rem_aria_label(),
                     no = icon("circle") |> rem_aria_label())
                 ) #radioGroupButtons
             ), #box
             box(width = 3, solidHeader = TRUE
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
                          h4(textOutput("development_percentage_concern_title"), style = "text-align: center;"),
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
                          h4(textOutput("development_numbers_title"), style = "text-align: center;"),
                          plotlyOutput("development_numbers_plotly", height = "300px")
                      )
                    ) #fluidRow
           ), #tabPanel ("Runcharts")
           
           
           
           ###Comparisons ----
           tabPanel(title = "Comparisons",
                    fluidRow(
                      box(width = 4,
                          uiOutput("geog_comparison_level_select_development")
                      ),
                      box(width = 8, solidHeader = TRUE,
                          uiOutput("geog_comparison_select_development")
                      )
                    ),
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          h4(textOutput("development_comparison_title"), style = "text-align: center;"),
                          plotlyOutput("development_comparison_plotly", height = "600px"))
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
                          h4(textOutput("development_concerns_by_domain_title"), style = "text-align: center;"),
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
                          h4(textOutput("development_concerns_by_simd_title"), style = "text-align: center;"),
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
           id = "development_about_tab",
           width = 12,
           height = "25px",
           tabPanel(title = "About this indicator",
                    
                    fluidRow(
                      box(title = "Why measure this?",
                          width = 5,
                          p("Early child development is influenced by both biological
                            factors (such as being born premature) and environmental 
                            factors (such as the parenting and opportunities for 
                            play and exploration children receive). Problems with
                            early child development are important as they are strongly 
                            associated with long-term health, educational, and wider
                            social difficulties."),
                          
                          p("Detecting developmental problems early provides the
                            best opportunity to support children and families to
                            improve outcomes. There is good evidence that parenting
                            support and enriched early learning opportunities can 
                            improve outcomes for children with, or at risk of, developmental
                            delay. There is also increasing evidence that intensive 
                            early interventions for children with serious developmental
                            problems can also improve outcomes."),
                          
                          p("All children in Scotland are offered the child health 
                            programme which includes a series of child health reviews. 
                            Health Visitors usually provide reviews for preschool 
                            children, including an assessment of children’s development
                            at 13-15 months, 27-30 months and 4-5 years. These reviews
                            involve asking parents about their child’s progress, 
                            carefully observing the child, and supporting parents 
                            to complete a structured questionnaire about the child’s
                            development. At the end of the review Health Visitors 
                            record whether they have any concerns about each area 
                            of the child’s development.")
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








#.----
server <- function(input, output, session) {
  
  #Geography Control ----
  #takes geography level choice from infant feeding tab
  output$geog_level_select_feeding <- renderUI({
    radioGroupButtons(
      inputId = "geog_level_chosen_feeding",
      label = "Select geography level to display:",
      choices = c("All Scotland", "Health Board", "Council Area"),
      selected = selected$geog_level, #syncs this with the equivalent choice on the child development tab
      justified = TRUE,
      status = "primary",
      checkIcon = list(
        yes = icon("circle-check") |> rem_aria_label(),
        no = icon("circle") |> rem_aria_label())
    )
  })
  
  #takes geography level choice from child development tab
  output$geog_level_select_development <- renderUI({
    radioGroupButtons(
      inputId = "geog_level_chosen_development",
      label = "Select geography level to display:",
      choices = c("All Scotland", "Health Board", "Council Area"),
      selected = selected$geog_level, #syncs this with the equivalent choice on the infant feeding tab
      justified = TRUE,
      status = "primary",
      checkIcon = list(
        yes = icon("circle-check") |> rem_aria_label(),
        no = icon("circle") |> rem_aria_label())
    )
  })
  
  
  
  #takes health board / council area choice from infant feeding tab
  output$geog_select_feeding <- renderUI({
    if (!("All Scotland" %in% selected$geog_level)) {
      pickerInput(
        inputId = "geog_chosen_feeding",
        label = paste0("Select ", selected$geog_level, ":"),
        choices = geog_choices(), #reactive element that switches options based on geog level choice
        selected = selected$geog, #syncs this with the equivalent choice on the child development tab
        choicesOpt = list(
          style = rep("color: #3F3685;", # PHS-purple text
                      length(geog_choices())) 
        ),        #makes sure colours work properly
        options = list(
          size = 10) #display 10 options at a time
      )
    }
  })
  
  #takes health board / council area choice from child development tab
  output$geog_select_development <- renderUI({
    if (!("All Scotland" %in% selected$geog_level)) {
      pickerInput(
        inputId = "geog_chosen_development",
        label = paste0("Select ", selected$geog_level, ":"),
        choices = geog_choices(), #reactive element that switches options based on geog level choice
        selected = selected$geog, #syncs this with the equivalent choice on the infant feeding tab
        choicesOpt = list(
          style = rep("color: #3F3685;", # PHS-purple text
                      length(geog_choices())) 
        ),        #makes sure colours work properly
        options = list(
          size = 10) #display 10 options at a time
      )
    }
  })
  
  #takes geography level choice from infant feeding comparisons tab
  output$geog_comparison_level_select_feeding <- renderUI({
    radioGroupButtons(
      inputId = "geog_comparison_level_feeding",
      label = "Select geography level to compare:",
      choices = list("Health Board", "Council Area"),
      selected = selected$geog_comparison_level, #syncs this with the equivalent choice on the child development tab
      status = "primary",
      justified = TRUE,
      checkIcon = list(
        yes = icon("circle-check") |> rem_aria_label(),
        no = icon("circle") |> rem_aria_label())
    )
  })
  
  #takes geography level choice from child development comparisons tab
  output$geog_comparison_level_select_development <- renderUI({
    radioGroupButtons(
      inputId = "geog_comparison_level_development",
      label = "Select geography level to compare:",
      choices = list("Health Board", "Council Area"),
      selected = selected$geog_comparison_level, #syncs this with the equivalent choice on the infant feeding tab
      status = "primary",
      justified = TRUE,
      checkIcon = list(
        yes = icon("circle-check") |> rem_aria_label(),
        no = icon("circle") |> rem_aria_label())
    )
  })
   
  #takes health board / council area choices from infant feeding comparisons tab
  output$geog_comparison_select_feeding <- renderUI({
    pickerInput(
      inputId = "geog_comparison_list_feeding",
      label = paste0("Select ", 
                     selected$geog_comparison_level,
                     "s to display:"),
      choices = comparison_choices(), #reactive element that switches options based on geog level choice
      selected = selected$geog_comparison_list, #syncs this with the equivalent choice on the child development tab
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  #takes health board / council area choices from child development comparisons tab
  output$geog_comparison_select_development <- renderUI({
    pickerInput(
      inputId = "geog_comparison_list_development",
      label = paste0("Select ", 
                     selected$geog_comparison_level,
                     "s to display:"),
      choices = comparison_choices(), #reactive element that switches options based on geog level choice
      selected =  selected$geog_comparison_list, #syncs this with the equivalent choice on the infant feeding tab
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  
  #Selected Values ----
  #these exist to sync choices where they appear on both indicators' pages
  
  #initialise the "selected" variables
  selected <- reactiveValues(geog_level = "All Scotland",
                             geog_comparison_level = "Health Board")
  
  #update geography level each time it is changed on either page
  observeEvent(input$geog_level_chosen_feeding, 
               selected$geog_level <- input$geog_level_chosen_feeding)
  
  observeEvent(input$geog_level_chosen_development, 
               selected$geog_level <- input$geog_level_chosen_development)
  
  
  #vary choices based on whether health board or council area is selected
  geog_choices <- reactive({
    if(length(selected$geog_level) == 1) {
      switch(selected$geog_level,
             "All Scotland" = "Scotland",
             "Health Board" = HBnames,
             "Council Area" = HSCPnames)
    }
  })
  
  
  #update health board / council area choice each time it is changed on either page
  observeEvent(input$geog_chosen_feeding,
               selected$geog <- input$geog_chosen_feeding)
  
  observeEvent(input$geog_chosen_development,
               selected$geog <- input$geog_chosen_development)
  
  
  #combine the selected geography values to output what geography to filter for in plots
  geog_final <- reactive({
    if (length(selected$geog_level) == 1) {
      switch(selected$geog_level,
             "All Scotland" = "All Scotland",
             "Health Board" = selected$geog,
             "Council Area" = selected$geog)
    }
  })
  
  
  
  #update geogrpahy level choices within the comparison tabs
  observeEvent(input$geog_comparison_level_feeding,
               selected$geog_comparison_level <- input$geog_comparison_level_feeding)
  
  observeEvent(input$geog_comparison_level_development,
               selected$geog_comparison_level <- input$geog_comparison_level_development)
  
  #list what comparison choices are available based on chosen geography level
  comparison_choices <- reactive({
    if(length(selected$geog_comparison_level) == 1) {
      c("All Scotland", 
        switch(selected$geog_comparison_level,
               "Health Board" = HBnames,
               "Council Area" = HSCPnames))
    }
  })
  
  
  
  
  #update list of health boards / council areas to compare each time it is changed on either tab
  observeEvent(input$geog_comparison_list_feeding,
               selected$geog_comparison_list <- input$geog_comparison_list_feeding)
  
  observeEvent(input$geog_comparison_list_development,
               selected$geog_comparison_list <- input$geog_comparison_list_development)
  
  #reset the selected options to select all whenever the geography level changes
  observeEvent(input$geog_comparison_level_feeding,
               selected$geog_comparison_list <- comparison_choices())
  
  observeEvent(input$geog_comparison_level_development,
               selected$geog_comparison_list <- comparison_choices())
  
  #based on how many health boards / council areas have been selected,
  #this reactive element tells plotly how many rows to put into the comparison grid
  nrows <- reactive({
    n <- length(selected$geog_comparison_list)
    nrows <- ceiling(n / ceiling(sqrt(n)))
    return(nrows)
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
           "development_27_30_month_review" = "27-30 month review",
           "development_4_5_year_review" = "4-5 year review")
  })
  
  
  
  
  
  
  
  
  
  #.----
  #Infant Feeding ----
  
  #"Ever breastfed" variable is only gathered at first visit so we need to exclude it
  #if the 6-8 week review data is selected
  feeding_choices <- reactive({
    switch(input$feeding_data,
           "feeding_first_visit" = c("Ever breastfed", "Overall breastfed", "Exclusively breastfed"), 
           "feeding_6_8_week_review" = c("Overall breastfed", "Exclusively breastfed"))
  })
  
  #takes choice on which options to display from ever, overall and exclusively breastfed
  output$feeding_type_select <- renderUI({
    checkboxGroupButtons(
      inputId = "feeding_type",
      label = "Select which types of breastfeeding statistics to show:",
      choices = feeding_choices(),
      selected = feeding_choices(),
      status = "primary",
      justified = TRUE,
      checkIcon = list(
        yes = icon("square-check") |> rem_aria_label(),
        no = icon("square") |> rem_aria_label())
    )
  })
  
  
  
  ##Percentage Plot ----
  #title of plot changes based on choices
  output$feeding_perc_title <- renderText({
    paste0("Percentage of children recorded as ",
           str_flatten_comma(str_to_lower(input$feeding_type), last = ", and "),
           " at ",
           feeding_data_name())
  })
  
  #wrangles the data to be fed into the plot
  feeding_perc_data <- reactive({
    pivoted_data[[input$feeding_data]] |>
      mutate(category = fct(case_when(category == "pc_ever" ~ "Ever breastfed",
                                      category == "pc_overall" ~ "Overall breastfed",
                                      category == "pc_excl" ~ "Exclusively breastfed",
                                      TRUE ~ category))) |>
      filter(geography %in% geog_final() &
               str_detect(category, " ")) |>
      mutate(category = fct_reorder2(category, month_review, measure)) |>
      filter(category %in% input$feeding_type)
  })
  
  
  #creates the plot to be displayed
  output$feeding_perc_plotly <- renderPlotly({
    
    data <- feeding_perc_data()
    ymax <- min(c(max(data$measure), 100))
    
    plot_ly(data = data,
            x = ~ month_review,
            y = ~ measure,
            type = "scatter",
            mode = "lines",
            color = ~ category,
            text = ~ first(geography),
            hovertemplate = "<b>%{text}</b><br>% of reviews: %{y:.2f}%<br>Month of review: %{x|%B %Y}") |>
      config(displayModeBar = FALSE) |>
      layout(yaxis = list(range = c(0, ymax), 
                          title = list(text = "% of reviews")),
             xaxis = list(title = list(text = "Month of review")),
             legend = list(itemclick = FALSE,
                           itemdoubleclick = FALSE,
                           title = list(text = "Feeding Type"))
      )
  })
  
  
  
  
  
  
  
  
  ##Numbers Plot ----
  #title of plot changes based on choices
  output$feeding_numbers_title <- renderText({
    paste0("Number of ",
           feeding_data_name(),
           "s; ",
           str_extract(feeding_data_name(), " \\w*$"),
           "s with data on infant feeding recorded; and children recorded as being breastfed")
  })
  
  #wrangles the data to be fed into the plot
  feeding_numbers_data <- reactive({
    pivoted_data[[input$feeding_data]] |>
      mutate(category = fct(case_when(category == "ever_bf" ~ "Ever breastfed",
                                      category == "overall_bf" ~ "Overall breastfed",
                                      category == "exclusive_bf" ~ "Exclusively breastfed",
                                      category == "no_reviews" ~ "Reviews",
                                      category == "no_valid_reviews" ~ "Valid reviews",
                                      TRUE ~ category))) |>
      filter(geography %in% geog_final() &
               str_detect(category, " |R")) |>
      mutate(category = fct_reorder2(category, month_review, measure)) |>
      filter(category %in% c("Reviews", "Valid reviews", input$feeding_type))
  })
  
  #creates the plot to be displayed
  output$feeding_numbers_plotly <- renderPlotly({
    
    data <- feeding_numbers_data()
    ymax <- max(data$measure)
    
    plot_ly(data = data,
            x = ~ month_review,
            y = ~ measure,
            type = "scatter",
            mode = "lines",
            color = ~ category,
            text = ~ first(geography),
            hovertemplate = "<b>%{text}</b><br>Number of reviews: %{y}<br>Month of review: %{x|%B %Y}") |>
      config(displayModeBar = FALSE) |>
      layout(yaxis = list(range = c(0,ymax),
                          title = list(text = "Number of reviews")),
             xaxis = list(title = list(text = "Month of review")),
             legend = list(itemclick = FALSE,
                           itemdoubleclick = FALSE,
                           title = list(text = "Number of:"))
      )
  })
  
  
  
  
  
  
  
  
  ##Comparisons ----
  #title of plot changes based on choices
  output$feeding_comparison_title <- renderText({
    paste0("Comparing ", 
           selected$geog_comparison_level, 
           "s for percentage of children recorded as ",
           str_flatten_comma(str_to_lower(input$feeding_type), last = ", and "),
           " at ",
           feeding_data_name())
  })
  
  #wrangles the data to be fed into the plots
  feeding_comparison_data <- reactive({
    pivoted_data[[input$feeding_data]] |>
      mutate(category = fct(case_when(category == "pc_ever" ~ "Ever breastfed",
                                      category == "pc_overall" ~ "Overall breastfed",
                                      category == "pc_excl" ~ "Exclusively breastfed",
                                      TRUE ~ category))) |>
      filter(geography %in% selected$geog_comparison_list &
               str_detect(category, " ")) |>
      mutate(category = fct_reorder2(category, month_review, measure)) |>
      filter(category %in% input$feeding_type)
  })
  
  #creates the plot grid to be displayed
  output$feeding_comparison_plotly <- renderPlotly({
    if(length(selected$geog_comparison_list) >= 1) {
      
      data <- feeding_comparison_data()
      ymax <- min(c(max(data$measure), 100))
      
      data |>      
        group_by(geography) %>%
        do(plots = plot_ly(x = .$month_review,
                           y = .$measure,
                           type = "scatter",
                           mode = "lines",
                           color = .$category,
                           text = first(.$geography),
                           hovertemplate = "<b>%{text}</b><br>% of reviews: %{y:.2f}%<br>Month of review: %{x|%B %Y}",
                           showlegend = (selected$geog_comparison_list[1] %in% .$geography)) |>
             layout(annotations = list(
               x = 0.5,
               y = 1.0,
               text = .$geography,
               xref = "paper",
               yref = "paper",
               xanchor = "center",
               yanchor = "bottom",
               showarrow = FALSE
             ),
             yaxis = list(range = c(0, ymax),
                          title = list(text = "% of reviews")),
             xaxis = list(title = list(text = "Month of review")),
             legend = list(itemclick = FALSE,
                           itemdoubleclick = FALSE,
                           title = list(text = "Feeding Type"))
             )) -> plot_list
      
      subplot(plot_list$plots, nrows = nrows(), shareX = TRUE, shareY = TRUE) |>
        config(displayModeBar = FALSE)
      
    }
  })
  

  
  
  
  
  # Child Development ----

  ##Percentage Concern ----
  #title of plot changes based on choices
  output$development_percentage_concern_title <- renderText({
    paste0("Percentage of children with one or more developmental concerns
           recorded at the ", 
           development_data_name())
  })
  
  #wrangles the data to be fed into the plot
  development_percentage_concern_data <- reactive({
    dashboard_data[[input$development_data]] |>
      filter(geography %in% geog_final())
  })
  
  #creates the plot to be displayed
  output$development_percentage_concern_plotly <- renderPlotly({
    data <- development_percentage_concern_data()
    
    ymax <- max(data$pc_1_plus)
    
    plot_ly(data = data,
            x =  ~ quarter,
            y =  ~ pc_1_plus,
            type = "scatter",
            mode = "lines",
            text = ~ first(geography),
            hovertemplate = "<b>%{text}</b><br>% of reviews: %{y:.2f}%<br>Quarter of review: %{x}<extra></extra>") |>
      config(displayModeBar = FALSE) |>
      layout(yaxis = list(range = c(0, ymax),
                          title = list(text = "% of reviews")),
             xaxis = list(title = list(text = "Quarter of review")))
  })
  
  
  
  
  ##Number of Reviews ----
  #title of plot changes based on choices
  output$development_numbers_title <- renderText({
    paste0("Number of ",
           development_data_name(),
           "s; reviews with full meaningful data on child development recorded; ",
           "and children with 1 or more developmental concerns recorded")
  })
  
  #wrangles the data to be fed into the plot
  development_numbers_data <- reactive({
    pivoted_data[[input$development_data]] |>
      mutate(category = fct(case_when(category == "no_reviews" ~ "Reviews",
                                      category == "no_meaningful_reviews" ~ "Meaningful reviews",
                                      category == "concerns_1_plus" ~ "Reviews with 1 or more developmental concerns",
                                      TRUE ~ category))) |>
      filter(geography %in% geog_final() &
               category != "pc_1_plus")
  })
  
  #creates the plot to be displayed
  output$development_numbers_plotly <- renderPlotly({
    
    data <- development_numbers_data()
    ymax <- max(data$measure)
    
    plot_ly(data = data,
            x = ~ quarter,
            y = ~ measure,
            type = "scatter",
            mode = "lines",
            color = ~ category,
            text = ~ first(geography),
            hovertemplate = "<b>%{text}</b><br>Number of reviews: %{y}<br>Quarter of review: %{x}") |>
      config(displayModeBar = FALSE) |>
      layout(yaxis = list(range = c(0, ymax),
                          title = list(text = "Number of reviews")),
             xaxis = list(title = list(text = "Quarter of review")),
             legend = list(itemclick = FALSE,
                           itemdoubleclick = FALSE,
                           title = list(text = "Number of:")))
  })
  
  
  
  ##Comparisons ----
  #title of plot changes based on choices
  output$development_comparison_title <- renderText({
    paste0("Comparing ", 
           selected$geog_comparison_level,
           " for percentage of children with one or more developmental concerns recorded at the ",
           development_data_name())
  })
  
  #wrangles the data to be fed into the plots
  development_comparison_data <- reactive({
    dashboard_data[[input$development_data]] |>
      filter(geography %in% selected$geog_comparison_list)
  })
  
  #creates the plot grid to be displayed
  output$development_comparison_plotly <- renderPlotly({
    if(length(selected$geog_comparison_list) >= 1) {
      
      data <- development_comparison_data()
      ymax <- max(data$pc_1_plus)
      
      data |>
        group_by(geography) %>%
        do(plots = plot_ly(x = .$quarter,
                           y = .$pc_1_plus,
                           type = "scatter",
                           mode = "lines",
                           text = first(.$geography),
                           hovertemplate = "<b>%{text}</b><br>% of reviews: %{y:.2f}%<br>Quarter of review: %{x}<extra></extra>") |>
             layout(showlegend = FALSE,
                    annotations = list(
                      x = 0.5,
                      y = 1.0,
                      text = .$geography,
                      xref = "paper",
                      yref = "paper",
                      xanchor = "center",
                      yanchor = "bottom",
                      showarrow = FALSE
                    ),
                    yaxis = list(range = c(0, ymax),
                                 title = list(text = "% of reviews")),
                    xaxis = list(title = list(text = "Quarter of review"))
             )) -> plot_list
      
      subplot(plot_list$plots, nrows = nrows(), shareX = TRUE, shareY = TRUE) |>
        config(displayModeBar = FALSE)
    }
  })
  
  
  
 
  
  
  
  ##Concerns by Domain ----
  #title of plot changes based on choices
  output$development_concerns_by_domain_title <- renderText({
    paste0("Percentage of ",
           development_data_name(),
           "s with a new or previous concern recorded by developmental domain")
  })
  
  #wrangles the data to be fed into the plot
  development_concerns_by_domain_data <- reactive({
    pivoted_data[[paste0(input$development_data, "_domains")]] |>
      left_join(domains, by = join_by(category == variable)) |>
      mutate(title = fct_reorder2(title, quarter, measure)) |>
      filter(category %in% input$domains_selected)
  })
  
  #creates the plot to be displayed
  output$development_concerns_by_domain_plotly <- renderPlotly({
    
    data <- development_concerns_by_domain_data()
    ymax <- max(data$measure)
    
    plot_ly(data = data,
            x = ~ quarter,
            y = ~ measure,
            type = "scatter",
            mode = "lines",
            color = ~ title,
            text = ~ first(geography),
            hovertemplate = "<b>%{text}</b><br>% of reviews: %{y:.2f}%<br>Quarter of review: %{x}") |>
      config(displayModeBar = FALSE) |>
      layout(yaxis = list(range = c(0, ymax),
                          title = list(text = "% of reviews")),
             xaxis = list(title = list(text = "Quarter of review")),
             legend = list(itemclick = FALSE,
                           itemdoubleclick = FALSE,
                           title = list(text = "Developmental Domain"))
      )
  })
  
  
  
  ##Concerns by SIMD Quintile ----
  #title of plot changes based on choices
  output$development_concerns_by_simd_title <- renderText({
    paste0("Percentage of children with 1 or more developmental concerns recorded at the ",
           development_data_name(),
           " by SIMD deprivation quintile")
  })
  
  #wrangles the data to be fed into the plot
  development_concerns_by_simd_data <- reactive({
    dashboard_data[[paste0(input$development_data, "_simd")]] |>
      filter(simd %in% input$simd_levels)
  })
  
  #creates the plot to be displayed
  output$development_concerns_by_simd_plotly <- renderPlotly({
    
    data <- development_concerns_by_simd_data()
    ymax <- max(data$pc_1_plus)
    
    plot_ly(data = data,
            x = ~ quarter,
            y = ~ pc_1_plus,
            type = "scatter",
            mode = "lines",
            color = ~ simd,
            text = ~ first(geography),
            hovertemplate = "<b>%{text}</b><br>% of reviews: %{y:.2f}%<br>Quarter of review: %{x}") |>
      config(displayModeBar = FALSE) |>
      layout(yaxis = list(range = c(0, ymax),
                          title = list(text = "% of reviews")),
             xaxis = list(title = list(text = "Quarter of review")),
             legend = list(itemclick = FALSE,
                           itemdoubleclick = FALSE,
                           title = list(text = "SIMD Quintile"))
      )
  })
  
  
  
  
  
  
  
  #.----
  #Testing! ----
  #little sidebar dev app to display variables for testing
  # output$testing <- renderPrint({
  #   str_view(c(paste0("A ", input$geog_level_chosen_feeding),
  #              paste0("B ", input$geog_level_chosen_development),
  #              paste0("C ", input$geog_chosen_feeding),
  #              paste0("D ", input$geog_chosen_development),
  #              paste0("H ", selected$geog_level),
  #              paste0("I ", selected$geog),
  #              paste0("J ", geog_final()),
  #              paste0("G ", input$geog_comparison_level_feeding),
  #              paste0("H ", input$geog_comparison_level_development),
  #              paste0("I ", input$feeding_type),
  #              paste0("J ", feeding_type_numbers()),
  #              selected$geog_comparison_list
  #   ))
  # })
  
  
}





shinyApp(ui, server)
