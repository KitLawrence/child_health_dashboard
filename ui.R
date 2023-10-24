

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
  
  #creates the controls which sit outwith tabs
  fluidRow(
    box(width = 5,
        radioGroupButtons(
          inputId = "feeding_data",
          label = "Select the data you want to explore:",
          choiceNames = list("Health Visitor first visit", "6-8 week review"),
          choiceValues = list("feeding_first_visit", "feeding_6_8_week_review"),
          selected = "feeding_first_visit",
          #justified = TRUE,
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
  
  fluidRow(
    tabBox(title = "Infant Feeding",
           id = "feeding_charts_tab",
           width = 12,
           height = "25px",
           
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
  
  #creates the controls which sit outwith tabs
  fluidRow(
    box(width = 7,
        radioGroupButtons(
          inputId = "development_data",
          label = "Select the data you want to explore:",
          choiceNames = list("13-15 month review", "27-30 month review", "4-5 year review"),
          choiceValues = list("development_13_15_month_review", "development_27_30_month_review", "development_4_5_year_review"),
          selected = "development_13_15_month_review",
          #justified = TRUE,
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
  
  fluidRow(
    tabBox(title = "Child Development",
           id = "development_charts_tab",
           width = 12,
           height = "25px",

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



##body ----
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
#pulls together all the elements made in this script
tagList( #needed for shinyjs
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

