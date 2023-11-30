


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
          tags$p("Health in the Early Years in Scotland (HEYS) Dashboard v0.1")
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
      ) |> rem_menu_aria_label(), #function fixes ARIA label of symbol on accordion menu
      
      menuItem("Child Development",
               icon = icon("children", verify_fa = FALSE) |> rem_aria_label(),
               menuSubItem("Charts",
                           tabName = "development_charts",
                           icon = shiny::icon("angle-double-right") |> rem_aria_label()),
               menuSubItem("About this indicator",
                           tabName = "development_about",
                           icon = shiny::icon("angle-double-right") |> rem_aria_label())
      ) |> rem_menu_aria_label() #function fixes ARIA label of symbol on accordion menu
      
    )    #sidebarMenu
  ),    #accessible_menu
  
  useShinyjs(),    #must be included to make javascript stuff happen
  
  #controls which appear in sidebar only on specific tabs
  uiOutput("feeding_data_select"),
  uiOutput("feeding_type_select"),
  uiOutput("development_data_select"),
  
  
  #three invisible widgets which initialise variables which would otherwise start empty
  hidden(textInput(inputId = "sidebarMenu", label = "", value = "home")),
  uiOutput("feeding_data_initialise"),
  uiOutput("development_data_initialise"),
  
  #dev panel to test and to track variables
  verbatimTextOutput("testing")
)



#.----
##Home ----
home <- tabItem(
  tabName = "home",
  fluidRow(
    tabBox(title = "Home",
           # The id lets us use input$home_tab on the server to find the current tab
           id = "home_tab",
           width = 12,
           height = "25px",
           
           ### Welcome ----
           tabPanel(title = "Using this dashboard",
                    fluidRow(
                      box(width = 12,
                                 solidHeader = TRUE,
                                 
                                 h1("Welcome to the Health in the Early Years in Scotland (HEYS) Dashboard"),
                                 
                                 p("The dashboard provides information on two measures
                                   of child health – infant feeding and child development. 
                                   The information is drawn from routine child heath 
                                   reviews and is shown here according to the date
                                   that children received their review. For more 
                                   information on the data source and other publications
                                   on child health outcomes, see the ‘Background’ tab.")
                          ),
                    
                      
                      box(width = 12, #solidHeader = TRUE, 
                          collapsible = TRUE, collapsed = TRUE, #status = "primary",
                          title = p(strong("Selecting which measure you are interested in")),
                        
                          p("Click on the measure you are interested in via the sidebar
                            on the left-hand side. Each measure has a drop-down list; 
                            choose the “Charts” to go to the pages displaying data,
                            and the “About this indicator” tab to read background 
                            information and context for that indicator.")
                          ),
                      
                      
                      box(width = 12, #solidHeader = TRUE, 
                          collapsible = TRUE, collapsed = TRUE, #status = "primary",
                          title = p(strong("Infant feeding details")),
                          
                          p("Within the infant feeding data, select from the options 
                            on left-hand side to show information about feeding at 
                            the Health Visitor first visit (around 10 days of age),
                            or the review at 6-8 weeks of age. You can also select 
                            whether you wish to see data on the percentage of babies 
                            who have been ‘Ever breastfed’, are currently ‘Overall 
                            breastfed’ (including mixed feeding), and currently ‘Exclusively 
                            breastfed’. More information about definitions is available 
                            in the “About this indicator” section.")
                      ),
                             
                      
                      box(width = 12, #solidHeader = TRUE, 
                          collapsible = TRUE, collapsed = TRUE, #status = "primary",
                          title = p(strong("Child development details")),
                          
                          p("Within the child development data, select from the options
                            on the left-hand side to show information about outcomes 
                            of the 13-15 month review, 27-30 month review, or 4-5
                            year review."),
                          
                          p("There are further tabs along the top to show information 
                            about outcomes by developmental domain and by SIMD at 
                            Scotland level only. It is possible to select which domains
                            or SIMD quintiles you wish to see in these charts by 
                            checking the relevant boxes. More information about definitions
                            is available in the “About this indicator” section.")
                      ),
                      
                      
                      box(width = 12, #solidHeader = TRUE, 
                          collapsible = TRUE, collapsed = TRUE, #status = "primary",
                          title = p(strong("Run charts to show shifts and trends in the data")),
                          
                          p("Data over time are shown as run charts to help indicate
                            when there may be changes in the data. Marking of shifts 
                            and trends is only shown when a single measure is selected 
                            in a given chart. For more information see the “How we 
                            identify patterns in the data” tab.")
                      ),
                      
                      
                      box(width = 12, #solidHeader = TRUE, 
                          collapsible = TRUE, collapsed = TRUE, #status = "primary",
                          title = p(strong("Copying charts and downloading the data")),
                          
                          p("To make a copy of any content it is recommended that 
                            a snipping tool is used. Ensure titles and footnotes
                            are included in the snapshot and the image is appropriately 
                            referenced. For Windows, use the Windows logo key + Shift + S; 
                            for Apple Mac use Command + Shift + 5."),
                          
                          p("For each measure there is a download button in the top 
                            right corner that looks like this:",
                            tags$imag(src = "download_button.png",
                                      alt = "Download button image"), 
                            ". Clicking this will downland an Excel spreadsheet containing
                            all the data used to make the plots for that measure. 
                            It will download all data, irrespective of what display 
                            options you have selected. The information can then be
                            manipulated in Excel to show the aspects of interest 
                            to you, for example using ‘filter’ or ‘pivot table’ options.")
                      ),
                      
                      
                      box(width = 12, #solidHeader = TRUE, 
                          collapsible = TRUE, collapsed = TRUE, #status = "primary",
                          title = p(strong("Tell us what you think")),
                          
                          p("The version of the dashboard available today is still 
                            in development and is subject to changes and refinements 
                            in the future. Contact ",
                            tags$a(
                              href = "mailto:phs.childhealthstats@phs.scot",
                              tags$u("phs.childhealthstats@phs.scot"),
                              class = "externallink", target = "_blank"),
                            " for more information or to provide feedback.")
                      )
                      
                      
                    ) #fluidRow
           ),  #tabPanel ("Welcome")
           
           ###Identifying patterns ----
           tabPanel(title = "How we identify patterns in the data",
                    fluidRow(box(width = 12,
                                 solidHeader = TRUE,
                                 h1("How we identify patterns in the data"),
                                 p(strong("Run charts"), "have been used to show 
                                   data over time for the measures in this dashboard.
                                   Run charts use a series of rules to help identify 
                                   important change in the data. Note these markings 
                                   are only visible when a single measure is selected
                                   in a given chart, to avoid the charts being ‘cluttered’ 
                                   when multiple measures are shown. These are the 
                                   rules that have been applied to these charts:"
                                 ),
                                 
                                 tags$div(HTML("<ul>
                <li><strong>Shifts:</strong> Six or more consecutive data points above or below
                            the centreline. Points on the centreline neither break nor contribute
                            to a shift (such points are marked with a star symbol on charts). </li>

                <li><strong>Trends:</strong> Five or more consecutive data points which are
                increasing or decreasing. An observation that is the same as the preceding value
                does not count towards a trend (marked as a semi-transparent shadow over the 
                line between these points on charts). </li>

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
           ), #tabPanel("How we identify patterns in the data")
           
           ###Background ----
           tabPanel(
             title = "Background and other publications",
             fluidRow(
               box(width = 12,  solidHeader = TRUE,
                   
                   h1("Background and other publications"),
                   
                   p("This dashboard has been developed by Public Health Scotland 
                     to provide timely, quarterly information on health outcomes 
                     among children in Scotland. If follows on from the data presented
                     on the",
                     tags$a(
                       href = "https://scotland.shinyapps.io/phs-covid-wider-impact/",
                       tags$u("COVID Wider Impacts dashboard"),
                       class = "externallink", target = "_blank")
                     ,
                     " which updated for the last time in October 2023. It also 
                     exists as a sister dashboard to the ", 
                     tags$a(
                       href = "https://scotland.shinyapps.io/phs-pregnancy-births-neonatal/",
                       tags$u("Scottish Pregnancy, Births and Neonatal Dashboard"),
                       class = "externallink", target = "_blank"),
                     " (SPBaND)."),
                   
                   h2("The child health programme"),
                   
                   p("The data on this dashboard are drawn from routine pre-school 
                     health reviews that are offered to all children in Scotland. 
                     Information on this child health programme is available ",
                     tags$a(
                       href = "https://publichealthscotland.scot/our-areas-of-work/early-years-and-young-people/child-health-data-and-intelligence/child-health-programme/pre-school-system/",
                       tags$u("on the PHS website"),
                       class = "externallink", target = "_blank"),
                     "."),
                   
                   h2("Publication of data on cohorts of children and by review date"),
                   
                   p("Information on ",
                     tags$a(
                     href = "https://publichealthscotland.scot/publications/infant-feeding-statistics/infant-feeding-statistics-financial-year-2022-to-2023/",
                     tags$u("infant feeding"),
                     class = "externallink", target = "_blank"),
                     " and ",
                     tags$a(
                       href = "https://publichealthscotland.scot/publications/early-child-development/early-child-development-statistics-scotland-2021-to-2022/",
                       tags$u("early childhood development"),
                       class = "externallink", target = "_blank"),
                     " is published each year. This includes an annual report, open 
                     data, and dashboard which present more detailed information, 
                     such as outcomes by deprivation and ethnicity. These annual 
                     data are based on groups of children as they reach the age at 
                     which they become eligible for each review, known as cohorts. 
                     This allows reporting of complete information for all those 
                     children that should be offered a review in a given period 
                     (“cohort data”). By contrast, this dashboard shows data by the
                     date those children who were reviewed received that review 
                     (“review date data”)."
                     )
                   
                   ) #box
             ) #fluidRow
           ), #tabPanel ("Background and other publications")
           
           ###Version ----
           tabPanel(
             title = "Version",
             fluidRow(
               box(width = 12, solidHeader = TRUE,
                   
                   h1("Version information"),
                   
                   p("Prospective 1.0 release date: Jan 16th 2024")
                   
                   )
             )
           ) #tabPanel ("Version")
           
    )# tabBox ("Home")
  ) #fluidRow
)



##Infant Feeding ----
feeding_charts <- tabItem(
  tabName = "feeding_charts",
  fluidRow(
    tabBox(title = "Infant Feeding",
           # The id lets us use input$feeding_charts_tab on the server to find the current tab
           id = "feeding_charts_tab",
           width = 12,
           height = "25px",
           
           ###Individual region charts tab----
           tabPanel(title = "Individual region charts",
                    
                    fluidRow(
                      box(width = 5, solidHeader = TRUE,
                          uiOutput("geog_level_select_feeding") #widget to select geography level
                      ),
                      box(width = 4, solidHeader = TRUE,
                          uiOutput("geog_select_feeding") #widget to select HB / CA
                      ),
                      box(width = 3, solidHeader = TRUE,
                          downloadButton(outputId = "feeding_download", #currently not operational
                                         label = "Download infant feeding data",
                                         icon = shiny::icon("download") |> rem_aria_label()
                          ) 
                      )
                    ), #fluidRow
                    
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          h4(textOutput("feeding_perc_title"), style = "text-align: center;"),
                          loading(plotlyOutput("feeding_perc_plotly", height = "300px"))
                      ),
                      box(width = 12, solidHeader = TRUE,
                          
                          p("The above plot is set up to display multiple lines at 
                            once in a line graph comparing multiple catagories. When
                            only one catagory is selected, this line graph becomes
                            a runchart by adding a median line as well as shift and 
                            trend information in line with what is described in the 
                            \"How we identify patterns in the data\" section of 
                            the Home page. These runcharts help identify unusual 
                            behaviour in data and indicate patterns that merit further 
                            investigation."),
                          
                          p("To provide a basis for identifying patterns in the data,
                          the chart above uses a solid horizontal line to show the 
                          average (median) percentage of children who are recorded
                          as breastfed over the pre-pandemic time period. This line 
                          is dashed where the median is projected outside that time 
                          range. Points on the line which are part of a shift of 
                          6 or more consecutive points above or below the centreline 
                          are denoted with a star symbol. Where there are 5 or more
                          consecutively increasing or decreasing points, a semi-transparent 
                          shadow appears over the original line."),
                          
                          p("Lastly, the line graph below shows the same categories 
                            as above, but gives the total numbers for these points 
                            rather than percentages. This plot also shows the total 
                            number of reviews and valid reviews for each month.")
                          
                          ),
                      
                      
                      box(width = 12,
                          h4(textOutput("feeding_numbers_title"), style = "text-align: center;"),
                          loading(plotlyOutput("feeding_numbers_plotly", height = "300px"))
                          )
                    ) #fluidRow
           ), #tabPanel
           
           
           
           ###Regional comparisons ----
           tabPanel(title = "Regional comparisons",
                    
                    fluidRow(
                      box(width = 4, solidHeader = TRUE,
                          uiOutput("geog_comparison_level_select_feeding") #widget to select geography level
                      ),
                      box(width = 6, solidHeader = TRUE,
                          uiOutput("geog_comparison_select_feeding") #widget to select HB ? CA
                      ),
                      box(width = 2, solidHeader = TRUE,
                          br(),
                          actionBttn(inputId = "update_feeding_comparison",
                                     label = "Update View",
                                     style = "unite", 
                                     color = "royal"
                                     ) #comparison graph only updates when this button is pressed
                          )
                    ), #fluidRow
                    
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          h4(textOutput("feeding_comparison_title"), style = "text-align: center;"),
                          loading(plotlyOutput("feeding_comparison_plotly", height = "600px")),
                          br(),
                          
                          p("This tab allows users to compare multiple health boards 
                            or council areas to each other, as well as to the total 
                            for all of Scotland. The graphs plot the same line graph
                            that would be seen on the \"Individual region charts\" tab, 
                            but omits the runchart features of median lines, shifts,
                            and trends as these additions make the graphs hard to 
                            read when in a comparison grid."),
                          
                          p("To view the graphs, first select which geography level
                            you wish to explore, and then select any number of health
                            boards / council areas from the drop-down list. When 
                            you have selected every region you want, click the Update
                            View button and the graphs will be generated and displayed
                            in a grid. As with the \"Individual region charts\" tab, the 
                            selection criteria in the sidebar will be applied when 
                            creating these graphs - but are only applied once the 
                            Update View button is pressed."),
                          
                          p("Please be aware that with many regions selected at once 
                            the dashboard may be slow to produce all of the graphs 
                            for these regions. ")
                          
                          )
                    ) #fluidRow
           ) #tabPanel
    ) # tabBox
  ) #fluidRow
) #tabItem


###About this indicator ----
feeding_about <- tabItem(
  tabName = "feeding_about",
  fluidRow(
    tabBox(title = "About this indicator",
           id = "feeding_about_tab", #is tabBox structure unnecessary if there's only one tab here?
           width = 12,
           height = "25px",
           tabPanel(title = "About this indicator",
                    
                    fluidRow(
                      box(title = "Why measure this?",
                          width = 5,
                          p("Encouraging and supporting breastfeeding is an important 
                            public health activity. There is strong evidence that 
                            breastfeeding protects the health of children and breastfeeding parents.  
                            The information is collected at Health Visitor reviews 
                            of children at around 10-14 days (First Visit) and at 
                            6-8 weeks."),
                          
                          p("At a review, health visitors record information on how
                            the baby was fed in the 24 hours before the review and 
                            also on whether the baby has ever been fed breastmilk 
                            since birth. This information is used to derive the three
                            measures reported on this dashboard. The definitions 
                            are provided in the “Data source and definitions” column.")
                      ), #box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          width = 5,
                          p("Data Source:",
                            tags$a(
                              href = "https://publichealthscotland.scot/our-areas-of-work/early-years-and-young-people/child-health-data-and-intelligence/child-health-programme/pre-school-system/",
                              tags$u("CHSP Pre-School"),
                              class = "externallink", target = "_blank")
                            ),
                          h3("Definitions"),
                          tags$div(HTML("<ul>
                <li><strong>Exclusively breastfed</strong>: babies who have only
                been fed breast milk in the 24 hours prior to review. Note that data
                on whether babies have been <i>always</i> exclusively breastfed 
                (i.e. since birth) is available in the annual PHS infant feeding
                report.</li>

                <li><strong>Overall breastfed</strong>: babies who have been fed 
                breast milk only, and those who have been fed both breast milk and
                formula milk, in the 24 hours prior to review. </li>

                <li><strong>Ever breastfed</strong>: babies who have ever been fed 
                breast milk between birth and the time of review.</li>
                
                <br>
                
                <li><strong>Denominators</strong>: the denominator used to calculate 
                the percentage of babies in each of these groups is the number of
                reviews in which there is valid data recorded (i.e. not ‘missing’ 
                or ‘unknown’) for infant feeding status.</li>
                
                <li><strong>Average</strong>: in these charts ‘average’ refers to
                median value of the period specified. </li>
                
                <li><strong>Geography</strong>: data is reported by the Health Board
                and Council Area residence recorded at the time of review.</li>
                
                <li><strong>Time</strong>: data is reported by the date at which 
                the review was conducted (“review date data”). Note that this is 
                different from the approach taken in the annual PHS infant feeding 
                report, which reports information for children according to the date 
                at which a group of children become eligible  for each review (“cohort
                data”).</li>


                </ul>"))
                          
                      ) #box
                    ) #fluidRow
           ) #tabPanel
    ) #tabBox
  ) #fluidRow
) #tabItem




##Child Development ----
development_charts <- tabItem(
  tabName = "development_charts",
  fluidRow(
    tabBox(title = "Child Development",
           # The id lets us use input$development_charts_tab on the server to find the current tab
           id = "development_charts_tab",
           width = 12,
           height = "25px",

           ###Individual region charts ----
           tabPanel(title = "Individual region charts",
                    
                    fluidRow(
                      box(width = 5, solidHeader = TRUE,
                          uiOutput("geog_level_select_development") #widget to select geography level
                      ),
                      box(width = 4, solidHeader = TRUE,
                          uiOutput("geog_select_development") #widget to select HB / CA
                      ),
                      box(width = 3, solidHeader = TRUE,
                          downloadButton(outputId = "development_download",  #currently not operational
                                         label = "Download child development data",
                                         icon = shiny::icon("download") |> rem_aria_label()
                          )
                      ) 
                    ), #fluidRow
                    
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          h4(textOutput("development_percentage_concern_title"), style = "text-align: center;"),
                          loading(plotlyOutput("development_percentage_concern_plotly", height = "300px")),
                          
                          p("We have used runcharts to present the data above. 
                          Run charts use a series of rules to help identify unusual 
                          behaviour in data and indicate patterns that merit further 
                          investigation. Read more about the rules used in the charts
                          in the \"How we identify trends in the data\" section of 
                          the Home page"),
                          
                          p("To provide a basis for identifying patterns in the data,
                          the chart above uses a solid horizontal line to show the 
                          average (median) percentage of children who are recorded
                          as breastfed over the pre-pandemic time period. This line 
                          is dashed where the median is projected outside that time 
                          range. Points on the line which are part of a shift of 
                          6 or more consecutive points above or below the centreline 
                          are denoted with a star symbol. Where there are 5 or more
                          consecutively increasing or decreasing points, a semi-transparent 
                          shadow appears over the original line."),
                          
                          p("Lastly, the line graph below shows the same measure 
                            as above, but gives the total numbers for these points 
                            rather than percentages. This plot also shows the total 
                            number of reviews and meaningful reviews for each quarter.")
                      ),
                      
                      box(width = 12,
                          h4(textOutput("development_numbers_title"), style = "text-align: center;"),
                          loading(plotlyOutput("development_numbers_plotly", height = "300px"))
                          )
                    ) #fluidRow
           ), #tabPanel
           
           
           
           ###Regional comparisons ----
           tabPanel(title = "Regional comparisons",
                    fluidRow(
                      box(width = 4, solidHeader = TRUE,
                          uiOutput("geog_comparison_level_select_development") #widget to select geography level
                      ),
                      box(width = 6, solidHeader = TRUE,
                          uiOutput("geog_comparison_select_development") #widget to select HB / CA
                      ),
                      box(width = 2, solidHeader = TRUE,
                          br(),
                          actionBttn(inputId = "update_development_comparison",
                                       label = "Update View",
                                       style = "unite", 
                                       color = "royal"
                                       ) #comparison graph only updates when this button is pressed
                      )
                    ), #fluidRow
                    
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          h4(textOutput("development_comparison_title"), style = "text-align: center;"),
                          loading(plotlyOutput("development_comparison_plotly", height = "600px")),
                          br(),

                          p("This tab allows users to compare multiple health boards 
                            or council areas to each other, as well as to the total 
                            for all of Scotland. The graphs plot the same line graph
                            that would be seen on the \"Individual region charts\" tab, 
                            but omits the runchart features of median lines, shifts,
                            and trends as these additions make the graphs hard to 
                            read when in a comparison grid."),
                          
                          p("To view the graphs, first select which geography level
                            you wish to explore, and then select any number of health
                            boards / council areas from the drop-down list. When 
                            you have selected every region you want, click the Update
                            View button and the graphs will be generated and displayed
                            in a grid. As with the \"Individual region charts\" tab, the 
                            selection criteria in the sidebar will be applied when 
                            creating these graphs - but are only applied once the 
                            Update View button is pressed."),
                          
                          p("Please be aware that with many regions selected at once 
                            the dashboard may be slow to produce all of the graphs 
                            for these regions. ")
                          
                          )
                    ) #fluidRow
           ), #tabPanel
           
           ###Developmental domains ----
           tabPanel(title = "Developmental domains",
                    
                    fluidRow(
                      box(width = 9, solidHeader = TRUE,
                          #widget to select which developmental domains to display on plot
                          checkboxGroupButtons(
                            inputId = "domains_selected",
                            label = "Select which developmental domains to include in the plot:",
                            choices = domains,
                            selected = domains,
                            status = "primary",
                            checkIcon = list(
                              yes = icon("square-check") |> rem_aria_label(),
                              no = icon("square") |> rem_aria_label())
                          )
                      ),
                      box(width = 3, solidHeader = TRUE,
                          downloadButton(outputId = "domains_download", #currently not operational
                                         label = "Download developmental domains data",
                                         icon = shiny::icon("download") |> rem_aria_label()
                          ) 
                      )
                    ), #fluidRow
                    
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          
                          h4(textOutput("development_concerns_by_domain_title"), style = "text-align: center;"),
                          loading(plotlyOutput("development_concerns_by_domain_plotly", height = "300px")),
                          
                          p("The above plot is set up to display multiple lines at 
                            once in a line graph comparing multiple catagories. When
                            only one catagory is selected, this line graph becomes
                            a runchart by adding a median line as well as shift and 
                            trend information in line with what is described in the 
                            \"How we identify patterns in the data\" section of 
                            the Home page. These runcharts help identify unusual 
                            behaviour in data and indicate patterns that merit further 
                            investigation."),
                          
                          p("To provide a basis for identifying patterns in the data,
                          the chart above uses a solid horizontal line to show the 
                          average (median) percentage of children who are recorded
                          as breastfed over the pre-pandemic time period. This line 
                          is dashed where the median is projected outside that time 
                          range. Points on the line which are part of a shift of 
                          6 or more consecutive points above or below the centreline 
                          are denoted with a star symbol. Where there are 5 or more
                          consecutively increasing or decreasing points, a semi-transparent 
                          shadow appears over the original line.")
                      )
                    ) #fluidRow
           ), #tabPanel
           
           
           ###SIMD quintiles ----
           tabPanel(title = "SIMD quintiles",
                    fluidRow(
                      box(width = 9, solidHeader = TRUE,
                          #widget to select which SIMD quintiles to display
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
                            )
                          ),
                      box(width = 3, solidHeader = TRUE,
                          downloadButton(outputId = "simd_download", #currently not operational
                                         label = "Download SIMD data",
                                         icon = shiny::icon("download") |> rem_aria_label()
                                         ) 
                          )
                    ), #fluidRow
                    
                    fluidRow(
                      box(width = 12, solidHeader = TRUE,
                          h4(textOutput("development_concerns_by_simd_title"), style = "text-align: center;"),
                          loading(plotlyOutput("development_concerns_by_simd_plotly", height = "300px")),
                          
                          p("The above plot is set up to display multiple lines at 
                            once in a line graph comparing multiple catagories. When
                            only one catagory is selected, this line graph becomes
                            a runchart by adding a median line as well as shift and 
                            trend information in line with what is described in the 
                            \"How we identify patterns in the data\" section of 
                            the Home page. These runcharts help identify unusual 
                            behaviour in data and indicate patterns that merit further 
                            investigation."),
                          
                          p("To provide a basis for identifying patterns in the data,
                          the chart above uses a solid horizontal line to show the 
                          average (median) percentage of children who are recorded
                          as breastfed over the pre-pandemic time period. This line 
                          is dashed where the median is projected outside that time 
                          range. Points on the line which are part of a shift of 
                          6 or more consecutive points above or below the centreline 
                          are denoted with a star symbol. Where there are 5 or more
                          consecutively increasing or decreasing points, a semi-transparent 
                          shadow appears over the original line.")
                          
                      ) #box
                    ) #fluidRow
           ) #tabPanel
    ) # tabBox 
  ) #fluidRow
) #tabItem

###About this indicator ----
development_about <- tabItem(
  tabName = "development_about",
  fluidRow(
    tabBox(title = "About this indicator",
           id = "development_about_tab", #does this need tabbox structure if it's only one tab?
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
                            programme which includes a series of child health reviews, 
                            including an assessment of children’s development at
                            13-15 months, 27-30 months and 4-5 years.  These reviews
                            involve asking parents about their child’s progress,
                            carefully observing the child, and supporting parents 
                            to complete a structured questionnaire about the child’s
                            development. At the end of the review Health Visitors 
                            record whether they have any concerns about each area
                            of the child’s development.")
                      ), #box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          width = 5,
                          p("Data Source:",
                            tags$a(
                              href = "https://publichealthscotland.scot/our-areas-of-work/early-years-and-young-people/child-health-data-and-intelligence/child-health-programme/pre-school-system/",
                              tags$u("CHSP Pre-School"),
                              class = "externallink", target = "_blank")
                          ),
                          
                          h3("Definitions"),
                          tags$div(HTML("<ul>
                          
                <li><strong>One or more developmental concern</strong>: this refers
                to those children who have a new or previously identified concern 
                recorded for one or more of the eight developmental domains assessed
                at that review. </li>

                <li><strong>Meaningful data</strong>: this refers to records where 
                a value has been recorded against each of the eight developmental 
                domains at that review. The values that can be recorded are N (no
                concerns), C (concern newly suspected), or P (concern previously 
                identified). </li>
                
                <br>
                
                <li><strong>Denominators</strong>: the denominator used to calculate
                the percentage of children with one or more developmental concerns 
                is the total number of reviews undertaken. It is not necessary for 
                records to contain meaningful data for each developmental domain
                for them to be included. This is because children may have a concern
                recorded in one domain, but have another without information recorded. </li>
                
                <li><strong>Average</strong>: in these charts ‘average’ refers to
                median value of the period specified. </li>
                
                <li><strong>Geography</strong>: data is reported by the Health Board
                and Council Area of residence recorded at the time of review. Note 
                that the date of implementation of the 13-15 month and 4-5 year review 
                varies by Health Board, and therefore the start date of time series 
                data may vary. For example, the 13-15 month review has been offered
                in NHS Greater Glasgow and Clyde since May 2019, therefore no data 
                are shown for this area for Jan-Apr 2019. This also influences the
                period of data available to calculate the median value used in the 
                run charts. </li>
                
                <li><strong>Time</strong>: data is reported by the date at which
                the review was conducted (“review date data”). Note that this is
                different from the approach taken in the annual PHS infant feeding 
                report, which reports information for children according to the date
                at which a group of children become eligible for each review (“cohort 
                data”).  </li>


                </ul>")),
                          
                      ) #box
                    ) #fluidRow
           ) #tabPanel
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
            tags$title("HEYS Dashboard")
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
) |> secure_app() #comment out to remove authentication
