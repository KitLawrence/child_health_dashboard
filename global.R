

#libraries ----
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(readr)
library(forcats)

#library(labelled)
#library(data.table)
#library(DT)
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

#library(phsstyles)
library(phsmethods)


source("functions.R")








#reference lists ----
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


#sets colours for each line that appears in the dashboard
line_colours <- c("#3F3685", #phs-purple
                  "#9B4393", #phs-magenta
                  "#9B4393", #phs-magenta
                  "#1E7F84", #phs-teal
                  "#C73918", #phs-rust
                  "#D26146", #phs-rust-80
                  "#D26146", #phs-rust-80
                  
                  "#3F3685", #phs-purple
                  "#9B4393", #phs-magenta
                  "#0078D4", #phs-blue
                  "#83BB26", #phs-green
                  "#948DA3", #phs-graphite
                  "#1E7F84", #phs-teal
                  "#6B5C85", #phs-liberty
                  "#C73918", #phs-rust
                  
                  "#C73918", #phs-rust
                  "#9B4393", #phs-magenta
                  "#3F3685", #phs-purple
                  "#0078D4", #phs-blue
                  "#83BB26" #phs-green
) |>
  setNames(c("Ever breastfed", 
             "Reviews with 1 or more developmental concerns",
             "Overall breastfed",
             "Exclusively breastfed",
             "Reviews",
             "Valid reviews",
             "Meaningful reviews",
             
             domains$title,
             
             as.character(1:5)
  ))






#read in files ----

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



#changing data to quarterly ----
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
#also made a pre-pandemic median for pc_1_plus
dashboard_data <- map(names(dashboard_data), \(x) {
  data <- dashboard_data
  if(str_starts(x, "development")) {
    data[[x]] <- data[[x]] |>
      #create a column marking the quarter
      mutate(quarter = fct_reorder(paste(str_extract(qtr(month_review), "^\\w*"),
                                         str_extract(qtr(month_review), "\\d*$")), 
                                   month_review)) |>
      #remove incomplete quarters
      filter((quarter != last(levels(quarter))) &
               !(geography == "NHS Greater Glasgow & Clyde" & quarter %in% c("January 2019", "April 2019"))) |>
      #agggregate by quarter
      group_by(pick(any_of(c("geography", "quarter", "simd")))) |>
      summarise(across(any_of(perc_cats),
                       \(x) {sum(x * no_reviews) / sum(no_reviews)}),
                across(any_of(no_cats),
                       sum),
                .groups = "drop")
    
      
      #generate a column which gives the median for before the pandemic
      if(str_ends(x, "review")) {
        data[[x]] <- data[[x]] |>
          group_by(geography) |>
          mutate(median_pc_1_plus = median(pc_1_plus[str_ends(quarter, "2019") | quarter == "January 2020"], na.rm = TRUE)) |>
          ungroup()
      }
  }
  return(data[[x]])
})

#need to reestablish the names of the datasets after making the above aggregation
names(dashboard_data) <- c("feeding_first_visit", "feeding_6_8_week_review",
                           "development_13_15_month_review", "development_27_30_month_review", "development_4_5_year_review",
                           "development_13_15_month_review_domains", "development_27_30_month_review_domains", "development_4_5_year_review_domains",
                           "development_13_15_month_review_simd", "development_27_30_month_review_simd", "development_4_5_year_review_simd")





#pre-pivot data ----
#the data was being pivoted within most of the plots in the dashboard, so this
#does the pivoting in a generalised way beforehand to reduce processing time
pivoted_data <- map(dashboard_data, \(x) {
  x |>
    pivot_longer(cols = any_of(c(no_cats, perc_cats)),
                 names_to = "category",
                 values_to = "measure") |>
    mutate(category = fct(category))
})





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
