

function(input, output, session) {
  
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
      #justified = TRUE,
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