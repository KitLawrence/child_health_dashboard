#START OF SCRIPT ----

function(input, output, session) {
  
    #these two elements create the dashboard authentication 
    res_auth <- secure_server(
      check_credentials = check_credentials(credentials)
    )
    
    output$auth_output <- renderPrint({
      reactiveValuesToList(res_auth)
    })
  
  
  #Geography Control ----
  #takes geography level choice from infant feeding tab
  output$geog_level_select_feeding <- renderUI({
    radioButtons(
      inputId = "geog_level_chosen_feeding",
      label = "Select geography level to display:",
      choices = c("All Scotland", "Health Board", "Council Area"),
      selected = selected$geog_level, #syncs this with the equivalent choice on the child development tab
      inline = TRUE
    )
  })
  
  #takes geography level choice from child development tab
  output$geog_level_select_development <- renderUI({
    radioButtons(
      inputId = "geog_level_chosen_development",
      label = "Select geography level to display:",
      choices = c("All Scotland", "Health Board", "Council Area"),
      selected = selected$geog_level, #syncs this with the equivalent choice on the infant feeding tab
      inline = TRUE
    )
  })
  
  
  
  #takes health board / council area choice from infant feeding tab
  output$geog_select_feeding <- renderUI({
    if (!("All Scotland" %in% selected$geog_level)) {
      pickerInput(
        inputId = "geog_chosen_feeding",
        label = paste0("Select ", selected$geog_level, " of residence:"),
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
        label = paste0("Select ", selected$geog_level, " of residence:"),
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
    radioButtons(
      inputId = "geog_comparison_level_feeding",
      label = "Select geography level to compare:",
      choices = list("Health Board", "Council Area"),
      selected = selected$geog_comparison_level, #syncs this with the equivalent choice on the child development tab
      inline = TRUE
    )
  })
  
  #takes geography level choice from child development comparisons tab
  output$geog_comparison_level_select_development <- renderUI({
    radioButtons(
      inputId = "geog_comparison_level_development",
      label = "Select geography level to compare:",
      choices = list("Health Board", "Council Area"),
      selected = selected$geog_comparison_level, #syncs this with the equivalent choice on the infant feeding tab
      inline = TRUE
    )
  })
  
  #takes health board / council area choices from infant feeding comparisons tab
  output$geog_comparison_select_feeding <- renderUI({
    pickerInput(
      inputId = "geog_comparison_list_feeding",
      label = paste0("Select ", 
                     selected$geog_comparison_level,
                     "s of residence to display:"),
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
                     "s of residence to display:"),
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
                             geog_comparison_level = "Health Board",
                             geog_comparison_list = HBnames,
                             domains = domains,
                             simd = c(1,5),
                             update = 1)
  
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
             "Council Area" = CAnames)
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
               "Council Area" = CAnames))
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
  
  
  
  
  #updates selected domains when each option is clicked
  observeEvent(input$domains_selected,
               selected$domains <- input$domains_selected)
  
  observeEvent(input$select_all_domains,
               selected$domains <- domains)
  
  observeEvent(input$deselect_all_domains,
               selected$domains <- character(0))
  
  
  #updates selected domains when each option is clicked
  observeEvent(input$simd_levels,
               selected$simd <- input$simd_levels)
  
  observeEvent(input$select_all_simd,
               selected$simd <- 1:5)
  
  observeEvent(input$deselect_all_simd,
               selected$simd <- numeric(0))
  
  
  observeEvent(input$update_feeding_comparison,
               selected$update <- selected$update + 1)
  
  observeEvent(input$update_development_comparison,
               selected$update <- selected$update + 1)
  
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
           "firstvisit" = "health visitor first visit",
           "6-8 week" = "6-8 week review")
  })
  
  development_data_name <- reactive({
    switch(input$development_data,
           "13-15m" = "13-15 month review",
           "27-30m" = "27-30 month review",
           "4-5y" = "4-5 year review")
  })
  
  
  
  
  
  
  
  
  
  #.----
  #Infant Feeding ----
  
  #allows user to select which infant feeding dataset to see
  output$feeding_data_select <- renderUI({
    if("feeding_charts" %in% input$sidebarMenu) { #only appear when on the infant feeding charts page
      radioButtons(
        inputId = "feeding_data",
        label = "Select the data you want to explore:",
        choiceNames = list("Health Visitor first visit", "6-8 week review"),
        choiceValues = feeding_data_options,
        selected = "firstvisit"
      )
    }
  })
  
  #this only exists to initialise input$feeding_data right away to stop errors occurring
  output$feeding_data_initialise <- renderUI({
    if(length(input$feeding_data) != 1) { #this if statement means this doesn't exist after it's done it's job
      hidden(
        textInput(inputId = "feeding_data", 
                  label = "", 
                  value = "firstvisit")
      )
    }
  })
  
  #"Ever breastfed" variable is only gathered at first visit so we need to exclude it
  #if the 6-8 week review data is selected
  feeding_choices <- reactive({
    if (length(input$feeding_data) == 1) {
      switch(input$feeding_data,
             "firstvisit" = c("Ever breastfed", "Overall breastfed", "Exclusively breastfed"), 
             "6-8 week" = c("Overall breastfed", "Exclusively breastfed"))
    }
  })
  
  #takes choice on which options to display from ever, overall and exclusively breastfed
  output$feeding_type_select <- renderUI({
    if("feeding_charts" %in% input$sidebarMenu) {
      checkboxGroupInput(
        inputId = "feeding_type",
        label = "Select which types of breastfeeding statistics to show:",
        choices = feeding_choices(),
        selected = feeding_choices()
        )
    }
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
    data <- feeding_percentage_data[[input$feeding_data]] |>
      filter(geography %in% geog_final() &
             category %in% input$feeding_type)
    
    #calculates the median if only one feeding option is selected
    if(length(input$feeding_type) == 1) {
      data <- data |>
          mutate(median = median(measure[date <= ymd("2020-02-01")], na.rm = TRUE),
               trend = trend(measure),
               shift = shift(measure, median))
    }
    
    return(data)
  })
  
  
  #creates the plot to be displayed
  output$feeding_perc_plotly <- renderPlotly({
    
    data <- feeding_perc_data() #read in the dataset
    ymax <- min(c(max(c(data$measure, 0)), 100)) * 1.05 #determine the y-axis max for the plot
    
    #create the basic line plot
    plot <- plot_ly(data = data,
                    x = ~ date,
                    text = ~ paste0("<b>", geography, "</b>",
                                    "<br><i>", category, "</i>")) |>
      add_trace(y = ~ measure,
                type = "scatter",
                mode = "lines+markers",
                color = ~ category,
                symbol = ~ category,
                colors = line_colours,
                hovertemplate = "%{text}<br>% of reviews: %{y:.2f}%<br>Month of review: %{x|%B %Y}<extra></extra>"
                )
      
    
    if(length(input$feeding_type) == 1) { #adds elements that only appear when one option is selected
      plot <- plot |>
        #adds a pre-pandemic median line
        add_trace(y = ~ median,
                  type = "scatter",
                  mode = "lines",
                  name = "Pre-pandemic median",
                  hovertemplate = "%{text}<br>Pre-pandemic median: %{y:.2f}%<extra></extra>",
                  line = list(color = line_colours[input$feeding_type], #match the other present line colour
                              dash = "4")
                  ) |>
        #adds shading around trend points
        add_trace(y = ~ measure / trend,
                  type = "scatter",
                  mode = "lines",
                  name = "Trend of 5 points or more",
                  hovertemplate = "%{text}<br>% of reviews: %{y:.2f}%<br>Month of review: %{x|%B %Y}<extra></extra>",
                  line = list(color = colour_switch(line_colours[input$feeding_type], 0.2), #adds alpha to colour
                              width = 10)
        ) |>
        #mark shifts by arrows around median
        add_trace(y = ~ median / (shift %% 2),
                  type = "scatter",
                  mode = "markers", 
                  name = "Upwards shift of 6 points or more",
                  hovertemplate = "%{text}<br>Pre-pandemic median: %{y:.2f}%<extra></extra>",
                  marker = list(color =  line_colours[input$feeding_type],
                                symbol = "arrow-up-open")) |>
        add_trace(y = ~ median / (shift %/% 2),
                  type = "scatter",
                  mode = "markers", 
                  name = "Downwards shift of 6 points or more",
                  hovertemplate = "%{text}<br>Pre-pandemic median: %{y:.2f}%<extra></extra>",
                  marker = list(color =  line_colours[input$feeding_type],
                                symbol = "arrow-down-open")) |>
        #adds a solid median line for the points from which the median is defined
        add_trace(y = ~ median / (date <= ymd("2020-02-01")),
                  type = "scatter",
                  mode = "lines",
                  showlegend = FALSE,
                  hovertemplate = "%{text}<br>Pre-pandemic median: %{y:.2f}%<extra></extra>",
                  line = list(color = line_colours[input$feeding_type])  #match the other present line colour
                  )
    }
    
    if(length(input$feeding_type) >= 1) { #if statement just stops a plot from being displayed when no options are selected
      plot |>
        config(displayModeBar = FALSE) |>
        layout(yaxis = list(range = c(0, ymax), 
                            title = list(text = "% of reviews")),
               xaxis = list(title = list(text = "Month of review")),
               legend = list(itemclick = FALSE,
                             itemdoubleclick = FALSE,
                             title = list(text = "Feeding Type"))
      )
    }
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
  feeding_nums_data <- reactive({
    feeding_numbers_data[[input$feeding_data]] |>
      filter(geography %in% geog_final() &
               category %in% c("Reviews", "Valid reviews", input$feeding_type))
  })
  
  #creates the plot to be displayed
  output$feeding_numbers_plotly <- renderPlotly({
    
    data <- feeding_nums_data()
    ymax <- max(c(data$measure, 0)) * 1.05
    
    plot_ly() |>
      add_trace(data = data |> filter(category == "Reviews"),
                x = ~ date,
                y = ~ measure,
                type = "scatter",
                mode = "lines+markers",
                symbol = ~ category,
                color = ~ category,
                colors = line_colours,
                text = ~ paste0("<b>", geography, "</b>",
                                "<br><i>", category, "</i>"),
                hovertemplate = "%{text}<br>Number of reviews: %{y}<br>Month of review: %{x|%B %Y}<extra></extra>"
                ) |>
      add_trace(data = data |> filter(category == "Valid reviews"),
                x = ~ date,
                y = ~ measure,
                type = "scatter",
                mode = "lines",
                name = "Valid reviews",
                line = list(dash = "dash",
                            color = line_colours["Valid reviews"]),
                text = ~ paste0("<b>", geography, "</b>",
                                "<br><i>", category, "</i>"),
                hovertemplate = "%{text}<br>Number of reviews: %{y}<br>Month of review: %{x|%B %Y}<extra></extra>") |>
      add_trace(data = data |> filter(!(category %in% c("Reviews","Valid reviews"))),
                x = ~ date,
                y = ~ measure,
                type = "scatter",
                mode = "lines+markers",
                color = ~ category,
                symbol = ~ category,
                colors = line_colours,
                text = ~ paste0("<b>", geography, "</b>", 
                                "<br><i>", category, "</i>"),
                hovertemplate = "%{text}<br>Number of reviews: %{y}<br>Month of review: %{x|%B %Y}<extra></extra>") |>
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
  feeding_comparison_data <- eventReactive(selected$update, {
    feeding_percentage_data[[input$feeding_data]] |>
      filter(geography %in% selected$geog_comparison_list &
               category %in% input$feeding_type)
  })
  
  #creates the plot grid to be displayed
  output$feeding_comparison_plotly <- renderPlotly({
    if(length(isolate(selected$geog_comparison_list)) >= 1) {
      
      data <- feeding_comparison_data()
      ymax <- min(c(max(c(data$measure, 0)), 100)) * 1.05
      
      data |>      
        group_by(geography) %>%
        do(plots = plot_ly(x = .$date,
                           y = .$measure,
                           type = "scatter",
                           mode = "lines+markers",
                           color = .$category,
                           symbol = .$category,
                           line = list(width = 1),
                           marker = list(size = 3),
                           colors = line_colours,
                           text = paste0("<b>", .$geography, "</b>", 
                                         "<br><i>", .$category, "</i>"),
                           hovertemplate = "%{text}<br>% of reviews: %{y:.2f}%<br>Month of review: %{x|%B %Y}<extra></extra>",
                           showlegend = (isolate(selected$geog_comparison_list)[1] %in% .$geography)) |>
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
               yaxis = list(range = c(0, ymax)),
               #xaxis = list(title = list(text = "Month of review")),
               legend = list(itemclick = FALSE,
                             itemdoubleclick = FALSE,
                             title = list(text = "Feeding Type"))
               )) -> plot_list
      
      subplot(plot_list$plots, nrows = isolate(nrows()), 
              shareX = TRUE, shareY = TRUE,
              margin = c(0.02, 0.02, 0.03, 0.03)
              ) |>
        layout(
          annotations = list(
            text = "% of reviews",
            font = list(size = 14),
            x = 0,
            y = 0.5,
            #standoff = 30, # distance between axis title and tick labels
            xshift = -60,
            textangle = 270,
            showarrow = FALSE,
            xref = "paper",
            yref = "paper"
          )
        ) |>
        config(displayModeBar = FALSE)
      
    }
  })
  
  
  
  
  
  
  # Child Development ----
  #allows user to select which child development dataset to see
  output$development_data_select <- renderUI({
    if("development_charts" %in% input$sidebarMenu) { #only appear when on the infant feeding charts page
      radioButtons(
        inputId = "development_data",
        label = "Select the data you want to explore:",
        choiceNames = list("13-15 month review", "27-30 month review", "4-5 year review"),
        choiceValues = development_data_options,
        selected = "13-15m"
      )
    }
  })
  #this only exists to initialise input$development_data right away to stop errors occurring
  output$development_data_initialise <- renderUI({
    if(length(input$development_data) != 1) { #this if statement means this doesn't exist after it's done it's job
      hidden(
        textInput(inputId = "development_data", 
                  label = "", 
                  value = "13-15m")
      )
    }
  })
  
  ##Percentage Concern ----
  #title of plot changes based on choices
  output$development_percentage_concern_title <- renderText({
    paste0("Percentage of children with one or more developmental concerns
           recorded at the ", 
           development_data_name())
  })
  
  #wrangles the data to be fed into the plot
  development_percentage_concern_data <- reactive({
    development_percentage_data[[input$development_data]] |>
      filter(geography %in% geog_final()) |>
      mutate(trend = trend(measure),
             shift = shift(measure, median))
  })
  
  #creates the plot to be displayed
  output$development_percentage_concern_plotly <- renderPlotly({
    data <- development_percentage_concern_data()
    
    ymax <- max(data$measure) * 1.05
    
    plot_ly(data = data,
            x =  ~ date
            ) |>
      add_trace(y =  ~ measure,
                type = "scatter",
                mode = "lines+markers",
                name = "% of reviews",
                marker = list(color = "#9B4393"), #phs-magenta
                line = list(color = "#9B4393"), #phs-magenta
                text = ~ paste0("<b>", geography, "</b>",
                                "<br>% of reviews: ", round(measure, 2), "%",
                                "<br>Quarter of review: ", 
                                month(date, label = TRUE), "-", month(date + 62, label = TRUE),
                                " ", year(date)
                                ),
                hovertemplate = "%{text}<extra></extra>") |>
      add_trace(y = ~ median,
                type = "scatter",
                mode = "lines",
                name = "Pre-pandemic median",
                text = ~ paste0("<b>", geography, "</b>",
                                "<br>Pre-pandemic median: ", round(median, 2), "%"),
                hovertemplate = "%{text}<extra></extra>",
                line = list(color = "#9B4393", #phs-magenta
                            dash = "4")
                ) |>
      add_trace(y = ~ measure / trend,
                type = "scatter",
                mode = "lines",
                name = "Trend of 5 points or more",
                text = ~ paste0("<b>", geography, "</b>",
                                "<br>% of reviews: ", round(measure, 2), "%",
                                "<br>Quarter of review: ", 
                                month(date, label = TRUE), "-", month(date + 62, label = TRUE),
                                " ", year(date)
                                ),
                hovertemplate = "%{text}<extra></extra>",
                line = list(color = colour_switch("#9B4393", 0.2), #adds aplha to phs-magenta colour
                            width = 10)
                ) |>
      add_trace(y = ~ median / (shift %% 2),
                type = "scatter",
                mode = "markers", 
                name = "Upwards shift of 6 points or more",
                text = ~ paste0("<b>", geography, "</b>",
                                "<br>Pre-pandemic median: ", round(median, 2), "%"),
                hovertemplate = "%{text}<extra></extra>",
                marker = list(color = "#9B4393",
                              symbol = "arrow-up-open")) |>
      add_trace(y = ~ median / (shift %/% 2),
                type = "scatter",
                mode = "markers", 
                name = "Downwards shift of 6 points or more",
                text = ~ paste0("<b>", geography, "</b>",
                                "<br>Pre-pandemic median: ", round(median, 2), "%"),
                hovertemplate = "%{text}<extra></extra>",
                marker = list(color = "#9B4393",
                              symbol = "arrow-down-open")) |>
      add_trace(y = ~ median / (date <= ymd("2020-02-01")),
                type = "scatter",
                mode = "lines",
                showlegend = FALSE,
                text = ~ paste0("<b>", geography, "</b>",
                                "<br>Pre-pandemic median: ", round(median, 2), "%"),
                hovertemplate = "%{text}<extra></extra>",
                line = list(color = "#9B4393") #phs-magenta
                ) |>
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
  development_nums_data <- reactive({
    development_numbers_data[[input$development_data]] |>
      filter(geography %in% geog_final())
  })
  
  #creates the plot to be displayed
  output$development_numbers_plotly <- renderPlotly({
    
    data <- development_nums_data()
    ymax <- max(data$measure) * 1.05
    
    plot_ly() |>
      add_trace(data = data |> filter(category == "Reviews"),
            x = ~ date,
            y = ~ measure,
            type = "scatter",
            mode = "lines+markers",
            color = ~ category,
            symbol = ~ category,
            colors = line_colours,
            text = ~ paste0("<b>", geography, "</b>",
                            "<br><i>", category, "</i>",
                            "<br>Number of reviews: ", round(measure, 2),
                            "<br>Quarter of review: ", 
                            month(date, label = TRUE), "-", month(date + 62, label = TRUE),
                            " ", year(date)
            ),
            hovertemplate = "%{text}<extra></extra>") |>
      add_trace(data = data |> filter(category == "Meaningful reviews"),
                x = ~ date,
                y = ~ measure,
                type = "scatter",
                mode = "lines",
                color = ~ category,
                line = list(dash = "dash"),
                colors = line_colours,
                text = ~ paste0("<b>", geography, "</b>",
                                "<br><i>", category, "</i>",
                                "<br>Number of reviews: ", round(measure, 2),
                                "<br>Quarter of review: ", 
                                month(date, label = TRUE), "-", month(date + 62, label = TRUE),
                                " ", year(date)
                ),
                hovertemplate = "%{text}<extra></extra>") |>
      add_trace(data = data |> filter(!(category %in% c("Reviews", "Meaningful reviews"))),
                x = ~ date,
                y = ~ measure,
                type = "scatter",
                mode = "lines+markers",
                color = ~ category,
                symbol = ~ category,
                colors = line_colours,
                text = ~ paste0("<b>", geography, "</b>",
                                "<br><i>", category, "</i>",
                                "<br>Number of reviews: ", round(measure, 2),
                                "<br>Quarter of review: ", 
                                month(date, label = TRUE), "-", month(date + 62, label = TRUE),
                                " ", year(date)
                ),
                hovertemplate = "%{text}<extra></extra>") |>
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
  development_comparison_data <- eventReactive(selected$update,{
    development_percentage_data[[input$development_data]] |>
      filter(geography %in% selected$geog_comparison_list)
  })
  
  #creates the plot grid to be displayed
  output$development_comparison_plotly <- renderPlotly({
    if(length(isolate(selected$geog_comparison_list)) >= 1) {
      
      data <- development_comparison_data()
      ymax <- max(data$measure) * 1.05
      
      data |>
        group_by(geography) %>%
        do(plots = plot_ly(x = .$date,
                           y = .$measure,
                           type = "scatter",
                           mode = "lines+markers",
                           line = list(color = "#9B4393",#phs-magenta
                                       width = 1), 
                           marker = list(color = "#9B4393",#phs-magenta
                                         size = 3),
                           text = paste0("<b>", .$geography, "</b>",
                                           "<br>% of reviews: ", round(.$measure, 2), "%",
                                           "<br>Quarter of review: ", 
                                           month(.$date, label = TRUE), "-", month(.$date + 62, label = TRUE),
                                           " ", year(.$date)
                           ),
                           hovertemplate = "%{text}<extra></extra>",) |>
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
                    #xaxis = list(title = list(text = "Quarter of review")),
                    yaxis = list(range = c(0, ymax))
                    
             )) -> plot_list
      
      subplot(plot_list$plots, nrows = isolate(nrows()), 
              shareX = TRUE, shareY = TRUE, 
              margin = c(0.02, 0.02, 0.03, 0.03)
              ) |>
        layout(
          annotations = list(
            text = "% of reviews",
            font = list(size = 14),
            x = 0,
            y = 0.5,
            #standoff = 30, # distance between axis title and tick labels
            xshift = -60,
            textangle = 270,
            showarrow = FALSE,
            xref = "paper",
            yref = "paper"
          )
        ) |>
        config(displayModeBar = FALSE)
    }
  })
  
  
  
  
  
  
  
  ##Concerns by Domain ----
  #selector widget for picking which developmental domains are displayed
  output$domains_selector <- renderUI({
    checkboxGroupInput(
      inputId = "domains_selected",
      label = "Select which developmental domains to include in the plot:",
      choices = domains,
      selected = selected$domains,
      inline = TRUE
    )
  })
  
  
  #title of plot changes based on choices
  output$development_concerns_by_domain_title <- renderText({
    paste0("Percentage of ",
           development_data_name(),
           "s with a new or previous concern recorded by developmental domain")
  })
  
  #wrangles the data to be fed into the plot
  development_concerns_by_domain_data <- reactive({
    data <- domains_data[[input$development_data]] |>
      filter(category %in% input$domains_selected)
    
    if(length(input$domains_selected) == 1) {
      data <- data |>
        mutate(median = median(measure[date <= ymd("2020-02-01")], na.rm = TRUE),
               trend = trend(measure),
               shift = shift(measure, median))
    }
    
    return(data)
  })
  
  #creates the plot to be displayed
  output$development_concerns_by_domain_plotly <- renderPlotly({
    
    data <- development_concerns_by_domain_data()
    ymax <- max(data$measure) * 1.05
    
    plot <- plot_ly(data = data,
            x = ~ date) |>
      add_trace(y = ~ measure,
                type = "scatter",
                mode = "lines+markers",
                color = ~ category,
                symbol = ~ category,
                colors = line_colours,
                symbols = domain_symbols,
                text = ~ paste0("<b>All Scotland</b>",
                                "<br><i>", category, "</i>",
                                "<br>% of reviews: ", round(measure, 2), "%",
                                "<br>Quarter of review: ", 
                                month(date, label = TRUE), "-", month(date + 62, label = TRUE),
                                " ", year(date)
                ),
                hovertemplate = "%{text}<extra></extra>")
    
    if(length(input$domains_selected) == 1) {
      plot <- plot |>
        add_trace(y = ~ median,
                  type = "scatter",
                  mode = "lines",
                  name = "Pre-pandemic median",
                  text = ~ paste0("<b>All Scotland</b>",
                                  "<br><i>", category, "</i>"),
                  hovertemplate = "%{text}<br>Pre-pandemic median: %{y:.2f}%<extra></extra>",
                  line = list(color = line_colours[input$domains_selected], #match the other present line colour
                              dash = "4")
        ) |>
        add_trace(y = ~ measure / trend,
                  type = "scatter",
                  mode = "lines",
                  name = "Trend of 5 points or more",
                  text = ~ paste0("<b>All Scotland</b>",
                                  "<br><i>", category, "</i>",
                                  "<br>% of reviews: ", round(measure, 2), "%",
                                  "<br>Quarter of review: ", 
                                  month(date, label = TRUE), "-", month(date + 62, label = TRUE),
                                  " ", year(date)
                  ),
                  hovertemplate = "%{text}<extra></extra>",
                  line = list(color = colour_switch(line_colours[input$domains_selected], 0.2), #adds alpha to colour
                              width = 10)
        ) |>
        #mark shifts by arrows around median
        add_trace(y = ~ median / (shift %% 2),
                  type = "scatter",
                  mode = "markers", 
                  name = "Upwards shift of 6 points or more",
                  text = ~ paste0("<b>All Scotland</b>",
                                  "<br><i>", category, "</i>"),
                  hovertemplate = "%{text}<br>Pre-pandemic median: %{y:.2f}%<extra></extra>",
                  marker = list(color =  line_colours[input$domains_selected],
                                symbol = "arrow-up-open")) |>
        add_trace(y = ~ median / (shift %/% 2),
                  type = "scatter",
                  mode = "markers", 
                  name = "Downwards shift of 6 points or more",
                  text = ~ paste0("<b>All Scotland</b>",
                                  "<br><i>", category, "</i>"),
                  hovertemplate = "%{text}<br>Pre-pandemic median: %{y:.2f}%<extra></extra>",
                  marker = list(color =  line_colours[input$domains_selected],
                                symbol = "arrow-down-open")) |>
        add_trace(y = ~ median / (date <= ymd("2020-02-01")),
                  type = "scatter",
                  mode = "lines",
                  showlegend = FALSE,
                  text = ~ paste0("<b>All Scotland</b>",
                                  "<br><i>", category, "</i>"),
                  hovertemplate = "%{text}<br>Pre-pandemic median: %{y:.2f}%<extra></extra>",
                  line = list(color = line_colours[input$domains_selected])  #match the other present line colour
        )
    }
    
    if(length(input$domains_selected) >= 1) {
      plot |>
        config(displayModeBar = FALSE) |>
        layout(yaxis = list(range = c(0, ymax),
                            title = list(text = "% of reviews")),
               xaxis = list(title = list(text = "Quarter of review")),
               legend = list(itemclick = FALSE,
                             itemdoubleclick = FALSE,
                             title = list(text = "Developmental Domain"))
      )
    }
  })
  
  
  
  ##Concerns by SIMD Quintile ----
  
  output$simd_selector <- renderUI({
    checkboxGroupInput(
      inputId = "simd_levels",
      label = "Select which SIMD quintiles to show. This
              scale ranges from 1 being the most deprived to 5
              being the least deprived.",
      choices = c(1:5),
      selected = selected$simd,
      inline = TRUE
    )
  })
  
  #title of plot changes based on choices
  output$development_concerns_by_simd_title <- renderText({
    paste0("Percentage of children with 1 or more developmental concerns recorded at the ",
           development_data_name(),
           " by SIMD deprivation quintile")
  })
  
  #wrangles the data to be fed into the plot
  development_concerns_by_simd_data <- reactive({
    data <- simd_data[[input$development_data]] |>
      filter(category %in% input$simd_levels)
    
    if(length(input$simd_levels) == 1) {
      data <- data |>
        mutate(median = median(measure[date <= ymd("2020-02-01")], na.rm = TRUE),
               trend = trend(measure),
               shift = shift(measure, median))
    }
    
    return(data)
  })
  
  #creates the plot to be displayed
  output$development_concerns_by_simd_plotly <- renderPlotly({
    
    data <- development_concerns_by_simd_data()
    ymax <- max(data$measure) * 1.05
    
    plot <- plot_ly(data = data,
            x = ~ date) |>
      add_trace(y = ~ measure,
                type = "scatter",
                mode = "lines+markers",
                color = ~ category,
                symbol = ~ category,
                colors = line_colours,
                text = ~ paste0("<b>All Scotland</b>",
                                "<br><i>SIMD quintile: ", category, "</i>",
                                "<br>% of reviews: ", round(measure, 2), "%",
                                "<br>Quarter of review: ", 
                                month(date, label = TRUE), "-", month(date + 62, label = TRUE),
                                " ", year(date)
                ),
                hovertemplate = "%{text}<extra></extra>")
    
    if(length(input$simd_levels) == 1) {
      plot <- plot |>
        add_trace(y = ~ median,
                  type = "scatter",
                  mode = "lines",
                  name = "Pre-pandemic median",
                  text = ~ paste0("<b>All Scotland</b>",
                                  "<br><i>SIMD quintile: ", category, "</i>"),
                  hovertemplate = "%{text}<br>Pre-pandemic median: %{y:.2f}%<extra></extra>",
                  line = list(color = line_colours[input$simd_levels], #match the other present line colour
                              dash = "4")
        ) |>
        add_trace(y = ~ measure / trend,
                  type = "scatter",
                  mode = "lines",
                  name = "Trend of 5 points or more",
                  text = ~ paste0("<b>All Scotland</b>",
                                  "<br><i>SIMD quintile: ", category, "</i>",
                                  "<br>% of reviews: ", round(measure, 2), "%",
                                  "<br>Quarter of review: ", 
                                  month(date, label = TRUE), "-", month(date + 62, label = TRUE),
                                  " ", year(date)
                  ),
                  hovertemplate = "%{text}<extra></extra>",
                  line = list(color = colour_switch(line_colours[input$simd_levels], 0.2), #adds alpha to colour
                              width = 10)
        ) |>
        #mark shifts by arrows around median
        add_trace(y = ~ median / (shift %% 2),
                  type = "scatter",
                  mode = "markers", 
                  name = "Upwards shift of 6 points or more",
                  text = ~ paste0("<b>All Scotland</b>",
                                  "<br><i>SIMD quintile: ", category, "</i>"),
                  hovertemplate = "%{text}<br>Pre-pandemic median: %{y:.2f}%<extra></extra>",
                  marker = list(color =  line_colours[input$simd_levels],
                                symbol = "arrow-up-open")) |>
        add_trace(y = ~ median / (shift %/% 2),
                  type = "scatter",
                  mode = "markers", 
                  name = "Downwards shift of 6 points or more",
                  text = ~ paste0("<b>All Scotland</b>",
                                  "<br><i>SIMD quintile: ", category, "</i>"),
                  hovertemplate = "%{text}<br>Pre-pandemic median: %{y:.2f}%<extra></extra>",
                  marker = list(color =  line_colours[input$simd_levels],
                                symbol = "arrow-down-open")) |>
      add_trace(y = ~ median / (date <= ymd("2020-02-01")),
                type = "scatter",
                mode = "lines",
                showlegend = FALSE,
                text = ~ paste0("<b>All Scotland</b>",
                                "<br><i>SIMD quintile: ", category, "</i>"),
                hovertemplate = "%{text}<br>Pre-pandemic median: %{y:.2f}%<extra></extra>",
                line = list(color = line_colours[input$simd_levels])  #match the other present line colour
      )
    }
    
    if(length(input$simd_levels) >= 1) {
      plot |>
        config(displayModeBar = FALSE) |>
        layout(yaxis = list(range = c(0, ymax),
                            title = list(text = "% of reviews")),
               xaxis = list(title = list(text = "Quarter of review")),
               legend = list(itemclick = FALSE,
                             itemdoubleclick = FALSE,
                             title = list(text = "SIMD Quintile"))
      )
    }
  })
  
  
  
  
  
  
  
  #.----
  
  
  #Downloads ----
  #will get this section working soon!
  output$feeding_download <- downloadHandler(
    
    filename = paste0(extract_date, "_infant_feeding.xlsx"),
    
    content = \(file) {
      openxlsx::saveWorkbook(
        openxlsx::loadWorkbook(paste0("downloads/", extract_date, "_infant_feeding.xlsx")),
        file
      )
    }
  )
  
  output$feeding_comparison_download <- downloadHandler(
    
    filename = paste0(extract_date, "_infant_feeding.xlsx"),
    
    content = \(file) {
      openxlsx::saveWorkbook(
        openxlsx::loadWorkbook(paste0("downloads/", extract_date, "_infant_feeding.xlsx")),
        file
      )
    }
  )

  
  output$development_download <- downloadHandler(
    filename = paste0(extract_date, "_child_development.xlsx"),
    
    content = \(file) {
      openxlsx::saveWorkbook(
        openxlsx::loadWorkbook(paste0("downloads/", extract_date, "_child_development.xlsx")),
        file
      )
    }
  )
  
  output$development_comparison_download <- downloadHandler(
    filename = paste0(extract_date, "_child_development.xlsx"),
    
    content = \(file) {
      openxlsx::saveWorkbook(
        openxlsx::loadWorkbook(paste0("downloads/", extract_date, "_child_development.xlsx")),
        file
      )
    }
  )

  output$domains_download <- downloadHandler(
    filename = paste0(extract_date, "_developmental_domains.xlsx"),
    
    content = \(file) {
      openxlsx::saveWorkbook(
        openxlsx::loadWorkbook(paste0("downloads/", extract_date, "_developmental_domains.xlsx")),
        file
      )
    }
  )

  output$simd_download <- downloadHandler(
    filename = paste0(extract_date, "_SIMD_quintiles.xlsx"),
    
    content = \(file) {
      openxlsx::saveWorkbook(
        openxlsx::loadWorkbook(paste0("downloads/", extract_date, "_SIMD_quintiles.xlsx")),
        file
      )
    }
  )
  
  
  
  
  
  #.----
  # Testing! ----
  # little sidebar dev app to display variables for testing
  # output$testing <- renderPrint({
  #   str_view(c(paste0("A ", input$update_feeding_comparison),
  #              paste0("B ", input$update_development_comparison),
  #              paste0("C ", selected$update)
  #   ))
  # })
  
  
  
}