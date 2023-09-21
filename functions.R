
#function copied from internet that takes a shiny menu and deletes the unnecessary
#div element that could confuse screen readers
accessible_menu = function(bad_menu) {
  tab_input = tags$script(
    "
function customMenuHandleClick(e) {
  let n = $(e.target).parents('ul.sidebar-menu').find('li.active:not(.treeview)').children('a')[0].dataset.value;
  doSomethingWith(n);
}
function doSomethingWith(val) {
  Shiny.setInputValue('sidebarMenu', val);
}
$(document).ready(
  function() {
    $('ul.sidebar-menu li').click(customMenuHandleClick)
  });
"
  )
  bad_menu$children[[length(bad_menu$children)]] = NULL
  real_menu = tagList(bad_menu, tab_input)
  real_menu
}

#function which removes the ara-label attribute that shiny automatically
#puts on icons, despite it often not being needed
rem_aria_label <- function(icon) {
  icon[["attribs"]][["aria-label"]] = NULL
  return(icon)
}

#similar function to the above, but it operates on a tree-view menu
rem_menu_aria_label <- function(menu) {
  menu[["children"]][[1]][["children"]][[3]][["attribs"]][["aria-label"]] = NULL
  return(menu)
}





create_runchart <- function(dataset, geog, variable) {
  dataset |>
  filter(geography %in% geog) |>
  ggplot(aes(x = month_review, y =  {{variable}} )) +
  geom_line()
}



create_runchart_plotly <- function(dataset, geog, variable) {
  dataset <- dataset |>
    filter(geography %in% geog)
  
  plot_ly(data = dataset,
          x =  ~ month_review,
          y =  dataset[[variable]],
          type = "scatter",
          mode = "lines") |>
    config(displayModeBar = FALSE)
}



