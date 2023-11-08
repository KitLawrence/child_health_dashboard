

loading <- function(whats_loading){
  withSpinner(whats_loading, type = 5, color = "#9B4393", size = 0.5)
}


#function copied from internet that takes a shiny menu and deletes the unnecessary
#div element that could confuse screen readers
accessible_menu = function(bad_menu) {
  tab_input = tags$script(
    "
function customMenuHandleClick(e) {
  let n = $(e.currentTarget).find('a')[0].dataset.value;
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



decreasing_count <- function(x) {
  count <- 0
  if((!is.na(lag(x))) & (lag(x) >= x)){
    count <- count +1
    decreasing_count(lag(x))
  }
  else{
    return(count)
  }
}



trend <- function(x) {
  
  l1 <- lag(x)
  l2 <- lag(l1)
  l3 <- lag(l2)
  l4 <- lag(l3)
  
  
  inc <- (l4 <= l3) & (l3 <= l2) & (l2 <= l1) & (l1 <= x)
  
  inc <- tidyr::replace_na(inc, FALSE)
  inc2 <- lead(inc, default = FALSE)
  inc3 <- lead(inc2, default = FALSE)
  inc4 <- lead(inc3, default = FALSE)
  inc5 <- lead(inc4, default = FALSE)
  
  inc_final <- as.logical(inc + inc2 + inc3 + inc4 + inc5)
  
  
  dec <- (l4 >= l3) & (l3 >= l2) & (l2 >= l1) & (l1 >= x)
  
  dec <- tidyr::replace_na(dec, FALSE)
  dec2 <- lead(dec, default = FALSE)
  dec3 <- lead(dec2, default = FALSE)
  dec4 <- lead(dec3, default = FALSE)
  dec5 <- lead(dec4, default = FALSE)
  
  dec_final <- as.logical(dec + dec2 + dec3 + dec4 + dec5)
  
  
  
  return(inc_final | dec_final)
}


colour_switch <- function(hex) {
  #input format is #abcdef
  #output should be rgba(ab,cd,ef,0.2)
  red_hex <- paste0("0X", substr(hex, 2,3))
  blue_hex <- paste0("0X", substr(hex, 4,5))
  green_hex <- paste0("0X", substr(hex, 6,7))
  
  rgb <- strtoi(c(red_hex, blue_hex, green_hex))
  
  return(paste0("rgba(",
                str_flatten(rgb, ","),
                ",0.2)"))
}




