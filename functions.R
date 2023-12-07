
#function adds a loading spinner with phs-magenta colour
#wrap any output in this function to make it show this while it loads
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

#function which removes the aria-label attribute that shiny automatically
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

rem_button_aria_label <- function(box) {
  box[["children"]][[1]][["children"]][[1]][["children"]][[2]][["children"]][[1]][["children"]][[1]][["attribs"]][["aria-label"]] = NULL
  box[["children"]][[1]][["children"]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]][["title"]] = "open and close button" 
  return(box)
}



#function takes a numeric vector x (usually the x-axis in a plot) and outputs a logical 
#column for whether each element of x is part of a run of 5 increasing or decreasing
#values
trend <- function(x) {
  #look at the four values preceding x
  l1 <- lag(x)
  l2 <- lag(l1)
  l3 <- lag(l2)
  l4 <- lag(l3)
  
  #if four values preceding x are all piecewise more than each other then x is the peak of an increasing trend
  inc <- (l4 <= l3) & (l3 <= l2) & (l2 <= l1) & (l1 <= x)
  
  #if the peak of a trend is 4 or less spaces in front of x then x is part of that trend
  inc <- tidyr::replace_na(inc, FALSE)
  inc2 <- lead(inc, default = FALSE)
  inc3 <- lead(inc2, default = FALSE)
  inc4 <- lead(inc3, default = FALSE)
  inc5 <- lead(inc4, default = FALSE)
  inc_final <- as.logical(inc + inc2 + inc3 + inc4 + inc5)
  
  #if four values preceding x are all piecewise less than each other then x is the peak of a decreasing trend
  dec <- (l4 >= l3) & (l3 >= l2) & (l2 >= l1) & (l1 >= x)
  
  #if the peak of a trend is 4 or less spaces in front of x then x is part of that trend
  dec <- tidyr::replace_na(dec, FALSE)
  dec2 <- lead(dec, default = FALSE)
  dec3 <- lead(dec2, default = FALSE)
  dec4 <- lead(dec3, default = FALSE)
  dec5 <- lead(dec4, default = FALSE)
  dec_final <- as.logical(dec + dec2 + dec3 + dec4 + dec5)
  
  #if x is part of an increasing OR decreasing trend then return TRUE, otherwise return FALSE
  return(inc_final | dec_final)
}


shift <- function(x, threshold) {
  
  above <- (x > threshold)
  
  above2 <- lag(above, default = FALSE)
  above3 <- lag(above2, default = FALSE)
  above4 <- lag(above3, default = FALSE)
  above5 <- lag(above4, default = FALSE)
  above6 <- lag(above5, default = FALSE)
  
  shift_up <- ((above + above2 + above3 + above4 + above5 + above6) == 6)
  
  shift_up2 <- lead(shift_up, default = FALSE)
  shift_up3 <- lead(shift_up2, default = FALSE)
  shift_up4 <- lead(shift_up3, default = FALSE)
  shift_up5 <- lead(shift_up4, default = FALSE)
  shift_up6 <- lead(shift_up5, default = FALSE)
  
  shift_up_final <- as.logical(shift_up + shift_up2 + shift_up3 + shift_up4 + shift_up5 + shift_up6)
  
  
  below <- (x < threshold)

  below2 <- lag(below, default = FALSE)
  below3 <- lag(below2, default = FALSE)
  below4 <- lag(below3, default = FALSE)
  below5 <- lag(below4, default = FALSE)
  below6 <- lag(below5, default = FALSE)

  shift_down <- ((below + below2 + below3 + below4 + below5 + below6) == 6)

  shift_down2 <- lead(shift_down, default = FALSE)
  shift_down3 <- lead(shift_down2, default = FALSE)
  shift_down4 <- lead(shift_down3, default = FALSE)
  shift_down5 <- lead(shift_down4, default = FALSE)
  shift_down6 <- lead(shift_down5, default = FALSE)

  shift_down_final <- as.logical(shift_down + shift_down2 + shift_down3 + shift_down4 + shift_down5 + shift_down6)
  
  return(shift_up_final | shift_down_final)
  
}


#this function just changes a colour from a hex code and alpha value to an rgba format
colour_switch <- function(hex, alpha) {
  #input format is #abcdef
  #output should be rgba(ab,cd,ef,0.2)
  red_hex <- paste0("0X", substr(hex, 2,3))
  blue_hex <- paste0("0X", substr(hex, 4,5))
  green_hex <- paste0("0X", substr(hex, 6,7))
  
  rgb <- strtoi(c(red_hex, blue_hex, green_hex)) #turns hex codes into base 10 numbers
  
  return(paste0("rgba(",
                str_flatten(rgb, ","),
                ",", 
                as.character(alpha),
                ")"))
}




