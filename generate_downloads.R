#START OF SCRIPT ----

#loads in libraries and files to be used
source("global.R")
#remove the things we won't use
rm(list = c(domains, pivoted_data, my_theme, no_cats, perc_cats))
#stop the default of openxlsx putting dates in american format
options("openxlsx.dateFormat" = "dd/mm/yyyy")



