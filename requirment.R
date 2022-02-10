library(tidyverse)
library(magrittr)
library(ggpubr)
library(ggplot2)
library(EnvStats)
library(hrbrthemes)
library(extrafont)
library(arrangements)
library(ggfortify)
library(cluster)
library(gplots)
library(heatmaply)
library(ggstatsplot)
library(ggrepel)
library(grid)
library(gridExtra)
library(htmlwidgets)
#library(devtools)
library(factoextra)
library(shinyHeatmaply)
library(shiny)
library(shinythemes)
#system('fc-cache -f ~/.fonts')

#lapply(requ, library , character.only = TRUE)

# 
# 
# requirement.install <- function() {
#   options(warn = -1)
#   
#   list.packages <- c('tidyverse','magrittr','ggpubr','dplyr','readxl', 'ggplot2',
#                      'writexl','EnvStats','hrbrthemes','extrafont','arrangements', 'readxl',
#                      'ggfortify','cluster','gplots','preprocessCore','heatmaply','ggstatsplot','ggrepel',
#                      'grid','gridExtra','htmlwidgets','devtools','factoextra','shinyHeatmaply')
#   
#   new.packages <- list.packages[!(list.packages %in% installed.packages()[,'Package'])]
#   if(length(new.packages)) {
#     cat('-- Installing packages,',as.character.Date(Sys.time()),'--\n')
#     install.packages(new.packages, quiet = T, repos = "http://cran.us.r-project.org")
#   }
#   
#   invisible(sapply(list.packages, require, character.only = T))
#   
# }
# 
# 
# shiny.requirement <- function(){
#   options(warn = -1)
#   
#   list.packages <- c('shiny', 'shinythemes')
#   new.packages <- list.packages[!(list.packages %in% installed.packages()[,'Package'])]
#   if(length(new.packages)) {
#     cat('-- Installing packages,',as.character.Date(Sys.time()),'--\n')
#     install.packages(new.packages, quiet = T, repos = "http://cran.us.r-project.org")
#   }
#   
#   invisible(sapply(list.packages, require, character.only = T))
#   
#   
# }
# 
