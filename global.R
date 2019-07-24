library(dplyr)
library(magrittr)
library(WHO)
library(tidyr)
library(DT)
library(plotly)
library(sparkline)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus)
library(htmlwidgets)
library(openxlsx)
library(rmarkdown)


letters_only <- function(x) !grepl("[^A-Za-z]", gsub('[[:punct:] ]+','', x))

numbers_only <- function(x) !grepl("\\D", gsub('[[:punct:] ]+','', x))

clean_value <- function(x){
  
  if(unique(letters_only(x)) == TRUE) {
    
    as.character(tolower(trimws(gsub('[[:punct:] ]+',' ', x))))
    
  }
  
  else if(unique(numbers_only(x)) == TRUE) {
    
    as.numeric(gsub(' ', '', (gsub("\\[[^}]*\\]", "", x))))
    
  }
  
}