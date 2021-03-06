############################################################################################
# LOAD REQUIRED PACKAGES
############################################################################################

rm(list=ls())
library(shiny)
library(shinydashboard)
library(shinyWidgets) 
library(markdown)
library(plotly)
library(DT)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(crosstalk)
library(lubridate)
library(dplyr)
library(arsenal)
library(zoo)
library(shinyjs)


options(shiny.maxRequestSize=30*1024^2) 
options(repos = c(CRAN = "https://cran.rstudio.com")) 

load("data/test.RData")
pt <- test
rm(test)
anames <- c("pdate", "sample.ID", "run.ID")

