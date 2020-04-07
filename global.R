############################################################################################
# LOAD REQUIRED PACKAGES
############################################################################################

##rm(list=ls())
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
load("data/test.RData")
pt <- test
# col2display <- names(pt)
# metricCols <- names(pt)[which(sapply(pt, function(x) is.numeric(x)))]
# types <- unique(pt$sample_type)
# 
# STypes <- c(types[c(2:length(types), 1)])
# names(STypes) <- c(types[c(2:length(types), 1)])
# gCols <- c("T13.call", "T18.call", "T21.call","version")
# labs <- c("sample.ID", "run.ID", gCols)
anames <- c("pdate", "sample.ID", "run.ID")

# ## run.ID is must have
# ## pdate, sample
