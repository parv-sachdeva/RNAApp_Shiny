# Importing all libraries required in analysis
library(shiny)
library(bs4Dash)
library(echarts4r)
library(plotly)
library(tidyverse)
require(DESeq2)
require(data.table)
require(sortable)
require(DT)
library(yaml)
require(httr)
require(XML)
library(heatmaply)

# Setting autoreload to false
options(shiny.autoreload = FALSE)
# Increasing max upload size for large analysis data
options(shiny.maxRequestSize = 300*1024^2)
# options(shiny.fullstacktrace = TRUE)

# Creating tmp directory for logs
if(!dir.exists("tmp")) dir.create("tmp")