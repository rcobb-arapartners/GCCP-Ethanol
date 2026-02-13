# 01-MetaLoad.R

# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------
library(shiny)
library(shinyjs)
library(bslib)
library(tidyverse)
library(data.table)
library(lubridate)
library(janitor)
library(rvest)
library(DBI)
library(lpSolve)
library(scales)
library(broom)
library(gtsummary)
library(gt)
library(grid)
library(ggExtra)
library(rJava)
library(RJDBC)
library(zoo)
library(glue)
library(plotly)

# ------------------------------------------------------------
# ggplot theme
# ------------------------------------------------------------
theme_set(theme_bw())

# ------------------------------------------------------------
# bslib themes
# ------------------------------------------------------------
light_theme <- bs_theme(version = 5, bootswatch = "flatly")
dark_theme  <- bs_theme(version = 5, bootswatch = "darkly")