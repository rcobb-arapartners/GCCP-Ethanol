# 01-MetaLoad.R
# Minimal libraries and themes only

# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------
library(shiny)
library(shinyjs)
library(bslib)
library(tidyverse)
library(lubridate)
library(gt)
library(plotly)
library(scales)

theme_set(theme_bw())

# ------------------------------------------------------------
# Themes
# ------------------------------------------------------------
light_theme <- bs_theme(version = 5, bootswatch = "flatly")
dark_theme  <- bs_theme(version = 5, bootswatch = "darkly")

# ------------------------------------------------------------
# Simple month code lookups (used throughout the app)
# ------------------------------------------------------------
MONTH_CODES <- c(
  F = 1, G = 2, H = 3, J = 4, K = 5, M = 6,
  N = 7, Q = 8, U = 9, V = 10, X = 11, Z = 12
)

MONTH_NAMES <- c(
  F = "January", G = "February", H = "March", J = "April",
  K = "May", M = "June", N = "July", Q = "August",
  U = "September", V = "October", X = "November", Z = "December"
)

# Ethanol -> Corn month mapping
ETHANOL_TO_CORN_MONTH <- c(
  Z = "H", X = "Z", V = "Z", U = "Z",
  Q = "U", N = "U", M = "N", K = "N",
  J = "K", H = "K", G = "H", F = "H"
)