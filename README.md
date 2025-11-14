
Community Fisheries Dashboard
=============================

Interactive R Shiny dashboard for visualizing small-scale fisheries data, including catch, effort, species composition, CPUE, and geospatial catch locations.

This project is built as part of a fisheries data analysis portfolio.

---

Features
--------
1. Upload CSV  
2. KPI Metrics  
3. Monthly CPUE Trend  
4. Species Composition  
5. Leaflet Map  
6. Data Table
Folder Structure
---------------
Dashboard_fish/
 ├── app.R
 └── data/
     └── catch_data.csv


CSV Required Columns
date, site, gear, species, catch_kg, effort_hours, num_fishers, lat, lng

How to Run
----------
install.packages("shiny")
install.packages("plotly")
install.packages("DT")
install.packages("leaflet")
install.packages("bslib")
setwd("C:/Users/staquio/Documents/Dashboard_fish") : This based on your folder in your computer
library(shiny)
runApp()
---
Contact
Developer: Estaquio Maia dos Reis  
Country: Timor-Leste

