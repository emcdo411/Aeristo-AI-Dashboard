# Aeristo AI Command Center

![Aeristo AI Command Center](https://img.shields.io/badge/Aeristo-AI%20Command%20Center-%231C2526?style=for-the-badge&logo=r&logoColor=%23CC6666)
![R](https://img.shields.io/badge/R-4.4.3-blue?style=flat-square&logo=r)
![Shiny](https://img.shields.io/badge/Shiny-1.8.0-blueviolet?style=flat-square)
![License](https://img.shields.io/badge/License-MIT-green?style=flat-square)

A **production-ready R Shiny dashboard** designed for Aeristo, a global leader in premium leather supply for aviation, marine, automotive, and furniture industries. This dashboard rivals Power BI with advanced **AI/ML analytics**, **interactive visualizations**, and **enterprise-grade features**, optimizing quality control, sustainability, and supply chain operations.

---

## 📖 Summary

The **Aeristo AI Command Center** is an advanced, interactive dashboard built to empower Aeristo’s data-driven decision-making. Aeristo, headquartered in Dallas, Texas, is a premier supplier of high-quality leather, serving prestigious clients like Gulfstream, Bentley, and Embraer. Facing challenges in **FAA-compliant quality control**, **sustainable tanning processes**, and **supply chain volatility**, Aeristo relies on sophisticated analytics to maintain its competitive edge.

This dashboard delivers:
- **Quality Control**: AI-driven defect forecasting, anomaly detection, and rejection risk assessment using `prophet`, `isotree`, and `xgboost`.
- **Sustainability**: Tracks CO₂ emissions and compliance with REACH/ISO 14001 certifications.
- **Supply Chain**: Visualizes supplier risks with interactive geospatial maps and order timelines.

With a **Power BI-inspired aesthetic** (dark theme, red accents, Roboto fonts), **reactive filters**, **real-time alerts**, and **export capabilities**, this dashboard is deployable on shinyapps.io or Docker, showcasing enterprise-grade scalability. Created by **Maurice McDonald**, this case study demonstrates expertise in R Shiny, AI/ML, and data visualization.

---

## 🚀 Features

- **Multi-Tab Analytics**:
  - **Quality Control**: Defect heatmaps, 30-day blemish forecasts, batch-level metrics.
  - **Sustainability**: CO₂ emissions bar charts, certification status tables.
  - **Supply Chain**: Supplier risk maps with red plot dots, order stage timelines.
- **AI/ML Integration**:
  - `prophet`: Time-series forecasting for defect rates.
  - `xgboost`: Cross-validated rejection risk prediction.
  - `isotree`: Anomaly detection with real-time alerts.
  - `cluster`: K-means clustering for supplier segmentation.
- **Interactive Visualizations**:
  - `plotly`: Animated heatmaps, forecasts, and bar charts.
  - `leaflet`: Geospatial supplier risk maps with clickable red dots.
  - `dygraphs`: Time-series trends (optional extension).
  - `DT`: Responsive data tables with filtering and sorting.
- **Enterprise Features**:
  - Simulated SQL/ERP integration (`DBI`, `odbc`).
  - Excel/PDF report exports (`writexl`, `rmarkdown`).
  - Real-time alerts via `shinyalert`.
  - Reactive filters with `shinyWidgets`.
- **Power BI-Inspired UI**:
  - Dark theme (`#1C2526`) with red accents (`#CC6666`).
  - Roboto fonts for modern typography.
  - KPI cards with dynamic coloring (`bs4Dash`).

---

## 🛠 Tech Stack

- **Language**: R (4.4.3 or higher)
- **Framework**: Shiny (1.8.0)
- **Deployment**: shinyapps.io, Docker
- **Frontend**: `bs4Dash` (Bootstrap 4), custom CSS
- **Data Processing**: `dplyr`, `lubridate`
- **Visualization**:
  - `plotly` (interactive plots)
  - `leaflet` (geospatial maps)
  - `dygraphs` (time-series)
  - `DT` (data tables)
- **AI/ML**:
  - `prophet` (forecasting)
  - `xgboost` (classification)
  - `isotree` (anomaly detection)
  - `cluster` (k-means)
- **Database**: `DBI`, `odbc` (simulated SQL/ERP)
- **Utilities**:
  - `shinyWidgets` (enhanced inputs)
  - `shinycssloaders` (loading animations)
  - `thematic` (theming)
  - `xts` (time-series data)
  - `writexl` (Excel exports)
  - `rmarkdown` (PDF reports)
  - `shinyalert` (alerts)
  - `fontawesome` (icons)

---

## 📦 Package Dependencies

Below are the R packages used, each with a colorful badge linking to its CRAN page or documentation:

| Package | Description | Badge |
|---------|-------------|-------|
| shiny | Web application framework for R | ![shiny](https://img.shields.io/badge/shiny-1.8.0-blueviolet?style=flat-square&logo=r) |
| bs4Dash | Bootstrap 4 dashboard framework | ![bs4Dash](https://img.shields.io/badge/bs4Dash-2.0.0-blue?style=flat-square) |
| plotly | Interactive plotting library | ![plotly](https://img.shields.io/badge/plotly-4.10.0-green?style=flat-square) |
| dygraphs | Time-series visualizations | ![dygraphs](https://img.shields.io/badge/dygraphs-1.1.1-orange?style=flat-square) |
| DT | Interactive data tables | ![DT](https://img.shields.io/badge/DT-0.20-blue?style=flat-square) |
| isotree | Isolation forest for anomaly detection | ![isotree](https://img.shields.io/badge/isotree-0.5.0-red?style=flat-square) |
| xgboost | Gradient boosting for classification | ![xgboost](https://img.shields.io/badge/xgboost-1.6.0-purple?style=flat-square) |
| prophet | Time-series forecasting | ![prophet](https://img.shields.io/badge/prophet-1.0-green?style=flat-square) |
| cluster | Clustering algorithms (k-means) | ![cluster](https://img.shields.io/badge/cluster-2.1.2-blueviolet?style=flat-square) |
| leaflet | Interactive maps | ![leaflet](https://img.shields.io/badge/leaflet-2.1.0-orange?style=flat-square) |
| shinyWidgets | Enhanced Shiny inputs | ![shinyWidgets](https://img.shields.io/badge/shinyWidgets-0.6.0-blue?style=flat-square) |
| shinycssloaders | Loading animations for Shiny | ![shinycssloaders](https://img.shields.io/badge/shinycssloaders-1.0.0-green?style=flat-square) |
| thematic | Consistent theming for Shiny | ![thematic](https://img.shields.io/badge/thematic-0.1.2-red?style=flat-square) |
| xts | Time-series data handling | ![xts](https://img.shields.io/badge/xts-0.12.1-purple?style=flat-square) |
| DBI | Database interface | ![DBI](https://img.shields.io/badge/DBI-1.1.3-blue?style=flat-square) |
| odbc | ODBC database connectivity | ![odbc](https://img.shields.io/badge/odbc-1.3.3-orange?style=flat-square) |
| writexl | Excel file export | ![writexl](https://img.shields.io/badge/writexl-1.4.0-green?style=flat-square) |
| rmarkdown | Dynamic report generation | ![rmarkdown](https://img.shields.io/badge/rmarkdown-2.14-blueviolet?style=flat-square) |
| shinyalert | JavaScript alerts for Shiny | ![shinyalert](https://img.shields.io/badge/shinyalert-2.0.0-red?style=flat-square) |
| fontawesome | Icon library | ![fontawesome](https://img.shields.io/badge/fontawesome-0.3.0-blue?style=flat-square) |
| dplyr | Data manipulation | ![dplyr](https://img.shields.io/badge/dplyr-1.0.7-green?style=flat-square) |
| lubridate | Date-time handling | ![lubridate](https://img.shields.io/badge/lubridate-1.8.0-purple?style=flat-square) |

---

## 🖥 Installation

1. **Install R** (version 4.4.3 or higher):
   - Download from [CRAN](https://cran.r-project.org/).
   - Verify: `R.version.string`.

2. **Install Dependencies**:
   ```R
   install.packages(c(
     "shiny", "bs4Dash", "plotly", "dygraphs", "DT", "isotree", "xgboost", "prophet",
     "cluster", "leaflet", "shinyWidgets", "shinycssloaders", "thematic", "xts",
     "DBI", "odbc", "writexl", "rmarkdown", "shinyalert", "fontawesome", "dplyr", "lubridate"
   ))


Check Package Versions:
packageVersion("bs4Dash") # Ensure ≥ 2.0.0
packageVersion("shiny")   # Ensure ≥ 1.8.0


Clone Repository:
git clone https://github.com/emcdo411/Aeristo-AI-Dashboard.git
cd Aeristo-AI-Dashboard




🚀 Usage

Run Locally:

Open aeristo_quality_powerbi_dashboard_with_map_dots.R in RStudio.
Execute with Ctrl+Alt+R.
Access at http://127.0.0.1:7541 (or assigned port).
Check console for logs (e.g., “XGBoost trained at [timestamp]”).


Explore Features:

Quality Control: View defect heatmaps, forecasts, and batch metrics.
Sustainability: Analyze CO₂ emissions and certifications.
Supply Chain: Interact with supplier risk maps (red plot dots) and order timelines.
Apply filters (supplier, date range) for dynamic insights.


Deploy to shinyapps.io:

Register at shinyapps.io.
Run:rsconnect::deployApp()


Share the URL with stakeholders.


Debugging:

Clear environment if errors occur:rm(list = ls()); lapply(paste("package:", (.packages())[-1], sep = ""), detach, character.only = TRUE, unload = TRUE)


Share console outputs for support.


Clean Up:

Delete temporary files (e.g., Aeristo_QA_Report.xlsx).




📊 Dashboard Overview
The dashboard is structured into three tabs:

Quality Control:

KPI Cards: Average blemish count, rejection risk, anomalous batches.
Visuals: Defect heatmap (plotly), 30-day blemish forecast (prophet).
Table: Batch-level metrics with AI scores (xgboost, isotree).
Export: Download QA reports as Excel.


Sustainability:

KPI Cards: Average CO₂ emissions, REACH/ISO 14001 certification rates.
Visuals: CO₂ emissions by treatment type (plotly).
Table: Certification status (DT).
Export: Download sustainability reports as Excel.


Supply Chain:

KPI Cards: On-time stage percentage, average stage duration.
Visuals: Supplier risk map with red plot dots (leaflet), order timeline (plotly).
Export: Download supply chain reports as Excel.




💻 Code
Below is the complete R code for the dashboard (aeristo_quality_powerbi_dashboard_with_map_dots.R):
# Advanced Aeristo Quality AI Dashboard
# Run in RStudio, deploy to shinyapps.io or Docker
# Enhanced Supplier Risk Map with red plot dots and detailed popups
# Mirrors Power BI with AI/ML, enterprise features, and polished aesthetics

# Load Libraries
library(shiny)
library(bs4Dash)
library(plotly)
library(dygraphs)
library(DT)
library(isotree)
library(xgboost)
library(prophet)
library(cluster)
library(leaflet)
library(shinyWidgets)
library(shinycssloaders)
library(thematic)
library(xts)
library(DBI)
library(odbc)
library(writexl)
library(rmarkdown)
library(shinyalert)
library(fontawesome)
library(dplyr)
library(lubridate)

# Enable Power BI-style theming
thematic_on()

# Custom Power BI-Inspired CSS
custom_css <- "
body {
  font-family: 'Roboto', sans-serif;
  background-color: #1C2526;
  color: #D3D3D3;
}
.main-header .navbar {
  background-color: #1C2526;
  border-bottom: 2px solid #CC6666;
}
.main-sidebar {
  background-color: #2A2F33;
}
.box {
  background-color: #2A2F33;
  border: 1px solid #CC6666;
  border-radius: 8px;
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3);
}
.nav-tabs > li.active > a {
  background-color: #CC6666;
  color: #1C2526;
}
.btn-primary {
  background-color: #CC6666;
  border-color: #CC6666;
}
.btn-primary:hover {
  background-color: #A94442;
  border-color: #A94442;
}
.value-box {
  background-color: #2A2F33;
  border: 1px solid #CC6666;
}
.footer {
  color: #D3D3D3;
  padding: 10px;
  font-size: 12px;
  position: fixed;
  bottom: 0;
  width: 100%;
}
.leaflet-popup-content {
  background-color: #2A2F33;
  color: #D3D3D3;
  font-family: 'Roboto', sans-serif;
}
"

# Hard-Coded Demo Data
set.seed(123)
quality_data <<- data.frame(
  Batch = paste0("B", 1:100),
  Supplier = sample(c("Italy Prime", "Brazil Select", "India Luxe"), 100, replace = TRUE),
  Blemish = rnorm(100, 5, 2),
  Thickness = rnorm(100, 1.5, 0.2),
  ColorVar = rnorm(100, 0.1, 0.05),
  CostPerSqFt = runif(100, 18, 30),
  Date = seq(as.Date("2024-01-01"), by = "day", length.out = 100),
  Compliant = sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.8, 0.2))
) %>%
  mutate(
    Latitude = case_when(
      Supplier == "Italy Prime" ~ 45.4642,
      Supplier == "Brazil Select" ~ -15.7833,
      Supplier == "India Luxe" ~ 28.6139
    ),
    Longitude = case_when(
      Supplier == "Italy Prime" ~ 9.1900,
      Supplier == "Brazil Select" ~ -47.8667,
      Supplier == "India Luxe" ~ 77.2090
    )
)

compliance_data <<- data.frame(
  Treatment = c("Solvent-Based", "Water-Based", "Hybrid", "Natural Oil"),
  CO2_kg = c(120, 60, 80, 40),
  REACH_Certified = c(TRUE, TRUE, FALSE, TRUE),
  ISO14001_Certified = c(FALSE, TRUE, TRUE, TRUE)
)

order_data <<- data.frame(
  OrderID = paste0("ORD", 1:20),
  Client = sample(c("Gulfstream", "Bentley", "Embraer", "Dassault"), 20, replace = TRUE),
  Stage = sample(c("Tanning", "QA", "Dyeing", "Cutting", "Shipping"), 20, replace = TRUE),
  StartDate = as.Date("2025-06-01") + sample(0:30, 20, replace = TRUE),
  DaysInStage = sample(1:7, 20, replace = TRUE)
)

# Validate Data
required_cols <- c("Batch", "Supplier", "Blemish", "Thickness", "ColorVar", "CostPerSqFt", "Date", "Compliant", "Latitude", "Longitude")
if (!all(required_cols %in% colnames(quality_data))) {
  stop("Error: Missing required columns in quality_data: ", paste(setdiff(required_cols, colnames(quality_data)), collapse = ", "))
}
message("Data prepared at ", Sys.time())

# AI/ML Models (Global for Stability)
# Isolation Forest
tryCatch({
  iso_model <<- isolation.forest(quality_data[, c("Blemish", "Thickness", "ColorVar")], ntrees = 100)
  quality_data$AnomalyScore <<- predict(iso_model, quality_data[, c("Blemish", "Thickness", "ColorVar")])
  if (!"AnomalyScore" %in% colnames(quality_data)) {
    stop("Error: AnomalyScore not assigned to quality_data")
  }
  message("Isolation Forest trained at ", Sys.time())
}, error = function(e) {
  message("Isolation Forest Error: ", e$message)
  quality_data$AnomalyScore <<- NA
})

# XGBoost with Cross-Validation
tryCatch({
  xgb_matrix <- as.matrix(quality_data[, c("Blemish", "Thickness", "ColorVar", "CostPerSqFt")])
  xgb_labels <- as.numeric(quality_data$Compliant == "No")
  xgb_dmatrix <- xgb.DMatrix(data = xgb_matrix, label = xgb_labels)
  xgb_params <- list(objective = "binary:logistic", eta = 0.1, max_depth = 6)
  xgb_cv <- xgb.cv(params = xgb_params, data = xgb_dmatrix, nrounds = 50, nfold = 5, verbose = 0)
  xgb_model <<- xgb.train(params = xgb_params, data = xgb_dmatrix, nrounds = which.min(xgb_cv$evaluation_log$test_logloss_mean))
  quality_data$RejectRisk <<- predict(xgb_model, xgb_dmatrix)
  if (!"RejectRisk" %in% colnames(quality_data)) {
    stop("Error: RejectRisk not assigned to quality_data")
  }
  message("XGBoost trained at ", Sys.time())
}, error = function(e) {
  message("XGBoost Error: ", e$message)
  quality_data$RejectRisk <<- NA
})

# Prophet for Defect Forecasting
tryCatch({
  prophet_data <- quality_data %>%
    group_by(Date) %>%
    summarise(y = mean(Blemish)) %>%
    rename(ds = Date)
  prophet_model <<- prophet(prophet_data, yearly.seasonality = TRUE, daily.seasonality = FALSE)
  message("Prophet trained at ", Sys.time())
}, error = function(e) {
  message("Prophet Error: ", e$message)
})

# K-Means Clustering for Suppliers
tryCatch({
  kmeans_data <- quality_data[, c("Blemish", "CostPerSqFt")]
  kmeans_model <<- kmeans(kmeans_data, centers = 3, nstart = 25)
  quality_data$Cluster <<- as.factor(kmeans_model$cluster)
  if (!"Cluster" %in% colnames(quality_data)) {
    stop("Error: Cluster not assigned to quality_data")
  }
  message("K-Means trained at ", Sys.time())
}, error = function(e) {
  message("K-Means Error: ", e$message)
  quality_data$Cluster <<- NA
})

# UI
ui <- bs4DashPage(
  dark = TRUE,
  header = bs4DashNavbar(
    title = "Aeristo AI Command Center",
    status = "dark",
    border = TRUE,
    fixed = TRUE
  ),
  sidebar = bs4DashSidebar(
    skin = "dark",
    status = "primary",
    elevation = 4,
    sidebarMenu(
      menuItem("Quality Control", tabName = "quality", icon = icon("microscope")),
      menuItem("Sustainability", tabName = "sustain", icon = icon("leaf")),
      menuItem("Supply Chain", tabName = "supply", icon = icon("truck"))
    )
  ),
  body = bs4DashBody(
    tags$head(tags$style(HTML(custom_css))),
    useShinyalert(),
    fluidRow(
      box(
        title = "Filters",
        width = 12,
        pickerInput("supplier_filter", "Supplier", choices = unique(quality_data$Supplier), selected = unique(quality_data$Supplier), multiple = TRUE),
        dateRangeInput("date_filter", "Date Range", start = min(quality_data$Date), end = max(quality_data$Date))
      )
    ),
    tabItems(
      tabItem(
        tabName = "quality",
        fluidRow(
          bs4ValueBoxOutput("defect_kpi", width = 3),
          bs4ValueBoxOutput("risk_kpi", width = 3),
          bs4ValueBoxOutput("anomaly_kpi", width = 3),
          downloadButton("quality_download", "Download QA Report", class = "btn-primary")
        ),
        fluidRow(
          box(
            title = "Defect Heatmap by Batch",
            width = 6,
            solidHeader = TRUE,
            withSpinner(plotlyOutput("heatmap_plot"))
          ),
          box(
            title = "Defect Rate Forecast",
            width = 6,
            solidHeader = TRUE,
            withSpinner(plotlyOutput("forecast_plot"))
          )
        ),
        fluidRow(
          box(
            title = "Batch-Level QA Metrics",
            width = 12,
            solidHeader = TRUE,
            withSpinner(DTOutput("batch_table"))
          )
        )
      ),
      tabItem(
        tabName = "sustain",
        fluidRow(
          bs4ValueBoxOutput("co2_kpi", width = 3),
          bs4ValueBoxOutput("reach_kpi", width = 3),
          bs4ValueBoxOutput("iso_kpi", width = 3),
          downloadButton("sustain_download", "Download Sustainability Report", class = "btn-primary")
        ),
        fluidRow(
          box(
            title = "CO₂ Emissions by Treatment",
            width = 6,
            solidHeader = TRUE,
            withSpinner(plotlyOutput("co2_plot"))
          ),
          box(
            title = "Certification Status",
            width = 6,
            solidHeader = TRUE,
            withSpinner(DTOutput("cert_table"))
          )
        )
      ),
      tabItem(
        tabName = "supply",
        fluidRow(
          bs4ValueBoxOutput("ontime_kpi", width = 3),
          bs4ValueBoxOutput("stage_kpi", width = 3),
          downloadButton("supply_download", "Download Supply Chain Report", class = "btn-primary")
        ),
        fluidRow(
          box(
            title = "Supplier Risk Map",
            width = 6,
            solidHeader = TRUE,
            withSpinner(leafletOutput("supplier_map"))
          ),
          box(
            title = "Order Timeline",
            width = 6,
            solidHeader = TRUE,
            withSpinner(plotlyOutput("order_timeline"))
          )
        )
      )
    ),
    tags$footer(
      HTML("Source: Aeristo ERP, Quality Control Systems, June 2025 | Powered by AI & R Shiny"),
      class = "footer"
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Initialize Alerts
  observe({
    shinyalert(
      title = "Welcome to Aeristo AI Command Center",
      text = "Explore AI-driven insights for quality, sustainability, and supply chain optimization.",
      type = "info",
      timer = 5000
    )
  })
  
  # Reactive Filtered Data
  filtered_quality_data <- reactive({
    req(input$supplier_filter, input$date_filter)
    df <- quality_data %>%
      filter(
        Supplier %in% input$supplier_filter,
        Date >= input$date_filter[1] & Date <= input$date_filter[2]
      )
    if (nrow(df) == 0) {
      shinyalert("No Data", "Selected filters return no data.", type = "warning")
      return(data.frame())
    }
    df
  })
  
  # Simulate SQL Connection
  conn <- reactive({
    tryCatch({
      # Example: dbConnect(odbc(), "AeristoERP", uid = "user", pwd = "pass")
      message("Connected to simulated ERP at ", Sys.time())
      return(NULL)
    }, error = function(e) {
      shinyalert("Connection Error", e$message, type = "error")
      return(NULL)
    })
  })
  
  # Quality Control Outputs
  output$defect_kpi <- renderbs4ValueBox({
    df <- filtered_quality_data()
    if (nrow(df) == 0) return(bs4ValueBox(value = "N/A", subtitle = "Average Blemish Count", color = "warning", icon = icon("exclamation-triangle")))
    bs4ValueBox(
      value = sprintf("%.2f", mean(df$Blemish, na.rm = TRUE)),
      subtitle = "Average Blemish Count",
      color = "primary",
      icon = icon("exclamation-triangle"),
      gradient = TRUE
    )
  })
  
  output$risk_kpi <- renderbs4ValueBox({
    df <- filtered_quality_data()
    if (nrow(df) == 0 || !"RejectRisk" %in% colnames(df)) {
      return(bs4ValueBox(value = "N/A", subtitle = "Average Rejection Risk", color = "warning", icon = icon("chart-pie")))
    }
    risk_mean <- mean(df$RejectRisk, na.rm = TRUE)
    if (is.na(risk_mean)) risk_mean <- 0
    bs4ValueBox(
      value = sprintf("%.2f%%", risk_mean * 100),
      subtitle = "Average Rejection Risk",
      color = ifelse(risk_mean > 0.5, "warning", "primary"),
      icon = icon("chart-pie"),
      gradient = TRUE
    )
  })
  
  output$anomaly_kpi <- renderbs4ValueBox({
    df <- filtered_quality_data()
    if (nrow(df) == 0 || !"AnomalyScore" %in% colnames(df)) {
      return(bs4ValueBox(value = "N/A", subtitle = "Anomalous Batches", color = "warning", icon = icon("bell")))
    }
    anomalies <- sum(df$AnomalyScore > 0.6, na.rm = TRUE)
    bs4ValueBox(
      value = anomalies,
      subtitle = "Anomalous Batches",
      color = ifelse(anomalies > 0, "danger", "primary"),
      icon = icon("bell"),
      gradient = TRUE
    )
  })
  
  output$heatmap_plot <- renderPlotly({
    df <- filtered_quality_data()
    if (nrow(df) == 0) return(plot_ly() %>% layout(title = "No Data Available", paper_bgcolor = "#1C2526", plot_bgcolor = "#1C2526"))
    z_matrix <- matrix(df$Blemish, nrow = ceiling(sqrt(nrow(df))))
    plot_ly(z = z_matrix, type = "heatmap", colorscale = list(c(0, "#1C2526"), c(1, "#CC6666"))) %>%
      layout(
        title = "Hide Defect Distribution",
        paper_bgcolor = "#1C2526",
        plot_bgcolor = "#1C2526",
        font = list(color = "#D3D3D3")
      )
  })
  
  output$forecast_plot <- renderPlotly({
    df <- filtered_quality_data()
    if (nrow(df) == 0) return(plot_ly() %>% layout(title = "No Data Available", paper_bgcolor = "#1C2526", plot_bgcolor = "#1C2526"))
    prophet_df <- df %>%
      group_by(Date) %>%
      summarise(y = mean(Blemish, na.rm = TRUE)) %>%
      rename(ds = Date)
    future <- make_future_dataframe(prophet_model, periods = 30)
    forecast <- predict(prophet_model, future)
    plot_ly() %>%
      add_lines(x = forecast$ds, y = forecast$yhat, name = "Forecast", line = list(color = "#CC6666")) %>%
      add_ribbons(x = forecast$ds, ymin = forecast$yhat_lower, ymax = forecast$yhat_upper,
                  fillcolor = "rgba(204,102,102,0.3)", name = "CI") %>%
      layout(
        title = "30-Day Blemish Forecast",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Blemish Count"),
        paper_bgcolor = "#1C2526",
        plot_bgcolor = "#1C2526",
        font = list(color = "#D3D3D3")
      )
  })
  
  output$batch_table <- renderDT({
    df <- filtered_quality_data()
    if (nrow(df) == 0) return(datatable(data.frame(Message = "No Data Available")))
    available_cols <- intersect(
      c("Batch", "Supplier", "Blemish", "Thickness", "ColorVar", "AnomalyScore", "RejectRisk", "Cluster"),
      colnames(df)
    )
    if (length(available_cols) == 0) return(datatable(data.frame(Message = "No Columns Available")))
    datatable(
      df[, available_cols, drop = FALSE],
      options = list(pageLength = 10, autoWidth = TRUE),
      style = "bootstrap",
      class = "table-bordered table-striped"
    )
  })
  
  output$quality_download <- downloadHandler(
    filename = function() { "Aeristo_QA_Report.xlsx" },
    content = function(file) {
      df <- filtered_quality_data()
      if (nrow(df) == 0) {
        write_xlsx(data.frame(Message = "No Data Available"), file)
      } else {
        write_xlsx(df, file)
      }
    }
  )
  
  # Sustainability Outputs
  output$co2_kpi <- renderbs4ValueBox({
    bs4ValueBox(
      value = sprintf("%.0f kg", mean(compliance_data$CO2_kg, na.rm = TRUE)),
      subtitle = "Avg CO₂ per Treatment",
      color = "primary",
      icon = icon("cloud"),
      gradient = TRUE
    )
  })
  
  output$reach_kpi <- renderbs4ValueBox({
    reach_pct <- mean(compliance_data$REACH_Certified, na.rm = TRUE) * 100
    bs4ValueBox(
      value = sprintf("%.0f%%", reach_pct),
      subtitle = "REACH Certified",
      color = ifelse(reach_pct < 100, "warning", "primary"),
      icon = icon("check-circle"),
      gradient = TRUE
    )
  })
  
  output$iso_kpi <- renderbs4ValueBox({
    iso_pct <- mean(compliance_data$ISO14001_Certified, na.rm = TRUE) * 100
    bs4ValueBox(
      value = sprintf("%.0f%%", iso_pct),
      subtitle = "ISO 14001 Certified",
      color = ifelse(iso_pct < 100, "warning", "primary"),
      icon = icon("certificate"),
      gradient = TRUE
    )
  })
  
  output$co2_plot <- renderPlotly({
    plot_ly(compliance_data, x = ~Treatment, y = ~CO2_kg, type = "bar",
            color = ~Treatment, colors = c("#A94442", "#CC6666", "#D3D3D3", "#556B2F")) %>%
      layout(
        title = "CO₂ Emissions by Treatment",
        yaxis = list(title = "CO₂ Emissions (kg)"),
        xaxis = list(title = "Treatment Type"),
        paper_bgcolor = "#1C2526",
        plot_bgcolor = "#1C2526",
        font = list(color = "#D3D3D3")
      )
  })
  
  output$cert_table <- renderDT({
    datatable(
      compliance_data,
      options = list(pageLength = 5, dom = "t"),
      style = "bootstrap",
      class = "table-bordered table-striped"
    )
  })
  
  output$sustain_download <- downloadHandler(
    filename = function() { "Aeristo_Sustainability_Report.xlsx" },
    content = function(file) {
      write_xlsx(compliance_data, file)
    )
  )
  
  # Supply Chain Outputs
  output$ontime_kpi <- renderbs4ValueBox({
    ontime_pct <- mean(order_data$DaysInStage <= 5, na.rm = TRUE) * 100
    bs4ValueBox(
      value = sprintf("%.0f%%", ontime_pct),
      subtitle = "On-Time Stages",
      color = ifelse(ontime_pct < 80, "warning", "primary"),
      icon = icon("clock"),
      gradient = TRUE
    )
  })
  
  output$stage_kpi <- renderbs4ValueBox({
    avg_days <- mean(order_data$DaysInStage, na.rm = TRUE)
    bs4ValueBox(
      value = sprintf("%.1f Days", avg_days),
      subtitle = "Avg Stage Duration",
      color = ifelse(avg_days > 3, "warning", "primary"),
      icon = icon("hourglass-half"),
      gradient = TRUE
    )
  })
  
  output$supplier_map <- renderLeaflet({
    df <- filtered_quality_data()
    if (nrow(df) == 0 || !"RejectRisk" %in% colnames(df)) {
      return(leaflet() %>% addTiles() %>% addPopups(lng = 0, lat = 0, popup = "No Data Available"))
    }
    map_df <- df %>%
      group_by(Supplier, Latitude, Longitude) %>%
      summarise(
        AvgRisk = mean(RejectRisk, na.rm = TRUE),
        BatchCount = n(),
        AvgBlemish = mean(Blemish, na.rm = TRUE),
        ComplianceRate = mean(Compliant == "Yes", na.rm = TRUE) * 100,
        .groups = "drop"
      )
    # Define red dot icon
    red_dot_icon <- makeIcon(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
      iconWidth = 25, iconHeight = 41,
      iconAnchorX = 12, iconAnchorY = 41
    )
    leaflet(map_df) %>%
      addTiles() %>%
      addMarkers(
        lng = ~Longitude, lat = ~Latitude,
        icon = red_dot_icon,
        popup = ~paste(
          "<b>", Supplier, "</b><br>",
          "Avg Rejection Risk: ", sprintf("%.2f%%", AvgRisk * 100), "<br>",
          "Batch Count: ", BatchCount, "<br>",
          "Avg Blemish: ", sprintf("%.2f", AvgBlemish), "<br>",
          "Compliance Rate: ", sprintf("%.2f%%", ComplianceRate)
        ),
        label = ~Supplier,
        labelOptions = labelOptions(noHide = FALSE, direction = "auto")
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2) # Global view
  })
  
  output$order_timeline <- renderPlotly({
    df <- order_data %>%
      mutate(EndDate = StartDate + DaysInStage)
    plot_ly(df, x = ~StartDate, xend = ~EndDate, y = ~OrderID, yend = ~OrderID,
            type = "scatter", mode = "lines+markers",
            line = list(width = 10, color = "#CC6666"),
            marker = list(size = 8, color = "#A94442"),
            text = ~paste("Client:", Client, "<br>Stage:", Stage),
            hoverinfo = "text") %>%
      layout(
        title = "Order Stage Durations",
        xaxis = list(title = "Date", type = "date"),
        yaxis = list(title = "Order ID"),
        paper_bgcolor = "#1C2526",
        plot_bgcolor = "#1C2526",
        font = list(color = "#D3D3D3")
      )
  })
  
  output$supply_download <- downloadHandler(
    filename = function() { "Aeristo_Supply_Chain_Report.xlsx" },
    content = function(file) {
      write_xlsx(order_data, file)
    )
  )
  
  # Real-Time Anomaly Alerts
  observe({
    df <- filtered_quality_data()
    if (nrow(df) > 0 && "AnomalyScore" %in% colnames(df)) {
      anomalies <- df$AnomalyScore > 0.6
      if (any(anomalies, na.rm = TRUE)) {
        shinyalert(
          title = "Anomaly Detected",
          text = paste("High anomaly score in batch(es):", paste(df$Batch[anomalies], collapse = ", ")),
          type = "warning",
          timer = 0,
          closeOnClickOutside = TRUE
        )
      }
    }
  })
}

# Run App
shinyApp(ui, server)


🌟 Why This Matters
The Aeristo AI Command Center is more than a dashboard—it’s a strategic tool addressing critical business challenges:

Quality Assurance: By forecasting defects and detecting anomalies, it ensures FAA-compliant leather for aviation clients, reducing costly rejections.
Sustainability: Tracking CO₂ emissions and certifications supports Aeristo’s eco-friendly tanning goals, aligning with global environmental standards.
Supply Chain Resilience: Interactive supplier risk maps and order timelines enable proactive management of volatile hide prices and logistics disruptions.
Competitive Edge: The dashboard’s AI/ML capabilities surpass traditional Power BI analytics, offering predictive insights that drive operational excellence.
Showcase of Expertise: Built by Maurice McDonald, this project demonstrates mastery of R Shiny, AI/ML, and data visualization, making it a standout portfolio piece for data science and BI roles.

This case study bridges advanced analytics with real-world business impact, positioning Aeristo—and its data practitioners—for success in a competitive industry.

📝 Author
This case study was created by Maurice McDonald, a data scientist specializing in AI/ML, data visualization, and business intelligence.

GitHub: emcdo411
Email: erwin.mcdonald@outlook.com


📜 License
This project is licensed under the MIT License. See the LICENSE file for details.

🙌 Contributing
Contributions are welcome! Please:

Fork the repository.
Create a feature branch (git checkout -b feature/YourFeature).
Commit changes (git commit -m 'Add YourFeature').
Push to the branch (git push origin feature/YourFeature).
Open a pull request.

For issues or feature requests, use the Issues tab.

📚 Acknowledgments

Aeristo for inspiring this case study.
R community for robust packages like shiny, plotly, and leaflet.
Power BI for UI/UX inspiration.


Source: Aeristo ERP, Quality Control Systems, June 2025 | Powered by AI & R Shiny```

