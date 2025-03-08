library(shiny)
library(ggplot2)
library(dplyr)
library(dbplyr)
library(lubridate)
library(bigrquery)
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)

mimic_icu_cohort <- readRDS("mimic_icu_cohort.rds")

satoken <- "biostat-203b-2025-winter-4e58ec6e5579.json"

bq_auth(path = satoken)

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter"
)

chartevents <- tbl(con_bq, "chartevents") |> arrange(subject_id)
d_items <- tbl(con_bq, "d_items") 
icustays_tble <- tbl(con_bq, "icustays") |> 
  arrange(subject_id, hadm_id, stay_id) 
lab_results_tble <- tbl(con_bq, "labevents") |> arrange(subject_id) 
diagnoses_codes_tble <- tbl(con_bq, "d_icd_diagnoses")
medical_procedures_tble <- tbl(con_bq, "procedures_icd") |> arrange(subject_id)
patient_diagnoses_tble <- tbl(con_bq, "diagnoses_icd") |> arrange(subject_id)
procedure_codes_tble <- tbl(con_bq, "d_icd_procedures")
patient_transfers_tble <- tbl(con_bq, "transfers") |> arrange(subject_id) 
admissions_tble <- tbl(con_bq, "admissions") |> arrange(subject_id, hadm_id) 
patients_tble <- tbl(con_bq, "patients") |> arrange(subject_id)
  
ui <- fluidPage(
  titlePanel("ICU Cohort Data"),
  
  tabsetPanel(
    tabPanel("Summary",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_group", "Variable Group:",
                             choices = c("Demographic", "Lab Measurements", 
                                         "Vitals", "ICU Stay Information")),
                 
                 selectInput("variable", "Variable:", choices = NULL),
                 
                 uiOutput("outlier_checkbox")  
               ),
               
               mainPanel(
                 plotOutput("plot"),
                 tableOutput("summary_table")
               )
             )
    ),
    tabPanel("Patient Info",
             sidebarLayout(
               sidebarPanel(
                 numericInput("subject_id", "Subject Id:", value = NULL),
                 radioButtons("plot_type", "Choose a plot type:",
                              choices = c("ADT History" = "adt", 
                                          "ICU Stay Record" = "icu")),
                 actionButton("submit", "Submit")
               ),
               mainPanel(
                 uiOutput("selected_plot_ui")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  demographic_vars <- c("Race" = "race", "Insurance" = "insurance", 
                        "Marital Status" = "marital_status", 
                        "Gender" = "gender", 
                        "Age at Intime" = "age_at_intime")
  
  lab_vars <- c("Bicarbonate" = "bicarbonate", "Chloride" = "chloride", 
                "Creatinine" = "creatinine", "Glucose" = "glucose", 
                "Potassium" = "potassium", "Sodium" = "sodium", 
                "Hematocrit" = "hematocrit", "WBC" = "wbc")
  
  vitals_vars <- c("Temperature (°F)" = "temperature_fahrenheit", 
                   "Systolic BP" = "non_invasive_blood_pressure_systolic", 
                   "Respiratory Rate" = "respiratory_rate", 
                   "Diastolic BP" = "non_invasive_blood_pressure_diastolic", 
                   "Heart Rate" = "heart_rate")
  
  icu_vars <- c("Last Care Unit" = "last_careunit")
  
  observeEvent(input$variable_group, {
    updateSelectInput(session, "variable",
                      choices = switch(input$variable_group,
                                       "Demographic" = demographic_vars,
                                       "Lab Measurements" = lab_vars,
                                       "Vitals" = vitals_vars,
                                       "ICU Stay Information" = icu_vars
                      ))
  })
  
  output$outlier_checkbox <- renderUI({
    if (input$variable_group %in% c("Lab Measurements", "Vitals")) {
      checkboxInput("remove_outliers", "Remove extreme outliers using IQR?", 
                    FALSE)
    }
  })
  
  remove_outliers_iqr <- function(data, column) {
    Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
    data %>% filter(data[[column]] >= lower_bound & data[[column]] 
                    <= upper_bound)
  }
  
  output$plot <- renderPlot({
    req(input$variable)
    
    df <- mimic_icu_cohort %>% filter(!is.na(!!sym(input$variable)))  
    
    if (!is.null(input$remove_outliers) && input$remove_outliers && 
        input$variable_group %in% c("Lab Measurements", "Vitals")) {
      df <- remove_outliers_iqr(df, input$variable)
    }
    
    if (input$variable %in% c("race", "insurance", "marital_status", "gender", 
                              "first_careunit", "last_careunit")) {
      ggplot(df, aes_string(x = input$variable, fill = input$variable)) +
        geom_bar() +
        scale_fill_brewer(palette = "Set2") + 
        theme_minimal() +
        labs(title = paste("Distribution of", input$variable), 
             x = input$variable, y = "Count", fill = "Category") +
        theme(
          legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 14, face = "bold")
        )
      
    } else if (input$variable == "age_at_intime") {
      ggplot(df, aes(x = age_at_intime)) +
        geom_histogram(binwidth = 1, fill = "#0072B2", color = "black") +  
        theme_minimal() +
        labs(title = "Age at Intime Distribution", 
             x = "Age at Intime", y = "Count")
      
    } else {
      ggplot(df, aes_string(x = input$variable)) +
        geom_histogram(binwidth = (max(df[[input$variable]], 
                                       na.rm = TRUE) - min(df[[input$variable]], 
                                                           na.rm = TRUE)) / 30, 
                       fill = "gray30", color = "black") +
        theme_minimal() +
        labs(title = paste("Distribution of", input$variable), 
             x = input$variable, y = "Count") 
    }
  })
  
  output$summary_table <- renderTable({
    req(input$variable)
    
    df <- mimic_icu_cohort %>%
      mutate(NA_Count = sum(is.na(!!sym(input$variable))))  
    if (!is.null(input$remove_outliers) && input$remove_outliers && 
        input$variable_group %in% c("Lab Measurements", "Vitals")) {
      df <- remove_outliers_iqr(df, input$variable)
    }
    if (input$variable_group %in% c("Lab Measurements", "Vitals") || 
        input$variable == "age_at_intime") {
      df %>%
        summarise(
          Min = min(!!sym(input$variable), na.rm = TRUE),
          Q1 = quantile(!!sym(input$variable), 0.25, na.rm = TRUE),
          Median = median(!!sym(input$variable), na.rm = TRUE),
          Mean = mean(!!sym(input$variable), na.rm = TRUE),
          Q3 = quantile(!!sym(input$variable), 0.75, na.rm = TRUE),
          Max = max(!!sym(input$variable), na.rm = TRUE),
          SD = sd(!!sym(input$variable), na.rm = TRUE),
        )
    } else {
      df %>% count(!!sym(input$variable)) %>% rename(Variable = 1, Count = 2)
    }
  })
  
  plot_data <- eventReactive(input$submit, {
    list(
      subject_id = input$subject_id,
      plot_type = input$plot_type
    )
  })
  
  output$selected_plot_ui <- renderUI({
    req(plot_data())
    if (plot_data()$plot_type == "adt") {
      plotOutput("patient_timeline")
    } else if (plot_data()$plot_type == "icu") {
      plotOutput("icu_plot")
    }
  })
  
  output$patient_timeline <- renderPlot({
    req(plot_data())
    
    selected_patient <- plot_data()$subject_id
    
    patient_record <- patients_tble |>
      filter(subject_id == !!selected_patient)
    admission_details <- admissions_tble |>
      filter(subject_id == !!selected_patient)
    transfer_details <- patient_transfers_tble |>
      filter(subject_id == !!selected_patient)
    lab_details <- lab_results_tble |>
      filter(subject_id == !!selected_patient)
    procedure_details <- medical_procedures_tble |>
      filter(subject_id == !!selected_patient)
    diagnosis_details <- patient_diagnoses_tble |>
      filter(subject_id == !!selected_patient) |> arrange(hadm_id, seq_num)
    icu_details <- icustays_tble |>
      filter(subject_id == !!selected_patient)
    
    diagnosis_details <- diagnosis_details %>%
      mutate(icd_code = sql("CASE 
                              WHEN LENGTH(icd_code) < 5 
                              THEN LPAD(icd_code, 5, '0') 
                              ELSE icd_code 
                            END")) %>%
      left_join(diagnoses_codes_tble, by = c("icd_code", "icd_version")) %>%
      collect() %>%   
      arrange(hadm_id) 
    
    diagnosis_column <- grep("long_title", colnames(diagnosis_details), 
                             value = TRUE)
    
    if ("long_title" %in% diagnosis_column) {
      diagnosis_details <- rename(diagnosis_details, 
                                  diagnosis_name = long_title)
    } else if ("long_title.x" %in% diagnosis_column) {
      diagnosis_details <- rename(diagnosis_details, 
                                  diagnosis_name = long_title.x)
    } else if ("long_title.y" %in% diagnosis_column) {
      diagnosis_details <- rename(diagnosis_details, 
                                  diagnosis_name = long_title.y)
    } else {
      stop("No appropriate long_title column found in diagnosis_details")
    }
    
    top_conditions <- diagnosis_details %>%
      filter(!is.na(diagnosis_name)) %>%  
      group_by(diagnosis_name) %>%  
      summarize(n = n(), .groups = "drop") %>%  
      arrange(desc(n)) %>%  
      slice_head(n = 3) %>%  
      pull(diagnosis_name)  
    
    top_conditions_text <- if (length(top_conditions) > 0) {
      paste(top_conditions, collapse = "\n")
    } else { "" }
    
    patient_summary_text <- patient_record %>%
      left_join(admission_details, by = "subject_id") %>%
      mutate(summary = trimws(
        paste0(
          "Patient ", subject_id, ", ",
          ifelse(is.na(gender), "", paste0(gender, ", ")),
          ifelse(is.na(anchor_age), "", paste0(anchor_age, " years old, ")),
          ifelse(is.na(race), "", race)
        )
      )) %>%
      pull(summary)
    
    transfer_details <- transfer_details %>%
      mutate(
        intime = sql("CAST(intime AS DATETIME)"),
        outtime = sql("CAST(outtime AS DATETIME)")
      ) %>%
      filter(!is.na(outtime)) %>%
      arrange(hadm_id)  
    
    transfer_details <- transfer_details %>% collect()
    
    lab_details <- lab_details %>%
      mutate(chartdate = sql("CAST(charttime AS DATETIME)"))
    
    procedure_details <- procedure_details %>%
      mutate(chartdate = sql("CAST(chartdate AS DATETIME)")) %>%
      left_join(procedure_codes_tble, by = c("icd_code", "icd_version")) 
    
    procedure_name_column <- grep("long_title", colnames(procedure_details), 
                                  value = TRUE)
    
    if (length(procedure_name_column) > 1) {
      procedure_details <- procedure_details %>%
        select(-all_of(procedure_name_column[-1])) %>%  
        rename(procedure_name = !!procedure_name_column[1])  
    } else if (length(procedure_name_column) == 1) {
      procedure_details <- procedure_details %>%
        rename(procedure_name = !!procedure_name_column)
    }
    
    procedure_details <- procedure_details %>% collect()
    
    procedure_details <- procedure_details %>%
      filter(!is.na(procedure_name))
    
    distinct_care_units <- transfer_details %>% pull(careunit) %>% unique()
    distinct_procedures <- procedure_details %>% 
      pull(procedure_name) %>% unique()
    
    care_unit_palette <- setNames(
      RColorBrewer::brewer.pal(n = min(length(distinct_care_units), 9), 
                               name = "Set1"),
      distinct_care_units
    )
    
    transfer_details <- transfer_details %>%
      mutate(line_width = ifelse(grepl("ICU|CCU|SICU", careunit, 
                                       ignore.case = TRUE), 5.5, 2.0))
    
    custom_shapes <- c(15, 16, 17, 18, 25, 8, 3, 4)
    procedure_shapes <- setNames(custom_shapes[1:length(distinct_procedures)], 
                                 distinct_procedures)
    
    procedure_details <- procedure_details %>%
      mutate(procedure_name = factor(procedure_name, levels = 
                                       distinct_procedures))
    
    patient_timeline <- ggplot() +
      geom_segment(data = transfer_details, 
                   aes(x = intime, xend = outtime, 
                       y = "ADT", yend = "ADT", 
                       color = careunit, 
                       linewidth = line_width), 
                   alpha = 0.8) + 
      geom_point(data = lab_details, 
                 aes(x = chartdate, y = "Lab"), shape = 3, size = 3) +
      geom_point(data = procedure_details, 
                 aes(x = chartdate, y = "Procedure", shape = procedure_name), 
                 size = 5) +
      scale_color_manual(values = care_unit_palette) +  
      scale_shape_manual(values = procedure_shapes, drop = FALSE) + 
      scale_linewidth_continuous(guide = "none") + 
      scale_y_discrete(limits = c("Procedure", "Lab", "ADT")) +  
      theme_minimal() +
      labs(title = patient_summary_text, 
           subtitle = top_conditions_text, 
           x = "Calendar Time", 
           y = NULL, 
           color = "Care Unit",  
           shape = "Procedure") +
      guides(
        color = guide_legend(order = 1, title.position = "top"), 
        shape = guide_legend(order = 2, title.position = "top")   
      ) +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "center",
        legend.spacing.y = unit(0.5, "cm"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)
      )
    
    print(patient_timeline)
  })
  
  output$icu_plot <- renderPlot({
    req(plot_data())
    
    subject_id_of_interest <- plot_data()$subject_id
    
    subject_stays <- icustays_tble %>%
      filter(subject_id == subject_id_of_interest) %>%
      select(stay_id, intime, outtime)
    
    subset_chartevents <- chartevents %>%
      filter(itemid %in% c(220045, 220179, 220180, 223761, 220210)) %>%
      arrange(subject_id, charttime, itemid)  
    
    chartevents_filtered <- subset_chartevents %>%
      semi_join(subject_stays, by = "stay_id") %>%  
      inner_join(subject_stays, by = "stay_id") %>%
      filter(charttime >= intime & charttime <= outtime) %>%
      select(stay_id, itemid, charttime, valuenum)
    
    
    chartevents_with_labels <- chartevents_filtered %>%
      inner_join(d_items %>% select(itemid, abbreviation), by = "itemid") %>%
      collect() %>%  
      mutate(charttime = as_datetime(charttime))
    
    ggplot(chartevents_with_labels, 
           aes(x = charttime, y = valuenum, color = abbreviation)) +
      geom_point(linewidth = 1.2) +  
      geom_line(linewidth = 0.8) +  
      facet_grid(abbreviation ~ stay_id, scales = "free") +  
      labs(
        title = paste("Patient", subject_id_of_interest, "ICU stays - Vitals"),
        x = "Time",
        y = "Vital Value"
      ) +
      scale_x_datetime(
        breaks = seq(
          floor_date(min(chartevents_with_labels$charttime, na.rm = TRUE), 
                     unit = "6 hours"),
          ceiling_date(max(chartevents_with_labels$charttime, na.rm = TRUE), 
                       unit = "6 hours"),
          by = "6 hours"
        ),
        date_labels = "%b %d %H:%M"  
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",  
        strip.text = element_text(size = 12, face = "bold", color = "white"), 
        strip.background = element_rect(fill = "darkgrey", color = "darkgrey"), 
        axis.text.x = element_text(angle = 0, hjust = 0.5),  
        panel.grid.major = element_line(size = 0.5, linetype = "dotted", 
                                        color = "gray"),  
        panel.grid.minor = element_blank()  
      )
  }) }

shinyApp(ui = ui, server = server)