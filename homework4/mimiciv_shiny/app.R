## Q2. Build a Shiny app to explore the ICU cohort

## The Necessary plugins to run this app. I found a package called scales that helps with coloring data so let us try it out
## The first six are what is required for the library. The last four are the ones that I thought was necessary or was recommended, like forcats

library(bigrquery)
library(dbplyr)
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)
library(forcats)
library(scales)
library(shiny)
library(stringr)

##Connecting it to the necessary document

satoken <- paste0(
  "~/Desktop/203b-HW/homework4/",
  "biostat-203b-2025-winter-4e58ec6e5579.json")

if (file.exists(satoken)) {
  bq_auth(path = satoken)
  message("✅ BigQuery authentication successful!")
} else {
  stop("🚨 Service account token not found! Check the file path.")
}

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2025-winter",
  dataset = "mimiciv_3_1",
  billing = "biostat-203b-2025-winter")

dbListTables(con_bq)

mimic_icu_cohort <- readRDS(file.path(
  "~/Desktop/203b-hw/homework4/mimiciv_shiny",
  "mimic_icu_cohort.rds"))

##User Interface Tabs

##Tab 1. This needs to include an option for demographics, labs, and vitals. 

##Within the demographics, we need the gender, age_intime, race, insurance, and marital status
##The labs and vitals are going to be the same as what we are looking for from the last few homeworks
##There should be a drop down selection where they can select from demographics, labs and vitals

ui <- fluidPage(
  titlePanel("ICU Cohort Analysis"),
  tabsetPanel(
    tabPanel("ICU Cohort Summary",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_group", "Variable Group", 
                             choices = c("demographics", "labs", "vitals")),
                 selectInput("variable", "Variable", choices = NULL)),
               mainPanel(
                 tableOutput("summary_table"),
                 plotOutput("summary_plot")))),
    tabPanel("Patient Lookup",
             sidebarLayout(
               sidebarPanel(
                 textInput("patient_id", "Enter Subject ID:", value = ""),
                 actionButton("fetch_patient", "Submit")),
               mainPanel(
                 verbatimTextOutput("patient_info"),
                 plotOutput("patient_adt_plot"))))))

##Inputting the choices for the drop down
server <- function(input, output, session) {
  observeEvent(input$variable_group, {
    variable_choices <- switch(input$variable_group,
                               "demographics" = c("gender", "age_intime", 
                                                  "race", "insurance",
                                                  "marital_status"),
                               "labs" = c("Bicarbonate", "Chloride", 
                                          "Creatinine", "Glucose", "Potassium", 
                                          "Sodium", "Hematocrit", "WBC"),
                               "vitals" = c("Heart_Rate", "SysBP", "DiaBP", 
                                            "Respiratory_Rate", "Temp"))
    updateSelectInput(session, "variable", choices = variable_choices)})
  
  output$summary_plot <- renderPlot({
    req(input$variable)
    
    if (input$variable == "race") {
      race_data <- mimic_icu_cohort %>%
        mutate(
          race = as.character(race),
          race = case_when(
            race %in% c("ASIAN", "ASIAN - CHINESE", 
                        "ASIAN - KOREAN", 
                        "ASIAN - ASIAN INDIAN", 
                        "ASIAN - SOUTH EAST ASIAN") ~ "ASIAN",
            race %in% c("BLACK", "BLACK/AFRICAN", "BLACK/AFRICAN AMERICAN", 
                        "BLACK/CAPE VERDEAN", 
                        "BLACK/CARIBBEAN ISLAND") ~ "BLACK",
            race %in% c("HISPANIC", "HISPANIC OR LATINO", 
                        "HISPANIC/LATINO - SALVADORAN", 
                        "HISPANIC/LATINO - PUERTO RICAN",
                        "HISPANIC/LATINO - GUATEMALAN", 
                        "HISPANIC/LATINO - CUBAN", 
                        "HISPANIC/LATINO - DOMINICAN", 
                        "HISPANIC/LATINO - CENTRAL AMERICAN",
                        "HISPANIC/LATINO - HONDURAN", 
                        "HISPANIC/LATINO - COLUMBIAN", 
                        "HISPANIC/LATINO - MEXICAN") ~ "HISPANIC",
            race %in% c("WHITE", "WHITE - RUSSIAN", "WHITE - OTHER EUROPEAN", 
                        "WHITE - EASTERN EUROPEAN", 
                        "WHITE - BRAZILIAN") ~ "WHITE",
            TRUE ~ "Other")) %>%
        
        group_by(race) %>%
        summarise(count = n()) %>%
        ungroup()
      
      ggplot(race_data, aes(x = race, y = count, fill = race)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = "Race Distribution in ICU Cohort",
             x = "Race Category", y = "Number of Patients") +
        theme(legend.position = "none")} 
    
    ##We need to make sure we set a range for the values since there are outliers. 
    else {
      data <- mimic_icu_cohort[[input$variable]]
      if (is.numeric(data)) {
        filtered_data <- mimic_icu_cohort
        if (input$variable == "SysBP") {
          filtered_data <- filtered_data %>% filter(SysBP < 300)} 
        else if (input$variable == "DiaBP") {
          filtered_data <- filtered_data %>% filter(DiaBP < 200)} 
        else if (input$variable == "Heart_Rate") {
          filtered_data <- filtered_data %>% filter(Heart_Rate < 250)} 
        else if (input$variable == "Respiratory_Rate") {
          filtered_data <- filtered_data %>% filter(Respiratory_Rate < 80)} 
        else if (input$variable == "Temp") {
          filtered_data <- filtered_data %>% filter(Temp < 110)}
        
        ggplot(filtered_data, aes(x = .data[[input$variable]])) +
          geom_histogram(fill = "pink", color = "black", bins = 30) +
          theme_minimal() +
          labs(title = paste("Distribution of", input$variable),
               x = input$variable, y = "Count")} 
      
      else {
        ggplot(mimic_icu_cohort, aes(x = as.factor(data), 
                                     fill = as.factor(data))) +
          geom_bar() +
          scale_fill_manual(values = scales::hue_pal()(length(unique(data)))) +
          theme_minimal() +
          labs(title = paste("Distribution of", input$variable),
               x = input$variable, y = "Count", fill = "Category")}}})
  
  #Rendering the summary table using what I learned from other biostat classes for what is expected in a summary table
  #Received a prior error about NA data so make sure we set that to TRUE
  
  output$summary_table <- renderTable({
    req(input$variable)
    data <- mimic_icu_cohort[[input$variable]]
    
    if (is.numeric(data)) 
      {summary_df <- data.frame(Statistic = c("Mean", "Median", "Min", "Max", 
                                              "Standard Deviation"),
        Value = c(mean(data, na.rm = TRUE), median(data, na.rm = TRUE),
                  min(data, na.rm = TRUE), max(data, na.rm = TRUE),
                  sd(data, na.rm = TRUE)))} 
    
    else {summary_df <- as.data.frame(table(data))
    colnames(summary_df) <- c("Category", "Count")}
    return(summary_df)})
  
  #Tab 2 Code is the same with what we did in Q1.1
  observeEvent(input$fetch_patient, {
    req(input$patient_id)
    subject_id <- as.numeric(input$patient_id)
    
    patients.filter <- tbl(con_bq, "patients") %>%
      filter(subject_id == !!subject_id) %>%
      collect()
    
    admissions.filter <- tbl(con_bq, "admissions") %>%
      filter(subject_id == !!subject_id) %>%
      collect()
    
    transfers.filter <- tbl(con_bq, "transfers") %>%
      filter(subject_id == !!subject_id) %>%
      collect()
    
    procedures_icd.filter <- tbl(con_bq, "procedures_icd") %>%
      filter(subject_id == !!subject_id) %>%
      collect()
    
    diagnoses_icd.filter <- tbl(con_bq, "diagnoses_icd") %>%
      filter(subject_id == !!subject_id) %>%
      collect()
    
    d_icd_procedures.filter <- tbl(con_bq, "d_icd_procedures") %>%
      collect()
    
    d_icd_diagnoses.filter <- tbl(con_bq, "d_icd_diagnoses") %>%
      collect()
    
    labevents.filter2 <- tbl(con_bq, "labevents") %>%
      filter(subject_id == !!subject_id) %>%
      collect() %>%
      mutate(charttime = as.POSIXct(charttime, format = "%Y-%m-%d %H:%M:%S"))
    
    LJDiagnoses <- diagnoses_icd.filter %>%
      left_join(d_icd_diagnoses.filter, by = c("icd_code", "icd_version"))
    
    LJProcedures <- procedures_icd.filter %>%
      left_join(d_icd_procedures.filter, by = c("icd_code", "icd_version"))
    
    LJDiagnosesT3 <- LJDiagnoses %>%
      group_by(long_title) %>%
      summarise(n = n()) %>%
      arrange(desc(n), long_title) %>%
      slice_head(n = 3) %>%
      collect() %>% 
      pull(long_title)
    
    transfer.filter2 <- as.data.frame(transfers.filter) %>%
      mutate(intime = as.POSIXct(intime, format = "%Y-%m-%d %H:%M:%S"),
             outtime = as.POSIXct(outtime, format = "%Y-%m-%d %H:%M:%S"))
    
    LJProcedures <- as.data.frame(LJProcedures) %>%
      mutate(chartdate = as.POSIXct(chartdate, format = "%Y-%m-%d"))
    
    ##Let us set it to unknown as well in case the value is not recorded
    
    patient_race <- ifelse(nrow(admissions.filter) > 0, 
                           admissions.filter$race[1], "Unknown")
    patient_gender <- ifelse(nrow(patients.filter) > 0, 
                             patients.filter$gender[1], "Unknown")
    patient_age <- ifelse(nrow(patients.filter) > 0, 
                          patients.filter$anchor_age[1], "Unknown")
    
    ##Make sure to add in the patients race as well since last time we did not
    patient_info <- paste0(
      "Patient ", subject_id, ", ", 
      patients.filter$gender, ", ", 
      patients.filter$anchor_age, " years old, ", 
      "", patient_race)
    
    LJProcedures$long_title_wrapped <- str_wrap(LJProcedures$long_title, 
                                                width = 17)
    transfer.filter2$size <- ifelse(
      transfer.filter2$careunit == "Surgical Intensive Care Unit (SICU)", 6, 3)
    
    RainbowColors <- scales::hue_pal()(length(
      unique(transfer.filter2$careunit)))
    
    output$patient_adt_plot <- renderPlot({
      ggplot() +
        
        geom_point(data = LJProcedures,  
                   aes(x = chartdate, y = "Procedure", 
                       shape = long_title_wrapped),  
                   color = "black",   
                   size = 4,  
                   alpha = 0.7, 
                   position = position_jitter(width = 0, height = -0.5)) + 
        
        geom_segment(data = transfer.filter2, 
                     aes(x = intime, xend = outtime, y = "ADT",  
                         color = careunit, size = size), 
                     alpha = 0.8)  +
        
        geom_point(data = labevents.filter2, 
                   aes(x = charttime, y = "Lab"),
                   shape = 3, size = 3, color = "black") +
        labs(
          y = "", 
          x = "Calendar Time", 
          title = patient_info,  
          subtitle = paste0("Top 3 Diagnoses:\n", 
                            paste(LJDiagnosesT3, collapse = "\n")),
          color = "Care Unit",
          shape = "Procedure",
          size = "ICU Stay Type") +  
        
        guides(
          color = guide_legend(title = "Care Unit", 
                               nrow = 6, title.position = "top"),  
          shape = guide_legend(title = "Procedure", 
                               nrow = 2, title.position = "top"),
          size = guide_legend(title = "ICU Stay Type", 
                              override.aes = list(linetype = 1))) +
        
        scale_color_manual(values = RainbowColors) +
        scale_size_identity() +
        scale_y_discrete(limits = rev) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 8),
          axis.text.x = element_text(angle = 0, hjust = 1),
          legend.position = "bottom",  
          legend.box = "horizontal", 
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 7.5),
          plot.margin = margin(10, 20, 10, 20),
          legend.spacing.y = unit(0.3, "cm"),
          legend.box.spacing = unit(0.5, "cm"))})})}

shinyApp(ui, server)
