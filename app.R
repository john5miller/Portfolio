# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)

# Define the UI
ui <- fluidPage(
    titlePanel("Acquisitions Practice Area: Workforce Stats"),
    sidebarLayout(
        sidebarPanel(
            p("Average Total Experience: 29.28 Years"),
            p("Average LMI Experience: 3.67 years"),
            p("Percent of Workforce with Masters Degrees: 43.8%"),
            p("Percent of Workforce with a DAU or DAWIA Certification: 55.6%"),
            p("Percent of Workforce with an Agile Certification: 34.8%"),
            p("Percent of Certified Project Management Professionals in the Workforce: 28.2%")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Masters Degrees vs Other Education Levels",
                         plotOutput("educationPlot")),
                tabPanel("DAU and DAWIA Certifications",
                         plotOutput("dauPlot")),
                tabPanel("Types and Levels of DAU/DAWIA Certifications",
                         plotOutput("dauTypesPlot")),
                tabPanel("Agile Certifications",
                         plotOutput("agilePlot")),
                tabPanel("Specific Agile Certifications",
                         plotOutput("agileTypesPlot")),
                tabPanel("PMP Certifications",
                         plotOutput("pmpPlot"))
            )
        )
    )
)

# Define the server logic
server <- function(input, output) {
    
    # Read data
    data <- read.csv("/Users/johnmiller/WorkForceStatsDataForR.csv")
    df <- data.frame(data)
    colnames(df) <- c("Name","PMP","DAU_DAWIA","Agile","Education")
    df <- df[-90, ]
    
    # Masters Degrees vs Other Education Levels
    output$educationPlot <- renderPlot({
        df_sorted <- df %>% arrange(Education)
        df_sorted <- df_sorted %>%
            mutate(Education_Level = ifelse(grepl("Master", Education, ignore.case = TRUE), "Masters", "Lower Degrees"))
        
        ggplot(df_sorted, aes(x = Education_Level)) +
            geom_bar() +
            labs(title = "Masters Degrees vs Other Education Levels",
                 x = "Education Level",
                 y = "Count")
    })
    
    # DAU and DAWIA Certifications
    output$dauPlot <- renderPlot({
        df_sorted2 <- df %>% arrange(DAU_DAWIA)
        df_sorted2$DAU_ <- factor(df_sorted2$DAU_DAWIA)
        
        df_sorted2 <- df_sorted2 %>%
            mutate(Certification_Status = ifelse(is.na(DAU_DAWIA), "No Certification", "Certified"))
        
        ggplot(df_sorted2, aes(x = Certification_Status)) +
            geom_bar() +
            labs(title = "DAU and DAWIA Certifications Bar Chart",
                 x = "Certification Status",
                 y = "Count")
    })
    
    # Types and Levels of DAU/DAWIA Certifications
    output$dauTypesPlot <- renderPlot({
        df_sorted3 <- df %>% arrange(DAU_DAWIA)
        df_sorted3$DAU_DAWIA <- factor(df_sorted3$DAU_DAWIA)
        
        certification_counts <- df_sorted3 %>%
            group_by(DAU_DAWIA) %>%
            summarize(count = n()) %>%
            mutate(percentage = (count / sum(count)) * 100)
        
        df_sorted3 <- df_sorted3 %>%
            left_join(certification_counts, by = "DAU_DAWIA")
        
        ggplot(df_sorted3, aes(x = DAU_DAWIA)) +
            geom_bar() +
            geom_text(aes(label = sprintf("%.1f%%", percentage), y = ..count..), stat = "count", vjust = -0.5) +
            labs(title = "Count of Different Types and Levels of DAU/DAWIA Certifications",
                 x = "DAU/DAWIA Certification Type and Level",
                 y = "Count") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))+
            ylim(0, max(certification_counts$count) * 1.2)
    })
    
    # Agile Certifications
    output$agilePlot <- renderPlot({
        plot_data_agile <- df %>%
            mutate(Certification_Status = ifelse(!is.na(Agile), "Certified", "No Certification")) %>%
            group_by(Certification_Status) %>%
            summarize(count = n()) %>%
            mutate(percent = round((count / sum(count)) * 100, digits = 0))
        
        ggplot(plot_data_agile, aes(x = Certification_Status, y = count)) +
            geom_bar(stat = "identity") +
            labs(title = "Agile Certifications Bar Chart",
                 x = "Certification Status",
                 y = "Count")
    })
    
    # Specific Agile Certifications
    output$agileTypesPlot <- renderPlot({
        df_sorted4 <- df %>%
            arrange(Agile) %>%
            mutate(Agile = factor(Agile))
        
        plot_data_agile <- df_sorted4 %>%
            mutate(Certification_Status = ifelse(is.na(Agile), "No Certification", as.character(Agile))) %>%
            group_by(Certification_Status) %>%
            summarize(count = n()) %>%
            mutate(percentage = (count / sum(count)) * 100)
        
        ggplot(plot_data_agile, aes(x = Certification_Status, y = count)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = sprintf("%.1f%%", percentage), y = count), vjust = -0.5, size = 4, position = position_stack(0.5)) +
            labs(title = "Specific Agile Certifications Bar Chart",
                 x = "Certification Status",
                 y = "Count") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            ylim(0, max(plot_data_agile$count) * 1.2)
    })
    
    # PMP Certifications
    output$pmpPlot <- renderPlot({
        df_filtered <- df %>%
            filter(PMP != "?")
        
        plot_data_pmp <- df_filtered %>%
            mutate(PMP_Status = ifelse(PMP == "Yes", "Certified", "No Certification")) %>%
            group_by(PMP_Status) %>%
            summarize(count = n()) %>%
            mutate(percentage = (count / sum(count)) * 100)
        
        ggplot(plot_data_pmp, aes(x = PMP_Status, y = count)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = sprintf("%.1f%%", percentage), y = count), vjust = -0.5, size = 4) +
            labs(title = "PMP Certifications Bar Chart",
                 x = "PMP Certification Status",
                 y = "Count") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            ylim(0, max(plot_data_pmp$count) * 1.2)
    })
}

# Run the application
shinyApp(ui = ui, server = server)