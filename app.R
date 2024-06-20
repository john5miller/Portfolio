library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(treemapify)

# UI
ui <- fluidPage(
    titlePanel('Acquisitions Practice Area: Workforce Stats Enhanced'),
    
    sidebarLayout(
        sidebarPanel(
            h3("Work Force Stats"),
            p("Average Total Experience: 29.28 Years"),
            p("Average LMI Experience: 3.67 Years"),
            p("Percent of Workforce with Masters Degrees: 43.8%"),
            p("Percent of Workforce with a DAU or DAWIA Certification: 55.6%"),
            p("Percent of Workforce with an Agile Certification: 34.8%"),
            p("Percent of Certified Project Management Professionals in the Workforce: 28.2%")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Masters Degrees", plotOutput("educationPlot")),
                tabPanel("DAU or DAWIA Certifications", plotOutput("dauPlot")),
                tabPanel("Types and Levels of DAU/DAWIA Certifications", plotOutput("dauTypesPlot")),
                tabPanel("Agile Certifications", plotOutput("agilePlot")),
                tabPanel("Specific Agile Certifications", plotOutput("agileSpecificPlot")),
                tabPanel("PMP Certifications", plotOutput("pmpPlot"))
            )
        )
    )
)
# Load data
data <- read.csv("/Users/johnmiller/WorkForceStatsDataForR.csv")
df <- data.frame(data)
colnames(df) <- c("Name", "PMP", "DAU_DAWIA", "Agile", "Education")
df <- df[-90, ]

# Server
server <- function(input, output) {
    
    output$educationPlot <- renderPlot({
        df_sorted <- df %>% arrange(Education)
        df_sorted <- df_sorted %>%
            mutate(EducationLevel = ifelse(grepl("Master", Education, ignore.case = TRUE), "Masters", "Lower Degrees"))
        
        total_count <- nrow(df_sorted)
        masters_count <- sum(df_sorted$EducationLevel == "Masters")
        percent_masters <- (masters_count / total_count) * 100
        
        df_pie <- df_sorted %>%
            group_by(EducationLevel) %>%
            summarize(count = n()) %>%
            mutate(percentage = (count / sum(count)) * 100)
        
        ggplot(df_pie, aes(x = "", y = percentage, fill = EducationLevel)) +
            geom_bar(width = 1, stat = "identity") +
            coord_polar("y") +
            geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5)) +
            labs(title = "Masters Degrees vs Other Education Levels",
                 x = NULL,
                 y = NULL) +
            theme_minimal() +
            scale_fill_brewer(palette = "Set3") +
            theme(axis.text.x = element_blank(),
                  axis.ticks = element_blank())
    })
    
    output$dauPlot <- renderPlot({
        df_sorted2 <- df %>% arrange(DAU_DAWIA)
        df_sorted2$DAU_ <- factor(df_sorted2$DAU_DAWIA)
        
        df_sorted2 <- df_sorted2 %>%
            mutate(Certification_Status = ifelse(is.na(DAU_DAWIA), "No Certification", "Certified"))
        
        total_count <- nrow(df_sorted2)
        certified_count <- sum(df_sorted2$Certification_Status == "Certified")
        percent_certified <- (certified_count / total_count) * 100
        
        df_donut <- df_sorted2 %>%
            group_by(Certification_Status) %>%
            summarize(count = n()) %>%
            mutate(percentage = (count / sum(count)) * 100)
        
        ggplot(df_donut, aes(x = 2, y = percentage, fill = Certification_Status)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar(theta = "y") +
            xlim(0.5, 2.5) +
            geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5)) +
            labs(title = "DAU and DAWIA Certifications",
                 x = NULL,
                 y = NULL) +
            theme_void() +
            theme(legend.title = element_blank()) +
            scale_fill_brewer(palette = "Set2")
    })
    
    output$dauTypesPlot <- renderPlot({
        df_sorted3 <- df %>% arrange(DAU_DAWIA)
        df_sorted3$DAU_DAWIA <- factor(df_sorted3$DAU_DAWIA)
        
        certification_counts <- df_sorted3 %>%
            group_by(DAU_DAWIA) %>%
            summarize(count = n()) %>%
            mutate(percentage = (count / sum(count)) * 100)
        
        df_sorted3 <- df_sorted3 %>%
            left_join(certification_counts, by = "DAU_DAWIA")
        
        ggplot(df_sorted3, aes(x = DAU_DAWIA, y = count)) +
            geom_segment(aes(x = DAU_DAWIA, xend = DAU_DAWIA, y = 0, yend = count), color = "skyblue") +
            geom_point(size = 5, color = "orange") +
            geom_text(aes(label = sprintf("%.1f%%", percentage), y = count), vjust = -0.5, size = 4) +
            labs(title = "Count of Different Types and Levels of DAU/DAWIA Certifications",
                 x = "DAU/DAWIA Certification Type and Level",
                 y = "Count") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$agilePlot <- renderPlot({
        total_count <- nrow(df)
        certified_count <- sum(!is.na(df$Agile))
        percent_certified <- (certified_count / total_count) * 100
        
        plot_data_agile <- df %>%
            mutate(CertificationStatus = ifelse(!is.na(Agile), "Certified", "No Certification")) %>%
            group_by(CertificationStatus) %>%
            summarize(count = n()) %>%
            mutate(percentage = (count / sum(count)) * 100)
        
        ggplot(plot_data_agile, aes(x = "", y = percentage, fill = CertificationStatus)) +
            geom_bar(width = 1, stat = "identity") +
            coord_polar("y") +
            geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5)) +
            labs(title = "Agile Certifications",
                 x = NULL,
                 y = NULL) +
            theme_minimal() +
            scale_fill_brewer(palette = "Pastel1", labels = c("Certified", "No Certification")) +
            theme(axis.text.x = element_blank(),
                  axis.ticks = element_blank())
    })
    
    output$agileSpecificPlot <- renderPlot({
        df_sorted4 <- df %>%
            arrange(Agile) %>%
            mutate(Agile = factor(Agile))
        
        total_count <- nrow(df_sorted4)
        certified_count <- sum(!is.na(df_sorted4$Agile))
        percent_certified <- (certified_count / total_count) * 100
        
        plot_data_agile <- df_sorted4 %>%
            mutate(Certification_Status = ifelse(is.na(Agile), "No Certification", as.character(Agile))) %>%
            group_by(Certification_Status) %>%
            summarize(count = n()) %>%
            mutate(percentage = (count / sum(count)) * 100)
        plot_data_agile <- plot_data_agile %>%
            mutate(adjusted_percentage = ifelse(Certification_Status == "No Certification", percentage * 0.5, percentage))
        
        ggplot(plot_data_agile, aes(area = adjusted_percentage, fill = Certification_Status, label = sprintf("%s\n%.1f%%", Certification_Status, percentage))) +
            geom_treemap() +
            geom_treemap_text(fontface = "italic", colour = "white", place = "centre", grow = TRUE) +
            labs(title = "Specific Agile Certifications") +
            theme(legend.position = "none") +
            scale_fill_brewer(palette = "Dark2")
    })
    
    output$pmpPlot <- renderPlot({
        df_filtered <- df %>%
            filter(PMP != "?")
        
        total_count <- nrow(df_filtered)
        pmp_count <- sum(df_filtered$PMP == "Yes")
        percent_pmp <- (pmp_count / total_count) * 100
        
        plot_data_pmp <- df_filtered %>%
            mutate(PMP_Status = ifelse(PMP == "Yes", "Certified", "No Certification")) %>%
            group_by(PMP_Status) %>%
            summarize(count = n()) %>%
            mutate(percentage = (count / sum(count)) * 100)
        
        ggplot(plot_data_pmp, aes(x = 2, y = percentage, fill = PMP_Status)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar(theta = "y") +
            xlim(0.5, 2.5) +
            geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5)) +
            labs(title = "PMP Certifications",
                 x = NULL,
                 y = NULL) +
            theme_void() +
            theme(legend.title = element_blank()) +
            scale_fill_brewer(palette = "Set1")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)