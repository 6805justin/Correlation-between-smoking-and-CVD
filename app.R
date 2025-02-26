library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(here)



data_path <- here("JustinData5002", "CVD.csv")

CVDdata <- read.csv(here("JustinData5002", "CVD.csv"), header = FALSE, stringsAsFactors = FALSE)
smoking <- read.csv(here("JustinData5002", "daily smoking.csv"), header = FALSE, stringsAsFactors = FALSE)


head(CVDdata)
head(smoking)

# Manually assigning column names for smoking dataset

colnames(smoking) <- c("Sex", "Year", "Smoking_Rate", "LL_95_CI", "UL_95_CI")
# converting the column names to numeric
Clean_Smoking <- smoking %>%
  mutate(Year = as.numeric(Year),
         Smoking_Rate = as.numeric(Smoking_Rate))

Clean_Smoking <- smoking[-1, ]

colnames(Clean_Smoking) <- c("Sex", "Year", "Smoking_Rate", "LL_95_CI", "UL_95_CI")


Clean_Smoking <- Clean_Smoking[!is.na(as.numeric(Clean_Smoking$Year)), ]

Clean_Smoking$Year <- as.numeric(Clean_Smoking$Year)
Clean_Smoking$Smoking_Rate <- as.numeric(Clean_Smoking$Smoking_Rate)
Clean_Smoking$LL_95_CI <- as.numeric(Clean_Smoking$LL_95_CI)
Clean_Smoking$UL_95_CI <- as.numeric(Clean_Smoking$UL_95_CI)


head(Clean_Smoking)
str(Clean_Smoking)

# The 1st row is just the description of the columns and hence deleting it
CVDClean <- CVDdata[-1, ]

# setting the column names manually
colnames(CVDClean) <- c("Disease_Type", "Sex", "SES", "Period", "Mortality_Rate", "LL_95_CI", "UL_95_CI")


CVDClean <- CVDClean %>%
  # The column "period" in cvd dataset is the year range and hence renaming it as year
  mutate(
    Year = as.numeric(str_extract(Period, "^\\d{4}")),
    Disease_Type = str_replace(Disease_Type, "Cardiovascular disease deaths by disease type by Disease type and Sex for NSW", "Coronary Heart Disease")  
  ) %>%
  # only selecting the required columns to merge and converting them to numeric datatype
  select(Year, Sex, SES, Disease_Type, Mortality_Rate, LL_95_CI, UL_95_CI)
CVDClean$Mortality_Rate <- as.numeric(CVDClean$Mortality_Rate)
CVDClean$LL_95_CI <- as.numeric(CVDClean$LL_95_CI)
CVDClean$UL_95_CI <- as.numeric(CVDClean$UL_95_CI)

# joining the 2 datasets
merged_data <- inner_join(CVDClean, Clean_Smoking, by = c("Year", "Sex"))

# Selecting necessary columns
cleaned_merged_data <- merged_data %>%
select(Year, Sex, SES, Disease_Type, Mortality_Rate,Smoking_Rate)

# Define the UI
ui <- fluidPage(
  includeCSS("DataStyle.css"),
  titlePanel("Cardiovascular Disease and Smoking Dashboard"),

  
  sidebarLayout(
    sidebarPanel(
      selectInput("sex_filter", "Select Sex:", choices = c("All", "Males", "Females")),
      selectInput("disease_type", "Select Disease Type:", choices = c("All", unique(cleaned_merged_data$Disease_Type))),
      sliderInput("year_filter", "Select Year Range:", 
                  min = min(cleaned_merged_data$Year), max = max(cleaned_merged_data$Year), 
                  value = c(min(cleaned_merged_data$Year), max(cleaned_merged_data$Year)))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("CVD Mortality Trends", plotOutput("cvd_trend_plot")),
        tabPanel("Smoking Trends", plotOutput("smoking_trend_plot")),
        tabPanel("Correlation", plotOutput("correlation_plot")),
        tabPanel("Correlation Heatmap", plotOutput("heatmap_plot")),
        tabPanel("Data Summary", tableOutput("data_summary"))
      )
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Filter data based on user input
  filtered_data <- reactive({
    df <- cleaned_merged_data
    if (input$sex_filter != "All") {
      df <- df %>% filter(Sex == input$sex_filter)
    }
    if (input$disease_type != "All") {
      df <- df %>% filter(Disease_Type == input$disease_type)
    }
    df <- df %>% filter(Year >= input$year_filter[1], Year <= input$year_filter[2])
    return(df)
  })
  
  # CVD Mortality Trend Plot
  output$cvd_trend_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = as.integer(Year), y = Mortality_Rate, color = Sex)) +
      geom_point(size = 2) +
      geom_line(size = 1) +
      labs(title = "CVD Mortality Trends Over Time by Sex", x = "Year", y = "CVD Mortality Rate") + theme_minimal()
  })
  
  # Smoking Rate Trend Plot
  output$smoking_trend_plot <- renderPlot({ggplot(filtered_data(), aes(x = as.factor(Year), y = Smoking_Rate, color = Sex, group = Sex)) + geom_point(size = 2) +geom_line(size = 1) + labs(title = "Smoking Rate Trends Over Time by Sex",x = "Year",y = "Smoking Rate (%)") +theme_minimal()
  })
  
  #  Correlation between Smoking Rate and CVD Mortality Rate
  output$correlation_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Smoking_Rate, y = Mortality_Rate, color = Sex)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(title = "Relationship Between Smoking Rate and CVD Mortality Rate", x = "Smoking Rate (%)",y = "CVD Mortality Rate") + theme_minimal()
  })
  
  # Correlation Heatmap
  output$heatmap_plot <- renderPlot({correlation_data <- filtered_data() %>% group_by(Sex) %>% summarise(correlation = cor(Smoking_Rate, Mortality_Rate, use = "complete.obs"))
  ggplot(correlation_data, aes(x = "Smoking Rate vs CVD Mortality", y = Sex, fill = correlation)) + geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +labs(title = "Correlation Between Smoking Rates and CVD Mortality Rates",x = "", y = "Sex") + theme_minimal()
  })
  
  # Display Data Summary
  output$data_summary <- renderTable({
    summary(filtered_data())
  })
}


shinyApp(ui = ui, server = server)