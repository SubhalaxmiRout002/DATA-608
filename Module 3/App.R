library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(tidyr)

df <- read.csv('https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv')

ui <- fluidPage(
  headerPanel(h2('Mortality Rates and Causes Across the United States',
              style='background-color:steelblue;
                     padding-left: 15px; color:white')),
  hr(),
  h5(strong('Question #1:'), 'As a researcher, you frequently compare mortality rates from particular
                   causes across different States. You need a visualization that will let you see (for
                   2010 only) the crude mortality rate, across all States, from one cause (for example,
                   Neoplasms, which are effectively cancers). Create a visualization that allows you
                   to rank States by crude mortality for each cause of death.',
                   style='padding-left: 15px; color:#8B008B'),
                  
  hr(),
  sidebarPanel(
    selectInput('icd', 'Cause of Death ', unique(df$ICD.Chapter), selected='Neoplasms')
  ),
  
  
  mainPanel(
    plotOutput('plot1', width="80%", height="550px")
  ),
  
  hr(),
  h5(strong('Question #2:'), 'Often you are asked whether particular States are improving their mortality rates (per cause)
     faster than, or slower than, the national average. Create a visualization that lets your clients see this for 
     themselves for one cause of death at the time. Keep in mind that the national average should be weighted 
     by the national population.',
     style='padding-left: 15px; color:#8B008B'),
  
  hr(),
  sidebarPanel(
    selectInput('state', 'State', sort(unique(df$State)), selected = 'AK'),
    selectInput('icd2', 'Cause of Death ', unique(df$ICD.Chapter), selected='Certain infectious and parasitic diseases')
  ),
  
  hr(),
  mainPanel(
    plotOutput('plot2', width="80%", height="650px")
  )
  
)

server <- function(input, output) {
  
  # plot 1
  output$plot1 <- renderPlot({
    
    dfSlice1 <- df %>% group_by(State) %>% 
      filter(ICD.Chapter==input$icd & Year == '2010')
    
    
    ggplot(dfSlice1, aes(x = reorder(State, Crude.Rate), y = Crude.Rate)) +
      geom_bar(fill="#276678", stat = "identity") + 
      ggtitle('States wise Mortality Rate in 2010') +
      coord_flip() +
      xlab("Mortality Rate") +
      geom_text(aes(label = Crude.Rate), size = 2,  hjust=-0.20) +
      theme(panel.background = element_rect(fill = "white", color = NA),
            plot.title = element_text(hjust = 0.5, size = 20, colour = "#03506f"),
            axis.title.y = element_text(size = 20, colour = "#03506f"),
            axis.title.x=element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x=element_blank()
      )
  }
  
  )
  
  
  # plot 2
  
  output$plot2 <- renderPlot({
    
    dfslice2 <- df  %>%  filter(ICD.Chapter == input$icd2) %>% group_by(Year) %>%
      mutate(National.Crude.Rate = sum(Deaths) / sum(Population) * 100000) 
    
    dfslice3 <- df %>% filter(ICD.Chapter == input$icd2) %>% group_by(Year)
    
    dfslice4 <- cbind(dfslice3, dfslice2$National.Crude.Rate)
    
    dfslice5 <- dfslice4 %>% filter(State == input$state)
    
    colnames(dfslice5)[7] <- "National.Crude.Rate" 
    
    dfslice5$National.Crude.Rate = round(dfslice5$National.Crude.Rate,1)
    
    dfslice5$National.Crude.Rate = round(dfslice5$National.Crude.Rate,1)
    
    dfslice5 <- dfslice5 %>% select(Year, Crude.Rate, National.Crude.Rate) %>% tidyr::gather("Crude", "Value", -Year)
    
    dfslice5 %>% 
      ggplot() + aes(x = Year, y = Value, fill = Crude) +
      geom_col(position = "dodge") +
      ggtitle('Crude Rate vs National Crude Rate') +
      xlab('Year') +
      ylab('Mortality Rate') +
      scale_fill_manual(values = c("#276678", "#999999")) +
      geom_text(aes(label = Value), size = 4, position = position_dodge(width = 1), vjust = -.50) +
      theme(panel.background = element_rect(fill = "white", color = NA),
            plot.title = element_text(hjust = 0.5, size = 15, colour = "#03506f"),
            axis.title.y = element_text(size = 10, colour = "#03506f")
      )
  }
  
  )
}

shinyApp(ui = ui, server = server)