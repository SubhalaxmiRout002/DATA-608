library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(tidyr)
library(scales)
library(plotly)
library(lazyeval)

df <- read.csv('https://raw.githubusercontent.com/SubhalaxmiRout002/DATA-608/main/Final%20Project/Supermarket%20Sales%20-%20Stock.csv')

df <- data.frame(df)
df$Date <- as.Date(df$Date, format =  "%m/%d/%Y")
df$Month <- months(as.Date(df$Date))
df$Time <- as.factor(df$Time)


ui <- shinyUI(navbarPage(title = "Sales Analysis",
                         # first tab to display project info of the project markdown file
                         tabPanel("Project Information",
                                  fluidPage(
                                    includeMarkdown("project_information.Rmd"))),
                         # next tab to the right contains sidebar with controls and chart 
                         tabPanel("Supermarket Sales Dashboard",
                                  
                                  sidebarPanel(width = 4,style='border:2px solid; padding: 10px',
                                               selectInput('Product.line', 'Product Line',sort(unique(df$Product.line)), selected='Electronic accessories'),
                                               selectInput('Month', 'Months Name', unique(df$Month), selected='January'),
                                               selectInput('Branch', 'Branch Name', sort(unique(df$Branch)), selected='Mandalay')
                                  ),
                                  
                                  mainPanel(
                                    
                                    tabsetPanel(type = "tabs",
                                                
                                                tabPanel("Gross Profit",
                                                         tabsetPanel(
                                                           tabPanel("Plot", plotOutput("plot1")),
                                                           tabPanel("Table", tableOutput("table1"))
                                                         ),
                                                ),
                                                tabPanel("Net Sales",
                                                         
                                                         tabsetPanel(
                                                           type = "tabs",
                                                           tabPanel("Plot", plotOutput("plot2")),
                                                           tabPanel("Table", tableOutput("table2"))
                                                           
                                                         ),
                                                ),
                                                
                                                tabPanel("Units Sold",
                                                         tabsetPanel(
                                                           type = "tabs",
                                                           tabPanel("Plot", plotOutput("plot3")),
                                                           tabPanel("Table", tableOutput("table3"))
                                                         ),
                                                ),
                                                
                                                tabPanel("Gender",
                                                         tabsetPanel(
                                                           type = "tabs",
                                                           tabPanel("Plot", plotOutput("plot4")),
                                                           tabPanel("Table", tableOutput("table4"))
                                                         ),
                                                ),
                                                
                                                tabPanel("Payment Type",
                                                         tabsetPanel(
                                                           type = "tabs",
                                                           tabPanel("Plot", plotOutput("plot5")),
                                                           tabPanel("Table", tableOutput("table5"))
                                                         ),
                                                ),
                                                
                                                tabPanel("Customer Type",
                                                         tabsetPanel(
                                                           type = "tabs",
                                                           tabPanel("Plot", plotOutput("plot6")),
                                                           tabPanel("Table", tableOutput("table6"))
                                                         ),
                                                ),
                                                
                                                tabPanel("Daily Sales / Gross Profit",
                                                         tabsetPanel(
                                                           type = "tabs",
                                                           tabPanel("Net Sales", plotlyOutput("aniplot")),
                                                           tabPanel("Gross Profit", plotlyOutput("aniplot2"))
                                                         )
                                                )
                                    )
                                  ) 
                                  
                         ),
                         
                         # tags$style-s below is to overwrite shiny default colours
                         tags$style(
                           type = 'text/css',
                           HTML('
                                   .navbar{background-color: #337ab7; border-color: #2e6da4}
                                ')),
                         tags$style(
                           type = 'text/css',
                           HTML('
                                   .navbar-default .navbar-brand {color: white; }
                                ')),
                         tags$style(
                           type = 'text/css',
                           HTML('
                                   .navbar-default .navbar-nav>li>a {color: white; }
                                ')),
                         tags$style(
                           type = 'text/css',
                           HTML('
                                   .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover {color: black; }
                                ')),
                         tags$style(
                           type = 'text/css',
                           HTML('
                                   .navbar-default .navbar-nav>li>a:hover {color: #2299D4;}
                                ')),
                         tags$style(
                           type = 'text/css',
                           HTML('
                                   .navbar-header .navbar-brand:hover {color: #2299D4;}
                                ')),
                         
                         
                         tags$style(
                           type = 'text/css',
                           HTML('
                                   .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav, .navbar-default .navbar-nav>a:hover {color: black; }
                                '))
                         
                         
                         
                         
))


server <- function(input, output) {
  
  # plot 1
  output$plot1 <- renderPlot(
    {
      df2 <- df %>% select(Month, Branch, Product.line,Gross.Income) %>% 
        group_by(Month, Branch, Product.line) %>% 
        summarise(across(everything(), sum)) %>% 
        filter(Product.line == input$Product.line)
      df2$Month = factor(df2$Month, levels = month.name)
      
      ggplot(data=df2, aes(x=Month, y=Gross.Income, group=Branch)) +
        geom_line(aes(color=Branch), size=2)+
        geom_point(aes(color=Branch), size=3) +
        labs(title="Gross Profit Trend",x="Month", y = "Gross Profit (Myanmar kyat in millions)") +
        scale_colour_manual(values=c(B="#F4A460",C="#A0522D",A="#A9A9A9"))+
        theme(panel.background = element_rect(fill = "white", color = NA),
              plot.title = element_text(hjust = 0.5, size = 20),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_text(size = 15),
              axis.ticks.x=element_blank()
        )
    },  height = 700, width = 800
  )
  # table 1
  output$table1 <- renderTable(
    {
      df2 <- df %>% select(Month, Branch, Product.line,Gross.Income) %>% 
        group_by(Month, Branch, Product.line) %>% 
        summarise(across(everything(), sum)) %>% 
        filter(Product.line == input$Product.line)
      
      df2
    }
  )
  
  # plot 2
  output$plot2 <- renderPlot(
    {
      df3 <- df %>% select(Month,Branch,Product.line, Total) %>% group_by(Month,Branch,Product.line) %>%  summarise(across(everything(), sum)) %>%
        filter(Product.line == input$Product.line)
      df3$Month = factor(df3$Month, levels = month.name)
      
      ggplot(data=df3, aes(x=Month, y=Total, group=Branch)) +
        geom_line(aes(color=Branch), size=2)+
        geom_point(aes(color=Branch), size=3) +
        scale_colour_manual(values=c(B="#F4A460",C="#A0522D",A="#A9A9A9"))+
        labs(title="Net Sales Trend",x="Month", y = "Net Sales (Myanmar kyat in millions)") +
        theme(panel.background = element_rect(fill = "white", color = NA),
              plot.title = element_text(hjust = 0.5, size = 20),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_text(size = 15),
              #axis.text.x = element_blank(),
              axis.ticks.x=element_blank()
        )
    }, height = 700, width = 800
  )
  
  # table 2
  output$table2 <- renderTable(
    {
      df3 <- df %>% select(Month,Branch,Product.line, Total) %>% group_by(Month,Branch,Product.line) %>%  summarise(across(everything(), sum)) %>%
        filter(Product.line == input$Product.line)
      
      df3
    }
  )
  # plot animation Total
  output$aniplot <- renderPlotly(
    {
      accumulate_by <- function(dat, var) {
        var <- f_eval(var, dat)
        lvls <- plotly:::getLevels(var)
        dats <- lapply(seq_along(lvls), function(x) {
          cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
        })
        bind_rows(dats)
      }
      
      df8 <- df %>% select(Month,Branch,Product.line, Date, Total) %>% 
        group_by(Month,Branch,Product.line, Date) %>%  summarise(across(everything(), sum)) %>%
        filter(Product.line == input$Product.line, Month == input$Month, Branch == input$Branch)
      
      
      df8 <- df8 %>% accumulate_by(~Date)
      
      plot_ly(data = df8,
              x = ~df8$Date,
              y = ~df8$Total,
              frame = ~df8$frame,
              type = 'scatter',
              mode = 'lines+markers',
              markers = list(size = 10, color = '#F4A460'),
              line = list(color = '#F4A460', width = 2)
      ) %>% layout(
        title = "Daily Net Sales",
        width = 1000, height = 700,
        xaxis = list(
          range= c(head(df8$Date, n = 1), tail(df8$Date, n=1)+1),
          title = "Date"
        ),
        yaxis = list(
          title = "Daily Net Sales (Myanmar kyat in millions)"
        )
      ) %>% animation_opts(
        frame = 100,
        transition = 0,
        easing = "elastic",
        redraw = FALSE
      ) %>% animation_button(
        x = 1, xanchor = "right", y = 0, yanchor = "bottom"
      ) %>% animation_slider(
        currentvalue = list(prefix = "Daily :", font = list(color="steelblue"))
      ) 
      
    }
  )
  
  # plot animation Gross Profit
  output$aniplot2 <- renderPlotly(
    {
      accumulate_by <- function(dat, var) {
        var <- f_eval(var, dat)
        lvls <- plotly:::getLevels(var)
        dats <- lapply(seq_along(lvls), function(x) {
          cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
        })
        bind_rows(dats)
      }
      
      df9 <- df %>% select(Month,Branch,Product.line, Date, Gross.Income) %>% 
        group_by(Month,Branch,Product.line, Date) %>%  summarise(across(everything(), sum)) %>%
        filter(Product.line == input$Product.line, Month == input$Month, Branch == input$Branch)
      
      
      df9 <- df9 %>% accumulate_by(~Date)
      
      plot_ly(data = df9,
              x = ~df9$Date,
              y = ~df9$Gross.Income,
              frame = ~df9$frame,
              type = 'scatter',
              mode = 'lines+markers',
              markers = list(size = 10, color = '#F4A460'),
              line = list(color = '#F4A460', width = 2)
      ) %>% layout(
        title = "Daily Gross Profit",
        width = 1000, height = 700,
        xaxis = list(
          range= c(head(df9$Date, n = 1), tail(df9$Date, n=1)+1),
          title = "Date"
        ),
        yaxis = list(
          title = "Daily Gross Profit (Myanmar kyat in millions)"
        )
      ) %>% animation_opts(
        frame = 100,
        transition = 0,
        easing = "elastic",
        redraw = FALSE
      ) %>% animation_button(
        x = 1, xanchor = "right", y = 0, yanchor = "bottom"
      ) %>% animation_slider(
        currentvalue = list(prefix = "Daily :", font = list(color="steelblue"))
      ) 
      
    }
  )
  
  # plot 3
  
  output$plot3 <- renderPlot({
    df4 <- df %>% select(Month,Branch,Product.line, Quantity) %>% group_by(Month,Branch,Product.line) %>%  summarise(across(everything(), sum)) %>%
      filter(Month == input$Month)
    
    ggplot(data=df4, aes(x=Product.line, y=Quantity, fill = Branch)) +
      geom_bar(stat = "identity", 
               position = position_dodge())+
      coord_flip() +
      scale_fill_manual(values=c(B="#F4A460",C="#A0522D",A="#A9A9A9"))+
      labs(title="Units Sold",x="Product Types", y = "# in thousands") +
      geom_text(aes(label = Quantity),
                position = position_dodge(width = 1), size = 4, hjust = -0.10) + 
      theme(panel.background = element_rect(fill = "white", color = NA),
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.title.y = element_text(size = 15),
            axis.title.x = element_text(size = 15),
            #axis.title.x=element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x=element_blank()
      )
  }, height = 700, width = 800)
  
  # table 3
  output$table3 <- renderTable(
    {
      
      df4 <- df %>% select(Month,Branch,Product.line, Quantity) %>% group_by(Month,Branch,Product.line) %>%  summarise(across(everything(), sum)) %>%
        filter(Month == input$Month)
      
      df4
    }
  )
  
  # plot 4
  
  output$plot4 <- renderPlot({
    df5 <- df %>% select(Month,Branch,Product.line, Gender) %>%  group_by(Month,Branch,Product.line, Gender) %>%
      summarise(gender_n=n()) %>% 
      group_by(Month,Branch,Product.line) %>% 
      mutate(Percentage=round(gender_n/sum(gender_n)*100, 2)) %>%
      filter(Product.line == input$Product.line & Month == input$Month, Branch == input$Branch)
    
    ggplot(df5, aes(x="", y=Percentage, fill=Gender))+
      geom_bar(width = 1,stat = "identity") +
      coord_polar("y", start=0) +
      scale_fill_manual(values=c("#E69F00", "#999999")) +
      labs(title="Gender") + 
      geom_text(aes(y =  Percentage,
                    label = percent(Percentage/100)), size=6, position = position_stack(vjust = 0.5)) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x=element_blank()
      )
    
  })
  
  # table 4
  output$table4 <- renderTable(
    {
      df5 <- df %>% select(Month,Branch,Product.line, Gender) %>%  group_by(Month,Branch,Product.line, Gender) %>%
        summarise(gender_n=n()) %>% 
        group_by(Month,Branch,Product.line) %>% 
        mutate(Percentage=round(gender_n/sum(gender_n)*100, 2)) %>%
        filter(Product.line == input$Product.line & Month == input$Month, Branch == input$Branch)
      
      df5
    }
  )
  
  # plot 5
  
  output$plot5 <- renderPlot({
    
    df6 <- df %>% select(Month,Branch,Product.line, Payment) %>%  group_by(Month,Branch,Product.line, Payment) %>%
      summarise(payment_n=n()) %>% 
      group_by(Month,Branch,Product.line) %>% 
      mutate(Percentage=round(payment_n/sum(payment_n)*100, 2)) %>%
      filter(Product.line == input$Product.line & Month == input$Month, Branch == input$Branch)
    
    
    ggplot(df6, aes(x="", y=Percentage, fill=Payment))+
      geom_bar(width = 1,stat = "identity") +
      coord_polar("y", start=0) +
      scale_fill_manual(values=c("#E69F00", "#999999", "#A0522D")) +
      labs(title="Payment Type") + 
      geom_text(aes(y =  Percentage,
                    label = percent(Percentage/100)), size=6, position = position_stack(vjust = 0.5)) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x=element_blank()
      )
    
  })
  
  # table 5
  output$table5 <- renderTable(
    {
      df6 <- df %>% select(Month,Branch,Product.line, Payment) %>%  group_by(Month,Branch,Product.line, Payment) %>%
        summarise(payment_n=n()) %>% 
        group_by(Month,Branch,Product.line) %>% 
        mutate(Percentage=round(payment_n/sum(payment_n)*100, 2)) %>%
        filter(Product.line == input$Product.line & Month == input$Month, Branch == input$Branch)
      
      df6
    }
  )
  
  # plot 6
  
  output$plot6 <- renderPlot({
    df7 <- df %>% select(Month,Branch,Product.line, Customer.type) %>%  group_by(Month,Branch,Product.line, Customer.type) %>%
      summarise(cust_n=n()) %>% 
      group_by(Month,Branch,Product.line) %>% 
      mutate(Percentage=round(cust_n/sum(cust_n)*100, 2)) %>%
      filter(Product.line == input$Product.line & Month == input$Month, Branch == input$Branch)
    
    
    ggplot(df7, aes(x="", y=Percentage, fill=Customer.type))+
      geom_bar(width = 1,stat = "identity") +
      coord_polar("y", start=0) +
      scale_fill_manual(values=c("#E69F00", "#999999")) +
      labs(title="Customer Type") + 
      geom_text(aes(y =  Percentage,
                    label = percent(Percentage/100)), size=6, position = position_stack(vjust = 0.5)) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x=element_blank()
      )
    
  })
  
  # table 6
  output$table6 <- renderTable(
    {
      df7 <- df %>% select(Month,Branch,Product.line, Customer.type) %>%  group_by(Month,Branch,Product.line, Customer.type) %>%
        summarise(cust_n=n()) %>% 
        group_by(Month,Branch,Product.line) %>% 
        mutate(Percentage=round(cust_n/sum(cust_n)*100, 2)) %>%
        filter(Product.line == input$Product.line & Month == input$Month, Branch == input$Branch)
      
      df7
    }
  )
  
}
shinyApp(ui = ui, server = server)