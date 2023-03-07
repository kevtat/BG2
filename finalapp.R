library(shiny)
library(tidyverse)
library(plotly)
library(ggplot2)
library(shinythemes)

data <- read_delim("world_population_df.csv")

pop <- data %>% 
  select(Country, Continent, population_2022:population_2000) %>%
  pivot_longer(col=population_2022:population_2000, names_to="year", values_to="pop")  %>%
  mutate(year=as.numeric(str_extract(year, "\\d+")))

ui <- navbarPage("Info 201 Group BG Final Project: Research of World Population",
                 tabPanel("Overview",
                          h2(strong("World Population Trends from 200-2022")),
                          p("Our final project utilizes data gathered by population from the years",em("2000, 2010, 2015, 2020, 2022."),
                            "The purpose of our project is to examine trends or patterns in different continents in order to grasp how different
                            social, economic, and cultural differences contributed to a region's growth rate over the majority of the 21st century.
                            Our dataset, which we gathered from",strong(a("Kaggle.", href = "https://www.kaggle.com/datasets/iamsouravbanerjee/world-population-dataset")),
                            "With this dataset, we were also able to ovserve trends in density by area for both the different countries and continents. Additionally,
                            we observed current growth rate statistics for the world's population. Below is a map we plotted using the pakages ggplot, and tidyverse."),
                          h2(strong("Why We Chose These Data")),
                          p("The importance of studying world population is that population is a factor into other variables like the quality of life, growth rates, and 
                            social changes. It will also allow us to make insightful prediction on future changes of growth rate and population for each individual country."),
                          plotOutput("wPlot")
                 ),
                 tabPanel("Population Trend",
                          sidebarLayout(
                            
                            # Sidebar panel for inputs
                            sidebarPanel(
                              selectInput("country", "Select country:", 
                                          choices = unique(pop$Country), selected = "China"),
                              tableOutput("avg_pop")
                            ),
                            
                            # Main panel for displaying outputs
                            mainPanel(
                              plotlyOutput(outputId = "plot1")
                              
                            )
                          )    
                 ),
                 
                 tabPanel("Top 10 Countries by Population",
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              selectInput("continent", "Select continent:", 
                                          choices = unique(data$Continent), selected = "Asia"),
                              tableOutput("growth_rate")
                            ),
                            
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              plotlyOutput(outputId = "plot2"),
                            )
                          )    
                 ),
                 
                 tabPanel("Distribution of Population",
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              
                              selectInput("year", "Select year", 
                                          choices = unique(pop$year), selected = 2000)
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              plotlyOutput("plot3"),
                            )
                          )    
                 ),
                 tabPanel("Summary",
                          tableOutput("density_by_area"),
                 ),
                 theme = shinytheme("cerulean"),
)


# Define server logic required to draw a histogram
server <- function (input, output) {
    output$wPlot <- renderPlot({
      md <- map_data("world") %>% 
        select(long, lat,region, group) %>% 
        add_row(region = "United States") %>% 
        rename(Country = region)
      mapdata <- right_join(md, data, by ="Country")
      combineddata<-mapdata %>% 
        filter(!is.na(mapdata$Growth_Rate))
      
      
      x <-ggplot(combineddata, aes( x = long, y = lat, group=group, )) +
        geom_polygon(aes(fill = Growth_Rate), color = "black")
      y <- x + scale_fill_gradient(name = "Growth Rate", low = "khaki1", high =  "lightblue1")+
        theme(axis.title=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank())
      y
    })
    output$plot1 <- renderPlotly({
      pop %>% 
        filter(Country==input$country) %>%
        ggplot(aes(x=year, y=pop)) +
        geom_line(color="cornflowerblue") + geom_point(color="cornflowerblue") +
        labs(x="Year", y="Population", title=paste0("Population in ", input$country))
    })
    output$avg_pop <- renderTable({
      pop %>% 
        filter(Country==input$country) %>%
        summarize(average_pop_size_over_22_years = mean(pop)) 
    })
  
  output$plot2<- renderPlotly({
    data %>% 
      filter(Continent==input$continent) %>%
      arrange(desc(population_2022)) %>% head(10) %>%
      ggplot(aes(x=reorder(Country,population_2022) , y=population_2022)) +
      geom_col(fill="cornflowerblue") +
      labs(x="Country", y="Population", title=paste0("Top 10 Countries by Population in ",input$continent," (2022)")) +
      theme(axis.title.y=element_blank()) +
      coord_flip()
  })
  
  output$growth_rate <- renderTable({
    data %>% 
      filter(Continent==input$continent) %>%
      arrange(desc(Growth_Rate)) %>% 
      select(Country, Growth_Rate) %>% 
      head(10)
  }, digits = 4)
  
  output$plot3 <- renderPlotly({
    df <- pop %>% 
      filter(year==input$year) %>%
      group_by(Continent) %>% 
      summarise(pop=sum(pop))
    fig <- plot_ly(df, labels = ~Continent, values = ~pop, type = 'pie')
    fig <- fig %>% 
      layout(title = 'Distribution of Population in Each Continent',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
  })
  
  output$density_by_area <- renderTable({
    data %>% 
      group_by(Country) %>% 
      summarize(Area_in_km = sum(Area), Density_in_km = mean(Density)) %>%
      arrange(desc(Area_in_km)) %>% 
      head(10)
  })
}
# Create Shiny app ----
shinyApp(ui = ui, server = server)