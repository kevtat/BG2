library(shiny)
library(tidyverse)
library(plotly)
library(ggplot2)
library(shinythemes)
library(maps)

data <- read_delim("world_population_df.csv")

pop <- data %>% 
  select(Country, Continent, population_2022:population_2000) %>%
  pivot_longer(col=population_2022:population_2000, names_to="year", values_to="pop")  %>%
  mutate(year=as.numeric(str_extract(year, "\\d+")))

# Navbar to move between pages
ui <- navbarPage("Info 201 Group BG Final Project: Research of World Population",
                 tabPanel("Overview",
                          h1(strong("World Population Trends (2000-2022)")),
                 # Split layouut between image and paragraphs
                 splitLayout(
                   img(src = "https://nouvelles.umontreal.ca/fileadmin/_processed_/csm_20230125_demographique_8M_df2b84274b.jpg", width = "562px", height = "375px"),
                   div(
                     h3(strong("Applying Our Data")),
                     p("Our final project utilizes data gathered by population from the years",em("2000, 2010, 2015, 2020, and 2022"),". Our project aims to examine trends or 
                     patterns in different continents to grasp how different social, economic, and cultural differences contributed to a region's growth rate over the 
                     majority of the 21st century. We accessed out dataset from",
                       strong(a("Kaggle.", href = "https://www.kaggle.com/datasets/iamsouravbanerjee/world-population-dataset")),
                       "With this dataset, we were also able to observe trends in density by area for both the different countries and continents. Additionally,
                        we observed current growth rate statistics for the world's population."),
                    h3(strong("Why We Chose These Data")),
                    p("The importance of studying world population is that population is a factor in other variables like the quality of life, growth rates, and social changes.
                      It will also allow us to make insightful predictions on future growth rate and population changes for each country. Understanding trends over time in relation 
                      to current trends will help researchers predict changes in our world."),
                   ),
                 # Removed error white space
                 cellArgs = list(style='white-space: normal;')
                   )
),
tabPanel("Population Trend",
         sidebarLayout(
           
           # Sidebar panel for inputs
           sidebarPanel(
             selectInput("country", "Select country:", 
                         choices = unique(pop$Country), selected = "China"),
             h4(strong("Top Ten Countries by Change in Population (2000-2022)")),
             tableOutput("avg_pop")
           ),
           
           # Main panel for displaying outputs
           mainPanel(
             plotlyOutput(outputId = "plot1"),
             h4("Findings:"),
             p("While most countries have experienced reasonable population growth over the past 22 years,",em("India and China"),
               "Have experienced dramatically high growth in population, with",em("India")," experiencing dramatic growth of",
               strong("3,357,539,498"),"in population."),
             p("We believe that this general trend in population growth throughout the world is due to the improvement in quality
               of life for many people. This includes access to life changing technology, medicine, and information. While there still
               must be more work done in order to improve the lives of all human beings, our findings from this dataset reflect a very 
               promising trend in the right direction. Our findings did; however, demonstrate reason to be concern of overpopulation"),
             p("Although population growth in many countries has recently slowed down, and is expected to experience a decrease, it is nevertheless
               concerning to see the extent of growth in countries such as",em("China and India,")," and if such growth may be possible 
               in other countries. Our group worries that there may be issues due to a lack of natural resources in the coming future.")
           )
         )    
),


tabPanel("Top 10 Countries by Population",
         sidebarLayout(
           
           # Sidebar panel for inputs ----
           sidebarPanel(
             selectInput("continent", "Select continent:", 
                         choices = unique(data$Continent), selected = "Asia"),
             h4(strong("Top Ten Countries by Growth Rate (2022)")),
             tableOutput("growth_rate")
           ),
           
           
           # Main panel for displaying outputs ----
           mainPanel(
             plotlyOutput(outputId = "plot2"),
             h4("Findings:"),
             p("According to our findings from the bar graph, the two largest countries by population in each continent are",em("China and India"),"in Asia, ",
               em("Russia and Germany"),"in Europe, ",em("Nigeria and Ethiopia"),"in Africa, ", em("Australia and Papau New Guinea"),"in Oceania, ",
               em("the United States and Mexico"),"in North America, and ",em("Brazil and Colombia"),"in South America.
             Nevertheless, our findings are not reflective of the top ten countries experiencing the highest population growth rate in each continet"),
             p("According to our findings from the table, the two countries experiencing the greatest population growth currently in each continent are",
               em("Syria and Afghanistan"),"in Asia, ", em("Moldova and Poland"),"in Europe, ",em("Niger and DRC"),"in Africa, ", em("Vanuatu and Solomon Islands"),
               "in Oceania, ",em("Honduras and Nicaragua"),"in North America, and ",em("French Guiana and Bolivia"),"in South America."),
             p("While the findings from our table were frankly far from what we had anticipated, due to the preportedly high current growth rate
               in countries such as Syria, Afghanistan, and Moldova, it is clear that a population's size does not necessarily correlate to its growth rate.
               In fact, many of the largest countries by population, are on a steady decline in population growth.")
             
           )
         )    
),

tabPanel("Distribution of Population",
         sidebarLayout(
           
           # Sidebar panel for inputs ----
           sidebarPanel(
             
             selectInput("year", "Select year", 
                         choices = unique(pop$year), selected = 2000),
             h4(strong("Top Ten Countries by Area in km2 and Their Respective Densities (2022)")),
             tableOutput("density_by_area")
           ),
           
           # Main panel for displaying outputs ----
           mainPanel(
             plotlyOutput("plot3"),
             h4("Findings:"),
             p("The pie chart demonstrates that Asia has and still makes up the largest percentage of the global population.
               However, as seen by changing the input for selected year, Asia has been on a very minor decline in its representation
               such a large portion of the global population over the past 22 years. On the other hand, Africa, which represents 17.9% of the
               global population, as of 2022, has been on a steady rise in their representation of the total global population. These findings indicate
               that Asia is on a population decline, whereas Africa is on an incline. Europe and the Americas have also seen a fall in 
               their representation of the total gloabl population. We found that Oceania occupies such as small percentage of the global population that
               any change in representation of the total population is insignificant."),
             p("According to our table, population density is not reflective of a country's area size, as many countries, such as Australia have
               a population density of 3.40, despite being ranked the 6th largest country by area. On the other hand, India, which is ranked the 7th
               largest country by area, has a notic high density of 431.07.")
           )
         )    
),
tabPanel("Summary",
         splitLayout(
           # divided between heading and paragraph
           div(
             h2(strong("Our Findings/Analysis")),
             h5(em(style="color:lightblue1","To the right is a map we plotted using the pakages ggplot, maps, and tidyverse.")),
             p("According to our endeavors, one can observe that the world population continues to rise over the past 20 years. Looking through the scope 
               of the population growth rate as demonstrated earlier in the web app, one can quickly conclude that almost all countries in the world are 
               showing positive population growth. We can attribute this positive population growth to the rapid modernization over the past 20 years. 
               This is especially salient in African countries due to their accelerated socio-economic growth. Overall, the quality of this dataset was 
               excellent for the most part--- with a few poorly named columns that we have to revise. Nonetheless, this dataset is unbiased and would not 
               harm certain population groups. In the future, we hope to find a replacement for the ggplot2 world map due to the missing values that it has. 
               We could also incorporate a dataset that includes facets such as GDP, GDP per capita, and GDP growth, to fully visualize the socioeconomic 
               advancements that the world has experienced over the last 20 years."),
           ),
           # plot output of world population graph
           plotOutput("wPlot"),
           cellArgs = list(style='white-space: normal;')
         )
),
# uses package shiny theme
theme = shinytheme("cerulean"),
)


# Define server logic required to draw a histogram
server <- function (input, output) {
  
  output$plot1 <- renderPlotly({
    pop %>% 
      filter(Country==input$country) %>%
      ggplot(aes(x=year, y=pop)) +
      geom_line(color="cornflowerblue") + geom_point(color="cornflowerblue") +
      labs(x="Year", y="Population", title=paste0("Population in ", input$country," (2000-2022)")) +
      theme(text = element_text(family = "Helvetica-Bold")) 
  })
  
  # Function to build table based on difference in population
  output$avg_pop <- renderTable({
    data %>% 
      group_by(Country) %>% 
      mutate(Difference = population_2022 - population_2000) %>% 
      arrange(desc(Difference)) %>% 
      select(Country, Difference) %>% 
      head(10)
  }, digits = 0) #Removes auto-rounding of population
  
  # Creates bar plot that shows top ten countries by population based on user input for continent
  output$plot2<- renderPlotly({
    data %>% 
      filter(Continent==input$continent) %>%
      arrange(desc(population_2022)) %>% head(10) %>%
      ggplot(aes(x=reorder(Country,population_2022) , y=population_2022)) +
      geom_col(fill="cornflowerblue") +
      labs(x="Country", y="Population", title=paste0("Top 10 Countries by Population in ",input$continent," (2022)")) +
      theme(text = element_text(family = "Helvetica-Bold")) +
      theme(axis.title.y=element_blank()) +
      coord_flip()
  })
  
  # Builds table that shows top ten countries by growth rate based on user input for continent
  output$growth_rate <- renderTable({
    data %>% 
      filter(Continent==input$continent) %>%
      arrange(desc(Growth_Rate)) %>% 
      select(Country, Growth_Rate) %>% 
      head(10)
  }, digits = 4)
  
  # Creates pie chart that shows the representation of population by continent based on user input for year
  output$plot3 <- renderPlotly({
    df <- pop %>% 
      filter(year==input$year) %>%
      group_by(Continent) %>% 
      summarise(pop=sum(pop))
    fig <- plot_ly(df, labels = ~Continent, values = ~pop, type = 'pie')
    fig <- fig %>% 
      layout(title = "Population Distribution",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
  })
  
  # Builds table that shows the top ten countries by area and displays their relative density in km^2
  output$density_by_area <- renderTable({
    data %>% 
      group_by(Country) %>% 
      summarize(Area_in_km = sum(Area), Density_in_km = mean(Density)) %>%
      arrange(desc(Area_in_km)) %>% 
      head(10)
  })
  
  # Creates map with map package that shows countries based on world population percentage as seen on our dataset 
  output$wPlot <- renderPlot({
    data$Country <- recode(data$Country,'United States' = 'USA',
                           'United Kingdom' = 'UK', 
                           'DR Congo' = 'Democratic Republic of the Congo',
                           'Republic of the Congo' = 'Republic of Congo')
    world_map <- map_data("world") %>% 
      rename(Country = region)
    new_map <- full_join(world_map, data, by = "Country")
    ggplot(new_map, aes(long, lat, group = group)) +
      geom_polygon(aes(fill = `World Population Percentage`), color="gray") +
      scale_fill_gradient(name = "World Population Percentage", low = "lightblue1", high =  "navyblue")+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank())
  })
}
# Create Shiny app ----
shinyApp(ui = ui, server = server)