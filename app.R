
library(tidyverse)
library(ggridges)
library(patchwork)
library(shiny)
library(viridis)
library(sf)
library(plotly)
library(DT)
library(ggradar)
library(readr)
library(maps)
library(shinylive)
library(httpuv)

# First Quarter 2023 Cost of Living

cost_of_living <- read_csv("Cost_of_living_Missouri_Economic_Research_and_Information_Center.csv")


# Clean Data
cost_of_living = cost_of_living%>%
  filter(State != "Ontario" & State != "British Columbia" & State != "Remote" & !is.na(State) & State != "PR" & State != "DC")%>%
  select(-Rank)%>%
  mutate(Rank = rank(Conversion))%>%
  rename(`Total Index` = Conversion)


#Pivot plot
cost_of_living_tall = cost_of_living%>%
  pivot_longer(cols = c(`Total Index`, Grocery, Housing, Utilities, Transportation, Health, Misc.), names_to = "Category", values_to = "Values" )


#App
line = function(data2, xaxis, yaxis='Housing') {
  xaxis2 = paste0('`', xaxis, '`')
  yaxis2 = paste0('`', yaxis, '`')
  p = ggplot(mapping = aes_string(x=xaxis2, y=yaxis2)) +
    geom_point(data = data2 %>% filter(selected),  aes(text = State), size = 2, alpha = 1) +
    geom_smooth(data = data2 %>% filter(selected), se = FALSE, color = "aquamarine3")+
    geom_point(data = data2 %>% filter(!selected),  size = 1, alpha = .3)+
    theme_minimal()+
    labs(title = paste0('All States: ', xaxis,  ' vs. ', yaxis ))
  ggplotly(p, tooltip = 'State') %>%
    style(hoveron = "fill")%>%
    config(displayModeBar = FALSE)
}


polar = function(data) {
  ggplot(data%>%
           filter(selected)%>%
           group_by(Category)%>%
           summarise(mean=mean(Values)),
         aes(y = mean , x = Category))+
    geom_col(fill = "aquamarine3", alpha = 0.9)+
    geom_text(aes(label = round(mean,1)), y = 80)+
    coord_polar(start = 2.15*3.14/3)+
    theme_void()+
    theme(axis.text.x = element_text(size = 12))+
    theme(plot.title = element_text(size = 19))+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(title = "Average Price Index",
         y = '',
         x = '')
}

map = function(cost_of_living_tall, category){
  data = cost_of_living_tall%>%
    filter(Category == category)%>%
    filter(selected)%>%
    mutate(hover = paste0(State,' ', Category, ": ", Values))
  plot_geo(data,
           locationmode = "USA-states")%>%
    add_trace(locations = ~State,
              z = ~Values,
              color = ~Values,
              text = ~hover,
              hoverinfo = 'text',
              reversescale = T)%>%
    layout(geo = list(scope = 'usa'),
           title = paste0("Price Index Map: ", category))%>%
    config(displayModeBar = FALSE) %>%
    colorbar(title = "Price Index")
}


ui <- fluidPage(
  titlePanel(
    h1("Cost of Living in America 2023 Q1", align = "center"),
  ),
  textOutput("credit"),
  textOutput("explain"),
  textOutput("total_index"),
  textOutput("space"),
  fluidRow(
    column(width = 3, 
           selectInput("selector", "Select States", choices = sort(cost_of_living$State), multiple = TRUE),
           selectInput("mapCat", "Map and X-axis", choices= c('Total Index', 'Grocery', 'Housing', 'Utilities', 'Transportation', 'Health', 'Misc.'), selected = 'Total Index', multiple = FALSE),
           selectInput("Yaxis", "Y-Axis", choices= c('Total Index', 'Grocery', 'Housing', 'Utilities', 'Transportation', 'Health', 'Misc.'), selected = 'Housing', multiple = FALSE)),
    column(width = 9, plotlyOutput("map"))
  ),
  fluidRow(
    column(width = 4, plotlyOutput("line")), 
    column(width = 8,  plotOutput("polar"))
  ),
  fluidRow(
    column(width = 8, DTOutput("tb"))
  )
)

server <- function(input, output) {
  cost_of_living2 <- reactive({
    cost_of_living%>%
      mutate(selected = (length(input$selector) == 0 | State %in% input$selector))
  })
  
  cost_of_living_tall2 <- reactive({
    cost_of_living2()%>%
      pivot_longer(cols = c(`Total Index`, Grocery, Housing, Utilities, Transportation, Health, Misc.), names_to = "Category", values_to = "Values" )
  })
  
  output$credit <-renderText(paste0("Data from Missouri Economic Research and Information Center (MERIC)"))
  
  output$explain <-renderText(paste0("Values are relative price indices. Mean Index is 100."))
  
  output$total_index <-renderText(paste0("'Total Index' is a weighted avearge of the distinct category indicies."))
  
  output$space <-renderText(paste0("______________________________________"))
  
  output$line <-renderPlotly({
    line(cost_of_living2(),input$mapCat, input$Yaxis)
  })
  
  output$polar<-renderPlot({
    polar(cost_of_living_tall2())
  })
  
  output$map <- renderPlotly({
    map(cost_of_living_tall2(), input$mapCat)
  })
  
  output$tb <- renderDT({
    datatable(
      data.frame(cost_of_living2()%>%
                   filter(selected)%>%
                   select(-selected)),
      options = list(
        columnDefs = list(
          list(targets = 0:5,
               width = '150px')
        )
      )
    )
  })
}

shinyApp(ui, server)
