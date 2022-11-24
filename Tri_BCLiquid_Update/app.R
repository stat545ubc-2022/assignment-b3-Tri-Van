library(shiny)
library(tidyverse)
library(colourpicker)


bcl <- read_csv("bcl-data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("BC Liquor Store Data"),
  h5("Welcome to my shiny app!"),
  br(),
  #New code
  checkboxGroupInput("CountryInput","New Feature:Select Country of Origin for your drinks!",
                     choices = c("CANADA","UNITED STATES OF AMERICA","FRANCE","IRELAND","ITALY","BRAZIL", "UNITED KINGDOM", "SPAIN", "GERMANY","PORTUGAL","ARGENTINA","ISRAEL","CZECH REPUBLIC","BELGIUM", "MEXICO","AUSTRALIA","SOUTH AFRICA","CHINA", "CHILE","NETHERLANDS"
                                 ,"JAMAICA","JAPAN","CUBA","GREECE","BULGARIA"
                                 , "DOMINICAN REPUBLIC","NEW ZEALAND","POLAND","AUSTRIA","TRINIDAD AND TOBAGO"
                                 ,"BERMUDA","ANTIGUA AND BARBUDA","MOROCCO","GUYANA","SWEDEN"
                                 ,"DENMARK","LATVIA","GEORGIA","FINLAND","BARBADOS"
                                 ,"NICARAGUA","INDIA","KOREA - SOUTH","LEBANON", "HUNGARY"
                                 ,"TAIWAN","TURKEY","SWITZERLAND", "SINGAPORE", "RUSSIA (USSR)"
                                 ,"ICELAND","VENEZUELA" ,"CROATIA","RUSSIA","PUERTO RICO"
                                 ,"THAILAND","FRENCH POLYNESIA (TAHITI)","GUATEMALA","PHILIPPINES","MONTENEGRO"
                                 ,"ST. CROIX","PANAMA","VIETNAM","PERU"  ),
                     selected = NULL), #New Feature 1: (functional widget) This feature is useful as it allows you to filter drink based on Country of origin.
  colourInput("col", "New Feature: Choose Color that you can see!", "green"), #New Feature 3: (functional widget/UI change) This feature is useful for user to choose the color of the graph that they can see, especially for color blind people.
  #End new code
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100,
                  value = c(25, 40), pre = "$"),
      radioButtons("typeInput", "Type",
                   choices = c("BEER", "REFRESHMENT","SPIRITS", "WINE"))
    ),
    mainPanel(
      plotOutput("alcohol_hist"),
      DT::dataTableOutput("data_table") #New Feature 2: (UI change) Create Interactive table that user can interact with to filter and reorder their search without changing the graph or other input.
    )
  ),
  a(href="https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv", "Link to the original data set")
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  filtered_data <-
    reactive({
      bcl %>% filter(Price > input$priceInput[1] &
                       Price < input$priceInput[2] &
                       Type == input$typeInput &
                       Country == input$CountryInput) #New Feature 1: (functional widget)
    })
  output$alcohol_hist <-
    renderPlot({
      filtered_data() %>%
        ggplot(aes(Alcohol_Content)) + geom_histogram(fill=input$col) #New Feature 3: (functional widget/UI change)
    })
  output$data_table <-
    DT::renderDataTable({   #New Feature 2: (UI change)
      filtered_data()
    })
}
# Run the application
shinyApp(ui = ui, server = server)
