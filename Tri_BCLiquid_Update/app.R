library(shiny)
library(tidyverse)
library(colourpicker)


bcl <- read_csv("C:/Users/Tri Van/Desktop/UBC PhamSci/UBC Y2 FALL 2022/STAT 545A Exploratory Data Analysis/stat545/Shiny App/bcl-data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("BC Liquor Store Data"),
  h5("Welcome to my shiny app!"),
  br(),
  #New code
  checkboxGroupInput("CountryInput","country",
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
                     selected = NULL), #New code 1
  colourInput("col", "Choose Color that you can see!", "green"), #New code 3: Choose color for colorblind
#  textOutput("text"),
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
      DT::dataTableOutput("data_table") #New code 2: Create Interactive table
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
                       Country == input$CountryInput) #New code 1
    })
  output$alcohol_hist <-
    renderPlot({
      filtered_data() %>%
        ggplot(aes(Alcohol_Content)) + geom_histogram(fill=input$col) #New code 3: Choose color for colorblind
    })
  output$data_table <-
    DT::renderDataTable({   #New code 2: Create Interactive table
      filtered_data()
    })
#  output$text <- renderText({
#      filtered_data()%>%row()%>%as.factor()
#    })
}
# Run the application
shinyApp(ui = ui, server = server)
