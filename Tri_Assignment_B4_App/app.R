library(shiny)
library(tidyverse)


apt_buildings<- read_csv("apt_buildings.csv")
#glimpse(apt_buildings)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Toronto Apartment Information"),
    h3(div("This app allows you to search for apartment in Toronto based on selected criteria, and shows the number of apartment found for each ward in a chart.", style = "color: blue;")),
    img(src = "AllWardKey.png"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          sliderInput("no_of_units_Input", "Number of Units", 0, 1000,
                      value = c(30, 500), pre = "# of Units "),
          sliderInput("year_built_Input", "year built", 1900, 2200,
                      value = c(1900, 2010), pre = "Year "),
          selectInput("Feature_Input", "Which additional features would you like to filter for?", choices = c("AC","Heating Type","Allow Pet")),
          conditionalPanel(
            condition = "input.Feature_Input == 'AC'",
            selectInput("air_conditioning_Input","Choose type of Air Conditioning",
                               choices = c("CENTRAL AIR","INDIVIDUAL UNITS","NONE"))
                           ),
          conditionalPanel(
            condition = "input.Feature_Input == 'Heating Type'",
            selectInput("heating_type_Input","Choose type of heating",
                        choices = c("HOT WATER","ELECTRIC","FORCED AIR GAS"))
                          ),
          conditionalPanel(
            condition = "input.Feature_Input == 'Allow Pet'",
            selectInput("pets_allowed_Input","Choose whether pet is allowed",
                        choices = c("YES","NO"))
                          ),
          uiOutput("companyOutput")
       ),

    # main outputs
        mainPanel(
    # Show a plot of the generated distribution
          plotOutput("apt_hist"),
          br(), br(),

    # Show a table
          DT::dataTableOutput("data_table")
        )
    ),
    a(href="https://github.com/UBC-MDS/datateachr", strong("Link to the original data set"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$companyOutput <- renderUI({
    selectInput("facilities_available_Input", "Available Facilities",
                sort(unique(apt_buildings$facilities_available)),
                selected = "Recycling bins")
  })

  filtered_data <-
  reactive({
     apt_buildings %>% filter(no_of_units >= input$no_of_units_Input[1]
                            & no_of_units <= input$no_of_units_Input[2]
                            & year_built >= input$year_built_Input[1]
                            & year_built <= input$year_built_Input[2]
                            & air_conditioning == input$air_conditioning_Input
                            & heating_type == input$heating_type_Input
                            & pets_allowed == input$pets_allowed_Input
                            & facilities_available == input$facilities_available_Input
                            )
  })

  output$apt_hist <-
    renderPlot({
      filtered_data() %>%
        ggplot(aes(ward)) + geom_dotplot() + xlab("Ward Number")
    })

  output$data_table <-
    DT::renderDataTable({
      filtered_data()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
