library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Here we go!"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins to put in:",
                        min = 1,
                        max = 50,
                        value = 30),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot")
        )
    )
)
