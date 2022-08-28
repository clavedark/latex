################
#shiny app to illustrate location/scale parameters in CDF/PDF
#dc - 8.28.22
################
library(shiny)
library(ggplot2)

# Define UI for application that draws a pdf/cdf
ui <- fluidPage(

    # Application title
    titlePanel("Normal and Logistic Distributions"),

    # Sidebar with a slider input for mean, sd 
    sidebarLayout(
        sidebarPanel(
            sliderInput("mean",
                        "Mean of x:",
                        min = -4.00,
                        max = 4.00,
                        value = 0),
          sliderInput("sd",
                      "sd of x:",
                      min = 0.50,
                      max = 4.00,
                      value = 1)
        ),

        # Show plots of the generated distributions
        mainPanel(
          fluidRow( 
          column(6, 
           plotOutput("distPlot1")),
          column(6,
           plotOutput("distPlot2"))
        )
    ))
)


# Define server logic required to draw distributions
server <- function(input, output) {
    output$distPlot1 <- renderPlot({
        # generate parameters based on input$  from ui.R
        x    <- runif(1000, min = -4, max = 4)
        mean <-  input$mean
        sd <- input$sd
        ncdf <- pnorm(x, mean = mean, sd = sd)
        lcdf <- plogis(x, location=mean, scale=sd)
        plotdata1 <- data.frame(x, ncdf, lcdf)
        # draw the cdf with the specified mean/sd
        ggplot()+ geom_line(data = plotdata1, aes(x=x, y=lcdf, lty=1), color="gray20", lty=1)+
          geom_line(data = plotdata1, aes(x=x, y=ncdf), color="gray20", lty=2)+
          labs(x="z", y="Probability(z)") 
    })
    
    output$distPlot2 <- renderPlot({
      # generate parameters based on input$  from ui.R
      x    <- runif(1000, min = -4, max = 4)
      mean <-  input$mean
      sd <- input$sd
      npdf <- dnorm(x, mean = mean, sd = sd)
      lpdf <- dlogis(x, location=mean, scale=sd)
      plotdata2 <- data.frame(x, npdf, lpdf)
      # draw the pdf with the specified mean/sd
      ggplot()+ geom_line(data = plotdata2, aes(x=x, y=lpdf, lty=1), color="gray20", lty=1)+
        geom_line(data = plotdata2, aes(x=x, y=npdf), color="gray20", lty=2)+
        labs(x="z", y="Probability(z)") 

      
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
