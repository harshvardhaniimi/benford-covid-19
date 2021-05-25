### Works!!!

library(shiny)
library(tidyverse)
library(benford)


data = read.csv("./data/owid-covid-data.csv")


# Define UI for the application
ui <- fluidPage(

    # Application title
    titlePanel("Benford's Law on COVID-19 Daily New Cases"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "country",
                label = "Select country or location",
                choices = unique(data$location),
                width = "100%"
            ),
            textOutput(outputId = "rmse"),
            hr(),
            helpText("Root Mean Square Error (RMSE) is a measure of deviation. Lower value implies greater adherance to Benford's Law. 
                     Live data is sourced from Our World in Data (OWID-COVID-19).")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           hr(),
           htmlOutput(outputId = "msg")
        )
    )
)

# Define server logic required
server <- function(input, output) {
    
    dat = read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
    
    
    check_benford = function(country)
    {
        daily_cases = dat %>% filter(location == country) %>% select(new_cases) %>% unlist() %>% as.numeric() %>% na.omit()
        bn = benford(daily_cases)
        
        mat = bn[[2]]
        
        plot(mat[,1], mat[,3], ylim = c(0, max(mat[,2], mat[,3])+5), type = "b", pch = 20, xlab = "Digit", ylab = "Frequency", main = paste0("Benford Distribution for ", country))
        lines(mat[,1], mat[,2], col = "red")
        points(mat[,1], mat[,2], col = "red", pch = 20)
        legend("topright", c("Expected", "Actual"), fill = c("black", "red"))
    }
    
    
    output$distPlot <- renderPlot({
        if(input$country == "") plot.new()
        if(input$country != "") {
            check_benford(input$country)
            }
    })
    
    output$rmse = renderText({
        country = input$country
        daily_cases = dat %>% filter(location == country) %>% select(new_cases) %>% unlist() %>% as.numeric() %>% discard(is.na)
        
        #if(length(daily_cases) == 0) 
        
        bn = benford(daily_cases)
        
        mat = bn[[2]]
        
        actual = mat[,2]
        expected = mat[,3]
        rmse = sqrt(mean((actual - expected)^2, na.rm = T))
        a = paste0("RMSE = ", round(rmse, 2))
        a
    })
    
    output$msg = renderText(
        paste("Benford analysis is used to detect fraud. First digits of a real-life dataset would ideally follow the distribution shown with 'black' colour. If the data is true, the red and black lines should overlap.
              <br><br>For more details, see <a href = 'https://www.harsh17.in/blog/is-covid-19-data-tampered'> my blog post</a>.")
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
