library(shiny)

shinyUI(fluidPage(
    titlePanel("Would you Survive the Titanic?"),
    tags$head(tags$style("
                  #container{font-size:25px;
                    color:black;
                    display:inline;
                  }")),
    sidebarLayout(
        sidebarPanel(
            selectInput("class", "Select your Class", c("1st", "2nd", "3rd")),
            selectInput("gender", "Select your Gender", c("Female", "Male")),
            sliderInput("parch", "How many parents/children do you have?", 0, 6, value = 0),
            sliderInput("age", "What is your age?", 1, 80, value = 28)
        ),
        mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("APP", br(), 
                                 div(id="container", style="text-align:center", h3("Your probability of surviving is: "), textOutput("pred")),
                                 plotOutput("plot")), 
                        tabPanel("About", br(), h3("This app calculates the probability of surviving the Titanic given 4 variables: gender, age, class, and number of parents and children. The probability is calculated with a Random Forest regression modelled on the historic data."), br(), h3("The output is the calculated probability of survival given the variables inputted by the user, and a graph showing how the probability of survival changes by changing some of the variables."))
            )
        )
    )
))