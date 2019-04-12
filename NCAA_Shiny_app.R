###############
# Regression Analysis of NCAA Coaching Salary Data
###############

# Load required libraries
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(shiny)
library(car)

# Set working directory and import data
setwd("/Users/morganwalterhiggins/Developer/R/NCAA")
total_salary <- read.csv("total_salary.csv")

##########
# Shiny Code: Multiple Regression
##########

# PART 1 - Import data and libraries (after running code above)
mydat <- total_salary

# PART 2 - Define User Interface
ui <- fluidPage(
  
  # Application title
  titlePanel("NCAA Basketball Coach Salaries: Multiple Linear Regression"),
  
  # Sidebar with input options
  sidebarLayout(
    sidebarPanel(
      
      # Choose independent variables for analysis
      selectInput("indepVar",                            # Name of input
                  "REQUIRED: Independent Variables",   # Display Label
                  multiple = TRUE,
                  choices = list("CAREER_TOTAL","CAREER_WIN","CAREER_LOSE","CAREER_NCAA","CAREER_S16",
                                 "CAREER_FF","CAREER_CHAMP","SCHOOL_TOTAL","SCHOOL_WIN","SCHOOL_LOSE",
                                 "SCHOOL_NCAA","SCHOOL_S16","SCHOOL_FF","SCHOOL_CHAMP")),   # Specify choices
      # Choose dependent variable for analysis
      selectInput("depVar",                                # Name of input
                  "Select Dependent Variable",             # Display Label
                  multiple = FALSE,
                  choices = list("SCHOOL_PAY","TOTAL_PAY","MAX_BONUS","SCHOOL_BUYOUT")),   # Specify choices
      
      # Select Conference from Dropdown Menu
      selectInput("conference",                       # Name of input
                  "Select Conference",                # Display Label
                  choices = c("All" = "all",          # Available choices in the dropdown
                              "ACC" = "ACC",
                              "Big Ten" = "Big Ten",
                              "Big 12" = "Big 12",
                              "Pac-12" = "Pac-12",
                              "SEC" = "SEC")),
      
      # Filter Input for School Rank
      sliderInput("rankInput",                   # Name of input
                  "School Ranks",                # Display Label
                  min = 1,                       # Lowest Value of Range
                  max = 76,                      # Highest Value of Range
                  value = c(1, 76),              # Pre-selected values
                  pre = "Rank ",                 # Unit to display
                  step = 1),                     # Size per step change
      
      # Filter Input for Salary Range
      sliderInput("salaryInput",                   # Name of input
                  "Salary Range",                  # Display Label
                  min = 250000,                    # Lowest Value of Range
                  max = 9000000,                   # Highest Value of Range
                  value = c(250000, 9000000),      # Pre-selected values
                  step = 10000)                   # Size per step change
      

    ),
    
    # Items to show in the Main Panel
    mainPanel(
      
      tabsetPanel(type = "tabs",

                 tabPanel("Scatterplot Matrix", plotOutput("scatterplot")),       # Scatter plot matrix
                 tabPanel("Model Summary", verbatimTextOutput("summary")),        # Regression output
                 tabPanel("Model Diagnostics", plotOutput("diagnostic"))  # Regression diagnostic plots
      )
    )
  )
)

# PART 3 - Define server logic required to make model and plot matrix
server <- function(input, output) {
  
    # TAB 1: Scatterplot Matrix  
    output$scatterplot <- renderPlot({
    
    # Filter data based on user input (rank/salary)
    filtered <- mydat %>%
      filter(RANK >= input$rankInput[1],
             RANK <= input$rankInput[2],
             SCHOOL_PAY >= input$salaryInput[1],
             SCHOOL_PAY <= input$salaryInput[2]
      )
    
    # Filter data based on conference
    ## All conferences
    if (input$conference == "All") {
      filtered <- filtered
    }
    ## ACC Only
    else if (input$conference == "ACC") {
      filtered <- filtered[filtered$CONFERENCE== 'ACC',]
    }
    ## Big Ten Only
    else if (input$conference == "Big Ten") {
      filtered <- filtered[filtered$CONFERENCE== 'Big Ten',]
    }
    ## Big 12 Only
    else if (input$conference == "Big 12") {
      filtered <- filtered[filtered$CONFERENCE== 'Big 12',]
    }
    ## Pac-12 Only
    else if (input$conference == "Pac-12") {
      filtered <- filtered[filtered$CONFERENCE== 'Pac-12',]
    }
    ## SEC Only
    else if (input$conference == "SEC") {
      filtered <- filtered[filtered$CONFERENCE== 'SEC',]
    }
    
    #Set up scatterplot matrix
      # Correlation panel
    panel.cor <- function(x, y){
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- round(cor(x, y), digits=2)
      txt <- paste0("R = ", r)
      cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = cex.cor * r)
    }
      # Customize upper panel
    upper.panel<-function(x, y){
      points(x,y, pch = 19)
    }
      # Histogram panel
    panel.hist <- function(x)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(usr[1:2], 0, 1.5) )
      h <- hist(x, plot = FALSE)
      breaks <- h$breaks; nB <- length(breaks)
      y <- h$counts; y <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = "cyan")
    }
    # Create the plots
    pairs(filtered[,c(input$depVar,input$indepVar)],
          lower.panel = panel.cor,
          upper.panel = upper.panel,
          diag.panel = panel.hist)
  }, height=800)
  
  # TAB 2: Regression output
  output$summary <- renderPrint({
    
    # Filter data based on user input (rank/salary)
    filtered <- mydat %>%
      filter(RANK >= input$rankInput[1],
             RANK <= input$rankInput[2],
             SCHOOL_PAY >= input$salaryInput[1],
             SCHOOL_PAY <= input$salaryInput[2]
      )
    
    # Filter data based on conference
    ## All conferences
    if (input$conference == "All") {
      filtered <- filtered
    }
    ## ACC Only
    else if (input$conference == "ACC") {
      filtered <- filtered[filtered$CONFERENCE== 'ACC',]
    }
    ## Big Ten Only
    else if (input$conference == "Big Ten") {
      filtered <- filtered[filtered$CONFERENCE== 'Big Ten',]
    }
    ## Big 12 Only
    else if (input$conference == "Big 12") {
      filtered <- filtered[filtered$CONFERENCE== 'Big 12',]
    }
    ## Pac-12 Only
    else if (input$conference == "Pac-12") {
      filtered <- filtered[filtered$CONFERENCE== 'Pac-12',]
    }
    ## SEC Only
    else if (input$conference == "SEC") {
      filtered <- filtered[filtered$CONFERENCE== 'SEC',]
    }
    
    # Set up multiple linear regression model
    factors <- paste(c(colnames(filtered[,input$indepVar])), collapse = "+")
    outcome <- paste(input$depVar)
    
    print(paste("Number of Independent Variables: ",length(input$indepVar)))
    
    if (length(input$indepVar) == 1){
      form <- filtered[,input$depVar] ~ filtered[,input$indepVar]
      fit <- lm(form)
      names(fit$coefficients) <- c("Intercept", input$indepVar)
    }

    else if (length(input$indepVar) > 1){
      form <- paste(outcome,"~", factors)
      print(paste("Model: ",form))
      fit <- lm(form, data = filtered)
    }
    
    summary(fit)
  })
    
    # TAB 3: Regression diagnostic plots
    output$diagnostic <- renderPlot({

      # Filter data based on user input (rank/salary)
      filtered <- mydat %>%
        filter(RANK >= input$rankInput[1],
               RANK <= input$rankInput[2],
               SCHOOL_PAY >= input$salaryInput[1],
               SCHOOL_PAY <= input$salaryInput[2]
        )

      # Filter data based on conference
      ## All conferences
      if (input$conference == "All") {
        filtered <- filtered
      }
      ## ACC Only
      else if (input$conference == "ACC") {
        filtered <- filtered[filtered$CONFERENCE== 'ACC',]
      }
      ## Big Ten Only
      else if (input$conference == "Big Ten") {
        filtered <- filtered[filtered$CONFERENCE== 'Big Ten',]
      }
      ## Big 12 Only
      else if (input$conference == "Big 12") {
        filtered <- filtered[filtered$CONFERENCE== 'Big 12',]
      }
      ## Pac-12 Only
      else if (input$conference == "Pac-12") {
        filtered <- filtered[filtered$CONFERENCE== 'Pac-12',]
      }
      ## SEC Only
      else if (input$conference == "SEC") {
        filtered <- filtered[filtered$CONFERENCE== 'SEC',]
      }

      # Set up multiple linear regression model
      factors <- paste(c(colnames(filtered[,input$indepVar])), collapse = "+")
      outcome <- paste(input$depVar)

      if (length(input$indepVar) == 1){
        form <- filtered[,input$depVar] ~ filtered[,input$indepVar]
        fit <- lm(form)
        names(fit$coefficients) <- c("Intercept", input$indepVar)
      }

      else if (length(input$indepVar) > 1){
        form <- paste(outcome,"~", factors)
        fit <- lm(form, data = filtered)
      }
      
      par(mfrow=c(2,2))
      plot(fit)

  }, height=800)
    
  
}

# PART 4 - Run the application on localhost
#shinyApp(ui = ui, server = server)

# To run the app over local network, unccomment the following:
runApp(shinyApp(ui = ui, server = server), port = 3838, host="0.0.0.0")


