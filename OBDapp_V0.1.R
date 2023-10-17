#Libraries: 
library(shiny)
library(ggplot2)
#library(googleway)
#library(rdrop2)
transmat <- matrix(nrow = 7, ncol = 2)
transmat[,1] <- c("cyl", "carb", "disp", "wt", "mpg", "hp", "qsec")
transmat[,2] <- c("Cylinders", "Carburetors", "Displacement", "Weight", "Miles per Gallon", "Horsepower", "Quarter Mile")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Motor Trend Cars"),
  
  # Sidebar layout with input and output definitions -----
  sidebarLayout(
    # Sidebar panel 
    sidebarPanel(
      selectInput("xvar", "Car Characteristic", c("Cylinders" = "cyl", "Carburetors" = "carb", "Displacement" = "disp", "Weight" = "wt")), 
      selectInput("yvar", "Performance Characteristic", c("Miles Per Gallon" = "mpg", "Horsepower" = "hp", "Quarter Mile" = "qsec"))
    ),

      # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plots", plotOutput(outputId = "plots")), 
                  tabPanel("Stastistics", tableOutput(outputId="statistics")), 
                  tabPanel("Data",
                    h3("Description from R Manual:"),
                    textOutput(outputId="paragraph1"),
                    h3("Strengths:"),
                    textOutput(outputId="paragraphStrengths"),
                    h3("Weaknesses:"),
                    textOutput(outputId="paragraphWeaknesses")
                  )
      )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  output$plots <- renderPlot({
    xvar <- which(names(mtcars) == input$xvar)
    yvar <- which(names(mtcars) == input$yvar)
    if(input$xvar %in% c("cyl", "carb")){
      qplot(factor(mtcars[,xvar]), mtcars[,yvar], data = mtcars, xlab = transmat[which(transmat[,1] == input$xvar),2], ylab = transmat[which(transmat[,1] == input$yvar),2], geom = c("boxplot"))
    } else {
      reg_res <- lm(mtcars[,yvar] ~ mtcars[,xvar])
      full_x_label <- paste0(transmat[which(transmat[,1] == input$xvar),2], "; r-squared: ", paste0(round(cor(mtcars[,xvar], mtcars[,yvar])^2, 3)*100, "%"))
      qplot(mtcars[,xvar], mtcars[,yvar], data = mtcars, geom = c("point", "smooth"), method = "lm", xlab = full_x_label, ylab = transmat[which(transmat[,1] == input$yvar),2])
    }
  })
  
  output$statistics <- renderTable({
    xvar <- which(names(mtcars) == input$xvar)
    yvar <- which(names(mtcars) == input$yvar)
    if(input$xvar %in% c("cyl", "carb")){
      uniquevars <- sort(unique(mtcars[,which(names(mtcars) == input$xvar)]))
      resmat <- matrix(nrow = length(uniquevars), ncol = 6)
      i <- 1
      for(uniquevar in uniquevars){
        resmat[i,] <- summary(mtcars[which(mtcars[,xvar] == uniquevar), yvar])
        i <- i + 1
      }
      resmat <- cbind(uniquevars, resmat)
      colnames(resmat) <- c(transmat[which(transmat[,1] == input$xvar),2], "Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
      resmat
    } else {
      resmat <- matrix(nrow=3, ncol=2)
      reg_res <- lm(mtcars[,xvar] ~ mtcars[,yvar])
      resmat[1,1] <- "Regr Intercept"
      resmat[1,2] <- round(reg_res$coefficients[1], 1)
      resmat[2,1] <- "Regr Other Coef"
      resmat[2,2] <- round(reg_res$coefficients[2], 1)
      resmat[3,1] <- "R-Squared"
      resmat[3,2] <- paste0(round(cor(mtcars[,xvar], mtcars[,yvar])^2, 3)*100, "%")
      colnames(resmat) <- c(" ", transmat[which(transmat[,1] == input$xvar),2])
      resmat
    }
  })
  
  output$paragraph1 <- renderText({
    "The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile 
    design and performance for 32 automobiles (1973–74 models)."
  })
  
  output$paragraphStrengths <- renderText({
    "The mtcars dataset is baked into all modern installations of R and used as an example in the help functions of many packages. This includes the
    ubiquitous graphics package ggplot2. This makes it an ideal data set for developing statistical functions in PHP, as verifying results against R results 
    is easy."
  })
  
  output$paragraphWeaknesses <- renderText({
    "The mtcars dataset is very small and very, very old. There is no practicality to looking at data from cars which for the most part are not on the road
    anymore, with the examples which are representing antiques. Modern audiences may be confused by archaic terms such as carburetor or rear axle ratio."
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

