library(shiny)
library(GuessR)
library(DT)

ui <- navbarPage(
    title = "GuessR Test Simulator",
    tabPanel("Create Quiz",
      splitLayout(
        verticalLayout(
          wellPanel(
            p(strong("Select question type to add")),
            br(),
          tabsetPanel(
            tabPanel("Multiple Choice",
              numericInput(inputId = "qus", label = "Number of Questions", value = 10),
              numericInput(inputId = "ans", label = "Number of Answers", value = 4),
              numericInput(inputId = "wts", label = "Weight of Questions", value = 1),
              numericInput(inputId = "ats", label = "Number of Attempts", value = 1),
              actionButton(inputId = "add", label = "Add to Test")
          ),
            tabPanel("True/False",
              numericInput(inputId = "qustf", label = "Number of Questions", value = 10),
              numericInput(inputId = "wtstf", label = "Weight of Questions", value = 1),
              numericInput(inputId = "atstf", label = "Number of Attempts", value = 1),              
              actionButton(inputId = "addtf", label = "Add to Test")
          ),
            tabPanel("Multiple Answer Question",
              numericInput(inputId = "qusmaq", label = "Number of Questions", value = 10),
              numericInput(inputId = "ansmaq", label = "Number of Answers", value = 4),
              numericInput(inputId = "corsmaq", label = "Number of Correct Answers", value = 2),
              numericInput(inputId = "wtsmaq", label = "Weight of Questions", value = 1),
              numericInput(inputId = "atsmaq", label = "Number of Attempts", value = 1),
              actionButton(inputId = "addmaq", label = "Add to Test")
          ),
          tabPanel("Options",
              sliderInput(inputId = "know", label = "Knowledge of Test Taker", min = 0, max = 1, step = 0.01, value = 0),
              numericInput(inputId = "trials", label = "Number of Trials",value = 1000)              
          )
        ),
          br(),
    
          fluidRow(
            column(width = 1),
            actionButton(inputId = "click", label = "Simulate"),
            actionButton(inputId = "reset", label = "Reset")
          )
      )),
      DT::DTOutput(outputId = "quiz", width = "60%"))),
    tabPanel("Results", 
      sidebarLayout(
        sidebarPanel(
          verticalLayout(
            p(strong("Summary Statistics")),
            tableOutput("stats"),
            sliderInput(inputId = "min",label = "Minimum",min = 0, max = 1,value = 0.4,step = 0.01),
            sliderInput(inputId = "max",label = "Maximum",min = 0, max = 1,value = 0.6,step = 0.01),
            actionButton("clickr","Calculate Ranges"),
            tableOutput("Ranges"))),
      mainPanel(
        plotOutput(outputId = "hist"))
      )
    ),
    tabPanel("Help",
               wellPanel(
            tabsetPanel(
              tabPanel("Creating a quiz",
                br(),
                p("On the first page you can see 4 tabs on the left, labelled 'Multiple Choice', 'True/False', 'Multiple Answer Question' and 'Options'. The first 3 of these tabs allow you to create a test to your desired specification. To add questions to the quiz, click on the tab describing to the type of question you would like to add, and adjust the inputs in the drop down bars to match your desired specifications. When you have set all options as you like, clicking on the 'Add to test' button will add them into the table seen on the right, which carries the details of the current test. You can use any of the tabs and add questions to the test as many times as you desire. If you have made a mistake when adding questions, the reset button at the bottom will allow you to clear the table and start over again.",style = "word-wrap: break-word;")),
              tabPanel("Simulation",
                br(),
                p("When you have created the desired test you can simulate the button by simply pressing the button labeled 'Simulate'. If you click on the options tab on the 'Create a quiz' page, it will give you options that will effect the simulation. The first option allows you to adjust to knowledge of the test taker. By increasing this, it introduces a probability that the simulated test taker will know the answer to the question, and as such will automatically get it correct without having to guess. This value is set to default at 0, but can be raised to a value between 0 and 1. If set to 1, all test takers will correctly answer every question. The next option down allows you to control the number of trials that the simulation will run. This is by default set at 1000, which will achieve a good degree of accuracy withouth the calculations taking an inconvenient amount of time. This value can be changed if desired. When the simulate button is clicked, there may be a delay while the simulation is being undertaken, after which the elements on the results will be populated.",style = "word-wrap: break-word;")),
              tabPanel("Evaluating Results",
                br(),
                p("If the test has been created and completed simulation, the results page will populate with figures and statistics relating to the simulation results. If you have clicked simulate and no results have appeared, it may be the case that the simulation is still ongoing, and needs more time to finish. On the right side of the page you can see a frequency plot, which shows the results of the simulation. At the top on the left side is a table showing various summary statistics, including the mean, median, mode and variance. Below this are two sliders, which allow you to calculate the percentage of results that fall over various ranges. To do this, adjust the sliders, and then press the 'Calculate Ranges' button. This will provide you with three values, the percentage of results below the range, the percentage of results inside the range, and the percentage of results above the range. It should be noted that the value for the percentage of the results inside the range is inclusive of the 2 slider values, and the other two values are not. If the minimum value is greater than the maximum value, the percentage below and percentage above will still be calculated based on the minimum and maximum slider values respectively, but the percentage between the values will be 0.",style = "word-wrap: break-word;"))
              
      ))),
    tabPanel("About",
             wellPanel(
      "This Shiny application was created by Daniel Nash, as part of his MMath project for the award of his MMORSE (Masters of Maths, Operational Research, Statistics and Economics) degree. The functionality of the application relies largely on a package also created as a part of this project, titled GuessR. This package is available for download at the github link https://github.com/DanNashSoton/GuessR. For question and comments, please contact me at  dn3g14@soton.ac.uk"
      ))
)

server <- function(input, output) {
  
  values <- reactiveValues()
  
  values$test <- data.frame(Answer = c(), PossAnswers = c(), Weights = c(), Attempts = c())
  
  newTest <- observe({
    if(input$add > 0) {
      quizinputans <- isolate(input$ans)
      quizinputqus <- isolate(input$qus)
      quizinputwts <- isolate(input$wts)
      quizinputats <- isolate(input$ats)
      isolate(values$test <- rbind(values$test,create_mc(quizinputans,quizinputqus,quizinputwts,quizinputats)))
    }
  })
  
  newTesttf <- observe({
    if(input$addtf > 0) {
      quizinputqus <- isolate(input$qustf)
      quizinputwts <- isolate(input$wtstf)
      quizinputats <- isolate(input$atstf)
      isolate(values$test <- rbind(values$test,create_tf(quizinputqus,quizinputwts,quizinputats)))
    }
  })
  
  newTestmaq <- observe({
    if(input$addmaq > 0) {
      quizinputans <- isolate(input$ansmaq)
      quizinputqus <- isolate(input$qusmaq)
      quizinputcors <- isolate(input$corsmaq)
      quizinputwts <- isolate(input$wtsmaq)
      quizinputats <- isolate(input$atsmaq)
      isolate(values$test <- rbind(values$test,create_maq(quizinputans,quizinputqus,quizinputcors,quizinputwts,quizinputats)))
    }
  })
  
  observeEvent(input$reset, {
    
    values$test <- data.frame(Answer = c(), PossAnswers = c(), Weights = c(), Attempts = c())
  })
  
  
  output$quiz <- DT::renderDataTable(values$test)
  
  result <- reactiveValues()
  
  observeEvent(input$click, {
    result <- sample_test(values$test,input$trials,input$know)
    meanres <- round(mean(result),3)
    medres <- round(median(result),3)
    modres <- round(modal(result),3)
    varres <- round(variance(result),3)
    resultsmat <- matrix(c("Mean","Median","Mode","Varience",meanres,medres,modres,varres),ncol = 2)
    colnames(resultsmat) <- c("Statistic","Value")
    output$hist <- renderPlot(plot(result))
    output$stats <- renderTable(resultsmat)
    
    observeEvent(input$clickr, {
      belres <- round(below_percentage(result,input$min,inclusive = FALSE),3)
      betres <- round(between_percentage(result,input$min,input$max),3)
      abores <- round(pass_percentage(result, input$max, inclusive  = FALSE),3)
      rangemat <- matrix(c("Below Range","Inside Range (Inclusive)","Above Range",belres,betres,abores),ncol = 2)
      colnames(rangemat) <- c("Range","Percentile")
      output$Ranges <- renderTable(rangemat)
    })
    
  })
  
  
}

shinyApp(ui, server)
