library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rintrojs)


####functions#####

transactions <- function(number,names,paid, advanced, due, curr) {
  
  #saldo table
  names = names[1:number]
  paid = paid[1:number]
  due = due[1:number]
  if (advanced==1 & sum(paid) ==sum(due)) {
    goal = due
  } else {
    goal = mean(paid) 
  }
  CF = paid - goal
  ft = data.frame(names, paid, CF)
  
  #transaction table
  trans <- matrix(c(1, 1, 1), ncol=3, byrow=TRUE)
  colnames(trans) <- c('Debtor','Amount','Receiver')
  rownames(trans) <- c('1')
  trans <- as.data.frame(trans)
  
  #Calculation
  sorted = ft[order(ft$CF),]
  count = 1
  
  while ((sum(abs(sorted$CF))) > 0.1) {
    sorted = sorted[order(sorted$CF),]
    trans[count,]=c(sorted$names[1], paste0( round(abs(sorted$CF[1]),2)," ", curr), sorted$names[number])
    sorted$CF[number] = sorted$CF[number] + sorted$CF[1]
    sorted$CF[1]=0
    
    count=count+1 }
  return(trans)
}

mean_pay <- function(number,amounts) {
  return(round(mean(amounts[1:number]),2))
}

sum_pay <- function(number,amounts) {
  return(round(sum(amounts[1:number]),2))
}




# Define UI for application 
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  
  # Application title
  titlePanel("Split the bill - app"),
  
  # input

  sliderInput("group", "Number of friends", value = 2, min = 0, max = 8),
  
  radioButtons("Currency","Currency:",
               choices = list("€" = "€", "$" = "$","£" = "£"),selected = "€", inline=T),
  
  fluidRow(
    column(6, splitLayout(
    textInput("name1", "Name first friend"),
    numericInput("num1", "Paid", value = 0, width = '30%'))),
    column(6, splitLayout(
    textInput("name2", "Name second friend"),
    numericInput("num2", "Paid", value = 0, width = '30%')))),
  
  fluidRow(
    column(6, 
    conditionalPanel(
      condition = "input.group >= 3",
      splitLayout(
      textInput("name3", "Name third friend"),
      numericInput("num3", "Paid", value = 0, width = '30%')))),
    
    column(6, 
    conditionalPanel(
      condition = "input.group >= 4",
      splitLayout(
      textInput("name4", "Name fourth friend"),
      numericInput("num4", "Paid", value = 0, width = '30%'))))),
  
  fluidRow(
    column(6, 
           conditionalPanel(
             condition = "input.group >= 5",
             splitLayout(
               textInput("name5", "Name fifth friend"),
               numericInput("num5", "Paid", value = 0, width = '30%')))),
    
    column(6, 
           conditionalPanel(
             condition = "input.group >= 6",
             splitLayout(
               textInput("name6", "Name sixth friend"),
               numericInput("num6", "Paid", value = 0, width = '30%'))))),
  
  fluidRow(
    column(6, 
           conditionalPanel(
             condition = "input.group >= 7",
             splitLayout(
               textInput("name7", "Name seventh friend"),
               numericInput("num7", "Paid", value = 0, width = '30%')))),
    
    column(6, 
           conditionalPanel(
             condition = "input.group >= 8 && input.advanced == 0",
             splitLayout(
               textInput("name8", "Name eighth friend"),
               numericInput("num8", "Paid", value = 0, width = '30%'))),
           conditionalPanel(
             condition = "input.group >= 8 && input.advanced == 1",
             splitLayout(
               textInput("name8", "Name eighth friend"),
               numericInput("num8", "Paid", value = 0, width = '30%'))))),
  
  # Advanced setting 
  
  fluidRow(
    column(3, 
  checkboxInput("advanced", "Advanced setting", value = FALSE)),
    column(3,
  actionButton('info', 'Info'))),
  

  conditionalPanel(
    condition = "input.advanced == 1",
    
  fluidRow(
    column(3, 
     div( id='color_due1' ,numericInput("due1", textOutput("name1"), value = 0 , width = '60%'))),
      tags$style(type="text/css", "#color_due1 {color: #315EB9}"),
    column(3, 
      div( id='color_due2' ,numericInput("due2", textOutput("name2"), value = 0 , width = '60%'))),
    tags$style(type="text/css", "#color_due2 {color: #315EB9}"),
    column(3, 
    conditionalPanel(
     condition = "input.group >= 3",
     div( id='color_due3' ,
     numericInput("due3", textOutput("name3"), value = 0 , width = '60%')))),
    tags$style(type="text/css", "#color_due3 {color: #315EB9}"),
    column(3,
    conditionalPanel(
     condition = "input.group >= 4",
     div( id='color_due4' ,
     numericInput("due4", textOutput("name4"), value = 0 , width = '60%'))))),
    tags$style(type="text/css", "#color_due4 {color: #315EB9}"),
  fluidRow(
    column(3,
   conditionalPanel(
     condition = "input.group >= 5",
     div( id='color_due5' ,
     numericInput("due5", textOutput("name5"), value = 0 , width = '60%')))),
   tags$style(type="text/css", "#color_due5 {color: #315EB9}"),
    column(3,
    conditionalPanel(
     condition = "input.group >= 6",
     div( id='color_due6' ,
      numericInput("due6", textOutput("name6"), value = 0 , width = '60%')))),
   tags$style(type="text/css", "#color_due6 {color: #315EB9}"),
    column(3, 
    conditionalPanel(
      condition = "input.group >= 7",
      div( id='color_due7' ,
     numericInput("due7", textOutput("name7"), value = 0 , width = '60%')))),
   tags$style(type="text/css", "#color_due7 {color: #315EB9}"),
    column(3, 
   conditionalPanel(
     condition = "input.group >= 8",
     div( id='color_due8' ,
     numericInput("due8", textOutput("name8"), value = 0 , width = '60%'))))),
    tags$style(type="text/css", "#color_due8 {color: #315EB9}")),
  
  # Show output
  
  conditionalPanel(
    condition = "input.advanced == 1",
    span(textOutput("stilldue"), style="color:#315EB9")),
  icon("credit-card"),
  textOutput("total"),
  textOutput("mean"),
  

  fluidRow(
    column(12, align="center", h3("Transactions"),
      tableOutput("Transactions"))),
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  hr(),
  h6("Webapp developed by Simon", align = "right")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$info,{
    showModal(modalDialog(
      title = "Info",
      "Here, you can specify exactly how much each person is due.",
      br(),
      "For example: when paying a restaurant bill, you can detail the price of everyone's dish."
    ))
  })
  
  output$name1 <- renderText({ 
    paste0(input$name1," is due")
  })
  
  output$name2 <- renderText({ 
    paste0(input$name2," is due")
  })
  
  output$name3 <- renderText({ 
    paste0(input$name3," is due")
  })
  
  output$name4 <- renderText({ 
    paste0(input$name4," is due")
  })
  
  output$name5 <- renderText({ 
    paste0(input$name5," is due")
  })
  
  output$name6 <- renderText({ 
    paste0(input$name6," is due")
  })
  
  output$name7 <- renderText({ 
    paste0(input$name7," is due")
  })
  
  output$name8 <- renderText({ 
    paste0(input$name8," is due")
  })
  
  output$stilldue <- renderText({ 
    if (sum_pay(input$group, c(input$num1 ,  input$num2 , input$num3 ,input$num4, input$num5 ,  input$num6 , input$num7 ,input$num8))
        -sum_pay(input$group, c(input$due1 ,  input$due2 , input$due3 ,input$due4, input$due5 ,  input$due6 , input$due7 ,input$due8)) == 0) {
      "The count is good !"
    }
      
    else{
      paste0("This won't work, you still need to add ", 
             sum_pay(input$group, c(input$num1 ,  input$num2 , input$num3 ,input$num4, input$num5 ,  input$num6 , input$num7 ,input$num8))
             -sum_pay(input$group, c(input$due1 ,  input$due2 , input$due3 ,input$due4, input$due5 ,  input$due6 , input$due7 ,input$due8))
             ," ", input$Currency)}
  })
  
  output$total <- renderText({ 
    paste0("Total amount paid is ",sum_pay(input$group, c(input$num1 ,  input$num2 , input$num3 ,input$num4, input$num5 ,  input$num6 , input$num7 ,input$num8))," ", input$Currency)
  })
  
  
  output$mean <- renderText({ 
    if (input$advanced == 1) {
      "Each person has to pay its due. Make sure that the sum of the due amounts equals the Total amount paid. "}
    
    else {
    paste0("Each person has to pay ", mean_pay(input$group, c(input$num1 ,  input$num2 , input$num3 ,input$num4, input$num5 ,  input$num6 , input$num7 ,input$num8))," ", input$Currency) }

  })
  
  output$Transactions <- renderTable(
    transactions(input$group, 
                 c(input$name1 ,  input$name2 , input$name3 ,input$name4, input$name5 ,  input$name6 , input$name7 ,input$name8),
                 c(input$num1 ,  input$num2 , input$num3 ,input$num4, input$num5 ,  input$num6 , input$num7 ,input$num8), 
                 input$advanced,
                 c(input$due1 ,  input$due2 , input$due3 ,input$due4, input$due5 ,  input$due6 , input$due7 ,input$due8),
                 input$Currency)                                 
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
