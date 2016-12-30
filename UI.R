library(shiny)

shinyUI(fluidPage(
  headerPanel('Spam Detection'),
  sidebarPanel(
    textInput("sms", "SMS", ""),
    actionButton("submit","Submit")
  ),
  mainPanel(
    tableOutput('probab'),
    textOutput('sms')
  )
))
