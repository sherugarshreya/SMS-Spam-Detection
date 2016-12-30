SVM:

uci spam text dataset
3 libraries- shiny, e1071, Rtext tools imported

Code::(SERVER.R)

dat = read.table("SMS.txt", 
                 sep="\t",quote = "" ,
                 col.names=c("Spam", "Message"), 
                 fill=FALSE)

dat$cat <- ifelse(dat$Spam == "ham","0","1")
#table(data$cat)

dat <-dat[,c("Message","cat")]


library(RTextTools)
library(shiny)
library(e1071)
shinyServer(function(input, output) {
  sms <- eventReactive(input$submit, {
    input$sms})
  #sms <- reactive({
  #output$sms <- renderText(input$sms)
  # input$sms
  #})
  
  output$probab <- renderTable({detection(sms())})
  detection <- function(sms)
  {
    
    data <- dat
    nrt <- nrow(data)
    
    predictionData <- data.frame(Message=numeric(),cat=numeric())
    predictionData[nrow(predictionData) + 1, ] <- c(sms, 1)
    data <- rbind(data,predictionData)
    nre<- nrow(data)
    dtM <- create_matrix(data$Message,language="english",
                         stemWords=TRUE, removeSparseTerms=.99,removeStopwords = TRUE)
    container <- create_container(dtM, data$cat, trainSize=1:nrt,testSize = (nrt+1):nre,
                                  virgin=FALSE)
    SVM_1 <- train_model(container,"SVM",cost = 2)
    SVM_CLASSIFY_1 <- classify_model(container, SVM_1)
    analytics <- create_analytics(container,
                                  SVM_CLASSIFY_1)
    result <- analytics@document_summary
    result<-result[-c(3:8)]
    colnames(result)<-c("Category","Accuracy")
    result$Category <- ifelse(result$Category=="1","Spam","Ham")
    return(result)
  }
})



Code: UI.R

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




1.Shiny Library:- 
Shiny applications have two components: a user-interface definition and a server script. At one level, it’s very simple–a random distribution is plotted as a histogram with the requested number of bins.
The Shiny web framework is fundamentally about making it easy to wire up input values from a web page, making them easily available to you in R, and have the results of your R code be written as output values back out to the web page.
2.RTextTools
RTextTools is a free, open source machine learning package for automatic text classification that makes it simple for both novice and advanced users to get started with supervised learning. The package includes nine algorithms for ensemble classification (svm, slda, boosting, bagging, random forests, glmnet, decision trees, neural networks, maximum entropy), comprehensive analytics, and thorough documentation.
3.e1071	
This is package which is used to train the SVM classification algorithm.It has many other functionality such as Cross Validation and other Kernel Classifiers.


