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
