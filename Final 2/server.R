library(shiny)
library(shinydashboard)
library(caret)
library(caTools)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(mlbench)

shinyServer(function(input,output) {
  
  output$histogram <- renderPlot({
    hist(faithful$eruptions , breaks = input$bins)
    
  })
  
  data <- read.csv( "cleveland.csv",  header=FALSE)
  
  names(data) <- c("age", "sex", "cp", "trestbps", "choi", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thai", "num")
  #head(data)
  
  data$num[data$num > 1] <- 1
  
  #summary(data)
  sapply(data, class)
  
  data <- transform(
    data,
    age=as.integer(age),
    sex=as.factor(sex),
    cp=as.factor(cp),
    trestbps=as.integer(trestbps),
    choi=as.integer(choi),
    fbs=as.factor(fbs),
    restecg=as.factor(restecg),
    thalach=as.integer(thalach),
    exang=as.factor(exang),
    oldpeak=as.numeric(oldpeak),
    slope=as.factor(slope),
    ca=as.factor(ca),
    thai=as.factor(thai),
    num=as.factor(num)
  )
  sapply(data, class)
  
  data[ data == "?"] <- NA
  colSums(is.na(data))
  
  data$thai[which(is.na(data$thai))] <- as.factor("3.0")
  data <- data[!(data$ca %in% c(NA)),]
  colSums(is.na(data))
  
  #summary(data)
  data$ca <- factor(data$ca)
  data$thai <- factor(data$thai)
  #summary(data)
  
  sample = sample.split(data$num, SplitRatio = .75)
  train = subset(data, sample == TRUE)
  test  = subset(data, sample == FALSE)
  #dim(train)
  #dim(test)
  
  rf <- randomForest(
    num ~ .,
    data=train
  )
  rtf <- rpart(
    num ~ .,
    data=train
  )
  pred = predict(rf, newdata=test[-14])  #random forest
  cm = table(test[,14], pred)

  control <- trainControl(method="cv", number=5)
  set.seed(7)
  fit <- train(num~., data=data, method="glm", metric="Accuracy", trControl=control)
  # display results
  
  
  
  output$accuray <- renderPrint({
    print(fit)
  })
  
  output$confusionmatrix <- renderPrint({
    table(test[,14], pred)
  })
  
  
  output$Tree <- renderPlot({
    rpart.plot(rtf, box.palette="RdBu", shadow.col="gray", nn=TRUE)
  })
  
  
  output$msgOutput <-renderMenu({
    
    msgs <- apply(read.csv("messages.csv") , 1 , function(row){
      messageItem(from = row[["from"]] , message = row[["messages"]])
    })
    
    dropdownMenu(type = "messages" , .list = msgs)
  })
  
} ) #closing of shiny server