data <- read.csv("map_data_GRI.csv", header = TRUE, sep = ";",comment.char= "")
inputYears <- colnames(data)[2:length(data)]
colnames(data)[2:length(data)]<-sub(pattern = "X*", replacement = "", x = inputYears)
colnames(data)[1]<-"Country"

myPlot <- function(countryName, timeInterval, values){
  plot(timeInterval, values, 
       type ="b", xlab="years", ylab="Number of GRI reports", 
       pch = 19, main = countryName)
  fit<-lm(values~timeInterval)
  beta <- round(fit$coefficients, 4)
  regr.formula = paste("y = ", beta[1]," + ",beta[2],"*x")
  mtext(regr.formula, col = "blue")
  abline(fit, col = "blue", lw = 2)
  confData <- data.frame(x = timeInterval)
  pred<-predict(fit, confData, interval = ("prediction"))
  lines(timeInterval, pred[,2], lty = 2);
  lines(timeInterval, pred[,3], lty = 2);
}

emptyPlot<- function(countryName, timeInterval){
  if (length(timeInterval>1)){
    plot(x = timeInterval, y = rep(0, length(timeInterval)), type = "n", 
         xlab = "years", ylab = "Number of GRI reports", main = countryName)  
  } else {
    plot(x = timeInterval, y = 0, type = "n", 
         xlab = "year", ylab = "Number of GRI reports", main = countryName)  
  }
  mtext("Sorry, not enough data to predict. Please, try another country or time interval.", col = "red")
}

shinyServer(
  function(input, output) {
    dataSet<-reactive({
      x <- c(input$years[1]:input$years[2])

      y <- data[data$Country==input$country, colnames(data) %in% x]
      good<- !is.na(y)
      y.good<-y[good]
      x.good<-x[good]
      data.frame(years = x.good, reports = y.good)
    })
    prediction <-reactive({
      if (length(dataSet()$years)>2){
        timeInterval<-dataSet()$years
        reports<-dataSet()$reports
        fit<-lm(reports~timeInterval)
        nextYear<-timeInterval[length(timeInterval)]+1
        timeInterval <- append(timeInterval, nextYear)
        confData <- data.frame(x = timeInterval)
        pred<-predict(fit, confData, interval = ("prediction"))
        str<-paste("Based on input parameters, in ",nextYear," number of GRI reports could be in [", 
        max(0,round(pred[dim(pred)[1],2])),",",
        max(0,round(pred[dim(pred)[1],3])), 
        "] interval.")
      } else {
        str<-paste ("Prediction is not possible.")
      }
    str
    })
    
    output$selectedInfo  <- renderText({
      paste("You have selected to show information about GRI reports in ", input$country, 
            " from ", input$years[1]," to ", input$years[2],
            ". In given period ",length(dataSet()$years), " years had some data.")
    })

    output$plot <- renderPlot({
      curData<-dataSet()
      if (length(curData$years) > 2){
        myPlot(input$country,curData$years,curData$reports)
      } else {
        emptyPlot(input$country, c(input$years[1]:input$years[2]))
      }   
   })
   
    output$predictionText<- renderText(prediction())
  }
)

