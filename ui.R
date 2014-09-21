data<-read.csv("map_data_GRI.csv", header = TRUE, sep = ";",comment.char= "")
inputYears <- colnames(data)[2:length(data)]
colnames(data)[2:length(data)]<-sub(pattern = "X*", replacement = "", x = inputYears)
colnames(data)[1]<-"Country"
minYear <- as.integer(colnames(data)[2])
maxYear <- as.integer(colnames(data)[length(colnames(data))])

shinyUI (pageWithSidebar (
  headerPanel ("GRI reports prediction"),
  sidebarPanel (
    helpText(a("Global reporting initiative (GRI)", 
               href="https://www.globalreporting.org/Pages/default.aspx",target="_blank"), 
             "is an organization, which helps companies all over the world to unify 
             their non-financial reporting in terms of",
             a("sustainable development", href = "http://en.wikipedia.org/wiki/Sustainable_development",target="_blank"),
             
             ". This app uses pre-processed data, obtained from appropriate",
             a("database", href = "http://database.globalreporting.org/search",target="_blank"),
             "about numbers of reports, published by companies of different countries in the world.",
             br(),
             br(),
             p("Application shows prediction interval of possible report numbers at the year right 
after the last one from the selected interval."),
             p("'Prediction' is quite theoretical, just for completing the course project. 
It is based on the simpliest linear regression model and, of course, can be improved:)
             Enjoy!")),
    br(),
    selectInput("country", 
                label = "Please, choose a country",
                choices = levels(data$Country),
                selected = "United States"),
    sliderInput('years', 'Years to show info about', 
                value =  c(minYear, maxYear), 
                min = minYear, max =  maxYear, 
                step =  1, format = "####")
  ),
  mainPanel (
    textOutput("selectedInfo"),
    plotOutput("plot"),
    strong((textOutput("predictionText")))
  )
)
)