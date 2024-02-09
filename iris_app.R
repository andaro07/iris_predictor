
# Import libraries
library(shiny)
library(data.table)
# library(randomForest)
library(RCurl)
library(randomForest)
library(caret)




iris_df = read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/iris.csv"))
# str(iris_df)

iris_df$Species = as.factor(iris_df$Species)

train_ids = createDataPartition(iris_df$Species, p=0.8, list = FALSE)
training_set = iris_df[train_ids,]
testing_set = iris_df[-train_ids,]

write.csv(training_set, "training_set.csv")
write.csv2(testing_set, "testing_set.csv")

train_set = read.csv("training_set.csv", header = TRUE)
train_set$Species = as.factor(train_set$Species)
train_set = train_set[,-1]


model = randomForest(Species~., data = train_set, ntree=500, mtry=4, importance=TRUE)

saveRDS(model, "model.rds")





iris_df = read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/iris.csv"))
# str(iris_df)

iris_df$Species = as.factor(iris_df$Species)

# Read in the RF model
model <- readRDS("model.rds")


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel(h1('Iris Species Predictor', style="color:steelblue")),
  
  # Input values
  sidebarPanel(
    tags$label(h3('Input parameters')),
    sliderInput("Sepal.Length", 
                 label = "Sepal Length", 
                  min = min(iris_df$Sepal.Length),
                  max = max(iris_df$Sepal.Length),
                  value = 5.3),
    sliderInput("Sepal.Width", 
                 label = "Sepal Width",
                  min = min(iris_df$Sepal.Width),
                  max = max(iris_df$Sepal.Width),
                 value = 3.6),
    sliderInput("Petal.Length", 
                 label = "Petal Length", 
                  min = min(iris_df$Petal.Length),
                  max = max(iris_df$Petal.Length),
                 value = 2.4),
    sliderInput("Petal.Width", 
                 label = "Petal Width", 
                 min = min(iris_df$Petal.Width),
                 max = max(iris_df$Petal.Width),
                 value = 0.8),
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Sepal Length",
               "Sepal Width",
               "Petal Length",
               "Petal Width"),
      Value = as.character(c(input$Sepal.Length,
                             input$Sepal.Width,
                             input$Petal.Length,
                             input$Petal.Width)),
      stringsAsFactors = FALSE)
    
    Species <- 0
    df <- rbind(df, Species)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)