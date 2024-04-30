#Importing libraries
library(shiny)
library(tidyverse)
library(caret)
library(kernlab)
library(imputeTS)

#Getting the data
hmo = read_csv(url("https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv"))

#Data Preparation
hmo$bmi <- na_interpolation(hmo$bmi)
hmo <- hmo[!is.na(hmo$hypertension),]
hmo<-hmo[complete.cases(hmo),]


#Preparing Expensive column
cost_threshold = 5000
hmo$expensive <- hmo$cost
hmo<-mutate(hmo, expensive = ifelse(cost > cost_threshold, TRUE, FALSE))
hmo <- data.frame(expensive=as.factor(hmo$expensive),
                  smoker=as.factor(hmo$smoker),
                  exercise=as.factor(hmo$exercise),
                  children=hmo$children,
                  gender=as.factor(hmo$gender),
                  age=hmo$age,bmi=hmo$bmi)

#Data preparation
set.seed(111)
trainList <- createDataPartition(y=hmo$expensive,p=.80,list=FALSE)
trainset <- hmo[trainList,]
testset <- hmo[-trainList,]

#Model
final_model <- ksvm(expensive ~ .,data=hmo,C=3,cross = 3, prob.model=TRUE)
our_model <- final_model
save(our_model, file = "our_model.rda")

#Getting the data
df_copy = read_csv(url("https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv"))

#Data Preparation
df_copy$bmi <- na_interpolation(df_copy$bmi)
df_copy <- df_copy[!is.na(df_copy$hypertension),]
df_copy<-df_copy[complete.cases(df_copy),]


us<-map_data("state")
df_copy$location<-tolower(df_copy$location)
m1<-aggregate(df_copy$cost,by=list(df_copy$location),FUN=mean)
m2<-aggregate(df_copy$cost,by=list(df_copy$location),FUN=max)
m3<-aggregate(df_copy$cost,by=list(df_copy$location),FUN=min)
m1<-m1%>%rename(location=Group.1)
m2<-m2%>%rename(location=Group.1)
aggmerge1<-merge(m1,m2,by = "location" )
m3<-m3%>%rename(location=Group.1)
aggmerge2<-merge(aggmerge1,m3,by= "location")
aggmerge2<-aggmerge2%>%rename(min=x,average=x.x,max=x.y)
m4<-aggmerge2[,c(2:4)]
usmerge<-merge(us,aggmerge2,all.x=TRUE,by.x="region",by.y="location")
usmerge<-usmerge%>%arrange(order)

usmap1<-ggplot(usmerge)+geom_polygon(aes(x=long,y=lat,group=group,fill=average),color="grey")+coord_map()
usmap1


library(shiny)
install.packages('shinythemes')
library(shinythemes)


# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("yeti"),
                
                
                navbarPage(
                  "HMO Analysis",
                  tabPanel("Upload Test",
                           fileInput(inputId = "upload", "Choose CSV File",accept = ".csv"),
                           fileInput("upload_Solution", label="HMO solution file",accept = c(".csv")),
                           #get a number (how much of the dataframe to show)
                           verbatimTextOutput("txt_results", placeholder = TRUE)),
                  
                  tabPanel("Dataset",
                           numericInput("n", "Number of Rows", value = 5, min = 1, step = 1),
                           #a place to output a table (i.e., a dataframe)
                           tableOutput("headForDF")),          
                  tabPanel("Visualisations",
                           
                          # plotOutput("distPlot",height = "400px"),
                           plotOutput("sp_age",height = "400px"),
                           plotOutput("sp_bmi",height = "400px"),
                           plotOutput("sp_children_cost",height = "400px"),
                           plotOutput("box_cost",height = "400px"),
                           plotOutput("map_plot",height = "400px"),
                           theme = shinytheme("yeti")
                           
                           
                           
                  ),
                ),
                
)


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  #require an input file, then read a CSV file
  getTestData <- reactive({
    req(input$upload)
    read_csv(input$upload$name)
  })
  #require an the actual values for the prediction (i.e. solution file)
  getSolutionData <- reactive({
    req(input$upload_Solution)
    read_csv(input$upload_Solution$name)
  })
  #show the output of the model
  output$txt_results <- renderPrint({
    #load the data
    dataset <- getTestData()
    dataset_solution <- getSolutionData()
    #load and use the model on the new data
    use_model_to_predict(dataset, dataset_solution)
  })
  
  #show a few lines of the dataframe
  output$headForDF <- renderTable({
    df <- getTestData()
    head(df, input$n)
  })
  
  
  #Scatter plot Age Vs Cost
  output$sp_age <- renderPlot(ggplot(data=df_copy) + aes(x=age, y=cost) + geom_point() + geom_smooth(method="lm", se=FALSE) + ggtitle("Age vs Cost"))
  #Scatter plot BMI Vs Cost
  output$sp_bmi <- renderPlot(ggplot(data=df_copy) + aes(x=bmi, y=cost) + geom_point() + geom_smooth(method="lm", se=FALSE) + ggtitle("BMI vs Cost"))
  #Scatter plot children Vs Cost
  output$sp_children_cost <- renderPlot(ggplot(data=df_copy) + aes(x=children, y=cost) + geom_point() + geom_smooth(method="lm", se=FALSE) + ggtitle("Children vs Cost"))
  output$box_cost <- renderPlot(boxplot(df_copy$cost,ylab = "cost",main = "Boxplot of healthcare cost"))
  output$map_plot <- renderPlot(ggplot(usmerge)+geom_polygon(aes(x=long,y=lat,group=group,fill=average),color="grey")+coord_map())

  
  output$TraindataConfusionMatrix <- renderPrint({
    #load the data
    dataset <- getTestData()
    dataset_solution <- getSolutionData()
    #load and use the model on the new data
    use_model_to_predict(dataset, dataset_solution)
  })
  }


#load a model, do prediction and compute the confusion matrix
use_model_to_predict <- function(df, df_solution){
  #load the pre-built model, we named it ‘out_model.rda’)
  load(file="our_model.rda")
  #use the model with new data
  svmPred <- predict(our_model, df, type = "response")
  #show how the model performed
  df_solution$expensive<- as.factor(df_solution$expensive)
  confusionMatrix(svmPred, df_solution$expensive)
  
}



# Run the application 
shinyApp(ui = ui, server = server)
