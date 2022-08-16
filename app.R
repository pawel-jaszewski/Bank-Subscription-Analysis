rm(list=ls())
data <- read.csv2("bank-additional-full.csv")

data$euribor3m <- scan(text=data$euribor3m, dec=".", sep=",")
data$nr.employed <- scan(text=data$nr.employed, dec=".", sep=",")
data$cons.conf.idx <- scan(text=data$cons.conf.idx, dec=".", sep=",")
data$emp.var.rate <- scan(text=data$emp.var.rate, dec=".", sep=",")
data$cons.price.idx <- scan(text=data$cons.price.idx, dec=".", sep=",")

library(shiny)
library(ggplot2)
library(pROC)
library(e1071)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)

# UI
ui <- fluidPage(
    
    navbarPage(title = "Bank subscription analysis",
               
   tabPanel("Description of the problem and dataset",
            
            fluidPage(
                mainPanel(
                    HTML(
                        "Source: <a href='https://archive.ics.uci.edu/ml/datasets/Bank+Marketing#'>
        https://archive.ics.uci.edu/ml/datasets/Bank+Marketing#</a>"
                    )
                )
            ),
            
            fluidPage(
                
                titlePanel("Analyzed problem"),
                mainPanel(
                    HTML(
                        "<p>
     The data used in the analysis is related to <b>the direct marketing campaigns of a Portuguese
     banking institution</b>, which were based on phone calls.<br/>
     The aim of the analysis is to find which variables are the most significant statistically
     when it comes to the fact<b> whether a given client subscribed to the product (bank term deposit)
     or not</b>.<br/>
     Using the analysis and the created models, it will be easy to predict if the exact client will be
     less or more eager to subscribe to the product. As a consequence, this knowledge will make planning
     marketing campaigns much more efficient.<br/>
     Two models were built: <b>the logistic regression model and the decision tree model</b>.
     Thanks to this, it is also possible to easily compare which model predicts values
     with higher accuracy and which model should be used for future purposes.
     <p>"
                    )
                )
            ),
            
            fluidPage(
                
                titlePanel("Dataset"),
                mainPanel(
                    HTML(
                        "<p>
        The dataset contains 41,188 observations and 21 variables: 20 attributes (independent variables) and 1 output
        attribute (dependent variable).</br>
        
        <h4><b>Input variables:</b></h4>
        
        <h5><b>Demographic and financial situation data:</b><br/></h5>
        - <b>age</b> (numeric)<br/>
        - <b>job</b>: type of job (categorical: 'admin.', 'blue-collar', 'entrepreneur', 'housemaid', 'management', 'retired', 'self-employed', 'services', 'student', 'technician', 'unemployed', 'unknown')</br>
        - <b>marital</b>: marital status (categorical: 'divorced', 'married', 'single' 'unknown'; Note: 'divorced' means divorced or widowed)<br/>
        - <b>education</b> (categorical: 'basic.4y', 'basic.6y', 'basic.9y', 'high.school', 'illiterate', 'professional.course', 'university.degree', 'unknown')<br/>
        - <b>default</b>: has credit in default? (categorical: 'no', 'yes', 'unknown')<br/>
        - <b>housing</b>: has housing loan? (categorical: 'no', 'yes', 'unknown')<br/>
        - <b>loan</b>: has personal loan? (categorical: 'no', 'yes', 'unknown')<br/>
        
        <h5><b>Related to the last contact of the current campaign:</b></h5>
        - <b>contact</b>: contact communication type (categorical: 'cellular', 'telephone')<br/>
        - <b>month</b>: last contact month of year (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')<br/>
        - <b>day_of_week</b>: last contact day of the week (categorical: 'mon','tue','wed','thu','fri')<br/>
        - <b>duration</b>: last contact duration, in seconds (numeric).<br/>
        Important note: this attribute highly affects the output target (e.g., if duration=0 then y='no'). Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.<br/>
        
        <h5><b>Other attributes:</b></h5>
        - <b>campaign</b>: number of contacts performed during this campaign and for this client (numeric, includes last contact)<br/>
        - <b>pdays</b>: number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)<br/>
        - <b>previous</b>: number of contacts performed before this campaign and for this client (numeric)<br/>
        - <b>poutcome</b>: outcome of the previous marketing campaign (categorical: 'failure', 'nonexistent', 'success')
        
        <h5><b>Social and economic context attributes:</b></h5>
        - <b>emp.var.rate</b>: employment variation rate - quarterly indicator (numeric)<br/>
        - <b>cons.price.idx</b>: consumer price index - monthly indicator (numeric)<br/>
        - <b>cons.conf.idx</b>: consumer confidence index - monthly indicator (numeric)<br/>
        - <b>euribor3m</b>: euribor 3 month rate - daily indicator (numeric)<br/>
        - <b>nr.employed</b>: number of employees - quarterly indicator (numeric)<br/>
        
        <h4><b>Output variable:</b></h4>
        - <b>y</b> - has the client subscribed a term deposit? (binary: 1 - 'yes', 0 - 'no')
        </p>"))
            )
   ),
   
   tabPanel("Data Visualization",
            
            fluidRow(column(width = 4,
                            
                            radioButtons(inputId = "vars_ggplot",
                                         label = "Select variables to see their distribution",
                                         choices = c("age", "job", "marital", "education", "default",
                                                     "housing", "loan", "contact", "day_of_week", "month", "duration",
                                                     "campaign", "pdays", "previous", "poutcome", "emp.var.rate",
                                                     "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed"),
                                         selected = c("job"))),
                     
                     column(width = 8, plotOutput("visualization_plot"))),
            
            fluidRow(column(width = 4,
                            
                            radioButtons(inputId = "vars_ggplot2",
                                         label = "Select variables to see their distribution with the breakdown of y values",
                                         choices = c("age", "job", "marital", "education", "default",
                                                     "housing", "loan", "contact", "day_of_week", "month", "duration",
                                                     "campaign", "pdays", "previous", "poutcome", "emp.var.rate",
                                                     "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed"),
                                         selected = c("job"))),
                     
                     column(width = 8, plotOutput("visualization_plot_y"))),
            
            fluidRow(column(width = 4,
                            
                            radioButtons(inputId = "vars_boxplot",
                                         label = "Select variables for the boxplot visualization",
                                         choices = c("age", "duration", "campaign", "pdays", "previous",
                                                     "emp.var.rate", "cons.price.idx", "cons.conf.idx",
                                                     "euribor3m", "nr.employed"),
                                         selected = c("age"))),
                     
                     column(width = 8, plotOutput("plot_boxplot"))),
            
            sidebarLayout(
                sidebarPanel(
                    
                    radioButtons(inputId = "vars_point_x",
                                 label = "Select x-axis variable for the scatterplot visualization
                                        (numeric variables)",
                                 choices = c("age", "duration", "campaign", "pdays", "previous",
                                             "emp.var.rate", "cons.price.idx", "cons.conf.idx",
                                             "euribor3m", "nr.employed"),
                                 selected = c("age")),
                    
                    
                    radioButtons(inputId = "vars_point_y",
                                 label = "Select x-axis variable for the scatterplot visualization
                                        (numeric variables)",
                                 choices = c("age", "duration", "campaign", "pdays", "previous",
                                             "emp.var.rate", "cons.price.idx", "cons.conf.idx",
                                             "euribor3m", "nr.employed"),
                                 selected = c("campaign"))
                    
                ),
                
                mainPanel(plotOutput("plot_point"))
            ),
            
            sidebarLayout(
                sidebarPanel(
                    
                    radioButtons(inputId = "vars_bar_x",
                                 label = "Select x-axis variable for the barplot visualization
                                        (categorical variables)",
                                 choices = c("job", "marital", "education", "default",
                                             "housing", "loan", "contact", "day_of_week", "month",
                                             "poutcome"),
                                 selected = c("job")),
                    
                    
                    radioButtons(inputId = "vars_bar_y",
                                 label = "Select stacked variable for the barplot visualization
                                        (categorical variables)",
                                 choices = c("job", "marital", "education", "default",
                                             "housing", "loan", "contact", "day_of_week", "month",
                                             "poutcome"),
                                 selected = c("housing"))
                    
                ),
                
                mainPanel(plotOutput("plot_bar"))
            )
            
   ),
   
   tabPanel("Logistic Regression",
            
            sidebarLayout(
                sidebarPanel(

                    sliderInput(inputId = "t",
                                label = "Choose a threshold to see how the confusion matrix and its statistics
                                    will change",
                                min = 0,
                                max = 1,
                                value = 0.15,
                                step = 0.01),
                    
                    
                plotOutput("plotPredict")),
                
                mainPanel(verbatimTextOutput("ConfusionMatrix"))
            ),
            
            sidebarLayout(
                sidebarPanel(
                    
                    numericInput(inputId = "seed",
                                 label = "Change the seed of the random number generator used to divide the dataset
                                        into train and test datasets",
                                 value = 3456,
                                 min = 2000,
                                 max = 8000,
                                 step = 1),
                    
                    
                    checkboxGroupInput(inputId = "vars_logistic",
                                       label = "Select variables for the logistic regression model",
                                       choices = c("age", "job", "marital", "education", "default",
                                                   "housing", "loan", "contact", "day_of_week", "month", "duration",
                                                   "campaign", "pdays", "previous", "poutcome", "emp.var.rate",
                                                   "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed"),
                                       selected = c("contact", "campaign", "pdays", "poutcome","emp.var.rate",
                                                    "cons.price.idx", "cons.conf.idx"))),
                
                mainPanel(
                    
                    plotOutput("ROC"),
                    verbatimTextOutput("ROCPrint"),
                    verbatimTextOutput("glmPrint"))
            ),
            
   ),
   
   tabPanel("Decision Tree",
            
            sidebarLayout(
                sidebarPanel(
                    
                    sliderInput(inputId = "t2",
                                label = "Choose a threshold to see how the confusion matrix and its statistics
                                        will change",
                                min = 0,
                                max = 1,
                                value = 0.15,
                                step = 0.01),
                    
                    
                    plotOutput("tree_predict")),
                
                mainPanel(verbatimTextOutput("tree_conf_matrix"))
            ),
            
            
            sidebarLayout(
                sidebarPanel(
                    
                    numericInput(inputId = "seed2",
                                 label = "Change the seed of the random number generator used to divide the dataset
                                    into train and test datasets",
                                 value = 3456,
                                 min = 2000,
                                 max = 8000,
                                 step = 1),
                    
                    checkboxGroupInput(inputId = "vars_tree",
                                       label = "Select variables for the decision tree model",
                                       choices = c("age", "job", "marital", "education", "default",
                                                   "housing", "loan", "contact", "day_of_week", "month", "duration",
                                                   "campaign", "pdays", "previous", "poutcome", "emp.var.rate",
                                                   "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed"),
                                       selected = c("contact", "campaign", "pdays","emp.var.rate",
                                                    "cons.price.idx")),
                ),
                
                mainPanel(
                    
                    verbatimTextOutput("tree"),
                    plotOutput("tree_plot")
                )
                
            ),
            
            fluidRow(column(width = 6, plotOutput("tree_plot2")),
                     column(width = 6, plotOutput("tree_ROC"))),
            
            fluidRow(column(width = 6, offset = 6, verbatimTextOutput("tree_auc")))
            ),
   
   tabPanel("Conclusions",
            
    fluidPage(
        
        titlePanel("Conclusions"),
        mainPanel(
            
            fluidRow(column(width = 8, plotOutput("conclusions_plot"))),
            
            HTML("</br>"),
            
            flowLayout(HTML("Logistic Regression AUC: "),
                textOutput("conclusions_AUC1")),
            
            HTML("</br>"),
            
            flowLayout(HTML("Decision Tree AUC: "),
                textOutput("conclusions_AUC2")),

            HTML(
                "<p>
                </br>
                The analysis indicates that <b>the logistic regression model predicts values with similar
                accuracy as the decision tree</b>.</br>
                As it was mentioned at the beginning, <b>it is highly recommended not to use the 'duration' variable 
                as an independent variable</b>, because it is already known that
                this attribute highly affects the output target (e.g. if duration=0 then y='no').</br>
                Therefore, while not taking the 'duration' variable into consideration, <b>in the case of both models
                AUC equals around 0.8</b>. Worth mentioning is that when the 'duration' variable is taken into
                consideration then in both cases AUC equals circa 0.9.
                Rejecting the 'duration' variable is therefore necessary before creating a model based on this data.</br>
                The analysis showed that <b>the following variables are the most significant statistically</b>:</br>
                 - <b>contact communication type</b> - contacts performed by telephone (not cellular) seem to have
                 much lower success rate (variable: 'contact')</br>
                 - <b>number of contacts regarding current and previous campaign(s) and their outcome</b>
                 (variables: 'campaign', 'pdays', 'poutcome')</br>
                 - <b>social and economic context attributes</b> (variables: 'emp.var.rate',
                 'cons.price.idx', 'cons.conf.idx')
                </p>"
                )
                )
        )
    
)
)
)

data$y[data$y=="yes"] = 1
data$y[data$y=="no"] = 0
data$y <- as.numeric(data$y)

n <- nrow(data)
i <- floor(0.6*n)

# Server
server <- function(input, output) {
    
    
    # Data Visualization - reactive
    
    visualization_plot <- reactive({
        if(input$vars_ggplot!="balance" & input$vars_ggplot!="duration"){
            ggplot(data) +
                geom_bar(aes_string(x = input$vars_ggplot)) +
                theme(axis.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5))}
        else if (input$vars_ggplot == "balance"){
            ggplot(data) +
                geom_histogram(aes_string(x = "balance")) +
                theme(axis.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5))}
        else{ggplot(data) +
                geom_histogram(aes_string(x = "duration")) +
                theme(axis.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5))}
    })
    
    visualization_plot_y <- reactive({
        y_factor <- as.factor(data$y)
        if(input$vars_ggplot2!="balance" & input$vars_ggplot2!="duration"){
            ggplot(data) +
                geom_bar(aes_string(x = input$vars_ggplot2, fill = "y_factor")) +
                theme(axis.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5))}
        else if (input$vars_ggplot2 == "balance"){
            ggplot(data) +
                geom_histogram(aes_string(x = "balance", fill = "y_factor")) +
                theme(axis.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5))}
        else{
            ggplot(data) +
                geom_histogram(aes_string(x = "duration", fill = "y_factor")) +
                theme(axis.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5))}
    })
    
    plot_boxplot <- reactive({
        y_factor <- as.factor(data$y)
        ggplot(data) +
            geom_boxplot(aes_string(x = "y_factor", y = input$vars_boxplot), fill = "purple")
    })
    
    plot_point <- reactive({
        ggplot(data) +
            geom_point(aes_string(x = input$vars_point_x, y = input$vars_point_y))
    })
    
    plot_bar<- reactive({
        ggplot(data) +
            geom_bar(aes_string(x = input$vars_bar_x, fill = input$vars_bar_y), position = "stack")
    })
    
    # Logistic Regression - reactive
    
    glm_train <- reactive({
        set.seed(input$seed)
        s <- sample.int(n, i, replace = FALSE)
        data.train <- data[s,]
        glm(data.train, formula = paste("y ~ ", paste(input$vars_logistic,collapse=" + ")),
            family = 'binomial')
    })
    
    predict_test <- reactive({
        set.seed(input$seed)
        s <- sample.int(n, i, replace = FALSE)
        data.test <- data[-s,]
        predict(glm_train(), newdata = data.test, type = 'response')
    })
    
    confusion_matrix <- reactive({
        set.seed(input$seed)
        s <- sample.int(n, i, replace = FALSE)
        data.test <- data[-s,]
        predict.model1 = predict(glm_train(), newdata = data.test, type = 'response')
        data.test$predict = 0
        data.test[predict.model1>input$t,c("predict")] = 1
        confusionMatrix(factor(data.test$predict, levels = c(1,0)),
                        factor(data.test$y, levels = c(1,0)))
    })
    
    ROC_plot <- reactive({
        set.seed(input$seed)
        s <- sample.int(n, i, replace = FALSE)
        data.test <- data[-s,]
        predict.model1 = predict(glm_train(), newdata = data.test, type = 'response')
        roc(data.test$y, predict.model1, direction="<")
    })
    
    
    # Decision Tree - reactive
    
    tree_model <- reactive({
        set.seed(input$seed2)
        s <- sample.int(n, i, replace = FALSE)
        data.train <- data[s,]
        data.test <- data[-s,]
        rpart(data = data.train, formula = paste("y ~ ", paste(input$vars_tree,collapse=" + ")))
    })
    
    tree_predict <- reactive({
        set.seed(input$seed2)
        s <- sample.int(n, i, replace = FALSE)
        data.train <- data[s,]
        data.test <- data[-s,]
        predict.dt <- predict(tree_model(), newdata=data.test, type = "vector")
    })
    
    tree_ROC <- reactive({
        set.seed(input$seed2)
        s <- sample.int(n, i, replace = FALSE)
        data.train <- data[s,]
        data.test <- data[-s,]
        predict.dt <- predict(tree_model(), newdata=data.test, type = "vector") 
        roc.dt <- roc(as.numeric(data.test$y),as.numeric(predict.dt), direction="<")
    })
    
    tree_conf_matrix <- reactive({
        set.seed(input$seed2)
        s <- sample.int(n, i, replace = FALSE)
        data.train <- data[s,]
        data.test <- data[-s,]
        predict.dt <- predict(tree_model(), newdata=data.test, type = "vector")
        data.test$predict = 0
        data.test[predict.dt>input$t2,c("predict")] = 1
        confusionMatrix(factor(data.test$predict, levels = c(1,0)),
                        factor(data.test$y, levels = c(1,0)))
    })
    
    # Data Visualization - output
    
    output$visualization_plot <- renderPlot({
        visualization_plot()
    })
    
    output$visualization_plot_y <- renderPlot({
        visualization_plot_y()
    })
    
    output$plot_boxplot <- renderPlot({
        plot_boxplot()
    })
    
    output$plot_point <- renderPlot({
        plot_point()
    })
    
    output$plot_bar <- renderPlot({
        plot_bar()
    })
    
    # Logistic Regression - output
    
    output$glmPrint <- renderPrint({
        print(summary(glm_train()))
    })
    
    output$plotPredict <- renderPlot({
        plot(predict_test())
    })
    
    output$ConfusionMatrix <- renderPrint({
        print(confusion_matrix())
    })
    
    output$ROC <- renderPlot({
        plot(ROC_plot(), col="red", lwd=1, main="ROC curve")
    })
    
    output$ROCPrint <- renderPrint({
        print(ROC_plot()$auc)
    })
    
    # Decision Tree - output
    
    output$tree <- renderPrint({
        printcp(tree_model())
    })
    
    output$tree_plot <- renderPlot({
        plotcp(tree_model(), cex.lab=1.1, cex.axis=1.1)
    })
    
    output$tree_plot2 <- renderPlot({
        fancyRpartPlot(tree_model(), sub = "", cex = 1)
    })
    
    output$tree_predict <- renderPlot({
        plot(tree_predict())
    })
    
    output$tree_ROC <- renderPlot({
        plot(tree_ROC(),col="red", lwd=1, main="ROC curve")
    })
    
    output$tree_auc <- renderPrint({
        tree_ROC()$auc
    })
    
    output$tree_conf_matrix <- renderPrint({
        print(tree_conf_matrix())
    })
    
    # Conclusions - output
    
    output$conclusions_plot <- renderPlot({
        plot(ROC_plot(), col="red", lwd=1, main="ROC curve")
        lines(tree_ROC(), col="blue", lwd=1)
        legend("bottom", legend = c("Logistic Regression", "Decision Tree"), col = c("red", "blue"), lty=1, cex=1)
    })
    
    output$conclusions_AUC1 <- renderText({
        print(ROC_plot()$auc)
    })
    
    output$conclusions_AUC2 <- renderText({
        tree_ROC()$auc
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)