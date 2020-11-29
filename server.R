library(shiny)
library(caret)
library(ggplot2)

shinyServer(function(input, output) {
    titanic <- read.csv("https://query.data.world/s/7dva5ttbu2ck5syuh3mllhsl5ud2w2", header=TRUE, stringsAsFactors=TRUE, na.strings = c(""));
    
    titanic <- titanic[,c(-3, -8, -10, -12, -13, -14)]
    titanic <- titanic[!is.na(titanic$pclass),]
    titanic_complete <- titanic[complete.cases(titanic),]
    
    mtry <- sqrt(30)
    tunegrid <- expand.grid(.mtry=mtry)
    controlRF <- trainControl(method="repeatedcv", number=3, verboseIter=FALSE)
    modRF <- suppressWarnings(train(survived ~ factor(pclass) + factor(sex) + age + parch, data = titanic_complete, method="rf", trControl=controlRF, tuneGrid=tunegrid))
    
    newdata <- data.frame(sex = rep(c("female", "male"), each = 1680), pclass = rep(c(1, 2, 3), each = 560, len = 3360), age = rep(1:80, each = 7, len = 3360), parch = rep(0:6, len = 3360))
    newdata$pred <- predict(modRF, newdata = newdata)
    newdata$pred <- round(newdata$pred, 2)
    
    pred <- reactive({
        age <- input$age
        parch <- input$parch
        class <- if (input$class=="1st"){
            1
        }
        else if (input$class=="2nd"){
            2
        }
        else if (input$class=="3rd"){
            3
        }
        gender <- tolower(input$gender)
        paste0(newdata[((newdata$pclass==class)&(newdata$parch==parch)&(newdata$age==age)&(newdata$sex==gender)),5]*100, "%")
    })
    
    output$plot <- renderPlot({
        age_input <- input$age
        parch_input <- input$parch
        class <- if (input$class=="1st"){
            1
        }
        else if (input$class=="2nd"){
            2
        }
        else if (input$class=="3rd"){
            3
        }
        gender <- tolower(input$gender)

        yourpred <- newdata[((newdata$pclass==class)&(newdata$parch==parch_input)&(newdata$age==age_input)&(newdata$sex==gender)),5]
        if (gender=="female"){
            gender1 <- "male"
        } else if (gender=="male"){
            gender1 <- "female"
        }
        predch_sex <- newdata[((newdata$pclass==class)&(newdata$parch==parch_input)&(newdata$age==age_input)&(newdata$sex==gender1)),5]
        if (class==1){
            class1 <- 2
            class2 <- 3
        } else if (class==2){
            class1 <- 1
            class2 <- 3
        } else if (class==3){
            class1 <- 2
            class2 <- 1
        }
        predch_class1 <- newdata[((newdata$pclass==class1)&(newdata$parch==parch_input)&(newdata$age==age_input)&(newdata$sex==gender)),5]
        predch_class2 <- newdata[((newdata$pclass==class2)&(newdata$parch==parch_input)&(newdata$age==age_input)&(newdata$sex==gender)),5]
        if (parch_input<=3){
            predch_parch <- newdata[((newdata$pclass==class)&(newdata$parch==6)&(newdata$age==age_input)&(newdata$sex==gender)),5]
            parch1 <- 6
        } else if (parch_input>3){
            predch_parch <- newdata[((newdata$pclass==class)&(newdata$parch==0)&(newdata$age==age_input)&(newdata$sex==gender)),5]
            parch1 <- "no"
        }
        
        graph_data <- data.frame(names = c(paste0("Changing to Class ", class1), paste0("Changing to Class ", class2), paste0("Having ", parch1, " parents and children"), paste0("Being ", gender1), "Your probability of survival"), probs = c(predch_class1, predch_class2, predch_parch, predch_sex, yourpred))
        graph_data$maxmin <- rep("", 5)
        graph_data[graph_data$probs==max(graph_data$probs), 3] <- "Max"
        graph_data[graph_data$probs==min(graph_data$probs), 3] <- "Min"
        graph_data[graph_data$maxmin=="", 3] <- "Other"
        
        ggplot(aes(x = names, y = probs, fill = factor(maxmin)), data = graph_data) + 
            geom_col() + 
            coord_flip() + 
            labs(y = "Probability of Survival", fill = "Max/Min Probability") + 
            scale_fill_manual(breaks = c("Max", "Min", "Other"), 
                              values=c("green", "red", "gray")) + 
            geom_label(aes(label = sprintf("%1.0f%%", probs*100), hjust = "left")) + 
            theme(text = element_text(size = 20), axis.title.y=element_blank())  
    })
    
    output$pred <- renderText({
        pred()
    })
})