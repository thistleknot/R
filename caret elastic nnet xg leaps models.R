library(matlib)
library(caret)
library(glmnet)
library(ModelMetrics)

options(digits = 4)
normalizeResponse <- "Y"

data <- read.csv(file="states.csv",header = TRUE)
#data2 <- data[,-1]

seed <- sample(1:100,1)
#seed=40

nr <- nrow(data)
nc <- ncol(data)

trainSetIndex <- (sample(1:(nr),(nr)*.8))
testSetIndex <- c(1:nr)[(1:nr) %in% c(trainSetIndex)==FALSE]

print(seed)
set.seed(seed)

#names(getModelInfo())

lapply(1:(ncol(data)-1),function(y)
{#y=1
        cat("\n")
        data2 <- data[,-1]
        print(colnames(data2[,y,drop=FALSE]))
        data2 <- cbind(data2[,y,drop=FALSE],data2[,-y,drop=FALSE])
        subsetNames <- colnames(data2[,-1])
        
        #derive interactions here
        combinations <- unique(combn(subsetNames, 2,simplify = FALSE))
        
        interactions <- do.call(cbind,(lapply(combinations, function (b)
        {#b=combinations[[1]]
                interactions <- data.frame(as.double(unlist(data2[,unlist(b)[1]]))*as.double(unlist(data2[,unlist(b)[2]])))
                colnames(interactions) <- paste0(unlist(b)[1],"*",unlist(b)[2])
                return(interactions)
                
        })))
        
        squared <- do.call(cbind,lapply(subsetNames, function(s)
        {#s=subsetNames[1]
                sqr <- data2[,s,drop=FALSE]^2
                colnames(sqr) <- paste0(s,"^2")
                return(sqr)
        }))
        
        data3 <- cbind(data2,interactions,squared)
        
        set.train <- data3[trainSetIndex, ]
        set.test <- data3[testSetIndex,]
        
        if(normalizeResponse=="Y")
        {
                trainParam <- caret::preProcess(as.matrix(set.train))
                
                set.train <- predict(trainParam, set.train)
                
                set.test <- predict(trainParam, set.test)
        }
        
        numFolds = 3
        
        f <- as.formula(paste0(colnames(set.train)[1], "~."))
        
        train.control <- trainControl(method = "cv", number = numFolds, allowParallel = TRUE, search = "grid")
        
        model_sets <- list("xgbLinear","enet","nnet","knn","rf","leapBackward")
        names(model_sets) <- model_sets
        
        models <- lapply(1:length(model_sets), function(h)
        {#h=4
                invisible(capture.output(suppressMessages(suppressWarnings(model <- train(f, data=set.train, method=model_sets[[h]], metric="RMSE", trControl=train.control, tuneLength = numFolds)))))
                return(model)
        })
        
        names(models) <- model_sets
        
        predictions <- lapply(1:length(model_sets), function(p)
        {#p=1
                
                #predict(models[[p]],set.test[,-1,drop=FALSE])
                switch(model_sets[[p]],
                       
                       xgbLinear = {
                               model_xg <- glmnet(data.matrix(set.train[,-1,drop=FALSE]), as.matrix(set.train[,1,drop=FALSE]), alpha = models[[p]]$finalModel$tuneValue$alpha, lambda=models[[p]]$finalModel$lambda, standardize = FALSE, family = "gaussian", type.measure="deviance")
                               predict_xg <- predict.glmnet(model_xg,data.matrix(set.test[,-1,drop=FALSE]), alpha = models[[p]]$finalModel$tuneValue$alpha, lambda=models[[p]]$finalModel$lambda)
                               return(predict_xg[,ncol(predict_xg)])
                               #return(predict(models[[p]],set.test[,-1,drop=FALSE]))
                       },
                       enet = {
                               enet = models[["enet"]]
                               model_enet <- glmnet(data.matrix(set.train[,-1,drop=FALSE]), as.matrix(set.train[,1,drop=FALSE]), alpha = enet$finalModel$tuneValue$fraction, lambda=enet$finalModel$lambda, standardize = FALSE, family = "gaussian", type.measure="deviance")
                               predict_enet <- predict(model_enet,as.matrix(set.test[,-1]))
                               return(predict_enet)
                               #return(predict(models[[p]],set.test[,-1,drop=FALSE]))
                       },
                       nnet = {
                               return(predict(models[[p]],set.test[,-1,drop=FALSE]))
                               
                       },
                       knn = {
                               return(predict(models[[p]],set.test[,-1,drop=FALSE]))
                               
                       },
                       rf = {
                               return(predict(models[[p]],set.test[,-1,drop=FALSE]))
                       },
                       leapBackward = {
                               leaps_model = models[["leapBackward"]]
                               best_leaps_model <- lm(cbind(set.train[,1,drop=FALSE],set.train[,gsub("\\`","",names(coef(leaps_model$finalModel, leaps_model$bestTune[,1])))[-1],drop=FALSE]))
                               predict_best_leaps <- predict(best_leaps_model,set.test[,-1,drop=FALSE])
                               return(predict_best_leaps)
                               #return(predict(models[[p]],set.test[,-1,drop=FALSE]))
                       })
                
        })

        names(predictions) <- model_sets
        predictions
        
        rmses <- lapply(1:length(model_sets), function(x)
        {
                return <- rmse((set.test[,1]* trainParam$std[1]) + trainParam$mean[1],(predictions[[x]]* trainParam$std[1]) + trainParam$mean[1])
        })
        names(rmses) <- model_sets
        print(unlist(rmses))
        
        best <- which.min(unlist(rmses))
        print(cbind(unlist(predictions[best]),set.test[,1]))
        
        plot((set.test[,1]* trainParam$std[1]) + trainParam$mean[1],(predictions[[best]]* trainParam$std[1]) + trainParam$mean[1])
        
        print(paste("correlation:", round(cor(unlist(predictions[which.min(unlist(rmses))]),set.test[,1]),4)))
        
        switch(names(best),
               xgbLinear = {
                       model_xg <- glmnet(data.matrix(set.train[,-1,drop=FALSE]), as.matrix(set.train[,1,drop=FALSE]), alpha = models[["xgbLinear"]]$finalModel$tuneValue$alpha, lambda=models[["xgbLinear"]]$finalModel$lambda, standardize = FALSE, family = "gaussian", type.measure="deviance")
                       models[["xgbLinear"]]$finalModel$tuneValue
                       plot(coef(model_xg)[,ncol(coef(model_xg))])
                       plot(models[[best]])
                       (coef(model_xg)[,ncol(coef(model_xg))])
                       df <- data.frame(as.matrix((coef(model_xg)[,ncol(coef(model_xg))])))
                       print(df[df!=0,,drop=FALSE])
                       
               },
               enet = {
                       model_enet <- glmnet(data.matrix(set.train[,-1,drop=FALSE]), as.matrix(set.train[,1,drop=FALSE]), alpha = models[["enet"]]$finalModel$tuneValue$fraction, lambda=models[["enet"]]$finalModel$lambda, standardize = FALSE, family = "gaussian", type.measure="deviance")
                       print(models[["enet"]]$finalModel$tuneValue)
                       plot(coef(model_enet))
                       plot(models[[best]])
                       df <- data.frame(as.matrix(model_enet$beta))
                       print(df[df!=0,,drop=FALSE])
               },
               nnet = {
                       plot(models[["nnet"]])
                       print(models[["nnet"]]$bestTune)
               },
               knn = {
                       plot(models[["knn"]])
                       print(models[["knn"]]$bestTune)
                       
               },
               rf = {
                       plot(models[[best]])
                       print(models[["rf"]]$bestTune)
               },
               leapBackward = {
                       best_caret = models[["leapBackward"]]
                       best_leaps_model <- lm(cbind(set.train[,1,drop=FALSE],set.train[,gsub("\\`","",names(coef(models[["leapBackward"]]$finalModel, models[["leapBackward"]]$bestTune[,1])))[-1],drop=FALSE]))
                       plot(models[[best]])
                       print(summary(best_leaps_model))
               })
        cat("\n")
})
