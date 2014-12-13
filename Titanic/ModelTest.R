source("GetData.R");
data = GetData("train.csv", "test.csv");

set.seed(23);
train.batch.rows = createDataPartition(data$train$Survived, p = 0.8, list = FALSE);
train.batch = data$train[train.batch.rows,];  
test.batch = data$train[-train.batch.rows,];

Titanic.logit.1 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare, 
                       data = train.batch, family=binomial("logit"));

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE);
glm.tune.1 <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl);

summary(glm.tune.1)

ada.grid = expand.grid(.iter = c(50, 100), .maxdepth = c(4, 8), .nu = c(0.1, 1))
set.seed(35)
ada.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, 
                  data = train.batch,
                  method = "ada",
                  metric = "ROC",
                  tuneGrid = ada.grid,
                  trControl = cv.ctrl)

rf.grid <- data.frame(.mtry = c(2, 3))
set.seed(35)
rf.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, 
                 data = train.batch,
                 method = "rf",
                 metric = "ROC",
                 tuneGrid = rf.grid,
                 trControl = cv.ctrl)

glm.tune.1.pred = predict(glm.tune.1, test.batch)
glm.tune.1.prob = predict(glm.tune.1, test.batch, type = "prob")
confusionMatrix(glm.tune.1.pred, test.batch$Fate)
glm.tune.1.ROC <- roc(response = test.batch$Fate,
               predictor = glm.tune.1.prob$Survived,
               levels = levels(test.batch$Fate))
plot(glm.tune.1.ROC)


data$test$Survived = revalue(predict(glm.tune.1, data$test), 
                                    c("Survived" = 1, "Perished" = 0))
write.csv(data$test[,c("PassengerId", "Survived")], 
          file="glm_tune_1_pred.csv",
          row.names=FALSE, quote=FALSE)