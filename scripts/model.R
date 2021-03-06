phones <- read.csv("../data/phones/phones.csv", stringsAsFactors = TRUE)
phones$resolution_Mpx <- phones$height_px*phones$width_px/1000000
phones <- phones[,-c(9, 10)]



library(mlr)
library(DALEXtra)

# gbm
task <- makeRegrTask("phones", phones[,-c(1)], "price")
lrn <- makeLearner("regr.gbm", par.vals = list(n.trees = 2672, interaction.depth = 2, n.minobsinnode=5, shrinkage=0.131))
model <- train(lrn, task)

rdesc <- makeResampleDesc("CV", iters = 5)
explainer <- explain_mlr(model, phones[,-c(1, 9)], phones$price)

mp <- model_performance(explainer)
mp$measures$rmse


# lm
phones[is.na(phones)] <- 0
phones <- createDummyFeatures(phones[,-1])
task <- makeRegrTask("phones", phones, "price")
lrn <- makeLearner("regr.lm")
rdesc <- makeResampleDesc("CV", iters = 5)
res <- resample(lrn, task, resampling = rdesc, measures = rmse)
model <- train(lrn, task)
sqrt(mean((predict(model, newdata = phones)$data$response - phones$price)^2))

