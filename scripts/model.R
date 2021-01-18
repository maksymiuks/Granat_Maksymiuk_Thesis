phones <- read.csv("phones.csv", stringsAsFactors = TRUE)
phones$resolution_Mpx <- phones$height_px*phones$width_px/1000000
phones <- phones[,-c(9, 10)]

library(mlr)

task <- makeRegrTask("phones", phones[,-c(1)], "price")
lrn <- makeLearner("regr.gbm", par.vals = list(n.trees = 2672, interaction.depth = 2, n.minobsinnode=5, shrinkage=0.131))
model <- train(lrn, task)

library(DALEXtra)

explainer <- explain_mlr(model, phones[,-c(1, 9)], phones$price)
