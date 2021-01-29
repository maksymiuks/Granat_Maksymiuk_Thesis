st_question_data <- readRDS("../data/annotations/result_question_1.rds")
source("model.R")

fi_measure <- function(fi, expl) {
  fi <- fi[fi$permutation == 0,]
  fi <- fi[fi$variable != "_full_model_",]
  fi <- fi[fi$variable != "_baseline_",]
  fi$dropout_loss <- fi$dropout_loss/max(fi$dropout_loss)
  fi <- fi[order(fi$variable),]

  expl <- expl[order(expl$feature),]

  dist(rbind(fi$dropout_loss, expl$freq_scaled))/sqrt(length(fi$dropout_loss))

}


res <- lapply(c(1, 5, 10, 20, 30, 50, 100), function(x){
  print(x)
  res <- sapply(1:20, function(y) {
    fi <- feature_importance(explainer, N = NULL, B = x)
    fi_measure(fi, st_question_data)
  })
  list(mean = mean(res), sd(res))
})


# LATEX table

rbind(names(fi_score), lapply(fi_score, function(x) {
  round(x$mean, 6)
}), lapply(fi_score, function(x) {
  round(x$sd, 6)
}))[-1,] -> fi_table

xtable::xtable(fi_table, digits = 6)