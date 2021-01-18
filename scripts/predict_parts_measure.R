rd_question_data <- readRDS("result_question_3.rds")
nd_question_data <- readRDS("result_question_2.rds")

bd_measure <- function(bd, expl, expl_fi = NULL, id) {

 expl <- expl[expl$id == id,]
 expl <- round(sapply(expl, mean))[-8]


 discretize <- quantile(-max(abs(bd$contribution)):max(abs(bd$contribution)),
                        c(0.2, 0.4, 0.6, 0.8, 1))[-5]
 discretize <- c(-max(abs(bd$contribution))-1, discretize, max(abs(bd$contribution))+1)
 bins <- cut(bd$contribution, breaks = discretize, labels = c(-2, -1, 0, 1, 2))
 bins <- as.numeric(as.character(bins))
 names(bins) <- bd$variable_name

 expl <- as.numeric(expl[bd$variable_name])
 norm_score <- norm(bins - expl, type = "2")

 if (is.null(expl_fi)) {
   return(norm_score)
 }

 expl_fi <- unlist(expl_fi[expl_fi$id == id,-8][1,])
 bd_fi <- seq_along(bd$variable_name)
 names(bd_fi) <- bd$variable_name
 fi_score <- norm(expl_fi/7 - bd_fi[names(expl_fi)]/7, type = "2")

 norm_score*fi_score

}

shap_measure <- function(bd, expl, expl_fi = NULL, id) {


    expl <- expl[expl$id == id,]
    expl <- round(sapply(expl, mean))[-8]

    bd <- bd[bd$B == 0,]
    discretize <- quantile(-max(abs(bd$contribution)):max(abs(bd$contribution)),
                           c(0.2, 0.4, 0.6, 0.8, 1))[-5]
    discretize <- c(-max(abs(bd$contribution))-1, discretize, max(abs(bd$contribution))+1)
    bins <- cut(bd$contribution, breaks = discretize, labels = c(-2, -1, 0, 1, 2))
    bins <- as.numeric(as.character(bins))
    names(bins) <- bd$variable_name

    expl <- as.numeric(expl[bd$variable_name])
    norm_score <- norm(bins - expl, type = "2")

    if (is.null(expl_fi)) {
        return(norm_score)
    }

    expl_fi <- unlist(expl_fi[expl_fi$id == id,-8][1,])
    bd_fi <- seq_along(bd$variable_name[order(bd$contribution, decreasing = TRUE)])
    names(bd_fi) <- bd$variable_name[order(bd$contribution, decreasing = TRUE)]
    fi_score <- norm(expl_fi/7 - bd_fi[names(expl_fi)]/7, type = "2")

    norm_score*fi_score

}

ids_compound <- intersect(unique(rd_question_data$id), unique(nd_question_data$id))
ids <- unique(rd_question_data$id)

ppsv_ibd <- sapply(ids_compound, function(x) {
    bd <- predict_parts(explainer, data[x,], type = "break_down")[-c(1,9),]
    bd_measure(bd, rd_question_data, nd_question_data, x)
})


pps_ibd <- sapply(ids, function(x) {
    bd <- predict_parts(explainer, data[x,], type = "break_down")[-c(1,9),]
    bd_measure(bd, rd_question_data, NULL, x)
})
names(pps) <- ids

ppsv_shap <- lapply(c(10, 20, 30), function(z) {
    sapply(ids_compound, function(x) {
        ibds <- sapply(1:10, function(y) {
            print(paste(z, y))
            bd <- predict_parts(explainer, data[x,], type = "shap", B = z)
            shap_measure(bd, rd_question_data, nd_question_data, x)
        })
        mean(ibds)
    })
})

pps_shap <- lapply(c(10, 20, 30), function(z) {
    sapply(ids, function(x) {
        ibds <- sapply(1:10, function(y) {
            print(paste(z, y))
            bd <- predict_parts(explainer, data[x,], type = "shap", B = z)
            shap_measure(bd, rd_question_data, NULL, x)
        })
        mean(ibds)
    })
})

# data.frame(ID = ids, PPS = pps, PPSv = ppsv_imputed, Price = data$price[ids]) -> ibd_table
# ibd_table <- round(ibd_table, 4)
# ibd_table$PPSv[is.na(ibd_table$PPSv)] <- "-"
# ibd_table <- ibd_table[order(ibd_table$Price, decreasing = TRUE),]
# xtable::xtable(ibd_table)