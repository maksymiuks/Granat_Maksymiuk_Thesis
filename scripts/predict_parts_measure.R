rd_question_data <- readRDS("../data/annotations/result_question_3.rds")
nd_question_data <- readRDS("../data/annotations/result_question_2.rds")
source("model.R")

bd_measure <- function(bd, expl, expl_fi = NULL, id, M) {
 expl <- expl[expl$id == id,]
 res <- apply(expl, MARGIN = 1, function(expl) {
     expl <- expl[-9]
     bd <- bd[bd$variable != "intercept" & bd$variable != "prediction",]
     
     discretize <- c(-Inf, -M/3, -M/9, M/9, M/3, Inf)
     bins <- cut(bd$contribution, breaks = discretize, labels = c(-2, -1, 0, 1, 2))
     bins <- as.numeric(as.character(bins))
     names(bins) <- bd$variable_name
     
     expl <- as.numeric(expl[bd$variable_name])
     norm_score <- norm(bins - expl, type = "2")
     
     if (is.null(expl_fi)) {
         return(norm_score)
     }
     
     expl_fi <- unlist(expl_fi[expl_fi$id == id,-9][1,])
     bd_fi <- seq_along(bd$variable_name)
     names(bd_fi) <- bd$variable_name
     fi_score <- norm(expl_fi/8 - bd_fi[names(expl_fi)]/8, type = "2")
     
     norm_score*fi_score
 })
 mean(res)
}

shap_measure <- function(bd, expl, expl_fi = NULL, id, M) {

    expl <- expl[expl$id == id,]
    res <- apply(expl, MARGIN = 1, function(expl) {
        expl <- expl[-9]
        
        bd <- bd[bd$B == 0,]
        discretize <- c(-Inf, -M/3, -M/9, M/9, M/3, Inf)
        bins <- cut(bd$contribution, breaks = discretize, labels = c(-2, -1, 0, 1, 2))
        bins <- as.numeric(as.character(bins))
        names(bins) <- bd$variable_name
        
        expl <- as.numeric(expl[bd$variable_name])
        norm_score <- norm(bins - expl, type = "2")
        
        if (is.null(expl_fi)) {
            return(norm_score)
        }
        
        expl_fi <- unlist(expl_fi[expl_fi$id == id,-9][1,])
        bd_fi <- seq_along(bd$variable_name[order(bd$contribution, decreasing = TRUE)])
        names(bd_fi) <- bd$variable_name[order(bd$contribution, decreasing = TRUE)]
        fi_score <- norm(expl_fi/8 - bd_fi[names(expl_fi)]/8, type = "2")
        
        norm_score*fi_score
        
    })
    mean(res)

}

lime_measure <- function(lime, expl, expl_fi = NULL, id, M) {
    expl <- expl[expl$id == id,]
    
    res <- apply(expl, MARGIN = 1, function(expl) {
        expl <- expl[-9]
        lime <- lime[lime$original_variable != "",]
        zero_variables <- setdiff(names(expl), lime$original_variable)
        discretize <- c(-Inf, -M/3, -M/9, M/9, M/3, Inf)
        contribtions <- c(
            structure(lime$estimated,
                      names = lime$original_variable),
            structure(rep(0, times = length(zero_variables)),
                      names = zero_variables)
        )
        bins <- cut(contribtions, breaks = discretize, labels = c(-2, -1, 0, 1, 2))
        bins <- as.numeric(as.character(bins))
        names(bins) <- names(contribtions)
        
        expl <- as.numeric(expl[names(bins)])
        norm(bins - expl, type = "2")
    })
    
    mean(res)

}

ids_compound <- intersect(unique(rd_question_data$id), unique(nd_question_data$id))
ids <- unique(rd_question_data$id)

ppsv_ibd <- sapply(ids_compound, function(x) {
    bd <- predict_parts(explainer, phones[x,], type = "break_down")
    bd_measure(bd, rd_question_data, nd_question_data, x, M = mean(phones$price))
})


pps_ibd <- sapply(ids, function(x) {
    bd <- predict_parts(explainer, phones[x,], type = "break_down")
    bd_measure(bd, rd_question_data, NULL, x, M = mean(phones$price))
})
names(pps_ibd) <- ids

ppsv_shap <- lapply(c(10, 20, 30, 40), function(z) {
    sapply(ids_compound, function(x) {
        ibds <- sapply(1:10, function(y) {
            print(paste(z, y))
            bd <- predict_parts(explainer, phones[x,], type = "shap", B = z)
            shap_measure(bd, rd_question_data, nd_question_data, x, M = mean(phones$price))
        })
        mean(ibds)
    })
})

pps_shap <- lapply(c(10, 20, 30, 40), function(z) {
    sapply(ids, function(x) {
        ibds <- sapply(1:10, function(y) {
            print(paste(z, y))
            bd <- predict_parts(explainer, phones[x,], type = "shap", B = z)
            shap_measure(bd, rd_question_data, NULL, x, M = mean(phones$price))
        })
        mean(ibds)
    })
})

pps_lime <- sapply(ids, function(x) {
    bd <- DALEXtra::predict_surrogate_local_model(explainer, phones[x,])
    lime_measure(bd, rd_question_data, NULL, x, M = mean(phones$price))
})


#BD

ppsv_imputed <- rep("-", times = length(ibd_pps))
names(ppsv_imputed) <- names(ibd_pps)
ppsv_imputed[names(ibd_ppsv)] <- round(ibd_ppsv, 4)
ibd_table <- data.frame(id = names(ibd_pps), PPS = ibd_pps, PPSv = ppsv_imputed, Price = as.integer(round(phones$price[as.numeric(names(ibd_pps))])))
ibd_table <- ibd_table[order(ibd_table$Price, decreasing = TRUE),]
ibd_table <- cbind(ibd_table[1:36,], rbind(ibd_table[37:71,], c(1L, 1L, 1L, 1L)))
ibd_table
xtable::xtable(ibd_table, digits = 4)

library(ggplot2)

plot_data_pps <- data.frame(Measure = ibd_pps, Price = phones$price[as.numeric(names(ibd_pps))], `Measure type` = "PPS")
plot_data_ppsv <- data.frame(Measure = ibd_ppsv, Price = phones$price[as.numeric(names(ibd_ppsv))], `Measure type` = "PPSv")
plot_data <- rbind(plot_data_pps, plot_data_ppsv)

ibd_plot <- ggplot(data = plot_data, aes(x = Price, y = Measure, color = Measure.type, fill = Measure.type)) +
    geom_point() +
    geom_smooth(alpha = 0.25) +
    labs(color = "Measure type", fill = "Measure type", x = "Phone price", y = "Measure value", title = "Break Down") +
    scale_x_continuous(expand = c(0.01, 0)) +
    theme_bw() +
    theme(legend.position = "bottom") +
    theme_drwhy()

ggsave("../plots/ibd_plot.png", plot = ibd_plot, height = 5, width = 7)

#shap

ppsv_imputed <- rep("-", times = length(shap_pps))
names(ppsv_imputed) <- names(shap_pps)
ppsv_imputed[names(shap_ppsv)] <- round(shap_ppsv, 4)
ibd_table <- data.frame(id = names(shap_pps), PPS = shap_pps, PPSv = ppsv_imputed, Price = as.integer(round(phones$price[as.numeric(names(shap_pps))])))
ibd_table <- ibd_table[order(ibd_table$Price, decreasing = TRUE),]
ibd_table <- cbind(ibd_table[1:36,], rbind(ibd_table[37:71,], c(1L, 1L, 1L, 1L)))
ibd_table
xtable::xtable(ibd_table, digits = 4)

library(ggplot2)
shap_pps <- shap_pps[[4]]
shap_ppsv <- shap_ppsv[[4]]
plot_data_pps <- data.frame(Measure = shap_pps, Price = phones$price[as.numeric(names(shap_pps))], `Measure type` = "PPS")
plot_data_ppsv <- data.frame(Measure = shap_ppsv, Price = phones$price[as.numeric(names(shap_ppsv))], `Measure type` = "PPSv")
plot_data <- rbind(plot_data_pps, plot_data_ppsv)

shap_plot <- ggplot(data = plot_data, aes(x = Price, y = Measure, color = Measure.type, fill = Measure.type)) +
    geom_point() +
    geom_smooth(alpha = 0.25) +
    labs(color = "Measure type", fill = "Measure type", x = "Phone price", y = "Measure value", title = "SHAP") +
    scale_x_continuous(expand = c(0.01, 0)) +
    theme_bw() +
    theme(legend.position = "bottom") +
    theme_drwhy()

ggsave("../plots/shap_plot.png", plot = shap_plot, height = 5, width = 7)

#lime


ibd_table <- data.frame(id = names(lime_pps), PPS = lime_pps, Price = as.integer(round(phones$price[as.numeric(names(lime_pps))])))
ibd_table <- ibd_table[order(ibd_table$Price, decreasing = TRUE),]
ibd_table <- cbind(ibd_table[1:36,], rbind(ibd_table[37:71,], c(1L, 1L, 1L, 1L)))
ibd_table
xtable::xtable(ibd_table, digits = 4)



# Arrange


gridExtra::grid.arrange(ibd_plot, shap_plot, lime_plot, nrow = 2)

ggsave(plot = gridExtra::grid.arrange(ibd_plot, shap_plot, lime_plot, nrow = 2), filename = "pp.png", height = 12, width = 12)

#Comparison


library(ggplot2)

plot_data <- data.frame(Measure = lime_pps, Price = phones$price[as.numeric(names(lime_pps))], `Measure type` = "PPS")

lime_plot <- ggplot(data = plot_data, aes(x = Price, y = Measure, color = Measure.type, fill = Measure.type)) +
    geom_point() +
    geom_smooth(alpha = 0.25) +
    labs(color = "Measure type", fill = "Measure type", x = "Phone price", y = "Measure value", title = "LIME") +
    scale_x_continuous(expand = c(0.01, 0)) +
    theme_bw() +
    theme(legend.position = "bottom") +
    theme_drwhy()

ggsave("../plots/lime_plot.png", plot = lime_plot, height = 5, width = 7)


plot_data <- rbind(
    data.frame(Measure = ibd_pps, Price = phones$price[as.numeric(names(ibd_pps))], `Measure type` = "Break Down"),
    data.frame(Measure = shap_pps, Price = phones$price[as.numeric(names(shap_pps))], `Measure type` = "SHAP"),
    data.frame(Measure = lime_pps, Price = phones$price[as.numeric(names(lime_pps))], `Measure type` = "LIME")
)

comaprison_plot <- ggplot(data = plot_data, aes(x = Price, y = Measure, color = Measure.type, fill = Measure.type)) +
    geom_point() +
    geom_smooth(alpha = 0.25) +
    labs(color = "Measure type", fill = "Measure type", x = "Phone price", y = "Measure value") +
    scale_x_continuous(expand = c(0.01, 0)) +
    theme_bw() +
    theme(legend.position = "bottom") +
    theme_drwhy()

ggsave("../plots/comparison.png", plot = comaprison_plot, height = 5, width = 7)
