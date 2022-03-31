# methods For neural network

library(dplyr)
library(neuralnet)



#' predict with neural network
#'
#' compute the prediction of the class of the data and return the confusion_matrix and the accuracy
#'
#' @param model A neural network
#' @param data A dataframe
#' @param labels A list of character 
#' 
#' @return A list of length 2 with the confusion matrix and the accuracy of the neural network on data
#' 
#' @examples
#' predict(model, data, c("Kecimen", "Besni"))
predict <- function(model, data, labels){ 
  prediction <- data.frame(neuralnet::compute(model,data.frame(data[,-ncol(data)]))$net.result) 

  # Create Confusion matrix
  prediction_label <- data.frame(max.col(prediction)) %>%
    mutate(prediction=labels[max.col.prediction.]) %>%
    select(2) %>%
    unlist()
  
  conf_matrix <- table(data$Class, prediction_label)
  acccuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
  return(list(conf_matrix = conf_matrix, acccuracy = acccuracy))
}