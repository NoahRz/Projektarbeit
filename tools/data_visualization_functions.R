library(ggplot2)
library("dplyr")

#' Display a box plot
#'
#' Display a box plot for each numerical attributes of the dataset
#'
#' @param data A data frame.
#' 
#' @return a box plot
#' 
#' @examples
#' draw_boxplot(data)
draw_boxplot <- function(data){ 
  data <- select_if(data, is.numeric)
  
  data %>%
    pivot_longer(1:ncol(data), names_to="attributes") %>%  
    ggplot(aes(attributes, value, fill=attributes)) + 
    geom_boxplot() 
}

#' Display a scatter matrix
#'
#' Display a scatter matrix with all the combination of pairs of attributes in the dataset
#'
#' @param data A data frame.
#' 
#' @return a scatter matrix
#' 
#' @examples
#' draw_scatter_matrix(data)
draw_scatter_matrix <- function(data){
  color_class <- NA
  color_class[data$Class == "Kecimen"] <- rgb(red = 0, green = 0, blue = 1 ) #blue
  color_class[data$Class == "Besni"] <- rgb(red = 1, green = 0, blue = 0 ) #red
  pairs(data[,1:ncol(data)-1],
        pch = 5,
        cex = 0.5,
        col = color_class)
}

#' Display a bar chart
#'
#' Display a box plot for each numerical attributes of the dataset
#'
#' @param data A data frame.
#' 
#' @return a bar chart
#' 
#' @examples
#' draw_barchart(data)
draw_barchart <-function(data){
  data <- select_if(data, is.numeric)
  barchart_data <- data %>%                          # Apply pivot_longer function
                          pivot_longer(colnames(data)) %>% 
                          as.data.frame() 
  
  ggplot(barchart_data, aes(x = value)) +    # Draw each column as histogram
  geom_histogram() +
  facet_wrap(~ name, scales = "free")
    
}