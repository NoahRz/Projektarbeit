library(referenceIntervals)


#' Apply the z-normalization and the z-log normalization
#'
#' Apply the z-normalization and the z-log normalization to all the attributes of the attribute_list
#' For this we calculate the lower_interval and the upper_interval from the reference_individuals (reference_individuals contains only data from the train data)
#' And we use them for the normalization for the train data but also for the test data
#' Note : We are not supposed to know the test data, but we also have to normalize them this is why we normalize them with the lower_interval and upper_interval computed with the train data
#' 
#' 
#' @param train_data A data frame
#' @param test_data A data frame
#' @param attribute_list A list
#' @param reference_individuals A data frame
#' 
#' @return a list of length 2 containing the train data normalized and the test data normalized
#' 
#' @examples
#' z_transform_and_zlog_transform(train_data, test_data, attribute_list, reference_individuals)
z_transform_and_zlog_transform <- function(train_data, test_data, attribute_list, reference_individuals) {
  for (attribute in attribute_list) {
    
    # For each attribute, we calculate the reference intervals
    ref_intervals = ref_intervals(reference_individuals, attribute)
    lower_interval = lower_interval(ref_intervals)
    upper_interval = upper_interval(ref_intervals)

    z_transformed_col_name <- paste("z_transformed", attribute, sep="_")
    z_log_transformed_col_name <- paste("z_log_transformed", attribute, sep="_")
    
    # Apply z-normalization and z-log normalization on train data
    train_data[z_transformed_col_name] <- z_normalization(train_data, attribute, lower_interval, upper_interval)
    train_data[z_log_transformed_col_name] <- z_log_normalization(train_data, attribute, lower_interval, upper_interval)

    # Apply the same transformations to the test data with the same parameters as the train data
    test_data[z_transformed_col_name] <- z_normalization(test_data, attribute, lower_interval, upper_interval)
    test_data[z_log_transformed_col_name] <- z_log_normalization(test_data, attribute, lower_interval, upper_interval)
  }
  return(list(train_data = train_data, test_data = test_data))
}


#' Calculate the reference intervals
#'
#' Calculate the reference intervals from the reference_individuals
#'
#' @param reference_individuals A data frame
#' @param attribute A character
#' 
#' @return a list of all the information on the reference intervals
#' 
#' @examples
#' ref_intervals(reference_individuals, attribute)
ref_intervals <-function(reference_individuals, attribute){
  ref_ind <- reference_individuals[[attribute]]
  return(refLimit(ref_ind, out.rm = FALSE, RI = 'r', CI = 'p')) # For the estimation of the reference interval, we use the robust method and we keep the outliers
}


#' return the lower_interval from a reference interval
#'
#' @param ref_intervals A list of all the information on the reference intervals
#' 
#' @return a list of all the information on the reference intervals
#' 
#' @examples
#' lower_interval(ref_intervals)
lower_interval<-function(ref_intervals){
  return(ref_intervals[["Ref_Int"]][["lowerRefLimit"]])
}


#' return the upper_interval from a reference interval
#'
#' @param ref_intervals A list of all the information on the reference intervals
#' 
#' @return a list of all the information on the reference intervals
#' 
#' @examples
#' upper_interval(ref_intervals)
upper_interval<-function(ref_intervals){
  return(ref_intervals[["Ref_Int"]][["upperRefLimit"]])
}


#' calculate the z-normalization
#'
#' Calculate the z-normalization to an attribute of a dataframe
#' 
#' @param data A data frame
#' @param attribute A character
#' @param lower_interval A numeric
#' @param upper_interval A numeric
#' 
#' @return a list of the z-normalized column
#' 
#' @examples
#' z_normalization(data, attribute, lower_interval, upper_interval)
z_normalization<-function(data, attribute, lower_interval, upper_interval){
  return((data[attribute] - (lower_interval + upper_interval)/2)*3.92/(upper_interval-lower_interval))
}

#' calculate the z-log normalization
#'
#' Calculate the z-log normalization to an attribute of a dataframe
#' 
#' @param data A data frame
#' @param attribute A character
#' @param lower_interval A numeric
#' @param upper_interval A numeric
#' 
#' @return a list of the z-log normalized column
#' 
#' @examples
#' z_normalization(data, attribute, lower_interval, upper_interval)
z_log_normalization<-function(data, attribute, lower_interval, upper_interval){
  return(log(data[attribute]) - (log(lower_interval) + log(upper_interval))/2)*3.92/(log(upper_interval) - log(lower_interval))
}


#' prepare data for k-fold cross validation
#'
#' Add a new column k to the dataframe indicating for each instances to which k-fold it belongs
#' The k is associated randomly.
#' 
#' @param data A data frame
#' @param K A integer. By default K=10
#' 
#' @return a dataframe with the k column
#' 
#' @examples
#' prepare_data_for_cross_validation(data, 3)
prepare_data_for_cross_validation <- function(data, K=10){
  nrow <- nrow(data)
  bloc_size <- ceiling(nrow/K)
  rand <- runif(nrow)
  rank <- rank(rand)
  bloc <- (rank-1)%/%bloc_size +1
  bloc <- as.factor(bloc)
  data$k <- bloc
  return(data)
}