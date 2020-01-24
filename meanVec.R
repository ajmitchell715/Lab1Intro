#' Mean Vector Function
#'
#' Creates a vector of mean values for each variable of a data frame
#'
#' @param x the data frame whose mean values of each variable will be calculated
#'
#' @return a vector of mean values
#' @export
#'
#' @examples
#' meanVec(x)
meanVec=function(x){
  # Define a vector of the same length as the number of columns in the matrix
  vec=c(length=ncol(x))
  for(i in 1:ncol(x)){
    # Initialize a value for the mean to 0
    val=0
    for(j in 1:nrow(x)){
      # Sum the values of the given variable
      val=sum(x[,i])
    }
    # Divide the sum by the number of trials
    val=val/nrow(x)
    # Place the mean in the appropriate position in the vector
    vec[i]=val
  }
  # Return the mean vector
  vec
}
