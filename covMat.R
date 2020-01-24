#' Covariance Matrix Function
#'
#' Creates the covariance matrix of variables from a data frame
#'
#' @param x The data frame whose covariances will be calculated
#'
#' @return a matrix of covariance values
#' @export
#'
#' @examples
#' covMat(x)
covMat=function(x){
  # Create the matrix that will contain covariances
  mat=matrix(nrow=ncol(x),ncol=ncol(x))
  for (i in 1:ncol(x)){
    for (j in 1:ncol(x)){
      # Initialize covariance to 0
      val=0
      # Place variances where applicable
      if (i==j){
        for (k in 1:nrow(x)){
          val=val+(x[k,i]-mean(x[,i]))^2
        }
        val=val/nrow(x)
        mat[i,j]=val
      }
      # Place covariances on either side of the diagonal of the matrix
      if (i>j){
        for (k in 1:nrow(x)){
          val=val+(x[k,i]-mean(x[,i]))*(x[k,j]-mean(x[,j]))
        }
        val=val/nrow(x)
        mat[i,j]=val
        mat[j,i]=val
      }
    }
  }
  # Return the covariance matrix
  mat
}
