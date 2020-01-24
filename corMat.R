#' Correlation Matrix Function
#'
#' Creates a matrix of correlation coefficients between variables in a data frame
#'
#' @param x The data frame whose correlation coefficients will be calculated
#'
#' @return a matrix of correlation coefficients
#' @export
#'
#' @examples
#' corMat(x)
corMat=function(x){
  # Create the matrix that will contain correlation coefficients
  mat=matrix(nrow=ncol(x),ncol=ncol(x))
  for (i in 1:ncol(x)){
    for (j in 1:ncol(x)){
      # Assign 1 to all positions on the diagonal of the matrix
      if (i==j){
        mat[i,j]=1
      }
      # Assign correlation coefficients to all positions on either side of the diagonal
      if (i>j){
        # Initialize the covariance to 0
        co=0
        # Calculate the covariance of the two variables
        for (k in 1:nrow(x)){
          co=co+(x[k,i]-mean(x[,i]))*(x[k,j]-mean(x[,j]))
        }
        co=co/nrow(x)
        # Initialize the first variable's variance to 0
        ivar=0
        # Calculate the first variable's variance
        for (k in 1:nrow(x)){
          ivar=ivar+(x[k,i]-mean(x[,i]))^2
        }
        ivar=ivar/nrow(x)
        # Initialize the second variable's covariance to 0
        jvar=0
        # Calculate the second variable's covariance
        for (k in 1:nrow(x)){
          jvar=jvar+(x[k,j]-mean(x[,j]))^2
        }
        jvar=jvar/nrow(x)
        # Assign correlation coefficients to positions in the matrix
        mat[i,j]=co/sqrt(ivar)/sqrt(jvar)
        mat[j,i]=co/sqrt(ivar)/sqrt(jvar)
      }
    }
  }
  # Return the correlation matrix
  mat
}
