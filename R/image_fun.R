
#' Returns the xval coordinate of a matrix
#'
#' This function simply returns a number correspondong to the x-index of a matrix, or creates a matrix with specified dimensions.
#'
#' @param x A matrix OR the x-dimensions of a matrix to be created.
#' @param y (optional) the y-dimensions of a matrix to be created.
#' @return A matrix of same dims as x or x*y
#' @export
xvals <- function(x, y=1){
  if(class(x)[1] == "matrix"){
    y <- dim(x)[2]
    x <- dim(x)[1]
  }
  my_m <- matrix(rep(0:(x-1),y), nrow=x)
  return (my_m)
}


#' Returns the yval coordinate of a matrix
#'
#' This function simply returns a number corresponding to the y-index of a matrix, or creates a matrix with specified dimensions.
#'
#' @param x A matrix OR the x-dimensions of a matrix to be created.
#' @param y (optional) the y-dimensions of a matrix to be created.
#' @return A matrix of same dims as x or x*y
#' @export
yvals <- function(x, y=1){
  if(class(x)[1] == "matrix"){
    y <- dim(x)[2]
    x <- dim(x)[1]
  }
  my_m <- matrix(rep(0:(y-1),each=x), nrow=x)
  return (my_m)
}


#' Returns the rvals coordinate of a matrix
#'
#' This function simply returns a number corresponding to the r-index of a matrix, or creates a matrix with specified dimensions.
#'
#' @param x A matrix OR the x-dimensions of a matrix to be created.
#' @param y (optional) the y-dimensions of a matrix to be created.
#' @param xoffset (optional) the x-offset of the origin
#' @param yoffset (optional) the y-offset of the origin
#' @return A matrix of same dims as x or x*y
#' @export
rvals <- function(x, y=1, xoffset=0, yoffset=0){
  if(class(x)[1] == "matrix"){
    y <- dim(x)[2]
    x <- dim(x)[1]
  }
  my_x <- matrix(rep(0:(x-1),y), nrow=x) - xoffset
  my_y <- matrix(rep(0:(y-1),each=x), nrow=x) - yoffset
  my_r <- sqrt(my_x^2 + my_y^2)
  return (my_r)
}






