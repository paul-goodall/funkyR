
#' Extract characters from the start (left) of a string
#'
#' This function simply returns a number of specified characters from the start of a string/character vector.
#'
#' @param my_str A character vector to be cut
#' @param nc Number of characters from the left to be kept
#' @return A character vector of extract strings of length nchar=nc
#' @export
left <- function(my_str, nc){
  my_str <- substr(my_str, 1, nc)
  return (my_str)
}


#' Extract characters from the end (right) of a string
#'
#' This function simply returns a number of specified characters from the end of a string/character vector.
#'
#' @param my_str A character vector to be cut
#' @param nc Number of characters from the right to be kept
#' @return A character vector of extract strings of length nchar=nc
#' @export
right <- function(my_str, nc){
  my_str <- stringi::stri_reverse(my_str)
  my_str <- substr(my_str, 1, nc)
  my_str <- stringi::stri_reverse(my_str)
  return (my_str)
}

#' Extract a character vector from another character vector
#'
#' This function extracts a body of string from between a prefix-string (head) and suffix-string (tail) of a character vector.
#'
#' @param master_string A character vector to be cut
#' @param string_head The pattern prefix
#' @param string_tail The pattern suffix
#' @return A character vector the same dimensions as master_string
#' @export
extract_string_body <- function(master_string, string_head, string_tail){
  my_str <- strsplit(as.character(master_string), string_head, fixed=T)
  ii <- 2*(1:length(my_str))
  my_str <- purrr::flatten(my_str)
  my_str <- as.character(my_str[ii])
  my_str <- strsplit(my_str, string_tail, fixed=T)
  if(length(my_str[[1]]) == 1) ii <- 1:length(my_str)
  if(length(my_str[[1]]) == 2) ii <- 2*(1:length(my_str))-1
  my_str <- purrr::flatten(my_str)
  my_str <- as.character(my_str[ii])
  return (my_str)
}



