#' Normalization Equation
#'
#' THis function normalizes values using the min and max of a given vector 
#' @param       col a column of data to normalize
#'
#' @return  Normalized column

normalize = function(col) {
  normalized_col = (col-min(col))/(max(col)-min(col))
  return(normalized_col)
}
                                    