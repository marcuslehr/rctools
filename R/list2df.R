# This function will covert lists with unequal numbers of items into a dataframe

list2df = function(list) {

  # Create an appropriately-sized empty matrix
  mat = matrix(NA,
               ncol = max(unlist(lapply(list, function(x) length(x)))),
               nrow = length(list))

  # Fill in the matrix using the list
  for (i in 1:length(list)) {
    x = list[[i]]
    for (j in 1:length(x)) {
      mat[i,j] = x[j]
    }
  }
  ## The whole frame can be filtered by keeping as a matrix, using grepl, then converting back to a df.

  as.data.frame(mat)
}
