CHI2 <- function(X,Y) {
	if(is.data.frame(Y))
		Y <- Y[[1]]	
	results = sapply(X, function(w) {
			cont = table(Y, w)
			row_sums = apply(cont, 1, sum)
			col_sums = apply(cont, 2, sum)
			all_sum = sum(col_sums)
			expected_matrix = t(as.matrix(col_sums) %*% t(as.matrix(row_sums))) / all_sum
			chis = sum((cont - expected_matrix) ^ 2 / expected_matrix)
			
			if(chis == 0 || length(col_sums) < 2 || length (row_sums) < 2) {
				return(0)
			} else {
				# phi or Cramer's V
				return(sqrt(chis / (all_sum * min(length(col_sums) - 1, length(row_sums) - 1))))
			}
		})

	attr_names = dimnames(X)[[2]]
	return(data.frame(attr_importance = results, row.names = attr_names))
}