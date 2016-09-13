
disp.distinct.values <- function(data,metadata){
	for(VAR in names(data)){
		print(VAR)
		print(metadata[VAR,"VAR_DESCR1"])
		print(levels(unlist(data[VAR])))
		print(" ")
		print("  -----------------------------------  ")
	}
}

count.na <- function(data){
	return(colSums(1*(is.na(data))))
}
