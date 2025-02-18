get_null_distribution <- function(concept_lattice,n_rep){
  n <- ncol(concept_lattice$extents)
  result <- rep(0,n_rep)
  for(k in (1:n_rep)){
    objective <- sample(c(-1,1),size=n,replace=TRUE)
	objective <- oofos::compute_objective(data.frame(y=objective),"y","1")
	
    result[k] <- max( concept_lattice$extents%*%objective)
  }
return(result)}

