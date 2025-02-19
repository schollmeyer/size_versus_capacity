get_null_distribution <- function(concept_lattice=NULL,context=NULL,n_rep,outputflag=0){
  n <- ncol(concept_lattice$extents)
  if(!is.null(context)){n <- nrow(context)}
  result <- rep(0,n_rep)
 if(!is.null(context)){model <- oofos::optimize_on_context_extents(context,objective=rep(0,n))}	
  for(k in (1:n_rep)){
    objective <- sample(c(-1,1),size=n,replace=TRUE)
    objective <- oofos::compute_objective(data.frame(y=objective),"y","1")
    if(!is.null(concept_lattice)){	
     result[k] <- max( concept_lattice$extents%*%objective)
    }
  else{
	model$obj[(1:n)] <- objective
	result[k] <- gurobi::gurobi(model,params=list(outputflag=outputflag))$objval  
	  }
  }
return(result)}

