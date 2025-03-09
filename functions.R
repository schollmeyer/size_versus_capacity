get_null_distribution <- function(concept_lattice=NULL,context=NULL,n_rep,outputflag=0,method){
  n <- ncol(concept_lattice$extents)
  if(!is.null(context)){n <- nrow(context)}
  result <- rep(0,n_rep)
 if(!is.null(context)){model <- oofos::optimize_on_context_extents(context,objective=rep(0,n))}
  for(k in (1:n_rep)){
    objective <- sample(c(-1,1),size=n,replace=TRUE)
	if(method=="Dn" | method=="D_n"){
	  indexs <- sample((1:n),size=n/2)
	  objective <- rep(1,n)
	  objective[indexs] <- -1
	  objective <- objective /n*2
	  #objective <- oofos::compute_objective(data.frame(y=objective),"y","1")
	}
	if(method!= "Rademacher" & method!="Dn" & method!="D_n"){print("no valid method specified");return(NULL)}

	if(method=="Rademacher"){
	  objective <- objective/n*2
	}
    if(!is.null(concept_lattice)){
     result[k] <- max( concept_lattice$extents%*%objective)
    }
  else{
	model$obj[(1:n)] <- objective
	result[k] <- gurobi::gurobi(model,params=list(outputflag=outputflag))$objval
	  }
  }
return(result)}

compute_vc_spectrum <- function(context){
remaining_context <- context
n_row_remaining_context <- nrow(remaining_context)
spectrum <- NULL
while(TRUE){
  n_row_remaining_context <- nrow(remaining_context)
  temp <- gurobi(oofos::compute_extent_vc_dimension(remaining_context))
  indexs <- which(temp$x[seq_len(n_row_remaining_context)] >0.5)

  n_row_remaining_context <- nrow(remaining_context)
  print(indexs)
  rc <<- remaining_context
  idx <<-indexs
  n_row <<- n_row_remaining_context
  spectrum2 <<- spectrum
  if(is.null(nrow(remaining_context[-indexs,])) | nrow(remaining_context[-indexs,])<=1){return(spectrum)}
  remaining_context <- remaining_context[-indexs,]

  spectrum <- c(spectrum,temp$objval)
 }
 }



