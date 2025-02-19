# context1

set.seed(1234567)
CT <- oofos:::compute_random_context(75,25,p=0.9)
d <- gurobi(oofos::compute_extent_vc_dimension(CT))$objval

concept_lattice <- oofos:::compute_concept_lattice(CT)
s <- nrow(concept_lattice$extents)
s
null_distribution <- get_null_distribution(concept_lattice=NULL,context=CT,n_rep=10000)
plot(ecdf(null_distribution))
# [1] 10660954
saveRDS(concept_lattice,"concept_lattice1.RDS")
saveRDS(CT,"context1.RDS")
saveRDS(null_distribution,"null_distribution1.RDS")





# context2

set.seed(1234567)
CT <- oofos:::compute_random_context(200,100,p=0.97)
#d <- gurobi(oofos::compute_extent_vc_dimension(CT))$objval
# d=65
#concept_lattice <- oofos:::compute_concept_lattice(CT)
#s <- nrow(concept_lattice$extents)
null_distribution <- get_null_distribution(concept_lattice=NULL,context=CT,n_rep=10000)
plot(ecdf(null_distribution))

#saveRDS(concept_lattice,"concept_lattice1.RDS")
saveRDS(CT,"context2.RDS")
saveRDS(null_distribution,"null_distribution2.RDS")



