set.seed(1234567)
CT <- oofos:::compute_random_context(75,25,p=0.9)
d <- gurobi(oofos::compute_extent_vc_dimension(CT))$objval

concept_lattice <- oofos:::compute_concept_lattice(CT)
s <- nrow(concept_lattice$extents)
null_distribution <- get_null_distribution(concept_lattice,n_rep=1000)

plot(ecdf(null_distribution))

