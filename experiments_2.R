set.seed(1234567)
CT <- oofos:::compute_random_context(100,40,p=0.9)
d <- gurobi(oofos::compute_extent_vc_dimension(CT))$objval

L <- oofos:::compute_concept_lattice(CT)

null_distribution <- get_null_distribution(L,n_rep=1000)

plot(ecdf(null_distribution))

