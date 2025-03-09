library(gurobi)
# Synthetic Contexts
# context1

set.seed(1234567)
CT1 <- oofos:::compute_random_context(75,25,p=0.9)
d1 <- gurobi(oofos::compute_extent_vc_dimension(CT))$objval
# [1] 20
concept_lattice1 <- oofos:::compute_concept_lattice(CT1)
s1 <- nrow(concept_lattice$extents)
s1
# 10660954
null_distribution1 <- get_null_distribution(concept_lattice=NULL,context=CT,n_rep=10000)
plot(ecdf(null_distribution1))

saveRDS(concept_lattice1,"concept_lattice1.RDS")
saveRDS(CT1,"context1.RDS")
saveRDS(null_distribution1,"null_distribution1.RDS")





# context2

set.seed(1234567)
CT2 <- oofos:::compute_random_context(200,100,p=0.97)
#d2 <- gurobi(oofos::compute_extent_vc_dimension(CT))$objval
# d=65
#concept_lattice2 <- oofos:::compute_concept_lattice(CT2)
#s2 <- nrow(concept_lattice2$extents)
null_distribution <- get_null_distribution(concept_lattice=NULL,context=CT2,n_rep=10000)
plot(ecdf(null_distribution))

#saveRDS(concept_lattice2,"concept_lattice2.RDS")
saveRDS(CT2,"context2.RDS")
saveRDS(null_distribution2,"null_distribution2.RDS")

# Real Contexts

# context 3


set.seed(123456789)
indexs <-sample((1:1354),size=100,replace=FALSE)
setwd("C:/GIT/Datasets/Allbus_2018_ISSP2017")
library(gurobi)
library(foreign)
library(rsubgroup)
a <- read.spss("ZA5270_v2-0-0.sav",to.data.frame=TRUE)
NAMES <- c("BUS-/LKW-FAHRER", "GESCHAEFTSFUEHRUNG","REINIGUNGSKRAFT","FRISEUR/IN","LEITER PERSONALABTEILUNG","RECHTSANWALT","AUTOMECHANIKER/IN","KRANKENPFLEGER/IN","POLIZIST/IN","LEHRER/IN","WIE OFT: MIT FREUNDEN AUSGEHEN","WIE OFT: NEUE FREUNDSCHAFTEN SCHLIESSEN")
dat <- na.omit(cbind(a$I001A_1,a$I001A_2,a$I001A_3,a$I001A_4,a$I001A_5,a$I001B_1,a$I001B_2,a$I001B_3,a$I001B_4,a$I001B_5,a$I017,a$I018) )
target <- as.factor(dat[indexs,12])
objective <- oofos::compute_objective(data.frame(target=target %in% c(4,5)), "target", TRUE)
dat <- as.data.frame(dat[,(1:10)])
for(k in (1:10)){dat[,k]=as.factor(dat[,k])}
context <- oofos:::get_auto_conceptual_scaling(dat[indexs,])
context <- 1-context
d <- gurobi(oofos::compute_extent_vc_dimension(context))$objval
d
#[1] 16
concept_lattice <- oofos:::compute_concept_lattice(context)
size <- nrow(concept_lattice$extents)
size


null_distribution <- get_null_distribution(concept_lattice=NULL,context=context,n_rep=100,outputflag=1)
plot(ecdf(null_distribution))
####

#context4 (synthetic)

set.seed(1234567)
CT4 <- oofos:::compute_random_context(75,25,p=0.3)
d4 <- gurobi(oofos::compute_extent_vc_dimension(CT4))$objval
# [1] 5
concept_lattice4 <- oofos:::compute_concept_lattice(CT4)
s4 <- nrow(concept_lattice4$extents)
s4
#[1] 1144
null_distribution4 <- get_null_distribution(concept_lattice=concept_lattice4,context=NULL,n_rep=10000)
plot(ecdf(null_distribution4))

saveRDS(concept_lattice4,"concept_lattice4.RDS")
saveRDS(CT1,"context4.RDS")
saveRDS(null_distribution4,"null_distribution4.RDS")



#context5 (synthetic)

set.seed(1234567)
CT5 <- oofos:::compute_random_context(75,20,p=0.5)
d5 <- gurobi(oofos::compute_extent_vc_dimension(CT5))$objval
# [1] 7
concept_lattice5 <- oofos:::compute_concept_lattice(CT5)
s5 <- nrow(concept_lattice5$extents)
s5
# [1] 8319
null_distribution5 <- get_null_distribution(concept_lattice=concept_lattice5,context=NULL,n_rep=10000)
plot(ecdf(null_distribution5))

saveRDS(concept_lattice5,"concept_lattice5.RDS")
saveRDS(CT5,"context5.RDS")
saveRDS(null_distribution5,"null_distribution5.RDS")


#context6 (synthetic)

set.seed(1234567)
CT6 <- oofos:::compute_random_context(75,15,p=0.9)
d6 <- gurobi(oofos::compute_extent_vc_dimension(CT6))$objval
# [1] 14
concept_lattice6 <- oofos:::compute_concept_lattice(CT6)
s6 <- nrow(concept_lattice6$extents)
s6
# [1] 29184
null_distribution6 <- get_null_distribution(concept_lattice=concept_lattice6,context=NULL,n_rep=10000)
plot(ecdf(null_distribution6))

saveRDS(concept_lattice6,"concept_lattice6.RDS")
saveRDS(CT6,"context6.RDS")
saveRDS(null_distribution6,"null_distribution6.RDS")


set.seed(1234567)
CT7 <- oofos:::compute_random_context(85,20,p=0.9)
d7 <- gurobi(oofos::compute_extent_vc_dimension(CT7))$objval
# [1] 16
concept_lattice7 <- oofos:::compute_concept_lattice(CT7)
s7 <- nrow(concept_lattice7$extents)
s7
# [1] 590094
null_distribution7 <- get_null_distribution(concept_lattice=NULL,context=CT7,n_rep=100)
plot(ecdf(null_distribution7))

saveRDS(concept_lattice7,"concept_lattice7.RDS")
saveRDS(CT7,"context7.RDS")
saveRDS(null_distribution7,"null_distribution7.RDS")




set.seed(1234567)
CT8 <- oofos:::compute_random_context(50,20,p=0.9)
d8 <- gurobi(oofos::compute_extent_vc_dimension(CT8))$objval
# [1] 15
concept_lattice8 <- oofos:::compute_concept_lattice(CT8)
s8 <- nrow(concept_lattice8$extents)
s8
# [1] 306568
null_distribution8 <- get_null_distribution(concept_lattice=NULL,context=CT8,n_rep=100)
plot(ecdf(null_distribution8))

saveRDS(concept_lattice8,"concept_lattice8.RDS")
saveRDS(CT8,"context8.RDS")
saveRDS(null_distribution8,"null_distribution8.RDS")

set.seed(1234567)
CT9 <- oofos:::compute_random_context(20,20,p=0.1)
d9 <- gurobi(oofos::compute_extent_vc_dimension(CT9))$objval
# [1] 3
concept_lattice9 <- oofos:::compute_concept_lattice(CT9)
s9 <- nrow(concept_lattice9$extents)
s9
# [1] 25
null_distribution9 <- get_null_distribution(concept_lattice=concept_lattice9,context=NULL,n_rep=10000)
plot(ecdf(null_distribution8))

saveRDS(concept_lattice8,"concept_lattice8.RDS")
saveRDS(CT8,"context8.RDS")
saveRDS(null_distribution8,"null_distribution8.RDS")


set.seed(1234567)
CT10 <- oofos:::compute_random_context(40,40,p=0.1,seed=123456)
d10 <- gurobi(oofos::compute_extent_vc_dimension(CT10))$objval
# [1] 3
concept_lattice10 <- oofos:::compute_concept_lattice(CT10)
s10 <- nrow(concept_lattice10$extents)
s10
# [1] 25
null_distribution10 <- get_null_distribution(concept_lattice=concept_lattice10,context=NULL,n_rep=10000)
plot(ecdf(null_distribution8))

saveRDS(concept_lattice8,"concept_lattice8.RDS")
saveRDS(CT8,"context8.RDS")
saveRDS(null_distribution8,"null_distribution8.RDS")


set.seed(1234567)
CT11 <- oofos:::compute_random_context(50,50,p=0.1,seed=123456)
d11 <- gurobi(oofos::compute_extent_vc_dimension(CT11))$objval
# [1] 3
concept_lattice11 <- oofos:::compute_concept_lattice(CT11)
s11 <- nrow(concept_lattice11$extents)
s11
# [1] 198
null_distribution11 <- get_null_distribution(concept_lattice=concept_lattice11,context=NULL,n_rep=10000)
plot(ecdf(null_distribution11))

saveRDS(concept_lattice11,"concept_lattice11.RDS")
saveRDS(CT11,"context11.RDS")
saveRDS(null_distribution11,"null_distribution11.RDS")



set.seed(1234567)
CT12 <- oofos:::compute_random_context(60,50,p=0.4,seed=123456)
d12 <- gurobi(oofos::compute_extent_vc_dimension(CT12))$objval
# [1] 7


concept_lattice12 <- oofos:::compute_concept_lattice(CT12)
s12 <- nrow(concept_lattice12$extents)
s12
# [1]  24597
null_distribution12 <- get_null_distribution(concept_lattice=concept_lattice12,context=NULL,n_rep=100)
plot(ecdf(null_distribution11))

saveRDS(concept_lattice11,"concept_lattice11.RDS")
saveRDS(CT11,"context11.RDS")
saveRDS(null_distribution11,"null_distribution11.RDS")

concept_lattice12 <- oofos:::compute_concept_lattice(CT12)
s12 <- nrow(concept_lattice12$extents)
s12
# [1]  24597
null_distribution12 <- get_null_distribution(concept_lattice=concept_lattice12,context=NULL,n_rep=100)
plot(ecdf(null_distribution11))

saveRDS(concept_lattice11,"concept_lattice11.RDS")
saveRDS(CT11,"context11.RDS")
saveRDS(null_distribution11,"null_distribution11.RDS")



set.seed(1234567)
CT13 <- oofos:::compute_random_context(20,50,p=0.4,seed=123456)
d13 <- gurobi(oofos::compute_extent_vc_dimension(CT13))$objval
# [1] 6
concept_lattice13 <- oofos:::compute_concept_lattice(CT13)
s13 <- nrow(concept_lattice13$extents)
s13
# [1]  1392
null_distribution13 <- get_null_distribution(concept_lattice=concept_lattice13,context=NULL,n_rep=100)
plot(ecdf(null_distribution11))

saveRDS(concept_lattice11,"concept_lattice11.RDS")
saveRDS(CT11,"context11.RDS")
saveRDS(null_distribution11,"null_distribution11.RDS")

d <- c(d1,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13)

s <- c(s1,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13)
C=0.25
p <- c(mean(null_distribution1>=C),mean(null_distribution4>=C),
       mean(null_distribution5>=C),mean(null_distribution6>=C),
       mean(null_distribution7>=C),mean(null_distribution8>=C),
       mean(null_distribution9>=C),mean(null_distribution10>=C),
       mean(null_distribution11>=C),mean(null_distribution12>=C),mean(null_distribution13>=C))
