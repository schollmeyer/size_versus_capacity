library(gurobi)
set.seed(123456789)
indexs <-sample((1:1354),size=50,replace=FALSE)
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

D <- as.matrix(dist(context))
nmds_results <- get_nmds_results(context=context,dimensions=(1:20))
CT1 <- get_context_from_distance(nmds_results$new_distances[[1]],threshold=10)#Inf)
CT2 <- get_context_from_distance(nmds_results$new_distances[[2]],threshold=3)
CT3 <- get_context_from_distance(nmds_results$new_distances[[3]],threshold=0)
CT4 <- get_context_from_distance(nmds_results$new_distances[[4]],threshold=0)

CT <- cbind(CT1$context)#,CT2$context)#,CT3$context,CT4$context)

dim(CT)





objval <- univariate_prediction(CT,objective)$objval
objval
h0_values <- rep(0,10000)
set.seed(1234567)
for(k in (1:10000)){
  h0_values[k] <- univariate_prediction(CT,sample(objective))$objval
}

mean(h0_values >= objval)
# [1] 0.0011

significance_plot(h0_values,objval)


cd <- get_context_distances(CT)
mean(cd)
model <- k_extent_opt_b(CT,objective=objective,K=10000)
res <- gurobi(model)
model2 <- k_extent_opt_b(CT,objective=objective,K=2)
model3 <- test(model,cd,threshold=10,K_max=2)  #16

res <- gurobi(model3)
res$runtime
model3$objval <- res$objval
set.seed(12345)
testt1 <- oofos::compute_extent_optim_test(model3,params=list(outputflag=1,timelimit=10),n_rep=100000)

#colnames(dat)=NAMES
dat=data.frame(dat)
dat$y=y
#####  END
for(k in (1:10)){dat[,k]=as.factor(dat[,k])}

res=DiscoverSubgroups(source=dat, target=as.target("y","TRUE"), config=new("SDTaskConfig",attributes=colnames(dat)[(1:10)],method="sdmap",qf="ps",k=1,maxlen=10))


#sd.new=function(dat,target,target.class,nrep,heuristic,remove.full.columns=TRUE,clarify.cols=FALSE,reduce.cols=FALSE,weighted=FALSE,small){

ans=sd.new(dat[i,],"y","TRUE",100,heuristic1, small=TRUE)
b=gurobi(ans)
colnames(ans$context)[which(b$x[-(1:1354)]>0.5)]


ans=sd.new(dat,"y","TRUE",100,heuristic1, small=TRUE)

sols=list()

nsub=1354
nrep=1000
objvals=rep(0,nrep)
Q=array(0,c(nrep,40))
v=ans$obj[(1:m)]
for(k in (1:nrep)){
i=sample((1:1354),size=1354,replace=TRUE)
#i=(1:1354)
v=ans$obj[(1:1354)]
#b=extent.opt(ans$context[i,],which(v>0),v)
w=table(factor(i,levels=(1:1354)))/1354

b=extent.opt(CT,which(v>0),v*w)
B=gurobi(b,list(outputFlag=0))
#print(colnames(ans$context)[which(B$x[-(1:nsub)]>0.5)])
#Q[k,]=B$x[-(1:1354)]
#print(colSums(Q))
#print(order(-colSums(Q)))
objvals[k]=B$objval
print(k)
print(which(B$x[-(1:1354)]>0.5))}
print(mean(objvals[(1:k)]>=objval))
plot(ecdf(objvals[(1:k)]))
abline(v=objval)
sols[[k]]=B



CT=cbind(ans$context,dat[,1]!=4,dat[,2]!=4,dat[,3]!=4,dat[,4]!=4,dat[,5]!=4,dat[,6]!=4,dat[,7]!=4,dat[,8]!=4,dat[,9]!=4,dat[,10]!=4)


dat2=1-a$context
dat2=data.frame(dat2)
dim(dat2)
dat2$y=y
for(k in (1:41)){dat2[,k]=as.factor(dat2[,k])}
res=DiscoverSubgroups(source=dat2, target=as.target("y","TRUE"), config=new("SDTaskConfig",attributes=colnames(dat2)[(1:40)],method="sdmap",qf="ps",k=1,maxlen=100))



#a$pd06a:oppositionunterstutzen
dat=na.omit(dat)
m=dim(dat)[1]
m=100
i=NULL
for(k in (1:m)){

if(any(dat[k,]=="KEINE ANGABE"|dat[k,]=="WEISS NICHT"|dat[k,]=="ITEM UNBEKANNT")){i=c(i,k)}}


#dat=dat[-i,]
#dat2=dat2[-i,]
yy=as.factor(dat[(1:m),9])

#yy=as.factor(yy %in% c("STIMME VOLL ZU","STIMME EHER ZU"))
dat3=array(0,dim(dat))
for(k in (1:8)){dat3[,k]=as.numeric(dat[,k])}

table(yy)/length(yy)


#m=dim(dat)[1]
dat=dat3[,(1:8)]
#m=100
######
######
######

library(foreign)

library(e1071)


q=8
#yy=(rep(0,m))

i=1;j=7


L=list()
R=array(0,c(m,q,q))
V=array(0,c(m,q*q))
for(k in (1:m)){

M=array(0,c(q,q))

for(l in (1:q)){
for(mm in (l:q)){
M[l,mm]=dat[k,l]<=dat[k,mm]

}}

#i=8;j=9

#yy[k]=M[i,j]


#M[i,]=999
#M[j,]=999
#M[,i]=999
#M[,j]=999

R[k,,]=M
L[[k]]=M
V[k,]=as.vector(M)


}

yy=as.factor(yy)


#iii=which(V[1,]==999)

#V=V[,-iii]

table(yy)/length(yy)


A=list(R=R,L=L,V=V)
BB=rankingSb(A)



x=V
ans=rep(0,10000);bns=ans
for(k in (1:10000)){
i=sample((1:m),size=90,replace=FALSE)

ii=sample((1:m)[-i],size=3,replace=FALSE)
if(any(ii%in%i)){print("ddd");break}

M=svm(x=x[i,],y=yy[i],kernel="linear",scale=FALSE)

ans[k]=mean(W[ii]*(predict(M,x[ii,])!=yy[ii]))

#www=exp(0*corr.weights(x[i,],yy[i]))#-0.9
temp=cwsb(x.train=x[i,],x.test=x[ii,],y.train=yy[i],y.test=yy[ii],stylizedBetweenness=c(sb1),VCDim=c(2),p=1.1,VCcut=TRUE,interval=c(0.000001,1),VCcorrection=FALSE)
bns[k]=mean(W[ii]*(temp$predictions!=yy[ii]))#temp$errorProb
Print(k)
Print("---")
Print(mean(ans[(1:k)]))
Print(mean(bns[(1:k)]))
}

Nachricht 1 von 18087
