library(wavelets)
library(foreign)


# Clustering
cluster = function(file, labelRep, method="average") {
library(dtw)
mnlccf = read.arff(file);
if (labelRep < 4) {
classIds = rep(c("1","3","4","5","6","7"),labelRep)
} else {
	classIds = cbind(rep(c("1_mnl","3_mnl","4_mnl","5_mnl","6_mnl","7_mnl"),labelRep/2), rep(c("1_cmo","3_cmo","4_cmo","5_cmo","6_cmo","7_cmo"),labelRep/2))
}
distMatrix = dist(mnlccf, method="DTW")
hc = hclust(distMatrix, method=method)
#dev.new()
png(filename= paste(file , "_cluster.png", sep=""))
plot(hc, labels=classIds, main=paste(file , " Cluster", sep=""))
dev.off()
}

# Classification
classifier = function(file, labelRep) {
library(party)
classIds = rep(c("1","3","4","5","6","7"),labelRep)
wtSc = read.arff(file)
ct = ctree(classIds~ ., data=wtSc, controls=ctree_control(minsplit=1, minbucket=1,maxdepth=10))
pClass = predict(ct)

table(classIds, pClass)
dev.new()
plot(ct, ip_args=list(pval=FALSE), ep_args=list(digits=0))
}
