library(wavelets)
library(foreign)

mnl1 = read.table("dataset_MNL_1_std.txt", header=FALSE, sep=",");
mnl2 = read.table("dataset_MNL_2_std.txt", header=FALSE, sep=",");

mnl11 = mnl1[mnl1$V4==1, ]
mnl13 = mnl1[mnl1$V4==3, ]
mnl14 = mnl1[mnl1$V4==4, ]
mnl15 = mnl1[mnl1$V4==5, ]
mnl16 = mnl1[mnl1$V4==6, ]
mnl17 = mnl1[mnl1$V4==7, ]

mnl21 = mnl2[mnl2$V4==1, ]
mnl23 = mnl2[mnl2$V4==3, ]
mnl24 = mnl2[mnl2$V4==4, ]
mnl25 = mnl2[mnl2$V4==5, ]
mnl26 = mnl2[mnl2$V4==6, ]
mnl27 = mnl2[mnl2$V4==7, ]

ccfLag = 200
mnl11ccf = ccf(mnl11[,3], mnl11[,2],ccfLag)
mnl13ccf = ccf(mnl13[,3], mnl13[,2],ccfLag)
mnl14ccf = ccf(mnl14[,3], mnl14[,2],ccfLag)
mnl15ccf = ccf(mnl15[,3], mnl15[,2],ccfLag)
mnl16ccf = ccf(mnl16[,3], mnl16[,2],ccfLag)
mnl17ccf = ccf(mnl17[,3], mnl17[,2],ccfLag)

mnl21ccf = ccf(mnl21[,3], mnl21[,2],ccfLag)
mnl23ccf = ccf(mnl23[,3], mnl23[,2],ccfLag)
mnl24ccf = ccf(mnl24[,3], mnl24[,2],ccfLag)
mnl25ccf = ccf(mnl25[,3], mnl25[,2],ccfLag)
mnl26ccf = ccf(mnl26[,3], mnl26[,2],ccfLag)
mnl27ccf = ccf(mnl27[,3], mnl27[,2],ccfLag)

lagRange = (ccfLag-1)/2:ccfLag*2
mnl1ccf = rbind(mnl11ccf$acf[lagRange], mnl13ccf$acf[lagRange], mnl14ccf$acf[lagRange], mnl15ccf$acf[lagRange], mnl16ccf$acf[lagRange], mnl17ccf$acf[lagRange])
mnl2ccf = rbind(mnl21ccf$acf[lagRange], mnl23ccf$acf[lagRange], mnl24ccf$acf[lagRange], mnl25ccf$acf[lagRange], mnl26ccf$acf[lagRange], mnl27ccf$acf[lagRange])
mnlccf = rbind(mnl1ccf, mnl2ccf)

write.arff(mnl1ccf, file="mnl1ccf.arff")
write.arff(mnl2ccf, file="mnl2cff.arff")
write.arff(mnlccf, file="mnlcff.arff")

wtData = NULL
for (i in 1:nrow(mnlccf)) {
	a = mnlccf[i,]
	wt = dwt(a, filter="haar", boundary="periodic")
	wtData = rbind(wtData, unlist(c(wt@W,wt@V[[wt@level]])))
}
wtData = as.data.frame(wtData)

classIds = rep(c("1","3","4","5","6","7"),2)

wtSc = data.frame(cbind(classIds, wtData))

write.arff(wtSc,file="mnl_dwt.arff")

# Clustering
library(dtw)
distMatrix = dist(mnlccf, method="DTW")
hc = hclust(distMatrix, method="average")
dev.new()
plot(hc, labels=classIds, main="MNL Cluster")

# Classification
library(party)
ct = ctree(classIds~ ., data=wtSc, controls=ctree_control(minsplit=5, minbucket=5,maxdepth=10))
pClass = predict(ct)

table(classIds, pClass)
dev.new()
plot(ct, ip_args=list(pval=FALSE), ep_args=list(digits=0))

