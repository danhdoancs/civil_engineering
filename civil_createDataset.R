library(wavelets)
library(foreign)

civil_extracter = function(prefix, col) {
mnl1 = read.arff(paste(prefix,"1_std.arff", sep=""));
mnl2 = read.arff(paste(prefix,"2_std.arff", sep=""));

mnl11 = mnl1[mnl1$class==1, ][,col]
mnl13 = mnl1[mnl1$class==3, ][,col]
mnl14 = mnl1[mnl1$class==4, ][,col]
mnl15 = mnl1[mnl1$class==5, ][,col]
mnl16 = mnl1[mnl1$class==6, ][,col]
mnl17 = mnl1[mnl1$class==7, ][,col]

mnl21 = mnl2[mnl2$class==1, ][,col]
mnl23 = mnl2[mnl2$class==3, ][,col]
mnl24 = mnl2[mnl2$class==4, ][,col]
mnl25 = mnl2[mnl2$class==5, ][,col]
mnl26 = mnl2[mnl2$class==6, ][,col]
mnl27 = mnl2[mnl2$class==7, ][,col]

lagRange = 5000:10000
mnl1 = rbind(mnl11[lagRange], mnl13[lagRange], mnl14[lagRange], mnl15[lagRange], mnl16[lagRange], mnl17[lagRange])
mnl2 = rbind(mnl21[lagRange], mnl23[lagRange], mnl24[lagRange], mnl25[lagRange], mnl26[lagRange], mnl27[lagRange])
mnl1 = as.data.frame(mnl1)
mnl2 = as.data.frame(mnl2)
mnl = rbind(mnl1, mnl2)
mnl = as.data.frame(mnl)

write.arff(mnl1, file=paste(prefix,"1_attr_",col,".arff", sep=""))
write.arff(mnl2, file=paste(prefix,"2_attr_",col,".arff", sep=""))
write.arff(mnl, file=paste(prefix,"_attr_",col,".arff", sep=""))
}

civil_generater = function(prefix) {
mnl1 = read.arff(paste(prefix,"1_std.arff", sep=""));
mnl2 = read.arff(paste(prefix,"2_std.arff", sep=""));

mnl11 = mnl1[mnl1$class==1, ]
mnl13 = mnl1[mnl1$class==3, ]
mnl14 = mnl1[mnl1$class==4, ]
mnl15 = mnl1[mnl1$class==5, ]
mnl16 = mnl1[mnl1$class==6, ]
mnl17 = mnl1[mnl1$class==7, ]

mnl21 = mnl2[mnl2$class==1, ]
mnl23 = mnl2[mnl2$class==3, ]
mnl24 = mnl2[mnl2$class==4, ]
mnl25 = mnl2[mnl2$class==5, ]
mnl26 = mnl2[mnl2$class==6, ]
mnl27 = mnl2[mnl2$class==7, ]

ccfLag = 500
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
mnl1ccf = as.data.frame(mnl1ccf)

mnl2ccf = as.data.frame(mnl2ccf)
mnlccf = rbind(mnl1ccf, mnl2ccf)
mnlccf = as.data.frame(mnlccf)

write.arff(mnl1ccf, file=paste(prefix,"1ccf_500.arff", sep=""))
write.arff(mnl2ccf, file=paste(prefix,"2ccf_500.arff", sep=""))
write.arff(mnlccf, file=paste(prefix,"ccf_500.arff", sep=""))

wtData = NULL
for (i in 1:nrow(mnlccf)) {
	a = t(mnlccf[i,])
	wt = dwt(a, filter="haar", boundary="periodic")
	wtData = rbind(wtData, unlist(c(wt@W,wt@V[[wt@level]])))
}
wtData = as.data.frame(wtData)

classIds = rep(c("1","3","4","5","6","7"),2)

wtSc = data.frame(cbind(classIds, wtData))

write.arff(wtSc,file=paste(prefix,"_dwt_500.arff", sep=""))
}
