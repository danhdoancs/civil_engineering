library(foreign)

civil_ccf_combinor = function() {
mnlccf = read.arff("mnlccf_500.arff")
cmoccf = read.arff("cmoccf_500.arff")

dataccf = rbind(mnlccf, cmoccf)
write.arff(dataccf,"dataccf_500.arff")

mnl_dwt = read.arff("mnl_dwt_500.arff")
cmo_dwt = read.arff("cmo_dwt_500.arff")

data_dwt = rbind(mnl_dwt, cmo_dwt)
write.arff(data_dwt, "data_dwt_500.arff")
}

civil_attr_combinor = function() {
mnl = read.arff("mnl_attr_3.arff")
cmo = read.arff("cmo_attr_3.arff")

data = rbind(mnl, cmo)
write.arff(data,"data_attr_3.arff")
}
