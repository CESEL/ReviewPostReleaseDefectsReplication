setwd("~/Work/inspections/data/")
##Requires this specific arules version, install if necessary
# pkgurl = 'https://cran.r-project.org/src/contrib/Archive/arules/arules_1.5-2.tar.gz'
# install.packages(pkgurl, repos=NULL, type="source")

# Library
library(bnlearn)
library(arules)

# Functions
clog = function(x){log(abs(x) + 1)}
proc_file = function(x){
  df = read.csv(file = x)
  k = strsplit(strsplit(x,"_")[[1]][3],".csv")[[1]][1]
  tx = rep(k,nrow(df))
  df = cbind(tx,df)
  dimnames(df)[[2]][1] = "release"
  return(df)
}
proc_data = function(csvdata){
  #Data Cleaning
  data.case2 = csvdata[,-c(1:2)]
  data.case2 = data.frame(sapply(data.case2,as.numeric))
  
  data.case2$typicaldisclennorm = NULL
  data.case2$typicalreviewwindownorm = NULL
  data.case2$typicalnumberofreviewers = NULL #since some releases have all 0 here
  #data.case2$authorownership = NULL
   
  #Discretize
  data2 = data.case2[complete.cases(data.case2),]
  data2$defects = as.factor((data2$defects>0)*1)
  
  v = sapply(data2, is.numeric)
  
  data2[,v] = data.frame(sapply(data2[,v], arules::discretize, method = "frequency", categories=3))
  
  #PGM - Boot
  boot2 = bnlearn::boot.strength(data = data2, R = 500,  algorithm = "hc", algorithm.args = list(score = "bic", restart = 100, perturb = 2))
  return(list(data2, boot2, data.case2))
}

#Data 

# Modify path to point to Chrome data files
filelist.chrome <- list.files(path = "../resources/csv_new", pattern = "*dir*", full.names = TRUE)
csvd.list.chrome = lapply(filelist.chrome, proc_file)
csvd.chrome = do.call(rbind,csvd.list.chrome )

# Modify path to point to Qt data files
filelist.qt <- list.files(path = "../resources/csv_new", pattern = "qt_5", full.names = TRUE)
csvd.list.qt = lapply(filelist.qt, proc_file)
csvd.qt = do.call(rbind, csvd.list.qt )

#Data Merging
csvdata = rbind(csvd.chrome, csvd.qt)

# Run BN model with appropriate dataset - 
# # chrome datasets from the list csvd.list.chrome
# [[1]] "chrome_39" [[2]] "chrome_40"
# [[3]] "chrome_41" [[4]] "chrome_42"
# [[5]] "chrome_43" [[6]] "chrome_44"
# # aggregate chrome dataset from csvd.chrome
# # qt datasets from the list csvd.list.qt
# [[1]] "qt_50_with_expertise" 
# [[2]] "qt_51_with_expertise"
# # aggregate qt dataset from csvd.qt
# # combined dataset from csvdata
all.lists = do.call(c, list(csvd.list.qt, csvd.list.chrome, 
                    list(csvd.qt,csvd.chrome,csvdata)))

result.bn = list()
for (i in 1:length(all.lists)){
  result.bn[[i]] = proc_data(all.lists[[i]])
}

# Change this index as needed
proc.res = result.bn[[11]]


data2 = proc.res[[1]]
boot2 = proc.res[[2]]
data.case2 = proc.res[[3]]
# plot bootstrap model
plot(boot2)

# !!!!! Adjust Threshold based on plot !!!!!!
avg.boot2 = bnlearn::averaged.network(boot2, threshold = 0.65)
# Plot final BN model
bnlearn::graphviz.plot(avg.boot2, highlight = list(nodes=bnlearn::mb(avg.boot2, "defects"), fill="orangered"), shape = "ellipse")


##################################
# Cross-Validation: Get precision and kappa values for different thresholds
##################################
library(e1071)
perf = data.frame(threshold = 0, prec = 0, kappa = 0)
cross.valid <- data2[sample(nrow(data2)),]
folds <- cut(seq(1,nrow(cross.valid)),breaks=10,labels=FALSE)
library(caret)
for (t in seq(0.4,0.9,0.05)){
  avg.boot2 = bnlearn::averaged.network(boot2, threshold = t)
  avg.boot2 = cextend(avg.boot2)
  avgPrec = 0
  avgKappa = 0
  for(i in 1:10){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- cross.valid[testIndexes, ]
    trainData <- cross.valid[-testIndexes, ]
    fitted = bnlearn::bn.fit(avg.boot2, trainData)
    pred = predict(fitted, "defects", testData)
    conf = caret::confusionMatrix(pred, testData[, "defects"])
    avgPrec = avgPrec + conf$overall[["Accuracy"]]
    avgKappa = avgKappa + conf$overall[["Kappa"]]
  }
  perf = rbind(perf, c(t, avgPrec/10, avgKappa/10))
}
plot(perf$threshold, perf$prec)
plot(perf$threshold, perf$kappa)

###############################################
# Inference
###############################################
library(gRain)
#### fit
b2 = bn.fit(avg.boot2, data2, method = "bayes")
jtree = compile(as.grain(b2))

(querygrain(jtree, nodes = c("defects")))

# Get conditional distribution for a set of variables
round(querygrain(jtree, nodes = c("defects","allreviewers"), type = "conditional"),3)

###########################################
# Fitting - to get coefficients for different links in the BN model
###########################################
data.2 = data.case2[complete.cases(data.case2),]
data.2 = data.frame(sapply(data.2,clog))
data.2 = data.frame(sapply(data.2,scale))
b2 = bn.fit(avg.boot2, data.2)

###############################
# comparing BIC scores between Empty graph and our model
###############################
emp = "[size][complexity][priordefects][churn][allchangescount][changeentropy][allauthors][minorauthors][majorauthors][authorownership][rushedreviews][changesnodisc][selfapprovedchanges][typicaldisclen][typicalreviewwindow][reviewissues][allreviews][allreviewers][typicalnumberofreviewers][defects][chnagesnoexpertise][typicalreviewerexpertise]"
emp.net = model2network(emp)
score(avg.boot2, data2, type = "loglik") 
(e = score(emp.net, data2, type = "loglik"))
empty.diff = score(avg.boot2, data2, type = "loglik") - score(emp.net, data2, type = "loglik")
empty.diff/e


