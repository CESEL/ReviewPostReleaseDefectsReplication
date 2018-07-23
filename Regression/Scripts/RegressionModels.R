######################################################################################################
# R scripts used for the replication part.
# Note that variable set in the final model 
# may vary between adatasets based on the variable selected in Steps 2 and 3.
#
# The additiona degrees of freedom are assigned based on the analysis of the image received in step 4. 
######################################################################################################

#Required packages

require(rms)
clog = function(x){log(x + 1)}


## Step 1. Load data from csv ##

data = read.csv("~/csv/chrome_39.csv", header=T, na.strings=c(""))

## Step 2. Hierarchy clustering anlysis ##

vcobj = varclus(~ size + priordefects + churn + changeentropy + allauthors + minorauthors + changesnodisc + selfapprovedchanges + 
typicaldisclen + typicalreviewwindow + majorauthors + rushedreviews + complexity + authorownership 
+ chnagesnoexpertise + typicalreviewerexpertise, data=data, similarity = 'spear')

plot(vcobj, labels = c('size', 'priorDefects', 'churn', 'changeEntropy', 'allAuthors', 'minorAuthors', 
                       'changesNoDisc', 'selfApprovedChanges', 'typicalDiscLen', 'typicalReviewWindow', 'majorAuthors', 
                       'rushedReviews', 'complexity', 'authorOwnership',
                       'changesNoExp', 'typicalRevExp'))

abline(h = 0.3, v = 0, col = "gray60", lty=2)


## Step 3. Redundancy analysis ##

redunobj = redun(~ size + complexity + priordefects + churn + changeentropy + allauthors 
+ minorauthors + changesnodisc + typicaldisclen + rushedreviews + typicalreviewwindow 
+ chnagesnoexpertise + typicalreviewerexpertise, data=data, nk=5)

paste(redunobj$Out, collapse = ", ")


## Step 4. Nonlinearity anlysis ##

spearman2obj = spearman2(clog(defects)~ size + complexity + priordefects + churn + changeentropy 
+ allauthors + selfapprovedchanges + typicaldisclen + rushedreviews + typicalreviewwindow 
+ chnagesnoexpertise + typicalreviewerexpertise, data=data, p=2)

plot(spearman2obj)


## Step 5. OLS with restricted cubic splines ##

fit = ols(clog(defects)~ rcs(size, 5)  + rcs(priordefects, 5) + complexity + rcs(churn,3) + rcs(changeentropy,3) 
+ rcs(allauthors, 5) + rcs(selfapprovedchanges,5) + typicaldisclen  + rushedreviews + typicalreviewwindow  
+ rcs(chnagesnoexpertise, 3) + typicalreviewerexpertise, data = data, x=T, y=T)

## Step 6. Model stability analysis ##
iter = 1000
validate(fit, B = iter)

## Step 7. Assessment of contribution of individual predictors ##
anova(fit, test = 'Chisq', tol=1e-13)

## Step 8. Ploting of single predictor impact. All authors example ## 

bootcovobj = bootcov(fit, B=iter)
dd <- datadist(data)
options(datadist='dd')
resp = Predict(bootcovobj, allauthors, fun=function(x) return(exp(x)))
plot(resp)

