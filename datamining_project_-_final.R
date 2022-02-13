## HCV Project - code 

dataset = read.csv('hcvdat0.csv')

######## Phase1 , Data preprocessing #################


#Date summary , Deleting missing values

summary(dataset)

sum(is.na(dataset)) 

dataset<-na.omit(dataset)
  
#Deleting Outliers one by one

outAge<- boxplot(dataset$Age, plot=FALSE)$outdataset<-
  
   dataset[-which(dataset$Age %in% outAge),] 
  outALB<-boxplot(dataset$ALB, plot=FALSE)$out 
  dataset<-dataset[-which(dataset$ALB%in%outALB),]
                                                           
   outALP<-boxplot(dataset$ALP, plot=FALSE)$out
dataset<-dataset[-which(dataset$ALP %in% outALP),]
                                                          
  outALT<-boxplot(dataset$ALT, plot=FALSE)$out
dataset<-dataset[-which(dataset$ALT %in% outALT),]
                                                           
   outAST<-boxplot(dataset$AST, plot=FALSE)$out 
dataset<-dataset[-which(dataset$AST %in% outAST),]
                                                          
  outBIL<-boxplot(dataset$BIL, plot=FALSE)$out 
dataset<-dataset[- which(dataset$BIL %in% outBIL),]
                                                          
   outCHE<-boxplot(dataset$CHE, plot=FALSE)$out

  dataset<-dataset[-which(dataset$CHE %in% outCHE),]
                                                          
  outCHOL<-boxplot(dataset$CHOL, plot=FALSE)$out 
  dataset<-dataset[-which(dataset$CHOL %in% outCHOL),]
                                                            
  outCREA<-boxplot(dataset$CREA, plot=FALSE)$out 
  
  dataset<-dataset[- which(dataset$CREA %in% outCREA),]
                                                           
 outGGT<- boxplot(dataset$GGT, plot=FALSE)$out
 
 dataset<-dataset[-which(dataset$GGT %in% outGGT),]
                                                           
 outPROT<- boxplot(dataset$PROT, plot=FALSE)$out 
 
 dataset<-dataset[-which(dataset$PROT %in% outPROT),]
                                                           
#correlation
cor(dataset$Age, datasetALP)
cor(dataset$Age, datasetCHOL)
cor(dataset$Age, datasetCHE)
cor(dataset$Age, datasetALB)
cor(dataset$Age, datasetALT)
cor(dataset$Age, datasetAST)
cor(dataset$Age, datasetBIL)
cor(dataset$Age, datasetCREA)
cor(dataset$Age, datasetGGT)
cor(dataset$Age, datasetPORT)


#encoding
dataset$Category=factor(dataset$Category , levels = c("0=Blood Donor" , "0s=suspect
Blood Donor" , "1=Hepatitis" , "2=Fibrosis" , "3=Cirrhosis"),labels =
                          c("BloodDonor",
                            "SusBloodDonor","Hepatitis" , "Fibrosis" , "Cirrhosis"))
#normalization
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
dataset$ALB<-normalize(dataset$ALB) dataset$ALP<-normalize(dataset$ALP)
dataset$ALT<-normalize(dataset$ALT) dataset$AST<-normalize(dataset$AST)
dataset$BIL<-normalize(dataset$BIL) dataset$CHE<-normalize(dataset$CHE)
dataset$CHOL<-normalize(dataset$CHOL) dataset$CREA<-
  normalize(dataset$CREA) dataset$GGT<-normalize(dataset$GGT)
dataset$PROT<-normalize(dataset$PROT) 



##-------------------------phase 2 - Data mining task code------------------------------------## 

#-------------------Classification mining task------------------------#

#-----------------------------Tree1-----------------------------------#


set.seed(1234)

#divied the trainig data and testing data by 70% training and 30% testing
ind <- sample(2, nrow(dataset) , replace = TRUE , prob = c(0.7 , 0.3))

train.data <- dataset[ind == 1,]
test.data <- dataset[ind == 2,]


#library party for the trees
library("party")

#formela

myF <- Category ~Age +ALB + ALP + ALT + AST + BIL + CHE + GGT + CHOL+ CREA +PROT



#bulid the tree using the ctree()

dataset.ctree <- ctree(myF , data = train.data)


# build a decision tree using training set and check the prediction

table(predict(dataset.ctree), train.data$Category)


#print and plot the tree

print(dataset.ctree)

plot(dataset.ctree )

plot(dataset.ctree , type="simple") #make it simple plot


#using the constructed model to predict the class lables of test data that we named test.data

testpredict <- predict(dataset.ctree, newdata= test.data)

#evalute the model

#library for evalution
library(e1071)
library(caret)

table(testpredict, test.data$Category)

#compute the result
result <- confusionMatrix(testpredict, test.data$Category)
result

#compute the accuracy

accuarcy<- result$overall["Accuracy"]*100
accuarcy

#compute the matrix by overall and by classes

as.matrix(result , what = "overall")
as.matrix(result , what = "classes")

#print all result
print(result)

#-----------------------------Tree2-----------------------------------#
  
  set.seed(1234)

#divied the trainig data and testing data by 80% training and 20% testing
ind <- sample(2, nrow(dataset) , replace = TRUE , prob = c(0.8 , 0.2))

train.data <- dataset[ind == 1,]
test.data <- dataset[ind == 2,]


#library party for the trees
library("party")

#formela

myF <- Category ~Age +ALB + ALP + ALT + AST + BIL + CHE + GGT + CHOL+ CREA +PROT



#bulid the tree using the ctree()

dataset.ctree <- ctree(myF , data = train.data)


# build a decision tree using training set and check the prediction

table(predict(dataset.ctree), train.data$Category)


#print and plot the tree

print(dataset.ctree)

plot(dataset.ctree )

plot(dataset.ctree , type="simple") #make it simple plot


#using the constructed model to predict the class lables of test data that we named test.data

testpredict <- predict(dataset.ctree, newdata= test.data)

#evalute the model

#library for evalution
library(e1071)
library(caret)

table(testpredict, test.data$Category)

#compute the result
result <- confusionMatrix(testpredict, test.data$Category)
result

#compute the accuracy

accuarcy<- result$overall["Accuracy"]*100
accuarcy

#compute the matrix by overall and by classes

as.matrix(result , what = "overall")
as.matrix(result , what = "classes")

#print all result
print(result)
 

  

#-----------------------------Tree3-----------------------------------#
set.seed(1234)

#divied the trainig data and testing data by 90% training and 10% testing
ind <- sample(2, nrow(dataset) , replace = TRUE , prob = c(0.9 , 0.1))

train.data <- dataset[ind == 1,]
test.data <- dataset[ind == 2,]


#library party for the trees
library("party")

#formela

myF <- Category ~Age +ALB + ALP + ALT + AST + BIL + CHE + GGT + CHOL+ CREA +PROT



#bulid the tree using the ctree()

dataset.ctree <- ctree(myF , data = train.data)


# build a decision tree using training set and check the prediction

table(predict(dataset.ctree), train.data$Category)


#print and plot the tree

print(dataset.ctree)

plot(dataset.ctree )

plot(dataset.ctree , type="simple") #make it simple plot


#using the constructed model to predict the class lables of test data that we named test.data

testpredict <- predict(dataset.ctree, newdata= test.data)

#evalute the model

#library for evalution
library(e1071)
library(caret)

table(testpredict, test.data$Category)

#compute the result
result <- confusionMatrix(testpredict, test.data$Category)
result

#compute the accuracy

accuarcy<- result$overall["Accuracy"]*100
accuarcy

#compute the matrix by overall and by classes

as.matrix(result , what = "overall")
as.matrix(result , what = "classes")

#print all result
print(result)

#-------------------Clustring mining task------------------------#

####################Prepare data for clustering#################################
#see labels before clustering 
dataset.label<-dataset$Category
table(dataset.label)

#Extracting the features we want to cluster in a new data set
HCVfeatures<-dataset
HCVfeatures$Category<-NULL
HCVfeatures$X<-NULL
HCVfeatures$Sex<-NULL

#set a seed for random number generation  to make the results reproducible
set.seed(8953)

#########################################################################
#install packages
#install.packages("gtools")
#install.packages("cluster")
#install.packages("kableExtra")
#install.packages("NbClust")
#install.packages("ggplot2")
#install.packages("factoextra")
#install.packages("clValid")
#install.packages("GGally")

# Loading package
#clustering and visualization
library(cluster)
library(ggplot2)
library(factoextra)
library(plotly)
#Valdation
library(GGally)
library(clValid)
library(kableExtra)
library(NbClust)
###############################
## K-Medoids clustering Model##
###############################
#---------------------------TRY FIRST NUMBER---------------------------------#
# Split the data set into 2 clusters using k-medoids method 
kmedoids.re <- pam(HCVfeatures, 2, nstart = 20)
# Print the result
kmedoids.re
# Visualize clusters 
fviz_cluster(kmedoids.re, data = HCVfeatures)
# Confusion Matrix
cm <- table(dataset$Category, kmedoids.re$cluster)
cm
#avg for each cluster 
avsilKmedoids<-silhouette(kmedoids.re$cluster,dist(HCVfeatures))
fviz_silhouette(avsilKmedoids)

#---------------------------TRY SECOND NUMBER---------------------------------#
# Split the data set into 3 clusters using k-medoids method 
kmedoids.re2 <- pam(HCVfeatures, 3, nstart = 20)
# Print the result
kmedoids.re2
# Visualize clusters 
fviz_cluster(kmedoids.re2, data = HCVfeatures)
# Confusion Matrix
cm <- table(dataset$Category, kmedoids.re2$cluster)
cm
#avg for each cluster 
avsilKmedoids2<-silhouette(kmedoids.re2$cluster,dist(HCVfeatures))
fviz_silhouette(avsilKmedoids2)
#---------------------------TRY THIRD NUMBER---------------------------------#
# Split the data set into 2 clusters using k-medoids method
kmedoids.re3 <- pam(HCVfeatures, 4, nstart = 20)
# Print the result
kmedoids.re3
# Visualize clusters 
fviz_cluster(kmedoids.re3, data = HCVfeatures)
# Confusion Matrix
cm <- table(dataset$Category, kmedoids.re3$cluster)
cm
#avg for each cluster 
avsilKmedoids3<-silhouette(kmedoids.re3$cluster,dist(HCVfeatures))
fviz_silhouette(avsilKmedoids3)


##############################
## K-Means clustering Model###
##############################

#---------------------------TRY FIRST NUMBER---------------------------------#
# Split the data set into 2 clusters using k-mean method 
kmeans.re <- kmeans(HCVfeatures, centers = 2, nstart = 20)
# Print the result
kmeans.re
# Visualize clusters 
fviz_cluster(kmeans.re, data = HCVfeatures)
# Confusion Matrix
cm <- table(dataset$Category, kmeans.re$cluster)
cm
#avg for each cluster 
avsilkmeans<-silhouette(kmeans.re$cluster,dist(HCVfeatures))
fviz_silhouette(avsilkmeans)
#---------------------------TRY SECOND NUMBER---------------------------------#
# Split the data set into 3 clusters using k-mean method 
kmeans.re2 <- kmeans(HCVfeatures, centers = 3, nstart = 20)
# Print the result
kmeans.re2
# Visualize clusters 
fviz_cluster(kmeans.re2, data = HCVfeatures)
# Confusion Matrix
cm <- table(dataset$Category, kmeans.re2$cluster)
cm
#avg for each cluster 
avsilkmeans2<-silhouette(kmeans.re2$cluster,dist(HCVfeatures))
fviz_silhouette(avsilkmeans2)
#############
##Interpret 
#############
dataset$cluster <- as.factor(kmeans.re2$cluster)
p <- ggparcoord(data = HCVfeatures, groupColumn = "cluster", scale = "std") + labs(x = "milk constituent", y = "value (in standard-deviation units)", title = "Clustering")
ggplotly(p)
#---------------------------TRY THIRD NUMBER---------------------------------#
# Split the data set into 4 clusters using k-mean method
kmeans.re3 <- kmeans(HCVfeatures, centers = 4, nstart = 20)
# Print the result
kmeans.re3
# Visualize clusters 
fviz_cluster(kmeans.re3, data = HCVfeatures)
# Confusion Matrix
cm <- table(dataset$Category, kmeans.re3$cluster)
cm
#avg for each cluster 
avsilkmeans3<-silhouette(kmeans.re3$cluster,dist(HCVfeatures))
fviz_silhouette(avsilkmeans3)

###################################
## Hierarchical clustering Model###
###################################
#---------------------------TRY FIRST NUMBER---------------------------------#
# Split the data set into 2 clusters using hierarchical method 
hierarchical.re <- eclust(HCVfeatures, "hclust", k = 2,
                          method="complete", graph = FALSE, nstart=20)
# Visualize clusters 
fviz_cluster(hierarchical.re, data = HCVfeatures)
# Confusion Matrix
cm <- table(dataset$Category, hierarchical.re$cluster)
cm
#avg for each cluster 
avsilHR<-silhouette(hierarchical.re$cluster,dist(HCVfeatures))
fviz_silhouette(avsilHR)

#---------------------------TRY SECOND NUMBER---------------------------------#
#Split the data set into 3 clusters using hierarchical method 
hierarchical.re2 <- eclust(HCVfeatures, "hclust", k = 3,
                           method="complete", graph = FALSE, nstart=20)
# Visualize clusters 
fviz_cluster(hierarchical.re2, data = HCVfeatures)
# Confusion Matrix
cm <- table(dataset$Category, hierarchical.re2$cluster)
cm
#avg for each cluster 
avsilHR2<-silhouette(hierarchical.re2$cluster,dist(HCVfeatures))
fviz_silhouette(avsilHR2)

#---------------------------TRY THIRD NUMBER---------------------------------#
#Split the data set into 4 clusters using hierarchical method 
hierarchical.re3 <- eclust(HCVfeatures, "hclust", k = 4,
                           method="complete", graph = FALSE, nstart=20)
# Visualize clusters 
fviz_cluster(hierarchical.re3, data = HCVfeatures)
# Confusion Matrix
cm <- table(dataset$Category, hierarchical.re3$cluster)
cm
#avg for each cluster 
avsilHR3<-silhouette(hierarchical.re3$cluster,dist(HCVfeatures))
fviz_silhouette(avsilHR3)

##################
####Validation####
##################
# (A) NbClust Validation 
fres.nbclust <- NbClust(HCVfeatures, distance="euclidean", min.nc = 2, max.nc = 10, method="kmeans", index="all")
# (B)cValid Valdation
intern <- clValid(HCVfeatures, nClust = 2:24, 
                  clMethods = c("hierarchical","kmeans","pam"), validation = "internal")
summary(intern) %>% kable() %>% kable_styling()
