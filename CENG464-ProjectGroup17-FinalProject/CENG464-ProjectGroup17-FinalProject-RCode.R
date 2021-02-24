library(caret)
library(mlbench)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)
library(readxl)
library(dplyr)
library(fpc)
library(factoextra)
library(NbClust)
library(cluster)
library(class)
library(rpart)
library(rpart.plot)
#load data set
df<-read_excel("data1.xlsx")
#column names
colnames(df)
str(df)
summary(df)
sum(is.na(df))
sum(is.na(df$CitationMetric_4a_CM2))
sum(is.na(df$CitationMetric_5a_CM2))
#Filling missing value
df$CitationMetric_4a_CM2 = ifelse(is.na(df$CitationMetric_4a_CM2), ave(df$CitationMetric_4a_CM2, FUN = function(x) mean(x, na.rm = 'TRUE')), df$CitationMetric_4a_CM2)
df$CitationMetric_5a_CM2 = ifelse(is.na(df$CitationMetric_5a_CM2), ave(df$CitationMetric_5a_CM2, FUN = function(x) mean(x, na.rm = 'TRUE')), df$CitationMetric_5a_CM2)
sum(is.na(df))
count_Countries_Unique_Count<-table(df$Countries_Unique_Count)
sort(count_Countries_Unique_Count,decreasing=TRUE)[1:5]
counts_Countries_First_Author <- table(df$Countries_First_Author)
sort(counts_Countries_First_Author,decreasing=TRUE)[1:5]
#Delete Categorical Variables
df$J_or_C  <- NULL
df$Countries_Perc<-NULL
df$Countries_Unique_Count<-NULL
df$Countries_First_Author<-NULL

#Correlation
correlations<-cor(df[,1:78])
#Delete 
df$comma_mark<-NULL
df$em_dash_mark<-NULL
df$exclamation_mark<-NULL
df$semicolon_mark<-NULL
df$underscore_mark<-NULL
df$numFreqLexItems_reviews<-NULL
df$id<-NULL
#Correlation
correlations<-cor(df[,1:71])
print(correlations)
diag(correlations)<-0
correlations<-abs (correlations)
which(correlations>0.75,arr.ind=T)

df$Countries_Num<-NULL
df$FLESCH_Title<-NULL
df$FLESCH_Abstract<-NULL
df$numCharTitle_onlyAlpha<-NULL
df$numWordTitle<-NULL
df$numTitleSubstantiveWordsWithStopwords<-NULL
df$numCharTitle_all<-NULL
df$binABV_Abstract<-NULL
df$period_mark<-NULL
df$binABV_Title<-NULL
df$PaperAge<-NULL
df$CitationMetric_4a_CM2<-NULL
df$CitationMetric_3<-NULL
df$CitationMetric_1<-NULL
df$CitationMetric_5b_CM3<-NULL
df$CitationMetric_5a_CM2<-NULL
df$`Cited by`<-NULL
df$CitationMetric_2<-NULL
df$CitationMetric_5_CB<-NULL
df$numSentAbstract<-NULL
df$numTitleSubstantiveWordsWoutStopwords<-NULL
df$numAbstractSubstantiveWordsWithStopwords<-NULL
df$question_mark_isExist<-NULL
df$question_mark_loc<-NULL
df$presenceInitialPosition_a<-NULL
df$presenceInitialPosition_a_or_the<-NULL
df$numPreposition<-NULL
df$question_mark<-NULL
df$parenthesis_mark<-NULL
df$plus_mark<-NULL
df$square_parenthesis_mark<-NULL
df$curly_parenthesis_mark<-NULL
df$backslash_mark<-NULL
df$apostrophe_mark<-NULL
df$and_mark<-NULL
df$equal_mark<-NULL
df$hyphen_mark<-NULL
df$slash_mark<-NULL
df$colon_mark<-NULL
df$presenceInitialPosition_the<-NULL
df$presenceInitialPosition_ing<-NULL
df$numPrepositionBeginning<-NULL
df$numFreqLexItems_connectives<-NULL
df$numFreqLexItems_previews<-NULL
df$numFreqLexItems_action_markers<-NULL
df$numFreqLexItems_closing<-NULL
df$numSylGreaThan2<-NULL
df$double_quote_mark<-NULL
df$single_quote_mark<-NULL
df$presenceColon<-NULL
df$numCharTitle_nonAlpha<-NULL
df$numAbstractSubstantiveWordsWoutStopwords<-NULL
df$Countries_Unique_Num<-NULL
df$CitationMetric_4_CB<-NULL
df$isFunding<-NULL
#Correlation
correlations<-cor(df[,1:18])

print(correlations)
diag(correlations)<-0
correlations<-abs (correlations)
which(correlations>0.75 ,arr.ind=T)


par(mfrow=c(1,5))


#boxplot

for (i in 1:16) {
  boxplot(df[,i],main=names(df)[i]) 
}


#Authors_Num
boxplot.stats(df$Authors_Num)$out
out <- boxplot.stats(df$Authors_Num)$out
out
out_ind <- which(df$Authors_Num %in% c(out))
out_ind
df[out_ind,1]=(df$Authors_Num)
df[which(df$Authors_Num %in% out),]
df$Authors_Num = ifelse(df$Authors_Num>7.99, ave(df$Authors_Num, FUN = function(x) mean(x, na.rm = 'TRUE')), df$Authors_Num)
boxplot(df[,1],main=names(df)[1])
dfk<-df
df<-dfk
#FRES_Title
boxplot.stats(df$FRES_Title)$out
out <- boxplot.stats(df$FRES_Title)$out
out
out_ind <- which(df$FRES_Title %in% c(out))
out_ind
df[which(df$FRES_Title %in% out),]
df$FRES_Title = ifelse(df$FRES_Title>100, ave(df$FRES_Title, FUN = function(x) mean(x, na.rm = 'TRUE')), df$FRES_Title)
df$FRES_Title = ifelse(df$FRES_Title<(-66.5), ave(df$FRES_Title, FUN = function(x) mean(x, na.rm = 'TRUE')), df$FRES_Title)
boxplot(df[,2],main=names(df)[2])
#TTR_Title
boxplot.stats(df$TTR_Title)$out
out <- boxplot.stats(df$TTR_Title)$out
out_ind <- which(df$TTR_Title %in% c(out))
out_ind
df[which(df$TTR_Title %in% out),]
df$TTR_Title = ifelse(df$TTR_Title<0.96, ave(df$TTR_Title, FUN = function(x) mean(x, na.rm = 'TRUE')), df$TTR_Title)
boxplot(df[,4],main=names(df)[4])
#numABV_Title
boxplot.stats(df$numABV_Title)$out
out <- boxplot.stats(df$numABV_Title)$out
out_ind <- which(df$numABV_Title %in% c(out))
out_ind
df[which(df$numABV_Title %in% out),]
df$numABV_Title = ifelse(df$numABV_Title>0.99, ave(df$numABV_Title, FUN = function(x) mean(x, na.rm = 'TRUE')), df$numABV_Title)
boxplot(df[,5],main=names(df)[5])
#numABV_Abstract
boxplot.stats(df$numABV_Abstract)$out
out <- boxplot.stats(df$numABV_Abstract)$out
out_ind <- which(df$numABV_Abstract %in% c(out))
out_ind
df[which(df$numABV_Abstract %in% out),]
df$numABV_Abstract = ifelse(df$numABV_Abstract>7, ave(df$numABV_Abstract, FUN = function(x) mean(x, na.rm = 'TRUE')), df$numABV_Abstract)
boxplot(df[,6],main=names(df)[6])
#numPage
boxplot.stats(df$numPage)$out
out <- boxplot.stats(df$numPage)$out
out
out_ind <- which(df$numPage %in% c(out))
out_ind
df[which(df$numPage %in% out),]
df$numPage = ifelse(df$numPage>20, ave(df$numPage, FUN = function(x) mean(x, na.rm = 'TRUE')), df$numPage)
boxplot(df[,8],main=names(df)[8])
#Year
boxplot.stats(df$Year)$out
out <- boxplot.stats(df$Year)$out
out_ind <- which(df$Year %in% c(out))
out_ind
df[which(df$Year %in% out),]
df$Year = ifelse(df$Year<1995, ave(df$Year, FUN = function(x) mean(x, na.rm = 'TRUE')), df$Year)
boxplot(df[,9],main=names(df)[9])
#Fres_abs
boxplot.stats(df$FRES_Abstract)$out
out <- boxplot.stats(df$FRES_Abstract)$out
out_ind <- which(df$FRES_Abstract %in% c(out))
out_ind
df[which(df$FRES_Abstract %in% out),]
df$FRES_Abstract = ifelse(df$FRES_Abstract>58.17, ave(df$FRES_Abstract, FUN = function(x) mean(x, na.rm = 'TRUE')), df$FRES_Abstract)
df$FRES_Abstract = ifelse(df$FRES_Abstract<(-8.70), ave(df$FRES_Abstract, FUN = function(x) mean(x, na.rm = 'TRUE')), df$FRES_Abstract)
boxplot(df[,10],main=names(df)[10])
#numKeywords
boxplot.stats(df$numKeywords)$out
out <- boxplot.stats(df$numKeywords)$out
out_ind <- which(df$numKeywords %in% c(out))
out_ind
df[which(df$numKeywords %in% out),]
df$numKeywords = ifelse(df$numKeywords>9.9, ave(df$numKeywords, FUN = function(x) mean(x, na.rm = 'TRUE')), df$numKeywords)
df$numKeywords = ifelse(df$numKeywords<1, ave(df$numKeywords, FUN = function(x) mean(x, na.rm = 'TRUE')), df$numKeywords)
boxplot(df[,12],main=names(df)[12])
#CitationMetric_4b
boxplot.stats(df$CitationMetric_4b_CM3)$out
out <- boxplot.stats(df$CitationMetric_4b_CM3)$out
out_ind <- which(df$CitationMetric_4b_CM3 %in% c(out))
out_ind
df[which(df$CitationMetric_4b_CM3 %in% out),]
df$CitationMetric_4b_CM3 = ifelse(df$CitationMetric_4b_CM3>3.5, ave(df$CitationMetric_4b_CM3, FUN = function(x) mean(x, na.rm = 'TRUE')), df$CitationMetric_4b_CM3)
boxplot(df[,14],main=names(df)[14])

#avgPunctua
boxplot.stats(df$avgPunctuation)$out
out <- boxplot.stats(df$avgPunctuation)$out
out_ind <- which(df$avgPunctuation %in% c(out))
out_ind
df[which(df$avgPunctuation %in% out),]
df <- df[-which(df$avgPunctuation %in% out),]
df$avgPunctuation = ifelse(df$avgPunctuation>1.10, ave(df$avgPunctuation, FUN = function(x) mean(x, na.rm = 'TRUE')), df$avgPunctuation)

boxplot(df[,16],main=names(df)[16])
#CitationMetric_4b_CM3
boxplot.stats(df$CitationMetric_4b_CM3)$out
out <- boxplot.stats(df$CitationMetric_4b_CM3)$out
out_ind <- which(df$CitationMetric_4b_CM3 %in% c(out))
out_ind
df[which(df$CitationMetric_4b_CM3 %in% out),]
df <- df[-which(df$CitationMetric_4b_CM3 %in% out),]




#normalizing attributes
#Normalize
#calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(df[,1:16], method=c("range"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, df[,1:16])
# summarize the transformed dataset
summary(transformed)
df1<-transformed
df_knn<-transformed

par(mfrow=c(1,5))

df1$Quartile<-NULL
#boxplot

for (i in 1:16) {
  boxplot(df1[,i],main=names(df1)[i]) 
}

#Elbow method
fviz_nbclust(df1, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) + 
  labs(subtitle = "Elbow method") 


#Applying k-means with 5
model2 <- kmeans(df1, centers = 5 ,nstart=50, iter.max = 15)
print(model2$cluster)

data_cluster <- data.frame(df1, cluster = as.factor(model2$cluster))
head(data_cluster)

# Compute k-means with k = 5
set.seed(123)
kmm <- kmeans(df1[,1:16],5, nstart = 20)
# K-means clusters showing the group of each individuals
kmm$cluster

# Print the results
print(kmm)
fviz_cluster(kmm, data = df1[, 1:16],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800","red" , "pink"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
str(kmm)
fviz_cluster(kmm, data = df1)
# Dissimilarity matrix
d <- dist(df1, method = "euclidean")
kmm

#Applying ward's method
d <- dist(df1, method = "euclidean")# distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendrogram
plot(fit, cex = 0.6, hang = -1)

rect.hclust(fit, k = 5, border = 1:16)


#cluster analysis
cluster.stats(d=NULL, model2$cluster, fit$cluster)



#KNN-y
ran <- sample(1:nrow(df),0.75 * nrow(df))
dia_train <- df_knn[ran,]
dia_test <- df_knn[-ran,]

dia_target <-(df[ran,14])
test_target <-(df[-ran,14])

#run knn function
library(class)
pr <- knn(dia_train,dia_test,cl=dia_train$Quartile,k=4)

#create the confusion matrix
tb <- table(pr,dia_test$Quartile)

#check the accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)

#Decision Tree

fit <- rpart(Quartile~., data = dia_train, method = 'class')
rpart.plot(fit, extra = 106)
predict_unseen <-predict(fit, dia_test, type = 'class')
table_mat <- table(dia_test$Quartile, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, knn_test, type = 'class')
  table_mat <- table(knn_test$Quartile, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}
control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(Quartile~., data = dia_train, method = 'class', control = control)
#random forest
require(randomForest)
model1 <- randomForest(Quartile ~ ., data = dia_train, importance = TRUE)
model1
#J_or_Q
library(caret)
library(mlbench)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(ggplot2)
library(readxl)
library(dplyr)
library(fpc)
library(factoextra)
library(NbClust)
library(cluster)
library(class)
library(rpart)
library(rpart.plot)
#load data set
df<-read_excel("data1.xlsx")
dataset<-read_excel("data1.xlsx")

#column names
sum(is.na(df$CitationMetric_4a_CM2))
sum(is.na(df$CitationMetric_5a_CM2))
#Filling missing value
df$CitationMetric_4a_CM2 = ifelse(is.na(df$CitationMetric_4a_CM2), ave(df$CitationMetric_4a_CM2, FUN = function(x) mean(x, na.rm = 'TRUE')), df$CitationMetric_4a_CM2)
df$CitationMetric_5a_CM2 = ifelse(is.na(df$CitationMetric_5a_CM2), ave(df$CitationMetric_5a_CM2, FUN = function(x) mean(x, na.rm = 'TRUE')), df$CitationMetric_5a_CM2)
sum(is.na(df))
#Delete Categorical Variables
df$Countries_Unique_Count<-NULL
df$Countries_Perc<-NULL
df$Countries_First_Author<-NULL
#j orQ
df$J_or_C <- ifelse(df$J_or_C == "J", 0 , 1)
table(df$J_or_C)
#Correlation
correlations<-cor(df[,1:78])
#Delete 
df$comma_mark<-NULL
df$em_dash_mark<-NULL
df$exclamation_mark<-NULL
df$semicolon_mark<-NULL
df$underscore_mark<-NULL
df$numFreqLexItems_reviews<-NULL
df$id<-NULL
#Correlation
correlations<-cor(df[,1:71])
print(correlations)
diag(correlations)<-0
correlations<-abs (correlations)
which(correlations>0.90 ,arr.ind=T)


df$Countries_Num<-NULL

df$FLESCH_Title<-NULL
df$FLESCH_Abstract<-NULL
df$numCharTitle_onlyAlpha<-NULL
df$numWordTitle<-NULL
df$numTitleSubstantiveWordsWithStopwords<-NULL

df$numCharTitle_all<-NULL
df$binABV_Abstract<-NULL
df$period_mark<-NULL
df$binABV_Title<-NULL

df$PaperAge<-NULL

df$CitationMetric_4a_CM2<-NULL
df$CitationMetric_3<-NULL
df$CitationMetric_1<-NULL
df$CitationMetric_5b_CM3<-NULL
df$CitationMetric_5a_CM2<-NULL
df$`Cited by`<-NULL
df$CitationMetric_2<-NULL

df$CitationMetric_5_CB<-NULL
df$numSentAbstract<-NULL
df$numTitleSubstantiveWordsWoutStopwords<-NULL
df$numAbstractSubstantiveWordsWithStopwords<-NULL
df$question_mark_isExist<-NULL
df$question_mark_loc<-NULL
df$presenceInitialPosition_a<-NULL
df$presenceInitialPosition_a_or_the<-NULL
df$numPreposition<-NULL
df$question_mark<-NULL
df$parenthesis_mark<-NULL
df$plus_mark<-NULL
df$square_parenthesis_mark<-NULL
df$curly_parenthesis_mark<-NULL
df$backslash_mark<-NULL
df$apostrophe_mark<-NULL
df$and_mark<-NULL
df$equal_mark<-NULL
df$hyphen_mark<-NULL
df$slash_mark<-NULL
df$colon_mark<-NULL
df$presenceInitialPosition_the<-NULL
df$presenceInitialPosition_ing<-NULL
df$numPrepositionBeginning<-NULL
df$numFreqLexItems_connectives<-NULL
df$numFreqLexItems_previews<-NULL
df$numFreqLexItems_action_markers<-NULL
df$numFreqLexItems_closing<-NULL
df$numSylGreaThan2<-NULL
df$double_quote_mark<-NULL
df$single_quote_mark<-NULL
df$presenceColon<-NULL
df$numCharTitle_nonAlpha<-NULL
df$numAbstractSubstantiveWordsWoutStopwords<-NULL
df$Countries_Unique_Num<-NULL
df$CitationMetric_4_CB<-NULL


#Correlation
correlations<-cor(df[,1:17])

print(correlations)
diag(correlations)<-0
correlations<-abs (correlations)
which(correlations>0.75 ,arr.ind=T)


par(mfrow=c(1,5))


#boxplot

for (i in 1:17) {
  boxplot(df[,i],main=names(df)[i]) 
}


#Authors_Num
boxplot.stats(df$Authors_Num)$out
out <- boxplot.stats(df$Authors_Num)$out
out
out_ind <- which(df$Authors_Num %in% c(out))
out_ind
df[out_ind,1]=(df$Authors_Num)
df[which(df$Authors_Num %in% out),]
df$Authors_Num = ifelse(df$Authors_Num>7.99, ave(df$Authors_Num, FUN = function(x) mean(x, na.rm = 'TRUE')), df$Authors_Num)
boxplot(df[,1],main=names(df)[1])
dfk<-df
df<-dfk
#FRES_Title
boxplot.stats(df$FRES_Title)$out
out <- boxplot.stats(df$FRES_Title)$out
out
out_ind <- which(df$FRES_Title %in% c(out))
out_ind
df[which(df$FRES_Title %in% out),]
df$FRES_Title = ifelse(df$FRES_Title>100, ave(df$FRES_Title, FUN = function(x) mean(x, na.rm = 'TRUE')), df$FRES_Title)
df$FRES_Title = ifelse(df$FRES_Title<(-66.5), ave(df$FRES_Title, FUN = function(x) mean(x, na.rm = 'TRUE')), df$FRES_Title)
boxplot(df[,2],main=names(df)[2])
#TTR_Title
boxplot.stats(df$TTR_Title)$out
out <- boxplot.stats(df$TTR_Title)$out
out_ind <- which(df$TTR_Title %in% c(out))
out_ind
df[which(df$TTR_Title %in% out),]
df$TTR_Title = ifelse(df$TTR_Title<0.96, ave(df$TTR_Title, FUN = function(x) mean(x, na.rm = 'TRUE')), df$TTR_Title)
boxplot(df[,4],main=names(df)[4])
#numABV_Title
boxplot.stats(df$numABV_Title)$out
out <- boxplot.stats(df$numABV_Title)$out
out_ind <- which(df$numABV_Title %in% c(out))
out_ind
df[which(df$numABV_Title %in% out),]
df$numABV_Title = ifelse(df$numABV_Title>0.99, ave(df$numABV_Title, FUN = function(x) mean(x, na.rm = 'TRUE')), df$numABV_Title)
boxplot(df[,5],main=names(df)[5])
#numABV_Abstract
boxplot.stats(df$numABV_Abstract)$out
out <- boxplot.stats(df$numABV_Abstract)$out
out_ind <- which(df$numABV_Abstract %in% c(out))
out_ind
df[which(df$numABV_Abstract %in% out),]
df$numABV_Abstract = ifelse(df$numABV_Abstract>7, ave(df$numABV_Abstract, FUN = function(x) mean(x, na.rm = 'TRUE')), df$numABV_Abstract)
boxplot(df[,6],main=names(df)[6])
#numPage
boxplot.stats(df$numPage)$out
out <- boxplot.stats(df$numPage)$out
out
out_ind <- which(df$numPage %in% c(out))
out_ind
df[which(df$numPage %in% out),]
df$numPage = ifelse(df$numPage>20, ave(df$numPage, FUN = function(x) mean(x, na.rm = 'TRUE')), df$numPage)
boxplot(df[,8],main=names(df)[8])
#Year
boxplot.stats(df$Year)$out
out <- boxplot.stats(df$Year)$out
out_ind <- which(df$Year %in% c(out))
out_ind
df[which(df$Year %in% out),]
df$Year = ifelse(df$Year<1995, ave(df$Year, FUN = function(x) mean(x, na.rm = 'TRUE')), df$Year)
boxplot(df[,9],main=names(df)[9])
#Fres_abs
boxplot.stats(df$FRES_Abstract)$out
out <- boxplot.stats(df$FRES_Abstract)$out
out_ind <- which(df$FRES_Abstract %in% c(out))
out_ind
df[which(df$FRES_Abstract %in% out),]
df$FRES_Abstract = ifelse(df$FRES_Abstract>58.17, ave(df$FRES_Abstract, FUN = function(x) mean(x, na.rm = 'TRUE')), df$FRES_Abstract)
df$FRES_Abstract = ifelse(df$FRES_Abstract<(-8.70), ave(df$FRES_Abstract, FUN = function(x) mean(x, na.rm = 'TRUE')), df$FRES_Abstract)
boxplot(df[,10],main=names(df)[10])
#numKeywords
boxplot.stats(df$numKeywords)$out
out <- boxplot.stats(df$numKeywords)$out
out_ind <- which(df$numKeywords %in% c(out))
out_ind
df[which(df$numKeywords %in% out),]
df$numKeywords = ifelse(df$numKeywords>9.9, ave(df$numKeywords, FUN = function(x) mean(x, na.rm = 'TRUE')), df$numKeywords)
df$numKeywords = ifelse(df$numKeywords<1, ave(df$numKeywords, FUN = function(x) mean(x, na.rm = 'TRUE')), df$numKeywords)
boxplot(df[,12],main=names(df)[12])
#CitationMetric_4b
boxplot.stats(df$CitationMetric_4b_CM3)$out
out <- boxplot.stats(df$CitationMetric_4b_CM3)$out
out_ind <- which(df$CitationMetric_4b_CM3 %in% c(out))
out_ind
df[which(df$CitationMetric_4b_CM3 %in% out),]
df$CitationMetric_4b_CM3 = ifelse(df$CitationMetric_4b_CM3>3.5, ave(df$CitationMetric_4b_CM3, FUN = function(x) mean(x, na.rm = 'TRUE')), df$CitationMetric_4b_CM3)
boxplot(df[,14],main=names(df)[14])

#avgPunctua
boxplot.stats(df$avgPunctuation)$out
out <- boxplot.stats(df$avgPunctuation)$out
out_ind <- which(df$avgPunctuation %in% c(out))
out_ind
df[which(df$avgPunctuation %in% out),]
df <- df[-which(df$avgPunctuation %in% out),]
df$avgPunctuation = ifelse(df$avgPunctuation>1.10, ave(df$avgPunctuation, FUN = function(x) mean(x, na.rm = 'TRUE')), df$avgPunctuation)

boxplot(df[,16],main=names(df)[16])
#CitationMetric_4b_CM3
boxplot.stats(df$CitationMetric_4b_CM3)$out
out <- boxplot.stats(df$CitationMetric_4b_CM3)$out
out_ind <- which(df$CitationMetric_4b_CM3 %in% c(out))
out_ind
df[which(df$CitationMetric_4b_CM3 %in% out),]
df <- df[-which(df$CitationMetric_4b_CM3 %in% out),]




#normalizing attributes
#Normalize
#calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(df[,1:17], method=c("range"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, df[,1:17])
# summarize the transformed dataset
summary(transformed)
df1<-transformed
df_knn<-transformed

par(mfrow=c(1,1))

for (i in 1:16) {
  boxplot(df1[,i],main=names(df1)[i]) 
}

#Applying k-means with 2
model2 <- kmeans(df1, centers = 2 ,nstart=50, iter.max = 15)
print(model2$cluster)

data_cluster <- data.frame(df1, cluster = as.factor(model2$cluster))
head(data_cluster)

# Compute k-means with k = 5
set.seed(123)
kmm <- kmeans(df1[,1:17],2, nstart = 20)
# K-means clusters showing the group of each individuals
kmm$cluster
# Print the results
print(kmm)
fviz_cluster(kmm, data = df1[, 1:17],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800","red" , "pink"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
str(kmm)
fviz_cluster(kmm, data = df1)
# Dissimilarity matrix
d <- dist(df1, method = "euclidean")
kmm


#Applying ward's method
d <- dist(df1, method = "euclidean")# distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendrogram
plot(fit, cex = 0.6, hang = -1)

rect.hclust(fit, k =3, border = 1:18)

#KNN-y
ran <- sample(1:nrow(df),0.75 * nrow(df))
dia_train <- df_knn[ran,]
dia_test <- df_knn[-ran,]

dia_target <-(df[ran,15])
test_target <-(df[-ran,15])

#run knn function
library(class)
pr <- knn(dia_train,dia_test,cl=dia_train$J_or_C,k=2)

#create the confusion matrix
tb <- table(pr,dia_test$J_or_C)

#check the accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)

#Decision Tree

fit <- rpart(J_or_C~., data = dia_train, method = 'class')
rpart.plot(fit, extra = 106)
predict_unseen <-predict(fit, dia_test, type = 'class')
table_mat <- table(dia_test$J_or_C, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, dia_test, type = 'class')
  table_mat <- table(dia_test$J_or_C, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}
control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(J_or_C~., data = dia_train, method = 'class', control = control)
accuracy_tune(tune_fit)
#random forest
require(randomForest)
model1 <- randomForest(J_or_C ~ ., data =dia_train, importance = TRUE)
model1

write.csv(df1, 'CENG464-ProjectGroup7-FinalProject-DataFile.csv' )

