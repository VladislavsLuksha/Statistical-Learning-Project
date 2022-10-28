library(factoextra)
library(tidyverse)
library(corrplot)
library(factoextra)
library("reshape2")
library("dendextend")
library(solitude) 
library(tidyr)
library(car)
library(skimr)
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(caret)
library(grid)
library(gridExtra)
library("epitools")
library(pROC)
library(MASS)
library(class)
library(gmodels)
library(randomForest)
library(stringr)
library(FactoMineR)

df <- read_csv("https://raw.githubusercontent.com/VladislavsLuksha/Statistical-Learning-Project/main/formula1.csv")
df <- as.data.frame(df)
colnames(df)

sapply(df, function(x) sum(is.na(x)))

###################### BASIC DESCRIPTIVE ANALYSIS ############################
#relabling levels for favorite circuit variable
df$fav_circuit <- as.factor(ifelse(df$fav_circuit %in% c("Suzuka Circuit","Marina Bay","Albert Park","Adelaide Street Circuit, Australia"), 'APAC',
                                   ifelse(df$fav_circuit %in% c("Autódromo José Carlos Pace" ,"Interlagos"    ,"Autodromo Hermanos Rodriguez"), 'LATAM',
                                          ifelse(df$fav_circuit %in% c("Circuit of The Americas","Miami International Autodrome" ,"Circuit Gilles Villeneuve" ,"24"), 'NORTH_AMERICA',
                                                 ifelse(df$fav_circuit %in% c("Baku City Circuit" ,"as Marina","Jeddah Corniche Circuit","Bahrain International Circuit","Kyalami, South Africa"), "MEAST_AFR", "EUROPE")))))

#c("Circuit Zandvoort" ,"Circuit de Monaco","Autodromo Nazionale di Monza","Circuit de Spa-Francorchamps","Red Bull Ring","Paul Ricard","Imola")
#'Europe'
#'
unique(df$team)
unique(df$country)
unique(df$fav_circuit)
df$team <- as.factor(df$team)

#summary statistics
df[, -c(1,2,3,4,12)] <- sapply(df[, -c(1,2,3,4,12)],as.numeric)
Mu <- colMeans(df[, -c(1, 2,3,4,12)])
sigma <- apply(df[, -c(1, 2,3,4,12)], 2, sd)
descriptive<-round(cbind(Mu, sigma),2)
descriptive

dim(df)
skim(df)

#correlation plot
M<-cor(df[, -c(1, 2,3,4,12)]) #without categorical variables
M
corrplot(M, method = 'square', tl.col="black", order = "AOE")

plot(x=df$years_in_f1,y=df$races)

#=============================================================#
#                   UNSUPERVISED LEARNING                     #
#=============================================================#

########################## PCA #################################
#computing PCA
res.pca <- prcomp(df[, -c(1,2,3,4,12)],scale.=TRUE)

# New correlation matrix
pca_df <- data.frame(res.pca$x)
M_pca<-cor(pca_df)
corrplot(M_pca, method = 'circle', tl.col="black", order = "AOE")

# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

# Loadings
loadings <- res.pca$rotation
#write.csv(loadings,'loadings.csv')

# Results for Formula 1 players
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation

fviz_pca_ind(res.pca,
             col.ind = 'cos2', # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             max.overlaps =300 # Avoid text overlapping
)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("black", "#E7B800", "red"),
             repel = TRUE,
             max.overlaps =300 # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "red", # Variables color
                col.ind = "lightblue"  # Individuals color
)

fviz_pca_ind(res.pca,
             col.ind = df$team,
             #addEllipses = TRUE, # Concentration ellipses
             #ellipse.type = "confidence",
             legend.title = "team",
             repel = TRUE
)

########################## K-MEANS ###########################################

# K-Means
set.seed(4321)
df_kmeans<- as.data.frame(scale(df[, -c(1,2,3,4,12)])) 
#scaling data for clustering
#only numeric features

#optimal number of clusters 'ELBOW'
fviz_nbclust(df_kmeans, kmeans, nstart =5, method = "wss", linecolor='red') + theme_minimal()+
  geom_vline(xintercept = 4, linetype = 2) 

#optimal number of clusters 'SILHOUETTE'
fviz_nbclust(df_kmeans, kmeans, nstart = 5, method = "silhouette", linecolor='red') + theme_minimal()

kmeans_fit <- kmeans(df_kmeans, 4, nstart = 5)

# plot the clusters
fviz_cluster(kmeans_fit, data = df_kmeans,
             palette = c("#00AFBB", "#E7B800","#FC4E07", '#33CC33'),
             geom = c("point"),ellipse.type = "euclid",
             ggtheme = theme_minimal())

kmeans_table <- data.frame(kmeans_fit$size, kmeans_fit$centers)
kmeans_df <- data.frame(Cluster = kmeans_fit$cluster, df)

#plotting association with races, years, annual salary and number of podiums
kmeans_df$Cluster <- as.factor(kmeans_df$Cluster)

p1 <- ggplot(kmeans_df, aes(x=races, fill=Cluster)) +
  geom_bar(position="fill", alpha = 0.6) + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07", '#33CC33'))

p2 <- ggplot(kmeans_df, aes(x=years_in_f1, fill=Cluster)) +
  geom_bar(position="fill", alpha = 0.6) + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07", '#33CC33'))

p3 <- ggplot(kmeans_df, aes(x=avg_yearly_salary, fill=Cluster)) +
  geom_bar(position="fill",  alpha = 0.7) + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07", '#33CC33'))

p4 <- ggplot(kmeans_df, aes(x=podiums, fill=Cluster)) +
  geom_bar(position="fill",  alpha = 0.7) + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07", '#33CC33'))

p1
p2
p3
p4

######################### HIERARCHICAL CLUSTERING ##############################
df_clust <- df[,-c(1,2)] #without id and name
df_clust[,-c(1,2,10)]<-scale(df_clust[,-c(1,2,10)])
df_clust$country <- as.factor(df_clust$country)
df_clust$team <- as.factor(df_clust$team)
df_clust$fav_circuit <- as.factor(df_clust$fav_circuit)

library(cluster) 
gower.dist <- daisy(df_clust, metric = c("gower"))
#------------ AGGLOMERATIVE CLUSTERING ------------#
aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c,
     main = "Agglomerative, complete linkages")

library("dendextend")
dendro <- as.dendrogram(aggl.clust.c)
dendro.col <- dendro %>%
  set("branches_k_color", k = 4, value =   c("orange","red", "grey", "pink")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 4")+
  geom_hline(yintercept = 0.59, linetype = 3) 

# Radial plot looks less cluttered
ggplot(ggd1, labels = T) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")

df_clust$id <- df[,1] #adding id for categorical variables appearance in clusters calculations

#------------ HIT-MAP FOR CATEGORICAL VARIABLES IN CLUSTERS ------------#

clust.num <- cutree(aggl.clust.c, k = 4)
df_clust.cl <- cbind(df_clust, clust.num)
cust.long <- melt(data.frame(lapply(df_clust.cl, as.character), stringsAsFactors=FALSE), 
                  id = c("id", "clust.num"), factorsAsStrings=T)
cust.long.q <- cust.long %>%
  group_by(clust.num, variable, value) %>%
  mutate(count = n_distinct(id)) %>%
  distinct(clust.num, variable, value, count)
# calculating the percent of each factor level in the absolute count of cluster members
cust.long.p <- cust.long.q %>%
  group_by(clust.num, variable) %>%
  mutate(perc = count / sum(count)) %>%
  arrange(clust.num) #manually specifying all the levels for each factor
heatmap.p <- ggplot(cust.long.p, aes(x = clust.num, y = factor(value, levels = c('Australia', 'Finland', 'France', 'Germany', 'Great Britain', 'Italy', 'Netherlands', 'Spain', 'United States','APAC', 'EUROPE', 'LATAM', 'MEAST_AFR', 'NORTH_AMERICA','Alfa Romeo', 'AlphaTauri', 'Alpine', 'Aston Martin', 'Ferrari', 'Haas', 'McLaren', 'Mercedes', 'Red Bull', 'Williams'), ordered = T))) +
  
  geom_tile(aes(fill = perc), alpha = 0.85)+
  labs(title = "Distribution of characteristics across clusters", x = "Cluster number", y = NULL) +
  scale_fill_gradient2(low = "grey", mid = "orange", high = "red")
heatmap.p

#------------ ALL VARIABLES APPEARANCE IN CLUSTERS (GRAPH) ----------------------
library(GGally)
library(plotly)
p5 <- ggparcoord(data = df_clust.cl, columns = c(1:12), groupColumn = "clust.num", scale = "std") +
  theme(axis.text.x = element_text(angle = 90))+scale_colour_gradient(low = "red", high = "black")

ggplotly(p5)


#=============================================================#
#                    SUPERVISED LEARNING                      #
#=============================================================#

#--------------------- DATA PREPROCESSING ----------------------

#one-hot encoding for team an circuit variable
df$team <- as.factor(df$team)

df_reg<-df
df_reg <-df_reg %>% mutate(value = 1)  %>% spread(team, value,  fill = 0 ) 
df_reg <-df_reg %>% mutate(value = 1)  %>% spread(fav_circuit, value,  fill = 0 ) 

#---------------------- OUTLIERS DETECTION -----------------------------

df_reg_out <- df_reg[,-c(1,2,3)]
library(janitor)
df_reg <-clean_names(df_reg ) 
df_reg_out <-clean_names(df_reg_out ) 

draw_boxplot <- function(){ 
  df_reg_out[,-c(9:23)]%>%  
    pivot_longer(1:8, names_to="indicators") %>%  
    ggplot(aes(indicators, value, fill=indicators)) + 
    geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
}

draw_boxplot()

#Ouliers with an Isolation Forest

iforest <- isolationForest$new(sample_size = 200, num_trees = 500,replace = TRUE)
iforest
iforest$fit(df_reg_out)

scores_train = df_reg_out %>%
  iforest$predict() 

df_reg_out$iforest_anomaly_score <- scores_train$anomaly_score

df_reg_out[order(-df_reg_out$iforest_anomaly_score),]
skim(df_reg_out$iforest_anomaly_score)
quantile(df_reg_out$iforest_anomaly_score, probs = c(0.85)) #recommended in literature

df_reg_out$iforest_outlier <- as.factor(ifelse(df_reg_out$iforest_anomaly_score >=0.60, "outlier", "normal"))

outliers  <- df_reg_out[df_reg_out$iforest_outlier == 'outlier',]
rownames(outliers)
count(outliers)/count(df_reg)

df_clean <- subset(df_reg_out, iforest_outlier == 'normal')
df_clean <- df_clean[,-c(2,14,16,17,18,23,24,25)]

#-------------  corrplot for all the numerical variables (dummies included)-----
M1<-cor(df_reg[,-c(1,2,3)])
M1
corrplot(M1, type = 'lower', order = 'hclust', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45)

#============== TRAIN/TEST SPLIT FOR REGRESSION ================

#train-test split
set.seed(12345)
split_train_test <- createDataPartition(df_clean$avg_yearly_salary,p=0.7,list=FALSE)
train.data<- df_clean[split_train_test,]
test.data <-df_clean[-split_train_test,]

dim(train.data)
dim(test.data)

#========================== RANDOM FOREST ======================================

#Dataset will all the indicators and the observations - train/test split

set.seed(1234)
# training
rf1 <- randomForest(avg_yearly_salary ~ ., data = test.data, importance=TRUE)
rf1

plot(rf1)
grid (NULL,NULL, lty = 6, col = "cornsilk2") 

varImpPlot(rf1)

#Dataset will all the indicators and the observations - entire set
rf2 <- randomForest(avg_yearly_salary ~ ., data = df_clean, importance=TRUE)
rf2
plot(rf2)
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
varImpPlot(rf2)

#Dataset will all the observations and PCs - entire set
head(pca_df)
pca_df$avg_yearly_salary <- df_reg$avg_yearly_salary

rf3 <- randomForest(avg_yearly_salary ~., data = pca_df, importance=TRUE)
rf3
plot(rf3) 
grid (NULL,NULL, lty = 6, col = "cornsilk2") 
varImpPlot(rf3)

#========================== LASSO REGRESSION ======================================
library(glmnet)
x=model.matrix(avg_yearly_salary~.,df_clean)[,-1]
y=df_clean$avg_yearly_salary
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
grid=10^seq(10,-2,length=100)
y.test=y[test]

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod, label=TRUE)

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
R_square<-1-(sum((lasso.pred - y.test)^2))/(sum((y.test - mean(y.test))^2))
R_square
RMSE <- sqrt((sum((y.test - mean(y.test))^2)/nrow(as.data.frame(test))))
RMSE
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:17,]
lasso.coef
lasso.coef[lasso.coef!=0]
best_model <- glmnet(x, y, alpha = 1, lambda = bestlam)
coef(best_model)
#========================== LINEAR REGRESSION ======================================

#eliminate anomaly rows 40, 677
df_clean <- df_clean[-c(40,677),]
library("PerformanceAnalytics")
chart.Correlation(df_clean[,-c(8:20)], histogram=TRUE, pch=19)

#model with all the variables

mod<-lm(avg_yearly_salary~., data=df_clean)
summary(mod)

library(MASS)
step.model <- stepAIC(mod, direction = "both", 
                      trace = FALSE, k=3)
summary(step.model)
vif(step.model)

sqrt(vif(step.model)) > 2 # MULTICOLLINEARITY!
par(mfrow=c(2,2))
plot(step.model)

summary(mod$residuals)
library(ggpubr)
ggqqplot(step.model$residuals)

#================== LINEAR REGRESSION (SEVERAL FEATURES ELIMINATED) ===========

mod2<-lm(avg_yearly_salary~., data=df_clean[,-c(9,10,11)])
summary(mod2)

step.model <- stepAIC(mod2, direction = "both", 
                      trace = FALSE, k=3)
summary(step.model)
vif(step.model)

sqrt(vif(mod2)) > 2 # we don't have collinearity
par(mfrow=c(2,2))
plot(mod2)

summary(mod2$residuals)
library(ggpubr)
ggqqplot(step.model$residuals) #QQ-plot isn't normal?
dev.off() #resolves the problem with graph when occurs 

