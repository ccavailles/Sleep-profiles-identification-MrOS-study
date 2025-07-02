
#### SLEEP PROFILES IDENTIFICATION


## STEP 1 : PCA ----

PCA2 <- prcomp(cluster2, scale = TRUE) #cluster 2 is a dataset containing only the sleep variables
summary(PCA2)

library(factoextra)
fviz_eig(PCA2, addlabels = TRUE, ylim = c(0, 30)) # scree plot

fviz_eig(PCA2, 
         addlabels = TRUE, 
         choice="eigenvalue") +
  geom_hline(yintercept=1, 
             linetype="dashed", 
             color = "red")

eig.val <- get_eigenvalue(PCA2)
eig.val

PCA2_transform <- as.data.frame(-PCA2$x[,1:6]) #we kept 6 principal components



## STEP 2 : MCGHD ----

library(MixGHD)

res1 <- MCGHD(data=PCA2_transform, gpar0=NULL, G=1, max.iter=1000, eps=1e-2, label=NULL,
              method="kmedoids", scale=TRUE, nr=10, modelSel="AIC")

res2 <- MCGHD(data=PCA2_transform, gpar0=NULL, G=2, max.iter=1000, eps=1e-2, label=NULL,
              method="kmedoids", scale=TRUE, nr=10, modelSel="AIC")

res3 <- MCGHD(data=PCA2_transform, gpar0=NULL, G=3, max.iter=1000, eps=1e-2, label=NULL,
              method="kmedoids", scale=TRUE, nr=10, modelSel="AIC")

res4 <- MCGHD(data=PCA2_transform, gpar0=NULL, G=4, max.iter=1000, eps=1e-2, label=NULL,
              method="kmedoids", scale=TRUE, nr=10, modelSel="AIC")

res5 <- MCGHD(data=PCA2_transform, gpar0=NULL, G=5, max.iter=1000, eps=1e-2, label=NULL,
              method="kmedoids", scale=TRUE, nr=10, modelSel="AIC")


summary(res1)
summary(res2) 
summary(res3) #the best
summary(res4)
summary(res5)


g <- c('1','2','3',
       '4','5')


## BIC plot
BIC <- c(res1@BIC,res2@BIC,res3@BIC,res4@BIC,res5@BIC)

bic.data <- data.frame(g, BIC)
bic.data$g <- as.numeric(bic.data$g)

library(ggplot2)
ggplot(bic.data) +
  aes(x = g, y = BIC) +
  geom_line() +
  geom_point() +
  scale_x_discrete(limits=c("1", "2","3","4","5")) +
  xlab("Number of clusters") +
  ylab("BIC")


## AIC plot 
AIC <- c(res1@AIC,res2@AIC,res3@AIC,res4@AIC,res5@AIC)

aic.data <- data.frame(g, AIC)
aic.data$g <- as.numeric(aic.data$g)

library(ggplot2)
ggplot(aic.data) +
  aes(x = g, y = AIC) +
  geom_line() +
  geom_point() +
  scale_x_discrete(limits=c("1", "2","3","4","5")) +
  xlab("Number of clusters") +
  ylab("AIC")



## ICL plot 
ICL <- c(res1@ICL,res2@ICL,res3@ICL,res4@ICL,res5@ICL)

icl.data <- data.frame(g, ICL)
icl.data$g <- as.numeric(icl.data$g)

library(ggplot2)
ggplot(icl.data) +
  aes(x = g, y = ICL) +
  geom_line() +
  geom_point() +
  scale_x_discrete(limits=c("1", "2","3","4","5")) +
  xlab("Number of clusters") +
  ylab("ICL")



base <- as.data.frame(res3@map) %>% 
  rename("clust" = "res3@map")
tab1(base$clust)

