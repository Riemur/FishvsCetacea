library(ggplot2)
library(report)
library(ggpubr)
library(car)
library(dplyr)
library(rstatix)
library(broom)
library(dplyr)
library(data.table)

#special thanks to StatQuest (https://www.youtube.com/@statquest) for the basis of the PCA script

##PCA code

data_complete <- read.csv("Thesis_data.csv") %>% 
  filter(Lengthmax.type == "TL") %>%
  arrange(rowSums(is.na(.))) %>%        
  distinct(Genus, .keep_all = TRUE) %>%
  select(GenusSpecies, Category, Wmax, Ymax, Lmax, Lm, Ym, Favg, Loffspring) %>% 
  na.omit() %>%              #only complete rows for PCA
  mutate_if(is.numeric, log) 


shapiro.test(data_complete$Wmax) #Weight is still normally distributed
ggplot(data_complete, aes(x=Wmax)) +
  geom_histogram()


#####PCA on whole dataset
data_complete_pca <- data_complete %>%
  select(Wmax, Ymax, Lmax, Lm, Ym, Favg, Loffspring)
PCA.raw <- prcomp(data_complete_pca, scale=TRUE)

PCA.category.raw <- data.frame(Sample=rownames(PCA.raw$x), data_complete$Category)

# compute total variance
variance = PCA.raw $sdev^2 / sum(PCA.raw $sdev^2)

# Scree plot
qplot(c(1:7), variance) + 
  geom_col()+
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("% of variance predicted by per PC in raw data") +
  ylim(0, 1) 

## now make a fancy looking plot that shows the PCs and the variation:

pca.var <- PCA.raw$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

pca.data <- data.frame(Sample=rownames(PCA.raw$x),
                       X=PCA.raw$x[,1],
                       Y=PCA.raw$x[,2]) %>%
  left_join(PCA.category.raw, by="Sample")
pca.data

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample, colour=data_complete.Category)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("% variation explained by PC1 and PC2 for raw data") +
  labs(colour = "Clade", subtitle = "A)") 

## get the name of the top 10 measurements (genes) that contribute
##pc1.
loading_scores <- PCA.raw$rotation[,1]

Trait_scores <- abs(loading_scores) ## get the magnitudes
Trait_score_ranked <- sort(Trait_scores, decreasing=TRUE)
top_traits <- names(Trait_score_ranked)

top_traits

PCA.raw$rotation[top_traits,1] ## show the scores (and +/- sign)

#pc2
loading_scores <- PCA.raw$rotation[,2]

Trait_scores <- abs(loading_scores) ## get the magnitudes
Trait_score_ranked <- sort(Trait_scores, decreasing=TRUE)
top_traits <- names(Trait_score_ranked)

top_traits

PCA.raw$rotation[top_traits,2] ## show the scores (and +/- sign)


#####PCA on body size corrected residuals
##getting residuals OF THE SUBSET not predicted by body size
RLmaxPCA <- lm(Lmax ~ Wmax, data = data_complete) 
RYmaxPCA <- lm(Ymax ~ Wmax, data = data_complete)
RLmPCA <- lm(Lm ~ Wmax, data = data_complete)
RYmPCA <- lm(Ym ~ Wmax, data = data_complete)
RFavgPCA <- lm(Favg ~ Wmax, data = data_complete)
RLoffPCA <- lm(Loffspring ~ Wmax, data = data_complete)


PCA.res.data <- data.frame(RLmaxPCA$residuals, RYmaxPCA$residuals, RLmPCA$residuals, RYmPCA$residuals, RFavgPCA$residuals, RLoffPCA$residuals) #category not included bc prcomp is being dramatic

PCA.res <- prcomp(PCA.res.data, scale=TRUE) 

PCA.category <- data.frame(Sample=rownames(PCA.res$x), data_complete$Category, PCA.res.data)


# compute total variance
variance = PCA.res $sdev^2 / sum(PCA.res $sdev^2)

# Scree plot
qplot(c(1:6), variance) + 
  geom_col()+
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("% variation explained by PC1 and PC2 for body-size corrected data") +
  ylim(0, 1)

## now make a fancy looking plot that shows the PCs and the variation:
pca.var <- PCA.res$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

pca.data <- data.frame(Sample=rownames(PCA.res$x),
                       X=PCA.res$x[,1],
                       Y=PCA.res$x[,2]) %>%
  left_join(PCA.category, by="Sample")
pca.data

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample, colour=data_complete.Category)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("% variation explained by PC1 and PC2 for body-size corrected data")+
  labs(colour = "Clade", subtitle = "B)") 



## get the name of the top 10 measurements (genes) that contribute
##pc1.
loading_scores <- PCA.res$rotation[,1]

Trait_scores <- abs(loading_scores) ## get the magnitudes
Trait_score_ranked <- sort(Trait_scores, decreasing=TRUE)
top_traits <- names(Trait_score_ranked)



PCA.res$rotation[top_traits,1] ## show the scores (and +/- sign)

#pc2
loading_scores <- PCA.res$rotation[,2]

Trait_scores <- abs(loading_scores) ## get the magnitudes
Trait_score_ranked <- sort(Trait_scores, decreasing=TRUE)
top_traits <- names(Trait_score_ranked)



PCA.res$rotation[top_traits,2] ## show the scores (and +/- sign)

###redundant
##getting residuals for our traits from best fit model
#Not right, new plan is to make seperate residuals for data_complete
#PCA.res.Lmax <- data.frame(data_complete$Lmax - predict(RLmax, newdata = data_complete))
#PCA.res.Ymax <- data.frame(data_complete$Ymax - predict(RYmax, newdata = data_complete))
#PCA.res.Lm <- data.frame(data_complete$Lm - predict(RLmC, newdata = data_complete))
#PCA.res.Ym <- data.frame(data_complete$Ym - predict(RYmC, newdata = data_complete))
#PCA.res.rFavg <- data.frame(data_complete$Favg - predict(RFavgCC, newdata = data_complete))
#PCA.res.rLoff <- data.frame(data_complete$Loffspring - predict(RLoffspringC, newdata = data_complete))
#PCA.category <- data.frame(Sample=rownames(pca$x), data_complete$Category)

#PCA.res.data <- data.frame(PCA.res.Lmax, PCA.res.Ymax, PCA.res.Lm, PCA.res.Ym, PCA.res.rFavg, PCA.res.rLoff) #category not included bc prcomp is being dramatic