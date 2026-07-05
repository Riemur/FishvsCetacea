#Plotting
library(ggplot2)
library(report)
library(ggpubr)
library(car)
library(dplyr)
library(rstatix)
library(broom)

##regression plots Lmax
#RLmax
Reg.graphs(data$Lmax, "Maximum Length", RLmax, RLmaxC, RLmaxCC)

#RYmax

## combination regression plots Wmax

PLmax <- ggplot(data, aes(x=log(Wmax), y=log(Lmax))) +
  geom_point(aes(colour = Category)) #+
 # geom_smooth(method = "lm") +
 # stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")), 
#           colour = "darkgrey", size=3)


PYmax <- ggplot(data, aes(x=log(Wmax), y=log(Ymax))) +
  geom_point(aes(colour = Category)) #+
#  geom_smooth(method = "lm") +
#  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")), 
#           colour = "darkgrey", size=3)


PYm <- ggplot(data, aes(x=log(Wmax), y=log(Ym))) +
  geom_point(aes(colour = Category)) #+
#  geom_smooth(method = "lm") +
#  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")), 
#           colour = "darkgrey", size=3)


PLm <- ggplot(data, aes(x=log(Wmax), y=log(Lm))) +
  geom_point(aes(colour = Category)) #+
 # geom_smooth(method = "lm") +
 # stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")), 
#           colour = "darkgrey", size=3)

PFavg <- ggplot(data, aes(x=log(Wmax), y=log(Favg))) +
  geom_point(aes(colour = Category)) #+
 # geom_smooth(method = "lm") +
#  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")), 
#           colour = "darkgrey", size=3)


PLoffspring <- ggplot(data, aes(x=log(Wmax), y=log(Loffspring))) +
  geom_point(aes(colour = Category)) #+
 # geom_smooth(method = "lm") +
#  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")), 
#           colour = "darkgrey", size=3)


ggarrange(PLmax, PYmax, PYm, PLm, PFavg, PLoffspring, common.legend = TRUE) +
  ggtitle("Full Life-History Dataset, as a Function of Body size") +
  theme(plot.title = element_text(size = 10, face="bold"))


#Regressions WmaxC
aug_RLmC <- augment(RLmC)

#ggplot(data , mapping=aes(x=log(Wmax), y=log(Loffspring), color=Category)) + 
#  geom_point() # + 
 # geom_line(data=aug_RLmC, mapping=aes(x=Lm, y=.fitted, color=Category))

#presentation plots
PPLmax <- ggplot(data, aes(x=log(Wmax), y=log(Lmax), )) +
  geom_point(aes(colour = Category)) +
  geom_smooth(method = "lm") +
  ggtitle("No Clade Effect") +
  theme(plot.title = element_text(size = 7))

PPYmax <- ggplot(data, aes(x=log(Wmax), y=log(Ymax), )) +
  geom_point(aes(colour = Category)) +
  geom_smooth(method = "lm") + 
  ggtitle("No Clade Effect") +
  theme(plot.title = element_text(size = 7))
  


aug_RLmC <- augment(RLmC)
PPLmC <- ggplot(data , mapping=aes(x=log(Wmax), y=log(Lm), color=Category)) + 
  geom_point() + 
  geom_line(data=aug_RLmC, mapping=aes(x=Wmax, y=.fitted, color=Category))+
  labs(y = paste0("log(Lm)")) +
  ggtitle("Clade influences intercept") +
  theme(plot.title = element_text(size = 7))


aug_RYmC <- augment(RYmC)
PPYmC <- ggplot(data , mapping=aes(x=log(Wmax), y=log(Ym), color=Category)) + 
  geom_point() + 
  geom_line(data=aug_RYmC, mapping=aes(x=Wmax, y=.fitted, color=Category))+
  labs(y = paste0("log(Ym)")) +
  ggtitle("Clade influences intercept") +
  theme(plot.title = element_text(size = 7))


PPFavgCC <- ggplot(data, aes(x=log(Wmax), y=log(Favg), color = Category)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(y = paste0("log(Fecundity)")) +
  ggtitle("Clade influences slope") +
  theme(plot.title = element_text(size = 7))

aug_RLoffspringC <- augment(RLoffspringC)
PPLoffC <- ggplot(data , mapping=aes(x=log(Wmax), y=log(Loffspring), color=Category)) + 
  geom_point() + 
  geom_line(data=aug_RLoffspringC, mapping=aes(x=Wmax, y=.fitted, color=Category))+
  labs(y = paste0("log(Offspring Length)")) +
  ggtitle("Clade influences intercept") +
  theme(plot.title = element_text(size = 7))

ggarrange(PPLmax,PPYmax, PPLmC,PPYmC, PPFavgCC,PPLoffC, common.legend = TRUE) +
  ggtitle("Best fit linear model per trait, as a function of body size (Wmax)") +
  theme(plot.title = element_text(size = 10, face="bold"))




ggplot(data, aes(x=log(Wmax), y=log(Loffspring),label=Common.Name)) + 
  geom_point() +
  geom_smooth(method="lm")#+
  #geom_text() #+
 # ylim(0,0.05)

lm(Loffspring ~ Wmax, data = data_analysis) 
summary(RLmax)
report(RLmax)
#plus category
RLmaxC <- lm(Loffspring ~ Wmax + Category, data = data_analysis) 
summary(RLmaxC)
report(RLmaxC)
#times category
RLmaxCC <- lm(Loffspring ~ Wmax*Category, data = data_analysis) 
summary(RLmaxCC)
report(RLmaxCC)


##normalities for all data
SWmax <- shapiro.test(data_analysis$Wmax) 
PSWmax <- ggplot(data_analysis, aes(x=Wmax, fill=Category)) +
  geom_histogram(aes(color = Category), alpha = 0.5) +
  annotate("text", y=15, x=7, size=2, label = paste0("Shapiro-Wilk P.value=", SWmax$p.value))

SLmax <- shapiro.test(data_analysis$Lmax) 
PSLmax <- ggplot(data_analysis, aes(x=Lmax, fill=Category)) +
  geom_histogram(aes(color = Category), alpha = 0.5) +
  annotate("text", y=15, x=5, size=2, label = paste0("Shapiro-Wilk P.value=", SLmax$p.value))

SYmax <- shapiro.test(data_analysis$Ymax) 
PSYmax <- ggplot(data_analysis, aes(x=Ymax, fill=Category)) +
  geom_histogram(aes(color = Category), alpha = 0.5) +
  annotate("text", y=15, x=2, size=2, label = paste0("Shapiro-Wilk P.value=", SYmax$p.value))

SLm <- shapiro.test(data_analysis$Lm) 
PSLm <- ggplot(data_analysis, aes(x=Lm, fill=Category)) +
  geom_histogram(aes(color = Category), alpha = 0.5) +
  annotate("text", y=15, x=4, size=2, label = paste0("Shapiro-Wilk P.value=", SLm$p.value))

SYm <- shapiro.test(data_analysis$Ym) 
PSYm <- ggplot(data_analysis, aes(x=Ym, fill=Category)) +
  geom_histogram(aes(color = Category), alpha=0.5) +
  annotate("text", y=15, x=1, size=2, label = paste0("Shapiro-Wilk P.value=", SYm$p.value))

SFavg <- shapiro.test(data_analysis$Favg) 
PSFavg <- ggplot(data_analysis, aes(x=Favg, fill=Category)) +
  geom_histogram(aes(color = Category), alpha=0.5) +
  annotate("text", y=15, x=10, size=2, label = paste0("Shapiro-Wilk P.value=", SFavg$p.value))

SLoff <- shapiro.test(data_analysis$Loffspring) 
PSLoff <- ggplot(data_analysis, aes(x=Loffspring, fill=Category)) +
  geom_histogram(aes(color = Category), alpha=0.5) +
  annotate("text", y=6, x=2, size=2 , label = paste0("Shapiro-Wilk P.value=", SLoff$p.value))

ggarrange(PSWmax, PSLmax, PSYmax, PSLm, PSYm, PSFavg, PSLoff, common.legend=TRUE)

## PCA biplots arrange

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
  ggtitle("% variation explained by PC1 and PC2") +
  labs(colour = "Clade", subtitle = "A)") 


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
  ggtitle(" ")+
  labs(colour = "Clade", subtitle = "B)") 

