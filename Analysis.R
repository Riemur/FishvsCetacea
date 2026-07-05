#Analysis and plotting
library(ggplot2)
library(report)
library(ggpubr)
library(car)
library(dplyr)
library(rstatix)
library(broom)



#raw data, for graphing
data <- read.csv("Thesis_data.csv") %>%
  filter(Lengthmax.type == "TL") %>%
  arrange(rowSums(is.na(.))) %>%        #this bit arranges rows based on how many NAs!
  distinct(Genus, .keep_all = TRUE) %>%  #then keeps row per genus with most data
  mutate(Rel.Favg=Favg/Wmax) %>%          #Put Favg and Loffspring relative (unused!)
  mutate(Rel.Loffspring=Loffspring/Wmax)  
  
#log and z transformed data, for analysis
#data_analysis <- read.csv("Thesis_data.csv") %>%
#  filter(Lengthmax.type == "TL") %>%
#  arrange(rowSums(is.na(.))) %>%        
#  distinct(Genus, .keep_all = TRUE) %>%
#  mutate_if(is.numeric, log) %>% #transformations to log and standardised 
#  mutate_if(is.numeric,scale)
#not transformed, seems more logical for interpretation

data_analysis <- read.csv("Thesis_data.csv") %>%  #creates dataset for analysis, is log transformed
  filter(Lengthmax.type == "TL") %>%
  arrange(rowSums(is.na(.))) %>%        
  distinct(Genus, .keep_all = TRUE) %>%
  mutate(Rel.Favg=Favg/Wmax) %>%              #relative values unused!
  mutate(Rel.Loffspring=Loffspring/Wmax)%>%
  mutate_if(is.numeric, log) 

#%>% #transformations to log and standardised 
#  mutate_if(is.numeric,scale)

#function graphs
Reg.graphs <- function(trait, trait_name, R, RC, RCC) {
  #trait_name <- deparse(substitute(trait))
  
  P <- ggplot(data, aes(x=log(Wmax), y=log(trait), )) +
    geom_point(aes(colour = Category)) +
    geom_smooth(method = "lm") +
    labs(y = paste0("log(", trait_name, ")"))+
    ggtitle("A) No Clade Influence") +
    theme(plot.title = element_text(size = 7))
  
  aug_RC <- augment(RC)
  PC <- ggplot(data , mapping=aes(x=log(Wmax), y=log(trait), color=Category)) + 
    geom_point() + 
    geom_line(data=aug_RC, mapping=aes(x=Wmax, y=.fitted, color=Category))+
    labs(y = paste0("log(", trait_name, ")")) +
    ggtitle("B) Clade Incluences Intercept") +
    theme(plot.title = element_text(size = 7))
  
  PCC <- ggplot(data, aes(x=log(Wmax), y=log(trait), color = Category)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(y = paste0("log(", trait_name, ")"))+
    ggtitle("C) Clade Incluences Slope") +
    theme(plot.title = element_text(size = 7))
  
  
  ggarrange(P, PC, PCC, common.legend = TRUE) +
    ggtitle(paste0("Regressions ", trait_name, " ~ Max Weight (Wmax) ", "n=", nrow(model.frame(R)))) +
    theme(plot.title = element_text(size = 10, face="bold"))
  
}

###assumption testing
#transformed log data
SWmax <- shapiro.test(data_analysis$Wmax) #weight is nicely normally distributed
ggplot(data_analysis, aes(x=Wmax)) +
  xlab("log(Wmax)") +
  geom_histogram(aes(color=Category)) +
  annotate("text", y=18, x=9, size=4, label = paste0("Shapiro-Wilk P.value=", SWmax$p.value, ", n=", nrow(data.table(data_analysis$Wmax)))) +
  ggtitle(paste0("Distribution of Maximum Weight in the dataset"))
 

###regressions###regressionsTRUE
##Lmax
#NO category effect
RLmax <- lm(Lmax ~ Wmax, data = data_analysis) 
  summary(RLmax)
  report(RLmax)
#plus category
RLmaxC <- lm(Lmax ~ Wmax + Category, data = data_analysis) 
summary(RLmaxC)
report(RLmaxC)
#times category
RLmaxCC <- lm(Lmax ~ Wmax*Category, data = data_analysis) 
  summary(RLmaxCC)
  report(RLmaxCC)
  
Reg.graphs(data$Lmax, "Maximum Length" , RLmax, RLmaxC, RLmaxCC)
  
 ##Ymax
#NO category effect
RYmax <- lm(Ymax ~ Wmax, data = data_analysis) 
  summary(RYmax)
  report(RYmax)
#plus category
RYmaxC <- lm(Ymax ~ Wmax+Category, data = data_analysis) 
  summary(RYmaxC)
  report(RYmaxC)
#times category
RYmaxCC <- lm(Ymax ~ Wmax*Category, data = data_analysis) 
  summary(RYmaxCC)
  report(RYmaxCC)
  
Reg.graphs(data$Ymax, "Longevity", RYmax, RYmaxC, RYmaxCC)


  ##Lm
#NO category effect
RLm <- lm(Lm ~ Wmax, data = data_analysis) 
  summary(RLm)
  report(RLm)
#plus category
RLmC <- lm(Lm ~ Wmax+Category, data = data_analysis) 
  summary(RLmC)
  report(RLmC)
#times category
RLmCC <- lm(Lm ~ Wmax*Category, data = data_analysis) 
  summary(RLmCC)
  report(RLmCC)
  
Reg.graphs(data$Lm, "Length at Maturation", RLm, RLmC, RLmCC)

  ##Ym
#NO category effect
RYm <- lm(Ym ~ Wmax, data = data_analysis) 
  summary(RYm)
  report(RYm)
#plus category
RYmC <- lm(Ym ~ Wmax+Category, data = data_analysis) 
  summary(RYmC)
  report(RYmC)
  exp(RYmC[["coefficients"]][["CategoryTeleostei"]])
#times category
RYmCC <- lm(Ym ~ Wmax*Category, data = data_analysis) 
  summary(RYmCC)
  report(RYmCC)
  
Reg.graphs(data$Ym, "Age at Maturation", RYm, RYmC, RYmCC)
#exp(RLmC[["coefficients"]][["CategoryTeleostei"]])

##Favg
RFavg <- lm(Favg ~ Wmax, data = data_analysis) 
summary(RFavg)
report(RFavg)
#plus category
RFavgC <- lm(Favg ~ Wmax+Category, data = data_analysis) 
summary(RFavgC)
report(RFavgC)
#times category
RFavgCC <- lm(Favg ~ Wmax*Category, data = data_analysis) 
summary(RFavgCC)
report(RFavgCC)

Reg.graphs(data$Favg, "Fecundity", RFavg, RFavgC, RFavgCC) 


##Loffspring
#NO category effect
RLoffspring <- lm(Loffspring ~ Wmax, data = data_analysis) 
summary(RLoffspring)
report(RLoffspring)
#plus category
RLoffspringC <- lm(Loffspring ~ Wmax+Category, data = data_analysis) 
summary(RLoffspringC)
report(RLoffspringC)
#times category
RLoffspringCC <- lm(Loffspring ~ Wmax*Category, data = data_analysis) 
summary(RLoffspringCC)
report(RLoffspringCC)

Reg.graphs(data$Loffspring, "Offspring Length", RLoffspring, RLoffspringC, RLoffspringCC)


###########redundant 
 ###Favg, relative fecundity
#NO category effect
#RFavg <- lm(Rel.Favg ~ Wmax, data = data_analysis) 
#  summary(RFavg)
#  report(RFavg)
#plus category
#RFavgC <- lm(Rel.Favg ~ Wmax+Category, data = data_analysis) 
#  summary(RFavgC)
#  report(RFavgC)
#times category
#RFavgCC <- lm(Rel.Favg ~ Wmax*Category, data = data_analysis) 
#  summary(RFavgCC)
#  report(RFavgCC)
  
#Reg.graphs(data$Rel.Favg, RFavg, RFavgC, RFavgCC) 
#maybe fecundity relative to Wmax not the play? maybe relative to Loffspring

##rel.Loffspring
#NO category effect
#RLoffspring <- lm(Rel.Loffspring ~ Wmax, data = data_analysis) 
#  summary(RLoffspring)
#  report(RLoffspring)
#plus category
#RLoffspringC <- lm(Rel.Loffspring ~ Wmax+Category, data = data_analysis) 
#  summary(RLoffspringC)
#  report(RLoffspringC)
#times category
#RLoffspringCC <- lm(Rel.Loffspring ~ Wmax*Category, data = data_analysis) 
#  summary(RLoffspringCC)
#  report(RLoffspringCC)
  
#Reg.graphs(data$Rel.Loffspring, RLoffspring, RLoffspringC, RLoffspringCC) 




##Loffspring/lmax? -> relative offspring size!!!!
#ggplot(data, aes(x=log(Wmax), y=log(Loffspring/Wmax), colour = Category)) +
#  geom_point() +
#  geom_smooth(method = "lm") +
#  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")), 
#           colour = "darkgrey", size=3)


#ggplot(data, aes(x=log(Wmax), y=log(Favg/Wmax), colour = Category)) +
#  geom_point() +
#  geom_smooth(method = "lm") +
#  stat_cor(aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~")), 
#           colour = "darkgrey", size=3)
 