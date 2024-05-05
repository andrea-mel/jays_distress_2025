rm(list=ls())

# load packages:
library(ggpubr) # for correlation plots
library(ordinal) # for CLMM
library(emmeans) # for CLMM comparisons

# load data:
mydata <- read.csv("mydata.csv", sep = ";")

# Sample size per each treatment
nE <- length(which(mydata$treatment=="distress familiar")) # 27
nC <- length(which(mydata$treatment=="distress unfamiliar")) # 24
nP <- length(which(mydata$treatment=="pine grosbeak")) # 24
nS <- length(which(mydata$treatment=="social unfamiliar")) # 24

# figure XY: response category vs social bond:
ggscatter(mydata[which(mydata$treatment=="distress familiar"),], x = "OBS_PercEatDyad", y = "category", add = "reg.line",
          title = "",
          conf.int = TRUE, cor.method = "spearman", # spearman for one continuoes and one ordinal variable
          # cor.coef = TRUE, 
          xlab = "dyadic social unfamiliar bond", ylab = "Response strength") +
  geom_count()+# bond: eating in dyad [% of time eating]
  scale_size_area()+
  theme(legend.direction = "vertical", 
        legend.position = "right",
        legend.box = "horizontal")

#- Cumulative link mixed models - what affects an individual’s probability and strength of helping behaviour?
mydata$category <- as.factor(mydata$category)
mydata$target.ID <- as.factor(mydata$target.ID)

modelD <- clmm(category ~ treatment + (1|target.ID), data = mydata) # category = 0-4 "response strength", treatment = playback-type

summary(modelD) # individual effects: tests null hypothesis that each coefficient is equal to zero

(pairwise <- emmeans(modelD, pairwise~treatment)) # to see differences for each pair of treatment: comparing the EMMs of the treatment levels against each other

emmeansTab <- as.data.frame(pairwise$contrasts)[,-4]

# table XY:
emmeansTab[,c(2:5)] <- round(emmeansTab[,c(2:5)], digits=3)

# to prepare corresponding plot:
newdf <- matrix(ncol=4,nrow=5) # table for percentages
colnames(newdf) <- c("distress familiar","distress unfamiliar","social unfamiliar","pine grosbeak")
rownames(newdf) <- c(4,3,2,1,0) # categories
for (i in 1:ncol(newdf)){
  tmp_name <- colnames(newdf)[i]
  tmp_data <- mydata[which(mydata$treatment==tmp_name),]
  tmp_length <- length(which(!is.na(tmp_data$category)))
  for (j in 1:nrow(newdf)){
    tmp_cat <- as.integer(rownames(newdf)[j])
    tmp_count <- length(which(tmp_data$category==tmp_cat))
    newdf[j,i] <- tmp_count/tmp_length
  }
}
newdf <- as.data.frame(newdf)
newdf$category <- rownames(newdf)
newdf_long <- reshape2::melt(newdf)
names(newdf_long) <- c("category","treatment", "value")
newdf_long$category <- as.factor(newdf_long$category)

# figure XY: treatment vs. reponse category
ggplot(newdf_long, aes(x = treatment, y = value, fill = category)) +
  geom_bar(stat = "identity", position = "fill", color = "white", width = 0.7) +
  scale_fill_manual(values = rev(c("#141c44","#042d94","steelblue","lightskyblue2","azure2")),
                    labels = c("none", "low", "medium", "high", "maximal")) +
  labs(x = "Treatment", y = "Proportion of response", title = "") +
  scale_x_discrete(labels = c("distress \nfamiliar", "distress \nunfamiliar", "social unfamiliar \nunfamiliar", "pine \ngrosbeak")) +
  theme_classic()+
  theme(legend.position = "right",
        axis.text.x = element_text(size = 11), 
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 12)),
        axis.title.y = element_text(margin = margin(r = 12)),
        legend.text = element_text(size = 11)) + 
  guides(fill = guide_legend(title = NULL)) +
  annotate("text", x = 1.5, y = 0.93, label = "ns", size = 4, color = "black") + # distress unfamiliar - distress familiar # ns, p=0.9995
  geom_segment(aes(x = 1, y = 0.9, xend = 2, yend = 0.9)) +
  annotate("text", x = 2.5, y = 0.86, label = "*", size = 4.8, color = "black") + # distress unfamiliar - social unfamiliar # p = 0.0131
  geom_segment(aes(x = 2, y = 0.84, xend = 3, yend = 0.84)) +
  annotate("text", x = 2, y = 0.98, label = "*", size = 4.8, color = "black") + # distress familiar - social unfamiliar # p = 0.0114
  geom_segment(aes(x = 1, y = 0.96, xend = 3, yend = 0.96)) +
  annotate("text", x = 4, y = 0.2, label = "**", size = 4.8, color = "black") + # pine grosbeak to all others p = <0.005
  annotate("text", x = 1, y = 0.04, label = nE, size = 3, color = "white") +
  annotate("text", x = 2, y = 0.04, label = nC, size = 3, color = "white")+
  annotate("text", x = 3, y = 0.04, label = nS, size = 3, color = "white")+
  annotate("text", x = 4, y = 0.04, label = nP, size = 3, color = "white")


### compare status: breeder vs. non-breeder:
newB <- matrix(ncol=2,nrow=5) # table for percentages
colnames(newB) <- c("breeder","non-breeder")
rownames(newB) <- c(4,3,2,1,0) # categories

br <- mydata[which(mydata$treatment=="distress familiar"&mydata$status_caller=="br"),]
nbr <- mydata[which(mydata$treatment=="distress familiar"&mydata$status_caller=="non-breeder"),]

for (j in 1:nrow(newB)){
  tmp_cat <- as.integer(rownames(newB)[j])
  br_count <- length(which(br$category==tmp_cat))
  newB[j,1] <- br_count/nrow(br)
  nbr_count <- length(which(nbr$category==tmp_cat))
  newB[j,2] <- nbr_count/nrow(nbr)
}

newB_long <- reshape2::melt(newB)
names(newB_long) <- c("category","status", "value")
newB_long$category <- as.factor(newB_long$category)

# figure XY: response category vs status
ggplot(newB_long, aes(x = status, y = value, fill = category)) +
  geom_bar(stat = "identity", position = "fill", color = "white", width = 0.7) +
  scale_fill_manual(values = rev(c("#141c44","#042d94","steelblue","lightskyblue2","azure2"))) +
  labs(x = "Status of caller", y = "Proportion of response", title = "") +
  scale_x_discrete(labels = c("breeder","non-breeder")) +
  theme_classic()+
  theme(legend.position = "none", 
    axis.text.x = element_text(size = 11), 
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),)+
  geom_segment(aes(x = 1, y = 0.95, xend = 2, yend = 0.95))+
  annotate("text", x = 1.5, y = 1, label = "ns", size = 4, color = "black")+
  annotate("text", x = 1, y = 0.07, label = "5", size = 4, color = "white")+
  annotate("text", x = 2, y = 0.07, label = "22", size = 4, color = "white")

# Fisher's non-parametric excact test:

newBCount <- newB
for (j in 1:nrow(newBCount)){
  tmp_cat <- as.integer(rownames(newBCount)[j])
  br_count <- length(which(br$category==tmp_cat))
  newBCount[j,1] <- br_count
  nbr_count <- length(which(nbr$category==tmp_cat))
  newBCount[j,2] <- nbr_count
}

# Perform Fisher's exact test on the entire table 5x2
(fisher_test_result <- fisher.test(newBCount)) # p=0.88 -- not enough evidence to reject the null hypothesis (H0 = there is no difference) -- no statistically significant association between the categories and the different status






#-------  Supplemental material: -----
#- PCA on helping behaviors only ------
library(psych) # for PCA

# remove NA: (only one NA for reaction_duration, this behavior is only used for PCA)
mydataN <- mydata[-which(is.na(mydata[,"reaction_duration"])),]

# scale dataset: formula of scale: xscaled = (xoriginal – mean(x)) / sd
subsetJayS <- scale(mydataN[,5:9])


# PCA: ----------
# scree plot:
scree(subsetJayS) # suggested: take 2 factors

# table XY: PCA results
(myPCA <- psych::principal(subsetJayS, nfactors = 2, rotate = "varimax", scores = T))

# label components:
colnames(myPCA$scores)[1] <- 'DIS'
colnames(myPCA$scores)[2] <- 'other'
mydataN <- cbind(mydataN,myPCA$scores) 

# figure XY:
box <- boxplot(DIS~treatment, data=mydataN, ylab = "PC1: helping", main=NA,
               names=c("distress \nunfamiliar","distress \nfamiliar","pine \ngrosbeak","social \nunfamiliar"),
               frame = FALSE, 
               col = c("#E69F00", "firebrick2", "#999999", "#56B4E9"))
stripchart(DIS~treatment, data=mydataN, method = "jitter", vertical=T, add=T, pch=16, cex=0.8)

#- ANOVA for treatments (after PCA)------
mydataN$treatment <- as.factor(mydataN$treatment)
mydataN$target.ID <- as.factor(mydataN$target.ID)

ano <- aov(DIS ~ treatment, data = mydataN) # ANOVA

# table XY: Tukey test of ANOVA
(tukey <- TukeyHSD(ano)) # Tukey-test of ANOVA (to get p-vals)


# figure XY: PCA vs social bond
ggscatter(mydataN[which(mydataN$treatment=="distress familiar"),], x = "OBS_PercEatDyad", y = "DIS", add = "reg.line",
          title = "",
          conf.int = TRUE, cor.method = "spearman", # spearman for one continuoes and one ordinal variable
          cor.coef = TRUE,
          xlab = "dyadic social unfamiliar bond", ylab = "PC1: helping")

