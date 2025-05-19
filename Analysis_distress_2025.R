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
nS <- length(which(mydata$treatment=="social unfamiliar")) # 26

#------------------------------------------------------------------------------
# --- for Main text -------
#------------------------------------------------------------------------------

###################
#- Cumulative link mixed models:
mydata$category <- as.factor(mydata$category)
mydata$target.ID <- as.factor(mydata$target.ID)

modelD <- clmm(category ~ treatment + (1|target.ID), data = mydata) # category = 0-4 "response strength", treatment = playback-type

summary(modelD) # individual effects: tests null hypothesis that each coefficient is equal to zero

(pairwise <- emmeans(modelD, pairwise~treatment)) # to see differences for each pair of treatment: comparing the EMMs of the treatment levels against each other

emmeansTab <- as.data.frame(pairwise$contrasts)[,-4]

#------- Table 1: 
# Tukey's pairwise comparisons of the output of CLMM
(emmeansTab[,c(2:5)] <- round(emmeansTab[,c(2:5)], digits=3))


###################
# to prepare plot:
newdf <- matrix(ncol=4,nrow=6) # empty table for percentages
colnames(newdf) <- c("distress familiar","distress unfamiliar","social unfamiliar","pine grosbeak") # treatments
rownames(newdf) <- c(5,4,3,2,1,0) # categories

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
newdf_long <- reshape2::melt(newdf) # to re-format
names(newdf_long) <- c("category","treatment", "value")
newdf_long$category <- as.factor(newdf_long$category)

#------- Figure 4 a): 
# Response of focal individuals, measured in ordinal categories # different playback treatments
ggplot(newdf_long, aes(x = treatment, y = value, fill = category)) +
  geom_bar(stat = "identity", position = "fill", color = "white", width = 0.7) +
  scale_fill_manual(values = rev(c("black","#00254d","#042d94","#6FAECB","lightblue2","azure2")),
                    labels = c("none", "minimal", "low", "medium", "high", "maximal")) +
  labs(x = "Treatment", y = "Proportion of response", title = "") +
  scale_x_discrete(labels = c("distress \nfamiliar", "distress \nunfamiliar", "social \nunfamiliar", "pine \ngrosbeak")) +
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
  annotate("text", x = 2.5, y = 0.86, label = "*", size = 4.8, color = "black") + # distress unfamiliar - social unfamiliar # p = 0.0103
  geom_segment(aes(x = 2, y = 0.84, xend = 3, yend = 0.84)) +
  annotate("text", x = 2, y = 0.98, label = "*", size = 4.8, color = "black") + # distress familiar - social unfamiliar # p = 0.0091
  geom_segment(aes(x = 1, y = 0.96, xend = 3, yend = 0.96)) +
  annotate("text", x = 4, y = 0.2, label = "**", size = 4.8, color = "black") + # pine grosbeak to all others p = <0.005
  annotate("text", x = 1, y = -0.04, label = "n=27", size = 3, color = "black") +
  annotate("text", x = 2, y = -0.04, label = "n=24", size = 3, color = "black")+
  annotate("text", x = 3, y = -0.04, label = "n=26", size = 3, color = "black")+
  annotate("text", x = 4, y = -0.04, label = "n=24", size = 3, color = "black")


###################
### compare status: breeder vs. non-breeder
# to prepare plot:
newB <- matrix(ncol=2,nrow=6) # empty table for percentages
colnames(newB) <- c("breeder","non-breeder") # breeding status
rownames(newB) <- c(5,4,3,2,1,0) # categories

br <- mydata[which(mydata$treatment=="distress familiar"&mydata$status_caller=="breeder"),]
nbr <- mydata[which(mydata$treatment=="distress familiar"&mydata$status_caller=="non-breeder"),]

for (j in 1:nrow(newB)){
  tmp_cat <- as.integer(rownames(newB)[j])
  br_count <- length(which(br$category==tmp_cat))
  newB[j,1] <- br_count/nrow(br)
  nbr_count <- length(which(nbr$category==tmp_cat))
  newB[j,2] <- nbr_count/nrow(nbr)
}

newB_long <- reshape2::melt(newB) # re-format
names(newB_long) <- c("category","status", "value")
newB_long$category <- as.factor(newB_long$category)

#------- Figure 4 b)
# Response of focal individuals, measured in ordinal categories # breeder vs non-breeder
ggplot(newB_long, aes(x = status, y = value, fill = category)) +
  geom_bar(stat = "identity", position = "fill", color = "white", width = 0.7) +
  scale_fill_manual(values = rev(c("black","#00254d","#042d94","#6FAECB","lightblue2","azure2"))) +
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
  annotate("text", x = 1, y = -0.04, label = "n=5", size = 3, color = "black")+
  annotate("text", x = 2, y = -0.04, label = "n=22", size = 3, color = "black")

###################
# Mann-Whitney U Test for different status
# re-shape table again:
newBCount <- newB
for (j in 1:nrow(newBCount)){
  tmp_cat <- as.integer(rownames(newBCount)[j])
  br_count <- length(which(br$category==tmp_cat))
  newBCount[j,1] <- br_count
  nbr_count <- length(which(nbr$category==tmp_cat))
  newBCount[j,2] <- nbr_count
}

# Create the breeder and non-breeder vectors
newBCount<- as.data.frame(newBCount)
breeder <- rep(5:0, times = newBCount$breeder)
non_breeder <- rep(5:0, times = newBCount$`non-breeder`)

# Perform the Mann-Whitney U Test:
(test_result <- wilcox.test(breeder, non_breeder)) # W = 56.5, p-value = 0.9478

###################
#------- Figure 4 c): 
# Response strength to a distress call in relation to social bond 
mydata$category <- as.numeric(mydata$category)
mydata$OBS_PerchDurPerEv_SQ <- sqrt(mydata$OBS_PerchDurPerEv) # take squareroot to improve distribution

ggscatter(mydata[which(mydata$treatment=="distress familiar"),], x = "OBS_PerchDurPerEv_SQ", y = "category", add = "reg.line",
          title = "",
          conf.int = TRUE, cor.method = "spearman", # spearman for one continuoes and one ordinal variable
          # cor.coef = TRUE,
          xlab = "social bond with caller", ylab = "Response strength",
          point = "FALSE") +
  geom_jitter(height = 0.15, width = 0, alpha=0.5, size=3)






#------------------------------------------------------------------------------------------
#-------  Supplemental material: -----
#------------------------------------------------------------------------------------------

###################
#- Assessing bias of order of treatment ------
mydata$order <- as.factor(mydata$order)
mydata$violin_color <- ifelse(mydata$order %in% c("exp,ctr", "ctr,exp"), "orange", "lightblue")
#---------- Figure S.1:
ggplot(mydata[which(!is.na(mydata$order)),], aes(x=order, y=category, fill=violin_color)) + 
  geom_violin(trim = FALSE, color = "black") + 
  geom_dotplot(binaxis = "y", stackdir = "center", 
               dotsize = 0.5, fill = "black", color = "black") +  
  scale_fill_manual(values = c("lightblue" = "lightblue","orange" = "orange"),
                    labels = c("Control", "Distress")) +  # Label the legend properly
  scale_x_discrete(labels = c(
    "ctr,exp" = "unfamiliar->familiar",
    "exp,ctr" = "familiar->unfamiliar",
    "P,S" = "grosbeak->social",
    "S,P" = "social->grosbeak"
  )) + 
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), limits = c(0, 5)) + # limit violins to categories
  theme_bw() +
  theme(legend.position = "top") +  # Position the legend at the top
  labs(x = "Order of treatment", y = "Response strength", fill = "Playback type") +
  geom_segment(x = 1.3, y = 2.5, xend = 1.7, yend = 2.5, color = "grey50")+
  annotate("text", x = 1.5, y = 2.7, label = "ns", size = 4, color = "grey50") +
  geom_segment(x = 3.3, y = 2.5, xend = 3.7, yend = 2.5, color = "grey50")+
  annotate("text", x = 3.5, y = 2.7, label = "ns", size = 4, color = "grey50")

wilcox.test(category ~ order, data = mydata[mydata$order %in% c("exp,ctr","ctr,exp"), ]) #ns
wilcox.test(category ~ order, data = mydata[mydata$order %in% c("P,S","S,P"), ]) #ns

###################
#- Assessing bias of other playback effects ------
# check for effect of length of  distress call playback:
library(lubridate)
mydata$length_call_S <- period_to_seconds(ms(mydata$length_call))
#---------- Figure S.2 a):
ggscatter(mydata, x = "length_call_S", y = "category", add = "reg.line",
          conf.int = TRUE, 
          cor.coef = F, 
          cor.method = "pearson",
          xlab = "length call sequence", ylab = "Response strength")

# check for effect of date on distress call playback:
mydata$date <- as.Date(mydata$date, "%d.%m.%y")
#---------- Figure S.2 b):
ggscatter(mydata[which(mydata$treatment=="distress unfamiliar"|mydata$treatment=="distress familiar"),], x = "date", y = "category", add = "reg.line",
          conf.int = TRUE, 
          cor.coef = F, 
          cor.method = "pearson",
          xlab = "Date", ylab = "Response strength") 

# check for effect of date on control call playback:
#---------- Figure S.2 c):
ggscatter(mydata[which(mydata$treatment=="social unfamiliar"|mydata$treatment=="pine grosbeak"),], x = "date", y = "category", add = "reg.line",
          conf.int = TRUE, 
          cor.coef = F, 
          cor.method = "pearson",
          xlab = "Date", ylab = "Response strength") 



###################
#- PCA on helping behaviors only ------
library(psych) # for PCA

# remove NA: (only one NA for reaction_duration, this behavior is only used for PCA)
mydataN <- mydata[-which(is.na(mydata[,"reaction_duration"])),]

# scale dataset: formula of 'scale': xscaled = (xoriginal – mean(x)) / sd
subsetJayS <- scale(mydataN[,c("calling","calling_distress","min_dist","approach_duration","reaction_duration")])

# scree plot:
scree(subsetJayS) # suggested: take 2 factors

#---------- Table S.1:
# Principle component analysis on helping behaviours
(myPCA <- psych::principal(subsetJayS, nfactors = 2, rotate = "none", scores = T)) 

# label components:
colnames(myPCA$scores)[1] <- 'helping'
colnames(myPCA$scores)[2] <- 'other'
mydataN <- cbind(mydataN,myPCA$scores) 

#---------- Figure S.3 a):
# Helping behaviour shown for different treatments
box <- boxplot(helping~treatment, data=mydataN, ylab = "PC1: helping", main=NA,
               names=c("distress \nunfamiliar","distress \nfamiliar","pine \ngrosbeak","social \nunfamiliar"),
               frame = FALSE, 
               col = c("#E69F00", "firebrick2", "#999999", "#56B4E9"))
stripchart(helping~treatment, data=mydataN, method = "jitter", vertical=T, add=T, pch=16, cex=0.8)

#- ANOVA for treatments (after PCA)------
mydataN$treatment <- as.factor(mydataN$treatment)

ano <- aov(helping ~ treatment, data = mydataN) # ANOVA

#---------- Table S.2:
# Tukey test for ANOVA of PC1 
tukey <- TukeyHSD(ano) 
(round(tukey$treatment, digits=3))

#---------- Figure S.3 b):
# Correlation of social bond vs PC1
ggscatter(mydataN[which(mydataN$treatment=="distress familiar"),], x = "OBS_PerchDurPerEv_SQ", y = "helping", add = "reg.line",
          title = "",
          conf.int = TRUE, cor.method = "spearman", # spearman for one continuoes and one ordinal variable
          # cor.coef = TRUE,
          xlab = "social bond with caller", ylab = "PC1: helping")

