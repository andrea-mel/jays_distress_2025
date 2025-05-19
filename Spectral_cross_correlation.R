rm(list=ls())

# load packages
library(warbleR)
library(tidyverse)
library(dplyr)

# here: setwd to folder "WAV"!

# load selection table, which contains information about start/end - points of distress call elements to analyse:
selec_table <- read.table("selection_table.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

#---------------------
# remove file19 for this analysis, which has a slightly higher sampling rate, and might cause comparison bias:
selec_table <- selec_table[-which(selec_table$sound.files=="file19.wav"),]

#---------------------
# get some numbers:
length(unique(selec_table$sound.files)) # 19 recordings / individuals
mean(table(selec_table$sound.files)) # 8.4
median(table(selec_table$sound.files)) # 6
range(table(selec_table$sound.files)) # 2-26
nrow(selec_table) # 160 elements in total
#---------------------


#---------------------
# calculate average SNR:
SNR_tab <- sig2noise(selec_table,
                     bp = c(0,20), # frequency range
                     mar = 0.1, # measures 0.1 sec before/after (100ms)
                     wl = 200)
(average_snr <- SNR_tab %>%
  group_by(sound.files) %>%
  summarise(avg_SNR = mean(SNR, na.rm = TRUE))) # returns a table with average SNR per call sequence
#---------------------


#---------------------
# perform checks of data set (resolution, etc)
chck <- warbleR::check_sels(selec_table) 
warbleR::check_sound_files(selec_table)
#---------------------


#---------------------
# Run spectral cross-correlation (running this step may be time consuming!)
xcorr_results <- warbleR::cross_correlation(
  X = selec_table,         # Selection table
  wl = 200,                # window length of the spectrogram
  ovlp = 70,               # Overlap between windows (%), default=70
  type = "fourier", # default
  cor.method = "pearson",  # Correlation method, default
  bp = c(0,20) # lower and upper limits of a frequency bandpass filter (in kHz). I use the whole range.
)
str(xcorr_results)
xcorrMat <- as.matrix(xcorr_results) # transform into matrix
#---------------------


#---------------------
# Test: is the between-file-variation higher than the within-file-variation?
#---

# Map elements (snippets) to their sound files:
selec_table_2 <- selec_table %>%
  mutate(Snippet = paste(sound.files, selec, sep = "-")) %>%
  select(Snippet, sound.files)

# Convert correlation matrix to long format:
xcorr_df <- as.data.frame(as.table(xcorrMat))
colnames(xcorr_df) <- c("Snippet1", "Snippet2", "Correlation")

# Remove self-comparisons (diagonal elements):
xcorr_df <- xcorr_df[xcorr_df$Snippet1 != xcorr_df$Snippet2, ]

# Merge with selec_table_2 to get file names for both snippets:
xcorr_df <- xcorr_df %>%
  left_join(selec_table_2, by = c("Snippet1" = "Snippet")) %>%
  rename(File1 = sound.files) %>%
  left_join(selec_table_2, by = c("Snippet2" = "Snippet")) %>%
  rename(File2 = sound.files)

# Categorize comparisons as "Within" recording/ID or "Between" recording/ID:
xcorr_df <- xcorr_df %>%
  mutate(Category = ifelse(File1 == File2, "Within", "Between"))

# visualize distribution of corr-values, to choose the right tests:
ggplot(xcorr_df, aes(x = Correlation, fill = Category)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Distribution of Correlation Values",
       x = "Correlation",
       y = "Density")

# due to non-normal distribution, use Mann-Whitney U Test
(stats <- wilcox.test(Correlation ~ Category, data = xcorr_df)) # probability that the two groups come from the same distribution is essentially zero

#------- Figure 3:
boxplot(Correlation ~ Category, 
        data = xcorr_df, 
        ylab = "Cross-correlation", 
        xlab = "Comparison type",
        main = NA,
        col = c("#E69F00", "#56B4E9"),
        boxwex = 0.6)
segments(x0 = 1.2, x1 = 1.8, y0 = 0.45, lwd = 1.5, col = "grey50")  # line above boxes 
text(x = 1.5, y = 0.49, labels = "***", cex = 1.2, col = "grey50")
