##################################################################################
# Playing with quanteda
# RHUL again 2023
# 
# EAJ July 2023
##################################################################################

# clear environment and set working directory
rm(list = ls())
wdir <- "~/Documents/Jobs/RHUL again/"
setwd(wdir)

# installation packages - uncomment/copy to command line as appropriate
#install.packages("quanteda") 
#install.packages("quanteda.textplots") 
### needs sudo apt-get install libpoppler-cpp-dev   # for antiword
#install.packages("pdftools")
#install.packages("readtext")
#install.packages("spacyr")

# load
library(readtext)
library(quanteda)
library(quanteda.textplots)

# read in a text file
rt_txt <- readtext("feasStudy.txt")

# create quanteda corpus
corpus_txt <- corpus(rt_txt)
summary(corpus_txt)

# lifted from https://joss.theoj.org/papers/10.21105/joss.00774
# construct the feature co-occurrence matrix
examplefcm <-
  tokens(corpus_txt, remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english"), padding = FALSE) %>%
  fcm(context = "window", window = 5, tri = FALSE)

# choose 30 most frequency features
topfeats <- names(topfeatures(examplefcm, 30))

# and plot the network
set.seed(100)

png("fS_network.png")
textplot_network(fcm_select(examplefcm, topfeats), min_freq = 0.8)
dev.off()
