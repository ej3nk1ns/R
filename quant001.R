##################################################################################
# Playing with quanteda, see https://quanteda.io/
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
#install.packages("quanteda.textstats") 
### needs sudo apt-get install libpoppler-cpp-dev   # for antiword
#install.packages("pdftools")
#install.packages("readtext")
#install.packages("spacyr")

# load
library(readtext)
library(quanteda)
library(quanteda.textplots)

# read in a text file
##rt_txt <- readtext("feasStudy.txt")
rt_pdf <- readtext("feasStudy.pdf")

# create quanteda corpus
##corpus_txt <- corpus(rt_txt)
corpus_pdf <- corpus(rt_pdf)
print(summary(corpus_pdf))

# lifted from https://joss.theoj.org/papers/10.21105/joss.00774
# construct the feature co-occurrence matrix
examplefcm <-
  tokens(corpus_pdf, remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english"), padding = FALSE) %>%
  fcm(context = "window", window = 5, tri = FALSE)

# choose 30 most frequency features
topfeats <- names(topfeatures(examplefcm, 30))

# and plot the network (textplots)
set.seed(100)

# ctrl-enter
png("fS_network_pdf.png")
textplot_network(fcm_select(examplefcm, topfeats), min_freq = 0.8)
dev.off()

# now create a document feature matrix (textstats)
tok_pdf <- tokens_remove(tokens(corpus_pdf, remove_punct = TRUE), 
                         stopwords("english"), padding = FALSE)
exampledfm <- dfm(tok_pdf)
print(exampledfm)

# and construct a word cloud
png("fS_wordcloud.png", width = 1440, height = 1440)
textplot_wordcloud(exampledfm, max_words = 100)
dev.off()
