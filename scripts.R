# Ustawienie katalogu roboczego i struktury projektu
install.packages("here")
library(here)

setwd(here())

inputDir <- ".\\data"
outputDir <- ".\\results"
utilsDir <- ".\\utils"

# Instalacja i aktywacja wymaganych pakietów
install.packages(c("tm", "hunspell"))

library(tm)
library(hunspell)
library(stringr)

# Utworzenie korpusu dokumentów
vcorpus <- VCorpus(
  DirSource(inputDir, pattern = "*.txt", encoding = "UTF-8"),
  readerControl = list(language = "pl_PL")
)

# Wstępne przetwarzanie: numery, znaki interpunkcyjne, do małej litery, 

vcorpus <- tm_map(vcorpus, removeNumbers)
vcorpus <- tm_map(vcorpus, removePunctuation)
vcorpus <- tm_map(vcorpus, content_transformer(tolower))


# Wstępne przetwarzanie: słowa ze stop listy

stoplistFile <- paste(utilsDir, "\\", "stopwords_pl.txt", sep = "", collapse = NULL)
stoplist <- readLines(stoplistFile, encoding = "UTF-8")
vcorpus <- tm_map(vcorpus, removeWords, stoplist)

# Wstępne przetwarzanie: nadmierne puste znaki

vcorpus <- tm_map(vcorpus, stripWhitespace)

# Wyświetlanie informacji o korpusie

summary(vcorpus)
inspect(vcorpus)
