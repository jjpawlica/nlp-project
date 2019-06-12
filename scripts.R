# Ustawienie katalogu roboczego i struktury projektu
install.packages("here")
library(here)

setwd(here())

inputDir <- ".\\data"
outputDir <- ".\\results"

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


# Wstępne przetwarzanie

vcorpus <- tm_map(vcorpus, removeNumbers)
vcorpus <- tm_map(vcorpus, removePunctuation)
vcorpus <- tm_map(vcorpus, content_transformer(tolower))

# Wyświetlanie informacji o korpusie
vcorpus
summary(vcorpus)
View(vcorpus)
inspect(vcorpus)

vcorpus[1]
summary(vcorpus[1])
inspect(vcorpus[1])

vcorpus[[1]]
summary(vcorpus[[1]])
inspect(vcorpus[[1]])