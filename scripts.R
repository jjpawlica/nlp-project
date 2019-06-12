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