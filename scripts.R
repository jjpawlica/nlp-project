# Instalacja pakietów

install.packages("here")
library(here)

install.packages("lsa")
library(lsa)

install.packages(c("tm", "hunspell"))

library(tm)
library(hunspell)
library(stringr)

# Ustawienie katalogu roboczego i struktury projektu

setwd(here())

inputDir <- ".\\data"
outputDir <- ".\\results"
utilsDir <- ".\\utils"

# Utworzenie korpusu dokumentów (a)
vcorpus <- VCorpus(
  DirSource(inputDir, pattern = "*.txt", encoding = "UTF-8"),
  readerControl = list(language = "pl_PL")
)

# Wstępne przetwarzanie: nadmierne puste znaki (b)

vcorpus <- tm_map(vcorpus, stripWhitespace)

# Wstępne przetwarzanie: numery, znaki interpunkcyjne, do małej litery (b)

vcorpus <- tm_map(vcorpus, content_transformer(tolower))
vcorpus <- tm_map(vcorpus, removeNumbers)
vcorpus <- tm_map(vcorpus, removePunctuation)


# Wstępne przetwarzanie: słowa ze stop listy (b)

stoplistFile <- paste(utilsDir, "\\", "stopwords_pl.txt", sep = "", collapse = NULL)
stoplist <- readLines(stoplistFile, encoding = "UTF-8")
vcorpus <- tm_map(vcorpus, removeWords, stoplist)

vcorpus <- tm_map(vcorpus, stripWhitespace)
vcorpus <- tm_map(vcorpus, removePunctuation)

# Wstępne przetwarzanie: usunięcie niechcianych znaków

removeChar <- content_transformer(
  function(x, pattern, replacement) gsub(pattern, replacement, x)
)

vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(8722), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(8217), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(8211), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(8220), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(8221), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(8222), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(8230), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(187), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(171), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(190), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(190), "")

vcorpus <- tm_map(vcorpus, stripWhitespace)

# Wstępne przetwarzanie: lematyzacja

polish <- dictionary(lang = "pl_PL", affix = NULL, cache = TRUE)

lemmatize <- function(text) {
  singleText <- str_trim(as.character(paste(text,collapse = "")))
  parsedText <- strsplit(singleText, " ")

  newTextVec <- hunspell_stem(parsedText[[1]], dict = polish)
  
  newTextVec
    for (i in 1:length(newTextVec)) {
     if (length(newTextVec[[i]]) == 0) newTextVec[i] <- parsedText[[1]][i]
     if (length(newTextVec[[i]]) > 1) newTextVec[i] <- newTextVec[[i]][1]
   }
  newText <- paste(newTextVec, collapse = " ")
  return(newText)
}

vcorpus <- tm_map(vcorpus, content_transformer(lemmatize))

# Usunięcie rozszerzeń z nazw dokumentów w korpusie

cutExtensions <- function(document) {
  meta(document, "id") <- gsub(
    pattern="\\.txt$",
    "",
    meta(document, "id")
  )
  return(document)
}

vcorpus <- tm_map(vcorpus, cutExtensions)

# Zapisanie korpusu do plików

preprocessedDir <- paste(outputDir, "\\", "preprocessed", sep = "", collapse = NULL)
dir.create(preprocessedDir)
writeCorpus(vcorpus, path = preprocessedDir)

# Wyświetlanie informacji o korpusie  --- A

summary(vcorpus)
inspect(vcorpus[1])


# Tworzenie macierzy częstości --- c 4 b

tdm_tf_all <- TermDocumentMatrix(vcorpus)
tdm_bin_all <- TermDocumentMatrix(vcorpus, control = list(weigthing = weightBin))

tdm_tf_1_20 <- TermDocumentMatrix(vcorpus, control = list(bounds = list(global = c(1, Inf))))
tdm_tfidf_1_20 <- TermDocumentMatrix(vcorpus, control = list( weighting = weightTfIdf, bounds = list(global = c(1, Inf))))
dtm_tfidf_1_20 <- DocumentTermMatrix(vcorpus, control = list(weighting = weightTfIdf, bounds = list(global = c(1,Inf))))

tdm_tf_2_19 <- TermDocumentMatrix(vcorpus, control = list(bounds = list(global = c(2,19))))
tdm_tfidf_2_19 <- TermDocumentMatrix(vcorpus, control = list( weighting = weightTfIdf, bounds = list(global = c(2,19))))
dtm_tfidf_2_19 <- DocumentTermMatrix(vcorpus, control = list(weighting = weightTfIdf, bounds = list(global = c(2,19))))

tdm_tf_3_18 <- TermDocumentMatrix(vcorpus, control = list(bounds = list(global = c(3,18))))
tdm_tfidf_3_18 <- TermDocumentMatrix(vcorpus, control = list( weighting = weightTfIdf, bounds = list(global = c(3,18))))
dtm_tfidf_3_18 <- DocumentTermMatrix(vcorpus, control = list(weighting = weightTfIdf, bounds = list(global = c(3,18))))

tdm_tf_4_17 <- TermDocumentMatrix(vcorpus, control = list(bounds = list(global = c(4,17))))
tdm_tfidf_4_17 <- TermDocumentMatrix(vcorpus, control = list( weighting = weightTfIdf, bounds = list(global = c(4,17))))
dtm_tfidf_4_17 <- DocumentTermMatrix(vcorpus, control = list(weighting = weightTfIdf, bounds = list(global = c(4,17))))

tdm_tf_all
tdm_bin_all

tdm_tf_1_20
tdm_tf_2_19
tdm_tf_3_18
tdm_tf_4_17

tdm_tfidf_1_20
tdm_tfidf_2_19
tdm_tfidf_3_18
tdm_tfidf_4_17

# Analiza głównych składowych - d 4 b

pca_1_20 <- prcomp(dtm_tfidf_1_20)
pca_2_19 <- prcomp(dtm_tfidf_2_19)
pca_3_18 <- prcomp(dtm_tfidf_3_18)
pca_4_17 <- prcomp(dtm_tfidf_4_17)

legend <- paste(paste("d", 1:20, sep = ""), rownames(dtm_tfidf_1_20), sep = " - ")

## Wykres dla 1 - 20
pca_plot_1_20 <- paste(outputDir,'\\',"pca_1_20.png",sep = "", collapse = NULL)

png(filename = pca_plot_1_20, width = 1024)
options(scipen = 5)
par(mar = c(5.1, 4.1, 4.1, 25.1), xpd = TRUE)
plot(pca_1_20$x[,1], pca_1_20$x[,2], pch = 1, col = "black", xlab = "", ylab = "")
text(pca_1_20$x[,1], pca_1_20$x[,2], paste("d",1:19,sep = ""), col = "black",pos = 4)
legend("topright",  inset = c(-0.57,0), legend, text.font = 16, cex = 0.5, text.col = "black")
dev.off()

## Wykres dla 2 - 19
pca_plot_2_19 <- paste(outputDir,'\\',"pca_2_19.png",sep = "", collapse = NULL)

png(filename = pca_plot_2_19, width = 1024)
options(scipen = 5)
par(mar = c(5.1, 4.1, 4.1, 25.1), xpd = TRUE)
plot(pca_2_19$x[,1], pca_2_19$x[,2], pch = 1, col = "black", xlab = "", ylab = "")
text(pca_2_19$x[,1], pca_2_19$x[,2], paste("d",1:19,sep = ""), col = "black",pos = 4)
legend("topright",  inset = c(-0.57,0), legend, text.font = 16, cex = 0.5, text.col = "black")
dev.off()

## Wykres dla 3 - 18
pca_plot_3_18 <- paste(outputDir,'\\',"pca_3_18.png",sep = "", collapse = NULL)

png(filename = pca_plot_3_18, width = 1024)
options(scipen = 5)
par(mar = c(5.1, 4.1, 4.1, 25.1), xpd = TRUE)
plot(pca_3_18$x[,1], pca_3_18$x[,2], pch = 1, col = "black", xlab = "", ylab = "")
text(pca_3_18$x[,1], pca_3_18$x[,2], paste("d",1:19,sep = ""), col = "black",pos = 4)
legend("topright",  inset = c(-0.57,0), legend, text.font = 16, cex = 0.5, text.col = "black")
dev.off()

## Wykres dla 4 - 17
pca_plot_4_17 <- paste(outputDir,'\\',"pca_4_17.png",sep = "", collapse = NULL)

png(filename = pca_plot_4_17, width = 1024)
options(scipen = 5)
par(mar = c(5.1, 4.1, 4.1, 25.1), xpd = TRUE)
plot(pca_4_17$x[,1], pca_4_17$x[,2], pch = 1, col = "black", xlab = "", ylab = "")
text(pca_4_17$x[,1], pca_3_18$x[,2], paste("d",1:19,sep = ""), col = "black",pos = 4)
legend("topright",  inset = c(-0.57,0), legend, text.font = 16, cex = 0.5, text.col = "black")
dev.off()

# Dekompozycja wedłud wartości osobliwych (analiza ukrytych wymiarów semantycznych) d 4 b

tdm_tfidf_1_20_matrix <- as.matrix(tdm_tfidf_1_20)
tdm_tfidf_2_19_matrix <- as.matrix(tdm_tfidf_2_19)
tdm_tfidf_3_18_matrix <- as.matrix(tdm_tfidf_3_18)
tdm_tfidf_4_17_matrix <- as.matrix(tdm_tfidf_4_17)

lsa_model_1_20 <- lsa(tdm_tfidf_1_20_matrix)
lsa_model_2_19 <- lsa(tdm_tfidf_2_19_matrix)
lsa_model_3_18 <- lsa(tdm_tfidf_3_18_matrix)
lsa_model_4_17 <- lsa(tdm_tfidf_4_17_matrix)

# lsa_model$tk # odpowiednik macierzy U, współrzędne wyrazów 
# lsa_modeldk # odpowiednik macierzy V, współrzędne dokumentów
# lsa_model$sk # odpowiednik macierzy D, znaczenie składowych

coordTerms <- lsa_model_1_20$tk %*% diag(lsa_model_1_20$sk)
coordDocs <- lsa_model_1_20$dk %*% diag(lsa_model_1_20$sk)
words <- c("samochód", "auto", "kierowca", "technologia", "przeglądarka", "internet", "serce", 'hiv', "rak", "zdrowie", "dieta", "gen", "dna") 
coordWords <- coordTerms[words,]

legend <- paste(paste("d",1:20,sep = ""), rownames(coordDocs), sep = " - ")

plot(coordDocs[,1],coordDocs[,2], pch=1, col="black", xlim=c(-0.2,0.05))
points(coordWords[,1],coordWords[,2], pch=2, col="red")
text(coordDocs[,1],coordDocs[,2],paste("d",1:19,sep=""),pos=4, col="black")
text(coordWords[,1],coordWords[,2],rownames(coordWords),pos=4, col="red")
legend("bottomleft", legend, cex=0.4, text.col="dark violet")





















