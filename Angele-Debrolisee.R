####################################################
#Angèle - Est-ce vraiment le brol ?
####################################################
# Installation de l'environnement
####################################################
#Intallation des packages (une fois)
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("tm") #une fois
#install.packages("qdap") #une fois
#install.packages("wordcloud")
#install.packages("RWeka")
#install.packages("stringi")
#install.packages("BSDA")
#install.packages("dplyr")
#Chargement des bibliothèques
library(ggplot2)
library(readxl) #pour read_excel ...
library(tm) #pour le text mining : Vectorsource(), VCorpus() et le nettoyage removeSparseTerms
library(qdap) #Aussi pour text mining et nettoyage de texte 
library(wordcloud) #Nuages de mots clés.
library(RWeka) #pour Weka_control (utilisé pour la création de bigrammes, trigrammes )
library(stringi) #pour stri_replace_all_fixed(x, " ", "")
library(BSDA)  #pour SIGN.test 
library(dplyr)  #pour slice
#######################################################################################
#Recupération du texte des chansons de Brol d'Angèle
#nous avons tout mis dans un fichier .xlsx chaque ligne représente un vers !!!!
angeleBrol <- read_excel("Angele-Brol.xlsx")
angeleBrol$lineLength <-  nchar(angeleBrol$line) #calcul de la longueur des vers.



##################################################################
#Verification normalité de la distribition des vers de tout l'album
myPValue <- shapiro.test(angeleBrol$lineLength)$p.value #verification de la normalité
#p-value =  1.521e-11 <<< 0.05   la loi n'est pas normale : c'est le brol !!!

#§Grahique 
#calcul de la bonne bindwidth #si on préfere un histogram
x <- angeleBrol$lineLength
hist(x,breaks="FD")
breaks <- pretty(range(x), n = nclass.FD(x), min.n = 1)
bwidth <- breaks[2]-breaks[1]
##############################################################

ggplot(data=angeleBrol, aes(x=lineLength)) + 
#  geom_histogram(binwidth=bwidth ) +
  geom_density(color="blue") +
  xlab("Nombre de vers") +
  ylab("Densité") +

  labs(title = "La loi n'est pas normale.",
       subtitle = paste("la pvaleur = ", format(myPValue, scientific=FALSE) , " << 0.05. C'est le brol."),
       caption = "Courbe de distribution du nombre de vers selon la taille")
##################################################################
#sauvegarde du dernier ggplot 
ggsave(filename = "AngeleBrolDistribution.jpeg",  dpi="print") 


##################################################################
# Normalité  par chanson
lineNChar <- vector()
myShapiroTest <- data.frame(p.value = double())
for (i in 1:12) {
  lineNChar <- angeleBrol[which(angeleBrol$numTitle==i), "lineLength"]
  myShapiroTest[i,"p.value" ] <- shapiro.test(lineNChar$lineLength)$p.value
}
str(myShapiroTest$p.value)
ggplot(data=myShapiroTest, aes(x=as.numeric(rownames(myShapiroTest)), y=p.value)) + 
  geom_line() +
  geom_hline(yintercept = 0.05, color = "red") +
  scale_x_discrete(name="Numéro de Chanson", limits=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  ylab("P Valeur") +
  labs(title = "Aucune distribution n'est normale.",
       subtitle = "Toutes les p.valeurs sont < 0.05. C'est le brol.",
       caption = "P valeur du test de shapiro pour chacune des chansons.")

ggsave(filename = "AngeleBrolNormalTest.jpeg",  dpi="print") #sauvegarde du dernier ggplot 

##################################################################
#Distributions par chanson
for (i in 1:12) {
  songTitle <- unique(angeleBrol[which(angeleBrol$numTitle ==i), "title"])
  pValeur <- myShapiroTest[i,"p.value" ]
  ggplot(data=angeleBrol[which(angeleBrol$numTitle ==i),], aes(x=lineLength)) + 
    geom_density(color="blue") +
    xlab("Nombre de vers") +
    ylab("Densité") +
    labs(title = "La distribution n'est pas normale.",
         subtitle = paste("la p Valeur est ", format(pValeur, scientific=FALSE), " < 0.05. C'est le brol."),
         caption = paste("Courbe de distribution du nombre de vers selon la taille pour ", songTitle ))
  ggsave(filename = stri_replace_all_fixed(paste("AngeleBrolSong-",i,".jpeg"), " ", ""),  dpi="print") #sauvegarde du dernier ggplot 
}



#Comparons les chansons !!  pas très lisible.
ggplot(data=angeleBrol, aes(x=lineLength, color=title)) + 
  geom_density()
  xlab("Nombre de vers") +
  ylab("Densité") +
  
  labs(title = "Aucune loi n'est normale.",
       subtitle = "C'est le brol. en plus c'est illisible !",
       caption = "Courbe de distribution du nombre de vers selon la taille par chanson")

  ggsave(filename = "AngeleBrolAllSongsDist.jpeg",  dpi="print") #sauvegarde du dernier ggplot en fonction du mois
  
############################################################"
#  Comparatif des chansons deux à deux avec SIGN.Test

k=1
myMd <- 0  #médiane attendue différence entre la distribution x et la distribution y
myCl <- 0.95
biSongs <-  data.frame(pValue=double(), 
                       numSong1 = integer(),
                       numSong2 = integer(),
                       conf.int.inf = double(),
                       conf.int.sup = double(),
                       statistic = double(),
                       estimate = double())

for (i in 1:11) {
  
  for (j in (i+1):12) {

    lineNChar1 <- angeleBrol[which(angeleBrol$numTitle==i), "lineLength"]
    lineNChar2 <- angeleBrol[which(angeleBrol$numTitle==j), "lineLength"]
    NbLineMax <- min(nrow(lineNChar1),nrow(lineNChar2)) #x et y doivent être égaux on prend le plus petit possible
    lineNChar1<- slice(lineNChar1, 1:NbLineMax)
    lineNChar2<- slice(lineNChar2, 1:NbLineMax )
    
    res <- SIGN.test(x=lineNChar1$lineLength, y=lineNChar2$lineLength , md=myMd, alternative="two.sided", conf.level = myCl)
    #str(res)
    biSongs[k, "pValue"] <- res$p.value
    biSongs[k, "numSong1"] <- i
    biSongs[k, "numSong2"] <- j
    biSongs[k, "conf.int.inf"] <- res$conf.int[1] #borne inférieure de l'intervalle de confiance
    biSongs[k, "conf.int.sup"] <- res$conf.int[2] #borne supérieure de l'intervalle de confiance
    biSongs[k, "statistic"] <- res$statistic
    biSongs[k, "estimate"] <- res$estimate
  
    #Dessinons les comparatifs
    songTitle1 <- unique(angeleBrol[which(angeleBrol$numTitle == i), "title"])
    songTitle2 <- unique(angeleBrol[which(angeleBrol$numTitle == j), "title"])
    ggplot() + 
      geom_density(data=lineNChar1, aes(x=lineLength), color="blue") +
      geom_density(data=lineNChar2, aes(x=lineLength), color="red") +
      xlab("Nombre de vers") +
      ylab("Densité") +
      labs(title = paste("Comparatif entre ", songTitle1, " (en bleu) et ", songTitle2," (en rouge)"),
           subtitle = paste("la p Valeur est ", format(res$p.value, scientific=FALSE), "\n la médiane de la distribution de la chanson 1 - la 2 est ", res$estimate),
           caption = paste("Courbes de distribution du nombre de vers selon la taille \n pour ", songTitle1, " et ", songTitle2 ))
    ggsave(filename = stri_replace_all_fixed(paste("AngeleBrolSongs-",i,"-",j,"-", k, ".jpeg"), " ", ""),  dpi="print") #sauvegarde du dernier ggplot
    #############
    k <- k+1  # suivant
    }
}

#Pour trouver le  plus 'ressemblant ' et le  plus "différent "
#avec la pValue
which.max(biSongs$pValue)
which.min(biSongs$pValue)
#avec Estimate  : median of x-y
which.min(abs(biSongs$estimate))
which.max(abs(biSongs$estimate))


nbBisongs <- nrow(biSongs)
nbEquBisongs <- nrow(biSongs[which(biSongs$pValue > 0.05),])
ggplot(data=biSongs, aes(x="", y=pValue)) + 
  geom_violin() + 
  geom_hline(yintercept = 0.05, color="red") +
  xlab("") +
  ylab("P-Valeur") +
  labs(title = paste("Finalement il y a ", nbEquBisongs, "couples de chansons sur ", nbBisongs, "qui se ressemblent" ), 
       subtitle = paste("le rapport est de ", round(nbEquBisongs / nbBisongs, digits=2), ". C'est quand même en majorité le brol !!!!"), 
       caption = paste("Boite de dispersion en violon de la P.valeur SIGN.test médiane ", myMd, " niveau de confiance ", myCl))
#sauvegarde du dernier ggplot
ggsave(filename = "biSongsPvalue.jpeg",  dpi="print") 










#############################################################
# Text Mining 
#################

############################################################
##  On va diviser le text par chanson 

laThune.text  <- paste(unlist(angeleBrol[which(angeleBrol$numTitle ==1) , "line"]), collapse=" ")
balanceTonQuoi.text <- paste(unlist(angeleBrol[which(angeleBrol$numTitle ==2) , "line"]), collapse=" ")
jalousie.text <- paste(unlist(angeleBrol[which(angeleBrol$numTitle ==3) , "line"]), collapse=" ")
toutOublier.text <- paste(unlist(angeleBrol[which(angeleBrol$numTitle ==4) , "line"]), collapse=" ")
laLoiDeMurphy.text <- paste(unlist(angeleBrol[which(angeleBrol$numTitle ==5) , "line"]), collapse=" ")
nombreux.text <- paste(unlist(angeleBrol[which(angeleBrol$numTitle ==6) , "line"]), collapse=" ")
victimeDesReseaux.text <- paste(unlist(angeleBrol[which(angeleBrol$numTitle ==7) , "line"]), collapse=" ")
lesMatins.text <- paste(unlist(angeleBrol[which(angeleBrol$numTitle ==8) , "line"]), collapse=" ")
jeVeuxTesYeux.text <- paste(unlist(angeleBrol[which(angeleBrol$numTitle ==9) , "line"]), collapse=" ")
taReine.text <- paste(unlist(angeleBrol[which(angeleBrol$numTitle ==10) , "line"]), collapse=" ")
flemme.text <- paste(unlist(angeleBrol[which(angeleBrol$numTitle ==11) , "line"]), collapse=" ")
flou.text <- paste(unlist(angeleBrol[which(angeleBrol$numTitle ==12) , "line"]), collapse=" ")  



##########################################################
#  Fonction de Nettoyage
##########################################################
cleanText <- function(myText) {
  #on enlève les URLs  :  il n'y en a pas dans notre cas
  myText <- gsub("(f|ht)tp(s?)://\\S+", "", myText)
  #On remplace les caractères spéciaux par des blancs
  myText <- gsub("[][!#$%()*,.:;<=>@^_|~.{}]", " ", myText)
  # Replace abbreviations
  myText <- replace_abbreviation(myText)
  # Replace contractions
  myText <- replace_contraction(myText)
  # Replace symbols with words
  myText <- replace_symbol(myText)
  #Enlève la ponctuation
  myText <- removePunctuation(myText)
  #Enlève les nombres
  #myText <- removeNumbers(myText)
  #bas de casse : en minuscule
  myText <- tolower(myText)
  #Nettoyage mots non significatifs en anglais avec stopwords("english")
  myText <- removeWords(myText, stopwords("en") )
  #Premier nettoyage mots non significatifs en Français avec stopwords("french")
  myText  <- removeWords(myText, stopwords("french") )
  #Nettoyage mots non significatifs en Français 2 :
  #Ici on récupère un fichier de stopwords sur le Net.
  #stopwords_fr <- scan(file = "http://members.unine.ch/jacques.savoy/clef/frenchST.txt", what = character())
  #myText  <- removeWords(myText, stopwords_fr  )
  #Nettoyage mots non significatifs 3 : liste spécifique fournie par nos soins.
  #specificWords <- c("cest", "faut", "être", "comme", "non", "alors", "depuis",
  #           "fait", "quil", "...")
  #myText  <- removeWords(myText, specificWords  )
  #Nettoyage mots personnalisés en fonction des cas 
  #myText <- removeWords(myText, c("", "", "")  )
  #Nettoyage des blancs
  myText <- stripWhitespace(myText)
  #/Nettoyage ###########################################################################
  return(myText)
}



termFrequencyText <- function(myText) {
#Fréquence des termes
#Transformation en vecteurs
  VmyText <- unlist(myText)
#Transformation en vecteurs source
  myText.source <- VectorSource(VmyText)
#Transformation en Corpus
  myText.corpus <- VCorpus(myText.source)
# creation de Term Document Matrix
  myText.tdm <- TermDocumentMatrix(myText.corpus)
# Conversion TDM en matrice
  myText.m  <- as.matrix(myText.tdm)
# Somme des lignes pour fréquences des termes et tri par fréquence.
  myText.term_frequency <- rowSums(myText.m)
  myText.term_frequency <- sort(myText.term_frequency, decreasing = TRUE)
  return(myText.term_frequency)
}  


#pour la  laThune
laThune.clean <- cleanText(laThune.text)
laThune.term_frequency <- termFrequencyText(laThune.clean)  
  # Graphiques en barre pour les 20 premiers termes.
barplot(laThune.term_frequency[1:20],  col = "tan", las = 2, main="Top des mots")
#WordCloud pour La Thune
par(mfrow=c(1,1)) #Raz Layout
#Jeu de données des mots et des occurences.
laThune.word_freqs <- data.frame(term = names(laThune.term_frequency), num = laThune.term_frequency)
# Nuage de Mots Clés
wordcloud(laThune.word_freqs$term, laThune.word_freqs$num, max.words = 100, colors = c("grey80", "darkgoldenrod1", "tomato"), title = "La Thune")



balanceTonQuoi.clean <- cleanText(balanceTonQuoi.text) 
jalousie.clean <- cleanText(jalousie.text) 
toutOublier.clean <- cleanText(toutOublier.text)
laLoiDeMurphy.clean <- cleanText(laLoiDeMurphy.text)
nombreux.clean <- cleanText(nombreux.text)
victimeDesReseaux.clean <- cleanText(victimeDesReseaux.text)
lesMatins.clean <- cleanText(lesMatins.text)
jeVeuxTesYeux.clean <- cleanText(jeVeuxTesYeux.text)
taReine.clean <- cleanText(taReine.text)
flemme.clean <- cleanText(flemme.text)
flou.clean <- cleanText(flou.text)










###############################################
# Nuage de Mots clés en commun ! selon les chansons 
all_texts <- c(laThune.clean, balanceTonQuoi.clean, jalousie.clean, toutOublier.clean, 
               laLoiDeMurphy.clean, nombreux.clean, victimeDesReseaux.clean, lesMatins.clean, 
               jeVeuxTesYeux.clean, taReine.clean, flemme.clean, flou.clean) 
# transformation
all_source <- VectorSource(all_texts)
all_corpus <- VCorpus(all_source)
all_tdm <- TermDocumentMatrix(all_corpus,
                              control = list(
                                removePunctuation = TRUE,
                                wordLengths=c(0,Inf)
                                )
                              )
all_tdm_frequent <- removeSparseTerms(all_tdm,0.50)
all_m <- as.matrix(all_tdm_frequent)


# Mots clés en commun
commonality.cloud(all_m, colors = "steelblue1",
                  max.words = 40, random.order=FALSE)

comparison.cloud(all_m, colors = "steelblue1",
                 max.words = 100, random.order=FALSE)

View(all_m_nz)
#Mots clés disjoints
# Ajoute des noms de colonnes.
#colnames(all_m) = c("angeleBrol", "Wauquiez", "Mélenchon", "Le Pen")
#Plot le graphique.
comparison.cloud(all_m,
                 random.order=FALSE,
                 colors = c("orange", "blue", "red", "black"),
                 title.size=1.5,
                 max.words = 500)


######################
#Graphe de mots associés 

word_associate(angeleBrol.text, match.string = c("président"), 
               stopwords = c(Top200Words,  "amp"), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))
# Add title
title(main = "Tweets sur angeleBrol associés à président")




#Dendrogramme pour angeleBrol
par(mfrow=c(1,1)) #Raz Layout
# On supprime les termes clairsemées de la Term Document Matrix
angeleBrol.tdm2 <- removeSparseTerms(angeleBrol.tdm , sparse = 0.8)
# Transformation en matrice
angeleBrol.m2 <- as.matrix(angeleBrol.tdm2)
# Transformation en Data Frame
angeleBrol.df2 <- as.data.frame(angeleBrol.m2)
str(angeleBrol.df2)
# Calcul de la distance entre les termes
angeleBrol.dist <- dist(angeleBrol.df2)
# Création de la classification hiérarchique (Hierarchical clustering:hc)
angeleBrol.hc <- hclust(angeleBrol.dist)
# hc en dendrorgramm
angeleBrol.dend <- as.dendrogram(angeleBrol.hc)
plot(angeleBrol.dend, horiz=TRUE)


dend <- angeleBrol.df2 %>%  scale %>% dist %>% 
  hclust %>% as.dendrogram %>%
  set("branches_k_color", k=3) %>% set("branches_lwd", 0.5) %>%
  set("labels_colors") %>% set("labels_cex", c(.5,.6)) # %>% 
# set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))
# plot the dend in usual "base" plotting engine:
plot(dend)

ggd1 <- as.ggdend(dend)
ggplot(ggd1, horiz = TRUE, theme = NULL) 





ggd1 <- as.ggdend(angeleBrol.dend)
ggplot(ggd1) 

ggplot(ggd1, labels = TRUE) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")


#on découpe
plot(cut(angeleBrol.dend, h=8)$upper, 
     main="Upper tree of cut at h=20")

plot(cut(angeleBrol.dend, h=3)$lower[[63]], 
    main="Second branch of lower tree with cut at h=")




plot(angeleBrol.hcd, horiz = TRUE)
#install.packages("ape")
#library("ape")
plot(as.phylo(angeleBrol.hcd), type = "fan")


library(ggplot2)
library(dendextend)
# Rectangle dendrogram using ggplot2
ggd1 <- as.ggdend(angeleBrol.hc)
ggplot(ggd1) 
###############################################
#Bi-grammes :  2 mots associés
###############################################

# Fonction de segmentation
#rem : RWeka::NGramTokenizer divise une chaine de caractère en "n-grammes" avec un maximum et un mimimum de n. 
#Dans notre cas c'est toujours 2.
tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 2, max = 2))

#Pour angeleBrol
# Creation  de la Document Term Matrix pour bi-gramme 
angeleBrol.bigram_dtm <- DocumentTermMatrix( 
  angeleBrol.corpus, 
  control = list(tokenize = tokenizer)
)
#########
# Creation bigram_dtm_m (matrice)
angeleBrol.bigram_dtm_m <- as.matrix(angeleBrol.bigram_dtm)
# Creation freq  (fréquences)
angeleBrol.bi_freq <- colSums(angeleBrol.bigram_dtm_m)
# Tri des fréquences
angeleBrol.bi_freq <- sort(angeleBrol.bi_freq, decreasing = TRUE)
# Creation  bi_words
angeleBrol.bi_words <- names(angeleBrol.bi_freq)
# Visualisation extrait de bi_words
angeleBrol.bi_words[1:100]
# Plot nuage de mots clés en bi-grammes
wordcloud(words = angeleBrol.bi_words, freq = angeleBrol.bi_freq , max.words = 100, colors = c("grey80", "darkgoldenrod1", "tomato"))


###############################################
#tri-grammes :  3 mots associés
###############################################

# Fonction de segmentation
#rem : RWeka::NGramTokenizer divise une chaine de caractère en "n-grammes" avec un maximum et un mimimum de n. 
#Dans notre cas c'est toujours 3.
tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 3, max = 3))

#Pour angeleBrol
# Creation  de la Document Term Matrix pour bi-gramme 
angeleBrol.trigram_dtm <- DocumentTermMatrix( 
  angeleBrol.corpus, 
  control = list(tokenize = tokenizer)
)
#########
# Creation trigram_dtm_m (matrice)
angeleBrol.trigram_dtm_m <- as.matrix(angeleBrol.trigram_dtm)
# Creation freq  (fréquences)
angeleBrol.bi_freq <- colSums(angeleBrol.trigram_dtm_m)
# Tri des fréquences
angeleBrol.bi_freq <- sort(angeleBrol.bi_freq, decreasing = TRUE)
# Creation  bi_words
angeleBrol.bi_words <- names(angeleBrol.bi_freq)
# Visualisation extrait de bi_words
angeleBrol.bi_words[1:50]
# Plot nuage de mots clés en bi-grammes
wordcloud(words = angeleBrol.bi_words, freq = angeleBrol.bi_freq , max.words = 100)
#, colors = c("grey80", "darkgoldenrod1", "tomato")

##############################################
#quadri-grammes :  4 mots associés
###############################################

# Fonction de segmentation
#rem : RWeka::NGramTokenizer divise une chaine de caractère en "n-grammes" avec un maximum et un mimimum de n. 
#Dans notre cas c'est toujours 3.
tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 4, max = 4))

#Pour angeleBrol
# Creation  de la Document Term Matrix pour bi-gramme 
angeleBrol.trigram_dtm <- DocumentTermMatrix( 
  angeleBrol.corpus, 
  control = list(tokenize = tokenizer)
)
#########
# Creation trigram_dtm_m (matrice)
angeleBrol.trigram_dtm_m <- as.matrix(angeleBrol.trigram_dtm)
# Creation freq  (fréquences)
angeleBrol.bi_freq <- colSums(angeleBrol.trigram_dtm_m)
# Tri des fréquences
angeleBrol.bi_freq <- sort(angeleBrol.bi_freq, decreasing = TRUE)
# Creation  bi_words
angeleBrol.bi_words <- names(angeleBrol.bi_freq)
# Visualisation extrait de bi_words
angeleBrol.bi_words[1:50]
# Plot nuage de mots clés en bi-grammes
wordcloud(words = angeleBrol.bi_words, freq = angeleBrol.bi_freq , max.words = 100)
#, colors = c("grey80", "darkgoldenrod1", "tomato")


# NOT RUN {
x <- c(7.8, 6.6, 6.5, 7.4, 7.3, 7., 6.4, 7.1, 6.7, 7.6, 6.8)
SIGN.test(x, md = 6.5)
# Computes two-sided sign-test for the null hypothesis 
# that the population median for 'x' is 6.5. The alternative 
# hypothesis is that the median is not 6.5. An interpolated 95% 
# confidence interval for the population median will be computed.

reaction <- c(14.3, 13.7, 15.4, 14.7, 12.4, 13.1, 9.2, 14.2, 
              14.4, 15.8, 11.3, 15.0)
SIGN.test(reaction, md = 15, alternative = "less")
# Data from Example 6.11 page 330 of Kitchens BSDA.  
# Computes one-sided sign-test for the null hypothesis 
# that the population median is 15.  The alternative 
# hypothesis is that the median is less than 15.  
# An interpolated upper 95% upper bound for the population 
# median will be computed.

x <- c(1,2,3,4,1,2,3,4,5)
y <- c(1,2,3,4,5,6,7,9, 10)
SIGN.test(x, y, md = 0)
