#AUTORA: CARLA PASSOS
#CRIADO EM: 13/02/2021

#https://www.ibpad.com.br/blog/comunicacao-digital/capturando-dados-do-twitter-com-r/

library(twitteR)

library(rtweet)
library(ROAuth)
library(ggmap)
library(tm)

##################################################################################################
#ATIVANDO A CONEXAO DO R COM O TWITTER
##################################################################################################
consumer_key    consumer_key    = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
consumer_secret = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
access_token    = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
access_secret   = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
a <- create_token("projetofinal",consumer_key, consumer_secret, access_token, access_secret)
############################################################################

woeid <- availableTrendLocations()
woeid$name[woeid$country=="Brazil"] #estados brasileiros que tem o codigo


#num <- woeid$woeid[woeid$name=="Rio de Janeiro"]

woeid$woeid[woeid$name=="Rio de Janeiro"]




woeid$woeid[woeid$name=="São Paulo"]
woeid$woeid[woeid$name=="Brasília"]
woeid$woeid[woeid$name=="Fortaleza"]
woeid$woeid[woeid$name=="Manaus"]
woeid$woeid[woeid$name=="Curitiba"]


# woeid -> where on earth id
# 455819 é o código de Brasília
trendsrio <- getTrends(woeid = 455825)
trendsrio
trendsrio$name
trj <- VCorpus(VectorSource(trendsrio$name))
#Build a term-document matrix
tdmrj <- tm_map(trj,PlainTextDocument)
tdmrj <- TermDocumentMatrix(tdmrj)
mtmrj <- as.matrix(tdmrj$dimnames$Terms)
mtdrj1 <- as.matrix(tdmrj)
freqrj <- sort(rowSums(mtdrj1),decreasing = T)
#freq_nova <-freq[freq>=100]
#freq_nova
#word = as.matrix(freq_nova)
#v = sort(rowSums(word), decreasing=TRUE)
#myNames = names(v)


trendsBrasilia <- getTrends(woeid = 455819)
trendsBrasilia
# 10 primeiros apenas
trendsBrasilia$name
tb <- VCorpus(VectorSource(trendsBrasilia$name))
#Build a term-document matrix
tdmb <- tm_map(tb,PlainTextDocument)
tdmb <- TermDocumentMatrix(tdmb)
mtmb <- as.matrix(tdmb$dimnames$Terms)
mtdb1 <- as.matrix(tdmb)
freqb <- sort(rowSums(mtdb1),decreasing = T)
#freq_nova <-freq[freq>=100]
#freq_nova
#word = as.matrix(freq_nova)
#v = sort(rowSums(word), decreasing=TRUE)
#myNames = names(v)


trendssp <- getTrends(woeid = 455827)
trendssp
# 10 primeiros apenas
trendssp$name
tsp <- VCorpus(VectorSource(trendssp$name))
#Build a term-document matrix
tdmsp <- tm_map(tsp,PlainTextDocument)
tdmsp <- TermDocumentMatrix(tdmsp)
mtmsp <- as.matrix(tdmsp$dimnames$Terms)
mtdsp1 <- as.matrix(tdmsp)
freqsp <- sort(rowSums(mtdsp1),decreasing = T)


trendsfortaleza <- getTrends(woeid = 455830)
trendsfortaleza
# 10 primeiros apenas
trendsfortaleza$name
tfortaleza <- VCorpus(VectorSource(trendsfortaleza$name))
#Build a term-document matrix
tdmfortaleza <- tm_map(tfortaleza,PlainTextDocument)
tdmfortaleza <- TermDocumentMatrix(tdmfortaleza)
mtmfortaleza <- as.matrix(tdmfortaleza$dimnames$Terms)
mtdfortaleza1 <- as.matrix(tdmfortaleza)
freqfortaleza <- sort(rowSums(mtdfortaleza1),decreasing = T)
#freq_nova <-freq[freq>=100]
#freq_nova
#word = as.matrix(freq_nova)
#v = sort(rowSums(word), decreasing=TRUE)
#myNames = names(v)

trendsmanaus <- getTrends(woeid = 455833)
trendsmanaus
# 10 primeiros apenas
trendsmanaus$name
tmanaus <- VCorpus(VectorSource(trendsmanaus$name))
#Build a term-document matrix
tdmmanaus <- tm_map(tmanaus,PlainTextDocument)
tdmmanaus <- TermDocumentMatrix(tdmmanaus)
mtmmanaus <- as.matrix(tdmmanaus$dimnames$Terms)
mtdmanaus1 <- as.matrix(tdmmanaus)
freqmanaus <- sort(rowSums(mtdmanaus1),decreasing = T)


trendscuri <- getTrends(woeid = 455822)
trendscuri
# 10 primeiros apenas
trendscuri$name
tmanaus <- VCorpus(VectorSource(trendsmanaus$name))
#Build a term-document matrix
tdmmanaus <- tm_map(tmanaus,PlainTextDocument)
tdmmanaus <- TermDocumentMatrix(tdmmanaus)
mtmmanaus <- as.matrix(tdmmanaus$dimnames$Terms)
mtdmanaus1 <- as.matrix(tdmmanaus)
freqmanaus <- sort(rowSums(mtdmanaus1),decreasing = T)

