#Por: Carla C. P. Cruz

##################################################################################################
#CONEXÃO R E TWITTER
##################################################################################################
#Ativação dos pacotes (caso não tenha instalado, primeiro usar install.packages("nome_do_pacote))
library(ROAuth)
library(twitteR)
library(rtweet)
library(NLP)

#Colocar aqui os códigos obtidos pelo passo a passo que está no READ.ME
consumer_key    = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
consumer_secret = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
access_token    = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
access_secret   = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#Aparecerão duas opções, escolher a 2
##################################################################################################


##################################################################################################
#COLETA DOS DADOS
##################################################################################################
#Coletando os Tweets com a hashtag #beirut ou #Beirut ou #BEIRUT
#Aqui o idioma escolhido foi o inglês, mas poderia ser qualquer outro idioma
#Aqui não foram incluídos os retweets (include_rts = FALSE)
#A quantidade de tweets coletados foi de 10.0000 (n = 100000)
tw <- search_tweets(q = "#beirut OR #Beirut OR #BEIRUT", lang = "en", retryonratelimit = TRUE,  
                    n = 10000, include_rts = FALSE)

#Salvando as informações obtidas
writexl::write_xlsx(tw,"tweets_beirut.xlsx")
##################################################################################################


##################################################################################################
#PREPARAÇÃO DOS DADOS
##################################################################################################
#Como na tabela salva têm outras informações, será necessário extrair somente o que deseja.
#Além disso, precisa ser realizado a limpeza dos textos

#Lendo o arquivo salvo
dados <- readxl::read_xlsx("Caminho do arquivo xlsx salvo")

#Extraindo somente os tweets
tw_text <- dados$text

#Remove links longos e curtos nos tweets
library(qdapRegex)
t1 <- rm_twitter_url(tw_text)

#Caso venha com emojis ou emotions, transformar em caracteres para serem retirados posteriormente
#OBS.: este passo irá variar de acordo com o idioma escolhido e o idioma do computador
t1 <- iconv(t1, 'UTF-8', 'ASCII')

#Criando o Corpus
library(tm) #pacote que trabalha com o text mining
t1 <- VCorpus(VectorSource(t1)) #caso não funcione, tentar Corpus no lugar de VCorpus

#Substituindo "/", "@", "|" por espaço em branco
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
t1 <- tm_map(t1, toSpace, "/")
t1 <- tm_map(t1, toSpace, "@")
t1 <- tm_map(t1, toSpace, "\\|")

#Convertendo os termos para minúsculo
t1 <- tm_map(t1, content_transformer(tolower))

#Removendo números
t1 <- tm_map(t1, removeNumbers)

#Removendo as stopwords
#O pacote do R não possui todas as stopwords. Será necessário complementar.
#Sugestão: https://www.ranks.nl/stopwords
library(stopwords) #pacote que contêm as stopwords  
t1 <- tm_map(t1, removeWords, stopwords("english")) #o idioma escolhido foi o inglês, mas há outros

#Aqui será lida as stopwords complementares
stop_comp <- readxl::read_xlsx("other_stopwords.xlsx")
stop_comp1 <- as.matrix(stop_comp) #transformação de data.frame em matrix
t1 <- tm_map(t1, removeWords, stop_comp1)

#OBS.: Pode ser que haja outras palavras a serem retiradas. Neste caso, fazer este passo (opicional):
#other <- c("uae","ago","cargo","aid","amp") #aqui seriam as outras palavras a serem retiradas
#t1 <- tm_map(t1, removeWords, other)

#Remove pontuação                               
t1 <- tm_map(t1, removePunctuation)

#Remove espaços extras
t1 <- tm_map(t1, stripWhitespace)

#Processo de Stemming (opicional)
#Reduz as palavras para um mesmo radical. O uso dele dependerá do trabalho que quer ser feito
#t1 <- tm_map(t1, stemDocument)

#Construindo a matriz termo-documento
tdm <- tm_map(t1,PlainTextDocument)
tdm <- TermDocumentMatrix(tdm)

#Verificando os termos da matriz
mtm <- as.matrix(tdm$dimnames$Terms)
                               
#Verificando a frquencia dos termos
#Nesta parte é que os termos são selecionados, ou seja há o corte
#A quantidade de termos a ser usada irá depender do trabalho que se deseja.
mtd1 <- as.matrix(tdm)
freq <- sort(rowSums(mtd1),decreasing = T)

#Seleção dos termos
#Foi utilizada a frequência absoluta. Os termos escolhidos foram os que apareciam 100 vezes ou mais                       
freq_nova <-freq[freq>=100]
freq_nova
##################################################################################################
                               
                                                        
##################################################################################################
#VISUALIZAÇÃO DOS DADOS
##################################################################################################
#Nuvem de Palavras
library(wordcloud2) #pacote da nuvem de palavras (obs.: existe o pacote "wordcloud")
library(RColorBrewer) #pacote para cores da nuvem de palavras
word <- as.matrix(freq_nova)
freqa <- sort(rowSums(word), decreasing=TRUE)
names <- names(freqa)
dwordc <- data.frame(word=names, freq=freqa)
wordcloud2(dwordc, size = 1.8, color= "random-light", shape= 'circle', rotateRatio = 0.0)

#Mapa Mundi
#Há várias formas de gerar o mapa. Esta é uma delas
library(maps)  #pacote para geração de mapas
options(stringsAsFactors = FALSE)                               
str(dados) #vendo as variáveis do data.frame salvo após a geração dos dados do twitter e salvo
geo_beirut <- lat_lng(data.frame(dados$coords_coords)) #coordenadas
par(mar = c(0,0,0,0))
maps::map("world",  lwd = 0.5) #o mapa gerado aqui é o mundi
with(geo_beirut, title ="a", points(lng, lat, pch = 20,cex = 1.0, col = "purple"),projection="polyconic")                              
                                                                                       
#Gráfico de Barras dos países
library(ggplot2)
country <- dados$country #lendo os países que foram captados
co <- data.frame(sort(table(country)))
names(co) <- c("pais","freq")
ggplot(co,aes(x = pais, y = freq)) + geom_col() + xlab(NULL) + coord_flip() +
labs(x = "Paises", y = "Quantidade", title = "Paises que mais comentaram sobre Beirute")
                                                              
#Análise de Sentimentos
library(syuzhet)
sentiment_beirut <- get_nrc_sentiment(mtm) #matriz termo-documento
scores_beirut <- colSums(sentiment_beirut)
dfs <- data.frame(names(scores_beirut),as.integer(scores_beirut))
names(dfs) <- c("sentiment","score");dfs
q <- get_sentiment(mtm) #scores sem classificacao
q1 <- as.matrix(q)
cpos <- 0 #Na análise de sentimentos, há outros sentimentos que podem ser explorados
cneg <- 0 #e que aparecem originalmente. No entanto, num primeiro momento, será trabalhado
cneu <- 0 #somente os positivos, neutros e negativos. 
vsen <- NULL
for (i in 1:nrow(q1)){ #classificando em sentimentos positivos, neutros e negativos
  if(q1[i,1]==0){
    cneu <- cneu + 1
  }else{
    if (q1[i,1] > 0){
      cpos <- cpos + 1
    }else{
      cneg <- cneg + 1
    }  
    }
  }
vsen <-data.frame(c(cpos,cneu,cneg),c("positivos","neutros","negativos"))
names(vsen) <- c("score","sentiment")
ggplot(data=vsen,aes(x=sentiment,y=score))+geom_bar(aes(fill=sentiment),stat="identity")+ #gráfico dos sentimentos
  theme(legend.position = "none")+
  xlab("Sentimentos")+ylab("Quantidade de termos")+ggtitle("Sentimentos captados no Twitter sobre a explosão em Beirute")
