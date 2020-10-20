# 0. 분석 환경 세팅 -------------------------------------------------------------
# - twitter에서 데이터를 수집하기 위해서는 먼저 twitter에서 개발자 인증을 받아야 합니다.
# - 참고 : http://www.evernote.com/l/ANJAXXo73qtGc5Ncsa-uTWmsE4zDdoS-9VA/
# - 트위터 API v1.1 Docu. : https://developer.twitter.com/en/docs/twitter-api/v1
source("script/ini.r")

# Twitter API URL
my_reqURL <- "https://api.twitter.com/oauth/request_token"
my_accessURL <- "https://api.twitter.com/oauth/access_token"
my_authURL <- "https://api.twitter.com/oauth/authorize"

# Twitter developer key and token
my_consumerKey <- twitter_consumerKey
my_consumerSecret <- twitter_consumerSecret
my_accesstoken <- twitter_accesstoken
my_accesstokensecret <- twitter_accesstokensecret

# # sentiment scoring
# score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
# {
#   require(plyr)
#   require(stringr)
#   
#   # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
#   # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
#   scores = laply(sentences, function(sentence, pos.words, neg.words) {
#     
#     # clean up sentences with R's regex-driven global substitute, gsub():
#     sentence = gsub('[[:punct:]]', '', sentence)
#     sentence = gsub('[[:cntrl:]]', '', sentence)
#     sentence = gsub('\\d+', '', sentence)
#     # and convert to lower case:
#     sentence = tolower(sentence)
#     
#     # split into words. str_split is in the stringr package
#     word.list = str_split(sentence, '\\s+')
#     # sometimes a list() is one level of hierarchy too much
#     words = unlist(word.list)
#     
#     # compare our words to the dictionaries of positive & negative terms
#     pos.matches = match(words, pos.words)
#     neg.matches = match(words, neg.words)
#     
#     # match() returns the position of the matched term or NA
#     # we just want a TRUE/FALSE:
#     pos.matches = !is.na(pos.matches)
#     neg.matches = !is.na(neg.matches)
#     
#     # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
#     score = sum(pos.matches) - sum(neg.matches)
#     
#     return(score)
#   }, pos.words, neg.words, .progress=.progress )
#   
#   scores.df = data.frame(score=scores, text=sentences)
#   return(scores.df)
# }

# 1. Twitter 에서 데이터 가져오기 --------------------------------------------------

# 패키지 설치nstall.packages("base64enc")
install.packages("RCurl")
install.packages("twitteR")
install.packages("ROAuth")

# 패키지 로드
library(base64enc)
library(RCurl)
library(twitteR)
library(ROAuth)

# 1.1 환경 설정 ####

# 트위터 API URL 설정
reqURL <- my_reqURL
accessURL <- my_accessURL
authURL <- my_authURL

# 트위터에서 받은 네 개의 키 값을 변수에 할당
consumerKey <- my_consumerKey
consumerSecret <- my_consumerSecret
accesstoken <- my_accesstoken
accesstokensecret <- my_accesstokensecret

# 인증키 설정 및 다운
options(RCurloptions = list(cainfo = system.file("CurlSSL", "cacert.pem", packge = "RCurl")))
download.file(url = "https://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")

# 인증처리
setup_twitter_oauth(consumerKey, consumerSecret, accesstoken, accesstokensecret)

# 1.2 트위터 데이터 수집 ####
# https://r-pyomega.tistory.com/16

getCurRateLimitInfo()

# 계정이 남긴 글을 가져오고 싶을 때 사용
# SeoulJazzFest.tweets <- searchTwitter("@inhauniv") # @ 계정, n 가져올 데이터 건수
# SeoulJazzFest.tweets
# save(SeoulJazzFest.tweets, file="c:/R/SeoulJazzFest.tweets.Rdata") # save a file

# 해당 단어를 언급한 글을 가져오고 싶을 때 사용
keyword <- enc2utf8("인하대") # 키워드 지정
tweets_inha <- searchTwitter(keyword, n=500)

tweets_inha_df <- twListToDF(tweets_inha)
str(tweets_inha_df)
tweets_inha_df %>% filter(isRetweet == TRUE)
tweets_inha_df %>% filter(isRetweet == FALSE)

# 2. 데이터 감성분석 ####
# - 감성분석을 위해서 필요한 패키지는 형태소 분석을 위한 KoNLP 패키지와
#   Java와의 연동을 위한 rJava 패키지 입니다. (KoNLP가 Java기반으로 짜여져 있습니다.)
# - 따라서, JRE가 설치된 경로를 설정해 주어야 합니다.

# 2.1 환경설정

# 패키지 설치
install.packages("rJava")
install.packages("KoNLP")

# 패키지 로드
# - 다운로드 : https://java.com/ko/
# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_65") # for 64-bit version
library(rJava)
library(KoNLP)
library(tidyverse)

# 2.2 긍부정 사전 로드

positive.dic <- readLines("dic/positive_words.txt", encoding="UTF-8") #긍정사전 로딩
negative.dic <- readLines("dic/negative_words.txt", encoding="UTF-8") #부정사전 로딩

# Twitter 데이터 중 Text 추출
# head(Busanhang.tweets)
tweets <- data.frame()
for(i in 1:length(tweets_inha)){
  if(i==1){
    tweets <- tweets_inha[[i]]$text
  } else {
    tweets.tmp <- tweets_inha[[i]]$text
    tweets <- rbind(tweets, tweets.tmp)
  }
}
row.names(tweets) <- NULL

# 데이터 전처리
tweets.1 <- gsub("RT", "", tweets)
tweets.1 <- gsub("ㅠ", "", tweets.1)
tweets.1 <- gsub("ㅋ", "", tweets.1)
tweets.1 <- gsub("ㄱ", "", tweets.1)
tweets.1 <- gsub('[[:punct:]]', '', tweets.1) #특수기호 제거
tweets.1 <- gsub('[[:cntrl:]]', '', tweets.1) #시스템문자 제거
tweets.1 <- gsub('\\d+', '', tweets.1) # 숫자 제거
tweets.1 <- gsub("[^[:alnum:]///' ]", "", tweets.1) # 특수기호 제거

# 데이터 명사 추출
tran <- Map(extractNoun, tweets.1)

# 데이터 전처리-2
tweets.2 <- gsub('[[:punct:]]', '', tran)
tweets.2 <- gsub('[[:cntrl:]]', '', tweets.2)
tweets.2 <- gsub('\\d+', '', tweets.2)
tweets.2 <- gsub("[^[:alnum:]///' ]", "", tweets.2)
tweets.2 <- gsub("[a-zA-Z]", "", tweets.2)
tweets.2 <- gsub("  ", "", tweets.2)

# 감성 분석
result <- score.sentiment(tweets.2, positive.dic, negative.dic)
result[result$score!=0,] # 감성분석에 걸린 결과 확인 

# 3. WordCloud 그려보기 ####

# 패키지 설치
install.packages("wordcloud")

# 패키지 로드
library(wordcloud2)

# 데이터 처리
twtTexts <- data.frame(tweets.2) # data.frame 타입으로 변경
twtTextsPated <- paste(twtTexts$tweets.2, collapse = " ") # 각 문장을 공백으로 연결

nouns <- extractNoun(twtTextsPated) # 명사 추출
nouns <- nouns[nchar(nouns)>=2] # 2글자 이상 필터링
# nouns <- nouns[is.hangul(nouns)] # 한글 필터링
cnouns <- count(nouns) # 명사 갯수 카운트

# Pal 선택
pal <- c("#5C6E00","#273B00","#F7DA00","#EB1200","#F78200") # Green-Orange

# pal2 <- brewer.pal(12,"Paired")
# 
# pal3 <-  brewer.pal(9, "RdPu")
# pal3 <- pal[-(1:4)] # Violet-Pink
# 
# pal4 <- brewer.pal(9,"BuGn") 
# pal4 <- pal[-(1:4)] # Green
# 
# pal5 <- brewer.pal(8,"Dark2")

# x11()
# display.brewer.all()
# par <- par[-(1)]
wordcloud2(cnouns)
# wordcloud(words=cnouns$x, scale=c(7,0.5), freq=cnouns$freq, colors=pal, min.freq=2,
#           use.r.layout=FALSE,random.order=F, family="malgun")

windowsFonts(malgun=windowsFont("맑은고딕"))
# windowsFonts(gungseo=windowsFont("궁서체"))
# windowsFonts(arial=windowsFont("Arial"))

wordcloud(words=cnouns$x, scale=c(7,0.5), freq=cnouns$freq, colors=pal2, min.freq=2,
          use.r.layout=FALSE,random.order=F, family="malgun")
