###########################################
# 04. 트위터(twitter) 데이터 수집 및 분석 #
###########################################

# 1. 트위터 개발자 등록 
# 2. Twitter 에서 데이터 가져오기
# 3. 데이터 전처리
# 4. 워드 클라우드, 감성분석

# 1. 트위터 개발자 등록  --------------------------------------------------------
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

# 2. Twitter 에서 데이터 가져오기 --------------------------------------------------

library(twitteR)
library(tidyverse)
library(plyr)

# 1) 환경 설정

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
# options(RCurloptions = list(cainfo = system.file("CurlSSL", "cacert.pem", packge = "RCurl")))
download.file(url = "https://curl.haxx.se/ca/cacert.pem", destfile = "data/cacert.pem")

# 인증처리
setup_twitter_oauth(consumerKey, consumerSecret, accesstoken, accesstokensecret)

# 2) 트위터 데이터 수집
# https://developer.twitter.com/en/docs/twitter-api/v1/developer-utilities/rate-limit-status/api-reference/get-application-rate_limit_status

# API로 제공되는 정보 확인
getCurRateLimitInfo() 

# 해당 단어를 언급한 글을 가져오고 싶을 때 사용
keyword <- enc2utf8("인하대") # 키워드 지정

# tweets_inha <- searchTwitter(keyword, n=500, lang="ko", since="2020-01-01", until="2020-10-23")
# keyword_ko <- enc2utf8( )에 할당한 키워드
# n = 가져오는 게시글 개수
# lang = 언어 (국어 ="kr" / 영어 = "en" / 일본어 = "ja)
# since = 시작일
# until = 종료일

tweets_inha <- searchTwitter(keyword, since="2020-11-01", until="2020-11-08") # 최근 500개 tweet 수집
tweets_inha_df <- twListToDF(tweets_inha) # DataFrame 형태로 변경
str(tweets_inha_df)

tweets_inha_df %>% filter(isRetweet == TRUE) # 리트윗
tweets_inha_df %>% filter(isRetweet == FALSE)

# 3. 데이터 전처리  ----------------------------------------------------

# 패키지 로드
# - 다운로드 : https://java.com/ko/
# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_65") # for 64-bit version
# library(rJava)
library(KoNLP)

# Twitter 데이터 중 Text 추출
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
# review = re.sub(r'<[^>]+>','',review) #remove Html tags
tweets_1 <- str_replace_all(tweets, "http\\S+", " ")
tweets_1 <- str_replace_all(tweets_1, "RT|ㅠ|ㅋ|ㄱ|\n|\t", " ")
tweets_1 <- str_replace_all(tweets_1, "[[:punct:]]", " ")
tweets_1 <- str_replace_all(tweets_1, "[[:cntrl:]]", " ")
tweets_1 <- str_replace_all(tweets_1, "[^[:alnum:]///' ]", " ")

tweets_1 <- data.frame(text=tweets_1)

# 명사 추출
tweet_nouns <- lapply(tweets_1$text, extractNoun)

# 단어 빈도 카운트
tweet_nouns_vec <- do.call(c, tweet_nouns) # 벡터화
tweet_nouns_count <- table(tweet_nouns_vec)

# 단어 전처리 
tweet_cnouns <- tweet_nouns_count[nchar(names(tweet_nouns_count)) > 1] # 1글자 이하 제거
# tweet_cnouns <- tweet_cnouns[names(tweet_cnouns)!="마사지"] # 특정 키워드 제거거
# youtube_contents %>% filter(str_detect(youtube_contents$comments, '박지환')) # 특정 키워드 확인
sort(tweet_cnouns, decreasing = T)[1:10]

# 4. 워드 클라우드, 감성분석 --------------------------------------------------------

# 1) WordCloud 그려보기

# install.packages("wordcloud2")
library(wordcloud2)

wordcloud2(data = sort(tweet_cnouns, decreasing = T), minSize = 5) # 기본
wordcloud2(data = sort(tweet_cnouns, decreasing = T), minSize = 10, color = "random-light") # 랜덤라이트 적용

# 2) 긍부정 분석
# remotes::install_github("mrchypark/KnuSentiLexR")
# https://github.com/mrchypark/KnuSentiLexR/blob/master/data-raw/SentiWord_info.json

## KnuSentiLex는 군산대 Data Intelligence Lab에서 기존 사전들을 참조, 활용하여 
## 18년 구축한 감성 사전. 구조가 단순하고 이모티콘 등을 추가한 점이 장점인 반면, 
## 형태소 형식이 아니라 점수의 신뢰도가 낮은 편임.

library(KnuSentiLexR)
library(tidytext)

tweets_inha_df %>% 
  unnest_tokens(sent, text, token = "sentences") %>% 
  # filter(nchar(sent) < 50) %>% 
  select(sent) -> 
  senti_inha

senti_inha %>% 
  mutate(score = senti_score(sent),
         magni = senti_magnitude(sent)) %>% 
  filter(score != 0) ->
  senti_inha_result

view(senti_inha_result)
