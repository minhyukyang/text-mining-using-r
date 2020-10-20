## 네이버 뉴스 분석
# 검색 키워드 : 코로나
# 날짜 : 2020-10-05

# 1. 데이터 수집 ------------------------------------------------------------

# 웹크롤링 코드
if (!require("rvest")) install.packages("rvest")
library(rvest)

# 페이지 단위로 정보 추출(list형태)
keyword <- "코로나"

n_news <- lapply(1:10, function(page){
  
  # 2020-10-05 날짜 고정
  # url <- paste0("https://search.naver.com/search.naver?where=news&query=코로나&sm=tab_opt&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2020.10.05&de=2020.10.05&docid=&nso=so%3Ar%2Cp%3Afrom20201005to20201005%2Ca%3Aall&mynews=0&refresh_start=1&related=0")
  url <- paste0("https://search.naver.com/search.naver?where=news&query=", keyword, 
                "&sm=tab_opt&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=2020.10.05&de=2020.10.05&docid=&nso=so%3Ar%2Cp%3Afrom20201005to20201005%2Ca%3Aall&mynews=0&refresh_start=", page,"&related=0")
  
  nr_table  <- read_html(url)
  
  title <- html_nodes(nr_table, xpath='//*[@class="type01"]/li/dl/dt/a')
  title <- html_text(title)
  
  sources <- html_nodes(nr_table, xpath='//*[@class="type01"]/li/dl/dd[1]/span[1]')
  sources <- html_text(sources)
  
  contents <- html_nodes(nr_table, xpath='//*[@class="type01"]/li/dl/dd[2]')
  contents <- html_text(contents)
  
  data.frame(title, sources, contents)
})

# Row 단위로 데이터 통합(list형태 해제)
n_news <- do.call(rbind, n_news) # rbind : row 결합

# n_news 변수의  확인
head(n_news)

# 2. 텍스트 처리 ---------------------------------------------------------------

# 2.1 문장부호 제거 -------------------------------------------------------------

# 정규표현식 예제
text <- "가나다 abc 123 !@#"
gsub("[[:alpha:]]", " ", text)
gsub("[[:digit:]]", " ", text)
gsub("[[:punct:]]", " ", text)

# 검토(비교)
head(n_news$contents, 1) # 기본
head(gsub("[[:punct:]]", " ", n_news$contents), 1) # 문장부호 제거

# 적용 - 문장부호 제거 후 contents 변수에 저장
contents <- gsub("[[:punct:]]", " ", n_news$contents)
head(contents) 

# 2.2 숫자 제거 ---------------------------------------------------------------

# 검토(비교)
head(contents, 3) # 기본
head(gsub("[[:digit:]]", " ", contents), 3) # 숫자 제거

# 적용 - 숫자 제거 후 contents 변수에 저장
contents <- gsub("[[:digit:]]", "", contents)
head(contents)

# 2.3 명사 추출 ---------------------------------------------------------------
# KoNLP 패키지 설치 (2020-10-06 CRAN 설치 불가)
# https://gumu.kr/blog/1010/r-konlp%ED%8C%A8%ED%82%A4%EC%A7%80-%EC%84%A4%EC%B9%98-%EC%98%A4%EB%A5%98fail-to-install-scala-library-2-11-8-jar/
# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_261")
# install.packages(c("rJava", "hash", "tau", "Sejong", "RSQLite"))
# install.packages("https://cran.r-project.org/src/contrib/Archive/KoNLP/KoNLP_0.80.2.tar.gz", repos=NULL, type="source", INSTALL_opts = c('--no-lock'))
# https://github.com/haven-jeon/KoNLP/blob/master/etcs/KoNLP-API.md
if (!require("KoNLP")) install.packages("KoNLP")

useNIADic()
## Backup was just finished!
## 983012 words dictionary was built.

# 명사 추출 : extractNoun 함수 사용
out1 <- lapply(contents, extractNoun)

# 비교
head(contents, 3) # 원문
out1[1:3] # 명사 추출 결과

# 2.4 명사 추출(사용자 사전 추가) ----------------------------------------------------

# 예제 : 사용자 사전 적용 전
extractNoun("방역당국은 국내에서도 다기관염증증후군 환자 2명을 처음으로 공식 확인했다고 밝혔습니다.")

# 사용자 단어 추가 
new_term <- c("코로나","다기관염증증후군", "진단검사", "방역당국")
new_dic <- data.frame(new_term , "ncn")

buildDictionary(ext_dic = c('sejong', 'woorimalsam'), user_dic = new_dic, replace_usr_dic = T)

# 예제 : 사용자 사전 적용 후
extractNoun("방역당국은 국내에서도 다기관염증증후군 환자 2명을 처음으로 공식 확인했다고 밝혔습니다.")

# 명사 추출 : 사용자 단어가 추가된 사전 사용
out2 <- lapply(contents, extractNoun)

# # 비교
# head(out1, 3)
# out1[1:3] # 사용자 사전 추가 X 
# out2[1:3] # 사용자 사전 추가 O

# 2.5 빈도 분석 ---------------------------------------------------------------

# 단어 빈도 - 사용자사전 적용
out_v1 <- do.call(c, out2)
# out_v <- do.call(c, out2)
wt <- table(out_v1)
wt <- wt[nchar(names(wt)) > 1]
data.frame(sort(wt, decreasing = T)[1:10])

# 단어 빈도 - 검색키워드 제거
out_v2 <- do.call(c, out2)
# out_v <- do.call(c, out2)
wt2 <- table(out_v2)
wt2 <- wt2[nchar(names(wt2)) > 1]
wt2 <- wt2[nchar(names(wt2)) > 1 & names(wt2)!="코로나"] # 검색 키워드를 제거하기
data.frame(sort(wt2, decreasing = T)[1:10])


# 3. 시각화 ------------------------------------------------------------------

# wordcloud
# x11()
if (!require("wordcloud")) install.packages("wordcloud")
pdf.options(family = "Korea1deb")
pal <- brewer.pal(8,"Dark2")
wordcloud(words=names(wt), freq=wt, min.freq=5, random.order=F, random.color=T, colors=pal) # 사용자사전 적용
wordcloud(words=names(wt2), freq=wt2, min.freq=5, random.order=F, random.color=T, colors=pal) # 검색키워드 제거

# wordcloud2
# x11()
if (!require("wordcloud2")) install.packages("wordcloud2")
wordcloud2(data = sort(wt, decreasing = T), minSize = 5) # 기본
wordcloud2(data = sort(wt2, decreasing = T), minSize = 5) # 검색키워드 제거
wordcloud2(data = sort(wt2, decreasing = T), minSize = 5, rotateRatio= 0) # 단어구름 각도변화
wordcloud2(data = sort(wt2, decreasing = T), minSize = 5, color = "random-light") # 랜덤라이트 적용
wordcloud2(data = sort(wt2, decreasing = T), minSize = 5, color = "random-light", fontFamily = '나눔바른고딕') # 폰트 적용

# 4. N2H4 : 네이버 뉴스 및 댓글 수집 비공식 패키지 ------------------------------------------------

# N2H4 패키지 설치 
if (!require("devtools")) install.packages("devtools")
devtools::install_github("forkonlp/N2H4")

library(N2H4)

# 뉴스내용 수집
# n_url = "https://news.naver.com/main/ranking/read.nhn?mid=etc&sid1=111&rankingType=popular_day&oid=001&aid=0010691079&date=20190313&type=1&rankingSeq=7&rankingSectionId=102"
n_url = "https://news.naver.com/main/ranking/read.nhn?mid=etc&sid1=111&rankingType=popular_day&oid=001&aid=0011923785&date=20201006&type=1&rankingSeq=6&rankingSectionId=101"
nn <- getContent(n_url)
nn

# 댓글 수집
x <- getAllComment(n_url)
x$contents[1:10]


# 5. KoSpacing : 한글 띄어쓰기 패키지 ----------------------------------------------

# R에서 한글 띄어쓰기 처리를 위한 패키지를 소개하는 페이지 입니다. 
# keras를 사용하기 위해 python이 설치되어 있어야 합니다.
# 링크 : https://mrchypark.github.io/post/kospacing-한글-띄어쓰기-패키지를-사용해보자/