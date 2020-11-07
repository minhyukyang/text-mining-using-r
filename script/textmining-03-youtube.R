###########################################
# 03. 유투브(Youtube) 데이터 수집 및 분석 #
###########################################

# 1. 사전 환경 세팅
# 2. 데이터 수집
# 3. 데이터 전처리 및 워드 클라우드

# 1. 사전 환경 세팅 -------------------------------------------------------------

# 1) chromedriver 설치
# - 링크 : https://sites.google.com/a/chromium.org/chromedriver/
# * 현재 설치된 chrome 버전에 맞는 드라이버 다운로드

# 2) selenium 설치
# - 프로그램을 이용해 자동화된 웹 테스트를 수행할 수 있도록 해주는 프레임 워크 (https://namu.wiki/w/Selenium)
# - 다운로드 링크 : https://www.seleniumhq.org/download/
# * Selenium Server (Grid) > stable version 받기

# 3) 설치
# 1), 2)를 c:\rselenium 폴더에 넣기
# chrome drive는 동일 폴더에 압출 해제 하기

# 4) Selenium 서버 띄우기 
# 명령 프롬프트 실행 (단축키 : Ctrl + R > "cmd")
# 설치 폴더로 이동 : "cd c:\rselenium"
# 명령어 입력 : "java -jar selenium-server-standalone-3.141.59.jar -port 4445"
# -> "Selenium Server is up and running on port 4445" 이란 로그가 나타나면 성공


# 2. 데이터 수집 ---------------------------------------------------------------

library(httr)
library(rvest)
library(RSelenium)

# 1) 크롬 드라이버 띄우기

remDr <- remoteDriver(port = 4445L, # 포트번호 입력
                      browserName = "chrome") #사용할 브라우저

remDr$open() #서버에 연결
# remDr$closeall() #서버에 연결

# 2) 동영상 목록 정보 추출 

# youtube_inha_univ <- "https://www.youtube.com/watch?v=3hSb8wCvfcg"
youtube_inha_univ <- "https://www.youtube.com/user/inhauniversity/videos"
remDr$navigate(url = youtube_inha_univ)
# remDr$navigate(paste0("https://www.youtube.com/results?search_query=",title_you)) #해당 홈페이지로 이동 

# 페이지의 소스 읽어오기 
youtube_html <- remDr$getPageSource()[[1]]
youtube_html <- read_html(youtube_html) 

# TITLE 추출 
youtube_title <- youtube_html %>% html_nodes("#video-title") %>% html_text() #선택된 노드를 텍스트 화
youtube_title <- str_replace_all(youtube_title, "\n|\t", "") %>% trimws()

# URL 추출
youtube_url <- youtube_html %>% html_nodes("#video-title") %>% html_attr("href")
youtube_url <- paste0("https://www.youtube.com",youtube_url)
  
# 조회수 추출 
youtube_cnt <- youtube_html %>% html_nodes("#metadata-line > span:nth-child(1)") %>% html_text()

# 생성일 추출 
youtube_date <- youtube_html %>% html_nodes("#metadata-line > span:nth-child(2)") %>% html_text()

# 본문 추출 
youtube_contents <- data.frame(youtube_title, youtube_date, youtube_cnt, youtube_url)   

# 3) 영상별 페이지 정보 추출  

url <- youtube_url[1]
remDr$navigate(url) #이 홈페이지로 이동 

# 영상 플레이 버튼 클릭 (재생 멈춤)
btn <- remDr$findElement(using="css selector", 
                         value='#movie_player > div.ytp-chrome-bottom > div.ytp-chrome-controls > div.ytp-left-controls > button')
btn$clickElement()

# 정보 추출 
youtube_html <- remDr$getPageSource()[[1]]
youtube_html <- read_html(youtube_html) #페이지의 소스 읽어오기 

# 각각의 태그들로 추출을 한다.
video_title <- youtube_html %>% html_nodes(xpath = '//*[@id="container"]/h1/yt-formatted-string') %>% html_text()
upload_date <- youtube_html %>% html_nodes(xpath = '//*[@id="date"]/yt-formatted-string') %>% html_text()
view_cnt <- youtube_html %>% html_nodes(xpath = '//*[@id="count"]/yt-view-count-renderer/span[1]') %>% html_text()
like_cnt <- youtube_html %>% html_nodes(xpath = '//*[@id="top-level-buttons"]/ytd-toggle-button-renderer[1]') %>% html_text()
unlike_cnt <- youtube_html %>% html_nodes(xpath = '//*[@id="top-level-buttons"]/ytd-toggle-button-renderer[2]/a') %>% html_text()
comment_cnt <- youtube_html %>% html_nodes(xpath = '//*[@id="count"]/yt-formatted-string') %>% html_text()
print(video_title)
print(upload_date)
print(view_cnt)
print(like_cnt)
print(unlike_cnt)
print(comment_cnt)

# 댓글 정보 수집
num_of_pagedowns <- 2

webElem <- remDr$findElement("css", "body")

while(num_of_pagedowns){
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(4)
  num_of_pagedowns = num_of_pagedowns - 1
  print(num_of_pagedowns)
}

youtube_comments_username <- youtube_html %>% html_nodes("#author-text > span") %>% html_text() %>% str_replace_all("\n","") %>% trimws()
youtube_comments_date <- youtube_html %>% html_nodes("#header-author > yt-formatted-string > a") %>% html_text()
youtube_comments_message <- youtube_html %>% html_nodes("#content-text") %>% html_text() 
youtube_comments_like <- youtube_html %>% html_nodes("#vote-count-middle") %>% html_text()
# ytube_comments_unlike <- youtube_html %>% youtube_html_nodes("#vote-count-middle") %>%html_text()

youtube_contents <- data.frame(youtube_comments_username, youtube_comments_date, youtube_comments_message, youtube_comments_like)   


# 4) youtube_url 내 모든 정보 수집

remDr <- remoteDriver(port = 4445L, # 포트번호 입력
                      browserName = "chrome") #사용할 브라우저
remDr$open() #서버에 연결
# remDr$closeall() #서버에 연결

youtube_contents <- tibble()

num_of_pagedowns <- 2

# for(i in 1:length(youtube_url)){
for(i in 1:10){
  
  tml_url <- youtube_url[i]
  cat('[',i,'/',length(youtube_url),'] ', tml_url, "\n")
  
  # 페이지 이동
  remDr$navigate(url = tml_url)
  Sys.sleep(4)
  
  # 영상 플레이 버튼 클릭 (재생 멈춤)
  btn <- remDr$findElement(using="css selector", 
                           value='#movie_player > div.ytp-chrome-bottom > div.ytp-chrome-controls > div.ytp-left-controls > button')
  btn$clickElement()
  
  # 스크롤 내리기 (횟수=num_of_pagedowns)
  webElem <- remDr$findElement("css", "body")
  while(num_of_pagedowns){
    webElem$sendKeysToElement(list(key = "end"))
    Sys.sleep(4)
    num_of_pagedowns = num_of_pagedowns - 1
    # print(num_of_pagedowns)
  }
  
  # 정보 추출 
  youtube_html <- remDr$getPageSource()[[1]]
  youtube_html <- read_html(youtube_html) #페이지의 소스 읽어오기
  
  # 각각의 태그들로 추출을 한다.
  video_title <- youtube_html %>% html_nodes(xpath = '//*[@id="container"]/h1/yt-formatted-string') %>% html_text()
  cat("- video_title : ", video_title, "\n")
  
  upload_date <- youtube_html %>% html_nodes(xpath = '//*[@id="date"]/yt-formatted-string') %>% html_text()
  cat("- upload_date : ", upload_date, "\n")
  
  view_cnt <- youtube_html %>% html_nodes(xpath = '//*[@id="count"]/yt-view-count-renderer/span[1]') %>% html_text() %>% str_replace_all("\\D", "")
  cat("- view_cnt : ", view_cnt, "\n")
  
  like_cnt <- youtube_html %>% html_nodes(xpath = '//*[@id="top-level-buttons"]/ytd-toggle-button-renderer[1]') %>% html_text()
  cat("- like_cnt : ", like_cnt, "\n")

  unlike_cnt <- youtube_html %>% html_nodes(xpath = '//*[@id="top-level-buttons"]/ytd-toggle-button-renderer[2]/a') %>% html_text()
  cat("- unlike_cnt : ", unlike_cnt, "\n")

  comment_cnt <- youtube_html %>% html_nodes(xpath = '//*[@id="count"]/yt-formatted-string') %>% html_text() %>% str_replace_all("\\D", "")
  cat("- comment_cnt : ", comment_cnt, "\n")
  
  tmp_comment_list <- youtube_html %>% html_nodes("#content-text") %>% html_text()
  
  for(j in 1:length(tmp_comment_list)){
  
    comments_id <- j
    comments <- tmp_comment_list[j]
    youtube_contents_tmp <- tibble(video_title, upload_date, view_cnt, like_cnt, unlike_cnt, comment_cnt, comments, comments_id)
    youtube_contents <- bind_rows(youtube_contents, youtube_contents_tmp)
    # print(tmp_comment)
  }
  
}

# 3. 데이터 전처리 및 워드 클라우드 ----------------------------------------------------

# 1) 데이터 전처리 하기
# 2) 명사 추출 및 빈도 분석
# 3) 워드 클라우드

head(youtube_contents)

# 1) 데이터 전처리 하기
youtube_contents$comments <- str_replace_all(youtube_contents$comments, "[^[:alnum:]]", " ") 
youtube_contents <- youtube_contents %>% filter(!is.na(comments))

# youtube_contents <- data.frame(comments=youtube_comments_message)

# 2) 명사 추출 및 빈도 분석

# 주요 키워드 분석 
library(KoNLP)
buildDictionary(ext_dic = c('sejong', 'woorimalsam', 'insighter'))
## ???????? words dictionary was built.

# 명사 추출
youtube_nouns <- lapply(youtube_contents$comments, extractNoun)

# 단어 빈도 카운트
youtube_nouns_vec <- do.call(c, youtube_nouns) # 벡터화
youtube_nouns_count <- table(youtube_nouns_vec)

# 단어 전처리 
youtube_nouns_count <- youtube_nouns_count[nchar(names(youtube_nouns_count)) > 1] # 1글자 이하 제거
# youtube_nouns_count <- youtube_nouns_count[names(youtube_nouns_count)!="박지환"] # 특정 키워드 제거거
# youtube_nouns_count <- youtube_nouns_count[names(youtube_nouns_count)!="엄마"] # 특정 키워드 제거거
# clien_nouns_count <- clien_nouns_count[nchar(names(clien_nouns_count)) > 1 & names(clien_nouns_count)!="핸드폰"] # 검색 키워드 제거
sort(youtube_nouns_count, decreasing = T)[1:10]

# 3) 워드 클라우드

library(wordcloud2)
# x11()

wordcloud2(data = sort(youtube_nouns_count, decreasing = T), minSize = 5) # 기본
wordcloud2(data = sort(youtube_nouns_count, decreasing = T), minSize = 5, color = "random-light") # 랜덤라이트 적용



