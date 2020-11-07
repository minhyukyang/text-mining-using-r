#########################################
# 02. 클리앙(Clien) 데이터 수집 및 분석 #
#########################################

# 1. 클리앙 URL 구조 분석
# 2. 데이터 수집
# 3. 데이터 전처리 및 워드 클라우드

# 1. URL 구조 분석  -----------------------------------------------------------

# 1) 클리앙에 접속하여 검색어 입력후 URL 추출
# - 클리앙 사이트 : https://www.clien.net/
# - 핸드폰 검색 후 : https://www.clien.net/service/search?q=핸드폰

# - 페이지 번호 변경  
# 1page : https://www.clien.net/service/search?q=핸드폰
# 2page : https://www.clien.net/service/search?q=핸드폰&sort=recency&p=1&boardCd=&isBoard=false
# 3page : https://www.clien.net/service/search?q=핸드폰&sort=recency&p=2&boardCd=&isBoard=false
# -> 1page 재확인 : https://www.clien.net/service/search?q=핸드폰&sort=recency&p=0&boardCd=&isBoard=false

# 2) URL 분리
# - 기본 : https://www.clien.net/service/search
# - 검색어 : q=핸드폰
# - 정렬(최신순) : sort=recency 
# - 페이지 번호 : p=1
# - 기타 : boardCd=, isBoard=false


# 2. 데이터 수집 ---------------------------------------------------------------

# 크롤링 구조 설명
# 1) 링크 수집
# 2) 수집한 링크를 타고가서 데이터 추출 

library(httr)
library(rvest)
library(stringr)
library(dplyr)

# 1) 링크 수집

n <- 10
keyword <- "핸드폰" 

clien_link <- c() 
clien_date <- c() 

for(i in 0:n) {
  tryCatch({
    crawling_result <- GET(
      url = "https://www.clien.net/service/search",
      query = list(
        q = keyword,
        sort = "recency",
        p = i,
        boardCd = "",
        isBoard = "false"
      )
    )
    
    cat("- 페이지 수집 중 : ", i, " / ", n, "\n")
    
    # LINK 정보 수집
    link_tmp <- crawling_result %>%
      read_html() %>%
      html_nodes("a.subject_fixed") %>%
      html_attr("href") %>%
      unique()
    
    link_tmp <- paste0("https://www.clien.net", link_tmp)
    clien_link <- append(link_tmp, clien_link)
    
    # DATE 정보 수집
    date_tmp <- crawling_result %>%
      read_html() %>%
      html_nodes("span.timestamp") %>%
      html_text()
    
    if (length(date_tmp) == 0) {
      clien_date <- append(clien_date, "check")
    } else {
      clien_date <- append(clien_date, date_tmp)
    }
    
    Sys.sleep(time = 1)  ## (중요!) 반복되는 작업으로 디도스(DDOS)로 오인 받지 않을려면 반드시 넣습니다!!
    
  }, error = function(e)
    cat("[ERROR] 페이지를 불러올 수 없습니다!\n"))
  
}

# 수집 결과 확인
length(clien_link);head(clien_link); 
length(clien_date);head(clien_date);

# 일자별 키워드 언급량 분석
library(ggplot2)

clien_daily <- as.Date(clien_date, format = '%Y-%m-%d')
clien_daily <- tibble(clien_daily)
clien_daily_count <- clien_daily %>% 
  group_by(clien_daily) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  ggplot(data = ., aes(x = clien_daily, y = count)) + 
    geom_bar(stat="identity", fill="steelblue") + geom_text(aes(label = count), vjust=1.6, color="white") + theme_bw()
clien_daily_count

# 일반 URL 형식으로 수정
clien_link_url <- c() 
clien_link_base <- str_replace_all(clien_link,"&sort=recency&boardCd=&isBoard=false","") 
clien_link_base <- strsplit(clien_link_base, split="\\?")  # 물음표(?)로 URL 분리

# 새로운 URL 리스트 생성
for(i in 1:length(clien_link_base)){ 
  
  clien_link_url_tmp <- clien_link_base[[i]][1] 
  clien_link_url <- append(clien_link_url, clien_link_url_tmp) 

} 
length(clien_link_url);head(clien_link_url); 

# 2) 수집한 링크를 타고가서 데이터 추출 

# 수집할 데이터 변수 생성
clien_title <- c() 
clien_content <- c() 
clien_comment <- c() 
clien_click_count <- c() 
clien_comment_count <- c() 
clien_address <- c() 

keyword <- "핸드폰" 

for(i in 1:length(clien_link_url)) {
  tryCatch({
    crawling_result <- GET(
      url = clien_link_url[i],
      query = list(
        q = keyword,
        sort = "recency",
        p = 0,
        boardCd = "",
        isBoard = "false"
      )
    )
    
    cat("- 페이지 수집 중 : ", i, " / ", length(clien_link_url), "\n")
    
    # 제목
    title_tmp <- crawling_result %>%
      read_html() %>%
      html_nodes("h3.post_subject") %>%
      html_text()
    
    if (length(clien_title) == 0) {
      clien_title <- append(clien_title, "check")
    } else {
      clien_title <- append(clien_title, title_tmp)
    }
    
    # 본문
    content_tmp <- crawling_result %>%
      read_html() %>%
      html_nodes("div.post_article") %>%
      html_text()
    
    if (length(clien_content) == 0) {
      clien_content <- append(clien_content, "check")
    } else {
      clien_content <- append(clien_content, content_tmp)
    }
    
    # 댓글
    comment_tmp <- crawling_result %>%
      read_html() %>%
      html_nodes("div.comment_view") %>%
      html_text()
    
    if (length(clien_comment) == 0) {
      clien_comment <- append(clien_comment, "check")
    } else {
      clien_comment <- append(clien_comment, comment_tmp)
    }
    
    # 조회수
    click_cnt_tmp <- crawling_result %>%
      read_html() %>%
      html_nodes("span.view_count") %>%
      html_text()
    
    if (length(clien_click_count) == 0) {
      clien_click_count <- append(clien_click_count, "check")
    } else {
      clien_click_count <- append(clien_click_count, click_cnt_tmp)
    }
    
    # 댓글수
    comment_cnt_tmp <- crawling_result %>%
      read_html() %>%
      html_nodes('div.comment_head') %>%
      html_text()
    
    if (length(clien_comment_count) == 0) {
      clien_comment_count <- append(clien_comment_count, "check")
    } else {
      clien_comment_count <- append(clien_comment_count, comment_cnt_tmp)
    }
    
    # 주소
    clien_address <- append(clien_address, clien_link[i])
    
    Sys.sleep(time = 1)  ## (중요!) 반복되는 작업으로 디도스(DDOS)로 오인 받지 않을려면 반드시 넣습니다!!
    
  }, error = function(e)
    cat("[ERROR] 페이지를 불러올 수 없습니다!\n"))
} 

## ref. triming 
# https://colab.research.google.com/drive/1FfhWsP9izQcuVl06P30r5cCxELA1ciVE?usp=sharing#scrollTo=fVSpOZIoYOOJ
# review = re.sub(r'[@%\\*=()/~#&\+á?\xc3\xa1\-\|\.\:\;\!\-\,\_\~\$\'\"]', '',str(texts[i])) #remove punctuation
# review = re.sub(r'\d+','', str(texts[i]))# remove number
# review = review.lower() #lower case
# review = re.sub(r'\s+', ' ', review) #remove extra space
# review = re.sub(r'<[^>]+>','',review) #remove Html tags
# review = re.sub(r'\s+', ' ', review) #remove spaces
# review = re.sub(r"^\s+", '', review) #remove space from start
# review = re.sub(r'\s+$', '', review) #remove space from the end


# 3. 데이터 전처리 및 워드 클라우드 ----------------------------------------------------

# 1) 데이터 전처리 하기
# 2) 명사 추출 및 빈도 분석
# 3) 워드 클라우드

# 1) 데이터 전처리 하기

head(clien_title)
head(clien_content, 2)
head(clien_click_count, 2)
head(clien_comment, 2)
head(clien_comment_count, 2)

clien_title <- str_replace_all(clien_title, "\n|\t", "")
clien_content <- str_replace_all(clien_content, "\n|\t", "")
clien_click_count <- str_replace_all(clien_click_count, "\\D", "")
clien_comment <- str_replace_all(clien_comment, "\n|\t", "")
clien_comment_count <- str_replace_all(clien_comment_count, "\\D", "")

# 결과 데이터셋 만들기
clien_contents <- data.frame(clien_date, clien_title, clien_content, clien_click_count, clien_comment_count, clien_address)   
# clien_comments <- data.frame(clien_comment)  
range(clien_contents$clien_date)

# 2) 명사 추출 및 빈도 분석

# 주요 키워드 분석 
library(KoNLP)
# new_term <- c("혁신성장전략회", "산업통상자원부")
# new_term <- c("독일차", "국토교통부","수입차","가격","현기차","수리비","오일","카톡")
# new_dic <- data.frame(new_term , "ncn")

buildDictionary(ext_dic = c('sejong', 'woorimalsam', 'insighter'))
## ???????? words dictionary was built.

# 명사 추출
clien_nouns <- lapply(clien_contents$clien_content, extractNoun)

# 단어 빈도 카운트
clien_nouns_vec <- do.call(c, clien_nouns) # 벡터화
clien_nouns_count <- table(clien_nouns_vec)

# 단어 전처리 
clien_nouns_count <- clien_nouns_count[nchar(names(clien_nouns_count)) > 1] # 1글자 이하 제거
# clien_nouns_count <- clien_nouns_count[nchar(names(clien_nouns_count)) > 1 & names(clien_nouns_count)!="핸드폰"] # 검색 키워드 제거
sort(clien_nouns_count, decreasing = T)[1:10]

# 3) 워드 클라우드

library(wordcloud2)
# x11()

wordcloud2(data = sort(clien_nouns_count, decreasing = T), minSize = 5) # 기본
wordcloud2(data = sort(clien_nouns_count, decreasing = T), minSize = 20, color = "random-light") # 랜덤라이트 적용

