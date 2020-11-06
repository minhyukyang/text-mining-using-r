## Text Mining - 02. 클리앙(Clien)

# - 참고 : https://r-pyomega.tistory.com/11
# - 클리앙 데이터 수집
# - 명사 빈도 분석
# - 워드 클라우드

# 1) url 검색
# 클리앙에 접속하여 검색어 입력후 url 추출
# - 기본 : https://www.clien.net/service/
# - 핸드폰 검색 후 : https://www.clien.net/service/search?q=%ED%95%B8%EB%93%9C%ED%8F%B0

# 2) url 분리
# - 기본 : https://www.clien.net/service/search
# - 검색어 : q=핸드폰

# 다음 페이지 
# https://www.clien.net/service/search?q=핸드폰&sort=recency&p=1&boardCd=&isBoard=false
# https://www.clien.net/service/search?q=%ED%95%B8%EB%93%9C%ED%8F%B0&sort=recency&p=1&boardCd=&isBoard=false

# url 분리
# - 기본 : https://www.clien.net/service/search
# - 검색어 : q=핸드폰
# - 정렬(최신순) : sort=recency 
# - 페이지 번호 : p=1
# - 기타 : boardCd=, isBoard=false

# 다시 첫 페이지
# https://www.clien.net/service/search?q=%ED%95%B8%EB%93%9C%ED%8F%B0&sort=recency&p=0&boardCd=&isBoard=false
# 페이지 번호가 0으로 시작하는 것 외엔 다른점 없음 -> 첫페이지 번호가 0으로 시작해야 함

# # HTTP 요청 및 응답
# keyword <- "핸드폰"
# res_cl <- GET(url = "https://www.clien.net/service/search", 
#               query = list(q = keyword, 
#                            sort = "recency", 
#                            p = 0, 
#                            boardCd = "",              ## boardCd: 에는 정보가 없는데, ""로 처리하면 됩니다
#                            isBoard = "false"))

# 크롤링 구조 설명
# 1) 1페이지의 링크를 수집
# 2) 수집한 링크를 타고가서 해당요소를 수집
# 3) for문(자동화)으로 검색한 결과 전체 크롤링


# 0. 분석 환경 세팅 -------------------------------------------------------------

# 필수 패키지 설치
# install.packages(c("dplyr", "httr", "jsonlite", "rJava", "RSelenium", "stringr"))

library(httr)
library(rvest)
library(stringr)

library(jsonlite)
library(rJava)
library(RSelenium)

library(dplyr)


# 1. 데이터 수집 ---------------------------------------------------------------

# 링크 수집

n <- 1
keyword <- "핸드폰" 

link_cl <- c() 
date_cl <- c() 

for(i in 0:n){ 
  tryCatch({ 
    res_cl <- GET(url = "https://www.clien.net/service/search", 
                  query = list(q = keyword, 
                               sort = "recency", 
                               p = i, 
                               boardCd = "", 
                               isBoard = "false")) 
    
    cat('현재', i, '페이지 수집 중! 상태코드는', status_code(x = res_cl), '입니다.\n') 
    
    ##link 
    link_tmp <- res_cl %>%
      read_html() %>%
      html_nodes("a.subject_fixed") %>%
      html_attr("href") %>%
      unique()
    
    link_tmp <- paste0("https://www.clien.net",link_tmp) 
    link_cl <- append(link_tmp,link_cl)  
    
    ##date
    date_tmp <- res_cl %>%  
      read_html() %>%  
      html_nodes("span.timestamp") %>% 
      html_text()  
    
    if (length(date_tmp) == 0) { 
      date_cl <- append(date_cl, "수동확인") 
    } else { 
      date_cl <- append(date_cl, date_tmp) 
    } 
    
    Sys.sleep(time = 1)  ## (중요!) 반복되는 작업으로 디도스(DDOS)로 오인 받지 않을려면 반드시 넣습니다!!
    
  }, error = function(e) cat("불러올 수 없습니다!\n")) 
  
} 


### link 분할 

link_clsp <- link_cl 
link_cl_url <- c() 
link_clsp <- str_replace_all(link_clsp,"&sort=recency&boardCd=&isBoard=false","") 
link_clsp <- strsplit(link_clsp, split="\\?") 

for(i in 1:length(link_clsp)){ 
  
  link_cl_url.tmp <- link_clsp[[i]][1] 
  link_cl_url <- append(link_cl_url, link_cl_url.tmp) 

} 

## element 수집 

title_cl <- c() 
content_cl <- c() 
comment_cl <- c() 
click_cnt_cl <- c() 
comment_cnt_cl <- c() 
address_cl <- c() 

keyword <- "핸드폰" 

for(i in 1:length(link_cl_url)){ 
  tryCatch({ 
    res_cl <- GET(url = link_cl_url[i], 
                  query = list(q = keyword, 
                               sort = "recency", 
                               p = 0, 
                               boardCd = "", 
                               isBoard = "false")) 
    
    cat('현재', i, '페이지 수집 중! 상태코드는', status_code(x = res_cl), '입니다.\n') 
    
    
    ##제목 
    
    title.tmp <- res_cl %>%  
      read_html() %>%  
      html_nodes("h3.post_subject") %>%  
      html_text() 
    
    if (length(title_cl) == 0) { 
      title_cl <- append(title_cl, "수동확인") 
    } else { 
      title_cl <- append(title_cl, title.tmp) 
    } 
    
    
    
    ##본문 
    
    content.tmp <- res_cl %>%  
      read_html() %>%  
      html_nodes("div.post_article") %>%  
      html_text() 
    
    if (length(content_cl) == 0) { 
      content_cl <- append(content_cl, "수동확인") 
    } else { 
      content_cl <- append(content_cl, content.tmp) 
    } 
    
    
    
    ##댓글 
    
    comment.tmp <- res_cl %>%  
      read_html() %>%  
      html_nodes("div.comment_view") %>%  
      html_text() 
    
    if (length(comment_cl) == 0) { 
      comment_cl <- append(comment_cl, "수동확인") 
    } else { 
      comment_cl <- append(comment_cl, comment.tmp) 
    } 
    
    
    
    ##조회수 
    
    click_cnt.tmp <- res_cl %>%  
      read_html() %>%  
      html_nodes("span.view_count") %>%  
      html_text() 
    
    if (length(click_cnt_cl) == 0) { 
      click_cnt_cl <- append(click_cnt_cl, "수동확인") 
    } else { 
      click_cnt_cl <- append(click_cnt_cl, click_cnt.tmp) 
    } 
    
    ##댓글수 
    
    comment_cnt.tmp <- res_cl %>%  
      read_html() %>%   
      html_nodes('div.comment_head') %>% 
      html_text() 
    
    if (length(comment_cnt_cl) == 0) { 
      comment_cnt_cl <- append(comment_cnt_cl, "수동확인") 
    } else { 
      comment_cnt_cl <- append(comment_cnt_cl, comment_cnt.tmp) 
    } 
    
    ##주소 
    address_cl <- append(address_cl, link_cl[i]) 
    
    Sys.sleep(time = 1)  ## (중요!) 반복되는 작업으로 디도스(DDOS)로 오인 받지 않을려면 반드시 넣습니다!!
    
  }, error = function(e) cat("불러올 수 없습니다!\n")) 
} 

## triming 
# https://colab.research.google.com/drive/1FfhWsP9izQcuVl06P30r5cCxELA1ciVE?usp=sharing#scrollTo=fVSpOZIoYOOJ
# review = re.sub(r'[@%\\*=()/~#&\+á?\xc3\xa1\-\|\.\:\;\!\-\,\_\~\$\'\"]', '',str(texts[i])) #remove punctuation
# review = re.sub(r'\d+','', str(texts[i]))# remove number
# review = review.lower() #lower case
# review = re.sub(r'\s+', ' ', review) #remove extra space
# review = re.sub(r'<[^>]+>','',review) #remove Html tags
# review = re.sub(r'\s+', ' ', review) #remove spaces
# review = re.sub(r"^\s+", '', review) #remove space from start
# review = re.sub(r'\s+$', '', review) #remove space from the end

title_cl <- str_replace_all(title_cl, "\n|\t", "")
content_cl <- str_replace_all(content_cl, "\n|\t", "")
click_cnt_cl <- str_replace_all(click_cnt_cl, "\\D", "")
comment_cl <- str_replace_all(comment_cl, "\n|\t", "")
comment_cnt_cl <- str_replace_all(comment_cnt_cl, "\\D", "")

## data.frame 

clien_contents <- data.frame(date_cl, title_cl, content_cl, click_cnt_cl, comment_cnt_cl, address_cl)   
# clien_comments <- data.frame(comment_cl)  

# write.csv(clien_갤럭시폴드_본문, file = "D:/clien_갤럭시폴드_본문.csv", row.names=FALSE) 
# write.csv(clien_갤럭시폴드_댓글, file = "D:/clien_갤럭시폴드_댓글.csv", row.names=FALSE