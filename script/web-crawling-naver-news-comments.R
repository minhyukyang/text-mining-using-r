if (!require("devtools")) install.packages("devtools")
devtools::install_github("forkonlp/N2H4")

library(rvest)
library(KoNLP)
library(dplyr)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(N2H4)

# 뉴스내용 수집
# ----------------------------------------------------------------------------------
# 현재 코드에서는 getContent 함수로 본문 수집이 되지 않고 있습니다.
# 접속해 보니 페이지에 컨텐츠가 숨겨져 있는 페이지 였습니다. -> `본문보기`버튼
# `본문보기`를 했을때의 링크가 변경되었고 해당 링크로 작업하였습니다.
# ----------------------------------------------------------------------------------

# n_url = "http://news.naver.com/main/read.nhn?m_view=1&includeAllCount=true&mode=LSD&mid=sec&sid1=103&oid=119&aid=0002362702"
n_url = "https://news.naver.com/main/read.nhn?mode=LSD&mid=sec&sid1=103&oid=119&aid=0002362702"
nn <- getContent(n_url)
nn

# 댓글 수집
x <- getAllComment(n_url)
x$contents[1:50]

# ---------------------------------------------------------
# 전처리 적용시 하단 주석 제거
contents <- gsub("독3사|독3차|독삼사", "벤츠, 아우디, BMW", x$contents) # 동의어 처리
contents <- gsub("[[:punct:]]", " ", contents) # 문장부호 제거
contents <- str_replace_all(contents, "[[:space:]]{1,}", " ") # 띄어쓰기 제거
contents <- gsub("[[:digit:]]", "", contents) # 숫자 제거
# ---------------------------------------------------------

# backup ---------------------------------------------------
# # 문장부호 제거
# contents <- gsub("[[:punct:]]", " ", x$contents)
# # 띄어쓰기 제거
# #apply(contents,2,function(x)gsub('\\s+', '',x)))
# #contents <- gsub( " ","",contents)
# contents <- str_replace_all(contents, "[[:space:]]{1,}", " ")
# contents
# # 숫자 제거
# # contents <- gsub("[[:digit:]]", "", contents)
# ---------------------------------------------------------

head(contents)

# 명사 추출 : 사용자 단어가 추가된 사전 사용
# [1] -----------------------------------------------------
out2 <- lapply(contents,SimplePos09)
# ---------------------------------------------------------

# [2] -----------------------------------------------------
# ko_words <- function(doc) {
#   d <- as.character(doc)
#   pos <- unlist(SimplePos22(d))
#
#   extracted <- str_match(pos, '([가-힣]+)/[NP][A-Z]')
#
#   keyword <- extracted[, 2]
#   keyword[!is.na(keyword)]
# }
# out_v2 <- ko_words(contents)
# ---------------------------------------------------------

# new_term <- c("SK텔레콤"자원부 "서울교통공사", "혼잡도", "과학기술정보통신부")
# new_term <- c("혁신성장전략회", "산업통상자원부")
new_term <- c("독일차", "국토교통부","수입차","가격","현기차","수리비","오일","카톡")
new_dic <- data.frame(new_term , "ncn")

buildDictionary(ext_dic = c('sejong', 'woorimalsam', 'insighter'),user_dic = new_dic)
## ???????? words dictionary was built.

# 단어 빈도 - 검색키워드 제거
# out_v2 <- do.call(c,out2) #r 기존 코드
out_v2 <- names(unlist(out2)) # 수정 코드

# out_v <- do.call(c, out2)
wt2 <- table(out_v2)
# wt2 <- wt2[nchar(names(wt2)) > 1]
wt2 <- wt2[nchar(names(wt2)) > 1 & names(wt2)!="스마트폰"] # 검색 키워드를 제거하기
sort(wt2, decreasing = T)[1:10]

