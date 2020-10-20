library(rvest)
library(dplyr)
library(jsonlite)

commentList <- data.frame()
page <- 1

# repeat는 condition 없이 계속 반복하다가 'break' 호출로 종료되는 반복문 입니다.
repeat{
  
  url <- paste0("https://apis.naver.com/commentBox/cbox/web_naver_list_jsonp.json?ticket=auto1&templateId=&pool=cbox&_callback=jQuery17017637762118716838_1574171369360&lang=ko&country=&objectId=129711&categoryId=&pageSize=10&indexSize=10&groupId=&listType=OBJECT&pageType=default&page=",page,"&refresh=false&sort=NEW&_=1574171452288")
  ref <- "https://auto.naver.com/car/talk.nhn?yearsId=129711"
  con <- httr::GET(url, httr::add_headers(Referer = ref))
  tt <- httr::content(con, "text")
  tt <- gsub("_callback|\\(|\\)|;|\n", "", tt)
  
  # 정규표현식을 이용하여 추출 contents 앞부분 삭제
  # condition : tt문서에서 'jQuery'로 시작하는 위치부터 처음 '{'가 나타난 위치-1까지 추출하여 삭제
  tt <- gsub(substr(tt, regexpr("jQuery", tt)[1], regexpr("\\{", tt)[1]-1), "", tt)
  
  data <- jsonlite::fromJSON(tt)
  
  # 현재 page의 댓글 startRow가 전체 댓글 endRow보다 커지면 종료(break)
  # startRow, endRow 는 index 번호로 댓글별 번호라고 생각하시면 됩니다.
  if(data$result$pageModel$startRow > data$result$pageModel$endRow){
    break
  }
  
  # 댓글 등록시간(regTime), 등록자(userName), 내용(contents)를 추출하여 새로운 변수 생성
  tmp_commentList <- dplyr::select(data$result$commentList, c(regTime, userName, contents))
  
  # commentList 라는 변수에 row bind(행 기준 이어 붙이기)하여 결과물을 만들어 갑니다.
  commentList <- rbind(commentList,tmp_commentList)  
  cat("\n - pages = ", page,"/",data$result$pageModel$totalPages,",\t numofComments = ", nrow(commentList))
  page = page + 1
}

head(commentList)
