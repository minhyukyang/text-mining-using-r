###########################################
# 05. 인스타그램 데이터 수집 및 분석 #
###########################################

# 1. 사전 환경 세팅
# 2. 데이터 수집
# 3. 데이터 전처리 및 워드 클라우드

# 1. 사전 환경 세팅 -------------------------------------------------------------

# selenium 다운로드 : https://www.selenium.dev/downloads/
# chrome 드라이버 다운로드 : https://sites.google.com/a/chromium.org/chromedriver/downloads
# - 본인 chrome 드라이버 버전에 맞춰 다운로드 필수

# 1) 환경 세팅

source("script/ini.r")
# java -jar selenium-server-standalone-3.141.59.jar -port 4445

library(RSelenium)
library(rvest)
library(tidyverse)
library(dplyr)

# url 만들기
insta_searching <- function(word){
  url <- paste0('https://www.instagram.com/explore/tags/',word)
  return(url)
}

# 첫번째 게시물 찾아 클릭하는 함수
select_first <- function(driver){
  first <- driver$findElement(using='xpath', value='//*[@id="react-root"]/section/main/article/div[1]/div/div/div[1]/div[1]/a/div[1]/div[2]')
  # first <- driver.find_element_by_css_selector('div._9AhH0') 
  #find_element_by_css_selector 함수를 사용해 요소 찾기
  first$clickElement()
  Sys.sleep(3) # time.sleep(3) #로딩을 위해 3초 대기
}

# 본문 내용, 작성 일시, 위치 정보 및 해시태그(#) 추출
get_content <- function(driver){
  
  # 1. 현재 페이지의 HTML 정보 가져오기
  html <- driver$getPageSource()[[1]] # html <- driver.page_source
  html <- read_html(html) # soup = BeautifulSoup(html, 'lxml')    
  
  # 2. 본문 내용 가져오기
  #첫 게시글 본문 내용이 <div class="C4VMK"> 임을 알 수 있다.
  #태그명이 div, class명이 C4VMK인 태그 아래에 있는 span 태그를 모두 선택.
  tryCatch({
    #여러 태그중 첫번째([0]) 태그를 선택  
    content <- html %>% html_nodes("div.C4VMK > span") %>% html_text() %>% .[1] #선택된 노드를 텍스트 화
    # content = soup.select('div.C4VMK > span')[0].text 
  }, error = function(e){
    content <- '' 
  })
    
  # 3. 본문 내용에서 해시태그 가져오기(정규표현식 활용)
  tags <- str_extract_all(content, '#[^#\\s]+') %>% .[[1]] %>% paste(., collapse = ",")
  # tags = re.findall(r'#[^\s#,\\]+', content) # content 변수의 본문 내용 중 #으로 시작하며, #뒤에 연속된 문자(공백이나 #, \ 기호가 아닌 경우)를 모두 찾아 tags 변수에 저장
  
  # 4. 작성 일자 가져오기
  tryCatch({
    date <- html %>% html_nodes("time._1o9PC.Nzb55") %>% html_attr('datetime') %>% str_sub(., 1, 10) #앞에서부터 10자리 글자
    # soup.select('time._1o9PC.Nzb55')[0]['datetime'][:10] 
  }, error = function(e){
    date = ''
  })
  
  # 5. 좋아요 수 가져오기
  tryCatch({
    like <- html %>% html_nodes("div.Nm9Fw > button") %>% html_text() %>% str_sub(., 5, -1) #앞에서부터 10자리 글자
    # like = soup.select('div.Nm9Fw > button')[0].text[4:-1] 
  }, error = function(e){
    like <- as.character(0)
  })

  # 6. 위치 정보 가져오기
  tryCatch({
    place <- html %>% html_nodes("div.JF9hh") %>% html_text()
    # place = soup.select('div.JF9hh')[0].text
  }, error = function(e){
    place <- ''
  })
  
  # 7. ID 가져오기
  tryCatch({
    user_id <- html %>% html_nodes("div.e1e1d") %>% html_text()
    # place = soup.select('div.JF9hh')[0].text
  }, error = function(e){
    user_id <- ''
  })

  # 8. 수집한 정보 저장하기
  result <- tibble(
    "user_id" = as.character(user_id),
    "content" = as.character(content),
    "date" = as.Date(date),
    "like" = as.character(like),
    "place" = as.character(place),
    "tags" = as.character(tags)
  )
  # result$content <- stri_unescape_unicode(gsub("<U\\+(....)>", "", result$content))
  # data = [content, date, like, place, tags]
  
  return(result)
}

move_next <- function(driver){
  right <- driver$findElement(using='css', value='a._65Bje.coreSpriteRightPaginationArrow')
  # right <- driver$findElement(using='xpath', value='/html/body/div[4]/div[1]/div/div/a')
  right$clickElement()
  Sys.sleep(3) # time.sleep(3)
}


# 2. 데이터 수집 ---------------------------------------------------------------

# 0) chrome 창 열기
remDr <- remoteDriver(
  remoteServerAddr="localhost",
  port=4445L,
  browserName="chrome")

remDr$open()
# remDr$closeall()

# 1) 크롬으로 인스타그램 - '인하대' 검색
word <- '인하대'
url <- insta_searching(word)
remDr$navigate(url) # driver.get(url) 
Sys.sleep(4) # time.sleep(4) 

# 2) 로그인 하기
# 로그인 버튼 클릭 
login_section <- remDr$findElement(using='xpath', value='//*[@id="react-root"]/section/nav/div[2]/div/div/div[3]/div/span/a[1]/button')
# login_section <- '//*[@id="react-root"]/section/nav/div[2]/div/div/div[3]/div/span/a[1]/button'
login_section$clickElement()
# driver.find_element_by_xpath(login_section).click()
Sys.sleep(3) # time.sleep(3) 

# ID 입력
elem_login_id <- remDr$findElement(using='xpath', value='//*[@id="loginForm"]/div/div[1]/div/label/input')
# elem_login <- driver.find_element_by_name("username")
elem_login_id$clickElement()
elem_login_id$clearElement() # elem_login.clear()
elem_login_id$sendKeysToElement(list(instagram_id)) # elem_login.send_keys('ID') 

# PW 입력
elem_login_pw <- remDr$findElement(using='xpath', value='//*[@id="loginForm"]/div[1]/div[2]/div/label/input')
# elem_login = driver.find_element_by_name('password')
elem_login_pw$clickElement()
elem_login_pw$clearElement() # elem_login.clear()
elem_login_pw$sendKeysToElement(list(instagram_pw)) # elem_login.send_keys('PASSWORD') 
Sys.sleep(1) # time.sleep(1) 

# 로그인 버튼 클릭
elem_login_button <- remDr$findElement(using='xpath', value='//*[@id="loginForm"]/div[1]/div[3]/button/div')
# xpath = """//*[@id="react-root"]/section/main/div/article/div/div[1]/div/form/div[4]/button"""
elem_login_button$clickElement() # driver.find_element_by_xpath(xpath).click() 
Sys.sleep(4) # time.sleep(4) 

# 3) 검색페이지 접속하기
remDr$navigate(url) # driver.get(url)
Sys.sleep(4) # time.sleep(4) 

# 4) 첫번째 게시글 열기 
select_first(remDr) 

# 5) 데이터 수집하기 

results <- tibble()
target <- 20 #크롤링할 게시물 수

for (i in 1:target){
  tmp_data <- get_content(remDr) #게시물 정보 가져오기
  
  if (nrow(results) == 0){
    results <- tibble(tmp_data)
  } else{
    results <- bind_rows(results, data.frame(tmp_data)) # results.append(data)
  }
  
  print(tmp_data$tags)
  move_next(remDr)    
}

view(results)

# 6) 분석
# https://www.si.re.kr/node/62271

# 일자별 포스팅 건수

library(ggplot2)

insta_daily <- as.Date(results$date, format = '%Y-%m-%d')
insta_daily <- tibble(insta_daily)
insta_daily_count <- insta_daily %>% 
  group_by(insta_daily) %>% 
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  ggplot(data = ., aes(x = insta_daily, y = count)) + 
  geom_bar(stat="identity", fill="steelblue") + geom_text(aes(label = count), vjust=1, color="white") + theme_bw() + theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
  scale_x_date(breaks=seq(min(insta_daily$insta_daily),max(insta_daily$insta_daily), "1 day"), 
               minor_breaks="1 day")
insta_daily_count

# content 분석
library(KoNLP)
library(widyr)

results %>% 
  filter(!is.na(content)) %>%
  mutate(content = str_replace_all(content, "[^가-힝a-zA-Z\\s]", " ")) %>%
  # mutate(content = str_replace_all(content, "[[:punct:]]", " ")) %>%
  unnest_tokens(sent, content, 
                token = "sentences") %>% 
  dplyr::mutate(id = as.numeric(1:n())) %>% 
  unnest_tokens(pos, sent, 
                token = SimplePos09) %>% 
  select(id, pos) %>% 
  filter(str_detect(pos, "/n|/v(v|a)")) %>% 
  mutate(pos = 
           str_remove_all(pos, "/.*$")) %>% 
  filter(nchar(pos) > 1)%>% 
  pairwise_count(pos, id, 
                 sort = T, upper = F) -> 
  insta_pos

insta_pos %>%
  filter(item1 == "인하대후문")

# #인하대 태그 분석
results %>% 
  dplyr::mutate(id = as.numeric(1:n())) %>% 
  unnest_tokens(insta_tags, tags, token = 'regex', pattern=",") %>% 
  select(id, insta_tags) %>% 
  mutate(insta_tags = str_remove_all(insta_tags, "/.*$")) %>% 
  filter(nchar(insta_tags) > 1) %>% 
  pairwise_count(insta_tags, id, sort = T, upper = F) -> 
  insta_tags

# #인하대 연관어 확인
insta_tags %>%
  filter(item1 == "#인하대")
