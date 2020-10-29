# https://quantumcomputer.tistory.com/109
# https://velog.io/@ek1816/%EC%9D%B8%EC%8A%A4%ED%83%80%EA%B7%B8%EB%9E%A8-%EC%82%AC%EB%8B%B9%EB%A7%9B%EC%A7%91-%ED%81%AC%EB%A1%A4%EB%A7%81

# 인스타그램 크롤링 소스스
# https://github.com/JonasSchroeder/InstaCrawlR

library("RSelenium")
library("rvest")

remDr <- remoteDriver(
  remoteServerAddr="localhost",
  port=4445L,
  browserName="chrome")

remDr$open()

# url 만들기
insta_searching <- function(word){
  url <- paste0('https://www.instagram.com/explore/tags/',word)
  return(url)
}

# 첫번째 게시물 찾아 클릭하는 함수
select_first <- function(driver){
  first <- remDr$findElement(using='xpath', value='//*[@id="react-root"]/section/main/article/div[1]/div/div/div[1]/div[1]/a/div[1]/div[2]')
  # first <- driver.find_element_by_css_selector('div._9AhH0') 
  #find_element_by_css_selector 함수를 사용해 요소 찾기
  first$clickElement()
  Sys.sleep(3) # time.sleep(3) #로딩을 위해 3초 대기
}

# 본문 내용, 작성 일시, 위치 정보 및 해시태그(#) 추출
get_content <- function(driver){
  
  # 1. 현재 페이지의 HTML 정보 가져오기
  html <- remDr$getPageSource()[[1]] # html <- driver.page_source
  html <- read_html(html) # soup = BeautifulSoup(html, 'lxml')    
  
  # 2. 본문 내용 가져오기
  #첫 게시글 본문 내용이 <div class="C4VMK"> 임을 알 수 있다.
  #태그명이 div, class명이 C4VMK인 태그 아래에 있는 span 태그를 모두 선택.
  tryCatch({
    #여러 태그중 첫번째([0]) 태그를 선택  
    content <- html %>% html_nodes("div.C4VMK > span") %>% html_text() %>% .[1] #선택된 노드를 텍스트 화
    # content = soup.select('div.C4VMK > span')[0].text 
  }, error = function(e){
    content <- ' ' 
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
    like <- 0
  })

  # 6. 위치 정보 가져오기
  tryCatch({
    place <- html %>% html_nodes("div.JF9hh") %>% html_text()
    # place = soup.select('div.JF9hh')[0].text
  }, error = function(e){
    place <- ''
  })
  
  # 7. 수집한 정보 저장하기
  data <- data.frame(content, date, like, place, tags)
  # data = [content, date, like, place, tags]
  
  return (data)
}

move_next <- function(driver){
  right <- remDr$findElement(using='xpath', value='/html/body/div[4]/div[1]/div/div/a')
  right$clickElement()
  Sys.sleep(3) # time.sleep(3)
}

# def move_next(driver):
#     right = driver.find_element_by_css_selector('a._65Bje.coreSpriteRightPaginationArrow') 
#     right.click()
#     time.sleep(3)

library("RSelenium")
library("rvest")

# 0. 환경 세팅 ----------------------------------------------------------------

remDr <- remoteDriver(remoteServerAddr = "localhost",
                      port = 4445L,
                      browserName = "chrome")
remDr$open()
# remDr$closeall()

# 1. 크롬으로 인스타그램 - '사당맛집' 검색 ----
word <- '인하대'
url <- insta_searching(word)
remDr$navigate(url) # driver.get(url) 
Sys.sleep(4) # time.sleep(4) 

# 2. 로그인 하기 ----
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
elem_login_id$sendKeysToElement(list("didalsgur85@naver.com")) # elem_login.send_keys('ID') 

# PW 입력
elem_login_pw <- remDr$findElement(using='xpath', value='//*[@id="loginForm"]/div[1]/div[2]/div/label/input')
# elem_login = driver.find_element_by_name('password')
elem_login_pw$clickElement()
elem_login_pw$clearElement() # elem_login.clear()
elem_login_pw$sendKeysToElement(list("rlathdus1!")) # elem_login.send_keys('PASSWORD') 
Sys.sleep(1) # time.sleep(1) 

# 로그인 버튼 클릭
elem_login_button <- remDr$findElement(using='xpath', value='//*[@id="loginForm"]/div[1]/div[3]/button/div')
# xpath = """//*[@id="react-root"]/section/main/div/article/div/div[1]/div/form/div[4]/button"""
elem_login_button$clickElement() # driver.find_element_by_xpath(xpath).click() 
Sys.sleep(4) # time.sleep(4) 

# 3. 검색페이지 접속하기 ----
remDr$navigate(url) # driver.get(url)
Sys.sleep(4) # time.sleep(4) 

# 4. 첫번째 게시글 열기 ----
select_first() 

# 5. 비어있는 변수(results) 만들기 ----

results <- c()
target <- 10 #크롤링할 게시물 수

for (i in 1:target){
  data <- get_content(driver) #게시물 정보 가져오기
  data <- rbind(results, data)# results.append(data)
  move_next(driver)    
}

head(results)
