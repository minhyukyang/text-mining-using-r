# https://github.com/Leeyua-airim/R_AIRIM/tree/master/R_Selenium_Youtube

## selenium
# https://github.com/mozilla/geckodriver/releases/tag/v0.17.0
# https://sites.google.com/a/chromium.org/chromedriver/
# https://www.seleniumhq.org/download/

##CMD 
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4445


## RSelenium
#CMD java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4445

library(httr)
library(rvest)
library(RSelenium)

remD <- remoteDriver(remoteServerAddr = 'localhost', 
                     port = 4445L, # 포트번호 입력 
                     browserName = "chrome") 
remD$open() #서버에 연결
remD$navigate("https://www.naver.com") #이 홈페이지로 이동 

html <- remD$getPageSource()[[1]] 
html <- read_html(html) #페이지의 소스 읽어오기 

sWords <- html %>% html_nodes("span.ah_k") %>% html_text() #선택된 노드를 텍스트 화

sWords[1:20] #1~20개가져오기 


id<-remD$findElement(using = "css selector", value = "input#id")
pw<-remD$findElement(using = "css selector", value = "input#pw")

id$sendKeysToElement(list("ID"))
pw$sendKeysToElement(list("PW"))

btn <- remD$findElement(using="css selector", value='input.btn_global') 
btn$clickElement()

btn$close()

## youtube_title.R
#CMD java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4445

library(httr)
library(rvest)
library(RSelenium)

remD <- remoteDriver(port = 4445L, # 포트번호 입력 
                     browserName = "chrome") #사용할 브라우저 

remD$open() #서버에 연결
title_you <- "에이림 인공지능"

remD$navigate(paste0("https://www.youtube.com/results?search_query=",title_you)) #해당 홈페이지로 이동 

html <- remD$getPageSource()[[1]]
html <- read_html(html) #페이지의 소스 읽어오기 

youtube_title <- html %>% html_nodes("#video-title") %>% html_text() #선택된 노드를 텍스트 화
youtube_title[1:20] #1~20개가져오기 


youtube_title<-gsub("\n","",youtube_title) #데이터 정제 1
youtube_title<-trimws(youtube_title) #데이터 정제 2

youtube_title

# write.table(youtube_title, file = "E:/R/R셀레니움/결과물/에이림_title.txt",sep=",",row.names=FALSE, quote = FALSE)

## R_youtube_comments.R
#java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4445

remD <- remoteDriver(remoteServerAddr = 'localhost', 
                     port = 4445L, # 포트번호 입력 
                     browserName = "chrome") 
remD$open() #서버에 연결
# remD$closeall() #서버에 연결
# remD$navigate("https://www.youtube.com/watch?v=-4Hi0QDEIk4") #이 홈페이지로 이동 
remD$navigate("https://www.youtube.com/watch?v=56T_5aMireI") #이 홈페이지로 이동 

#영상 플레이
btn <- remD$findElement(using="css selector", value='.html5-main-video')
btn$clickElement()

#홈페이지 스크롤
# remD$executeScript("window.scrollTo(0,500)")
# remD$executeScript("window.scrollTo(500,1000)")
# remD$executeScript("window.scrollTo(1000,1500)")
# remD$executeScript("window.scrollTo(0,100000)")
num_of_pagedowns <- 10

webElem <- remD$findElement("css", "body")

while(num_of_pagedowns){
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(4)
  num_of_pagaedowns = num_of_pagaedowns - 1
  print(num_of_pagaedowns)
}

html <- remD$getPageSource()[[1]]
html <- read_html(html) #페이지의 소스 읽어오기 

ytube_comments <- html %>% html_nodes("#content-text") %>% html_text() 
ytube_comments <- ytube_comments[1:50]

ytube_comments <- gsub("\n","",ytube_comments) #데이터 정제 1
ytube_comments<-trimws(ytube_comments)

ytube_comments

# 각각의 태그들로 추출을 한다.

video_title <- html %>% html_nodes(xpath = '//*[@id="container"]/h1/yt-formatted-string') %>% html_text
# soup.find('h1', 'title style-scope ytd-video-primary-info-renderer').string
upload_date <- html %>% html_nodes(xpath = '//*[@id="date"]/yt-formatted-string') %>% html_text
# soup.find('div', {'id' : 'date'}).find('yt-formatted-string', 'style-scope ytd-video-primary-info-renderer').string
view_cnt <- html %>% html_nodes(xpath = '//*[@id="count"]/yt-view-count-renderer/span[1]') %>% html_text
# soup.find('span', 'view-count style-scope yt-view-count-renderer').string
like_cnt <- html %>% html_nodes(xpath = '//*[@id="top-level-buttons"]/ytd-toggle-button-renderer[1]') %>% html_text
# soup.find('yt-formatted-string',{'id':'text','class':'style-scope ytd-toggle-button-renderer style-text','aria-label':re.compile('좋아요')}).string
unlike_cnt <- html %>% html_nodes(xpath = '//*[@id="top-level-buttons"]/ytd-toggle-button-renderer[2]/a') %>% html_text 
# soup.find('yt-formatted-string',{'id':'text','class':'style-scope ytd-toggle-button-renderer style-text','aria-label':re.compile('싫어요')}).string
comment_cnt <- html %>% html_nodes(xpath = '//*[@id="count"]/yt-formatted-string') %>% html_text 
# soup.find('ytd-comments', {'id' : 'comments'}).find('yt-formatted-string', 'count-text style-scope ytd-comments-header-renderer').string
print(video_title)
print(upload_date)
print(view_cnt)
print(like_cnt)
print(unlike_cnt)
print(comment_cnt)

# write.table(ytube_comments, file = "E:/R/R셀레니움/결과물/youtube_comments.txt",sep=",",row.names=FALSE,quote = FALSE)

# 제목, 게시일, 조회수, 좋아요, 싫어요, 댓글 추출 (using python)
# # 반복문을 사용 video_url list에서 URL에 접속한다.
# for i in range(len(video_url)):
#     start_url = video_url[i]
#     print(start_url)
#     driver.implicitly_wait(delay)
#     driver.get(start_url)
#     driver.maximize_window()
#     body = driver.find_element_by_tag_name('body')
#     time.sleep(delay)
# 
#     # 페이지를 한 번 스크롤하는 이유는 페이지를 스크롤 하지 않으면 댓글수가 나오지 않기 때문이다.
#     num_of_pagedowns = 1
#     while num_of_pagedowns:
#         body.send_keys(Keys.PAGE_DOWN)
#         time.sleep(1.5)
#         num_of_pagedowns -= 1
# 
#     html = driver.page_source
#     soup = BeautifulSoup(html, 'lxml')
# 
#     # 각각의 태그들로 추출을 한다.
#     video_title = soup.find('h1', 'title style-scope ytd-video-primary-info-renderer').string
#     upload_date = soup.find('div', {'id' : 'date'}).find('yt-formatted-string', 'style-scope ytd-video-primary-info-renderer').string
#     view_cnt = soup.find('span', 'view-count style-scope yt-view-count-renderer').string
#     like_cnt = soup.find('yt-formatted-string',{'id':'text','class':'style-scope ytd-toggle-button-renderer style-text','aria-label':re.compile('좋아요')}).string
#     unlike_cnt = soup.find('yt-formatted-string',{'id':'text','class':'style-scope ytd-toggle-button-renderer style-text','aria-label':re.compile('싫어요')}).string
#     comment_cnt = soup.find('ytd-comments', {'id' : 'comments'}).find('yt-formatted-string', 'count-text style-scope ytd-comments-header-renderer').string
#     print(video_title, upload_date, view_cnt, like_cnt, unlike_cnt, comment_cnt)
# 
# 
# driver.close()