# facebook for developers : https://developers.facebook.com/?locale=ko_KR
# facebook app 만들기 : http://www.econovation.co.kr/ecnvb/%ED%8E%98%EC%9D%B4%EC%8A%A4%EB%B6%81-%EC%95%B1-%EB%A7%8C%EB%93%A4%EA%B8%B0/
# R분석 : http://airpage.org/xe/language_data/22905
# R분석2 : https://brunch.co.kr/@dugi/2

## 2020-10-29
# 현재 코로나19로 개인 인증이 막혀있고 비즈니스 계정으로만 API 서비스 이용 가능
# - 참고 : https://developers.facebook.com/apps/2507109199588116/settings/basic/

source("script/ini.r")

#필요 패키지
# install.packages("Rfacebook")
library(Rfacebook)

#페이스북 앱id 정보와 secret 값 입
fbAuth <- fbOAuth(app_id = facebook_app_id, 
                 app_secret = facebook_app_secret,
                 extended_permissions = FALSE)

start_date <- '2020/10/01'
end_date <- '2020/10/27'

scrape_days <- seq(from = as.Date(start_date), to = as.Date(end_date), by = 'days')

# 공인 페이지
# stars <- c("iu.loen", "OfficialLeeMinho", "barackobama")
# stars <- c("jaemyunglee1")
# posts <- c()

# searchGroup(name="KoreaRUsers", token="2507109199588116|35g_uQHLDzmpy172R5oLUyxzf8Y")

getUsers("me", token=fbAuth) # success
getUsers("facebook", token=fbAuth) # error

getPage(page="me", token=fbAuth, n=10) # error
getPage(page="huffpostkorea", token=fbAuth, n=30) # error
getPage(page="facebook", token=fbAuth, n=30) # error

# test
# me_token <-"EAAjoM9lBKxQBADxMEHr0Xn6rEIUZCNogVOakHzwOmaZCZAclnPTGLCu9kZADZAY74UhhKZCmWUqgBLFKrmnByK0XCVgmvjKmIKxamsPZBMoQLHjcoGRTVOED2PCnIZA40zAxNgm4xWDVBgUJ5KcXOhHlY41RaJ4ZADo2Gsn5zrJsXiWlQ9kbfqZBfsfNrG0IA6HvQZD"
# me <- getUsers("533419894", token=me_token, private_info=TRUE)
# my_friends <- getFriends(token=me_token, simplify = FALSE)
# my_page <- getPage(page="facebook", token=me_token, n=30)

page <- "facebook"
url <- paste0("https://graph.facebook.com/", page, 
              "/posts?fields=from,message,created_time,type,link,story,comments.summary(true)", 
              ",likes.summary(true),shares")
feed <- TRUE
if (feed) {
  url <- paste0("https://graph.facebook.com/", page, 
                "/feed?fields=from,message,created_time,type,link,story,comments.summary(true)", 
                ",likes.summary(true),shares")
}
n <- 10
if (n <= 25) {
  url <- paste0(url, "&limit=", n)
}
api <- NULL
token <- fbAuth
content <- callAPI(url = url, token = token, api = api)

#공인 페이지로부터 페이지 정보 얻기
for (i in 1:length(stars)) {
  for(scrape_day in scrape_days) {
    tryCatch(
      {
        daypost=getPage(page = stars[i],
                        token = fbAuth,
                        since = as.Date(scrape_day, origin = "1970-01-01"),
                        until = as.Date(scrape_day, origin = "1970-01-01") + 1)
        
        posts = rbind(posts, daypost)
      },      
      error = function(e){}
    )
  }
}

View(posts)

# -----
# https://3months.tistory.com/91
# https://cran.r-project.org/web/packages/Rfacebook/Rfacebook.pdf
# 페이스북 페이지 분석 : http://statkclee.github.io/politics/22-vote-fb-page.html

library(Rfacebook)
library(ggplot2)
library(scales)

# get auth token
fb_oauth = fbOAuth(app_id = "310016876042099", app_secret = "6772bfc30e27720eac8d67122157aa47", extended_permissions = FALSE)

# 해당 페이지의 시작날짜와 종료날짜 사이의 모든 포스트 가져옴
getPosts <- function(page_name, start_date, end_date) {
  # 날짜 sequence
  scrape_days <- seq(from=as.Date(start_date), to=as.Date(end_date), by='days')
  posts = c()
  for(scrape_day in scrape_days){
    daypost = c()
    tryCatch({
      daypost = getPage(page=page_name, token=fb_oauth,
                        since=as.Date(scrape_day, origin='1970-01-01'),
                        until=as.Date(scrape_day, origin='1970-01-01')+1)},
      error = function(e){})
    posts = rbind(posts, daypost)
  }
  return(posts)
}

drawLikeGraph <- function(data){
  ggplot(data, aes(x=created_time, y=likes_count)) + geom_line() + 
    theme_bw() + scale_x_date(labels = date_format("%m-%Y")) +
    labs(x = "날짜(MM-yyyy)", y = "좋아요수(n)") + 
    ggtitle(paste(start_date, end_date, sep="~"))
}

drawPostNumGraph <- function(data){
  ggplot(data=data, aes(x=name, y=num, fill=name)) +
    geom_bar(stat="identity", width=0.8) +
    labs(x='대선주자', y='포스트수') +
    geom_text(aes(label=num), vjust=1.6, color="white", size=3.5) +
    theme_minimal() +
    ggtitle(paste(start_date, end_date, sep="~"))
}

drawAverageLikeGraph <- function(data){
  ggplot(data=data, aes(x=name, y=average_like, fill=name)) +
    geom_bar(stat="identity", width=0.8) +
    labs(x='대선주자', y='평균 좋아요수') +
    geom_text(aes(label=round(average_like, 0)), vjust=1.6, color="white", size=3.5) +
    theme_minimal() +
    ggtitle(paste(start_date, end_date, sep="~"))
}

getSummaryDataFrame <- function(posts, name){
  average_like = sum(posts$likes_count)/nrow(posts)
  summary <- data.frame(name=name, num=nrow(posts),
                        average_like=average_like)
  return(summary)
}

moon_page = "moonbyun1" # 문재인 페이지 
lee_page = "jaemyunglee1" # 이재명 페이지
hwang_page = "PM0415HwangKyoahn" # 황교안 페이지
yoo_page = "yooseongmin21" # 유승민 페이지
ahn_hwee_page = "steelroot" # 안희정 페이지
ahn_chul_page = "ahncs111" # 안철수 페이

start_date <- '2016/11/01' # 시작 날짜
end_date <- '2017/02/19' # 종료 날짜

moon_posts <- getPosts(moon_page, start_date, end_date)
lee_posts <- getPosts(lee_page, start_date, end_date)
hwang_posts <- getPosts(hwang_page, start_date, end_date)
yoo_posts <- getPosts(yoo_page, start_date, end_date)
ahn_hwee_posts <- getPosts(ahn_hwee_page, start_date, end_date)
ahn_chul_posts <- getPosts(ahn_chul_page, start_date, end_date)

print(nrow(hwang_posts))

# preprocess
moon_posts$created_time <- as.Date(moon_posts$created_time) # string to date
lee_posts$created_time <- as.Date(lee_posts$created_time)
hwang_posts$created_time <- as.Date(hwang_posts$created_time)
yoo_posts$created_time <- as.Date(yoo_posts$created_time)
ahn_hwee_posts$created_time <- as.Date(ahn_hwee_posts$created_time)
ahn_chul_posts$created_time <- as.Date(ahn_chul_posts$created_time)

# summary 데이터 프레임 만듦
moon_summary <- getSummaryDataFrame(moon_posts, '문재인')
lee_summary <- getSummaryDataFrame(lee_posts, '이재명')
hwang_summary <- getSummaryDataFrame(hwang_posts, '황교안')
yoo_summary <- getSummaryDataFrame(yoo_posts, '유승민')
ahn_hwee_summary <- getSummaryDataFrame(ahn_hwee_posts, '안희정')
ahn_chul_summary <- getSummaryDataFrame(ahn_chul_posts, '안철수')

summary <- data.frame() # 빈 데이터 프레임을 만듦
summary <- rbind(summary, moon_summary) # 행추가
summary <- rbind(summary, lee_summary)
summary <- rbind(summary, hwang_summary)
summary <- rbind(summary, yoo_summary)
summary <- rbind(summary, ahn_hwee_summary)
summary <- rbind(summary, ahn_chul_summary)

drawPostNumGraph(summary)
drawAverageLikeGraph(summary)

# reactions <- getReactions(post=posts$id, fb_oauth, verbose=TRUE)
drawLikeGraph(moon_posts)
drawLikeGraph(lee_posts)
drawLikeGraph(hwang_posts)
drawLikeGraph(yoo_posts)
drawLikeGraph(ahn_hwee_posts)
drawLikeGraph(ahn_chul_posts)

all_posts <- data.frame()
all_posts <- rbind(all_posts, moon_posts)
all_posts <- rbind(all_posts, lee_posts)
all_posts <- rbind(all_posts, hwang_posts)
all_posts <- rbind(all_posts, yoo_posts)
all_posts <- rbind(all_posts, ahn_hwee_posts)
all_posts <- rbind(all_posts, ahn_chul_posts)

ggplot(all_posts, aes(x=created_time, y=likes_count, group=from_name, colour=from_name)) + geom_line(size=0.5) + 
  geom_smooth() +
  theme_minimal() + scale_x_date(labels = date_format("%m-%Y")) +
  labs(x = "날짜(MM-yyyy)", y = "좋아요수(n)") + 
  ggtitle(paste(start_date, end_date, sep="~"))


