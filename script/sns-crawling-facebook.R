#  http://airpage.org/xe/language_data/22905

#필요 패키지
install.packages("Rfacebook")
library(Rfacebook)

#페이스북 앱id 정보와 secret 값 입
fbAuth = fbOAuth(app_id = "facebook_app_id", 
                 app_secret = "facebook_app_secret",
                 extended_permissions = FALSE)

start_date = '2016/12/01'
end_date = '2017/01/30'
scrape_days=seq(from = as.Date(start_date), to = as.Date(end_date), by = 'days')

#공인 페이지s
stars <- c("iu.loen", "OfficialLeeMinho", "barackobama")
posts <- c()

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