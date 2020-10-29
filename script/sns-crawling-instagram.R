# https://www.slideshare.net/SeungchanBaeg/r-api-77467823
# https://www.instagram.com/developer/

source("script/ini.r")

library(rjson) 
library(RCurl) 
library(httr) 
library(rCharts) 
library(reshape2) 
# --------------------------------------------------------
require(devtools) 
install_github('rCharts', 'ramnathv')

app_name <- "daniel2" 
client_id <- instagram_client_id
client_secret <- instagram_client_secret
scope <- "basic" 

instagram <-
  oauth_endpoint(authorize = "https://api.instagram.com/oauth/authorize", 
                 access = "https://api.instagram.com/oauth/access_token") 
myapp <- oauth_app(app_name, client_id, client_secret) 
ig_oauth <- oauth2.0_token(instagram,  myapp, scope = "basic", type = "application / x - www - form - urlencoded", cache=FALSE) 

tmp <- strsplit(toString(names(ig_oauth$credentials)), '"') 
token <- tmp[[1]][4]
username <- "seungchanbaeg" 
user_info <- fromJSON(getURL(paste('https:// api.instagram.com / v1 / users / search ? q = ', username, ' &
      access_token = ', token, sep="")), unexpected.escape="keep") received_profile <- user_info$data[[1]]