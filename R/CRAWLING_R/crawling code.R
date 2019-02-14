#=====================#
# 2019-02-11 Crawling #
#=====================#

# 크롤링 패키지 설치
install.packages("rvest")
install.packages("stringr")
install.packages("RSelenium")

# 크롤링을 위해 필요한 패키지 불러오기
library(rvest)     # 파이썬에서 beautifulsoup 같은 라이브러리
library(stringr)   # 문자열 조작하는 패키지
library(RSelenium) # 셀레니움 패키지, 자바가 있어야 실행 가능
library(dplyr)

# 주요 함수
# read_html(): 웹페이지 주소를 통해 html 소스를 읽어들인다.
# html_node(): 매칭되는 한 요소만 반환하며 id를 찾을 경우
# html_nodes(): read_html()에서 읽어온 소스 중 매칭되는 모든 요소(tag, class..) 반환.
# html_text(): html_nodes()에서 읽어온 데이터에서 텍스트 데이터만 추출한다.
# str_replace_all(): 문자열에서 정해준 패턴을 원하는 패턴으로 변경한다. 
# trimws(): 양쪽 공백 제거

# url <- "사이트 주소"
# url_html <- read_html(url)
# html_nodes(url_html, xpath='')

#==========================================================
# 실검 뽑아오기
# naver
naver <-"https://www.naver.com/"
naver_html <- read_html(naver)
k <- html_nodes(naver_html, css=".ah_k")    # class - css
html_text(k)[1:20]    # 중복제거

#daum
daum<-"https://www.daum.net/"
daum_html<-read_html(daum)
k<-html_nodes(daum_html, css=".link_issue")
html_text(k)[1:20%%2==0][1:10]

# RSeledium
# 자바 설치 덜됐을 때
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_201')
rD <- rsDriver() 
remDr <- rD$client
remDr$navigate("http://www.naver.com")
# 검색어창 찾기 
webElem <- remDr$findElement(using = "css", "[class = 'input_text']") 
webElem$sendKeysToElement(list("boaz", key = "enter")) # boaz 검색 후 enter
# 블로그 속성 태그
webElem_1 <- remDr$findElements(using = 'css', "[class = 'spnew name']")
# webElem_1의 텍스트정보만 빼옴
category <- unlist(lapply(webElem_1, function(x){x$getElementText()}))
# 블로그 카테고리 정보 저장
webElem_1 <- webElem_1[[which(category=="블로그")]]
# 블로그 카테고리 클릭 
webElem_1$clickElement()
# 셀리니움 창 닫기
remDr$close()

#=============#
# 네이버 영화 #
#=============#
# 네이버 영화 상세 페이지 url (극한직업)
naver_movie_detail_url <- "https://movie.naver.com/movie/bi/mi/basic.nhn?code=167651"
naver_movie_detail <- read_html(naver_movie_detail_url)

# 메인 제목
# 두가지 방법: xpath, selector-css
# copy xpath
naver_movie_detail %>% 
  html_nodes(xpath = '//*[@id="content"]/div[1]/div[2]/div[1]/h3/a[1]') %>% 
  html_text()
# copy selector -> css
naver_movie_detail %>% 
  html_nodes(css="#content > div.article > div.mv_info_area > div.mv_info > h3 > a:nth-child(1)") %>% 
  html_text()

# 부제목
naver_movie_detail %>% 
  html_nodes(xpath = '//*[@id="content"]/div[1]/div[2]/div[1]/strong') %>% 
  html_text()

# 관람객 평점
naver_movie_detail %>% 
  html_nodes(xpath = '//*[@id="actualPointPersentBasic"]/div/span/span') %>% 
  html_text()

# 장르
naver_movie_detail %>% 
  html_nodes(xpath = '//*[@id="content"]/div[1]/div[2]/div[1]/dl/dd[1]/p/span[1]/a') %>% 
  html_text()

# 국가
naver_movie_detail %>% 
  html_nodes(xpath = '//*[@id="content"]/div[1]/div[2]/div[1]/dl/dd[1]/p/span[2]/a') %>% 
  html_text()

# 개봉일
naver_movie_detail %>% 
  html_nodes(xpath = '//*[@id="content"]/div[1]/div[2]/div[1]/dl/dd[1]/p/span[4]') %>% 
  html_text() %>% 
  str_replace_all(pattern = "\r|\n|\t", replacement = "") %>% 
  trimws()    # 양쪽 공백 제거

# 출연
naver_movie_detail %>% 
  html_nodes(xpath = '//*[@id="content"]/div[1]/div[2]/div[1]/dl/dd[3]/p') %>% 
  html_text() %>% 
  str_replace_all(pattern = "\r|\n|\t", replacement = "") %>% 
  trimws()

# 줄거리
naver_movie_detail %>% 
  html_nodes(xpath = '//*[@id="content"]/div[1]/div[4]/div[1]/div/div[1]/p') %>% 
  html_text() %>% 
  str_replace_all(pattern = "\r|\n|\t", replacement = "")


# 하나의 data.frame으로 만들기
# 영화번호를 입력변수로 
get_naver_movie_info <- function(naver_movie_code) {
  # 네이버 영화 상세 페이지 url (극한직업)
  naver_movie_detail_url <- paste0("https://movie.naver.com/movie/bi/mi/basic.nhn?code=", naver_movie_code)
  naver_movie_detail <- read_html(naver_movie_detail_url)
  
  # 메인 제목
  main_title <- naver_movie_detail %>% html_nodes(xpath = '//*[@id="content"]/div[1]/div[2]/div[1]/h3/a[1]') %>% html_text()
  
  # 부제목
  sub_title <- naver_movie_detail %>% html_nodes(xpath = '//*[@id="content"]/div[1]/div[2]/div[1]/strong') %>% html_text()
  
  # 관람객 평점
  audience_rate <- naver_movie_detail %>% html_nodes(xpath = '//*[@id="actualPointPersentBasic"]/div/span') %>% html_text()
  
  # 장르
  genre <- naver_movie_detail %>% html_nodes(xpath = '//*[@id="content"]/div[1]/div[2]/div[1]/dl/dd[1]/p/span[1]') %>% html_text() %>% str_replace_all(pattern = "\r|\n|\t", replacement = "")
  
  # 국가
  county <- naver_movie_detail %>% html_nodes(xpath = '//*[@id="content"]/div[1]/div[2]/div[1]/dl/dd[1]/p/span[2]') %>% html_text() %>% str_replace_all(pattern = "\r|\n|\t", replacement = "")
  
  # 개봉일
  opening_date <- naver_movie_detail %>% html_nodes(xpath = '//*[@id="content"]/div[1]/div[2]/div[1]/dl/dd[1]/p/span[4]') %>% html_text() %>% str_replace_all(pattern = "\r|\n|\t", replacement = "") %>% trimws()
  
  # 출연
  actors <- naver_movie_detail %>% html_nodes(xpath = '//*[@id="content"]/div[1]/div[2]/div[1]/dl/dd[3]/p') %>% html_text() %>% str_replace_all(pattern = "\r|\n|\t", replacement = "") %>% trimws()
  
  # 줄거리
  synopsis <- naver_movie_detail %>% html_nodes(xpath = '//*[@id="content"]/div[1]/div[4]/div[1]/div/div[1]/p') %>% html_text() %>% str_replace_all(pattern = "\r|\n|\t", replacement = "")
  
  naver_movie_info_df <- data.frame(
    main_title = main_title,
    sub_title = sub_title,
    audience_rate = audience_rate,
    genre = genre,
    county = county,
    opening_date = opening_date,
    actors = actors,
    synopsis = synopsis
  )
  
  return(naver_movie_info_df)
}
get_naver_movie_info(167651) #극한직업
get_naver_movie_info(164173) #뺑반
get_naver_movie_info(109193) #드래곤길들이기3
get_naver_movie_info(66728)  #알리타: 배틀 엔젤
get_naver_movie_info(181554) #극장판 헬로카봇: 옴파로스 섬의 비밀

# 여러개의 영화 데이터를 한번에 수집
naver_movie_codes <- c(167651, 164173, 109193, 66728, 181554)
# 데이터프레임 틀 
naver_movie_info_total <- data.frame()
# for문으로 한번에 
for (naver_movie_code in naver_movie_codes) {
  print(naver_movie_code)
  naver_movie_info_part <- get_naver_movie_info(naver_movie_code)
  naver_movie_info_total <- rbind(naver_movie_info_total, naver_movie_info_part)
}

#영화댓글크롤링
own_url <- "https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=167651&target=after&page="
# 벡터 틀 
all.reviews<-c()
# for문 
for(page in 1:20){
  url <- paste(own_url,page,sep='',encoding="ISO-8859-1") # cp949
  htxt <- read_html(url)  
  content <- html_nodes(htxt, '.title')
  reviews <- html_text(content)
  if(length(reviews)==0){break}    # 댓글이 없으면 if문 나오기 
  all.reviews <- c(all.reviews, reviews)
  print(page)
}
all.reviews
# str_replace
review <- str_replace_all(all.reviews,"\n|\t|신고|극한직업", replacement = "")
review






