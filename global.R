library(dplyr)
library(plyr)
library(XML)
library(RCurl)

# from:http://shiny.rstudio.com/gallery/unicode-characters.html
# Cairo包的PNG设备似乎无法显示中文字符，强制使用R自身的png()设备
options(shiny.usecairo = FALSE)

# 请忽略以下代码，它只是为了解决ShinyApps上没有中文字体的问题
font_home <- function(path = '') file.path('~', '.fonts', path)
if (Sys.info()[['sysname']] == 'Linux' &&
      system('locate wqy-zenhei.ttc') != 0 &&
      !file.exists(font_home('wqy-zenhei.ttc'))) {
  if (!file.exists('wqy-zenhei.ttc'))
    shiny:::download(
      'https://github.com/rstudio/shiny-examples/releases/download/v0.10.1/wqy-zenhei.ttc',
      'wqy-zenhei.ttc'
    )
  dir.create(font_home())
  file.copy('wqy-zenhei.ttc', font_home())
  system2('fc-cache', paste('-f', font_home()))
}
rm(font_home)

####載入資料####

f <- file("data/roadRepairData.csv",encoding="UTF-8")
road<-read.csv(f,stringsAsFactors = F)

road$find_d<-as.Date(road$find_d)
road$deal_d<-as.Date(road$deal_d)

road$weeks_find <- weekdays(road$find_d)
road$weeks_deal <- weekdays(road$deal_d)
road$month_find <-format(road$find_d,format="%m")
road$month_deal <-format(road$deal_d,format="%m")
####計算完成時間級距####
dat_st <-paste(road[,5],road[,6],sep=" ")
dat_et <-paste(road[,7],road[,8],sep=" ")
dat_st <- as.POSIXlt(dat_st,format="%Y-%m-%d %H:%M")
dat_et <- as.POSIXlt(dat_et,format="%Y-%m-%d %H:%M")
road$cost <- as.numeric(dat_et-dat_st)
road$cost <- round(abs(road$cost)/60,2)

road$timelev <- ""
road[(road$cost/60)>48,26]<-"0ver"
road[(road$cost/60)<=48 & (road$cost/60)>24,26]<-"48hours"
road[(road$cost/60)<=24 & (road$cost/60)>4,26]<-"24hours"
road[(road$cost/60)<=4,26]<-"4hours"

rm(dat_st)
rm(dat_et)


wherelist<-ddply(road,c("where","lev"),summarise,cout=sum(cout),area=sum(area),mean_time=round(mean(cost),2))

####肇事資料####
f<-file("data/form_all.csv",encoding="big5")
acc<-read.csv(f,stringsAsFactors = F)
f2<-file("data/form2orig.csv",encoding="big5")
acc2<-read.csv(f,stringsAsFactors = F)

####建案資料####
f3 <- file("data/buildingGeo.csv",encoding="big5")
building <- read.csv(f3,encoding="big5",stringsAsFactors=F)
#building <-mutate(building,發文日期=as.Date(發文日期)) %>% arrange(.,desc(發文日期))

####道路挖掘督導抽選####
url <-'http://pipe.kinmen.gov.tw/kinmen/Civilian/LocationQuery.aspx'
url_list <- unlist(url) #去格式化

##Get the table online
#核心程式
myTemp <- function(url){
  #抓取url
  get_url = getURL(url,encoding = "UTF-8")
  #將url解析
  get_url_parse = htmlParse(get_url, encoding = "UTF-8")
  #抓取關鍵的變項，我們需要的變項夾在一個div的class=tqtongji2，裡面<ul>中的<li>標籤裡面
  #標籤裡面還有一些沒有用到的東西沒關係，事後再一併移除
  tablehead <- xpathSApply(get_url_parse, "//table[@class='TurnInTable']/tr/td", xmlValue)
  #將擷取到的關鍵字轉成容易閱讀的矩陣格式
  table <- matrix(tablehead, ncol = 8, byrow = TRUE)
  #回傳值
  return(table)
}

Temp_Total <- lapply(url_list, myTemp)

#清理資料

rcheck<-as.data.frame(Temp_Total,stringsAsFactors = F)
rcheck2<-list()

for (i in 1:nrow(rcheck)) {
  for ( t in 1:ncol(rcheck)) {
    rcheck[i,t]<-gsub("\r\n","",rcheck[i,t])
    rcheck[i,t]<-gsub("\\s","",rcheck[i,t])
    rcheck2<-rbind(rcheck2,rcheck[i,t])
  }
}
rcheck2<-unlist(rcheck2)
Temp <- matrix(ncol =8,nrow=length(rcheck2)/8)
Temp<-matrix(rcheck2,ncol=8,byrow=T)
Temp<-as.data.frame(Temp,stringsAsFactors = F)

colnames(Temp)<-c("CaseNumber","Date","Time","Applicant","Contract","Township","Roads","Note")
