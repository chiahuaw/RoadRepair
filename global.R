library(dplyr)
library(plyr)

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