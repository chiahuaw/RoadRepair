

library(shiny)
library(dplyr)
library(ggplot2)
library(ggmap)
library(leaflet)

thm <- function() {
  theme_gray(base_family = "STHeiti") + # 讓Mac使用者能夠顯示中文, Windows使用者應省略這行
    theme(text=element_text(size=18))} # 將字體調整至18號

source("global.R")
  
g<-ggmap(get_map(location = c(lon = 118.375, lat = 24.445), zoom = 12, maptype = "terrain"))

shinyServer(function(input, output,session) {

  output$distPlot <- renderPlot({

    x<-road[grepl(input$roadname,road$where) & road$deal_d>=input$bins[1] & road$deal_d<=input$bins[2],]
    g+geom_point(data=x,aes(x=Longitude,y=Latitude))

  })
  
  output$hoursPlot <- renderPlot({
    
    x<-road[grepl(input$roadname,road$where) & road$deal_d>=input$bins[1] & road$deal_d<=input$bins[2],]
    x<-ddply(x,.(timelev),summarise,times=sum(cout))
    if (nrow(x) < 4) {
      if (is.na(match("0ver",x$timelev))) { x <- rbind(x,c("0ver",0)) }
      if (is.na(match("24hours",x$timelev))) { x <- rbind(x,c("24hours",0)) }
      if (is.na(match("48hours",x$timelev))) { x <- rbind(x,c("48hours",0)) }
      if (is.na(match("4hours",x$timelev))) { x <- rbind(x,c("4hours",0)) }
    }
    x$times<-as.numeric(x$times)
    x<-x[order(x$timelev,decreasing = T),]
    x$times<-round(x$times/sum(x$times,4))*100
    ggplot(data=x,aes(x=timelev,y=times))+geom_bar(stat="identity")+annotate("text",label=paste(x$times,"%"),x=x$timelev,y=x$times+3)+thm()
    
  })
  
  output$sourcePlot <- renderPlot({
    
    x<-road[grepl(input$roadname,road$where) & road$deal_d>=input$bins[1] & road$deal_d<=input$bins[2],]
    x<-ddply(x,.(source),summarise,times=sum(cout))
    if (nrow(x) < 3) {
      if (is.na(match("自行",x$source))) { x <- rbind(x,c("自行",0)) }
      if (is.na(match("民眾",x$source))) { x <- rbind(x,c("民眾",0)) }
      if (is.na(match("其他",x$source))) { x <- rbind(x,c("其他",0)) }
    }
    x$times<-as.numeric(x$times)
    x<-x[order(x$source,decreasing = T),]
    x$times<-round(x$times/sum(x$times,4))*100
    ggplot(data=x,aes(x=source,y=times))+geom_bar(stat="identity")+annotate("text",label=paste(x$times,"%"),x=x$source,y=x$times+3)+thm()
    #ggplot(data=x,aes(x=source))+geom_bar()+thm()
    
  })
  
  output$table <- renderDataTable({
    x<-road[grepl(input$roadname,road$where) & road$deal_d>=input$bins[1] & road$deal_d<=input$bins[2],]
    x[,c(1,26,15,5:11,16)]
  })
  
  ####肇事####
  output$roadtable<-renderDataTable({ #列表，顯示查詢路段的資料。
    x_road<-acc[grepl(input$roadnames,acc$道路1)==T |
                grepl(input$roadnames,acc$道路2)==T | 
                grepl(input$roadnames,acc$概略位置)==T,]
    x_road[,c(59,8,9,60:62,33:54,65:78)]
  })
  
  output$plotroad <- renderPlot({ #出圖，顯示查詢路段的散佈狀況
    x_road<-acc[grepl(input$roadnames,acc$道路1)==T |
                grepl(input$roadnames,acc$道路2)==T | 
                grepl(input$roadnames,acc$概略位置)==T,] #input$roadnames
    
    g+ geom_point(data = x_road, aes(x = lon, y = lat),col="red")
    
  })
  
  output$roadplot1 <- renderPlot({ #出圖，路段的肇因分析圖
    
    x_road<-acc[grepl(input$roadnames,acc$道路1)==T |
                grepl(input$roadnames,acc$道路2)==T | 
                grepl(input$roadnames,acc$概略位置)==T,] #input$roadnames
    
    ggplot(data=x_road,aes(x=主要肇因))+geom_bar()+thm()+
      theme(axis.text.x = element_text(angle=90, colour="black"),legend.position="none")
    
  })
  
  output$causetable<-renderDataTable({ #列表，顯示查詢主要肇因的資料。
    x_cause<-acc[grepl(input$cause,acc$主要肇因)==T,]
    x_cause[,c(59,8,9,60:62,33:54,65:78)]
  })
  
  output$plotcause <- renderPlot({ #出圖，顯示查詢主要肇因的散佈狀況
    
    x_cause<-acc[grepl(input$cause,acc$主要肇因)==T,]
    g+ geom_point(data = x_cause, aes(x = lon, y = lat),col="red")
    
  })
  
  output$downloadData1 <- downloadHandler(
    filename = function() { 'road_Accident.csv'},
    content = function(file) {
      x_road<-acc[grepl(input$roadnames,acc$道路1)==T |
                  grepl(input$roadnames,acc$道路2)==T | 
                  grepl(input$roadnames,acc$概略位置)==T,]
      write.csv(x_road[,c(59,8,9,60:62,33:54,65:78)], file,row.names=FALSE,fileEncoding = "big5")
    }
  )
  
  
  output$analysistable <-renderDataTable({ #統計區間列表。
    x_road<-acc[as.Date(acc$日期)>=as.Date(input$times[1]) & as.Date(acc$日期)<=as.Date(input$times[2]),]
    x_road[,c(59,8,9,60:62,33:54,65:78)]
  })
  
  output$analysismap<- renderPlot({ #統計區間散佈圖。
    
    x_road<-acc[as.Date(acc$日期)>=as.Date(input$times[1]) & as.Date(acc$日期)<=as.Date(input$times[2]),]
    g+ geom_point(data = x_road, aes(x = lon, y = lat),col="red")
    
  })
  
  output$analysiscauseplot<- renderPlot({ #統計區間肇因長條圖。
    
    x_road<-acc[as.Date(acc$日期)>=as.Date(input$times[1]) & as.Date(acc$日期)<=as.Date(input$times[2]),]
    ggplot(x_road,aes(x=主要肇因))+geom_bar()+thm()+theme(axis.text.x = element_text(angle=90, colour="black"))
    
  })
  
  output$analysisroadplot<- renderPlot({ #統計區間道路型態長條圖。
    
    x_road<-acc[as.Date(acc$日期)>=as.Date(input$times[1]) & as.Date(acc$日期)<=as.Date(input$times[2]),]
    ggplot(x_road,aes(x=道路型態))+geom_bar()+thm()
    
  })
  
  output$analysiscarplot<- renderPlot({ #統計區間車種長條圖。
    
    x_road<-acc[as.Date(acc$日期)>=as.Date(input$times[1]) & as.Date(acc$日期)<=as.Date(input$times[2]),]
    x_road2<-data.frame()
    for (i in 1:nrow(x_road)) {
      x_road2<-rbind(x_road2,acc2[acc2$處理編號==x_road$處理編號[i],])
    }
    ggplot(x_road2,aes(x=車種))+geom_bar()+thm()+theme(axis.text.x = element_text(angle=90, colour="black"))
    
  })
  
  output$analysishourplot<- renderPlot({ #統計區間道路型態長條圖。
    
    x_road<-acc[as.Date(acc$日期)>=as.Date(input$times[1]) & as.Date(acc$日期)<=as.Date(input$times[2]),]
    ggplot(x_road,aes(x=時))+geom_bar()+thm()
    
  })
  
  output$analysisstreetplot<-renderPlot({ #統計區間街道長條圖。
    
    x_road<-acc[as.Date(acc$日期)>=as.Date(input$times[1]) & as.Date(acc$日期)<=as.Date(input$times[2]),]
    ggplot(x_road,aes(x=街道))+geom_bar()+thm()+theme(axis.text.x = element_text(angle=90, colour="black"))
    
  })
  
  ####建案####
  output$map<-renderLeaflet({
    maps<-leaflet() %>%
      addTiles() %>% 
      addMarkers(lng=building$lon,lat=building$lat,popup=paste("建號：",building$建照號)) %>%
      setView(lng=118.375,lat=24.445,zoom=12)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      #map %>% clearPopups()
      dist <- 0.0015
      lat <- input$goto$lat
      lng <- input$goto$lon
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  output$ziptable <- DT::renderDataTable({
    temp<-building[order(building$發文日期,decreasing=T),c(3,5,6,7,9,10)]
    ztable <- temp %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', lon, '">location</a>', sep=""))
    action <- DT::dataTableAjax(session, ztable)
    
    DT::datatable(ztable, options = list(ajax = list(url = action)),escape = FALSE)
  })
  
  ####挖掘隨機抽案####
  output$rchecktable <- renderDataTable({
    
    #隨機挑選案號
    rcase<<-Temp[sample(seq(1,nrow(Temp),1),input$runcase),]
    
  })
  
  output$rcheckdownloadData <- downloadHandler(
    filename = function() { paste('RandomCheck_',input$checkdate,'.csv',sep="")},
    content = function(file) {
      write.csv(rcase, file,row.names=FALSE,fileEncoding = "big5")
    }
  )

})
