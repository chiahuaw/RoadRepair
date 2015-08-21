

library(shiny)
library(leaflet)

causelist<-as.character(unique(acc$主要肇因))
shinyUI(navbarPage("金門縣道路資料查詢及統計",
                   tabPanel("道路養護查詢",
                            titlePanel("道路養護查詢"),
                            textInput("roadname","查詢路段",value=""),
                            dateRangeInput("bins","查詢期間：",
                                           start=as.Date(paste(format(max(road$deal_d),format="%Y"),"-01-01",sep="")),
                                           end=max(road$deal_d),
                                           min=min(road$deal_d),
                                           max=max(road$deal_d)),
                            navlistPanel("查詢",
                                         tabPanel("路段一覽表",dataTableOutput("table")),
                                         tabPanel("位置圖",plotOutput("distPlot")),
                                         tabPanel("養護時效統計長條圖",plotOutput("hoursPlot")),
                                         tabPanel("通報來源統計長條圖",plotOutput("sourcePlot"))
                            )
                   ),
                   tabPanel("101~103肇事紀錄查詢",
                            titlePanel("肇事紀錄查詢"),
                            textInput("roadnames","A.路名：",value="民生路"),
                            selectInput("cause","B.主要肇因：",
                                        choices=causelist,
                                        selected="未依規定讓車"),
                            dateRangeInput("times","C.統計區間：",
                                           start=as.Date(paste(format(max(as.Date(acc$日期)),format="%Y"),"-01-01",sep="")),
                                           end=max(as.Date(acc$日期)),
                                           min=min(as.Date(acc$日期)),
                                           max=max(as.Date(acc$日期))
                            ),
                            downloadButton('downloadData1', '下載路段查詢結果CSV檔'),
                            navlistPanel("查詢",
                                         tabPanel("A.路段查詢：一覽表",dataTableOutput(outputId="roadtable")),
                                         tabPanel("A.路段查詢：位置圖",plotOutput("plotroad")),
                                         tabPanel("B.肇因查詢：主要肇因長條圖",plotOutput("roadplot1")),
                                         tabPanel("B.肇因查詢：一覽表",dataTableOutput(outputId="causetable")),
                                         tabPanel("B.肇因查詢：位置圖",plotOutput("plotcause")),
                                         tabPanel("C.時間區間統計：一覽表",dataTableOutput(outputId="analysistable")),
                                         tabPanel("C.時間區間統計：位置圖",plotOutput("analysismap")),
                                         tabPanel("C.時間區間統計：肇因分析",plotOutput("analysiscauseplot")),
                                         tabPanel("C.時間區間統計：道路型態",plotOutput("analysisroadplot")),
                                         tabPanel("C.時間區間統計：肇事車種",plotOutput("analysiscarplot")),
                                         tabPanel("C.時間區間統計：肇事時段",plotOutput("analysishourplot")),
                                         tabPanel("C.時間區間統計：肇因街道",plotOutput("analysisstreetplot"))
                            )
                   ),
                   tabPanel("新建案位置檢視",
                            tags$head(
                              # Include our custom CSS
                              includeScript("gomap.js")
                            ),
                            
                            leafletOutput("map"),
                            DT::dataTableOutput(outputId="ziptable")
                   ),
                   tabPanel("道路挖掘督導隨機抽選",
                            titlePanel("道路挖掘督導隨機抽選"),
                            sliderInput("runcase",
                                        "隨機抽選案件數",
                                        min=1,
                                        max=length(unique(Temp[,1])),
                                        value=2,
                                        step=1),
                            dateInput("checkdate","預定督導日期：",value=Sys.Date()),
                            print(paste("系統時間：",Sys.Date())),
                            
                            navlistPanel("",
                                         tabPanel("抽選結果表",
                                                  dataTableOutput("rchecktable"),
                                                  downloadButton('rcheckdownloadData', '下載抽選結果CSV檔')
                                                  )
                            )
                   )
)
)