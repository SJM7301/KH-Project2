ui <- navbarPage(
  title = "대한민국 대기환경 시각화",
  tabPanel("미세먼지 현황",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "item_code",
                   label = "측정 항목 선택",
                   choices = c("PM10", "PM2.5" = "PM25", "SO2", "CO", "O3", "NO2"),
                   selected = "PM10"
                 ),
                 
                 # 연도 선택
                 selectInput("year", "연도 선택", 
                             choices = unique(format(as.Date(available_dates), "%Y")),
                             selected = unique(format(as.Date(available_dates), "%Y"))[1]),
                 
                 # 월 선택
                 selectInput("month", "월 선택", 
                             choices = unique(format(as.Date(available_dates), "%m")),
                             selected = unique(format(as.Date(available_dates), "%m"))[1]),
                 
                 # 일자 선택 슬라이드 (초기 값은 1로 설정)
                 sliderInput("day", "날짜 선택", 
                             min = 1, 
                             max = 31, 
                             value = 1, 
                             step = 1)
               ),
               mainPanel(
                 h4("건강 영향 및 권고 사항"),
                 htmlOutput("recommend_text"),
                 tags$style(HTML(
                   "#recommend_text {
              font-size: 15px;
              padding: 10px;
              background-color: #f8f9fa;
              border-radius: 8px;
              border: 1px solid #ccc;
              margin-bottom: 10px;
            }"
                 )),
                 leafletOutput("pm_map", height = "700px"),
                 hr(),
                 plotlyOutput("metal_pie", height = "400px")
               )
             )
           )
  ),
  tabPanel("미세먼지 동향",
           fluidPage(
             # 첫 번째 Row - 지역 선택과 날짜 범위 선택을 한 영역으로 배치
             fluidRow(
               column(12,  
                      wellPanel(  # 지역 선택과 날짜 범위 선택을 동일한 영역에 배치
                        fluidRow(
                          column(6,  # 첫 번째 컬럼 - 지역 선택
                                 checkboxGroupInput(
                                   inputId = "selected_regions",
                                   label = "지역 선택",
                                   choices = c(
                                     "서울" = "Seoul", "부산" = "Busan", "대구" = "Daegu", 
                                     "인천" = "Incheon", "광주" = "Gwangju", "대전" = "Daejeon", 
                                     "울산" = "Ulsan", "경기" = "Gyeonggi-do", "강원" = "Gangwon-do", 
                                     "충북" = "Chungcheongnam-do", "충남" = "Chungcheongbuk-do", 
                                     "전북" = "Jeollabuk-do", "전남" = "Jeollanam-do", 
                                     "경북" = "Gyeongsangbuk-do", "경남" = "Gyeongsangnam-do", 
                                     "제주" = "Jeju", "세종" = "Sejong"
                                   ),
                                   selected = c(),  
                                   inline = TRUE  # 체크박스를 수평으로 배치
                                 )
                          ),
                          column(6,  # 두 번째 컬럼 - 날짜 범위 선택
                                 radioButtons(
                                   inputId = "date_range", 
                                   label = "날짜 범위 선택", 
                                   choices = c("1개월 전", "3개월 전", "6개월 전", "12개월 전"),
                                   selected = "1개월 전",  # 기본값은 1개월 전
                                   inline = TRUE
                                 )
                          )
                        )
                      )
               )
             ),
             # 두 번째 Row - 그래프 출력
             fluidRow(  
               column(12,  
                      plotlyOutput("pm_plot", height = "800px")  # PM10 농도 그래프
               )
             )
           )
  ),
  tabPanel("쿠팡 파트너",
           fluidPage(
             tags$style(
               "body.tab-pane.coupang-tab {
                overflow: hidden;
              }",
               "iframe { 
                height: 100vh; 
                width: 100%; 
                border: none; 
                display: block;
              }"
             ),
             div(class = "tab-pane coupang-tab",
                 HTML('<iframe src="https://m.coupang.com/"></iframe>')
             )
           )
  )
)
