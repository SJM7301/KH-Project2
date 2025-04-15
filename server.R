server <- function(input, output, session) {
  
  clicked_region <- reactiveVal(NULL)
  metal_data_rv <- reactiveVal()
  value_rv <- reactiveVal()
  
  selected_date <- reactive({
    req(input$year, input$month, input$day)
    as.Date(paste(input$year, input$month, input$day, sep = "-"))
  })
  
  observeEvent(input$item_code, {
    clicked_region(NULL)
    metal_data_rv(NULL)
  })
  
  observeEvent(selected_date(), {
    clicked_region(NULL)
    metal_data_rv(NULL)
  })
  
  
  
  observeEvent(input$year, {
    # 선택된 연도에 해당하는 월만 필터링
    filtered_months <- unique(format(as.Date(available_dates), "%m")[format(as.Date(available_dates), "%Y") == input$year])
    
    # 월을 업데이트
    updateSelectInput(session, "month", 
                      choices = filtered_months, 
                      selected = filtered_months[1])  # 기본값은 첫 번째 월
  })
  
  observeEvent(c(input$year, input$month), {
    # 선택된 연도와 월에 맞는 날짜들을 필터링
    filtered_dates <- available_dates[format(as.Date(available_dates), "%Y") == input$year & 
                                        format(as.Date(available_dates), "%m") == input$month]
    
    # 선택된 월에 해당하는 일자 추출
    if (length(filtered_dates) > 0) {
      day_values <- as.integer(format(as.Date(filtered_dates), "%d"))
      
      # 일자 슬라이더 업데이트
      updateSliderInput(session, "day", 
                        min = min(day_values), 
                        max = max(day_values), 
                        value = min(day_values), 
                        step = 1)
    } else {
      # 데이터가 없으면 슬라이더를 초기화 또는 경고
      updateSliderInput(session, "day", 
                        min = 1, 
                        max = 1, 
                        value = 1, 
                        step = 1)
      showNotification("선택된 연도와 월에 해당하는 데이터가 없습니다.", type = "warning")
    }
  })
  
  output$pm_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 127.8, lat = 36, zoom = 7)
  })
  
  observe({
    req(selected_date())
    
    df <- get(paste0("pm_data_", input$item_code))
    
    # dataTime 컬럼이 문자형인 경우, 이를 Date 형식으로 변환
    df$dataTime <- as.Date(df$dataTime, format="%Y-%m-%d")
    
    # 필터링
    filtered <- df %>%
      filter(dataTime == selected_date())
    
    if (nrow(filtered) > 0) {
      # 각 지역 열을 기반으로 매칭
      regions <- c("Busan", "Chungcheongbuk-do", "Chungcheongnam-do", "Daegu", "Daejeon", 
                   "Gangwon-do", "Gwangju", "Gyeonggi-do", "Gyeongsangbuk-do", "Gyeongsangnam-do", 
                   "Incheon", "Jeju", "Jeollabuk-do", "Jeollanam-do", "Sejong", "Seoul", "Ulsan")
      
      merged <- filtered %>% 
        select(dataTime, one_of(regions)) %>% 
        pivot_longer(cols = one_of(regions), names_to = "NAME_1", values_to = "value") %>% 
        left_join(korea, by = "NAME_1")  # 지역 이름 기준으로 병합
      
      # 좌표 시스템을 맞추기 위해 sf 객체로 변환 (geometry 컬럼이 포함되어야 함)
      merged_sf <- st_as_sf(merged, sf_column_name = "geometry")
      
      merged_sf$value <- as.numeric(merged_sf$value)
      
      pal <- colorBin("YlOrRd", domain = merged_sf$value, bins = 6, na.color = "gray90")
      
      leafletProxy("pm_map") %>%
        clearShapes() %>%
        addPolygons(
          data = st_as_sf(merged_sf),  # sf 객체로 변환하여 전달
          fillColor = ~pal(value),
          fillOpacity = 0.7,
          weight = 1,
          color = "white",
          label = ~paste0(NAME_1, ": ", round(value, 3)),
          popup = ~paste0("<strong>", NAME_1, "</strong><br/>", input$item_code, ": ", round(value, 3)),
          layerId = ~NAME_1
        ) %>%
        clearControls() %>%
        addLegend(pal = pal, values = merged_sf$value, title = paste(input$item_code, "(단위)"))
    } else {
      showNotification("선택한 날짜에 대한 데이터가 없습니다.", type = "warning")
    }
  })
  
  observeEvent(input$pm_map_shape_click, {
    req(selected_date())
    selected_region <- input$pm_map_shape_click$id
    
    clicked_region(selected_region)
    
    df <- get(paste0("pm_data_", input$item_code))
    df$dataTime <- as.Date(df$dataTime)
    row <- df %>% filter(dataTime == selected_date())
    value <- row[[selected_region]]

    value_rv(value)
  })
  
  observe({
    req(selected_date(), clicked_region())
    region <- clicked_region()
    date <- selected_date()
    
    # 유효성 체크
    if (is.null(region) || !(region %in% names(station_map))) {
      metal_data_rv(NULL)
      return()
    }
    
    station_code <- as.character(station_map[[region]])
    
    filtered_list <- lapply(metal_codes, function(code) {
      var_name <- paste0("metal_data_", code)
      
      # 변수 존재 여부 체크
      if (!exists(var_name, envir = .GlobalEnv)) {
        message(paste("변수 없음:", var_name))
        return(NULL)
      }
      
      df <- get(var_name, envir = .GlobalEnv)
      
      # stationCode 열 존재 여부 체크
      if (!(station_code %in% colnames(df))) {
        message(paste("station code 없음:", station_code, "in", var_name))
        return(NULL)
      }
      
      # 날짜 필터링 후 결과 확인
      df_filtered <- df %>%
        filter(dataTime == date)
      
      if (nrow(df_filtered) == 0) {
        message(paste("필터 결과 없음:", var_name, date))
        return(NULL)
      }
      
      df_filtered <- df_filtered %>%
        select(dataTime, itemCode, !!sym(station_code)) %>%
        rename(Concentration = !!sym(station_code), Metal = itemCode)
      
      return(df_filtered)
    })
    
    # NULL 제거 및 병합
    metal_long <- bind_rows(filtered_list[!sapply(filtered_list, is.null)])
    
    if (nrow(metal_long) == 0) {
      message(" 모든 금속 데이터가 비어 있음")
      metal_data_rv(NULL)
    } else {
      metal_data_rv(metal_long)
    }
  })
  
  output$metal_pie <- renderPlotly({
    metal_df <- metal_data_rv()
    req(clicked_region(), selected_date())
    
    metal_df <- metal_df %>%
      mutate(Concentration = as.numeric(Concentration)) %>%
      filter(!is.na(Concentration) & Concentration > 0)
    
    if (nrow(metal_df) == 0) {
      return(plotly_empty(type = "pie") %>%
               layout(title = list(text = paste0(
                 clicked_region(), " 금속 데이터 없음 (", selected_date(), ")"),
                 font = list(color = "gray")),
                 margin = 100))
    }
    
    plot_ly(
      metal_df,
      labels = ~Metal,
      values = ~Concentration,
      type = 'pie',
      textinfo = 'label+percent',
      insidetextorientation = 'radial',
      hole = 0.4,
      marker = list(line = list(color = '#FFFFFF', width = 1))
    ) %>%
      layout(
        title = list(text = paste0(clicked_region(), " 금속 성분 분포 (", selected_date(), ")")),
        margin = 100,
        showlegend = TRUE
      )
  })
  
  output$recommend_text <- renderUI({
    req(value_rv(), clicked_region())
    
    metal_df <- metal_data_rv()
    pm10_value <- value_rv()
    
    pm_msg <- pm_risk_message(pm10_value)
    
    if (is.null(metal_df) || nrow(metal_df) == 0) {
      metal_msg <- "선택한 지역/날짜에 금속 성분 데이터가 없습니다."
    } else {
      metal_msg <- metal_risks(metal_df)
    }
    
    HTML(paste(pm_msg, metal_msg, sep = "<br/><hr style='margin: 10px 0;'>"))
  })
  
  pm_risk_message <- function(pm10) {
    if (is.na(pm10)) return("PM10 정보 없음")
    case_when(
      pm10 <= 30 ~ "🌿 공기가 맑아요! 마스크 없이 외출해도 괜찮습니다.",
      pm10 <= 80 ~ paste0("🙂 보통 수준입니다. 민감군은 마스크를 착용하세요.", "<a href='https://search.shopping.naver.com/search/all?bt=-1&frm=NVSCPRO&query=%EB%8D%B4%ED%83%88+%EB%A7%88%EC%8A%A4%ED%81%AC' target='_blank'>[덴탈 마스크 구매 링크]</a>"),
      pm10 <= 150 ~ paste0("😷 나쁨 수준입니다. KF80 이상 마스크를 권장합니다. ", "<a href='https://search.shopping.naver.com/search/all?query=KF80&vertical=search' target='_blank'>[KF80 구매 링크]</a>"),
      pm10 > 150 ~ paste0("🚨 매우 나쁨! KF94 이상 착용하고 외출은 피하세요. ", "<a href='https://search.shopping.naver.com/search/all?query=KF94&vertical=search' target='_blank'>[KF94 구매 링크]</a>"),
      TRUE ~ "PM10 정보 없음"
    )
  }
  
  metal_risks <- function(metal_df) {
    out <- c()
    
    if ("Pb" %in% metal_df$Metal) {
      pb_val <- metal_df$Concentration[metal_df$Metal == "Pb"]
      if (length(pb_val) > 0 && !is.na(pb_val) && pb_val > 0.05) {
        out <- c(out, "납(Pb) 농도가 높습니다. 어린이/임산부는 외출을 자제하세요.")
      }
    }
    
    if ("Mn" %in% metal_df$Metal) {
      mn_val <- metal_df$Concentration[metal_df$Metal == "Mn"]
      if (length(mn_val) > 0 && !is.na(mn_val) && mn_val > 0.02) {
        out <- c(out, "망간(Mn) 수치가 높습니다. 마스크 착용이 필수입니다.")
      }
    }
    
    if ("Ni" %in% metal_df$Metal) {
      ni_val <- metal_df$Concentration[metal_df$Metal == "Ni"]
      if (length(ni_val) > 0 && !is.na(ni_val) && ni_val > 0.015) {
        out <- c(out, "니켈(Ni) 수치가 높습니다. 민감군은 KF94 마스크 착용을 권장합니다.")
      }
    }
    
    if (length(out) == 0) return("금속 성분 수치는 비교적 안정적인 편입니다.")
    paste(out, collapse = "<br/>")
  }
  
  output$pm_plot <- renderPlotly({
    selected_regions <- input$selected_regions
    selected_date_range <- input$date_range
    
    # 오늘 날짜 구하기
    today <- Sys.Date()
    
    # 날짜 범위 계산 (1개월, 3개월, 6개월, 12개월 전)
    start_date <- switch(selected_date_range,
                         "1개월 전" = today %m-% months(1),
                         "3개월 전" = today %m-% months(3),
                         "6개월 전" = today %m-% months(6),
                         "12개월 전" = today %m-% months(12),
                         today)  # 기본값은 오늘
    
    # PM10 데이터만 필터링 (선택한 지역과 날짜 범위 적용)
    filtered_data <- pm_data_PM10 %>%
      mutate(dataTime = as.Date(dataTime)) %>%  # dataTime을 Date 형식으로 변환
      select(dataTime, all_of(selected_regions), Average) %>%
      filter(dataTime >= start_date & dataTime <= today) %>%
      arrange(dataTime)
    
    # filtered_data에 데이터가 없는 경우 처리
    if (nrow(filtered_data) == 0) {
      showNotification("선택한 날짜 범위에 해당하는 데이터가 없습니다.", type = "warning")
      return(NULL)
    }
    
    # 날짜 범위 내에서 7일 간격으로 날짜 리스트 만들기
    date_seq <- seq(from = start_date, to = today, by = "7 days")
    
    # 선 그래프 그리기 (기본 평균값)
    p <- plot_ly(filtered_data, x = ~dataTime, y = ~Average, type = 'scatter', mode = 'lines+markers', name = "Average")
    
    # 선택된 지역들에 대해 트레이스 추가
    for (region in selected_regions) {
      p <- p %>% add_trace(x = filtered_data$dataTime, y = filtered_data[[region]], name = region)
    }
    
    # x축 설정 (7일 간격의 날짜 표시)
    p <- p %>% layout(
      title = "지역별 PM10 농도",
      xaxis = list(
        title = "날짜",
        tickmode = "array",  # 배열 방식으로 tick값 설정
        tickvals = date_seq,  # 7일 간격으로 생성된 날짜 값들
        tickformat = "%Y-%m-%d",  # 날짜 형식
        tickangle = 45  # 각도 조정
      ),
      yaxis = list(title = "PM10 농도"),
      margin = list(t = 50, r = 50, b = 50, l = 50)
    )
    
    return(p)
  })
}
