# install.packages(c('XML', 'shiny', 'leaflet', 'dplyr', 'tidyr', 'sf', 'geodata', 'ggplot2'))

library(XML)
library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(geodata)
library(sf)
library(ggplot2)
library(furrr)
library(future)
library(promises)
library(plotly)
library(openxlsx)
library(fs)
library(lubridate)
plan(multisession)

korea <- sf::st_as_sf(gadm("KOR", level = 1, path = tempdir()))
korea <- st_transform(korea, crs = 4326)

station_map <- list(
  "Seoul" = 1,
  "Incheon" = 1,
  "Gyeonggi-do" = 1,
  
  "Busan" = 6,
  "Ulsan" = 6,
  "Gyeongsangnam-do" = 6,
  
  "Daegu" = 8,
  "Gyeongsangbuk-do" = 8,
  
  "Daejeon" = 4,
  "Sejong" = 4,
  "Chungcheongnam-do" = 4,
  
  "Chungcheongbuk-do" = 11,
  
  "Jeollabuk-do" = 3,
  "Gwangju" = 3,
  
  "Jeollanam-do" = 9,
  
  "Gangwon-do" = 10,
  
  "Jeju" = 5
)

metal_itemcodes <- c(
  Pb = 90303,  # 납
  Ni = 90304,  # 니켈
  Mn = 90305,  # 망간
  Zn = 90314,  # 아연
  Ca = 90319,  # 칼슘
  K  = 90318,  # 칼륨
  S  = 90325   # 황
)

map_region_to_name <- function(region) {
  case_when(
    region == "seoul" ~ "Seoul",
    region == "busan" ~ "Busan",
    region == "daegu" ~ "Daegu",
    region == "incheon" ~ "Incheon",
    region == "gwangju" ~ "Gwangju",
    region == "daejeon" ~ "Daejeon",
    region == "ulsan" ~ "Ulsan",
    region == "sejong" ~ "Sejong",
    region == "gyeonggi" ~ "Gyeonggi-do",
    region == "gangwon" ~ "Gangwon-do",
    region == "chungbuk" ~ "Chungcheongbuk-do",
    region == "chungnam" ~ "Chungcheongnam-do",
    region == "jeonbuk" ~ "Jeollabuk-do",
    region == "jeonnam" ~ "Jeollanam-do",
    region == "gyeongbuk" ~ "Gyeongsangbuk-do",
    region == "gyeongnam" ~ "Gyeongsangnam-do",
    region == "jeju" ~ "Jeju",
    TRUE ~ NA_character_
  )
}

load_pm_data <- function(code) {
  service_key <- "WuIaAWF%2FNdLestNiUoCSY8GZDyAwg%2BllkHbHc2R3jMFAvCOBBQLy7%2Bks8wGpqybpN13A29YXjv9yh%2FKLnvVxng%3D%3D"
  service_url <- "http://apis.data.go.kr/B552584/ArpltnStatsSvc/"
  operation <- "getCtprvnMesureLIst"
  
  url <- paste0(
    service_url, operation,
    "?serviceKey=", service_key,
    "&resultType=xml",
    "&numOfRows=100",
    "&pageNo=1",
    "&itemCode=", code,
    "&dataGubun=DAILY",
    "&searchCondition=MONTH"
  )
  
  doc <- xmlTreeParse(url, useInternalNodes = TRUE, encoding = "UTF-8")
  rootNode <- xmlRoot(doc)
  xmlData <- xmlToDataFrame(nodes = getNodeSet(rootNode, "//item"))
  
  if (length(xmlData) == 0) {
    message("API 응답에 데이터가 없습니다.")
    return(data.frame())
  }
  df <- xmlData %>%
    select(dataTime, seoul, busan, daegu, incheon, gwangju, daejeon, ulsan,
           sejong, gyeonggi, gangwon, chungbuk, chungnam,
           jeonbuk, jeonnam, gyeongbuk, gyeongnam, jeju) %>%
    pivot_longer(cols = -dataTime, names_to = "region", values_to = "value") %>%
    mutate(
      region = tolower(trimws(region)),
      value = na_if(value, "-"),
      value = as.numeric(value),
      NAME = map_region_to_name(region),
      itemCode = code
    ) %>%
    select(dataTime, itemCode, NAME, value) %>%
    spread(key = NAME, value = value)
  
  if (code == "PM10") {
    df <- df %>%
      mutate(Average = rowMeans(select(df, -dataTime, -itemCode), na.rm = TRUE))  # 데이터 열들의 평균을 계산
  } else {
    # Average 열이 존재하면 그대로 두고, 없으면 NA로 추가
    if (!"Average" %in% colnames(df)) {
      df$Average <- NA
    }
  }
  
  return(df)
}

load_or_update_pm_data <- function(code) {
  file_path <- "data_all.xlsx"
  
  if (file_exists(file_path)) {
    message("파일이 이미 존재합니다. 데이터를 업데이트합니다.")
    existing_data <- read.xlsx(file_path, sheet = 1)
    
    # itemCode가 이미 존재하는지 확인
    if (!(code %in% unique(existing_data$itemCode))) {
      # itemCode가 존재하지 않으면 새 데이터 추가
      new_data <- try(load_pm_data(code), silent = TRUE)
      
      if (inherits(new_data, "try-error") || nrow(new_data) == 0) {
        message("API 호출 실패 또는 데이터 없음. 기존 데이터만 반환합니다.")
        return(existing_data)
      }
      
      all_data <- rbind(existing_data, new_data)
      write.xlsx(all_data, file_path)
      message("새로운 itemCode 데이터가 추가되었습니다.")
      return(all_data)
    } else {
      # itemCode가 이미 존재하면 데이터 날짜를 체크하여 업데이트 여부 판단
      existing_data_filtered <- existing_data %>% filter(itemCode == code)
      
      latest_date <- max(existing_data_filtered$dataTime, na.rm = TRUE)
      
      print(paste0("대기성분: ", code, " 최신날짜: ", latest_date))
      
      new_data <- try(load_pm_data(code), silent = TRUE)
      
      if (inherits(new_data, "try-error") || nrow(new_data) == 0) {
        message("API 호출 실패 또는 데이터 없음. 기존 데이터만 반환합니다.")
        return(existing_data)
      }
      
      new_data <- load_pm_data(code) %>%
        filter(dataTime > latest_date)  # 기존 데이터 이후만 필터링
      if (nrow(new_data) > 0) {
        updated_data <- rbind(existing_data, new_data)  # 데이터 병합
        updated_data <- updated_data %>% arrange(dataTime) # 데이터 정렬
        write.xlsx(updated_data, file_path)
        message(paste0("data_all.xlsx 업데이트 완료: ", code))
        return(existing_data)
      } else {
        message("새로운 데이터가 없습니다.")
      }
      return(existing_data)
    }
  } else {
    message("파일이 존재하지 않습니다. 데이터를 새로 만듭니다.")
    
    # 새로운 데이터 호출
    all_data <- load_pm_data(code)
    
    write.xlsx(all_data, file_path)
    message("새로운 엑셀 파일이 생성되었습니다.")
    
    return(all_data)
  }
}

load_metal_data <- function(metal_code, date, stationcode) {
  service_key <- "WuIaAWF%2FNdLestNiUoCSY8GZDyAwg%2BllkHbHc2R3jMFAvCOBBQLy7%2Bks8wGpqybpN13A29YXjv9yh%2FKLnvVxng%3D%3D"
  service_url <- "http://apis.data.go.kr/1480523/MetalMeasuringResultService/"
  operation <- "MetalService"  # 실제 API에 맞게 수정 필요
  
  itemcode <- metal_itemcodes[[metal_code]]
  date <- as.Date(date)
  date_str <- format(as.Date(date), "%Y%m%d")  # 날짜 포맷 YYYYMMDD
  
  url <- paste0(
    service_url, operation,
    "?serviceKey=", service_key,
    "&pageNo=1",
    "&numOfRows=1000",
    "&resultType=XML",
    "&date=", date_str,
    "&stationcode=", stationcode,
    "&itemcode=", itemcode,
    "&timecode=RH24"
  )
  
  doc <- xmlTreeParse(url, useInternalNodes = TRUE, encoding = "UTF-8")
  rootNode <- xmlRoot(doc)
  items <- getNodeSet(rootNode, "//item")
  xmlData <- xmlToDataFrame(items)
  if (nrow(xmlData) == 0) return(NULL)
  
  df <- xmlData %>%
    filter(stringr::str_sub(sdate, -6, -1) == "220000") %>%  # 22시 데이터만
    mutate(
      dataTime = as.Date(sdate, format = "%Y%m%d%H%M%S"),
      stationCode = as.character(stationcode),
      value = as.character(round(as.numeric(value))),
      itemCode = metal_code
    ) %>%
    select(dataTime, itemCode, stationCode, value)
  
  return(df)
}

load_or_update_metal_data <- function(metal_code) {
  file_path <- "metal_all.xlsx"
  station_codes <- unique(unlist(station_map, use.names = FALSE))
  yesterday <- Sys.Date() - 1
  
  # 기존 데이터 읽기
  if (file.exists(file_path)) {
    existing_data <- read.xlsx(file_path)
    # 날짜 컬럼 변환 처리 (Excel serial number 또는 문자형 모두 대응)
    if ("dataTime" %in% names(existing_data)) {
      if (is.numeric(existing_data$dataTime)) {
        existing_data$dataTime <- as.Date(existing_data$dataTime, origin = "1899-12-30")
      } else {
        existing_data$dataTime <- as.Date(existing_data$dataTime)
      }
    }
  } else {
    message("metal_all.xlsx 파일이 존재하지 않아 새로 생성합니다.")
    existing_data <- data.frame()
  }
  
  if ("itemCode" %in% names(existing_data) && metal_code %in% existing_data$itemCode) {
    # 기존 데이터에서 해당 금속코드만 추출
    filtered <- existing_data[existing_data$itemCode == metal_code, ]
    
    if (length(filtered$dataTime) > 0) {
      latest_date <- max(filtered$dataTime, na.rm = TRUE)
    } else {
      latest_date <- as.Date("2024-02-01")
    }
  } else {
    latest_date <- as.Date("2024-02-01")
  }
  print(paste0("금속성분: ", metal_code, " 최신날짜: ", latest_date))
  # 날짜 벡터 생성 (누락된 날짜들만)
  if (latest_date < yesterday) {
    dates_to_fetch <- seq.Date(from = latest_date + 1, to = yesterday, by = "day")
  } else {
    dates_to_fetch <- c()  # 최신이면 빈 벡터로
  }
  if (length(dates_to_fetch) == 0) {
    message("최신 데이터가 이미 반영되어 있습니다.")
    return(existing_data %>% filter(itemCode == metal_code))
  }
  
  all_new_data <- list()
  
  for (date in dates_to_fetch) {
    for (station in station_codes) {
      df <- try(load_metal_data(metal_code, date, station), silent = TRUE)
      if (!inherits(df, "try-error") && !is.null(df)) {
        all_new_data[[length(all_new_data) + 1]] <- df
      }
    }
  }
  
  # 실패한 경우
  if (length(all_new_data) == 0) {
    message("API 호출 실패 또는 새로운 데이터가 없습니다.")
    return(existing_data %>% filter(itemCode == metal_code))
  }
  
  new_data_long <- bind_rows(all_new_data)
  
  new_data_wide <- bind_rows(all_new_data) %>%
    pivot_wider(
      names_from = stationCode,
      values_from = value
    ) %>%
    arrange(dataTime)

  # 병합
  if (nrow(existing_data) > 0) {
    combined <- bind_rows(existing_data, new_data_wide) %>%
      distinct(dataTime, itemCode, .keep_all = TRUE) %>%
      arrange(desc(dataTime))
  } else {
    combined <- new_data_wide
  }
  
  write.xlsx(combined, file_path, overwrite = TRUE)
  message(paste0("metal_all.xlsx 업데이트 완료: ", metal_code))
  
  return(new_data_wide)
}


item_codes <- c("PM10", "PM25", "SO2", "CO", "O3", "NO2")
metal_codes <- c("Ca", "K", "Mn", "Ni", "Pb", "S", "Zn")

for (code in item_codes) {
  data <- load_or_update_pm_data(code)
  filtered_data <- data %>%
    filter(itemCode == code)
  assign(paste0("pm_data_", code), filtered_data)
}

for (code in metal_codes) {
  data <- load_or_update_metal_data(code)  
  filtered_data <- data %>%
    filter(itemCode == code)              
  assign(paste0("metal_data_", code), filtered_data)
}

available_dates <- sort(unique(pm_data_PM10$dataTime))