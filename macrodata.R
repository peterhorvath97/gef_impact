data.query.Q <- function(){
  require(fredr)

  intrates <- function(){
    require(tidyverse)
    require(imfr)
    require(lubridate)
    require(stringr)
    
    data <- imfr::imf_dataset('ifs', 
                              #ref_area = countries,
                              indicator = c('FIMM_PA', 'FPOLM_PA', 'FILIBOR_1M_PA', 'FITB_3M_PA', 'FII_3M_PA'),
                              freq = 'Q')
    
    data <- data %>% 
      as_tibble() %>% 
      select(ccode2 = ref_area, indicator, date, value) %>% 
      mutate(year = substr(date, 1, 4),
             month = case_when(str_detect(date, 'Q1') ~ '01',
                               str_detect(date, 'Q2') ~ '04',
                               str_detect(date, 'Q3') ~ '07',
                               str_detect(date, 'Q4') ~ '10'),
             date = paste(year, month, '01', sep = '-') %>% as_date(),
             value = as.numeric(value)) %>% 
      select(-year, -month) %>% 
      rename(R = value)
    
    
    data <- data %>% 
      group_by(ccode2, indicator) %>% 
      mutate(n = n()) %>% 
      ungroup(indicator) %>% 
      filter(n == max(n)) %>% 
      ungroup() %>% 
      select(-n)
    
    
    data <- data %>% 
      select(ccode2, date, R)
    
    
    
    data2 <- bind_rows(fredr('IR3TIB01ATM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'AT'),
                       fredr('IR3TIB01BEM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'BE'),
                       fredr('IR3TIB01GRM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'GR'),
                       fredr('LVAIR3TIB01STM', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'LV'),
                       fredr('IR3TIB01SKM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'SK'),
                       fredr('IR3TIB01LUM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'LU'),
                       fredr('IR3TIB01EEM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'EE'),
                       fredr('IR3TIB01PTM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'PT'),
                       fredr('LTUIR3TIB01STM', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'LT'),
                       fredr('IRSTCI01CHM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'CH'),
                       fredr('IR3TIB01NLM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'NL'),
                       fredr('IR3TIB01FRM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'FR'),
                       fredr('IR3TIB01DEM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'DE'),
                       fredr('IR3TIB01GBM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'GB'),
                       fredr('IR3TIB01IDM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'ID'),
                       fredr('IR3TIB01SIM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'SI'),
                       fredr('IRSTCI01JPM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'JP'),
                       fredr('IR3TIB01IEM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'IE'),
                       fredr('IR3TIB01CAM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'CA'),
                       fredr('IR3TIB01SEM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'SE'),
                       fredr('IR3TIB01FIM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'FI'),
                       fredr('IR3TIB01ITM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'IT'),
                       fredr('IR3TIB01ESM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'ES'),
                       fredr('FEDFUNDS', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'US'),
                       fredr('IRSTCI01INM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'IN'),
                       fredr('IR3TIB01IDM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'ID'),
                       fredr('IR3TIB01PLM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'PL'),
                       fredr('INTDSRTRM193N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'TR'),
                       fredr('IR3TIB01CZM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'CZ'),
                       fredr('COLIR3TIB01STM', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'CO'),
                       fredr('IRSTCI01MXM156N', frequency = 'q') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'MX')
                       
    )
    
    
    data <- data %>% 
      filter(!(ccode2 %in% data2$ccode2)) %>% 
      bind_rows(data2)
    
    data
    
  }
  
  gdp <- function(){
    require(tidyverse)
    require(imfr)
    require(lubridate)
    require(stringr)
    require(fredr)
    
    
    data1 <- imfr::imf_dataset('ifs', 
                               #ref_area = countries[i],
                               indicator = 'NGDP_SA_XDC',
                               freq = 'Q')
    
    data2 <- imfr::imf_dataset('ifs', 
                               #ref_area = countries[i],
                               indicator = 'NGDP_D_SA_IX',
                               freq = 'Q')
    
    
    data1 <- data1 %>% 
      as_tibble() %>% 
      select(ccode2 = ref_area, date, value) %>% 
      mutate(year = substr(date, 1, 4),
             month = case_when(str_detect(date, 'Q1') ~ '01',
                               str_detect(date, 'Q2') ~ '04',
                               str_detect(date, 'Q3') ~ '07',
                               str_detect(date, 'Q4') ~ '10'),
             date = paste(year, month, '01', sep = '-') %>% as_date(),
             value = as.numeric(value)) %>% 
      select(-year, -month) %>% 
      rename(Y = value)
    
    
    data2 <- data2 %>% 
      as_tibble() %>% 
      select(ccode2 = ref_area, date, value) %>% 
      mutate(year = substr(date, 1, 4),
             month = case_when(str_detect(date, 'Q1') ~ '01',
                               str_detect(date, 'Q2') ~ '04',
                               str_detect(date, 'Q3') ~ '07',
                               str_detect(date, 'Q4') ~ '10'),
             date = paste(year, month, '01', sep = '-') %>% as_date(),
             value = as.numeric(value)) %>% 
      select(-year, -month) %>% 
      rename(P = value)
    
    data <- data1 %>% 
      inner_join(data2) %>% 
      mutate(Y = 100*Y/P) %>% 
      select(-P)
    
    
    
    data <- data %>% 
      bind_rows(fredr('NAEXKP01RUQ652S') %>% 
                  select(date, Y = value) %>% 
                  mutate(ccode2 = 'RU')) %>% 
      bind_rows(fredr('CLVMNACSAB1GQIS') %>% 
                  select(date, Y = value) %>% 
                  mutate(ccode2 = 'IS'))
    
    
    
    data <- data %>% 
      mutate(year = year(date)) %>% 
      group_by(ccode2, year) %>% 
      mutate(mean = mean(Y),
             Y2010 = ifelse(year == 2010,1,NA),
             mean = mean*Y2010) %>% 
      ungroup(year) %>% 
      mutate(mean = mean(mean, na.rm = T),
             Y = 100*Y/mean) %>% 
      ungroup()
    
    
    data <- data %>% 
      select(ccode2, date, Y)
    
    data
  }
  
  cpi <- function(){
    require(tidyverse)
    require(imfr)
    require(lubridate)
    require(stringr)
    
    data <-   imfr::imf_dataset('ifs', 
                                #ref_area = countries,
                                indicator = 'PCPI_IX',
                                freq = 'Q')
    
    
    
    data <- data %>% 
      as_tibble() %>% 
      select(ccode2 = ref_area, date, value) %>% 
      mutate(year = substr(date, 1, 4),
             month = case_when(str_detect(date, 'Q1') ~ '01',
                               str_detect(date, 'Q2') ~ '04',
                               str_detect(date, 'Q3') ~ '07',
                               str_detect(date, 'Q4') ~ '10'),
             date = paste(year, month, '01', sep = '-') %>% as_date(),
             value = as.numeric(value)) %>% 
      select(-year, -month) %>% 
      rename(P = value)
    
    data
    
  }
  
  reer <- function(){
    # Load required packages
    require(rvest)
    require(httr)
    require(readxl)
    
    # Step 1: Specify the URL of the webpage
    url <- "https://www.bruegel.org/publications/datasets/real-effective-exchange-rates-for-178-countries-a-new-database"
    
    # Step 2: Scrape the webpage and find the download link
    webpage <- read_html(url)
    
    download_link <- webpage %>%
      html_nodes("a") %>%               # Find all anchor tags
      html_attr("href") %>%             # Extract the 'href' attributes
      .[grepl("\\.zip$", ., ignore.case = TRUE)] # Filter links ending with '.zip'
    
    # Check if a download link was found
    if (length(download_link) == 0) {
      stop("No download link found!")
    }
    
    # Since the link is relative, make it absolute if necessary
    download_link <- ifelse(grepl("^http", download_link), download_link, paste0("https://www.bruegel.org", download_link))
    
    # Step 3: Download the ZIP file
    # Step 3: Create a dedicated temporary directory
    temp_dir <- file.path(getwd(), "bruegel_temp")
    if (!dir.exists(temp_dir)) {
      dir.create(temp_dir)
    }
    zip_file <- file.path(temp_dir, "data.zip")
    download.file(download_link, zip_file, mode = "wb")
    
    # Step 4: Unzip the ZIP file
    unzip_dir <- file.path(temp_dir, "unzipped")
    unzip(zip_file, exdir = unzip_dir)
    
    # Step 5: Find the Excel file in the unzipped content
    xls_files <- list.files(unzip_dir, pattern = "\\.xls$", full.names = TRUE)
    if (length(xls_files) == 0) {
      stop("No Excel file found in the unzipped content!")
    }
    
    # Step 6: Read the specific sheet 'REER_MONTHLY_120' from the Excel file
    sheet_name <- "REER_MONTHLY_120"
    reer_data <- read_excel(xls_files[1], sheet = sheet_name)
    
    
    # Clean up temporary files
    unlink(zip_file)
    unlink(unzip_dir, recursive = TRUE)
    
    names(reer_data)[1] <- "date"
    
    
    reer_data <- reer_data %>% 
      mutate(year = substr(date, 1, 4),
             month = substr(date, 6,7),
             date = paste(year, month, '01', sep = '-') %>% as_date()) %>% 
      select(-year, -month) %>% 
      gather(key, value, -date) %>% 
      mutate(ccode2 = substr(key, nchar(key)-1, nchar(key))) %>% 
      rename(E = value) %>% 
      select(-key) %>% 
      mutate(quarter = quarter(date),
             year = year(date),
             month = month(date)) %>% 
      group_by(ccode2, quarter, year) %>% 
      drop_na() %>% 
      mutate(E = mean(E, na.rm = T)) %>% 
      filter(month %in% c(1, 4, 7, 10)) %>% 
      ungroup() %>% 
      select(ccode2, date, E)
    
    
    unlink(temp_dir, recursive = TRUE)
    
    
    reer_data
    
  }
  
  tot <- function(){
    require(tidyverse)
    require(imfr)
    require(lubridate)
    require(stringr)
    
    pimp <- imfr::imf_dataset('ifs', 
                              indicator = 'PMP_IX',
                              freq = 'Q') %>% 
      mutate(value = as.numeric(value)) %>% 
      select(ccode2 = ref_area, date, pimp = value) %>% 
      as_tibble() %>% 
      mutate(date = paste(date, '-01', sep = '') %>% as_date())
    
    pexp <- imfr::imf_dataset('ifs', 
                              indicator = 'PXP_IX',
                              freq = 'Q') %>% 
      mutate(value = as.numeric(value)) %>% 
      select(ccode2 = ref_area, date, pexp = value) %>% 
      as_tibble() %>% 
      mutate(date = paste(date, '-01', sep = '') %>% as_date())
    
    
    
    data <- pexp %>% 
      inner_join(pimp) %>% 
      mutate(TOT = 100*pexp/pimp) %>% 
      select(ccode2, date, TOT)
    
    data
    
  }
  
  

  
  data_intrates <- intrates()
  data_gdp <- gdp()
  data_cpi <- cpi()
  data_reer <- reer()
  data_tot <- tot()
  

  
  
  dbnames <- ls()[str_detect(ls(), 'data_')]
  
  data <- get(dbnames[1])
  
  for(name in dbnames[-1]){
    data <- full_join(data, get(name))
  }
  
  
  
  data <- data %>% 
    filter(!is.na(P),
           !is.na(Y),
           !is.na(R), 
           !is.na(E),
           !is.na(TOT))
  
  data
}

data.query.M <- function(){
  require(fredr)
  
  intrates <- function(){
    require(tidyverse)
    require(imfr)
    require(lubridate)
    require(stringr)
    
    data <- imfr::imf_dataset('ifs', 
                              #ref_area = countries,
                              indicator = c('FIMM_PA', 'FPOLM_PA', 'FILIBOR_1M_PA', 'FITB_3M_PA', 'FII_3M_PA'),
                              freq = 'M')
    
    data <- data %>% 
      as_tibble() %>% 
      select(ccode2 = ref_area, indicator, date, value) %>% 
      mutate(date = paste(date, '-01', sep = '') %>% as_date()) %>% 
      rename(R = value)
    
    
    data <- data %>% 
      group_by(ccode2, indicator) %>% 
      mutate(n = n()) %>% 
      ungroup(indicator) %>% 
      filter(n == max(n)) %>% 
      ungroup() %>% 
      select(-n)
    
    
    data <- data %>% 
      select(ccode2, date, R) %>% 
      mutate(R = as.numeric(R))
    
    
    
    data2 <- bind_rows(fredr('IR3TIB01ATM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'AT'),
                       fredr('IR3TIB01BEM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'BE'),
                       fredr('IR3TIB01GRM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'GR'),
                       fredr('LVAIR3TIB01STM', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'LV'),
                       fredr('IR3TIB01SKM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'SK'),
                       fredr('IR3TIB01LUM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'LU'),
                       fredr('IR3TIB01EEM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'EE'),
                       fredr('IR3TIB01PTM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'PT'),
                       fredr('LTUIR3TIB01STM', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'LT'),
                       fredr('IRSTCI01CHM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'CH'),
                       fredr('IR3TIB01NLM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'NL'),
                       fredr('IR3TIB01FRM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'FR'),
                       fredr('IR3TIB01DEM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'DE'),
                       fredr('IR3TIB01GBM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'GB'),
                       fredr('IR3TIB01IDM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'ID'),
                       fredr('IR3TIB01SIM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'SI'),
                       fredr('IRSTCI01JPM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'JP'),
                       fredr('IR3TIB01IEM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'IE'),
                       fredr('IR3TIB01CAM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'CA'),
                       fredr('IR3TIB01SEM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'SE'),
                       fredr('IR3TIB01FIM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'FI'),
                       fredr('IR3TIB01ITM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'IT'),
                       fredr('IR3TIB01ESM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'ES'),
                       fredr('FEDFUNDS', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'US'),
                       fredr('IRSTCI01INM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'IN'),
                       fredr('IR3TIB01IDM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'ID'),
                       fredr('IR3TIB01PLM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'PL'),
                       fredr('INTDSRTRM193N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'TR'),
                       fredr('IR3TIB01CZM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'CZ'),
                       fredr('COLIR3TIB01STM', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'CO'),
                       fredr('IRSTCI01MXM156N', frequency = 'm') %>% 
                         drop_na() %>% 
                         select(date, R = value) %>% 
                         mutate(ccode2 = 'MX')
                       
    )
    
    
    data <- data %>% 
      filter(!(ccode2 %in% data2$ccode2)) %>% 
      bind_rows(data2)
    
    data
    
  }
  
  ip <- function(){
    require(tidyverse)
    require(imfr)
    require(lubridate)
    require(stringr)
    require(fredr)
    
    
    
    data <- imfr::imf_dataset('ifs', 
                            indicator = 'AIP_IX',
                            freq = 'M') %>% 
      mutate(value = as.numeric(value)) %>% 
      select(ccode2 = ref_area, date, IP = value) %>% 
      as_tibble() %>% 
      mutate(date = paste(date, '-01', sep = '') %>% as_date())
    
    data
  }
  
  cpi <- function(){
    require(tidyverse)
    require(imfr)
    require(lubridate)
    require(stringr)
    
    data <-   imfr::imf_dataset('ifs', 
                                #ref_area = countries,
                                indicator = 'PCPI_IX',
                                freq = 'M')
    
    
    
    data <- data %>% 
      as_tibble() %>% 
      select(ccode2 = ref_area, date, value) %>% 
      mutate(date = paste(date, '-01', sep = '') %>% as_date()) %>% 
      rename(P = value) %>% 
      mutate(P = as.numeric(P))
    
    data
    
  }
  
  reer <- function(){
    # Load required packages
    require(rvest)
    require(httr)
    require(readxl)
    
    # Step 1: Specify the URL of the webpage
    url <- "https://www.bruegel.org/publications/datasets/real-effective-exchange-rates-for-178-countries-a-new-database"
    
    # Step 2: Scrape the webpage and find the download link
    webpage <- read_html(url)
    
    download_link <- webpage %>%
      html_nodes("a") %>%               # Find all anchor tags
      html_attr("href") %>%             # Extract the 'href' attributes
      .[grepl("\\.zip$", ., ignore.case = TRUE)] # Filter links ending with '.zip'
    
    # Check if a download link was found
    if (length(download_link) == 0) {
      stop("No download link found!")
    }
    
    # Since the link is relative, make it absolute if necessary
    download_link <- ifelse(grepl("^http", download_link), download_link, paste0("https://www.bruegel.org", download_link))
    
    # Step 3: Download the ZIP file
    # Step 3: Create a dedicated temporary directory
    temp_dir <- file.path(getwd(), "bruegel_temp")
    if (!dir.exists(temp_dir)) {
      dir.create(temp_dir)
    }
    zip_file <- file.path(temp_dir, "data.zip")
    download.file(download_link, zip_file, mode = "wb")
    
    # Step 4: Unzip the ZIP file
    unzip_dir <- file.path(temp_dir, "unzipped")
    unzip(zip_file, exdir = unzip_dir)
    
    # Step 5: Find the Excel file in the unzipped content
    xls_files <- list.files(unzip_dir, pattern = "\\.xls$", full.names = TRUE)
    if (length(xls_files) == 0) {
      stop("No Excel file found in the unzipped content!")
    }
    
    # Step 6: Read the specific sheet 'REER_MONTHLY_120' from the Excel file
    sheet_name <- "REER_MONTHLY_120"
    reer_data <- read_excel(xls_files[1], sheet = sheet_name)
    
    
    # Clean up temporary files
    unlink(zip_file)
    unlink(unzip_dir, recursive = TRUE)
    
    names(reer_data)[1] <- "date"
    
    
    reer_data <- reer_data %>% 
      mutate(year = substr(date, 1, 4),
             month = substr(date, 6,7),
             date = paste(year, month, '01', sep = '-') %>% as_date()) %>% 
      select(-year, -month) %>% 
      gather(key, value, -date) %>% 
      mutate(ccode2 = substr(key, nchar(key)-1, nchar(key))) %>% 
      rename(E = value) %>% 
      select(-key) %>% 
      select(ccode2, date, E)
    
    
    unlink(temp_dir, recursive = TRUE)
    
    
    reer_data
    
  }
  
  tot <- function(){
    require(tidyverse)
    require(imfr)
    require(lubridate)
    require(stringr)
    
    pimp <- imfr::imf_dataset('ifs', 
                              indicator = 'PMP_IX',
                              freq = 'Q') %>% 
      mutate(value = as.numeric(value)) %>% 
      select(ccode2 = ref_area, date, pimp = value) %>% 
      as_tibble() %>% 
      mutate(date = paste(date, '-01', sep = '') %>% as_date())
    
    pexp <- imfr::imf_dataset('ifs', 
                              indicator = 'PXP_IX',
                              freq = 'Q') %>% 
      mutate(value = as.numeric(value)) %>% 
      select(ccode2 = ref_area, date, pexp = value) %>% 
      as_tibble() %>% 
      mutate(date = paste(date, '-01', sep = '') %>% as_date())
    
    
    
    data <- pexp %>% 
      inner_join(pimp) %>% 
      mutate(TOT = 100*pexp/pimp) %>% 
      select(ccode2, date, TOT)
    
    data
    
  }
  
  

  
  data_intrates <- intrates()
  data_ip <- ip()
  data_cpi <- cpi()
  data_reer <- reer()
  data_tot <- tot()
  
  
  
  dbnames <- ls()[str_detect(ls(), 'data_')]
  
  data <- get(dbnames[1])
  
  for(name in dbnames[-1]){
    data <- full_join(data, get(name))
  }
  
  
  
  data <- data %>% 
    filter(!is.na(P),
           !is.na(IP),
           !is.na(R), 
           !is.na(E),
           !is.na(TOT))
  
  data
}

library(tidyverse)
library(lubridate)
library(BISdata)
library(BIS)
library(imfr)
library(fredr)
fredr_set_key("cda47ae66b38ed7988c0a9c2ec80c94f")
library(stringr)
library(rebus)


dataQ <- data.query.Q()
dataM <- data.query.M()

gta <- function(){
load_gta <- function(){
# Load required package
library(httr)

# Step 1: Specify the URL of the `.Rdata` file
url <- "https://gtaupload.s3.eu-west-1.amazonaws.com/Uploads/web/GTA+Database+2008-latest.Rdata"

# Step 2: Create a stable temporary directory in the working directory
temp_dir <- file.path(getwd(), "gta_temp")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir, recursive = TRUE)
}

# Step 3: Define the path for the downloaded `.Rdata` file
rdata_file <- file.path(temp_dir, "GTA_Database.Rdata")

# Step 4: Download the `.Rdata` file
download.file(url, rdata_file, mode = "wb")

# Step 5: Load the `.Rdata` file into R
if (file.exists(rdata_file)) {
  # Load the `.Rdata` file and capture the objects loaded
  loaded_objects <- load(rdata_file)
  
  # Print the names of the loaded objects
  cat("Loaded objects:\n")
  print(loaded_objects)
} else {
  stop("The file could not be downloaded or does not exist.")
}

# Step 6: Clean up the temporary directory
unlink(temp_dir, recursive = TRUE)

cat("Temporary files cleaned up successfully.\n")

master
}
master <- load_gta()
clean_gta <- function(){
master <- master %>% 
  as_tibble()

colnames(master) <- c('state_id',
                      'interv_id',
                      'title',
                      'announc_date',
                      'gta_eval',
                      'inforce',
                      'date_in',
                      'date_out',
                      'country_imp',
                      'gov_level',
                      'elig_firm',
                      'type',
                      'mast',
                      'sectors',
                      'products',
                      'country_aff')


#Basic cleanups
master <- master %>% 
  filter(!is.na(country_imp),
         !is.na(gta_eval),
         !is.na(date_in),
         !is.na(type),
         !is.na(sectors),
         !is.na(products),
         year(date_in) <= 2023,
         #Exclude the "Amber" labelled group - we want to be sure that 
         #These dates refer to interventions that are reflecting on 
         #de-liberalization / liberalization measures
         gta_eval %in% c('Red', 'Green'), 
         #Remove unclear measures or any other type that we might not think is 
         #related to geoeconomic fragmentation
         !str_detect(tolower(type), or('unclear',
                                       'customer', #foreign customer limit?
                                       'local',
                                       'state aid',
                                       'financial assistance',
                                       'trade finance',
                                       'trade payment',
                                       'fdi',
                                       'procurement',
                                       'surrender',
                                       'anti-',
                                       'competitive',
                                       'control'
         )),
         !str_detect(tolower(mast), or(#'unclear', 
           'tendering', 
           'migration', 
           #'local', 
           #'internal', 
           'fdi', 
           #'capital control',
           'trade-balancing', #argentina specific outlier for some reason
           'intellectual',
           'l subsidies'
         ))) %>% 
  mutate(country_aff = replace_na(country_aff, 'General'),
         across(c(products, sectors), ~replace_na(.x, '-'))) %>% 
  distinct(date_in, announc_date, country_imp, country_aff, gta_eval, inforce, type, mast, sectors, products, .keep_all = T)




#Assigning the dates to closest months
master <- master %>% 
  #cleaning up the dates of entry into force to the according month
  mutate(year = year(date_in),
         month = month(date_in),
         day = day(date_in),
         month = ifelse(day > 15, month+1, month),
         year = ifelse(month == 13, year +1, year),
         month = ifelse(month == 13, 1, month),
         month = ifelse(nchar(month) == 2, as.character(month), paste('0', month, sep = '')),
         date_in = as_date(paste(year, month, '01', sep = '-'))) %>% 
  #cleaning up the dates of no longer in force to the according month
  mutate(year = year(date_out),
         month = month(date_out),
         day = day(date_out),
         month = ifelse(day > 15, month+1, month),
         year = ifelse(month == 13, year +1, year),
         month = ifelse(month == 13, 1, month),
         month = ifelse(nchar(month) == 2, as.character(month), paste('0', month, sep = '')),
         date_out = as_date(paste(year, month, '01', sep = '-'))) %>% 
  #Clean announcement dates
  mutate(year = year(announc_date),
         month = month(announc_date),
         day = day(announc_date),
         month = ifelse(day > 15, month+1, month),
         year = ifelse(month == 13, year +1, year),
         month = ifelse(month == 13, 1, month),
         month = ifelse(nchar(month) == 2, as.character(month), paste('0', month, sep = '')),
         announc_date = as_date(paste(year, month, '01', sep = '-'))) %>% 
  select(-year, -month, -day)

master <- master %>% 
  select(country_imp, country_aff, date_ann = announc_date, date_in, date_out, 
         gta_eval, type, sectors, products)

master <- master %>% 
  mutate(country_imp = ifelse(country_imp == 'United States of America', 'United States', country_imp),
         country_aff = ifelse(country_aff == 'United States of America', 'United States', country_aff))

master
}
master <- clean_gta()
master
}
GTAdata <- gta()







