library(httr)
library(rvest)

####Opgave 2.4 - Scrape & SQL med Miljødata ####

#Headers
headers <- add_headers(
  `accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
  `accept-encoding` = "gzip, deflate, br, zstd",
  `accept-language` = "da-DK,da;q=0.9,en-US;q=0.8,en;q=0.7",
  `cache-control` = "max-age=0",
  `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36"
)

#Liste med URL'er til de forskellige sider
urls <- list(
  "HCAB" = "https://envs2.au.dk/Luftdata/Presentation/table/Copenhagen/HCAB",
  "ANHO" = "https://envs2.au.dk/Luftdata/Presentation/table/Rural/ANHO",
  "AARH3" = "https://envs2.au.dk/Luftdata/Presentation/table/Aarhus/AARH3",
  "RISOE" = "https://envs2.au.dk/Luftdata/Presentation/table/Rural/RISOE"
)

#Funktion til at hente data fra en URL
url="https://envs2.au.dk/Luftdata/Presentation/table/Copenhagen/HCAB"
# Send GET-anmodning for at hente CSRF-token

fetch_data <- function(url) {
  get_response <- GET(url, headers)
  
  # Kontroller om GET-anmodningen var succesfuld
  print(get_response$status_code)
  page_content <- content(get_response, as = "text")
  #writeLines(page_content,"out.txt")
  page <- read_html(page_content)
  # Udtræk CSRF-token
  csrf_token <- page %>%
    html_element("input[name='__RequestVerificationToken']") %>%
    html_attr("value")
  
  # Fejlhåndtering hvis CSRF-token ikke er udtrukket
  if (is.na(csrf_token)) {
    stop("Failed to extract CSRF-token for URL: ", url)
  } else {
    print(paste("CSRF-token extracted for", url, ":", csrf_token))
  }
  
  # Brug URL-encode til at sikre korrekt formatering af tokenet
  encoded_csrf_token <- URLencode(csrf_token)
  
  # Forbered POST-anmodningen (payload)
  payload <- list(
    `__RequestVerificationToken` = encoded_csrf_token  # Den korrekte CSRF-token
  )
  
  # Cookies 
  cookies <- set_cookies(
    CookieScriptConsent = '{"bannershown":1,"action":"accept","consenttime":1718103120,"categories":"[\"targeting\",\"functionality\",\"performance\",\"unclassified\"]","key":"c414c3ce-0f5d-45f7-9258-0e3eb062b385"}',
    `__RequestVerificationToken_L0x1ZnRkYXRhL1ByZXNlbnRhdGlvbg2` = "vGZmXU8znQRoPge8zmnonE-UF08FjDOMu2TY_sQ3Zvdz98-8n2j2yrCvva-pnPoBm262cJMWEq85s9eEuvObNwJM5SGksjnmZOfY8Yo8tFE1"
  )
  
  # Send POST-anmodning
  post_url <- paste0("https://envs2.au.dk/Luftdata/Presentation/table/MainTable/", gsub(".*table/", "", url))
  post_response <- POST(
    url = post_url,
    body = payload,
    encode = "form",
    headers = headers,
    cookies = cookies
  )
  
  # Hent data efter POST-anmodning
  if (post_response$status_code == 200) {
    print(paste("POST request successful for", url))
    
    # Parse HTML-indholdet fra POST-anmodningen
    post_page <- read_html(content(post_response, as = "text"))
    
    # Ekstrahér tabellen
    table <- post_page %>%
      html_element("table.table-bordered") %>%
      html_table(header = TRUE)
    
  } else {
    stop("Failed to fetch data with POST request for URL:", url, "Status code: ", post_response$status_code)
  }
}

#Hent data fra alle URL'er og gem i separate dataframes
data_hcab <- fetch_data(urls$HCAB)
data_anho <- fetch_data(urls$ANHO)
data_aarh3 <- fetch_data(urls$AARH3)
data_risoe <- fetch_data(urls$RISOE)

