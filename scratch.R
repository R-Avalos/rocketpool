### Scratch ### 

# https://beaconcha.in/api/v1/docs/index.html
# https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html

# Calls are rate limited to 10 requests / 1 minute / IP
# All API results are cached for 1 minute

# via: 1.1 google, 1.1 google 
# www-authenticate: Key realm="kong" 
# x-ratelimit-limit-day: 10000 
# x-ratelimit-limit-hour: 10000 
# x-ratelimit-limit-minute: 20 
# x-ratelimit-limit-month: 80000 
# x-ratelimit-limit-second: 20 
# x-ratelimit-remaining-day: 9998 
# x-ratelimit-remaining-hour: 9998 
# x-ratelimit-remaining-minute: 19 
# x-ratelimit-remaining-month: 79998 
# x-ratelimit-remaining-second: 19


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(httr, jsonlite, dplyr, tidyr, stringr)

#Rmpfr  ### large hex
# goal, setup api access framework to beaconchain
# https://beaconcha.in/api/v1/epoch/82289/blocks   epoch 82289 returns the slots/blocks within it... much more data

base_url <- "https://beaconcha.in/api/v1"

paste0(base_url,
       "/api/v1/epoch/", 
       str_trim("82289"), 
       "/blocks")

beaconcha_in_epoch_blocks <- function(epoch_number = "82289") {
  
  url <- paste0(base_url,
                "/epoch/", 
                str_trim(epoch_number), 
                "/blocks")
  
  print(url)
  
  resp <- GET(url)
  
  # Check if content is JSON
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  
  parsed <- fromJSON(content(resp, "text"), simplifyVector = TRUE)
  
  # Print out errors
  if (http_error(resp)) {
    stop(
      sprintf(
        "Beaconcha.in API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  
  parsed_transformed <- parsed$data %>% 
    as_tibble()
  
  structure(
    list(
      content = parsed_transformed,
      path = url,
      response = resp
    ),
    class = "beaconcha.in_api"
  )
  
}

print.beaconcha_in_api <- function(x, ...) {
  cat("<Beaconcha_in ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

beaconcha_in_epoch_blocks()


