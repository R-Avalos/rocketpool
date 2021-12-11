### Scratch ### 

# local get sync committeee
# curl -X GET "http://10.1.1.30:5052/eth/v1/beacon/states/head/sync_committees?epoch=84480" -H  "accept: application/json" | jq '.data.validators'
# uncomment ports in rocketpool/docker-compose.yml

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
p_load(httr, jsonlite, dplyr, tidyr, stringr, usethis, DT)

#Rmpfr  ### large hex
# goal, setup api access framework to beaconchain
# https://beaconcha.in/api/v1/epoch/82289/blocks   epoch 82289 returns the slots/blocks within it... much more data

base_url <- "https://beaconcha.in/api/v1"



beaconchain_epoch_blocks <- function(epoch_number = "82289") {
  
  # epoch is defined in the url stucture (www.llama.com/api/epoch/epoch_num) instead of parameter (wwwl.llama.com/api/epoch?epoch_num) for this call
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
  
  # Transform json output to dataframe
  parsed_transformed <- parsed$data %>% 
    as_tibble()
  
  # Define class
  structure(
    list(
      content = parsed_transformed,
      path = url
    ),
    class = "beaconchain_api"
  )
  
  return(parsed_transformed)
  
  
}

# Print out for class
print.beaconcha_in_api <- function(x, ...) {
  cat("<Beaconchain ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}


test2 <- beaconchain_epoch_blocks()
test2

######## Blocks from Multiple Epochs ####

max_epoch = "82280"
min_epoch = "82275"

is.na(as.numeric(max_epoch))

epochs <- seq(as.numeric(min_epoch), as.numeric(max_epoch))


epochs[1]

df <- beaconchain_epoch_blocks(epoch_number = epochs[1])
df


test <- lapply(epochs, beaconchain_epoch_blocks) %>%
  bind_rows()
