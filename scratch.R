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
p_load(httr, jsonlite, dplyr, tidyr, stringr, usethis, DT, ratelimitr, ggplot2, readr)

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
  
  
  #################################
  # Add rate limiter #############
  ###############################
  
  Sys.sleep(3.1)
  
  # Return dataframe
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


######## Rate Limit ######


beaconchain_health <- function() {
  
  url <- paste0("https://beaconcha.in/api/healthz")
  
  print(url)
  
  resp <- GET(url)
  
  # Check if content is JSON
  # if (http_type(resp) != "application/json") {
  #   stop("API did not return json", call. = FALSE)
  # }
  
  return(resp)
  # parsed <- fromJSON(content(resp, "text"), simplifyVector = TRUE)
  # 
  # # Print out errors
  # if (http_error(resp)) {
  #   stop(
  #     sprintf(
  #       "Beaconcha.in API request failed [%s]\n%s\n<%s>", 
  #       status_code(resp),
  #       parsed$message,
  #       parsed$documentation_url
  #     ),
  #     call. = FALSE
  #   )
  # }
  # 
  # # Transform json output to dataframe
  # parsed_transformed <- parsed$data %>% 
  #   as_tibble()
  # 
  # # Define class
  # structure(
  #   list(
  #     content = parsed_transformed,
  #     path = url
  #   ),
  #   class = "beaconchain_api"
  # )
  
  
  #################################
  # Add rate limiter #############
  ###############################
  
  
  # Return dataframe
  return(parsed_transformed)
  
  
}

health <- beaconchain_health()

health$headers$`ratelimit-reset`



content(health, "text")
health$all_headers


# period is measured in seconds
beaconchain_epoch_blocks_lim() <-  ratelimitr::limit_rate(beaconchain_epoch_blocks,
                       rate(n = as.numeric(health$headers$`x-ratelimit-limit-second`), period = 1),
                       rate(n = as.numeric(health$headers$`x-ratelimit-limit-minute`), period = 60),
                       rate(n = as.numeric(health$headers$`x-ratelimit-limit-hour`), period = 3600))


# ........... Incoprorate rate limit reset
health$headers$`ratelimit-reset`  ### that... is not unix time :/



######## Blocks from Multiple Epochs ####

max_epoch = "82300"
min_epoch = "82200"

is.na(as.numeric(max_epoch))

epochs <- seq(as.numeric(min_epoch), as.numeric(max_epoch))


epochs[1]

df <- beaconchain_epoch_blocks(epoch_number = epochs[1])
df

### Add function that gets count of epochs to set rate limit

length(epochs)



###
test <- lapply(epochs, beaconchain_epoch_blocks) %>%
  bind_rows()


test %>%
  summary()

test %>%
  count(graffiti_text) %>%
  arrange(desc(n))

test %>%
  count(proposer) %>%
  arrange(desc(n))



# Validator details -------------------------------------------------------


beaconchain_validator_balance_history <- function(validator = minipool) {
  
  # epoch is defined in the url stucture (www.llama.com/api/epoch/epoch_num) instead of parameter (wwwl.llama.com/api/epoch?epoch_num) for this call
  url <- paste0(base_url,
                "/validator/", 
                validator, 
                "/balancehistory")
  
  print(url)
  
  resp <- GET(url)
  
  # Check if content is JSON
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  Sys.sleep(1)
  parsed <- fromJSON(content(resp, "text"))

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
  # structure(
  #   list(
  #     content = parsed_transformed,
  #     path = url
  #   ),
  #   class = "beaconchain_api"
  # )
}  

test_balance <- beaconchain_validator_balance_history(validator = "260627")


minipools$minipool

# multiple validators
# test <- lapply(epochs, beaconchain_validator_balance_history) %>%
#   bind_rows()

minipool_balanance <- function(minipools = minipools) {
  
  df <- lapply(minipools$minipool, beaconchain_validator_balance_history) %>%
    bind_rows()
  
  return(df)
}


balance_history <- minipool_balanance(minipools = minipools)


balance_history %>%
  summary()



### Save locally ######

# create data directory if does not exist
ifelse(test = dir.exists(file.path(getwd(), "/data")),
       yes = "/data directory exists",
       no = dir.create(file.path(getwd(), "/data"))
       )



# Load local to compare
ifelse(test = file.exists(file.path(getwd(), "/data/minipools.csv")),
       yes = minipools_historical <- read_csv("./data/minipools.csv"),
       no = "minipools.csv does not exist"
)

ifelse(test = file.exists(file.path(getwd(), "/data/balance_history.csv")),
       yes = balance_history_historical <- read_csv("./data/balance_history.csv"),
       no = "balance_history.csv does not exist"
)


balance_history_historical %>%
  summary()
max(balance_history_historical$epoch)

minipools %>%
  filter(minipool %in% minipools_historical$minipool)

minipools$minipool

balance_history %>%
  filter(epoch > max(balance_history_historical$epoch)) 


balance_history <- balance_history %>%
  filter(epoch > max(balance_history_historical$epoch)) %>%
  bind_rows(balance_history_historical)

balance_history %>%
  write_csv(paste0("./data/",
                   "balance_history",
                   ".csv"))

minipools %>%
  write_csv(paste0("./data/",
                   "minipools",
                   ".csv"))



######



balance_history %>%
  mutate(eth_balance = balance / 1000000000) %>% 
  mutate(validatorindex = as.factor(validatorindex)) %>%
  ggplot(aes(x = epoch, y = eth_balance, color = validatorindex)) +
  geom_point() +
  ylim(c(32, max(balance_history$balance/1000000000)))

balance_history %>%
  mutate(eth_balance = balance / 1000000000) %>% 
  left_join(minipools,
            by = c("validatorindex" = "minipool")) %>%
  mutate(total_earnings = eth_balance - 32,
         no_earnings = ((eth_balance - 32)/2)*(commission/100) +((eth_balance - 32)/2)) %>%
  mutate(rocketpool_earnings = total_earnings - no_earnings)  %>%
  pivot_longer(cols = total_earnings:rocketpool_earnings, names_to = 'earning_type', values_to = 'ETH') %>%
  # group_by(epoch, earning_type) %>%
  # summarise(ETH = sum(ETH)) %>%
  ggplot(aes(x = epoch, y = ETH, color = earning_type)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point() +
  facet_wrap(~validatorindex) +
  #ylim(c(0, 0.25)) +
  ggtitle("Accumlated Earnings by a Node Operator")

balance_history %>%
  mutate(eth_balance = balance / 1000000000) %>% 
  left_join(minipools,
            by = c("validatorindex" = "minipool")) %>%
  mutate(total_earnings = eth_balance - 32,
         no_earnings = ((eth_balance - 32)/2)*(commission/100) +((eth_balance - 32)/2)) %>%
  mutate(rocketpool_earnings = total_earnings - no_earnings)  %>%
  pivot_longer(cols = total_earnings:rocketpool_earnings, names_to = 'earning_type', values_to = 'ETH') %>%
  group_by(epoch, earning_type) %>%
  summarise(ETH = sum(ETH)) %>%
  ggplot(aes(x = epoch, y = ETH, color = earning_type)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_point() +
  #facet_wrap(~validatorindex) +
  #ylim(c(0, 0.25)) +
  ggtitle("Accumlated Earnings by a Node Operator")


# Convert to eth ####
#32189263385/1000000000


### Proposer lookup
# lookup validator/proposer details