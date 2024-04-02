library(dplyr)

files <- paste0("Data/", list.files("Data/", pattern = "*.csv"))
data_by_invoice <- map(files,
                       ~fread(.x) %>% 
                         as_tibble()) %>% 
  reduce(bind_rows)

data_unified_names <- data_by_invoice %>% 
  mutate(yearmonth = yearmonth(as.Date(InvoiceDate)),
         product = str_sub(StockCode, 1, 5))

description_names <- data_unified_names %>% 
  filter(!str_detect(product, "^[[:alpha:]]")) %>% 
  select(Description, product, yearmonth) %>% 
  arrange(product, yearmonth) %>% 
  group_by(product) %>% 
  distinct(Description, product) %>%
  filter(!str_detect(Description, "[:lower:]") & 
           str_detect(Description, "[:upper:]") &
           !str_detect(Description, "mazon") &
           !str_detect(Description, "djustment")) %>% 
  mutate(product = as.integer(product))

unique_names <- description_names %>% 
  distinct(product, .keep_all = TRUE)

diff_names <- anti_join(description_names, unique_names)

clean_names <- map(unique_names$product,
                   ~intersect(
                     strsplit(unique_names %>%
                                as_tibble() %>% 
                                filter(product == .x) %>%
                                pull(Description),
                              split = " ")[[1]],
                     str_replace_all(strsplit(diff_names %>% 
                                                as_tibble() %>% 
                                                filter(product == .x) %>% 
                                                select(Description) %>% 
                                                as.character(),
                                              split = " ")[[1]],
                                     "[^[A-Z]]", "")) %>% 
                     paste(collapse = " ") %>% 
                     as_tibble()) %>% 
  reduce(rbind) %>% 
  cbind(unique_names) %>% 
  mutate(product_name = ifelse(value != "", value, Description)) %>% 
  select(product, product_name) %>% 
  as_tibble()

data_monthly <- data_unified_names %>% 
  filter(Quantity > 0,
         Price > 0,
         !str_detect(product, "^[[:alpha:]]")) %>% 
  group_by(yearmonth, product) %>% 

  summarise(quantity_sum = sum(Quantity, na.rm = TRUE),
            price_mean = weighted.mean(Price, Quantity, na.rm = TRUE),
            revenue = sum(Price * Quantity),
            n_receipts = n()) %>% 
  group_by(product) %>% 
  mutate(n_months = n()) %>% 
  ungroup() %>% 
  filter(n_months >= 24) %>% 
  arrange(product, yearmonth)

data_to_arima <- data_monthly %>% 
  group_by(product) %>% 
  do(efp = efp(.$price_mean ~ 1,
               type = "Rec-CUSUM")) %>% 
  mutate(p_value = sctest(efp)$p) %>% 
  filter(p_value > p_value_threshold) %>%
  select(product) %>% 
  inner_join(data_monthly) %>%
  as_tsibble(key = "product", index = "yearmonth") %>% 
  fill_gaps()

product_scope <- data_to_arima %>% 
  distinct(product) %>% 
  pull()  

rfm_result <- data_unified_names %>% 
  filter(product %in% product_scope) %>% 
  mutate(date = as.Date(InvoiceDate),
         revenue = Quantity * Price) %>% 
  rfm_table_order(product,
                  date,
                  revenue,
                  max(as.Date(.$InvoiceDate)))

segment_names <- c("Champions", "Good", "Average",
                   "New", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")

segments <- create_segments(rfm_result)

data_to_arima <- data_to_arima %>%
  left_join(segments %>% 
              select(customer_id, segment),
            by = c("product" = "customer_id")) %>% 
  left_join(clean_names %>% 
              mutate(product = as.character(product)),
            by = c("product" = "product"))

plan(multisession)

tic <- Sys.time()
models <- data_to_arima %>% 
  model(ARIMA(quantity_sum ~ price_mean))
(toc <- Sys.time() - tic)

saveRDS(models, "Data/models.RDS")
saveRDS(segments, "Data/segments.RDS")
saveRDS(rfm_result, "Data/rfm_results.RDS")
saveRDS(data_to_arima, "Data/data_to_arima.RDS")
