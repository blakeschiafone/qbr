top_10_messages_engagement <- function(namespace, date_begin = FALSE, date_end = FALSE){
  
  
  if(date_begin == FALSE){
    date_begin <- lubridate::floor_date(seq(from = Sys.Date(), by = '-1 year', length.out = 2)[2], unit = 'year')
  } else {
    date_begin <- as.Date(date_begin)
  }
  if(date_end == FALSE){
    date_end <- lubridate::ceiling_date(seq(from = Sys.Date(), by = '-1 month', length.out = 2)[1], unit = 'month') - 1
  } else {
    date_end <- as.Date(date_end)
  }
  
  
  daily_counters %>% 
    filter(table_id == namespace, 
           label == 'non-control', 
           date >= date_begin, 
           date <= date_end, 
           name %in% c('delivered', 'ia_delivered', 'engaged'),
           channel %in% c('push', 'email')) %>% 
    select(key, channel, name, total, algo_type) %>%
    mutate(algo_type = recode(algo_type,
                              'tiq' = 'Time Optimization',
                              'so' = 'Send Optimally',
                              'miq' = 'Message Optimization',
                              'miq/tiq' = 'Message Optimization + Time Optimization',
                              'none' = 'No Optimization',
                              'miq/so' = 'Message Optimization + Send Optimally')) %>%
    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name)) %>%
    left_join(daily_campaign, by = c("key" = "key")) %>%
    select(name, algo_type, total, message_text) %>%
    group_by(name, algo_type, message_text) %>%
    summarize(total = sum(total)) %>%
    tidyr::spread(name, total) %>%
    mutate(engagement_rate = engaged / delivered) %>%
    filter(delivered > 1000, 
           message_text != '<none>') %>%
    arrange(desc(engagement_rate)) %>%
    ungroup() %>%
    slice(1:10) %>%
    kable(.)
}

