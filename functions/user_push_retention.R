user_push_retention <- function(namespace, return_date = FALSE, date_begin = FALSE, date_end = FALSE){
  
  
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
  
  
  #' check for missing return_date
  #' default for 1st day of previous month
  if(return_date == FALSE){
    return_date <- format(seq(from = Sys.Date(), by = '-1 month', length.out = 2)[2], '%Y-%m-01')
  }
  
  
  #date_end <- ifelse(return_date < date_end, return_date, date_end)
  #curr_month_begin <- format(seq(from = Sys.Date(), by = '-1 month', length.out = 2)[2], format = '%Y-%m-01')
  
  
  
  query <- paste0("select
  user_group,
  push_enabled_retention / push_enabled_count as push_enabled_retention,
  push_disabled_retention / push_disabled_count as push_disabled_retention
  from
  (SELECT
  concat(string(month(created)), '-01-', string(year(created))) as user_group,
    EXACT_COUNT_DISTINCT(CASE
        WHEN push_enabled IS TRUE AND last_push_sent is not null AND last_visit >= timestamp('",return_date,"') AND
        last_visit > first_visit 
        THEN id END) AS push_enabled_retention, 
        
        EXACT_COUNT_DISTINCT(CASE
        WHEN push_enabled IS TRUE AND last_push_sent is not null THEN id END) AS push_enabled_count,
        
        
     EXACT_COUNT_DISTINCT(CASE
        WHEN push_enabled IS FALSE AND last_push_sent is null AND last_visit >= timestamp('",return_date,"') AND
        last_visit > first_visit 
        THEN id END) AS push_disabled_retention, 
        
        EXACT_COUNT_DISTINCT(CASE
        WHEN push_enabled IS FALSE AND last_push_sent is null THEN id END) AS push_disabled_count
  
    FROM
    [tap-nexus:kahuna_users.", namespace, "]
    group by user_group)")
  
  results <- query_exec(project = 'kahuna-bq-access', query = query)
  
  
  results$user_group <- as.Date(results$user_group, format = '%m-%d-%Y')
  results <- results[order(results$user_group),]
  results <- na.omit(results)
  results <- results %>% tidyr::gather(type, value, push_enabled_retention:push_disabled_retention)
  results <- results %>% filter(as.character(user_group) != format(Sys.Date(), '%Y-%m-01'))
  results <- results %>% filter(user_group >= date_begin,
                                user_group <= date_end)
  
  ggplot(results, aes(x = user_group, y = value, color = type, label = scales::percent(round(value, 2)))) + 
    geom_line(stat = 'identity', size = 1.5) +
    scale_color_manual(values = c('push_enabled_retention' = '#66308dff', 
                                  'push_disabled_retention' = '#999999ff')) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = c(0, .25, .50, .75, 1)) +
    scale_x_date(date_labels = '%b %Y', date_breaks = '1 month') +
    geom_hline(aes(yintercept = .5), color='#e5e5e5', linetype = 'dashed') +
    theme_bw() +
    theme(axis.text.x = element_text(),
          plot.title = element_text(hjust = 0.5, size = 11),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.x = element_text(size = 9),
          axis.title.y = element_text(size = 9),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          rect = element_blank(),
          line = element_blank(),
          legend.position = 'none') +
    geom_text(size = 3, vjust = -1) +
    labs(x = '\nUser Acquisition Date\n',
         y = '\n% of Users Returning\n',
         title = paste0('\n% of Users Returning in ', format(as.Date(return_date), format = '%B %Y')),
         subtitle = 'User Messaged vs. Not Messaged\n\n\n')
}
