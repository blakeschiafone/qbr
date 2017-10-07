compare_app_opens <- function(namespace, date_begin = FALSE, date_end = FALSE){
  
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
  
  
  
  query <- paste0('select
  month,
  year,
  (opens_push_enabled / users_push_enabled) as ratio_push_enabled,
  (opens_push_disabled / users_push_disabled) as ratio_push_disabled,
  (opens_push_sent / users_push_sent) as ratio_push_sent,
  (opens_no_push_sent / users_no_push_sent) as ratio_no_push_sent
  from
  (SELECT
  month(first_visit) as month,
  year(first_visit) as year,
  sum(if(push_enabled is true, start_ios, 0)) as opens_push_enabled,
  exact_count_distinct(case when push_enabled is true then id end) as users_push_enabled,
  
  sum(if(last_push_sent is not null, start_ios, 0)) as opens_push_sent,
  exact_count_distinct(case when last_push_sent is not null then id end) as users_push_sent,
  
  sum(if(push_enabled is false, start_ios, 0)) as opens_push_disabled,
  exact_count_distinct(case when push_enabled is false then id end) as users_push_disabled,
  
  sum(if(last_push_sent is null, start_ios, 0)) as opens_no_push_sent,
  exact_count_distinct(case when last_push_sent is null then id end) as users_no_push_sent
  FROM
    [tap-nexus:kahuna_users.', namespace, ']
  where not username contains "historical" and not last_dev_id contains "historical"
  group by month, year)
  order by year desc, month desc')
  
  results <- query_exec(project = 'kahuna-bq-access', query = query)
  
  
  
  results %>%
    mutate(date = as.Date(stringr::str_c(month, '01', year, sep = '-'), format = '%m-%d-%Y')) %>%
    filter(!is.na(ratio_push_sent), !is.na(ratio_no_push_sent)) %>%
    tidyr::gather(type, value, ratio_push_enabled:ratio_no_push_sent) %>%
    filter(type %in% c('ratio_push_sent', 'ratio_no_push_sent'),
           date >= date_begin,
           date <= date_end) %>%
    select(date, type, value) %>%
    arrange(desc(date)) %>%
    slice(1:(n() -2)) %>%
    ggplot(., aes(x = date, y = value, color = type)) +
    geom_line(size = 1.5) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
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
    scale_color_manual(values = c("ratio_push_sent" = "#66308dff",
                                  "ratio_no_push_sent" = "#999999ff")) +
    labs(x = '\nUser Acquisition Date',
         y = '\nAvg App Opens (per User)',
         title = '\nComparing App Opens',
         subtitle = 'User Messaged vs. Not Messaged\n\n')
}
