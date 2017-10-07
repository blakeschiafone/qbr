compare_namespace_campaigns <- function(namespace1, namespace2, date_begin = FALSE, date_end = FALSE){
  
  
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
    filter(table_id %in% c(namespace1, namespace2), 
           label == 'non-control', 
           date >= date_begin, 
           date <= date_end, 
           name %in% c('delivered', 'ia_delivered', 'engaged', 'goal'),
           channel %in% c('push', 'email')) %>% 
    select(table_id, key, name, total) %>%
    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name)) %>%
    left_join(daily_campaign, by = c("key" = "key")) %>%
    select(table_id, display_name, name, total) %>%
    group_by(table_id, display_name, name) %>%
    summarize(total = sum(total)) %>%
    tidyr::spread(name, total) %>%
    mutate(engagement_rate = engaged / delivered,
           conversion_rate = goal / delivered,
           ranked_rate = (goal + engaged) / delivered) %>%
    filter(delivered > 1000, engagement_rate < 1, conversion_rate < 1) %>%
    arrange(desc(ranked_rate)) %>%
    group_by(table_id) %>%
    slice(1:50) %>%
    ungroup() %>%

    
    ggplot(., aes(x = conversion_rate, 
                  y = engagement_rate, 
                  color = table_id)) + 
    geom_point(alpha = .7) +
    scale_x_continuous(labels = scales::percent,
                       limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1)) +
    # scale_color_manual(values = c('Conversion' = '#ae0a45ff', 
    #                               'Lifecycle' = '#66308dff', 
    #                               'Adaptive' = '#27ab7eff', 
    #                               'One Time' = '#ff942eff', 
    #                               'Experiences' = '#3c78d8ff')) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      rect = element_blank(),
      line = element_blank(),
      legend.position = c(.8, .02),
      legend.title = element_blank(),
      legend.direction = 'horizontal',
      plot.title = element_text(size = 12, hjust = .5)
    ) +
    guides(size = FALSE) +
    labs(
      x = '\nConversion Rate',
      y = 'Engagement Rate\n',
      title = 'Comparing Top 50 Campaigns by Customer\n'
    ) 
    # geom_hline(aes(yintercept = total_engagement), color = 'grey', linetype = 'dashed') +
    # geom_vline(aes(xintercept = total_goal), color = 'grey', linetype = 'dashed') + 
    # geom_text(data = data.frame(x = 1, y = 1, label = 'high conversion,\nhigh engagement'), aes(x = x, y = y, label = label), size = 3, color = 'black') +  #hconv, heng
    # geom_text(data = data.frame(x = 1, y = .01, label = 'high conversion,\nlow engagement'), aes(x = x, y = y, label = label), size = 3, color = 'black') +  #hconv, leng
    # geom_text(data = data.frame(x = .01, y = .01, label = 'low conversion,\nlow engagement'), aes(x = x, y = y, label = label), size = 3, color = 'black') + #lconv, leng
    # geom_text(data = data.frame(x = .01, y = 1, label = 'low conversion,\nhigh engagement'), aes(x = x, y = y, label = label), size = 3, color = 'black')   #lconv, heng
}
