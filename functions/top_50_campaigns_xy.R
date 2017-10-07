top_50_campaigns_xy <- function(namespace, date_begin = FALSE, date_end = FALSE){
  
  
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
           name %in% c('delivered', 'ia_delivered', 'engaged', 'goal'),
           channel %in% c('push')) %>% 
    select(key, name, total, algo_type, campaign_type) %>%
    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name),
           campaign_type = case_when(
             .$campaign_type == 'auto' ~ 'Lifecycle',
             .$campaign_type == 'one_time' ~ 'One Time',
             .$campaign_type == 'program_message' ~ 'Experiences',
             .$campaign_type %in% c('trigger', 'immediate_trigger') ~ 'Conversion',
             .$campaign_type == 'adaptive' ~ 'Adaptive')) %>%
    left_join(daily_campaign, by = c("key" = "key")) %>%
    select(display_name, name, campaign_type, algo_type, total) %>%
    group_by(display_name, name, campaign_type, algo_type) %>%
    summarize(total = sum(total)) %>%
    tidyr::spread(name, total) %>%
    mutate(engagement_rate = engaged / delivered,
           conversion_rate = goal / delivered,
           ranked_rate = (goal + engaged) / delivered) %>%
    filter(delivered > 1000, engagement_rate < 1, conversion_rate < 1) %>%
    arrange(desc(ranked_rate)) %>%
    ungroup() %>%
    top_n(50) %>%
    mutate(total_goal = sum(goal) / sum(delivered),
           total_engagement = sum(engaged) / sum(delivered)) %>%
    
    ggplot(., aes(x = conversion_rate, 
                  y = engagement_rate, 
                  color = campaign_type, 
                  size = delivered)) + 
    geom_point(alpha = .7) +
    scale_x_continuous(labels = scales::percent,
                       limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1)) +
    scale_color_manual(values = c('Conversion' = '#ae0a45ff', 
                                  'Lifecycle' = '#66308dff', 
                                  'Adaptive' = '#27ab7eff', 
                                  'One Time' = '#ff942eff', 
                                  'Experiences' = '#3c78d8ff')) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      rect = element_blank(),
      line = element_blank(),
      legend.position = c(.8, .02),
      legend.title = element_blank(),
      legend.direction = 'horizontal'
    ) +
    guides(size = FALSE) +
    labs(
      x = '',
      y = ''
    ) +
    geom_hline(aes(yintercept = total_engagement), color = 'grey', linetype = 'dashed') +
    geom_vline(aes(xintercept = total_goal), color = 'grey', linetype = 'dashed') + 
    geom_text(data = data.frame(x = 1, y = 1, label = 'high conversion,\nhigh engagement'), aes(x = x, y = y, label = label), size = 3, color = 'black') +  #hconv, heng
    geom_text(data = data.frame(x = 1, y = .01, label = 'high conversion,\nlow engagement'), aes(x = x, y = y, label = label), size = 3, color = 'black') +  #hconv, leng
    geom_text(data = data.frame(x = .01, y = .01, label = 'low conversion,\nlow engagement'), aes(x = x, y = y, label = label), size = 3, color = 'black') + #lconv, leng
    geom_text(data = data.frame(x = .01, y = 1, label = 'low conversion,\nhigh engagement'), aes(x = x, y = y, label = label), size = 3, color = 'black')   #lconv, heng
}
