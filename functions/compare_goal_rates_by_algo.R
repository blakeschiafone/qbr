compare_goal_rates_by_algo <- function(namespace, date_begin = FALSE, date_end = FALSE){
  
  
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
  
  
  #' Temp table for counters
  daily_counters %>% 
    filter(table_id == namespace, 
           date >= date_begin, 
           date <= date_end, 
           name %in% c('delivered', 'ia_delivered'),
           channel %in% c('push', 'email', 'in_app'),
           label == 'non-control') %>% 
    select(key, campaign_type, algo_type, name, total) %>%
    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name),
           campaign_type = case_when(
             .$campaign_type == 'auto' ~ 'Lifecycle',
             .$campaign_type == 'one_time' ~ 'One Time',
             .$campaign_type == 'program_message' ~ 'Experiences',
             .$campaign_type %in% c('trigger', 'immediate_trigger') ~ 'Conversion',
             .$campaign_type == 'adaptive' ~ 'Adaptive'),
           algo_type = case_when(
             .$algo_type == 'miq/tiq' ~ 'Message Optimization + Time Optimization',
             .$algo_type == 'none' ~ 'No Optimization',
             .$algo_type == 'so' ~ 'Send Optimally',
             .$algo_type == 'tiq' ~ 'Time Optimization',
             .$algo_type == 'miq/so' ~ 'Message Optimization + Send Optimally',
             .$algo_type == 'miq' ~ 'Message Optimization')) %>%
    group_by(key, campaign_type, algo_type, name) %>%
    summarize(total = sum(total, na.rm = TRUE)) %>%
    tidyr::spread(name, total) -> tmp_df_counters
    
  
  #' Temp table for goals
  goals %>%
    filter(table_id == namespace, 
           when >= date_begin, 
           when <= date_end,
           !(goal_name %in% c('start_ios', 'start_android'))) %>% 
    select(key2, count) %>%
    group_by(key2) %>%
    summarize(goal = sum(count, na.rm = TRUE)) %>%
    select(key2, goal) -> tmp_df_goals
  
  
  #' Group temp tables
  left_join(tmp_df_counters, tmp_df_goals, by = c("key" = "key2")) -> tmp_df_joined
  
  
  #' Continue operating with newly joined table
  tmp_df_joined %>%
    ungroup() %>%
    tidyr::gather(name, total, delivered:goal) %>%
    group_by(campaign_type, algo_type, name) %>%
    summarize(total = sum(total, na.rm = TRUE)) %>%
    tidyr::spread(name, total) %>%
    mutate(goal_rate = goal / delivered) %>%
    filter(!is.na(delivered), !is.na(goal)) %>%
    mutate(delivered_percentage = delivered / sum(.$delivered),
           ymax = max(.$goal_rate)  + .05) %>%
    ggplot(., aes(x = algo_type, y = goal_rate, fill = algo_type)) + 
    geom_bar(stat = 'identity') + 
    scale_fill_manual(values = c(  'Message Optimization + Time Optimization' = '#ae0a45ff', 
                                   'No Optimization' = '#999999ff', 
                                   'Send Optimally' = '#66308dff', 
                                   'Time Optimization' = '#27ab7eff',
                                   'Message Optimization + Send Optimally' = '#2a5191',
                                   'Message Optimization' = '#ff942eff')) +
    facet_grid( . ~ campaign_type, scales = 'free_x') +
    geom_text(aes(x = algo_type, 
                  y = goal_rate, 
                  label = ifelse(delivered_percentage < .01, 
                                 '<1%', 
                                 scales::percent(round(delivered_percentage, 2)))), 
              size = 3, vjust = -1) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = '',
         y = 'Goal Completion Rate\n') +
    theme_bw() +
    theme(axis.text.x = element_text(vjust = -.1, hjust = 1, angle = 90),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          strip.background = element_rect(fill = NA, color = NA),
          panel.border = element_rect(color = 'grey', size = .5),
          legend.position = 'none')
}
