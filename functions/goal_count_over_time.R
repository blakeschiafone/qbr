goal_count_over_time <- function(namespace, date_begin = FALSE, date_end = FALSE){
  
  
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
  
  
  goals %>%
    filter(table_id == namespace,
           when >= date_begin,
           when <= date_end,
           !goal_name %in% c('start_ios', 'start_android')) %>%
    select(key2, when, count) %>%
    mutate(date = as.Date(stringr::str_c(format(when, '%m'), '01', format(when, '%Y'), sep = '-'), format = '%m-%d-%Y')) %>%
    group_by(key2, date) %>%
    summarize(count = sum(count, na.rm = TRUE)) -> tmp_df_goals
  
  daily_counters %>%
    filter(table_id == namespace,
           date >= date_begin,
           date <= date_end) %>%
    select(key, campaign_type) %>%
    mutate(campaign_type = recode(campaign_type,
                                  'adaptive' = 'Adaptive',
                                  'auto' = 'Lifecycle',
                                  'one_time' = 'One Time',
                                  'trigger' = 'Conversion',
                                  'immediate_trigger' = 'Conversion',
                                  'program_message' = 'Experience')) %>%
    group_by(key, campaign_type) %>%
    unique() -> tmp_df_counters
  
  tmp_df_goals %>%
    left_join(tmp_df_counters, by = c("key2" = "key")) %>%
    select(-key2) %>%
    group_by(date, campaign_type) %>%
    summarize(count = sum(count)) %>%
    #mutate(campaign_type = forcats::fct_reorder(campaign_type, count)) %>%
    #arrange(desc(count)) %>%
    #mutate(rn = row_number()) %>%
    #        goal_name2 = ifelse(rn %in% c(1:5), goal_name, 'other'),
    #        rn_color = ifelse(rn == 1, '1', 
    #                          ifelse(rn == 2, '2',
    #                                 ifelse(rn == 3, '3',
    #                                        ifelse(rn == 4, '4',
    #                                               ifelse(rn == 5, '5', '6')))))) %>%
    ggplot(., aes(x = date, y = count, group = forcats::fct_reorder(campaign_type, count))) +
    geom_bar(aes(fill = campaign_type), stat = 'identity') +
    labs(x = '',
         y = '') +
    #geom_col(position = position_stack(reverse = FALSE), aes(fill = campaign_type)) +
    #geom_text(position = position_stack(vjust = .5, reverse = TRUE), size = 2, aes(label = campaign_type)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    scale_y_continuous(labels = scales::comma, breaks = pretty_breaks()) + 
    scale_fill_manual(values =  c("Adaptive" = "#28235e",
                                  "Lifecycle" = "#ae0a45ff",
                                  "One Time" = "#2a5191",
                                  "Conversion" = "#27ab7eff",
                                  "Experience" = "#66308dff"), na.value = "#999999ff") +
    theme_bw() +
    theme(axis.text.x = element_text(),
          plot.title = element_text(hjust = 0.5, size = 11),
          axis.title.x = element_text(size = 9),
          axis.title.y = element_text(size = 9),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          rect = element_blank(),
          line = element_blank(),
          legend.position = 'bottom',
          legend.direction = 'horizontal',
          legend.title = element_blank())
}
