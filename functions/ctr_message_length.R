ctr_message_length <- function(namespace, channels = 'all', channel_facet = FALSE, month_facet = FALSE, date_begin = FALSE, date_end = FALSE){
  library(stringi)
  
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

  if(channels == 'all'){channels <- c('push', 'email', 'in_app')}
  
  if(month_facet & channel_facet == FALSE){
    exclude_var <- ''
  }

  
  daily_counters %>%
    filter(table_id == namespace,
           date >= date_begin,
           date <= date_end,
           name %in% c('delivered', 'ia_delivered', 'clicks'),
           channel %in% channels,
           label == 'non-control') %>%
    select(key, campaign_type, algo_type, name, date, total) %>%

    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name),
           campaign_type = case_when(
             .$campaign_type == 'auto' ~ 'Lifecycle',
             .$campaign_type == 'one_time' ~ 'One Time',
             .$campaign_type == 'program_message' ~ 'Experiences',
             .$campaign_type %in% c('trigger', 'immediate_trigger') ~ 'Conversion',
             .$campaign_type == 'adaptive' ~ 'Adaptive'),
           algo_type = case_when(
             .$algo_type == 'miq/tiq' ~ 'Message + Time Optimization',
             .$algo_type == 'none' ~ 'No Optimization',
             .$algo_type == 'so' ~ 'SendOptimally',
             .$algo_type == 'tiq' ~ 'Time Optimization',
             .$algo_type == 'miq/so' ~ 'Message + SendOptimally')) %>%
           #date = factor(format(date, '%b %Y'), levels = c('Jan 2016', 'Feb 2016', 'Mar 2016', 'Apr 2016', 'Jun 2016', 'Jul 2016', 'Aug 2016', 'Sep 2016', 'Oct 2016', 'Nov 2016', 'Dec 2016', 'Jan 2017'))) %>%
    left_join(daily_campaign, by = c("key" = "key")) %>%
    select(message_text, campaign_type, algo_type, name, total) %>%
    filter(message_text != '<none>', !(is.na(message_text))) %>%
    mutate(message_length = stri_count(message_text, regex="\\S+")) %>%
    group_by(message_length, name) %>%
    summarize(total = sum(total)) %>%
    tidyr::spread(name, total) %>%
    na.omit(.) %>%
    mutate(ctr = clicks / delivered) %>%
    filter(ctr < 1) %>%

    ggplot(., aes(x = message_length, y = ctr, size = delivered)) +
    geom_point() +
    geom_smooth(alpha = .5, fill = '#d8d8d8', color = '#66308dff') +
    #facet_wrap(~ date, scales = 'free_x') +
    #scale_color_manual(values = c('Message + Time Optimization' = '#ae0a45ff',
                                  # 'No Optimization' = '#999999ff',
                                  # 'SendOptimally' = '#66308dff',
                                  # 'Time Optimization' = '#27ab7eff',
                                  # 'Message + SendOptimally' = '#2a5191')) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,NA), oob = squish) +
    # expand_limits(y = 0) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 11),
          axis.title.x = element_text(size = 9),
          axis.title.y = element_text(size = 9),
          rect = element_blank(),
          line = element_blank(),
          legend.position = 'none') +
    labs(x = '\nLength of Message Text',
         y = 'Click-Thru Rate (%)\n',
         title = 'Message Length and Corresponding CTR\n\n')
}
