compare_algo_type_vs_control <- function(namespace, date_begin = FALSE, date_end = FALSE){
  
  
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
           date >= date_begin, 
           date <= date_end, 
           name %in% c('delivered', 'ia_delivered', 'goal'),
           channel %in% c('push', 'email', 'in_app')) %>% 
    select(date, key, label, name, total, algo_type, campaign_type) %>%
    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name),
           campaign_type = ifelse(campaign_type %in% c('trigger', 'immediate_trigger'), 'Conversion Campaigns', 'Non-Conversion Campaigns'),
           algo_type = case_when(
             .$algo_type == 'miq/tiq' ~ 'Message Optimization + Time Optimization',
             .$algo_type == 'none' ~ 'No Optimization',
             .$algo_type == 'so' ~ 'Send Optimally',
             .$algo_type == 'tiq' ~ 'Time Optimization',
             .$algo_type == 'miq/so' ~ 'Message Optimization + Send Optimally',
             .$algo_type == 'miq' ~ 'Message Optimization')) %>%
    left_join(daily_campaign, by = c("key" = "key")) %>%
    select(-key) %>%
    group_by(display_name, label, name, algo_type, campaign_type) %>%
    summarize(total = sum(total)) %>%
    tidyr::spread(label, total) %>%
    filter(!is.na(control)) -> tmp_df
  #assign('tmp_df', value = tmp_df, pos = 1)
 # stop()
  
  #' Check if namespace has control group data
  if(!any(names(tmp_df) %in% c('control'))){
    warning_flag <- 1
  }
  
  #' check if namespace has goals data
  if(!any(tmp_df$name == 'goal')){
    warning_flag <- 1
  }
  
  
  if(!exists("warning_flag")){
    tmp_df %>%
    tidyr::gather(label, total, control:`non-control`) %>%
    tidyr::spread(name, total) %>%
    ungroup() %>%
    group_by(algo_type, campaign_type, label) %>%
    summarize(delivered = sum(delivered, na.rm = TRUE),
              engaged = sum(goal, na.rm = TRUE)) %>%
    mutate(engagement_rate = engaged / delivered) %>%
    select(label, algo_type, engagement_rate, campaign_type) %>%
    tidyr::spread(label, engagement_rate) %>%
    ungroup() %>%
    mutate(diff = `non-control` - control,
           ymid = mean(diff)) %>%
    arrange(desc(diff)) %>%
    ggplot(., aes(x = forcats::fct_reorder(algo_type, diff, .desc = FALSE), y = diff, fill = algo_type)) + 
    geom_bar(stat = 'identity') + 
    geom_text(aes(label = algo_type, y = diff, hjust = ifelse(diff < ymid, -.2, 1.1), color = ifelse(diff < ymid, 'black', 'white')), size = 3.8) +
    scale_color_manual(values = c("white" = "white",
                                  "black" = "black")) +
    coord_flip() +
    facet_grid(campaign_type ~ ., scales = 'free') +
    scale_fill_manual(values = c(  'Message Optimization + Time Optimization' = '#ae0a45ff', 
                                   'No Optimization' = '#999999ff', 
                                   'Send Optimally' = '#66308dff', 
                                   'Time Optimization' = '#27ab7eff',
                                   'Message Optimization + Send Optimally' = '#2a5191',
                                   'Message Optimization' = '#ff942eff')) +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() + 
    geom_blank() + 
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 10),
          rect = element_blank(),
          line = element_blank(),
          panel.spacing.y =unit(2, "lines"),
          strip.text.y = element_text(size = 11),
          legend.position = 'none') +
    labs(x = '',
         y = '')
  }
}
