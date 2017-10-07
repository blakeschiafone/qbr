top_10_campaign_engagement <- function(namespace, date_begin = FALSE, date_end = FALSE){
  
  
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
         channel %in% c('push', 'email', 'in_app')) %>% 
  select(key, channel, name, total, algo_type) %>%
  mutate(name = ifelse(name == 'ia_delivered', 'delivered', name),
         algo_type = recode(algo_type,
                            'tiq' = 'Time Optimization',
                            'so' = 'Send Optimally',
                            'miq' = 'Message Optimization',
                            'miq/tiq' = 'Message Optimization + Time Optimization',
                            'none' = 'No Optimization',
                            'miq/so' = 'Message Optimization + Send Optimally')) %>%
  left_join(daily_campaign, by = c("key" = "key")) %>%
  select(display_name, name, algo_type, total) %>%
  group_by(display_name, name, algo_type) %>%
  summarize(total = sum(total)) %>%
  tidyr::spread(name, total) %>%
  mutate(engagement_rate = engaged / delivered) %>%
  filter(delivered > 1000,
         display_name != 'Individual Reachout') %>%
  arrange(desc(engagement_rate)) %>%
  ungroup() %>%
  #top_n(10) %>%
  mutate(rn = row_number(),
         rn2 = ifelse(rn <= 50, 'top_50', rn)) -> tmp_df; tmp_df$display_name <- ifelse(nchar(tmp_df$display_name) > 80,
                                                                                          paste0(strtrim(tmp_df$display_name, 80), '...'),
                                                                                          tmp_df$display_name)

#' remove NA engaged
tmp_df <- na.omit(tmp_df)

#top 50 engagement
tmp_df_50 <- tmp_df %>% filter(rn2 == 'top_50') %>% group_by(rn2) %>% summarize(delivered = sum(delivered), engaged = sum(engaged)) %>% mutate(engagement_rate = engaged / delivered) %>% select(engagement_rate)
#all engagement
tmp_df_all <- tmp_df %>% summarize(delivered = sum(delivered, na.rm = TRUE), engaged = sum(engaged, na.rm = TRUE)) %>% mutate(engagement_rate = engaged / delivered) %>% select(engagement_rate)

ggplot(tmp_df[tmp_df$rn <= 10,], aes(x = forcats::fct_reorder(display_name, engagement_rate, .desc = FALSE), 
                                     y = engagement_rate, 
                                     fill = algo_type)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() +
  geom_hline(aes(yintercept = tmp_df_all$engagement_rate), color = 'grey', linetype = 'dashed') +
  geom_hline(aes(yintercept = tmp_df_50$engagement_rate), color = 'black', linetype = 'dashed') +
  geom_text(data = tmp_df %>% group_by(algo_type) %>% slice(which.max(engagement_rate)) %>% filter(rn <= 10) %>% arrange(rn), aes(label = algo_type), color = 'white', size = 4, hjust = 1.05) +
  geom_text(aes(y = tmp_df_all$engagement_rate, x = 0.4, label = '\n\n  all campaigns\n', hjust = 0, vjust = .5), size = 4, color = 'grey') +
  geom_text(aes(y = tmp_df_50$engagement_rate, x = 0.4, label = '\n\n  top 50 campaigns\n', hjust = 0, vjust = .5), size = 4, color = 'black') +
  theme_bw() +
  theme(
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          rect = element_blank(),
          line = element_blank(),
          legend.position = 'none'
        ) + 
  expand_limits(x = -.05) +
  scale_fill_manual(values = c('Message Optimization + Time Optimization' = '#ae0a45ff', 
                               'No Optimization' = '#999999ff', 
                               'Send Optimally' = '#66308dff', 
                               'Time Optimization' = '#27ab7eff',
                               'Message Optimization + Send Optimally' = '#2a5191',
                               'Message Optimization' = '#ff942eff')
                    ) +
  labs(
        x = '',
        y = ''
      ) +
  scale_y_continuous(labels = scales::percent, position = 'top')
}
