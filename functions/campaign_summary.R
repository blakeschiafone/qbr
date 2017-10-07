campaign_summary <- function(namespace, date_begin = FALSE, date_end = FALSE, return_chart = TRUE){
  suppressPackageStartupMessages(library(dplyr))
  options(error = stop)
  
  
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
  
  
  #create an empty matrix as placeholder
  define_columns <- c("Experiences", "Lifecycle", "Conversion", "One Time", "Adaptive", ' ', "Total")
  define_rows <- c("Campaigns:", "Push", "Email", "In App", 
                   ' ', 
                   "Messages: ", "Push", "Email", "In App",
                   '  ',
                   "Goals: ", "Push", "Email", "In App",
                   '   ',
                   "Goal Rate: ", "Push", "Email", "In App",
                   '    ',
                   "Revenue: ", "Push", "Email", "In App")
  
  campaign_matrix <- matrix(nrow = length(define_rows),
                            ncol = length(define_columns),
                            byrow = TRUE,
                            dimnames = list(define_rows, define_columns))
  
  #campaign_matrix <- as.data.frame(as.table(campaign_matrix))
  
  #append campaign_matrix with a key, which will be used to find matching (row, column) index
  #campaign_matrix <- cbind(c("c", "c-push", "c-email", "c-in_app", NA, "m", "m-push", "m-email", "m-in_app", NA, "g", "g-push", "g-email", "g-in_app"), campaign_matrix)
  
  
  #create data frame that will be used for summaries
  daily_counters %>% 
    filter(table_id == namespace, 
           label == 'non-control', 
           date >= as.Date(date_begin), 
           date <= as.Date(date_end), 
           name %in% c('delivered', 'ia_delivered', 'engaged')) %>% 
    select(name, channel, campaign_type, total) %>% 
    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name),
           campaign_type = case_when(
             .$campaign_type == 'auto' ~ 'Lifecycle',
             .$campaign_type == 'one_time' ~ 'One Time',
             .$campaign_type == 'program_message' ~ 'Experiences',
             .$campaign_type %in% c('trigger', 'immediate_trigger') ~ 'Conversion',
             .$campaign_type == 'adaptive' ~ 'Adaptive')
           ) %>% 
    group_by(channel, campaign_type, name) %>% 
    summarize(total = sum(total, na.rm = TRUE)) %>%
    tidyr::spread(name, total) %>%
    mutate(engagement_rate = ifelse(is.na(engaged), 0, engaged) / delivered) -> tmp_message_stats_df
  
  daily_counters %>% 
    filter(table_id == namespace, 
           label == 'non-control', 
           date >= as.Date(date_begin), 
           date <= as.Date(date_end), 
           name %in% c('delivered', 'ia_delivered')) %>% 
    select(name, channel, campaign_type, total, campaign_suite_id) %>% 
    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name),
           campaign_type = case_when(
             .$campaign_type == 'auto' ~ 'Lifecycle',
             .$campaign_type == 'one_time' ~ 'One Time',
             .$campaign_type == 'program_message' ~ 'Experiences',
             .$campaign_type %in% c('trigger', 'immediate_trigger') ~ 'Conversion',
             .$campaign_type == 'adaptive' ~ 'Adaptive')
    ) %>% 
    group_by(channel, campaign_suite_id, campaign_type, name) %>% 
    summarize(total = sum(total, na.rm = TRUE)) %>%
    filter(total > 10) %>%
    ungroup() %>%
    group_by(channel, campaign_type) %>%
    summarize(campaign_count = n_distinct(campaign_suite_id)) -> tmp_campaign_count_df

  daily_counters %>% 
    filter(table_id == namespace, 
           label == 'non-control', 
           date >= as.Date(date_begin), 
           date <= as.Date(date_end), 
           name %in% c('campaign_value', 'ia_campaign_value', 'email_campaign_value')) -> tmp_revenue_df
  if(nrow(tmp_revenue_df) > 0){
    tmp_revenue_df %>% select(name, channel, campaign_type, total) %>% 
    mutate(name = ifelse(name %in% c('email_campaign_value', 'ia_campaign_value'), 'campaign_value', name),
             campaign_type = case_when(
             .$campaign_type == 'auto' ~ 'Lifecycle',
             .$campaign_type == 'one_time' ~ 'One Time',
             .$campaign_type == 'program_message' ~ 'Experiences',
             .$campaign_type %in% c('trigger', 'immediate_trigger') ~ 'Conversion',
             .$campaign_type == 'adaptive' ~ 'Adaptive')
    ) %>% 
    group_by(channel, campaign_type, name) %>% 
    summarize(total = sum(total, na.rm = TRUE)) %>%
    tidyr::spread(name, total) -> tmp_revenue_df
    } else {
     tmp_revenue_df <- data.frame(expand.grid(channel = c('push', 'email', 'in_app'), 
                                              campaign_type = c('One Time', 'Conversion', 'Lifecycle', 'Experiences', 'Adaptive'),
                                              stringsAsFactors = FALSE))
     tmp_revenue_df$campaign_value <- 0
     }

  daily_counters %>% 
    filter(table_id == namespace, 
           label == 'non-control', 
           date >= as.Date(date_begin), 
           date <= as.Date(date_end), 
           name == 'goal') %>% 
    select(name, channel, campaign_type, total) -> tmp_goal_df
  if(nrow(tmp_goal_df) > 0){
    tmp_goal_df %>% mutate(campaign_type = case_when(
             .$campaign_type == 'auto' ~ 'Lifecycle',
             .$campaign_type == 'one_time' ~ 'One Time',
             .$campaign_type == 'program_message' ~ 'Experiences',
             .$campaign_type %in% c('trigger', 'immediate_trigger') ~ 'Conversion',
             .$campaign_type == 'adaptive' ~ 'Adaptive')
    ) %>% 
    group_by(channel, campaign_type, name) %>% 
    summarize(total = sum(total, na.rm = TRUE)) %>%
    tidyr::spread(name, total) -> tmp_goal_df
  } else {
      tmp_goal_df <- data.frame(expand.grid(channel = c('push', 'email', 'in_app'), 
                                            campaign_type = c('One Time', 'Conversion', 'Lifecycle', 'Experiences', 'Adaptive'),
                                            stringsAsFactors = FALSE))
      tmp_goal_df$goal <- 0
    }
  
  tmp_message_stats_df %>% 
    left_join(tmp_campaign_count_df, by = c('channel', 'campaign_type')) %>% 
    left_join(tmp_revenue_df, by = c('channel', 'campaign_type')) %>%
    left_join(tmp_goal_df, by = c('channel', 'campaign_type')) %>%
    filter(delivered > 10) %>% 
    mutate(goal_rate = goal / delivered,
           revenue = campaign_value / 100) -> tmp_combined
    #kable(.)
    #write.csv(., file = 'mindbody_message_stats.csv', row.names = FALSE)
  
    row_levels <- c('Campaigns:' = 'campaign_count', 'Messages: ' = 'delivered', 'Goals: ' = 'goal', 
                    'Goal Rate: ' = 'goal_rate', 'Revenue: ' = 'revenue')
    col_levels <- factor(colnames(campaign_matrix), levels = colnames(campaign_matrix))
    channel_levels <- c('Push' = 'push', 'Email' = 'email', 'In App' = 'in_app')
    
    
    tmp_x <- c()
    for (i in rownames(campaign_matrix)[seq(1, length(rownames(campaign_matrix)), 5)]){
      #for (x in 1:colnames(campaign_matrix)){
        
        tmp_x <- c(tmp_x, which(row_levels[i][[1]] == names(tmp_combined)))
        #tmp_y <- which(colnames(campaign_matrix)[x]) == 
        
        tmp.matrix <- tmp_combined[,c(1, 2, tmp_x)]
      #}
    }
    

    for (i in subset(colnames(campaign_matrix), !(colnames(campaign_matrix) %in% c('Total', ' ')))){
      for (x in seq_along(channel_levels)){
        campaign_matrix[1 + x, i] <- formatC(sum(tmp.matrix$campaign_count[tmp.matrix$campaign_type == i & tmp.matrix$channel == channel_levels[x]]), format = 'd', big.mark = ',') #' campaign_count
        campaign_matrix[6 + x, i] <- formatC(sum(tmp.matrix$delivered[tmp.matrix$campaign_type == i & tmp.matrix$channel == channel_levels[x]]), format = 'd', big.mark = ',') #' delivered
        campaign_matrix[11 + x, i] <- formatC(sum(tmp.matrix$goal[tmp.matrix$campaign_type == i & tmp.matrix$channel == channel_levels[x]]), format = 'd', big.mark = ',') #' goal
        campaign_matrix[16 + x, i] <- scales::percent(sum(tmp.matrix$goal_rate[tmp.matrix$campaign_type == i & tmp.matrix$channel == channel_levels[x]])) #' goal_rate
        campaign_matrix[21 + x, i] <- scales::dollar(sum(tmp.matrix$revenue[tmp.matrix$campaign_type == i & tmp.matrix$channel == channel_levels[x]])) #' revenue
        # print(i, x)
        # i_num <- which(subset(colnames(campaign_matrix), colnames(campaign_matrix) != 'Total') == i)
        # campaign_matrix[1 + x, i] <- ifelse(is.error(sum(tmp.matrix[tmp.matrix$campaign_type == i & tmp.matrix$channel == channel_levels[x], 2 + i_num], na.rm = TRUE)), 
        #                                     NA,
        #                                     sum(tmp.matrix[tmp.matrix$campaign_type == i & tmp.matrix$channel == channel_levels[x], 2 + i_num], na.rm = TRUE))
      }
      
      #' total columns for each section (campaigns, messages, goals, goal rate, revenue)
      campaign_matrix[1, i] <- formatC(sum(as.numeric(gsub(',', '', campaign_matrix[c(2:4), i])), na.rm = TRUE), format = 'd', big.mark = ',')
      campaign_matrix[6, i] <- formatC(sum(as.numeric(gsub(',', '', campaign_matrix[c(7:9), i])), na.rm = TRUE), format = 'd', big.mark = ',')
      campaign_matrix[11, i] <- formatC(sum(as.numeric(gsub(',', '', campaign_matrix[c(12:14), i])), na.rm = TRUE), format = 'd', big.mark = ',')
      campaign_matrix[16, i] <- scales::percent(as.numeric(gsub(',', '', campaign_matrix[11, i])) / as.numeric(gsub(',', '', campaign_matrix[6, i])))
      campaign_matrix[21, i] <- scales::dollar(sum(as.numeric(gsub(',', '', gsub('\\$', '', campaign_matrix[c(22:24), i]))), na.rm = TRUE))
      
      #' check if last column 'Adaptive' is complete, 
      #' if so, we can calculate row totals now
      if (i == 'Adaptive'){
        #' campaigns and messages
        campaign_matrix[1:14, 7] <- apply(campaign_matrix[1:14,1:5], 1, function(x) formatC(sum(as.numeric(gsub('\\$', '', gsub(',', '', x)))), format = 'd', big.mark = ','))
        #' goal rates
        campaign_matrix[16, 7] <- scales::percent(as.numeric(gsub(',', '', campaign_matrix[11,7])) / as.numeric(gsub(',', '', campaign_matrix[6,7])))
        campaign_matrix[17, 7] <- scales::percent(as.numeric(gsub(',', '', campaign_matrix[12,7])) / as.numeric(gsub(',', '', campaign_matrix[7,7])))
        campaign_matrix[18, 7] <- scales::percent(as.numeric(gsub(',', '', campaign_matrix[13,7])) / as.numeric(gsub(',', '', campaign_matrix[8,7])))
        campaign_matrix[19, 7] <- scales::percent(as.numeric(gsub(',', '', campaign_matrix[14,7])) / as.numeric(gsub(',', '', campaign_matrix[9,7])))
        #' revenue
        campaign_matrix[21:24, 7] <- apply(campaign_matrix[21:24,1:5], 1, function(x) formatC(sum(as.numeric(gsub('NA', '', gsub('\\$', '', gsub(',', '', x)))), na.rm = TRUE), format = 'd', big.mark = ','))
        
        #' remove NA, NaN%
        campaign_matrix[which(is.na(campaign_matrix))] <- ''
        campaign_matrix[which(campaign_matrix == 'NaN%')] <- ''
        campaign_matrix[which(campaign_matrix == 'NA')] <- ''
        campaign_matrix[which(campaign_matrix == '$NA')] <- ''
        campaign_matrix[which(campaign_matrix == 'NA%')] <- ''
        campaign_matrix[which(campaign_matrix == '$0')] <- ''
        campaign_matrix[which(campaign_matrix == '0%' & nchar(campaign_matrix) == 2)] <- ''
        campaign_matrix[which(campaign_matrix == '0' & nchar(campaign_matrix) == 1)] <- ''
        
      }
    }
    
    if (return_chart == TRUE){
      grid.draw(tableGrob(campaign_matrix, theme = ttheme_default()))
    } else {
      tableGrob(campaign_matrix, theme = ttheme_default())
    }
  
}
