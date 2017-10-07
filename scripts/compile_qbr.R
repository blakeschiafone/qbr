compile_qbr <- function(namespace, return_date = FALSE, date_begin = FALSE, date_end = FALSE, skip_goals = FALSE){
  library(dplyr)
  library(ggplot2)
  library(svglite)
  library(knitr)
  library(plotrix)
  library(bigrquery)
  library(grid)
  library(gtable)
  library(scales)
  #library(purrr)
  
  flush.console()
  print('load functions')
  
  setwd('/home/rstudio/scripts/qbr/')
  source('./functions/campaign_summary.R')
  source('./functions/compare_campaign_messaged_control_engagement.R')
  source('./functions/top_10_campaign_engagement.R')
  source('./functions/compare_goal_rates_by_algo.R')
  source('./functions/top_50_campaigns_xy.R')
  source('./functions/ctr_message_length.R')
  source('./functions/compare_algo_type_vs_control.R')
  source('./functions/goal_count_over_time.R')
  source('./functions/weekly_message_volume_goal_rate.R')
  source('./functions/compare_app_opens.R')
  source('./functions/goals_bump_chart.R')
  source('./functions/calendar_heatmap_daily.R')
  source('./functions/calendar_heatmap_month.R')
  source('./functions/top_10_messages_engagement.R')
  source('./functions/goal_completion_times.R')
  source('./functions/user_push_retention.R')
  source('./functions/function_is_error.R')
  #' borrowed functions
  source('/home/rstudio/scripts/kss/functions/function_date_to_week_range.R')
  source('/home/rstudio/scripts/kss/functions/function_find_metric.R')
  source('/home/rstudio/scripts/kss/functions/function_create_table_output.R')
  source('/home/rstudio/scripts/kss/functions/function_date_to_week_range.R')
  source('/home/rstudio/scripts/kss/functions/top_n_messages_engagement.R')
  source('/home/rstudio/scripts/kss/functions/top_bottom_campaigns.R')
  source('/home/rstudio/scripts/kss/functions/goal_som_by_channel.R')
  source('/home/rstudio/scripts/kss/functions/largest_volume_sends.R')
  source('/home/rstudio/scripts/kss/functions/campaign_performance_data.R')
  source('/home/rstudio/scripts/kss/functions/largest_volume_sends.R')
  #load connections
  source('/home/rstudio/scripts/db_connection.R')

  
  file_path <- '/home/rstudio/gdrive/Reports/QBR/'
  
  #' gather namespace data from database connection
  flush.console()
  print('gathering namespace data from database')
  daily_counters <- dbGetQuery(db_connection, paste0("select * from bq.counters where table_id = '", namespace, "'"))
  assign('daily_counters', value = daily_counters, pos = 1)
  daily_campaign <- dbGetQuery(db_connection, paste0("select * from bq.campaigns where key like '", namespace, "%'"))
  assign('daily_campaign', value = daily_campaign, pos = 1)
  goals <- dbGetQuery(db_connection, paste0("select * from bq.goals where table_id = '", namespace, "'"))
  assign('goals', value = goals, pos = 1)
  
  flush.console()
  print('check for namespace in data')
  #check if namespace used in benchmark has data
  #if it does not, then error out
  if(!(namespace %in% unique(daily_counters$table_id))){
    stop(paste0(toupper(namespace), ' does not exist is data'))
  } else if (sum(daily_counters$total[daily_counters$table_id == namespace]) < 1000){
    stop(paste0(toupper(namespace), ' does not have enough data to benchmark'))
  }
  
  
  flush.console()
  print('check date format')
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
  
  
  flush.console()
  print('campaign_summary')
  campaign_summary_kable <- campaign_summary(namespace = namespace,
                                             date_begin = date_begin,
                                             date_end = date_end)
  
  flush.console()
  print('campaign performance: engagement rate')
  campaign_performance_engagement_graph <- campaign_performance_data(namespace = namespace, 
                                                                     metric = 'delivered, engagement_rate', 
                                                                     channel = 'push, email, in_app', 
                                                                     date_begin = date_begin, 
                                                                     date_end = date_end, 
                                                                     table_output = TRUE)
  if(skip_goals == FALSE){
  flush.console()
  print('campaign performance: goal rate')
  campaign_performance_goal_graph <- campaign_performance_data(namespace = namespace, 
                                                                     metric = 'delivered, goal_rate', 
                                                                     channel = 'push, email, in_app', 
                                                                     date_begin = date_begin, 
                                                                     date_end = date_end, 
                                                                     table_output = TRUE)
  }
  
  if(skip_goals == FALSE){
  flush.console()
  print('largest volume sends')
  largest_volume_sends_graph <- largest_volume_sends(namespace = namespace, 
                                                     date_begin = date_begin, 
                                                     date_end = date_end)
  }
  
  if(skip_goals == FALSE){
  flush.console()
  print('som by goal')
  goal_som_graph <- goal_som_by_channel(namespace = namespace, 
                                        date_begin = date_begin, 
                                        date_end = date_end)
  }
  
  flush.console()
  print('top_10_messages_engagement')
  top_10_messages_engagement_kable <- top_10_messages_engagement(namespace = namespace, 
                                                                 date_begin = date_begin, 
                                                                 date_end = date_end)
  
  if(skip_goals == FALSE){
  flush.console()
  print('weekly_message_volume_goal_rate')
  weekly_message_volume_goal_rate_graph <- weekly_message_volume_goal_rate(namespace = namespace, 
                                                                          date_begin = date_begin, 
                                                                          date_end = date_end)
  }
  
  flush.console()
  print('top_10_campaign_engagement')
  top_10_campaign_engagement_graph <- top_10_campaign_engagement(namespace = namespace, 
                                                           date_begin = date_begin, 
                                                           date_end = date_end)
  
  if(skip_goals == FALSE){
  flush.console()
  print('top_50_campaigns_xy')
  top_50_campaigns_xy_graph <- top_50_campaigns_xy(namespace = namespace, 
                                                  date_begin = date_begin, 
                                                  date_end = date_end)
  }
  
  flush.console()
  print('compare_campaign_messaged_control_engagement')
  compare_campaign_messaged_control_engagement_graph <- compare_campaign_messaged_control_engagement(namespace = namespace, 
                                                                                                     date_begin = date_begin, 
                                                                                                     date_end = date_end)
  
  if(skip_goals == FALSE){
  flush.console()
  print('compare_goal_rates_by_algo')
  compare_goal_rates_by_algo_graph <- compare_goal_rates_by_algo(namespace = namespace, 
                                                                 date_begin = date_begin, 
                                                                 date_end = date_end)
  }
  
  flush.console()
  print('ctr_message_length')
  ctr_message_length_graph <- ctr_message_length(namespace = namespace, 
                                                 date_begin = date_begin, 
                                                 date_end = date_end)
  if(skip_goals == FALSE){  
  flush.console()
  print('compare_algo_type_vs_control')
  compare_algo_type_vs_control_graph <- compare_algo_type_vs_control(namespace = namespace, 
                                                                     date_begin = date_begin, 
                                                                     date_end = date_end)
  }
  
  if(skip_goals == FALSE){
  flush.console()
  print('goal_count_over_time')
  goal_count_over_time_graph <- goal_count_over_time(namespace = namespace, 
                                                     date_begin = date_begin, 
                                                     date_end = date_end)
  }
  
  if(skip_goals == FALSE){
  flush.console()
  print('goals_bump_chart')
  goals_bump_chart_graph <- goals_bump_chart(namespace = namespace, 
                                             date_begin = date_begin, 
                                             date_end = date_end)
  }
  
  if(skip_goals == FALSE){
  flush.console()
  print('goal_completion_times_channel_false')
  goal_completion_times_false_graph <- goal_completion_times(namespace = namespace, 
                                                       channels = FALSE)
  }
  
  if(skip_goals == FALSE){
  flush.console()
  print('goal_completion_times_channel_true')
  goal_completion_times_true_graph <- goal_completion_times(namespace = namespace,
                                                            channels = TRUE)
  }
  
  flush.console()
  print('user_push_retention')
  user_push_retention_graph <- user_push_retention(namespace = namespace,
                                             date_begin = date_begin, 
                                             date_end = date_end)
  
  flush.console()
  print('compare_app_opens')
  compare_app_opens_graph <- compare_app_opens(namespace = namespace, 
                                               date_begin = date_begin, 
                                               date_end = date_end)
  
  flush.console()
  print('calendar_heatmap_daily')
  calendar_heatmap_daily_graph <- calendar_heatmap_daily(namespace = namespace, 
                                                         date_begin = date_begin, 
                                                         date_end = date_end)
  
  flush.console()
  print('calendar_heatmap_month')
  calendar_heatmap_month_graph <- calendar_heatmap_month(namespace = namespace, 
                                                         date_begin = date_begin, 
                                                         date_end = date_end)
  
  
  #' check if directory exists for namespace
  if(file.exists(paste0(file_path, namespace)) == FALSE){
    dir.create(paste0(file_path, namespace))
  }
  
  
  #' knit benchmark report together
  rmarkdown::render('/home/rstudio/scripts/qbr/scripts/render_html.R',
                    params = list(namespace = namespace_param, date_begin = date_begin, date_end = date_end),
                    output_dir = paste0(file_path, namespace, '/'),
                    output_file = paste0('QBR_', 
                                         format(Sys.Date(), format = '%b%d%Y'), 
                                         '_daterange_',
                                         date_begin,
                                         '_',
                                         date_end,
                                         '.html'))
  
  
  #' push file to googledrive
  system(paste0("/home/rstudio/go/bin/drive push -ignore-name-clashes -exclude-ops \"delete\" -force ", 
                '"/home/rstudio/gdrive/Reports/QBR/', namespace, "/", '"'))
  
  
  #' remove files created
  rm(daily_campaign, daily_counters, goals, table_conversion, db_connection)
  
  gc()
  gc()
}
