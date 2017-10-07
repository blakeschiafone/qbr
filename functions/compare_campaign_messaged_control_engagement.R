#' Because a customer can have several thousand campaigns running
#' This function will only pull the top/bottom 30
#' Otherwise, the y-axis will have too many campaigns to display

compare_campaign_messaged_control_engagement <- function(namespace, date_begin = FALSE, date_end = FALSE){
  
  
  #' Reset device screen
  if(!is.null(dev.list())){
    dev.off()
  }
  
  
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
           name %in% c('delivered', 'ia_delivered', 'engaged', 'goal'),
           channel %in% c('push')) %>% 
    select(date, key, label, name, total, algo_type) %>%
    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name),
           algo_type = recode(algo_type,
                              'tiq' = 'Time Optimization',
                              'so' = 'Send Optimally',
                              'miq' = 'Message Optimization',
                              'miq/tiq' = 'Message Optimization + Time Optimization',
                              'none' = 'No Optimization',
                              'miq/so' = 'Message Optimization + Send Optimally')) %>%
    left_join(daily_campaign, by = c("key" = "key")) %>%
    select(-key) %>%
    group_by(display_name, label, name, algo_type) %>%
    summarize(total = sum(total)) %>%
    tidyr::spread(name, total) %>%
    mutate(engagement_rate = engaged / delivered) %>%
    #filter(!is.na(delivered), !is.na(goal)) %>%
    filter(!is.na(delivered)) %>%
    select(display_name,label, algo_type, engagement_rate) %>%
    tidyr::spread(label, engagement_rate) -> tmp_df
  
  
   #' Check if namespace has control group data
   if(!any(names(tmp_df) %in% c('control'))){
     warning_flag <- 1
   }
  
  
  if(!exists("warning_flag")){
    tmp_df %>%
      filter(!is.na(control), !is.na(`non-control`)) %>%
      mutate(diff = `non-control` - control) %>%
      arrange(desc(diff)) -> tmp_df; tmp_df$display_name <- ifelse(nchar(tmp_df$display_name) > 80,
                                                                           paste0(strtrim(tmp_df$display_name, 80), '...'),
                                                                           tmp_df$display_name)
    #' Determine if campaigns returned is >60
    #' If so, then split the results to just top/bottom 30
    if(nrow(tmp_df) > 60){
      
      tmp_df <- tmp_df[order(tmp_df$diff, decreasing = TRUE),]
      tmp_df <- tmp_df[is.finite(tmp_df$diff),]
      
      #' Get top 30 campaigns (defined as diff > 0 (diff is lift between messaged vs. control))
      tmp_df_top <- na.omit(tmp_df[tmp_df$diff > 0,][1:30,])
      
      #' Get bottom 30 campaigns (defined as diff < 0 (diff is lift between messaged vs. control))
      tmp_df_bottom <- tail(tmp_df, 30)
      tmp_df_bottom <- tmp_df_bottom[tmp_df_bottom$diff < 0,]
      
      tmp_df <- rbind(tmp_df_top, tmp_df_bottom)
    }
        
    
    #' Get xmin and xmax values
    #' Need for correctly placing text outside plot area
    ymax_value <- max(tmp_df$diff)
    ymin_value <- min(tmp_df$diff)
    xmax_value_pos <- nrow(tmp_df[tmp_df$diff > 0,]) #number of rows for positive diff
    xmax_value_neg <- nrow(tmp_df[tmp_df$diff < 0,]) #number of rows for negative diff
    total_rows_value <- nrow(tmp_df)
    
    
    chart1 <- ggplot(tmp_df, aes(x = forcats::fct_reorder(display_name, diff, .desc = FALSE), 
                    y = diff, 
                    fill = algo_type)) + 
      geom_bar(stat = 'identity') + 
      coord_flip() +
      scale_fill_manual(values = c(  'Message Optimization + Time Optimization' = '#ae0a45ff', 
                                     'No Optimization' = '#999999ff', 
                                     'Send Optimally' = '#66308dff', 
                                     'Time Optimization' = '#27ab7eff',
                                     'Message Optimization + Send Optimally' = '#2a5191',
                                     'Message Optimization' = '#ff942eff'
                                   )
                        ) +
      scale_y_continuous(labels = scales::percent, limits = c(ymin_value, ymax_value)) +
      theme_bw() + 
      geom_blank() + 
      theme(panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_blank(), 
            panel.grid.minor.y = element_blank(),
            rect = element_blank(),
            line = element_blank(),
            plot.margin = unit(c(0,10,2,0), "lines"),
            legend.position = 'bottom',
            legend.direction = 'horizontal',
            legend.title = element_blank()) +
      labs(x = '',
           y = '')
    
    
    if(total_rows_value > 0 & xmax_value_pos > 0){
      #' ====================
      #' Positive annotations
      #' ====================
      #' Add positive text
      chart1 <- chart1 + annotation_custom(grob = textGrob("Performed Better \nthan Control", x = unit(.1, "npc"), 
                                        gp = gpar(fontsize = 7, color = "black")), ymin = ymax_value + .03, ymax = ymax_value + .03, 
                                        xmin = total_rows_value - (xmax_value_pos / 2), xmax = total_rows_value - (xmax_value_pos / 2)) + 
      #' Add positive lines
      annotation_custom(grob = linesGrob(), ymin = ymax_value + .005, ymax = ymax_value + .01, xmin = total_rows_value, xmax = total_rows_value) +  #top hline for pos diff
      annotation_custom(grob = linesGrob(), ymin = ymax_value + .005, ymax = ymax_value + .01, xmin = (total_rows_value - xmax_value_pos) + 1, xmax = (total_rows_value - xmax_value_pos) + 1) + #bottom hline for pos diff
      annotation_custom(grob = linesGrob(), ymin = ymax_value + .01, ymax = ymax_value + .01, xmin = (total_rows_value - xmax_value_pos) + 1, xmax = total_rows_value) #vline for pos diff
    }
      
    
    if(total_rows_value > 0 & xmax_value_neg > 0){
      # ' ====================
      # ' Negative annotations
      # ' ====================
      # ' Add negative text
      chart1 <- chart1 + annotation_custom(grob = textGrob("Performed Worse \nthan Control", x = unit(.1, "npc"),
                                        gp = gpar(fontsize = 7, color = "black")), ymin = ymax_value + .03, ymax = ymax_value + .03,
                        xmin = (xmax_value_neg / 2), xmax = (xmax_value_neg / 2)) +
      # ' Add negative lines
      annotation_custom(grob = linesGrob(), ymin = ymax_value + .005, ymax = ymax_value + .01, xmin = xmax_value_neg, xmax = xmax_value_neg) +  #top hline for neg diff
      annotation_custom(grob = linesGrob(), ymin = ymax_value + .005, ymax = ymax_value + .01, xmin = 1, xmax = 1) + #bottom hline for neg diff
      annotation_custom(grob = linesGrob(), ymin = ymax_value + .01, ymax = ymax_value + .01, xmin = 1, xmax = xmax_value_neg) #vline for neg diff
    }
    
    
    chart1 <- ggplot_gtable(ggplot_build(chart1))
    chart1$layout$clip[chart1$layout$name=="panel"] <- "off"
    
    chart1
    
    #if(grid_draw == TRUE){grid.draw(chart1)}
  }
}
