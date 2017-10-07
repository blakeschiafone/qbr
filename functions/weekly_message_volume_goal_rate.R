weekly_message_volume_goal_rate <- function(namespace, date_begin = FALSE, date_end = FALSE){
  
  
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
           channel %in% c('push', 'email', 'in_app')) %>% 
    select(date, name, total, algo_type) %>%
    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name)) %>%
    mutate(week_begin = lubridate::floor_date(date, unit = "week"),
           week_end = week_begin + 6) %>%
    select(-c(date)) %>%
    group_by(week_begin, week_end, name) %>%
    summarize(total = sum(total)) %>%
    tidyr::spread(name, total) %>%
    filter(delivered > 1) %>% 
    arrange(week_begin) %>%
    mutate(goal_rate = goal / delivered) -> tmp_df
  
  #filter out weeks with delivered <100
  #used to filter out weeks when delivery stops
  #which causes problems in graphing
   # tmp_df$delivered[tmp_df$delivered < 100] <- 0
   # tmp_df$goal[tmp_df$delivered < 100] <- 0
   # tmp_df$goal_rate <- ifelse(tmp_df$goal == 0, 0, tmp_df$goal) / tmp_df$delivered
  
  #plot message volume
  chart1 <- ggplot(tmp_df, aes(x = week_begin, y = delivered)) + 
    geom_area(stat = 'identity', fill = 'grey') +
    scale_y_continuous(labels = scales::comma) + 
    scale_x_date(date_labels = '%b %Y', date_breaks = '1 month') + 
    theme_bw() + 
    geom_blank() + 
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          rect = element_blank(),
          line = element_blank()) +
    labs(x = '',
         y = '')
  
  #plot goal completion rates
  chart2 <- ggplot(tmp_df, aes(x = week_begin, 
                               y = goal_rate)) + 
    geom_line(color = '#613889') + 
    scale_y_continuous(labels = scales::percent) + 
    scale_x_date(date_labels = '%b %Y') + 
    theme_bw() + 
    geom_blank() + 
    theme(panel.background = element_rect(fill = NA), 
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          rect = element_blank(),
          line = element_blank())
  
  grid.newpage()
  
  # extract gtable
  g1 <- ggplot_gtable(ggplot_build(chart1))
  g2 <- ggplot_gtable(ggplot_build(chart2))
  
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  #ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  g
  # draw it
  #grid.draw(g)
}
