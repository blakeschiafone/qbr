calendar_heatmap_daily <- function(namespace, date_begin = FALSE, date_end = FALSE){
  
  
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
           name %in% c('delivered', 'ia_delivered'),
           channel %in% c('push', 'email', 'in_app'),
           label == 'non-control') %>% 
    select(date, name, total) %>%
    mutate(name = ifelse(name == 'ia_delivered', 'delivered', name)) %>%
    group_by(date, name) %>%
    summarize(total = log(sum(total))) %>%
    mutate(weekday = factor(weekdays(date), levels = rev(c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))),
           weekyears = strftime(date, '%W'),
           year = strftime(date, '%Y'),
           month = factor(strftime(date, '%b'), levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')),
           total = ifelse(!is.finite(total), NA, total),
           q = as.character(findInterval(total, quantile(.$total, na.rm = TRUE)))) %>%
    ggplot(., aes(x = weekyears, y = weekday, fill = q)) + 
    geom_tile(color = 'black') + 
    scale_fill_manual(values = c('1' = '#9e1c1c', '2' = '#d36363', '3' = '#eae9da', '4' = '#579674', '5' = '#066d36'), na.value = 'black') +
    #scale_x_continuous(expand = c(0,0)) +
    coord_fixed() + 
    facet_grid(year ~ month, scales = 'free') +
    xlab("") + 
    ylab("") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_blank(),
          rect = element_blank(),
          line = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          panel.spacing.x = unit(-.05, "lines"),
          legend.position = 'none',
          axis.text.x = element_blank()) +
    scale_x_discrete(expand = c(.02, 0)) + 
    scale_y_discrete(expand = c(.02, 0))
}
