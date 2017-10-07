goals_bump_chart <- function(namespace, date_begin = FALSE, date_end = FALSE){
  library(plotrix)
  
  
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
    select(goal_name, when, count) %>%
    mutate(date_chr = factor(format(when, '%b %Y'))) %>%
    mutate(date_order = as.Date(stringr::str_c(format(when, '%m'), '01', format(when, '%Y'), sep = '-'), format = '%m-%d-%Y')) %>%
    group_by(date_order, date_chr, goal_name) %>%
    summarize(count = sum(count, na.rm = TRUE)) %>%
    arrange(desc(count)) %>%
    #assign('del', value = ., pos = 1) %>%
    mutate(rn = row_number()) %>%
    filter(rn <= 5) -> tmp_df
  
  
  #' Reorder factor date_chr
  tmp_df$date_chr <- forcats::fct_reorder(tmp_df$date_chr, tmp_df$date_order)
  
  #' Get min/max date_chr for labeling
  min_date_chr <- as.character(levels(tmp_df$date_chr)[1])
  max_date_chr <- as.character(levels(tmp_df$date_chr)[length(levels(tmp_df$date_chr))])
    
  
    ggplot(tmp_df, aes(x = date_chr, y = rn, group = goal_name)) + 
    geom_line(aes(color = goal_name), size = 2) +
    geom_point(aes(color = goal_name), size = 2.3) +
    geom_point(color = "#FFFFFF", alpha = .8, size = .3) +
    geom_text(data = tmp_df %>% filter(date_chr == max_date_chr, rn <= 5), aes(label = goal_name, x = max_date_chr) , hjust = -.05, color = "#888888", size = 4) +
    geom_text(data = tmp_df %>% filter(date_chr == min_date_chr, rn <= 5), aes(label = goal_name, x = min_date_chr) , hjust = 1.05, color = "#888888", size = 4) +
    scale_x_discrete(expand = c(.3, .3)) +
    scale_y_reverse(breaks = c(1,5,10,15)) +
    scale_alpha_discrete(range = c(.4,.9)) +
    labs(title = "") +
    labs(subtitle = "") +
    labs(x = "", y = "") +
    theme(panel.grid.major.x = element_line(color = "#F3F3F3")) +  
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.text.x = element_text(size = 10)) +
    theme(legend.position = "none") +
    scale_color_manual(values = c("#28235E","#2FD3B8","#FCC212","#FF7070","#6E66CB","#597FD9","#6995FF","#4141A8",
                                  "#524BA1","#597FD9","#e88e27","#6D6D6D","#8FADFF","#151B28","#4e79a5","#f18f3b",
                                  "#af0a64","#e0585b","#5aa155","#edc958","#77b7b2"))
  
}
