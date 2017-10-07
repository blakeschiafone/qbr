goal_completion_times <- function(namespace, channels = FALSE, date_begin = FALSE, date_end = FALSE){
  library(bigrquery)
  
  date_begin <- Sys.Date() - 110
  date_end <- Sys.Date() - 10
  
  if (channels == FALSE){
    channels <- FALSE
  } else {
    channels <- 'all'
  }
  
  query <- paste0("
  select
  month,",
                  
  ifelse(channels == 'all', 'category,', ''),
                  
  "string(diff) as diff,
  count(1) as total
  from
  (select 
  month(logs_b.created) as month,",
  ifelse(channels == 'all', '(logs_b.category) as category,', ''),
  "ceil((timestamp_to_sec(logs_a.created) - timestamp_to_sec(logs_b.created))/60) as diff
  from
                    
  (select min(created) as created, campaign_id, user_id", ifelse(channels == 'all', ', category', ''),
  "\nfrom table_date_range([tap-nexus:kahuna_logs.", namespace, "_], timestamp('", date_begin + 2, "'),timestamp('", date_end, "'))
  where (action='goal' and category = 'push') or (action = 'email_goal' and category = 'email') or (action = 'ia_goal' and category = 'inapp')
  group by campaign_id, user_id", ifelse(channels == 'all', ', category', ''), ") as logs_a
                    
  left join each
                    
  (select min(created) as created, campaign_id, user_id", ifelse(channels == 'all', ', category', ''),
  "\nfrom table_date_range([tap-nexus:kahuna_logs.", namespace, "_],timestamp('", date_begin, "'),timestamp('", date_end - 2, "'))
  where (category = 'push' and action = 'push') or (category = 'email' and action = 'email') or (category = 'inapp' and action = 'ia_delivered') and ghost is null
  group by campaign_id, user_id", ifelse(channels == 'all', ', category', ''), ") as logs_b
                    
  on logs_a.user_id = logs_b.user_id AND logs_a.campaign_id = logs_b.campaign_id
  where logs_b.created is not null)
  group by month, diff", ifelse(channels == 'all', ', category', ''))
  
  results <- query_exec(project = 'kahuna-bq-access', query = query, max_pages = Inf)
  
  #results$diff <- as.numeric(round((results$goal - results$pushed)/60, 1))
  results$diff <- as.numeric(results$diff)
  results <- results[results$diff <= 2880,]
  results <- results[results$diff >= 0,]
  
  #results$label <- cut(results$diff, breaks = seq.int(0, 2880, 60), dig.lab = 4)
  results$label <- round(results$diff / 60)
  
  if (channels == FALSE){  
    #channels flag not included, 
    #so data is being grouped by goal buckets and month
    results %>% 
      group_by(month, label) %>%
      summarize(sum_goals = sum(total)) %>%
      mutate(cumsum = cumsum(sum_goals), 
             cum_prop = cumsum / sum(sum_goals)) %>%
      filter(!is.na(label)) -> results2
  } else {
    #channels flag is included,
    #data will be grouped by channel, goal buckets, and month
    results %>%
      group_by(category, label) %>%
      summarize(sum_goals = sum(total)) %>%
      mutate(cumsum = cumsum(sum_goals),
             cum_prop = cumsum / sum(sum_goals)) %>%
      filter(!is.na(label)) -> results2
  }
  
  #ggplot(., aes(x = label, y = cum_prop, group = 1)) + 
  #here we need to check what data is being passed to ggplot
  #currently there are two types:  data by channels and data !channels
  if (channels == FALSE){
    grouped <- factor(month.abb[results2$month])
    colored <- forcats::fct_reorder(month.abb[results2$month], results2$month)
  } else {
    grouped <- factor(stringi::stri_trans_general(results2$category, id = 'Title'))
    colored <- factor(stringi::stri_trans_general(results2$category, id = 'Title'))
  }
  
  ggplot(results2, aes(x = label, 
                       y = cum_prop, 
                       group = grouped, color = colored)) + 
    geom_line(stat = 'identity', lwd = 1) + 
    #scale_color_brewer(palette = 'PRGn') +
    labs(x = '\n\nGoal Attribution Window (60 minute intervals)', 
         y = 'Cumulative %', 
         title = paste0('\n\n', 'After a campaign is sent, how long until a goal is completed?\n'), 
         subtitle = paste0('Based on data between ', format(date_begin, '%B %d'), ' - ',  format(date_end, '%B %d'))) + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank(),
          panel.background = element_rect(fill = NA), 
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          rect = element_blank(),
          line = element_blank(),
          legend.position = 'bottom',
          legend.direction = 'horizontal'
          ) +
    #scale_x_continuous(breaks = 1:30) + 
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(limits = c(0, 48), breaks = c(0, 6, 12, 24, 36, 48)) +
    scale_color_manual(values = c("#28235E","#2FD3B8","#FCC212","#FF7070","#6E66CB","#597FD9","#6995FF","#4141A8")) +
    geom_text(aes(x = label,
                  y = cum_prop,
                  color = colored,
                  label = ifelse(as.character(label) == '0' | 
                                 as.character(label) == '6' | 
                                 as.character(label) == '12' | 
                                 as.character(label) == '24' |
                                 as.character(label) == '36', 
                                 round(cum_prop, 2) * 100, 
                                 NA)),
              size = 3,
              vjust = -1,
              show.legend = FALSE
    )
}
