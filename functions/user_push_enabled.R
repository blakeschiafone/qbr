user_push_enabled <- function(namespace){

  
  query <- paste0("select
  _TABLE_SUFFIX as table_id,
  sum(if(devices.push_enabled is true and devices.push_token is not null and devices.os_name = 'ios', 1, 0)) as ios_pushenabled,
  sum(if(devices.id is not null and devices.os_name = 'ios', 1, 0)) as ios_devices,
  sum(if(devices.push_enabled is true and devices.push_token is not null and devices.os_name = 'android', 1, 0)) as android_pushenabled,
  sum(if(devices.id is not null and devices.os_name = 'android', 1, 0)) as android_devices
  from `tap-nexus.kahuna_users.*`, UNNEST(devices) as devices
  where not devices.id like '%historical%' and
  _TABLE_SUFFIX like '", namespace, "%' and not
  _TABLE_SUFFIX like '%_qa%' and not
  _TABLE_SUFFIX like '%_sandbox%' and
  SUBSTR(_TABLE_SUFFIX, -9, 1) = '_' and
  date(devices.last_seen) >= DATE_ADD(date(modified), INTERVAL -90 DAY)
  group by table_id")
  
  results <- query_exec(query = query, project = 'kahuna-bq-access', useLegacySql = FALSE)
  
  #' format table_id column removing namespace_
  #' convert class to date
  results$table_id <- gsub(paste0(namespace, '_'), '', results$table_id)
  results$table_id <- zoo::as.yearmon(results$table_id, '%Y%m%d')
  
  #' calculate opt-in rates total and by OS
  results$total_optin <- (results$ios_pushenabled + results$android_pushenabled) / (results$ios_devices + results$android_devices)
  results$ios_optin <- results$ios_pushenabled  / results$ios_devices
  results$android_optin <- results$android_pushenabled  / results$android_devices
  
  #' format dataframe into correct view
  results %>% 
    select(table_id, total_optin, ios_optin, android_optin) %>%
    mutate(table_id = factor(table_id)) %>%
    tidyr::gather(type, value, 2:ncol(.)) %>%
    mutate(type = case_when(
      .$type == 'total_optin' ~ 'Total Opt-In',
      .$type == 'ios_optin' ~ 'iOS Opt-In',
      .$type == 'android_optin' ~ 'Android Opt-In')
    ) %>%
    mutate(type = factor(type, levels = c('Total Opt-In', 'iOS Opt-In', 'Android Opt-In'))) -> results
  
  
  #' plot opt-in rates:
  #' total, ios, android
  ggplot(results, aes(x = table_id, y = value, group = type, color = type)) + 
    geom_line(size = 1) +
    scale_y_continuous(labels = scales::percent) +
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
          legend.position = 'bottom',
          legend.direction = 'horizontal',
          legend.title = element_blank()) +
    scale_color_manual(values = c('#ae0a45ff', '#66308dff', '#2a5191')) +
    labs(x = '',
         y = '\nOpt-In Rate\n',
         title = '')
}
