#' ---
#' title: 'QBR Report'
#' author: '`r NULL`'
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'      toc: true
#'      highlight: zenburn
#' ---

```{r global_options, echo = FALSE, results = "hide", message = FALSE}
knitr::opts_chunk$set(fig.width=14, fig.height=12, echo=FALSE, warning=FALSE, message=FALSE)
```
<br>
<br>
<br>
  

```{r}
#' ### Campaign Summary 
#' This table shows total message volume, goal volume, revenue and engagement rates.  The data is shown for each
#' channel (push, email, in-app) and campaign type (lifecycle, one time, conversion, adaptive, experience).
campaign_summary_kable
```
<br>
<br>
<br> 
  
  
```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: Engagement Rate
#' This graph shows total message volume (grey) and engagement rates (line graph).  Below the graph is
#' a table showing the graphed data.  
if(is.gtable(campaign_performance_engagement_graph)){grid.draw(campaign_performance_engagement_graph)}
```
<br>
<br>
<br>
  

```{r fig.width = 20, fig.height = 14}
#' ### Campaign Performance: Goal Rate
#' This graph shows total message volume (grey) and goal completion rates (line graph).  Below the graph is
#' a table showing the graphed data.  
if(exists('campaign_performance_goal_graph') && is.gtable(campaign_performance_goal_graph)){grid.draw(campaign_performance_goal_graph)}
```
<br>
<br>
<br>
  

```{r fig.width = 14, fig.height = 14}
#' ### Largest Campaign Delivered Volume
#' This graph shows the top 5 (or less) campaigns that had the largest message delivered volume.  Also
#' included is the channel, optimization used, reach (delivered count), goals achieved from campaign, and
#' the engagement rate (push) or unique open rate (email).  *Please note In App campaigns are not shown here
#' because we do not capture engagement for in app campaigns.*
if(exists('largest_volume_sends_graph') && is.gtable(largest_volume_sends_graph)){grid.draw(largest_volume_sends_graph)}
```
<br>
<br>
<br>
  
  
```{r}
#' ### Top 10 Messages (based on engagement)
#' These are the top 10 messages during the specified time-frame.  The engagement rate produced
#' from each message copy is used to determine its ranking.
top_10_messages_engagement_kable
```
<br>
<br>
<br> 
  
  
```{r}
#' ### Weekly Message Volume & Goal Completion Rates
#' This graph shows weekly message volume (push, email, in app) and the corresponding weekly
#' goal completion rate.  Message volume is the grey shaded area, goal completion rate is the 
#' purple line.  Message volume is on left y-axis.  Goal completion rate is on right y-axis.
if(exists('weekly_message_volume_goal_rate_graph') && is.gtable(weekly_message_volume_goal_rate_graph)){grid.draw(weekly_message_volume_goal_rate_graph)}
```
<br>
<br>
<br>
  
  
```{r}
#' ### Top 10 Campaigns (based on engagement)
#' This graph shows the top 10 campaigns.  The ranking is determined by the campaigns engagement rate.
#' The coloring of each bar denotes if an Optimization was used.  Optimizations labels are only listed once,
#' to prevent cluttering of labeling each bar with the same label.  
#' There are two vertical lines included.  The black vertical line is the engagement rate for the customers
#' top 50 campaigns.  The grey vertical line is the engagement rate for all campaigns for the customer.  Think
#' of these as benchmarks within the customer.
if(!is.error(ggplot_build(top_10_campaign_engagement_graph))){plot(top_10_campaign_engagement_graph)}
```
<br>
<br>
<br>
  

```{r fig.width = 14, fig.height = 10}
#' ### Top 50 Campaigns (comparing goal completion & engagement)
#' This graph is a scatterplot of the top 50 campaigns showing the relationship between goal conversion (x-axis) and engagement rate (y-axis).
#' The graph is split into a quadrant, with the vertical and horizontal line representing the customers overall
#' conversion and goal completion rates.  With these 4 areas, the top-right are campaigns producing high conversion and high engagement. 
#' The top-left are campaigns producing low conversion and high engagement.  The bottom-left corner are campaigns producing low conversion
#' and low engagement.  The bottom-right are campaigns producing high converison and low engagement.
#' The color of each dot represents the campaign type (lifecycle, conversion, adaptive, experience, one time).
#' The size of each dot represents the message volume attached to each campaign.
if(exists('top_50_campaigns_xy_graph') && !is.error(ggplot_build(top_50_campaigns_xy_graph))){plot(top_50_campaigns_xy_graph)}
```
<br>
  <br>
  <br>
  
  
```{r fig.width = 12, fig.height = 20}
#' ### Kahuna Messaged Campaigns vs. Control Group
#' This graph shows the top/bottom campaigns and their engagement vs. control.  If a customer has more than 60 campaigns
#' that ran during the specified QBR date range, then only the top 30 (or maximum positive campaigns) and bottom 30 (or maximum negative campaigns)
#' are returned.  This is cutoff is used so the y-axis (campaign list) is readable and not over-crowded.  A campaign can only show here if a control
#' group was used.  The top portion are campaigns that out-performed control in respect to engagement.  The bottom portion are
#' campaigns that under-performed control in respect to engagement. 
#' The color represents the campaign type (lifecycle, conversion, adaptive, experience, one time)
if(is.gtable(compare_campaign_messaged_control_engagement_graph)){grid.draw(compare_campaign_messaged_control_engagement_graph)}
```
<br>
  <br>
  <br>
  
  
```{r}
#' ### Which Optimizations Produce Higher Goal Completion (by channel)
#' This graph shows goal completion rates by channel.  The y-axis is the goal completion rate.  The x-axis is the optimization.
#' The percentages listed above each bar chart is message volume.  This is to show what channels and optimizations are receiving
#' the majority/minority of message volume.
if(exists('compare_goal_rates_by_algo_graph') && !is.error(ggplot_build(compare_goal_rates_by_algo_graph))){plot(compare_goal_rates_by_algo_graph)}
```
<br>
  <br>
  <br>
  
  
```{r}
#' ### Does Message Length/Email Subject Length Affect CTR?
#' This graph shows the relationship between message copy length (push/in app text, email subject) and the corresponding
#' click-thru rate (y-axis).  The size of each bubble represents the message volume that was sent with each message text length (x-axis)
if(!is.error(ggplot_build(ctr_message_length_graph))){plot(ctr_message_length_graph)}
```
<br>
  <br>
  <br>
  
  
```{r}
#' ### Which Optimizations Produce Better Goal Completion (vs. Control)
#' This graph shows goal completion rates against control.  The color of each bar is based on optimization used.  The x-axis
#' shows the goal completion rate difference between Kahuna messaged vs. control.  Because conversion campaigns have access
#' to different optimizations, the graph is separated into two parts: Conversion Campaigns, Non-Conversion Campaigns.
if(exists('compare_algo_type_vs_control_graph') && !is.error(ggplot_build(compare_algo_type_vs_control_graph))){plot(compare_algo_type_vs_control_graph)}
```
<br>
  <br>
  <br>
  

```{r}
#' ### Goal SOM by Channel
#' This graph shows the SOM % by channel for goals completed.  In other words, for each week period, what % of goals were 
#' completed for each channel.  If the customer is only using one channel, then you'll see a 100% SOM for the channel.  
#' It's worth noting that this is largely influenced by the message volume sent across each channel.  As an example, if
#' 1,000,000 messages were sent by push and only 30,000 messages sent by email, then naturally I would expect the push
#' channel to have a higher goal count than email.  Thus, "skewing" the SOM to push, and having it seem like the push
#' channel is better at completing goals.
if(exists('goal_som_graph') && !is.error(ggplot_build(goal_som_graph))){plot(goal_som_graph)}
```
<br>
<br>
<br>
  
  
```{r}
#' ### Goal Count (excluding start ios/start android) and the Campaign Type attributed
#' This graph shows total goal counts.  Start iOS/Start Android are excluded.  These are goals from campaigns.
#' The colors are based on campaign type (lifecycle, adaptive, conversion, experience, one time).  This helps
#' see what campaign types are contributing to goal completion counts.
if(exists('goal_count_over_time_graph') && !is.error(ggplot_build(goal_count_over_time_graph))){plot(goal_count_over_time_graph)}
```
<br>
  <br>
  <br>
  
  
```{r fig.width = 14, fig.height = 10}
#' ### Top 5 Goals Each Month and how they Compare Over Time
#' This graph is meant to show each months top 5 goals (from campaigns).  It is meant to show how
#' the ranking of the beginning top 5 goals flow over time.  In some cases, the chart might be cumbersome.  However, 
#' in other cases, it can produce a clean visual.
if(exists('goals_bump_chart_graph') && !is.error(ggplot_build(goals_bump_chart_graph))){plot(goals_bump_chart_graph)}
```
<br>
  <br>
  <br>
  
  
```{r fig.width = 14, fig.height = 10}
#' ### How Long until Goals are Completed (by month)
#' This graph shows the past 100 days of campaign data.  More specifically, it shows the hours that elapsed until
#' a campaign goal was completed.  The x-axis is 0 - 48 hours.  The y-axis represents percentage of all goals
#' completed during a specific hour window.  So, for example, the x-axis has callouts of 0, 6, 12, 24, 36 and 48 hours.
#' On the line graph, you see numbers for each month.  Those numbers tell you the proportion of goals completed at each hour.
#' Use this graph to see if some months performed better than others.  Look back at campaigns during those months to determine
#' what might cause better/worse performance for some months.
if(exists('goal_completion_times_false_graph') && !is.error(ggplot_build(goal_completion_times_false_graph))){plot(goal_completion_times_false_graph)}
```

<br>
  <br>
  <br>
  
  
  ```{r fig.width = 14, fig.height = 10}
#' ### How Long until Goals are Completed (by channel)
#' This graph shows the past 100 days of campaign data.  More specifically, it shows the hours that elapsed until
#' a campaign goal was completed.  The x-axis is 0 - 48 hours.  The y-axis represents percentage of all goals
#' completed during a specific hour window.  So, for example, the x-axis has callouts of 0, 6, 12, 24, 36 and 48 hours.
#' On the line graph, you see numbers for each month.  Those numbers tell you the proportion of goals completed at each hour.
#' Use this graph to see if some channels performed better than others.
if(exists('goal_completion_times_true_graph') && !is.error(ggplot_build(goal_completion_times_true_graph))){plot(goal_completion_times_true_graph)}
```

<br>
  <br>
  <br>
  
```{r}
#' ### What's the Retention Rate for Users Messaged vs. Not Messaged
#' This graph shows retention/return rate for messaged (purple) vs. non-messaged users (grey).  The x-axis is the
#' user aquisition month/year.  The y-axis shows what percentage of x-axis cohorts returned.  By default, the return date
#' will always be todays date minus 1 month.  So, for example, today is `r format(Sys.Date(), format = '%B %Y')`, which means
#' this graph shows users who returned during `r format(seq(from = Sys.Date(), by = '-1 month', length.out = 2)[2], format = '%B %Y')`.
if(!is.error(ggplot_build(user_push_retention_graph))){plot(user_push_retention_graph)}
```
<br>
  <br>
  <br>
  
  
```{r}
#' ### What's the Average App Open for Users Messaged vs. Not Messaged
#' This graph compares app openings for messaged users (purple) vs. non-messaged (grey).  The x-axis is the user 
#' acquisition month/year.  The y-axis shows how many times x-axis cohorts opened the app.
if(!is.error(ggplot_build(compare_app_opens_graph))){plot(compare_app_opens_graph)}
```
<br>
  <br>
  <br>
  
  
```{r}
#' ### A Daily Heat Map of Messages Delivered 
#' Green: High Volume, Grey: Normal Volume, Red: Low Volume, Black: No Volume
#' This graph is a daily heat map of message volume (push, email, in app).  It's meant to show patterns in message volume.
#' Each bar represents a day of the year. The colors key is listed at the top of this paragraph.
if(!is.error(ggplot_build(calendar_heatmap_daily_graph))){plot(calendar_heatmap_daily_graph)}
```
<br>
  <br>
  <br>
  
  
```{r}
#' ### A Monthly Heat Map of Messages Delivered
#' This graph is a monthly heat map of message volume (push, email, in app).  It's meant to show patterns in message volume.
#' Each bar represents a week of the month.  The colors key is listed at the top of this paragraph.
if(!is.error(ggplot_build(calendar_heatmap_month_graph))){plot(calendar_heatmap_month_graph)}
```
<br>
  <br>
  <br>
  