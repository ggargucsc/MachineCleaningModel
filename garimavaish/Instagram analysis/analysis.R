library(dplyr)
library(ggplot2)
library(bigrquery)
library(reshape2)


# theme for plots
my_theme = theme(axis.line = element_line(size=1, colour = "black"),
                 panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
                 panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 15, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma", face = "bold"),
        axis.text.x=element_text(colour="black", size = 15),
        axis.text.y=element_text(colour="black", size = 15)) +
  theme(legend.position="top") + theme(legend.title = element_blank())

options(scipen=999)
# Connect to big query
project <- "bf-data-sci" 

####### How many posts/stories/live on instagram by different accounts? 
# currently we are not collecting any data on Story
# post - original feature of instagram
# all instagram posts will be now instagram images
# story - multiple posts and videos appear together in a slideshow, only active for 24 hrs
# live - similar to facebook live
# impressions - No. of times a post from your page was displayed, whether the post is clicked or not. 
#               People may see multiple impressions of the same post. 
#               For ex, someone might see a page update in the News Feed once, and then a second time if a friend shares it
#               it gives you a clear understanding of howmany times your content has been exposed
# reach - No. of unique people who received impressions of a page post. Reach might be less than impressions since one person can 

# We have only post and video data (engagement/impressions/reach/video_views/saved)

# 1) Total posts published (categorize as image/video) 
# 2) Does impression, reach and engagement follows the same trend?
# 3) Top 5 accounts based on total engagement, total impressions, total reach for a week 
# 4) Followers per account


################################################# Aggregate metrics ######################
ga_imp_metrics <- 
  "WITH latest_ts_ga AS (
            SELECT
                external_object_id,
                MAX(stats_ts) as latest_ts,
                DATE(stats_date) as stats_date
            FROM prod.distributed_stats_storage_instagram_post
            WHERE DATE(stats_date) >= '2017-12-10' 
                and DATE(stats_date) <= '2017-12-24'
            GROUP BY DATE(stats_date), external_object_id
            ),
          latest_week_metrics_ga AS (
            SELECT
                latest_ts.external_object_id,
                post.engagement,  
                post.saved,
                post.impressions,
                post.reach,
                post.video_views,
                latest_ts.stats_date as stats_dt
            FROM latest_ts_ga as latest_ts 
            LEFT JOIN prod.distributed_stats_storage_instagram_post as post
            ON latest_ts.external_object_id = post.external_object_id
            AND latest_ts.latest_ts = post.stats_ts
            WHERE DATE(post.stats_date) >= '2017-12-10' 
                and DATE(post.stats_date) <= '2017-12-24')

SELECT * FROM latest_week_metrics_ga"

ga_stats <- query_exec(ga_imp_metrics, project = project, use_legacy_sql = FALSE)
ga_records = ga_stats %>% group_by(external_object_id, stats_dt) %>% summarise(count=n()) %>% filter(count>1)
sapply(ga_stats, function(x) sum(is.na(x)))

# get aggregated stats
ga_summary = ga_stats %>% group_by(stats_dt) %>% summarise(Total_engagement = sum(as.numeric(engagement)), 
                                                           Total_impressions = sum(as.numeric(impressions)), 
                                                           Total_reach = sum(as.numeric(reach)), 
                                                           Total_saved = sum(as.numeric(saved)))

ga_summary = melt(ga_summary, id.vars = c("stats_dt"))

# plot that shows total impressions, reach & engagement
# a two fold increase in impressions but only slight increase in engagement
ggplot(ga_summary, aes(x=stats_dt, y=value, color=variable)) + geom_line(size=1, aes(color=variable)) + my_theme + 
  scale_x_date(date_labels="%b %d",date_breaks  ="1 day") +
  geom_text(data=ga_summary[ga_summary$stats_dt == as.Date("2017-12-15"),], aes(label= variable), vjust = -1, size = 3.5, fontface = "bold") +
  xlab("Stats date") + ylab("Metrics") 


##############################  Metrics by accounts ###########################################

accounts_info_query = 
  "SELECT 
      a.external_id, 
      published, 
      b.display_name, 
      c.name
  FROM `prod.platform_data_api_post` a
  JOIN `prod.platform_data_api_account` b
  ON a.account_id = b.id
  JOIN `prod.platform_data_api_post_type` c
  ON a.post_type_id = c.id
  WHERE a.platform_id = 1 and b.platform_id = 1"

accounts_info = query_exec(accounts_info_query, project = project, use_legacy_sql = FALSE)

# merge accounts with posts
posts_with_accounts = merge(ga_stats, accounts_info, by.y=c("external_id"), by.x=c("external_object_id"), all.y=TRUE)

posts_summary = as.data.frame(posts_with_accounts %>% group_by(stats_dt, display_name) %>% summarise(Total_engagement = sum(as.numeric(engagement)), 
                                                                                     Total_impressions = sum(as.numeric(impressions)), 
                                                                                     Total_reach = sum(as.numeric(reach)), 
                                                                                     Total_saved = sum(as.numeric(saved))))

# Account Tasty - Impressions, reach, engagement & saved
posts_tasty = as.data.frame(posts_summary %>% filter(display_name == "Tasty"))
posts_tasty$start_date = posts_tasty$stats_dt
posts_tasty$end_date = posts_tasty$stats_dt - 1
posts_tasty = melt(posts_tasty, id.vars = c("stats_dt", "display_name"))

ggplot(posts_tasty, aes(x=stats_dt, y=value, color=variable)) + geom_line(size=1, aes(color=variable)) + my_theme + 
  scale_x_date(date_labels="%b %d",date_breaks  ="1 day") +
  geom_text(data=posts_tasty[posts_tasty$stats_dt == as.Date("2017-12-15"),], aes(label= variable), vjust = -1, size = 3.5, fontface = "bold") +
  xlab("Stats date") + ylab("Metrics") 


posts_tasty = merge(posts_tasty, posts_tasty, by.x = c('end_date'), by.y = c('start_date'))
