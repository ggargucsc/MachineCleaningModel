setwd('~/Desktop/Data_validation/')
library(dplyr)
library(ggplot2)
library(bigrquery)
library(reshape2)

"
In this script, compare Graph API BQ (prod.distributed_stats_storage_instagram_post) 
to CrowdTangle BQ (prod.distributed_stats_storage_instagram_media)
"

# theme for plots
my_theme = theme(axis.line = element_line(size=1, colour = "black"),
                 panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
                 panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 15, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma", face = "bold"),
        axis.text.x=element_text(colour="black", size = 15),
        axis.text.y=element_text(colour="black", size = 15))




# Connect to big query
project <- "bf-data-sci" 

################################# GRAPH API DURATION #################################
# Get the dates from Graph API
query_dates_ga <- 
        "SELECT DATE(stats_date) as Date, COUNT(*) as num_ts 
        from `prod.distributed_stats_storage_instagram_post`
        GROUP BY Date
        ORDER BY Date"

ga_dates <- query_exec(query_dates_ga, project = project, use_legacy_sql = FALSE)
# less timestamps during the week of Nov. 16 - 20 

ggplot(ga_dates, aes(x=Date, y=num_ts)) + geom_bar(stat="identity") + my_theme +
  xlab("Stats date") + ylab("Num. of timestamps")


################################# MISSING ACCOUNTS ###################################
# get account info
accounts_info_query = 
  "SELECT a.external_id, published, b.display_name FROM `prod.platform_data_api_post` a
   LEFT JOIN `prod.platform_data_api_account` b
   ON a.account_id = b.id
   WHERE a.platform_id = 1 and b.platform_id = 1"

accounts_info = query_exec(accounts_info_query, project = project, use_legacy_sql = FALSE)

# get crowd tangle posts (make sure duration is same as of Graph API)
ct_data_query = 
  "SELECT external_object_id FROM `prod.distributed_stats_storage_instagram_media`
    WHERE DATE(stats_date) >= (SELECT MIN(DATE(stats_date)) FROM `prod.distributed_stats_storage_instagram_post`)
    AND DATE(stats_date) <= (SELECT MAX(DATE(stats_date)) FROM `prod.distributed_stats_storage_instagram_post`)
    GROUP BY external_object_id"

ct_data = query_exec(ct_data_query, project = project, use_legacy_sql = FALSE)

# get graph api posts
ga_data_query = 
  "SELECT external_object_id FROM `prod.distributed_stats_storage_instagram_post`
   GROUP BY external_object_id"

ga_data = query_exec(ga_data_query, project = project, use_legacy_sql = FALSE)

# merge all data
posts_by_accounts_ga = merge(ga_data, accounts_info[,c("external_id", "display_name")], 
                             by.x=c("external_object_id"), by.y=c("external_id"), all.x=TRUE)
posts_by_accounts_ct = merge(ct_data, accounts_info[,c("external_id", "display_name")], 
                             by.x=c("external_object_id"), by.y=c("external_id"), all.x=TRUE)

# count posts by account
count_posts_by_acct_ga = posts_by_accounts_ga %>% group_by(display_name) %>% summarise(count_posts = n())
count_posts_by_acct_ct = posts_by_accounts_ct %>% group_by(display_name) %>% summarise(count_posts = n())
count_posts_by_acct = merge(count_posts_by_acct_ga, count_posts_by_acct_ct, by=c("display_name"), all.y=TRUE)
names(count_posts_by_acct) = c("display_name", "Graph API", "Crowd Tangle")

# reshape data
count_posts_by_acct = melt(count_posts_by_acct, id.var="display_name")

# plot showing the accounts missing grom Graph API
ggplot(count_posts_by_acct, aes(x=display_name, y=value, fill=variable))  + geom_bar(stat="identity") +
  coord_flip() + my_theme + ylab("Total unique posts") +
  xlab("Accounts") +
  geom_text(aes(label=value), position = position_stack(vjust = 0.5), size = 3, fontface = "bold") + 
  scale_fill_manual(labels = c("CrowdTangle", "Graph API"), values = c("#7570B3", "red")) 
ggsave("total unique posts per account_new.png")

###################################### MISSING POSTS -- FOR DIFFERENT ACCOUNTS #############################
posts_by_accounts_ga$post_type = 1
posts_by_accounts_ct$post_type = 1
all_posts = merge(posts_by_accounts_ga, posts_by_accounts_ct, by=c("external_object_id", "display_name"), 
                  all.x=TRUE, all.y = TRUE)
names(all_posts) = c("external_object_id", "display_name", "post_ga", "post_ct")

# convert na to 0
all_posts[is.na(all_posts)] = 0

# remove posts that are not linked to any aacount
all_posts = all_posts[!all_posts$display_name == "0",]
all_posts = all_posts %>% mutate(status = ifelse(post_ga > 0 & post_ct == 0 ,"Posts not in CT", 
                                                 ifelse(post_ga == 0 & post_ct > 0, "Posts not in Graph API", 
                                                        "Posts matched")))

all_posts = all_posts[, c("external_object_id", "display_name", "status")]
all_posts = all_posts[!duplicated(all_posts),]
posts_per_acct = all_posts %>% group_by(display_name) %>% summarise(total_posts = n())
posts_per_acct_per_status = all_posts %>% group_by(display_name, status) %>% summarise(count_posts = n())
missing_posts = merge(posts_per_acct_per_status, posts_per_acct, by=c("display_name"))
missing_posts$perc = round((missing_posts$count_posts/missing_posts$total_posts)*100, 0)

# filter by accounts that are in Graph api BQ
accounts_ga = unique(posts_by_accounts_ga$display_name)
missing_posts = missing_posts[missing_posts$display_name %in% accounts_ga, ]
ggplot(missing_posts, aes(x=display_name, y=count_posts, fill=status)) + geom_bar(stat="identity") + 
  coord_flip() + ylab("Total unique posts") + xlab("Accounts") + my_theme +
  geom_text(aes(label=paste(perc, "%", sep="")), position = position_stack(vjust = 0.5), size = 3.5, fontface = "bold" )
ggsave("Posts_status.png")

#write.csv(all_posts, file="missing_posts.csv")
###################################### MISSING POSTS - TASTY JAPAN #########################################
tasty_japan_posts = all_posts[all_posts$display_name == "Tasty Japan", ]

# get published date for these posts
tasty_japan_posts = merge(tasty_japan_posts, accounts_info, by.x = c("external_object_id", "display_name"), 
                          by.y=c("external_id", "display_name"), 
                          all.x=TRUE)

###################################### MISSING POSTS -- STATS ON DAILY BASIS ###############################
# get published date 
accounts_info$published = as.POSIXct(as.numeric(accounts_info$published), origin = '1970-01-01', tz = 'UTC')
accounts_info$published = as.Date(accounts_info$published, format="%Y-%m-%d %H:%M:%S")
accounts_info = accounts_info[!duplicated(accounts_info),]


# get latest stats from Graph API BQ
latest_stats_ga_query = "
WITH latest_ts_ga AS (
          SELECT
              external_object_id,
              MAX(stats_ts) as latest_ts,
              DATE(stats_date) as stats_date
          FROM prod.distributed_stats_storage_instagram_post 
          GROUP BY DATE(stats_date), external_object_id),
    latest_month_metrics_ga AS (
          SELECT
              latest_ts.external_object_id,
              post.engagement - post.saved as metrics_to_compare,
              latest_ts.stats_date as stats_dt
          FROM latest_ts_ga as latest_ts 
          LEFT JOIN prod.distributed_stats_storage_instagram_post as post
          ON latest_ts.external_object_id = post.external_object_id
          AND latest_ts.latest_ts = post.stats_ts)

SELECT * FROM latest_month_metrics_ga"


latest_stats_ga = query_exec(latest_stats_ga_query, project = project, use_legacy_sql = FALSE, max_pages = Inf)

# get latest stats from CT BQ
latest_stats_ct_query = "
WITH latest_ts_ct AS (
          SELECT
              external_object_id,
              MAX(stats_ts) as latest_ts,
              DATE(stats_date) as stats_date
          FROM prod.distributed_stats_storage_instagram_media 
          WHERE DATE(stats_date) >= '2017-11-16' and DATE(stats_date) <= '2017-12-12'
          GROUP BY DATE(stats_date), external_object_id),
      latest_month_metrics_ct AS (
          SELECT
              latest_ts.external_object_id,
              post.likes + post.comments as metrics_to_compare,
              latest_ts.stats_date as stats_dt
          FROM latest_ts_ct as latest_ts 
          LEFT JOIN prod.distributed_stats_storage_instagram_media as post
          ON latest_ts.external_object_id = post.external_object_id
          AND latest_ts.latest_ts = post.stats_ts
          WHERE DATE(post.stats_date) >= '2017-11-16' and DATE(post.stats_date) <= '2017-12-12')

SELECT * FROM latest_month_metrics_ct"


latest_stats_ct = query_exec(latest_stats_ct_query, project = project, use_legacy_sql = FALSE, max_pages = Inf)


# get all the posts published on Nov - 16th
account_info_pub_15nov = accounts_info[accounts_info$published == "2017-11-15", ]
account_info_pub_15nov = account_info_pub_15nov %>% group_by(display_name) %>% summarise(count_posts = n())
posts_pub_15nov = accounts_info[accounts_info$published == "2017-11-15", ]$external_id

# get only those posts that were published on Nov - 15
posts_pub_15nov_ga = latest_stats_ga[latest_stats_ga$external_object_id %in% posts_pub_15nov,]
posts_pub_15nov_ct = latest_stats_ct[latest_stats_ct$external_object_id %in% posts_pub_15nov, ]


posts_pub_15nov = merge(posts_pub_15nov_ga, posts_pub_15nov_ct, by=c("external_object_id", "stats_dt"), all.x = TRUE, all.y=TRUE)
posts_pub_15nov = melt(posts_pub_15nov, id.vars = c("external_object_id", "stats_dt"))
levels(posts_pub_15nov$variable) = c("Graph API", "CrowdTangle")
write.csv(posts_pub_15nov, file="posts_pub_15nov.csv")
# plot to get daily stats
ggplot(posts_pub_15nov, aes(x=stats_dt, y=value, group=interaction(external_object_id, variable))) +
  geom_line(size=1, aes(linetype = variable, color=variable)) + 
  scale_linetype_manual(values = c("Graph API" = "solid", "CrowdTangle" = "dotted")) + 
  theme(legend.position="top") + geom_point(aes(color=variable)) +
  geom_vline(xintercept = as.numeric(posts_pub_15nov$stats_dt[1])) +
  geom_vline(xintercept = as.numeric(posts_pub_15nov$stats_dt[5])) +
  xlab("Stats date") +
  ylab("Likes + Comments from CT compared to Engagement - saved from Graph API") + 
  #scale_x_date(date_labels="%b %d",date_breaks  ="1 day") + 
  my_theme +
  theme(legend.title = element_blank())
#ggsave("posts_missing_stats_pub_date_15_nov.png")

# posts published on Nov - 15 that are not in Graph API but in CT 
posts_pub_15nov[is.na(posts_pub_15nov)] = 0
posts_pub_15nov$have_stats = ifelse(posts_pub_15nov$value == 0, 0, 1)
posts_pub_15nov = posts_pub_15nov[, c("external_object_id", "variable", "have_stats")]
posts_pub_15nov = posts_pub_15nov[!duplicated(posts_pub_15nov),]

# get those posts that do not exist in Graph API BQ
posts_in_ga = posts_pub_15nov[posts_pub_15nov$variable == "Graph API" & posts_pub_15nov$have_stats == 1, ]$external_object_id
posts_missing_in_ga = posts_pub_15nov[posts_pub_15nov$variable == "Graph API" & !posts_pub_15nov$external_object_id %in% posts_in_ga, ]
posts_missing_in_ga = merge(posts_missing_in_ga, accounts_info, by.x = c("external_object_id"), by.y = c("external_id"), all.x=TRUE)
write.csv(posts_missing_in_ga, file="posts_missing_in_ga_15nov.csv")









  
  
  
  