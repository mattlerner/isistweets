library(rstanarm)
library(dplyr)
library(ggplot2)
library(rgdal)
library(tidyr)
library(maptools)
gpclibPermit()
library(GGally)

setwd("/users/matt/desktop/qmss/data science and public policy/isis")

load("world_data.rdata")

colnames((world_data))

# create a variable we vitally need
world_data$log_isis_following <- log(world_data$num_isis_following + 1)

# we fit it on 100,000 observations
sample_size <- 100000
train_sample_indices <- sample(1:nrow(world_data), size=sample_size)
test_sample_indices <- sample(1:nrow(world_data[-train_sample_indices,]), size=sample_size)
train_sample_ <- world_data[train_sample_indices,]
test_sample_ <- world_data[test_sample_indices,]

# fit generalized linear model
model_ <- glm(activist ~ log_isis_following + syrian_war, data=train_sample_, family=binomial(link="logit"))

# predict model
predictions <- predict(model_, test_sample_, type="response")
predictions_frame <- data.frame(predictions=predictions, log_predictions = log(predictions))

# density of predictions
ggplot(predictions_frame, aes(x=log_predictions)) + geom_density(adjust = 5, alpha=0.1) + theme_classic() + xlab("Log of predicted probability") + ylab("Density") + geom_vline(xintercept=log(0.5), colour="red") + geom_vline(xintercept=log(0.21), colour="blue") + guides(colour=FALSE) + ggtitle("Distribution of predicted probabilities")

test_sample_$predictions <- predictions

for (i in seq(0.5,0,-0.001)) {
  test_sample_$assigned_activist <- as.numeric(test_sample_$predictions > i)
  
  # portion correctly predicted by decision rule
  portion_correctly_predicted <- round(sum(test_sample_$assigned_activist == test_sample_$activist) / nrow(test_sample_), 4)
  portion_false_positives <- round(sum(test_sample_$assigned_activist == 1 & test_sample_$activist == 0) / nrow(test_sample_), 4)
  portion_false_negatives <- round(sum(test_sample_$assigned_activist == 0 & test_sample_$activist == 1) / nrow(test_sample_), 4)
  
  current_difference <- portion_false_negatives - portion_false_positives

  if (current_difference < 0) {
    print(paste("Scoring threshold:",i))
    print(paste("Accuracy:",portion_correctly_predicted))
    print(paste("False positives:",portion_false_positives))
    print(paste("False negatives:",portion_false_negatives))
    break
  }
}

print(summary(model_1), digits=2)
n_parcoord_sample = 1000
parcoord_sample = rbind(sample_n(world_data[world_data$activist==1,], n_parcoord_sample), sample_n(world_data[world_data$activist==0,], n_parcoord_sample))
ggparcoord(parcoord_sample, columns = 8:13, scale = 'uniminmax', groupColumn = 'activist', mapping=aes(color=as.factor(activist))) + scale_colour_manual(values = c("0" = "#619cff", "1" = "#f8766d"), guide=FALSE)

########## Plots ##########
data <- world_data
data$log_isis_following <- log(data$num_isis_following)

# top 20 countries where people tweet from
bycountry <- data %>% group_by(ADMIN) %>% summarise(num = n()) %>% arrange(desc(num))
top10counties <- bycountry %>% top_n(10)
top10countries_vec <- top10countries$ADMIN

# separating by activist
bycountry_activist <- data %>% subset(activist == 1) %>% group_by(ADMIN) %>% summarise(activists = n()) %>% arrange(desc(activists))
bycountry_noactivist <- data %>% subset(activist == 0) %>% group_by(ADMIN) %>% summarise(nonactivists = n()) %>% arrange(desc(nonactivists))


# what percent of users are from the top 10 countries?
sum(top10countries$num) / nrow(data) # 75% r.n.

activist_by_country <- data %>% subset(ADMIN %in% top10countries_vec) %>% group_by(activist) %>% mutate(users_in_category = n()) %>% group_by(activist, ADMIN) %>% summarise(users_category_country = n(), users_in_category=max(users_in_category)) %>% mutate(percent_users = round(users_category_country/users_in_category, 2))
activist_by_country$activist <- as.factor(activist_by_country$activist)
activist_by_country <- activist_by_country %>% group_by(ADMIN) %>% mutate(users_in_country = sum(users_category_country))

##### Activist by country

ggplot(activist_by_country) + aes(x = reorder(ADMIN,users_in_country), y = percent_users, fill=activist) + geom_bar(stat="identity", position=position_dodge()) + coord_flip() + ggtitle("Proportion of ISIS activists by country\nin descending order of total users in sample") + theme_bw() + xlab("") + ylab("Portion of users in category") + theme(legend.title=element_blank()) + scale_fill_discrete(labels=c("Activist","Non-activist"))

##### Suspended by country

suspended_by_country <- data %>% subset(ADMIN %in% top10countries_vec) %>% group_by(suspended) %>% mutate(users_in_category = n()) %>% group_by(suspended, ADMIN) %>% summarise(users_category_country = n(), users_in_category=max(users_in_category)) %>% mutate(percent_users = round(users_category_country/users_in_category, 2))
suspended_by_country$suspended <- as.factor(suspended_by_country$suspended)
suspended_by_country <- suspended_by_country %>% group_by(ADMIN) %>% mutate(users_in_country = sum(users_category_country))

ggplot(suspended_by_country) + aes(x = reorder(ADMIN,users_in_country), y = percent_users, fill=suspended) + geom_bar(stat="identity", position=position_dodge()) + coord_flip() + ggtitle("Proportion of suspended users by country\nin descending order of total users in sample") + theme_bw() + xlab("") + ylab("Portion of users in category") + theme(legend.title=element_blank()) + scale_fill_discrete(labels=c("Suspended","Active"))

##### Chloropleth

# download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
# system("unzip world_shape_file.zip")

world_spdf <- readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")
world_spdf@data$id <- rownames(world_spdf@data)
fortified <- fortify(world_spdf, region="id") %>% mutate(order = row_number())
full_map_data <- merge(fortified, world_spdf@data, by="id") %>% arrange(order)
complete_map <- merge(full_map_data, bycountry_activist, by.x = "NAME", by.y="ADMIN", all.x=TRUE) %>% arrange(order)
complete_map[is.na(complete_map$activists), 'activists'] <- 0

activists_only <- data %>% subset(activist == 1)
nonactivists_only <- data %>% subset(activist == 0)

# non activists
map <- ggplot(data = complete_map, aes(x = long, y = lat, group = group))
#map + geom_polygon(fill="white", colour="#cccccc") + coord_fixed(1.3) + guides(fill = FALSE) + geom_point(data=nonactivists_only, aes(long, lat), color="blue", size=0.3, inherit.aes=FALSE) + theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank()) + xlab("") + ylab("")

# activists
#map <- ggplot(data = complete_map, aes(x = long, y = lat, group = group))
#map + geom_polygon(fill="white", colour="#cccccc") + coord_fixed(1.3) + guides(fill = FALSE) + geom_point(data=activists_only, aes(long, lat), color="red", size=0.7, inherit.aes=FALSE) + theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank()) + xlab("") + ylab("")

# both together
map + geom_polygon(fill="white", colour="#cccccc") + coord_fixed(1.3) + guides(fill = FALSE) + geom_point(data=nonactivists_only, aes(long, lat), color="blue", size=0.3, inherit.aes=FALSE) + geom_point(data=activists_only, aes(long, lat), color="red", size=0.3, inherit.aes=FALSE) + theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank()) + xlab("") + ylab("")

##### Density Inspecting num_isis_following
activists_only$log_isis_following <- log(activists_only$num_isis_following)
nonactivists_only$log_isis_following <- log(nonactivists_only$num_isis_following)

# plot logs
ggplot(data, aes(x=log_isis_following, color=as.factor(activist), fill=as.factor(activist))) + geom_density(adjust = 10, alpha=0.1) + theme_classic() + scale_color_manual(values=c("0"="#28b9ad","1"="#f699c9")) + scale_fill_manual(values=c("0"="#28b9ad","1"="#f699c9"), labels=c("Nonactivist","Activist"), name=NULL) + xlab("Log of ISIS following") + ylab("Density") + guides(colour=FALSE)
