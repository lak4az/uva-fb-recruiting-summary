#247 Data Scraper

#load necessary packages
library(rvest) #for all scraping
library(stringr) #to remove spaces
library(reshape2) #colsplit
library(dplyr) #arrange
library(ggplot2)

#vector for each year
year <- seq(2002,2019)
df_all <- data.frame(year_class_rank_data=numeric(0),
                     national_class_rank_data=numeric(0),
                     conference_class_rank_data=numeric(0),
                     name_data=character(0), 
                     position_data=character(0),
                     height_in_data=numeric(0),
                     weight_data=numeric(0),
                     school_data=character(0),
                     city_data=character(0),
                     state_data=character(0),
                     rating_data=numeric(0),
                     star_rating_data=numeric(0),
                     nat_rank_data=numeric(0),
                     pos_rank_data=numeric(0),
                     state_rank_data=numeric(0))
for (i in year) {
#Specifying the url for desired website to be scraped
url <- paste("https://247sports.com/college/virginia/Season/",i,"-Football/Commits/", sep="")

#Reading the HTML code from the website
webpage <- read_html(url)

#Recruiting Class National & Conference Ranking
#Using CSS selectors to scrape the section, convert html data to text, convert to numeric
class_rank_data_html <- html_nodes(webpage,"a.ir-bar__number , .teams+ .tltp_bm")
class_rank_data <- html_text(class_rank_data_html) %>% trimws() %>% as.numeric()
#split into national and conference rank, convert into dataframe
year_class_rank_data <- class_rank_data[1]
national_class_rank_data <- class_rank_data[2]
conference_class_rank_data <- class_rank_data[3]
recruit_class_ranks <- cbind(year_class_rank_data, national_class_rank_data, 
                             conference_class_rank_data) %>% as.data.frame()
#

#Player Name
#Using CSS selectors to scrape the section, convert html data to text
name_data_html <- html_nodes(webpage, ".ri-page__name-link")
name_data <- html_text(name_data_html)
#

#Player Position
#Using CSS selectors to scrape the section, convert html data to text, get rid of spaces
position_data_html <- html_nodes(webpage, ".position")
position_data <- html_text(position_data_html) %>% trimws()
#

#Player Height/Weight
#Using CSS selectors to scrape the section, convert html data to text
height_weight_data_html <- html_nodes(webpage, ".metrics")
height_weight_data <- html_text(height_weight_data_html) %>% trimws()
#

#Player School/City
#Using CSS selectors to scrape the section, convert html data to text
school_city_data_html <- html_nodes(webpage, ".recruit .meta")
school_city_data <- html_text(school_city_data_html) %>% trimws()

#Player Rating
#Using CSS selectors to scrape the section, convert html data to text, convert to numeric
rating_data_html <- html_nodes(webpage, ".ri-page__star-and-score .score")
rating_data <- html_text(rating_data_html) %>% as.numeric()
#

#Star Rating
star_rating_data <- NULL
rating_data[is.na(rating_data)] <- 0
for (i in 1:length(rating_data)) {
  if(rating_data[i]>=.9833){
    star_rating_data[i] <- 5
  }
  if(rating_data[i]>=.8900 && rating_data[i]<.9833){
    star_rating_data[i] <- 4
  }
  if(rating_data[i]>=.7970 && rating_data[i]<.8900){
    star_rating_data[i] <- 3
  }
  if(rating_data[i]>=.7000 && rating_data[i]<.7970){
    star_rating_data[i] <- 2
  }
  if(rating_data[i]>0 && rating_data[i]<.7000){
    star_rating_data[i] <- 1
  }
  if(rating_data[i]==0){
    star_rating_data[i] <- 0
  }
}
#

#Player National Overall Rank
#Using CSS selectors to scrape the section, convert html data to text, convert to numeric
nat_rank_data_html <- html_nodes(webpage, ".natrank")
nat_rank_data <- html_text(nat_rank_data_html) %>% as.numeric()
nat_rank_data[is.na(nat_rank_data)] <- 0
#

#Player National Position Rank
#Using CSS selectors to scrape the section, convert html data to text, convert to numeric
pos_rank_data_html <- html_nodes(webpage,".posrank")
pos_rank_data <- html_text(pos_rank_data_html) %>% as.numeric()
pos_rank_data[is.na(pos_rank_data)] <- 0
#

#Player State Rank
#Using CSS selectors to scrape the section, convert html data to text, convert to numeric
state_rank_data_html <- html_nodes(webpage,".sttrank")
state_rank_data <- html_text(state_rank_data_html) %>% as.numeric()
state_rank_data[is.na(state_rank_data)] <- 0
#

#combine player vectors into dataframe

recruit_class_players <- cbind(name_data, position_data, height_weight_data, school_city_data, 
                       rating_data, star_rating_data, nat_rank_data, pos_rank_data, state_rank_data)%>%
  as.data.frame()

##height weight cleaning
#split height and weight into separate columns and save as new dataframe
height_weight <- colsplit(recruit_class_players$height_weight_data, " / ", c("height", "weight"))

#separate height from weight
height_data <- height_weight$height

#separate height into feet/inches and create vector of total height in inches
height_data <- colsplit(height_data, "-", c("feet", "inches"))
height_in_data <- height_data$feet*12 + height_data$inches

#separate weight data
weight_data <- height_weight$weight

#add height and weight back into dataframe
recruit_class_players <- cbind(recruit_class_players, height_in_data, weight_data)

#remove original height/weight vector, reorder new height and weight vectors to that position
recruit_class_players$height_weight_data <- NULL
recruit_class_players <- recruit_class_players[,c(1:2, 9:10, 3:8)]
##

##school city cleaning
#split school and city into separate columns and save as new dataframe
school_city <- colsplit(recruit_class_players$school_city_data, " [(]", c("school", "city"))

#separate school data
school_data <- school_city$school

#split city and state into separate columns and save as new dataframe
city_state <- colsplit(school_city$city, ", ", c("city", "state"))

#separate city data
city_data <- city_state$city

#separate state data and only display state abbreviation
state_data <- strtrim(city_state$state, 2)

#add school, city, and state back into dataframe
recruit_class_players <- cbind(recruit_class_players, school_data, city_data, state_data)

#remove original school/city vector and reorder new school, city, state vectors into that position
recruit_class_players$school_city_data <- NULL
recruit_class_players <- recruit_class_players[,c(1:4, 10:12, 5:9)]
##

#sort by composite rating
recruit_class_players <- arrange(recruit_class_players, desc(rating_data))

#combine recruit and class data and save as new df
d <- recruit_class_ranks
n <- length(recruit_class_players$name_data)
recruit_class_ranks <- do.call("rbind", replicate(n, d, simplify = FALSE))
recruit_class_df <- cbind(recruit_class_ranks,recruit_class_players)

#combine df for each year
df_all <- rbind(df_all,recruit_class_df)
}

#edit unique columns
df_all$school_data <- df_all$school_data %>% as.character()
df_all$city_data <- df_all$city_data %>% as.character()
df_all$state_data <- df_all$state_data %>% as.character()
for (i in 1:length(df_all$city_data)) {
  if(df_all$name_data[i]=="Malcolm Cook"){
    df_all$school_data[i] <- "Fork Union Military HS"
    df_all$city_data[i] <- "Fork Union"
  }
  if(df_all$name_data[i]=="Brent Urban" | df_all$name_data[i]=="Trent Corney"){
    df_all$city_data[i] <- "Ontario"
    df_all$state_data[i] <- "CAN"
  }
  if(df_all$city_data[i]=="Germany"){
    df_all$state_data[i] <- "GER"
  }
}

#vector for footprint area
footprint_data <- NULL
area1 <- c("NY","NJ","PA","DE","MD","WV","OH","KY","VA","TN","NC","SC","DC")
area2 <- c("CT","RI","MA","MI","IN","AL","GA","FL")
for(i in 1:length(df_all$state_data)) {
  if(df_all$state_data[i] %in% area1){
    footprint_data[i] <- 1
  }
  if(df_all$state_data[i] %in% area2){
    footprint_data[i] <- 2
  }
  if(!df_all$state_data[i] %in% area1 && !df_all$state_data[i] %in% area2){
    footprint_data[i] <- 3
  }
}
df_all$footprint_data <- footprint_data

df_all <- df_all[,c(1:10, 16, 11:15)]

#remove unranked players
df_all <- df_all[!(df_all$star_rating_data == 0),]

#rename columns
names(df_all) <- c("Year", "National Class Rank", "Conference Class Rank", "Name", "Position",
                   "Height", "Weight", "School", "City", "State", "Footprint", "Composite Rating", 
                   "Star_Rating", "National Recruit Rank", "Position Recruit Rank", 
                   "State Recruit Rank")
#######################################################################################################
#write csv
# write.csv(df_all,"C:/Users/Student/Downloads/UVA_Recruiting_Classes.csv",row.names = F)
#######################################################################################################
#Read csv
# df_all <- read.csv("C:/Users/Student/Downloads/UVA_Recruiting_Classes.csv")
#remove NA from new vectors
# df_all$All.ACC[is.na(df_all$All.ACC)] <- 0
# df_all$All.American[is.na(df_all$All.American)] <- 0
#write csv
write.csv(df_all,"C:/Users/Student/Downloads/UVA_Recruiting_Classes.csv",row.names = F)
