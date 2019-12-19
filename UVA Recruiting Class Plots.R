library(dplyr) #arrange
library(ggplot2) #plots
library(openintro)
library(reshape2)
library(fiftystater)
#######################################################################################################
#Creating summary plots
#
#######################################################################################################
#Read csv
df_all <- read.csv("C:/Users/Student/Desktop/UVA Football Recruiting/UVA Recruiting Class Analysis/UVA_Recruiting_Classes.csv")

##Unique Star Rating Plot
#create dataframe
star_summary<- table(df_all$Year) %>% as.data.frame()
star_summary$five_star <- NA
star_summary$four_star <- NA
star_summary$three_star <- NA
star_summary$two_star <- NA

for (i in unique(df_all$Year)) {
  star_table <- table(df_all[["Star_Rating"]][df_all$Year==i]) %>% as.data.frame()
  star_summary[["five_star"]][star_summary$Var1==i] <- ifelse(5 %in% star_table$Var1,
                                                              star_table[["Freq"]][star_table$Var1==5],
                                                              0)
  star_summary[["four_star"]][star_summary$Var1==i] <- ifelse(4 %in% star_table$Var1,
                                                              star_table[["Freq"]][star_table$Var1==4],
                                                              0)
  star_summary[["three_star"]][star_summary$Var1==i] <- ifelse(3 %in% star_table$Var1,
                                                              star_table[["Freq"]][star_table$Var1==3],
                                                              0)
  star_summary[["two_star"]][star_summary$Var1==i] <- ifelse(2 %in% star_table$Var1,
                                                              star_table[["Freq"]][star_table$Var1==2],
                                                              0)
}

#make sure it is numeric
star_summary[] <- lapply(star_summary, as.numeric)
#add names
names(star_summary) <- c("year", "total", "five_star", "four_star", "three_star", "two_star")

#melt it
melted_star_summary <- melt(star_summary,id="year")
melted_star_summary$year <- melted_star_summary$year + 2001 
#add 2001 b/c as.numeric turned years into numeric variables 1-18

#create plot of all years
melted_star_summary %>% ggplot(aes(x=year, y=value, colour=variable, shape=variable)) + geom_line() + 
  geom_point() + scale_x_continuous(breaks=seq(2002,2019)) + scale_y_continuous(breaks=seq(0,30,5)) +
  theme(axis.text.x=element_text(angle=90,vjust=.5)) +
  labs(title="Recruiting Class Star Caliber Count, by Year", x="Year", y="Number of Recruits",
       colour="Key", shape="Key")
#create plot of Bronco years
melted_star_summary %>% ggplot(aes(x=year, y=value, colour=variable, shape=variable)) + geom_line() + 
  geom_point() + scale_x_continuous(breaks=seq(2002,2019)) + scale_y_continuous(breaks=seq(0,30,5)) +
  theme(axis.text.x=element_text(angle=90,vjust=.5)) +
  labs(title="Recruiting Class Star Caliber Count, by Year", subtitle="(Under Coach Mendenhall)",
       x="Year", y="Number of Recruits",colour="Key", shape="Key") + xlim(2016,2019)
##

##Footpint plot
#create dataframe
fp_df <- df_all[,c("Year","Footprint")]
fp_table <- table(fp_df) %>% as.data.frame()
fp_one <- fp_table[fp_table$Footprint==1,]
names(fp_one) <- c("year", "fp", "one_count")
fp_two <- fp_table[fp_table$Footprint==2,]
names(fp_two) <- c("year", "fp", "two_count")
fp_three <- fp_table[fp_table$Footprint==3,]
names(fp_three) <- c("year", "fp", "three_count")
fp12 <- left_join(fp_one,fp_two, by="year")
fp_all <- left_join(fp12, fp_three, by="year")
fp_all <- fp_all[,-c(2,4,6)]
fp_all$total <- rowSums(fp_all[,-1])
fp_all <- fp_all[,c(1,5,2:4)]

#make sure it is numeric
fp_all[] <- lapply(fp_all, as.numeric)

#melt it
melted_fp_all <- melt(fp_all, id="year")
melted_fp_all$year <- melted_fp_all$year + 2001
#add 2001 b/c as.numeric turned years into numeric variables 1-18

#create plot of all years
melted_fp_all %>% ggplot(aes(x=year, y=value, colour=variable, shape=variable)) + geom_line() + 
  geom_point() + scale_x_continuous(breaks=seq(2002,2019)) + scale_y_continuous(breaks=seq(0,30,5)) +
  theme(axis.text.x=element_text(angle=90,vjust=.5)) + 
  labs(title="Recruiting Class Footprint Count, by Year", x="Year", y="Number of Recruits",
       colour="Key", shape="Key")
#create plot of Bronco years
melted_fp_all %>% ggplot(aes(x=year, y=value, colour=variable, shape=variable)) + geom_line() + 
  geom_point() + scale_x_continuous(breaks=seq(2002,2019)) + scale_y_continuous(breaks=seq(0,30,5)) +
  theme(axis.text.x=element_text(angle=90,vjust=.5)) + 
  labs(title="Recruiting Class Footprint Count, by Year",subtitle="(Under Coach Mendenhall)",
       x="Year", y="Number of Recruits",colour="Key", shape="Key") + xlim(2016,2019)
##

##State plot
#plot of all years with all conference/amer
rct_all <- df_all %>% group_by(State) %>% 
  summarize(count=n(),all.acc=sum(All.ACC),all.amer=sum(All.American)) %>% arrange(desc(count))
rct_all$all.acc[rct_all$all.acc==0]<-NA
rct_all$all.amer[rct_all$all.amer==0]<-NA
melted_rct_all <- melt(rct_all,id='State')
melted_rct_all %>% ggplot(aes(x=State,fill=variable,color=variable,alpha=variable)) +
  geom_bar(data=subset(melted_rct_all,variable=='count'),
           aes(x=reorder(State,-value),y=value),stat='identity') +
  geom_bar(data=subset(melted_rct_all,variable=='all.acc'),aes(y=value),stat='identity') +
  geom_bar(data=subset(melted_rct_all,variable=='all.amer'),aes(y=value),stat='identity') +
  scale_fill_manual(labels=c('All ACC','All American','Total Recruits'),
                    values=c('darkorange1','white','dodgerblue4')) +
  scale_color_manual(labels=c('All ACC','All American','Total Recruits'),
                    values=c('darkorange4','black','navy')) +
  scale_alpha_manual(labels=c('All ACC','All American','Total Recruits'),
                    values=c(1,1,.4)) +
  geom_text(data=subset(melted_rct_all,variable=='count'),aes(y=value+18,label=value),alpha=1) +
  geom_text(data=subset(melted_rct_all,variable=='all.acc'),aes(y=value+7,label=value),alpha=1) +
  geom_text(data=subset(melted_rct_all,variable=='all.amer' & State=='VA'),
            aes(y=value-10,label=value),alpha=1) +
  geom_text(data=subset(melted_rct_all,variable=='all.amer' & State!='VA'),
            aes(y=value-7,label=value),alpha=1) +
  labs(title='Total Recruits, All-ACC, and All-Americans, by State',subtitle='(All Years)',
       x='State',y='Total') + theme(axis.text.x=element_text(angle=90,vjust=.5,hjust=1))
#plot of all years
df_all %>% group_by(State) %>% summarize(count=n()) %>%
  arrange(desc(count)) %>% 
  ggplot(aes(x=reorder(State,-count),y=count)) + geom_bar(stat="identity") +
  geom_text(aes(y=count+10,label=count)) + 
  labs(title="Number of Recruits, by State",subtitle="(All Years)", x="State", y="Number of Recruits")
#choropleth of all years
data("fifty_states")
state_df <- df_all %>% group_by(State) %>% summarize(count=n()) %>% arrange(desc(count))
state_df$State <- abbr2state(state_df$State) %>% tolower()
fifty_states_merge <- left_join(fifty_states,state_df, by=c("id"="State"))
fifty_states_merge <- fifty_states_merge[fifty_states_merge$id!="virginia",]
fifty_states_merge %>%  ggplot(aes(map_id = id)) + 
  geom_map(aes(fill = count), map = fifty_states_merge) + 
  expand_limits(x = fifty_states_merge$long, y = fifty_states_merge$lat) + coord_map() + 
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) + labs(x = "", y = "") + 
  theme(legend.position = "right", panel.background = element_blank()) + 
  fifty_states_inset_boxes() +
  geom_polygon(data = fifty_states_merge, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  scale_fill_continuous(low="lightblue", high="midnightblue", guide="colorbar") + 
  labs(title= "Recruit Map", subtitle="(All Years)",fill="Number of Recruits")

#plot of bronco years with all conference/amer
df_bronco <- df_all[df_all$Year>=2016,]
rct_all <- df_bronco %>% group_by(State) %>% 
  summarize(count=n(),all.acc=sum(All.ACC),all.amer=sum(All.American)) %>% arrange(desc(count))
rct_all$all.acc[rct_all$all.acc==0]<-NA
rct_all$all.amer[rct_all$all.amer==0]<-NA
melted_rct_all <- melt(rct_all,id='State')
melted_rct_all %>% ggplot(aes(x=State,fill=variable,color=variable,alpha=variable)) +
  geom_bar(data=subset(melted_rct_all,variable=='count'),
           aes(x=reorder(State,-value),y=value),stat='identity') +
  geom_bar(data=subset(melted_rct_all,variable=='all.acc'),aes(y=value),stat='identity') +
  geom_bar(data=subset(melted_rct_all,variable=='all.amer'),aes(y=value),stat='identity') +
  scale_fill_manual(labels=c('All ACC','Total Recruits'),
                    values=c('darkorange1','dodgerblue4')) +
  scale_color_manual(labels=c('All ACC','Total Recruits'),
                    values=c('darkorange4','navy')) +
  scale_alpha_manual(labels=c('All ACC','Total Recruits'),
                    values=c(1,.4)) +
  geom_text(data=subset(melted_rct_all,variable=='count'),aes(y=value+2,label=value),alpha=1) +
  geom_text(data=subset(melted_rct_all,variable=='all.acc'),aes(y=value+1,label=value),alpha=1) +
  labs(title='Total Recruits, All-ACC, and All-Americans, by State',
       subtitle='(Under Coach Mendenhall)',x='State',y='Total') + 
  theme(axis.text.x=element_text(angle=90,vjust=.5,hjust=1))
#plot of bronco years
df_bronco <- df_all[df_all$Year>=2016,]
df_bronco %>% group_by(State) %>% summarize(count=n()) %>% arrange(desc(count)) %>% 
  ggplot(aes(x=reorder(State,-count),y=count)) + geom_bar(stat="identity") + 
  geom_text(aes(y=count+.7,label=count)) +
  labs(title="Number of Recruits, by State",subtitle="(Under Coach Mendenhall)", x="State",
       y="Number of Recruits")
#choropleth of bronco years
data("fifty_states")
bronco_state_df <- df_bronco %>% group_by(State) %>% summarize(count=n()) %>% arrange(desc(count))
bronco_state_df$State <- abbr2state(bronco_state_df$State) %>% tolower()
fifty_states_merge <- left_join(fifty_states,bronco_state_df, by=c("id"="State"))
fifty_states_merge <- fifty_states_merge[fifty_states_merge$id!="virginia",]
fifty_states_merge %>%  ggplot(aes(map_id = id)) + 
  geom_map(aes(fill = count), map = fifty_states_merge) + 
  expand_limits(x = fifty_states_merge$long, y = fifty_states_merge$lat) + coord_map() + 
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) + labs(x = "", y = "") + 
  theme(legend.position = "right", panel.background = element_blank()) + 
  fifty_states_inset_boxes() +
  geom_polygon(data = fifty_states_merge, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff") +
  scale_fill_continuous(low="lightblue", high="midnightblue", guide="colorbar") +
  labs(title="Number of Recruits",subtitle="(Under Coach Mendenhall)",fill="Number of Recruits")
##

##tables for pipeline recruit caliber
pipeline_all_yr <- subset(df_all, 
       State == "VA"|State == "PA"|State == "MD"|State == "FL"|State == "NJ"|State == "GA")
table_pipeline_all <- pipeline_all_yr %>% group_by(State) %>% 
  summarize(caliber=mean(Composite.Rating)) %>% arrange(desc(caliber)) %>% as.data.frame()
pipeline_bronco_yr <- subset(df_bronco, 
       State == "VA"|State == "PA"|State == "MD"|State == "FL"|State == "NJ"|State == "GA"|
         State == "HI")
table_pipeline_bronco <- pipeline_bronco_yr %>% group_by(State) %>% 
  summarize(caliber=mean(Composite.Rating)) %>% arrange(desc(caliber)) %>% as.data.frame()
##

##Average Composite Rating Plot
df_all %>% group_by(Year) %>% summarize(caliber=mean(Composite.Rating)) %>% 
  ggplot(aes(x=Year,y=caliber)) + geom_line() + geom_point() + 
  scale_x_continuous(breaks=seq(2002,2019)) + scale_y_continuous(breaks=seq(.8,.9,.01)) +
  theme(axis.text.x=element_text(angle=90,vjust=.5)) + 
  geom_text(aes(x=Year+.7,label=round(caliber,3)),size=2.5) +
  labs(title="Average Recruiting Class Composite Rating, by Year", x="Year",
       y="Average Composite Rating")
##
all_acc_df <- df_all[df_all$All.ACC==1,]
all_amer_df <- df_all[df_all$All.American==1,]
star_acc <- all_acc_df %>% 
  group_by(Star_Rating) %>% summarize(all.acc=n()) %>% arrange(desc(Star_Rating))
star_amer <- all_amer_df %>% 
  group_by(Star_Rating) %>% summarize(all.amer=n()) %>% arrange(desc(Star_Rating))
star_all <- df_all %>% group_by(Star_Rating) %>% summarize(count=n()) %>% arrange(desc(Star_Rating))
star_df <- left_join(star_all, star_acc, by= "Star_Rating")
star_df <- left_join(star_df,star_amer,by="Star_Rating")
star_df[is.na(star_df)]<-0

scps_state <- df_all %>% group_by(State) %>% summarize(count=n()) %>% arrange(desc(count))
df_five <- df_all[df_all$Star_Rating==5,]
scps_five <- df_five %>% group_by(State) %>% summarize(five=n())
df_four <- df_all[df_all$Star_Rating==4,]
scps_four <- df_four %>% group_by(State) %>% summarize(four=n())
df_three <- df_all[df_all$Star_Rating==3,]
scps_three <- df_three %>% group_by(State) %>% summarize(three=n())
df_two <- df_all[df_all$Star_Rating==2,]
scps_two <- df_two %>% group_by(State) %>% summarize(two=n())

scps_all <- left_join(scps_state,scps_five,by='State')
scps_all <- left_join(scps_all,scps_four,by='State')
scps_all <- left_join(scps_all,scps_three,by='State')
scps_all <- left_join(scps_all,scps_two,by='State') %>% as.data.frame()
scps_all[is.na(scps_all)]<-0

melted_scps_all <- melt(scps_all,id='State')
  
melted_scps_all %>% ggplot(aes(x=State,fill=variable,color=variable,alpha=variable)) +
  geom_bar(data=subset(melted_scps_all,variable=='count'),
           aes(x=reorder(State,-value),y=value),stat='identity') +
  geom_bar(data=subset(melted_scps_all,variable=='five'),aes(y=value),stat='identity') +
  geom_bar(data=subset(melted_scps_all,variable=='four'),aes(y=value),stat='identity') +
  geom_bar(data=subset(melted_scps_all,variable=='three'),aes(y=value),stat='identity') +
  geom_bar(data=subset(melted_scps_all,variable=='two'),aes(y=value),stat='identity') +
  scale_fill_manual(labels=c('Count','Five Star','Four Star','Three Star','Two Star'),
                    values=c('dodgerblue4','black','darkgray','darkorange1','white')) +
  scale_color_manual(labels=c('Count','Five Star','Four Star','Three Star','Two Star'),
                    values=c('navy','black','dimgray','darkorange4','gray')) +
  scale_alpha_manual(labels=c('Count','Five Star','Four Star','Three Star','Two Star'),
                    values=c(.2,1,1,.2,.5)) +
  geom_text(data=subset(melted_rct_all,variable=='count'),aes(y=value+2,label=value),alpha=1) +
  geom_text(data=subset(melted_rct_all,variable=='all.acc'),aes(y=value+1,label=value),alpha=1) +
  labs(title='Total Recruits, All-ACC, and All-Americans, by State',
       subtitle='(Under Coach Mendenhall)',x='State',y='Total') + 
  theme(axis.text.x=element_text(angle=90,vjust=.5,hjust=1))

##################Use below if needed
star_summary %>% ggplot(aes(x=year)) +
  geom_line(aes(y=total, color = )) +
  geom_line(aes(y=five_star, color = "blue")) +
  geom_line(aes(y=four_star, color = "gray")) +
  geom_line(aes(y=three_star)) +
  geom_line(aes(y=two_star))


df_all %>% group_by(Star_Rating, Year) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=Year, y=count)) + geom_line(aes(color=Star_Rating)) + geom_point() +
  geom_line(data=df_all %>% group_by(Year) %>% summarize(total=n()), aes(x=Year, y=total))

#for help making choropleths
states_map <- map_data("state")
state_df <- df_all %>% group_by(State) %>% summarize(count=n()) %>% arrange(desc(count))
state_df$State <- abbr2state(state_df$State) %>% tolower()
states_merge <- left_join(states_map, state_df, by = c("region"="State"))
states_merge_not_va <- states_merge[states_merge$region!="virginia",]
states_merge_not_va %>%
  ggplot(aes(long, lat, group = group, fill = count)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Number of Recruits") +
  geom_polygon(data = states_merge, mapping = aes(long, lat, group = group),
               fill = NA, color = "#ffffff")
