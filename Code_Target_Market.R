# install required package
install.packages("tidyverse")
library(tidyverse)

# load the NDNS individual level data | data for the food items they consume
ind <- read_csv("individuals.csv")
view(ind)
print(ind)

# querying the data + summarising some details
# know the details of the individual with the id of 1020
filter(ind, id=="1020")
# join 'filter functions' with other functions
# find the max. consumption of beef by an individual
filter(ind, Beef==max(Beef)) %>% 
  select(id, Beef)  

# summarise the dataset to show how many individuals are within each category
table(ind$Ethnicity)

#1. find the ethnicity of the individual with id 1071
filter(ind, id=="1071") %>% 
  select(id, Ethnicity)
#2. find the max recorded pork consumption by an individual
filter(ind, Pork==max(Pork)) %>% 
  select(id, Pork)
#3. how many individuals classify their general health as 'good'?
filter(ind, Health=="good") %>% 
  count()
---

# create a new table which shows avg. beef consumption for each ethnic group
  summary_beef_eth <- ind %>% 
  group_by(Ethnicity) %>% 
  summarise(consumption=mean(Beef))
print(summary_beef_eth)
---

# PLOTTING THE NEWLY CREATED TABLE
# install required package
install.packages("ggplot2")
library(ggplot2)

# plot the beef_eth table
ggplot(data=summary_beef_eth, aes(x=Ethnicity, y= consumption))+
  geom_bar(stat="identity")+
  ylab("Consumption per capita (g/day)")+
  ggtitle("Beef consumption By Ethnic Group")

# plot multiple variables
# plot a graph showing consumption of Beef according to both age and sex
# for this, create a new summary table 'summary_beef_agesex'
summary_beef_agesex <- ind %>% 
  group_by(Sex, Age) %>% 
  summarise(consumption=mean(Beef))
print(summary_beef_agesex)
view(summary_beef_agesex)

# plot this
ggplot(data=summary_beef_agesex, aes(x=Age, y=consumption, fill=Sex)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8)) +
  ylab("Consumption per capita (g/day)") +
  ggtitle("Beef consumption by Age and Sex groups") +
  scale_fill_manual(values=c("F"="#FF9999", "M"="#6699CC")) +
  theme_minimal()

# calculate the average lamb consumption for each category of self reported 'Health' column in the 'ind; table
# create a new summary table 'summary_lamb_health'
summary_lamb_health <- ind %>% 
  group_by(Health) %>% 
  summarise(consumption=mean(Lamb))
view(summary_lamb_health)

#4. find the average lamb consuption of ind who classify their health as bad?
summary_lamb_health %>%
  filter(Health == "bad")
# plot the table
ggplot(data=summary_lamb_health, aes(x=Health, y=consumption))+
  geom_bar(stat="identity")+
  ylab("Consumption per capita(g/day)")+
  ggtitle("Lamb Consumption and Health")

# GEOGRAPHICALLY AGGREGATED DATA
# querying Geographically Aggregated Data

# load the "age_sex.csv" and "Ethnicity.csv"
AgeSex <- read_csv("age_sex.csv")
Ethnicity <- read_csv("Ethnicity.csv")

# select a specific geographical zone 
filter(AgeSex, zone_id=="E01011300")

#  geographical zone with the highest number of female residents aged between 5-9
filter(AgeSex, Fa05_09==max(Fa05_09)) %>% 
  select(zone_id, Fa05_09)

# how many residents are female and aged b/w 10 and 15 years (Fa10_15) in the LSOA with
# id LSOA E01010579
filter(AgeSex, zone_id=="E01010579") %>% 
  select(zone_id, Fa10_15)

# what is the zone id of the geo zone with highest no. of male residents aged 75 and over
# Ma75_pl
filter(AgeSex, Ma75_pl==max(Ma75_pl)) %>% 
  select(Ma75_pl, zone_id)




# COMPARING INDIVIDUAL GEO ZONES TO THE ENTIRE POPULATION
# load "Ethnicity.csv" file
Ethnicity <- read_csv("Ethnicity.csv")

# create a table containing the PROPORTION OF PEOPLE IN ETHNIC GROUP FOR THE ENTIRE POP.
# we will use this metric to compare the results for an individual zone

all_eth <- Ethnicity %>% 
  select(-c("zone_name", "zone_id")) %>% 
  colSums() %>% 
  enframe(.,value = "all_count") %>% 
  mutate(all_prop = all_count / sum(all_count))

# to generate similar table, but for a different demographic characteristic eg. Age and Sex
all_agesex <- AgeSex %>% 
  select(-c("zone_name", "zone_id")) %>% 
  colSums() %>% 
  enframe(.,value = "all_count") %>% 
  mutate(all_prop = all_count / sum(all_count))

# similar table but for an individual goe zone. zone id E01011307
# code is similar to above, but contains a filter for our particular zone 
zone_eth <- Ethnicity %>% 
  filter(zone_id %in% c("E01011307")) %>% 
  select(-c("zone_name", "zone_id")) %>% 
  colSums() %>% 
  enframe(., value = "zone_count") %>% 
  mutate(zone_prop=zone_count/sum(zone_count))

# joining two tables 
# join is based on the 'name' of the column ('name' i.e. the ethnic group) as it is common b/w two variables

join_table <- left_join(all_eth, zone_eth, by = "name")

# add a column "diff" to the "join_table" which wil calculate the proportions of the entire population
# i.e. the "all_prop" column and the individual zone i.e. the "zone_prop" column
# because the original data is in the form of proportions, we will multiply it with 100
# so that it is in the form of percentage
diff_table <- mutate(join_table, diff=(zone_prop-all_prop)*100)

# plot the data in the form of a bar chart to show the deviation b/w the ethnicity profile of zone E01011307 and the enitre pop.
# or understand it as you are presenting the data from diff_table
# the chart is viewed by typing its name (i.e. the last line of the code)

ggplot(data=diff_table)+
  geom_bar(aes(x=name, y=diff),
           stat="identity")+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        text = element_text(size=13))+
  ylab("Deviation (%)")+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  ggtitle("Deviation of Ethnic Groups between zone E01011307 and the enitre population")

# code for the particular geo zone containing proportion of people's ethnicity to the enitre pop
grade_zone_eth <- Ethnicity %>% 
  filter(zone_id %in% c("E01010674")) %>% 
  select(-c("zone_name", "zone_id")) %>% 
  colSums() %>% 
  enframe(., value = "zone_count") %>% 
  mutate(zone_prop=zone_count/sum(zone_count))

# join the table to join our particular zone to the enitre pop
grade_join_table <- left_join(all_eth, grade_zone_eth, by = "name")

# adding "diff" to the grade_join_table
grade_diff_table <- mutate(grade_join_table, diff=(zone_prop-all_prop)*100)

## plotting the grade_diff table 
ggplot(data=grade_diff_table)+
  geom_bar(aes(x=name, y=diff),
           stat="identity")+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        text = element_text(size=13))+
  ylab("Deviation (%)")+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)+
  ggtitle("Deviation of Ethnic Groups between zone E01010674 and the enitre population") 