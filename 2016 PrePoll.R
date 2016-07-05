library(dplyr)
library(tidyr)
library(ggplot2)
library(scales) # Needed for ggplo2

setwd("~/2016 Federal Election")

enrolment_2013 <- 14712799 # from http://results.aec.gov.au/17496/Website/GeneralEnrolmentByState-17496.htm
enrolment_2016 <- 15676659 # From http://www.aec.gov.au/election/enrolment-statistics.htm

# Get the latest pre-polls from http://aec.gov.au/election/downloads.htm

prepoll_2016 <- read.csv("http://www.aec.gov.au/election/files/20160702_WEB_Prepoll_Report.csv")
prepoll_2016 <- prepoll_2016 %>% rename(State = m_state_ab, Division = m_div_nm)

prepoll_2013 <- read.csv("http://www.aec.gov.au/Elections/Federal_Elections/2013/files/statistics/e2013-prepoll-stats-07-09.csv")

names(prepoll_2016)[4:ncol(prepoll_2016)] <- sprintf("Day%02d", 1:(ncol(prepoll_2016)-3))
names(prepoll_2013)[4:ncol(prepoll_2013)] <- sprintf("Day%02d", 1:(ncol(prepoll_2013)-3))

prepoll_2013$Election <- "2013"
prepoll_2016$Election <- "2016"

prepoll_all <- bind_rows(prepoll_2013, prepoll_2016)
prepoll_all <- prepoll_all %>% mutate_each(funs(type.convert(as.character(.))))
prepoll_all$Election <- as.factor(prepoll_all$Election)

rm(prepoll_2013)
rm(prepoll_2016)

prepoll_numbers <- prepoll_all %>% 
  group_by(Election) %>% 
  select(starts_with("Day")) %>% 
  summarise_each(funs(sum(.,na.rm=TRUE))) %>% 
  gather("Day", "Votes", 2:17)

prepoll_numbers$Day <- as.factor(prepoll_numbers$Day)

# Cumuative totals
prepoll_numbers <- prepoll_numbers %>% 
  group_by(Election) %>% 
  mutate(Cum.Total = cumsum(Votes))

prepoll_numbers$Votes[prepoll_numbers$Votes == 0] = NA
prepoll_numbers <- prepoll_numbers %>% 
  mutate(Prop.Votes = ifelse(Election == "2016", Votes/enrolment_2016, Votes/enrolment_2013))

prepoll_numbers <- prepoll_numbers %>% 
  mutate(Cum.Prop = ifelse(Election == "2016", Cum.Total/enrolment_2016 * 100, Cum.Total/enrolment_2013 * 100))

prepoll_numbers$Cum.Total[prepoll_numbers$Election == 2016 & is.na(prepoll_numbers$Votes)] <- NA
prepoll_numbers$Cum.Prop[prepoll_numbers$Election == 2016 & is.na(prepoll_numbers$Votes)] <- NA

# Pre-poll 2013 and 2016, by absolute number
#ggplot(prepoll_numbers, aes(Day, Votes)) + 
#  geom_bar(stat = "identity", aes(fill = Election), position = "dodge") +
#  scale_x_discrete(breaks = levels(prepoll_numbers$Day), labels=1:16) +
#  scale_y_continuous(labels=comma) +
#  ggtitle("Pre-poll votes by day")

# # Pre-poll Line graph 2013 and 2016, by absolute number
# ggplot(prepoll_numbers, aes(Day, Votes, group = Election, color = Election)) + 
#   geom_line(size=1.2) + geom_point(size=4, pch=19) +
#   scale_x_discrete(breaks = levels(prepoll_numbers$Day), labels=1:16) +
#   scale_y_continuous(labels=comma) +
#   ggtitle("Pre-poll votes by day")
# 
# ggsave("PrePoll Votes Final.png")

# Cumulative pre-poll Line graph 2013 and 2016, by absolute number
ggplot(prepoll_numbers, aes(Day, Cum.Total, group = Election, color = Election)) + 
  geom_line(size=1.2) + geom_point(size=4, pch=19) +
  scale_x_discrete(breaks = levels(prepoll_numbers$Day), labels=1:16) +
  scale_y_continuous(labels=comma) +
  labs(title="Cumulative pre-poll votes by day", y="Total number of votes")

ggsave("Cumulative PrePoll Votes Final.png")
# # Pre-poll Line graph 2013 and 2016, by proportion of enrolled population
# ggplot(prepoll_numbers, aes(Day, Prop.Votes, group = Election, color = Election)) + 
#   geom_line(size=1.2) + geom_point(size=4, pch=19) +
#   scale_x_discrete(breaks = levels(prepoll_numbers$Day), labels=1:16) +
#   ggtitle("Pre-poll votes by day (proportioprepoll_numbers %>% group_by(Election) %>% mutate(cumsum = cumsum(Votes))n of enrolled population)") +
#   ylab("Proportion of enrolled population")

# Cumulative pre-poll Line graph 2013 and 2016, by absolute number
ggplot(prepoll_numbers, aes(Day, Cum.Prop, group = Election, color = Election)) + 
  geom_line(size=1.2) + geom_point(size=4, pch=19) +
  scale_x_discrete(breaks = levels(prepoll_numbers$Day), labels=1:16) +
  scale_y_continuous(labels=comma) +
  labs(title="Cumulative pre-poll votes by day (percent of enrolled population)",
       y="Percent of enrolled population")
ggsave("Cumulative PrePoll Percent Final.png")

# # Analysis by Division by election
# prepoll_div <- prepoll_all %>% 
#   group_by(Election, Division) %>% 
#   select(starts_with("Day")) %>% 
#   summarise_each(funs(sum(.,na.rm=TRUE))) %>% 
#   gather("Day", "Votes", 3:18) %>% 
#   group_by(Election, Division) %>% 
#   mutate(Cum.Total = cumsum(Votes))
# 
# prepoll_div$Day <- as.factor(prepoll_div$Day)
# 
# prepoll_div$Cum.Total[prepoll_div$Election=="2016" & prepoll_div$Votes == 0] = NA
# 
# prepoll_div$Division[prepoll_div$Election == "2013" & prepoll_div$Division == "Fraser"] <- "Fenner"
# prepoll_div$Division[prepoll_div$Election == "2013" & prepoll_div$Division == "Throsby"] <- "Whitlam"
# 
# ggplot(prepoll_div, aes(Day, Cum.Total, group=Election, colour=Election)) +
#   scale_x_discrete(breaks = c("Day04", "Day08", "Day12", "Day16"), labels=c(4,8,12,16)) +
#   scale_y_continuous(name="Cumulative votes (x 1000)", breaks=c(0, 20000, 40000), labels=c(0, 20, 40)) +
#   geom_line() +
#   facet_wrap(~Division)
#   #facet_wrap(~Division, scales = "free_y")
# 
# ggsave("Prepoll Total Division.png")

# By State
prepoll_state <- prepoll_all %>% 
  group_by(Election, State) %>% 
  select(starts_with("Day")) %>% 
  summarise_each(funs(sum(.,na.rm=TRUE))) %>% 
  gather("Day", "Votes", 3:18)

prepoll_state <- prepoll_state %>% 
  group_by(Election, State) %>% 
  mutate(Cum.Total = cumsum(Votes))

prepoll_state$Cum.Total[prepoll_state$Election=="2016" & prepoll_state$Votes == 0] = NA

prepoll_div$Day <- as.factor(prepoll_div$Day)

ggplot(prepoll_state, aes(Day, Cum.Total, group=Election, colour=Election)) +
  scale_x_discrete(breaks = c("Day04", "Day08", "Day12", "Day16"), labels=c(4,8,12,16)) +
  scale_y_continuous(labels=comma) +
  labs(title = "Cumulative total pre-poll votes by state", y="Total votes") +
  geom_line(size=1) + 
  geom_point(size=2, pch=19) +
  facet_wrap(~State, scales = "free_y")

ggsave("Cumulative PrePoll State Final.png")

#
# Proportion of vote by state
#

# 2016 Enrolments

enrolment_all_2016 <- read.csv("http://aec.gov.au/election/files/enrolment-statistics.csv", skip = 4, header=TRUE) 

enrolment_all_2016$Electors.on.2016.Certified.list <- as.numeric(gsub(",", "", enrolment_all_2016$Electors.on.2016.Certified.list))

# 2016 Enrolment by state
enrolment_state_2016 <- enrolment_all_2016 %>% 
  group_by(State) %>% 
  summarise(enrolled.total=sum(Electors.on.2016.Certified.list,na.rm=TRUE))

# # I don't think I ended up needing this.
# tmp_state <- prepoll_all %>% 
#   group_by(Election, State) %>% 
#   select(starts_with("Day")) %>% 
#   summarise_each(funs(sum(.,na.rm=TRUE)))
# 
# tmp_state$State.Totals <- rowSums(tmp_state[,-c(1,2)], na.rm = TRUE)
# 
# tmp_state_2016 <- tmp_state %>% 
#   group_by(Election) %>% 
#   filter(Election==2016) %>% 
#   select(State,State.Totals) %>% 
#   left_join(enrolment_state_2016) %>% 
#   mutate(State.Vote.Prop = State.Totals/enrolled.total)

# 2013 enrolment rates

library("rvest")
url <- "http://www.aec.gov.au/Enrolling_to_vote/Enrolment_stats/national/2013.htm"
enrolment_state_2013_web <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="contentstart"]/div/div[1]/table[3]') %>%
  html_table()

enrolment_state_2013 <- data.frame(enrolment_state_2013_web[1])[1:8,1:2]
enrolment_state_2013$Electors.enrolled <- as.numeric(gsub(" ", "", enrolment_state_2013$Electors.enrolled))
names(enrolment_state_2013) <- c("State","enrolled.total")

# # I don't think I needed this.
# tmp_state_2013 <- tmp_state %>% 
#   group_by(Election) %>% 
#   filter(Election==2013) %>% 
#   select(State,State.Totals) %>% 
#   left_join(enrolment_state_2013) %>% 
#   mutate(State.Vote.Prop = State.Totals/enrolled.total)

enrolment_state_2013$Election <- 2013
enrolment_state_2016$Election <- 2016

enrolment_state_all <- rbind(enrolment_state_2013, enrolment_state_2016)
enrolment_state_all$Election <- as.factor(enrolment_state_all$Election)

# Add it all together
prepoll_state_rate <- prepoll_state %>% left_join(enrolment_state_all) %>% 
  mutate(State.Prop = Cum.Total/enrolled.total)

prepoll_state_rate$State.Prop[prepoll_state_rate$Election == 2016 & prepoll_state_rate$Votes == 0] <- NA

ggplot(prepoll_state_rate, aes(Day, State.Prop, group=Election, colour=Election)) +
  scale_x_discrete(breaks = c("Day04", "Day08", "Day12", "Day16"), labels=c(4,8,12,16)) +
  labs(title = "Cumulative percent pre-poll votes by state") +
  scale_y_continuous(labels = percent, name="Percent of state enrolments") +
  geom_line(size=1) + 
  geom_point(size=2, pch=19) +
  facet_wrap(~State, scales = "free_y")

ggsave("Cumulative PrePoll Rate State Final.png")