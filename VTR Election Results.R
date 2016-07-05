library(dplyr)
library(tidyr)

sen_fp_div <- read.csv("http://vtr.aec.gov.au/Downloads/SenateFirstPrefsByDivisionByVoteTypeDownload-20499.csv", 
                       skip=1, sep=",", header=TRUE)
enrolment <- read.csv("http://vtr.aec.gov.au/Downloads/GeneralEnrolmentByStateDownload-20499.csv",
                      skip=1, sep=",", header=TRUE)

# HoR First Preference by Polling Place
pp_url <- vector()
pp_url["nsw"] <- "http://vtr.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-NSW.csv"
pp_url["vic"] <- "http://vtr.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-VIC.csv"
pp_url["qld"] <- "http://vtr.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-QLD.csv"
pp_url["wa"] <- "http://vtr.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-WA.csv"
pp_url["sa"] <- "http://vtr.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-SA.csv"
pp_url["tas"] <- "http://vtr.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-TAS.csv"
pp_url["act"] <- "http://vtr.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-ACT.csv"
pp_url["nt"] <- "http://vtr.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-NT.csv"

hor_fp_pp <- read.csv(pp_url["nsw"], skip=1, sep=",", header=TRUE)
hor_fp_pp <- rbind(hor_fp_pp,
                   read.csv(pp_url["vic"], skip=1, sep=",", header=TRUE), 
                   read.csv(pp_url["qld"], skip=1, sep=",", header=TRUE), 
                   read.csv(pp_url["wa"], skip=1, sep=",", header=TRUE), 
                   read.csv(pp_url["sa"], skip=1, sep=",", header=TRUE), 
                   read.csv(pp_url["tas"], skip=1, sep=",", header=TRUE), 
                   read.csv(pp_url["act"], skip=1, sep=",", header=TRUE), 
                   read.csv(pp_url["nt"], skip=1, sep=",", header=TRUE)
                   )

# TCP by polling place
hor_tcp_pp <- read.csv("http://vtr.aec.gov.au/Downloads/HouseTcpByCandidateByPollingPlaceDownload-20499.csv",
                       skip=1, sep=",", header=TRUE)
# Polling place locations
pp_location <- read.csv("http://vtr.aec.gov.au/Downloads/GeneralPollingPlacesDownload-20499.csv",
                        skip=1, sep=",", header=TRUE)

# First preference by candidate by vote type (by division)
hor_fp_cand_type <- read.csv("http://vtr.aec.gov.au/Downloads/HouseFirstPrefsByCandidateByVoteTypeDownload-20499.csv",
                             skip=1, sep=",", header=TRUE) 

# Enrolment by division
enrolment_div <- read.csv("http://vtr.aec.gov.au/Downloads/GeneralEnrolmentByDivisionDownload-20499.csv",
                          skip=1, sep=",", header=TRUE)

# Senate votes by state:
sen_fp_div %>% 
  select(StateAb, PartyName, TotalVotes) %>% 
  group_by(StateAb) %>% 
  summarise(votes=sum(TotalVotes)) %>% 
  left_join(enrolment) %>% 
  mutate(percent_enrolment = votes/CloseOfRollsEnrolment * 100) %>% 
  select(StateAb, votes, percent_enrolment)

# House of Reps votes by Division
hor_fp_pp %>% 
  filter(PartyNm != "Informal") %>% 
  group_by(DivisionNm, PartyAb) %>% 
  summarise(votes=sum(OrdinaryVotes))
  
# Two candidate preferred by parties by Division
derived_tcp_div <- hor_tcp_pp %>% 
  group_by(DivisionNm, PartyAb) %>% summarise(votes = sum(OrdinaryVotes)) %>% spread(key = PartyAb, value = votes)

derived_tcp_div$total <- rowSums(derived_tcp_div[-1], na.rm=TRUE)
derived_tcp_div <- derived_tcp_div %>% mutate_each(funs(./total*100), -total)

# Two candidate preferred by parties by Polling Place
derived_tcp_pp <- hor_tcp_pp %>% 
  group_by(DivisionNm, PollingPlace, PartyAb) %>% 
  summarise(votes = sum(OrdinaryVotes)) %>% 
  spread(key = PartyAb, value = votes)

derived_tcp_pp$total <- rowSums(derived_tcp_pp[-c(1,2)], na.rm=TRUE)
derived_tcp_pp <- derived_tcp_pp %>% 
  mutate_each(funs(./total*100), -total)

# Grim reaper - Note, not exactly the same as the AEC's Grim Reaper because it goes by former candidate, not party.
derived_tcp_div <- derived_tcp_div %>% 
  left_join(hor_fp_cand_type %>% filter(HistoricElected == "Y") %>% select(DivisionNm, Historic=PartyAb))

derived_reaper <- derived_tcp_div %>% 
  gather(PartyAb, vote.percent, 2:10) %>% 
  filter(PartyAb == Historic & vote.percent < 50) %>% 
  left_join(hor_fp_cand_type 
            %>% select(DivisionNm, PartyAb, Surname, GivenNm)) %>% 
  left_join(enrolment_div 
            %>% select(DivisionNm, Enrolment, StateAb)) %>% 
  left_join(derived_tcp_div 
            %>% gather(PartyAb, vote.percent, 2:10) %>% filter(vote.percent > 50) %>% select(DivisionNm, lead.party=PartyAb)) %>% 
  mutate(percent.counted = total/Enrolment * 100) %>% 
  select(StateAb, DivisionNm, held.party=PartyAb, held.tpp=vote.percent, held.surname=Surname, held.givenname=GivenNm, lead.party, 
         number.counted=total, percent.counted) %>% 
  ungroup %>% 
  arrange(percent.counted)

# Not sure where this one should go...
derived_prop_vote_div <- hor_fp_cand_type %>% 
  filter(PartyNm != "Informal") %>% 
  left_join(hor_fp_cand_type %>% filter(PartyNm != "Informal") %>% group_by(DivisionNm) %>% summarise(div.total = sum(TotalVotes))) %>% 
  mutate(prop.vote = TotalVotes/div.total * 100) %>% 
  select(StateAb, DivisionNm, HistoricElected, PartyAb, TotalVotes, prop.vote)

# Total votes by type by division
derived_div_total_type <- hor_fp_cand_type %>% 
  group_by(StateAb, DivisionNm) %>% 
  summarise(ordinary = sum(OrdinaryVotes),
            absent = sum(AbsentVotes),
            provisional = sum(ProvisionalVotes),
            prepoll.dec = sum(PrePollVotes),
            postal = sum(PostalVotes), 
            total = sum(TotalVotes))

# Total votes by polling place
derived_pp_totals <- hor_fp_pp %>% 
  group_by(StateAb, DivisionNm, DivisionID, PollingPlaceID, PollingPlace) %>% 
  summarise(votes = sum(OrdinaryVotes)) %>% 
  mutate(prepoll = ifelse(grepl("PPVC", PollingPlace) | grepl("PREPOLL", PollingPlace), "Y", "N"))

# Add a pre-poll ordinaries column on to derived_div_total_type
derived_div_total_type <- derived_div_total_type %>% 
  left_join(derived_pp_totals %>% group_by(DivisionNm) %>% filter(prepoll == "Y") %>% summarise(prepoll.ordinary = sum(votes)))
