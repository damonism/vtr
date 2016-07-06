library(dplyr)
library(tidyr)

# What time is the data current as of?
vtr_time <-  Sys.time()

get_vtr_file <- function(filename) {
  filepath <- "http://vtr.aec.gov.au/Downloads"
  url <- paste(filepath, filename, sep = "/")
  return(read.csv(url, skip=1, sep=",", header=TRUE))
}

# Senate first preference by division
sen_fp_div <- get_vtr_file("SenateFirstPrefsByDivisionByVoteTypeDownload-20499.csv")

# Senate first preference by group by vote type
sen_fp_group_type <- get_vtr_file("SenateFirstPrefsByStateByGroupByVoteTypeDownload-20499.csv")

enrolment <- get_vtr_file("GeneralEnrolmentByStateDownload-20499.csv")

# HoR First Preference by Polling Place
pp_url <- vector()
pp_url["nsw"] <- "HouseStateFirstPrefsByPollingPlaceDownload-20499-NSW.csv"
pp_url["vic"] <- "HouseStateFirstPrefsByPollingPlaceDownload-20499-VIC.csv"
pp_url["qld"] <- "HouseStateFirstPrefsByPollingPlaceDownload-20499-QLD.csv"
pp_url["wa"] <- "HouseStateFirstPrefsByPollingPlaceDownload-20499-WA.csv"
pp_url["sa"] <- "HouseStateFirstPrefsByPollingPlaceDownload-20499-SA.csv"
pp_url["tas"] <- "HouseStateFirstPrefsByPollingPlaceDownload-20499-TAS.csv"
pp_url["act"] <- "HouseStateFirstPrefsByPollingPlaceDownload-20499-ACT.csv"
pp_url["nt"] <- "HouseStateFirstPrefsByPollingPlaceDownload-20499-NT.csv"

hor_fp_pp <- rbind(get_vtr_file(pp_url["nsw"]),
                   get_vtr_file(pp_url["vic"]), 
                   get_vtr_file(pp_url["qld"]), 
                   get_vtr_file(pp_url["wa"]), 
                   get_vtr_file(pp_url["sa"]), 
                   get_vtr_file(pp_url["tas"]), 
                   get_vtr_file(pp_url["act"]), 
                   get_vtr_file(pp_url["nt"]))

# TCP by polling place
hor_tcp_pp <- get_vtr_file("HouseTcpByCandidateByPollingPlaceDownload-20499.csv")

# TCP by vote type
hor_tcp_type <- get_vtr_file("HouseTcpByCandidateByVoteTypeDownload-20499.csv")

# Polling place locations
pp_location <- get_vtr_file("GeneralPollingPlacesDownload-20499.csv")

# First preference by candidate by vote type (by division)
hor_fp_cand_type <- get_vtr_file("HouseFirstPrefsByCandidateByVoteTypeDownload-20499.csv") 

# Enrolment by division
enrolment_div <- get_vtr_file("GeneralEnrolmentByDivisionDownload-20499.csv")

# Senate votes by state:
derived_sen_turnout <- sen_fp_div %>% 
  select(StateAb, PartyName, TotalVotes) %>% 
  group_by(StateAb) %>% 
  summarise(votes=sum(TotalVotes)) %>% 
  left_join(enrolment) %>% 
  mutate(percent_enrolment = votes/CloseOfRollsEnrolment * 100) %>% 
  select(StateAb, votes, percent_enrolment)

# House of Reps votes by Division
derived_formal_votes_div <- hor_fp_pp %>% 
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

# Senate Quotas (DD and half Senate)
derived_sen_quotas <- sen_fp_group_type %>% 
  group_by(StateAb) %>% 
  left_join(sen_fp_group_type %>% group_by(StateAb) %>% summarise(state.total=sum(TotalVotes))) %>% 
  mutate(quota.dd=ifelse(StateAb == "ACT"|StateAb == "NT", (state.total/3)+1, (state.total/13)+1)) %>% 
  mutate(quota.hs=ifelse(StateAb == "ACT"|StateAb == "NT", (state.total/3)+1, (state.total/7)+1)) %>% 
  mutate(num_quotas.dd = TotalVotes/quota.dd, num_quotas.hs = TotalVotes/quota.hs) %>% 
  select(StateAb, GroupAb, TotalVotes, num_quotas.dd, num_quotas.hs)
 
# # To find out the current Senate results for a state:
# derived_sen_quotas %>% filter(StateAb == "SA") %>% arrange(desc(num_quotas.dd))
