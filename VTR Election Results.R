library(dplyr)
library(tidyr)

##
## Functions list
##
## get_vtr_file() <- Download a CSV file from the VTR. 
## polling_place_results_by_candidate() <- Results by polling place and candidate with dec votes as dummy PPs
## prelim_senate_turnout_by_state() <- Percentage turnout for Senate votes by state based on state enrolments
## formal_hor_votes_by_party_by_div() <- Formal HoR votes by party by division
## tcp_by_party_by_pollingplace() <- Two candidate perferred by party by polling place
## vote_type_by_div_inc_ppord() <- Table of states and divisions with ord, abs, prov, pp.dec., postals, pp.ord, total 
## calculate_senate_quotas(vacancies) <- Calculate senate first preference quotas (based on vacancies)
## grim_reaper() <- Show divisions with sitting candidate lagging in the count.

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

# enrolment <- get_vtr_file("GeneralEnrolmentByStateDownload-20499.csv")
# Replace with: enrolment_div %>% group_by(StateAb) %>% summarise(enrolment=sum(Enrolment))

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

hor_fp_pp <- do.call("rbind", lapply(pp_url, get_vtr_file))

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

# Combined polling place table (with declaration votes as dummy polling places)
polling_place_results_by_candidate <- function(){
  
  tmp_pp_all <- hor_fp_pp
  tmp_pp_all$DeclarationVote <- "N"
  
  # Absent votes:
  tmp_pp_all <- bind_rows(tmp_pp_all, hor_fp_cand_type %>% 
                            select(-OrdinaryVotes, -ProvisionalVotes, -PrePollVotes, -PostalVotes, -TotalVotes) %>% 
                            rename(OrdinaryVotes = AbsentVotes) %>% 
                            mutate(PollingPlace=as.factor("_dummy_absent"), DeclarationVote="Y"))
  
  # Provisional votes:
  tmp_pp_all <- bind_rows(tmp_pp_all, hor_fp_cand_type %>% 
                            select(-OrdinaryVotes, -AbsentVotes, -PrePollVotes, -PostalVotes, -TotalVotes) %>% 
                            rename(OrdinaryVotes = ProvisionalVotes) %>% 
                            mutate(PollingPlace=as.factor("_dummy_provisional"), DeclarationVote="Y"))
  
  # PrePoll dec votes:
  tmp_pp_all <- bind_rows(tmp_pp_all, hor_fp_cand_type %>% 
                            select(-OrdinaryVotes, -AbsentVotes, -ProvisionalVotes, -PostalVotes, -TotalVotes) %>% 
                            rename(OrdinaryVotes = PrePollVotes) %>% 
                            mutate(PollingPlace=as.factor("_dummy_prepolldec"), DeclarationVote="Y"))
  
  # Postal votes:
  tmp_pp_all <- bind_rows(tmp_pp_all, hor_fp_cand_type %>% 
                            select(-OrdinaryVotes, -AbsentVotes, -ProvisionalVotes, -PrePollVotes, -TotalVotes) %>% 
                            rename(OrdinaryVotes = PostalVotes) %>% 
                            mutate(PollingPlace=as.factor("_dummy_postal"), DeclarationVote="Y"))
  # PrePoll ordinaries
  tmp_pp_all <- tmp_pp_all %>% 
    mutate(PrepollOrdinary = ifelse(grepl("PPVC", PollingPlace) | grepl("PREPOLL", PollingPlace), "Y", "N"))
  
  tmp_pp_all$PollingPlace <- as.factor(tmp_pp_all$PollingPlace)
  tmp_pp_all$DeclarationVote <- as.factor(tmp_pp_all$DeclarationVote)
  tmp_pp_all$PrepollOrdinary <- as.factor(tmp_pp_all$PrepollOrdinary)
  return(tmp_pp_all)
}

derived_pp_all <- polling_place_results_by_candidate()

# Senate turnout by state (as a proportion of enrolments):
prelim_senate_turnout_by_state <- function() {
  sen_fp_div %>% 
    select(StateAb, PartyName, TotalVotes) %>% 
    group_by(StateAb) %>% 
    summarise(votes=sum(TotalVotes)) %>% 
    left_join(enrolment_div %>% group_by(StateAb) %>% summarise(enrolment=sum(Enrolment))) %>% 
    mutate(percent_enrolment = votes/enrolment * 100) %>% 
    select(StateAb, votes, percent_enrolment)
}

# Formal House of Reps votes by Division
formal_hor_votes_by_party_by_div <- function() {
  tmp_formal_votes_div <- hor_fp_pp %>% 
    filter(PartyNm != "Informal") %>% 
    group_by(DivisionNm, PartyAb) %>% 
    summarise(votes=sum(OrdinaryVotes))
  
  return(tmp_formal_votes_div)
}

derived_formal_votes_div <- formal_hor_votes_by_party_by_div()

# Two candidate preferred by parties by Polling Place
tcp_by_party_by_pollingplace <- function(){
  tmp_tcp_pp <- hor_tcp_pp %>% 
    group_by(DivisionNm, PollingPlace, PartyAb) %>% 
    summarise(votes = sum(OrdinaryVotes)) %>% 
    spread(key = PartyAb, value = votes)
  
  # Add TCP counts for Postal, Absent, Provisional and PrePoll Declaration votes as dummy polling places
  tmp_tcp_pp <- bind_rows(tmp_tcp_pp, hor_tcp_type %>% 
                            group_by(DivisionNm, PartyAb) %>% 
                            summarise(postal=sum(PostalVotes)) %>% 
                            spread(key=PartyAb, postal) %>% 
                            mutate(PollingPlace=as.factor("_dummy_postal")))
  
  tmp_tcp_pp <- bind_rows(tmp_tcp_pp, hor_tcp_type %>% 
                            group_by(DivisionNm, PartyAb) %>% 
                            summarise(absent=sum(AbsentVotes)) %>% 
                            spread(key=PartyAb, absent) %>% 
                            mutate(PollingPlace=as.factor("_dummy_absent")))
  
  tmp_tcp_pp <- bind_rows(tmp_tcp_pp, hor_tcp_type %>% 
                            group_by(DivisionNm, PartyAb) %>% 
                            summarise(provisional=sum(ProvisionalVotes)) %>% 
                            spread(key=PartyAb, provisional) %>% 
                            mutate(PollingPlace=as.factor("_dummy_provisional")))
  
  tmp_tcp_pp <- bind_rows(tmp_tcp_pp, hor_tcp_type %>% 
                            group_by(DivisionNm, PartyAb) %>% 
                            summarise(prepolldec=sum(PrePollVotes)) %>% 
                            spread(key=PartyAb, prepolldec) %>% 
                            mutate(PollingPlace=as.factor("_dummy_prepolldec")))
  
  tmp_tcp_pp$total <- rowSums(tmp_tcp_pp[-c(1,2)], na.rm=TRUE)
  tmp_tcp_pp$PollingPlace <- as.factor(tmp_tcp_pp$PollingPlace)
  
  return(tmp_tcp_pp)
}

derived_tcp_pp <- tcp_by_party_by_pollingplace()

# TCP by polling place as percentage
tcp_by_party_by_pollingplace_percent <- function(){
  
  tmp_tcp_pp_percent <- tcp_by_party_by_pollingplace() %>% 
    mutate_each(funs(./total*100), -total, -DivisionNm, -PollingPlace)
  
  return(tmp_tcp_pp_percent)
}

# TCP division totals as a precentage (note only works with dplyr > 0.5)
tcp_by_party_by_division_percent <- function() {
  tmp_tcp_pp_percent <- tcp_by_party_by_pollingplace() %>% 
    group_by(DivisionNm) %>% 
    summarise_each(funs(sum(.,na.rm=TRUE)), -PollingPlace, -DivisionNm)
  
  # TCP division as a percentage
  tmp_tcp_pp_percent <- tmp_tcp_pp_percent %>% 
    mutate_each(funs(./total*100), -total, -DivisionNm)
  
  return(tmp_tcp_pp_percent)
}

# Grim reaper - Note, not exactly the same as the AEC's Grim Reaper because it goes by former candidate, not party.
grim_reaper <- function() {
  
  tmp_reaper_1 <- tcp_by_party_by_division_percent() %>% 
    left_join(hor_fp_cand_type %>% filter(HistoricElected == "Y") %>% select(DivisionNm, Historic=PartyAb), 
              by = "DivisionNm")
  
  tmp_reaper <- tmp_reaper_1 %>% 
    gather(PartyAb, vote.percent, 2:10) %>% 
    filter(PartyAb == Historic & vote.percent < 50) %>% 
    left_join(hor_fp_cand_type 
              %>% select(DivisionNm, PartyAb, Surname, GivenNm), 
              by = c("DivisionNm", "PartyAb")) %>% 
    left_join(enrolment_div 
              %>% select(DivisionNm, Enrolment, StateAb), 
              by = "DivisionNm") %>% 
    left_join(tcp_by_party_by_division_percent()
              %>% gather(PartyAb, vote.percent, 2:10) %>% filter(vote.percent > 50) %>% select(DivisionNm, lead.party=PartyAb), 
              by = "DivisionNm") %>%
    mutate(percent.counted = total/Enrolment * 100) %>% 
    select(StateAb, DivisionNm, held.party=PartyAb, held.tpp=vote.percent, held.surname=Surname, held.givenname=GivenNm, lead.party, 
           number.counted=total, percent.counted) %>% 
    ungroup %>% 
    arrange(percent.counted)
  
  return(tmp_reaper)
  
}

# Not sure where this one should go...
derived_prop_vote_div <- hor_fp_cand_type %>% 
  filter(PartyNm != "Informal") %>% 
  left_join(hor_fp_cand_type %>% filter(PartyNm != "Informal") %>% group_by(DivisionNm) %>% summarise(div.total = sum(TotalVotes))) %>% 
  mutate(prop.vote = TotalVotes/div.total * 100) %>% 
  select(StateAb, DivisionNm, HistoricElected, PartyAb, TotalVotes, prop.vote)

# # Total votes by type by division
# derived_div_total_type <- hor_fp_cand_type %>% 
#   group_by(StateAb, DivisionNm) %>% 
#   summarise(ordinary = sum(OrdinaryVotes),
#             absent = sum(AbsentVotes),
#             provisional = sum(ProvisionalVotes),
#             prepoll.dec = sum(PrePollVotes),
#             postal = sum(PostalVotes), 
#             total = sum(TotalVotes))

# Total votes by polling place
derived_pp_totals <- hor_fp_pp %>% 
  group_by(StateAb, DivisionNm, DivisionID, PollingPlaceID, PollingPlace) %>% 
  summarise(votes = sum(OrdinaryVotes)) %>% 
  mutate(prepoll = ifelse(grepl("PPVC", PollingPlace) | grepl("PREPOLL", PollingPlace), "Y", "N"))

# Add a pre-poll ordinaries column on to derived_div_total_type
vote_type_by_div_inc_ppord <- function() {
  
  tmp_vote_type_total <- hor_fp_cand_type %>% 
    group_by(StateAb, DivisionNm) %>% 
    summarise(ordinary = sum(OrdinaryVotes),
              absent = sum(AbsentVotes),
              provisional = sum(ProvisionalVotes),
              prepoll.dec = sum(PrePollVotes),
              postal = sum(PostalVotes), 
              total = sum(TotalVotes))
  
  tmp_vote_type_ppord <- tmp_vote_type_total %>% 
    left_join(derived_pp_totals %>% 
                group_by(DivisionNm) %>% 
                filter(prepoll == "Y") %>% 
                summarise(prepoll.ordinary = sum(votes))) %>% 
    select(StateAb,DivisionNm,ordinary,prepoll.ordinary,absent,provisional,prepoll.dec,postal,total)
  return(tmp_vote_type_ppord)
}

derived_div_total_type <- vote_type_by_div_inc_ppord()

# Senate Quotas (DD and half Senate)
calculate_senate_quotas <- function(vacancies=6){

    # derived_sen_quotas <- sen_fp_group_type %>% 
    # group_by(StateAb) %>% 
    # left_join(sen_fp_group_type %>% group_by(StateAb) %>% summarise(state.total=sum(TotalVotes))) %>% 
    # mutate(quota.dd=ifelse(StateAb == "ACT"|StateAb == "NT", (state.total/3)+1, (state.total/13)+1)) %>% 
    # mutate(quota.hs=ifelse(StateAb == "ACT"|StateAb == "NT", (state.total/3)+1, (state.total/7)+1)) %>% 
    # mutate(num_quotas.dd = TotalVotes/quota.dd, num_quotas.hs = TotalVotes/quota.hs) %>% 
    # select(StateAb, GroupAb, TotalVotes, num_quotas.dd, num_quotas.hs)

  tmp_sen_quotas <- sen_fp_group_type %>% 
  group_by(StateAb) %>% 
  left_join(sen_fp_group_type %>% group_by(StateAb) %>% summarise(state.total=sum(TotalVotes))) %>% 
  mutate(quota=ifelse(StateAb == "ACT"|StateAb == "NT", (state.total/3)+1, (state.total/(vacancies+1))+1)) %>% 
  mutate(num_quotas = TotalVotes/quota) %>% 
  select(StateAb, GroupAb, TotalVotes, num_quotas)
  
  return(tmp_sen_quotas)
}

# # To show both full and half senate quotas in the one table:
# calculate_senate_quotas() %>% 
#   rename(quotas.fs=num_quotas) %>% 
#   left_join(calculate_senate_quotas(12) %>% select(StateAb, GroupAb, quotas.hs=num_quotas))

# # To find out the current Senate results for a state:
# derived_sen_quotas %>% filter(StateAb == "SA") %>% arrange(desc(num_quotas.dd))
