##
## Functions list -----
##
## get_vtr_file() <- Download a CSV file from the VTR. 
##
## HoR functions
## 
## polling_place_results_by_candidate() <- Results by polling place and candidate with dec votes as dummy PPs
## prelim_senate_turnout_by_state() <- Percentage turnout for Senate votes by state based on state enrolments
## formal_hor_votes_by_party_by_div() <- Formal HoR votes by party by division
## vote_type_by_div_inc_ppord() <- Table of states and divisions with ord, abs, prov, pp.dec., postals, pp.ord, total 
## grim_reaper() <- Show divisions with sitting candidate lagging in the count.
## division_total_votes(division) <- Table with the votes per candidate/group in the division
## division_total_percent(division) <- Same thing, but with everything as a percentage of the total vote
## tcp_by_vote_type_by_division() <- Adds the pre-poll ordinaries to the TCP by type by division table
## percent_party_vote_by_division() <- Number and proportion of each party's vote by division.
## public_funding_group_hor() <- Public funding by group (HoR) - does not include independents.
## seat_summary(save_as_csv) <- Seat overview. Use arguement "Y" to save as CSV to working directory.
##
### TCP
## tcp_by_vote_type_by_division()
## tcp_by_party_by_pollingplace() <- Two candidate perferred by party by polling place
##
## Senate functions
##
## senate_quotas_candidate(vacancies) <- Senate quotas by vote type by group by candidate
## calculate_senate_quotas(vacancies) <- Calculate senate first preference quotas (based on vacancies)

##
## Global functions and libraries ----
##

library(dplyr) # Requires dplyr >= 0.5
library(tidyr)

setwd("//home4/mullerd$/My Documents/Data/2016 Federal Election")
CoalitionAb <- c("LP", "LNP", "NP", "CLP")

# What time is the data current as of?
vtr_time <-  Sys.time()

get_vtr_file <- function(filename) {
  filepath <- "http://vtr.aec.gov.au/Downloads"
  url <- paste(filepath, filename, sep = "/")
  return(read.csv(url, skip=1, sep=",", header=TRUE))
}

##
## HoR Analysis ----
##

get_hor_results <- function(){
  
  vtr_time_hor <<-  Sys.time()
  
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
  
  hor_fp_pp <<- do.call("rbind", lapply(pp_url, get_vtr_file))
  
  # TCP by polling place
  hor_tcp_pp <<- get_vtr_file("HouseTcpByCandidateByPollingPlaceDownload-20499.csv")
  
  # TCP by vote type
  hor_tcp_type <<- get_vtr_file("HouseTcpByCandidateByVoteTypeDownload-20499.csv")
  
  # Polling place locations
  pp_location <<- get_vtr_file("GeneralPollingPlacesDownload-20499.csv")
  
  # First preference by candidate by vote type (by division)
  hor_fp_cand_type <<- get_vtr_file("HouseFirstPrefsByCandidateByVoteTypeDownload-20499.csv") 
  
  # Enrolment by division
  enrolment_div <<- get_vtr_file("GeneralEnrolmentByDivisionDownload-20499.csv")
  
}

#get_hor_results()

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

# Formal House of Reps votes by Division
formal_hor_votes_by_party_by_div <- function() {
  tmp_formal_votes_div <- hor_fp_pp %>% 
    filter(PartyNm != "Informal") %>% 
    group_by(DivisionNm, PartyAb) %>% 
    summarise(votes=sum(OrdinaryVotes))
  
  return(tmp_formal_votes_div)
}

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

tcp_by_vote_type_by_division <- function(){
  
  # tmp_total_div <- derived_tcp_pp %>% 
  #   gather(PartyAb, votes, -DivisionNm, -PollingPlace) %>% 
  #   group_by(DivisionNm, PartyAb) %>% 
  #   filter(PartyAb != "total") %>% 
  #   summarise(votes = sum(votes)) %>% 
  #   ungroup() %>% 
  #   filter(!is.na(votes))
  
  tmp_prepoll_ordinary <- derived_tcp_pp %>% 
    mutate(PrepollOrdinary = ifelse(grepl("PPVC", PollingPlace) | grepl("PREPOLL", PollingPlace), "Y", "N")) %>% 
    filter(PrepollOrdinary == "Y") %>% 
    group_by(DivisionNm) %>% 
    summarise_if(is.numeric, sum) %>% 
    gather(PartyAb, PrepollOrdinary, -DivisionNm) %>% 
    #ungroup() %>% 
    filter(!is.na(PrepollOrdinary)) %>% 
    filter(PartyAb != "total")
  
  tmp_prepoll_ordinary$PartyAb <- as.factor(tmp_prepoll_ordinary$PartyAb)
  
  tmp_hor_tcp_type <- hor_tcp_type %>% 
    left_join(tmp_prepoll_ordinary, by = c("DivisionNm", "PartyAb")) %>% 
    select(1:12, PrepollOrdinary, 13:18)
  
  return(tmp_hor_tcp_type)
}

tcp_by_vote_type_by_division_percent <- function(){
  
  # This is essentially the "TCP Everything" table
  # Note:
  # tcp.percent. => vote as a percentage of total vote for all candidates in the division
  # party.percent. => vote as a percentage of that party's total TCP vote. 
  # div.percent. => vote as a proportion of all votes in that division (all candidate votes)
  
  tmp_totals <- derived_hor_tcp_type %>% 
    group_by(DivisionNm) %>% 
    select(DivisionNm, PartyAb, HistoricElected, OrdinaryVotes:TotalVotes) %>% 
    summarise_if(is.numeric, funs(sum))
  
  names(tmp_totals)[-1] <- gsub("^", "total.", names(tmp_totals)[-1])
  
  tmp_div_totals <-  derived_hor_tcp_type %>% 
    left_join(tmp_totals, by="DivisionNm")
  
  tmp_div_totals <- tmp_div_totals %>% 
    mutate(tpp.percent.OrdinaryVotes = OrdinaryVotes / total.OrdinaryVotes * 100,
           tpp.percent.PrepollOrdinary = PrepollOrdinary / total.PrepollOrdinary * 100,
           tpp.percent.AbsentVotes = AbsentVotes / total.AbsentVotes * 100,
           tpp.percent.ProvisionalVotes = ProvisionalVotes / total.ProvisionalVotes * 100,
           tpp.percent.PrePollVotes = PrePollVotes / total.PrePollVotes * 100,
           tpp.percent.PostalVotes = PostalVotes / total.PostalVotes * 100,
           tpp.percent.TotalVotes = TotalVotes / total.TotalVotes * 100)

  tmp_div_totals <- tmp_div_totals %>% 
    mutate(party.percent.OrdinaryVotes = OrdinaryVotes / TotalVotes * 100,
           party.percent.PrepollOrdinary = PrepollOrdinary / TotalVotes * 100,
           party.percent.AbsentVotes = AbsentVotes / TotalVotes * 100,
           party.percent.ProvisionalVotes = ProvisionalVotes / TotalVotes * 100,
           party.percent.PrePollVotes = PrePollVotes / TotalVotes * 100,
           party.percent.PostalVotes = PostalVotes / TotalVotes * 100,
           party.percent.TotalVotes = TotalVotes / TotalVotes * 100)

  tmp_div_totals <- tmp_div_totals %>% 
    mutate(div.percent.OrdinaryVotes = OrdinaryVotes / total.TotalVotes * 100,
           div.percent.PrepollOrdinary = PrepollOrdinary / total.TotalVotes * 100,
           div.percent.AbsentVotes = AbsentVotes / total.TotalVotes * 100,
           div.percent.ProvisionalVotes = ProvisionalVotes / total.TotalVotes * 100,
           div.percent.PrePollVotes = PrePollVotes / total.TotalVotes * 100,
           div.percent.PostalVotes = PostalVotes / total.TotalVotes * 100,
           div.percent.TotalVotes = TotalVotes / total.TotalVotes * 100)
  
  tmp_div_totals <- tmp_div_totals %>% 
    mutate(margin.votes = (TotalVotes - (total.TotalVotes - TotalVotes))) %>% 
    mutate(margin.percent = margin.votes / total.TotalVotes * 100)
  
  tmp_div_totals <- tmp_div_totals %>% 
    left_join(enrolment_div %>% select(DivisionID, Enrolment) %>% 
                mutate(fifty.percent.enrolment = ceiling(Enrolment/2)), by = "DivisionID")
  
  return(tmp_div_totals)
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
percent_party_vote_by_division <- function(){
  tmp_prop_vote_div <- hor_fp_cand_type %>% 
    filter(PartyNm != "Informal") %>% 
    left_join(hor_fp_cand_type %>% 
                filter(PartyNm != "Informal") %>% 
                group_by(DivisionNm) %>% 
                summarise(div.total = sum(TotalVotes)), by = "DivisionNm") %>% 
    mutate(prop.vote = TotalVotes/div.total * 100) %>% 
    select(StateAb, DivisionNm, HistoricElected, PartyAb, TotalVotes, prop.vote)
  return(tmp_prop_vote_div)
}

# # Total votes by type by division
# derived_div_total_type <- hor_fp_cand_type %>% 
#   group_by(StateAb, DivisionNm) %>% 
#   summarise(ordinary = sum(OrdinaryVotes),
#             absent = sum(AbsentVotes),
#             provisional = sum(ProvisionalVotes),
#             prepoll.dec = sum(PrePollVotes),
#             postal = sum(PostalVotes), 
#             total = sum(TotalVotes))


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
  
  tmp_vote_type_total$DivisionNm <- as.character(tmp_vote_type_total$DivisionNm)
  
  # Total votes by polling place
  tmp_pp_totals <- hor_fp_pp %>% 
    group_by(StateAb, DivisionNm, DivisionID, PollingPlaceID, PollingPlace) %>% 
    summarise(votes = sum(OrdinaryVotes)) %>% 
    mutate(prepoll = ifelse(grepl("PPVC", PollingPlace) | grepl("PREPOLL", PollingPlace), "Y", "N"))
  
  tmp_pp_totals$DivisionNm <- as.character(tmp_pp_totals$DivisionNm)
  
  tmp_vote_type_ppord <- tmp_vote_type_total %>% 
    left_join(tmp_pp_totals %>% 
                group_by(DivisionNm) %>% 
                filter(prepoll == "Y") %>% 
                summarise(prepoll.ordinary = sum(votes)), by = "DivisionNm") %>% 
    select(StateAb,DivisionNm,ordinary,prepoll.ordinary,absent,provisional,prepoll.dec,postal,total)
  tmp_vote_type_ppord$DivisionNm <- as.factor(tmp_vote_type_ppord$DivisionNm)
  return(tmp_vote_type_ppord)
}

division_total_votes <- function(division) {
  
  tmp_totals <- derived_pp_all %>% 
    filter(DivisionNm == division) %>% 
    group_by(DivisionNm, PartyAb, CandidateID, Surname, GivenNm, Elected, HistoricElected) %>% 
    summarise(ordinary = sum(OrdinaryVotes[DeclarationVote == "N"]),
              prepoll.ordinary = sum(OrdinaryVotes[PrepollOrdinary == "Y"]),
              absent = sum(OrdinaryVotes[PollingPlace == "_dummy_absent"]),
              provisional = sum(OrdinaryVotes[PollingPlace == "_dummy_provisional"]),
              prepoll.dec = sum(OrdinaryVotes[PollingPlace == "_dummy_prepolldec"]),
              postal = sum(OrdinaryVotes[PollingPlace == "_dummy_postal"]),
              total = sum(OrdinaryVotes)
    ) %>% 
    ungroup %>% 
    arrange(desc(total))
  
  tmp_infomal <- tmp_totals %>% 
    filter(Surname == "Informal")
  
  tmp_totals <- rbind(tmp_totals %>% filter(Surname != "Informal"), tmp_infomal)
  
#   tmp_tcp <- tcp_by_party_by_division_percent() %>% filter(DivisionNm == division)
#   tmp_tcp$DivisionNm <- as.character(tmp_tcp$DivisionNm)
#   tmp_tcp <- gather(tmp_tcp, PartyAb, tcp.percent)
#   
#   tmp_tcp <- tmp_tcp[tmp_tcp$PartyAb != "DivisionNm",]
#   tmp_tcp_total <- as.numeric(tmp_tcp$tcp.percent[tmp_tcp$PartyAb == "total"])
#   tmp_tcp <- tmp_tcp[tmp_tcp$PartyAb != "total",]
#   tmp_tcp$tcp.percent <- as.numeric(tmp_tcp$tcp.percent)
#   tmp_tcp$tcp.percent[tmp_tcp$tcp.percent == 0] <- NA
#   tmp_tcp$tcp.votes <- tmp_tcp$tcp.percent / 100 * tmp_tcp_total
#   
#   #tmp_tcp_leading <- tmp_tcp$PartyAb[tmp_tcp$tcp.percent >= 50]
#   #tmp_tcp_laging <- tmp_tcp$PartyAb[tmp_tcp$tcp.percent > 0 & tmp_tcp$tcp.percent < 50]
#   
#   tmp_table <- tmp_totals %>% 
#     left_join(tmp_tcp, by = "PartyAb") %>% 
#     select(-DivisionNm, -tcp.percent)
#   
#   tmp_tcp_table <- derived_hor_tcp_type %>% 
#     filter(DivisionNm == division) %>% 
#     select(CandidateID, 12:19)
#   
#   names(tmp_tcp_table)[-1] <- gsub("^", "tcp.", names(tmp_tcp_table)[-1])
#   
#   tmp_table <- tmp_table %>% 
#     left_join(tmp_tcp_table, by = "CandidateID")
  
  return(tmp_totals)
  
}

division_total_percent <- function(division){
  
  ## FIXME: This would be more like the AEC's VTR display if it should percentages by column.
  
  tmp_votes <- division_total_votes(division)
  
  tmp_informal <- tmp_votes %>% 
    filter(Surname == "Informal")
  
  tmp_all_totals <- tmp_votes %>% 
    summarise_each(funs(sum(.,na.rm=TRUE)), -DivisionNm, -PartyAb, -Surname, -GivenNm, -Elected, -HistoricElected)
  
  tmp_informal <- tmp_informal %>% mutate(ordinary = ordinary / tmp_all_totals$total * 100,
                                          prepoll.ordinary = prepoll.ordinary / tmp_all_totals$total * 100,
                                          absent = absent / tmp_all_totals$total * 100,
                                          provisional = provisional / tmp_all_totals$total * 100,
                                          prepoll.dec = prepoll.dec / tmp_all_totals$total * 100,
                                          postal = postal / tmp_all_totals$total * 100,
                                          total = total / tmp_all_totals$total * 100
  )  
  
  tmp_formal_totals <- tmp_votes %>% 
    filter(Surname != "Informal") %>% 
    summarise_each(funs(sum(.,na.rm=TRUE)),-CandidateID, -DivisionNm, -PartyAb, -Surname, -GivenNm, -Elected, -HistoricElected)
  
  tmp_formal <- tmp_votes %>% 
    filter(Surname != "Informal") %>% 
    mutate(ordinary = ordinary / tmp_formal_totals$ordinary * 100,
           prepoll.ordinary = prepoll.ordinary / tmp_formal_totals$prepoll.ordinary * 100,
           absent = absent / tmp_formal_totals$absent * 100,
           provisional = provisional / tmp_formal_totals$provisional * 100,
           prepoll.dec = prepoll.dec / tmp_formal_totals$prepoll.dec * 100,
           postal = postal / tmp_formal_totals$postal * 100,
           total = total / tmp_formal_totals$total * 100)
    #mutate_each(funs(. / tmp_formal_totals$total * 100), -DivisionNm, -PartyAb, -Surname, -GivenNm, -Elected, -HistoricElected)
  
  tmp_table <- rbind(tmp_formal, tmp_informal) %>% select(-CandidateID)
  
  tmp_table <- tmp_table %>% 
    mutate_if(is.numeric, funs(round(., digits = 2)))
  
  tmp_table <- replace(tmp_table, is.na(tmp_table), 0)
  
  return(tmp_table)
  
}

division_tcp_votes <- function(division){
  tmp_div_tcp <- tcp_by_vote_type_by_division_percent() %>% 
    filter(DivisionNm == division) %>% 
    select(DivisionNm, PartyAb, Surname, GivenNm, Elected, HistoricElected, OrdinaryVotes:TotalVotes)
  return(tmp_div_tcp)
}

division_tcp_percent <- function(division){
  tmp_div_tcp_percent <- tcp_by_vote_type_by_division_percent() %>% 
    filter(DivisionNm == division) %>% 
    select(DivisionNm, PartyAb, Surname, GivenNm, Elected, HistoricElected, tpp.percent.OrdinaryVotes:tpp.percent.TotalVotes) %>% 
    mutate_if(is.numeric, funs(round(., digits = 2)))
  
  names(tmp_div_tcp_percent)[7:13] <- gsub("tpp.percent.", "", names(tmp_div_tcp_percent)[7:13])

  tmp_div_tcp_percent <- replace(tmp_div_tcp_percent, is.na(tmp_div_tcp_percent), 0)
  
  return(tmp_div_tcp_percent)
}

public_funding_group_hor <- function(){
  
  # Note that this is only by party. Certain independents will receive sufficient votes to
  # get public funding too. Maybe do a new function by candidates.
  
  tmp_funding_hor <- formal_hor_votes_by_party_by_div() %>% 
    left_join(formal_hor_votes_by_party_by_div() %>% 
                group_by(DivisionNm) %>% 
                summarise(total = sum(votes)), by = "DivisionNm") %>% 
    mutate(percent = votes/total * 100) %>% 
    filter(percent > 4.0) %>% 
    ungroup() %>% 
    select(PartyAb, votes) %>% 
    group_by(PartyAb) %>% 
    summarise(total.votes = sum(votes)) %>% 
    mutate(funding = total.votes * 262.784 /100) %>% 
    arrange(desc(funding))
  
  # Group the Coalition parties
  tmp_funding_hor$group <- as.character(tmp_funding_hor$PartyAb)
  tmp_funding_hor$group[tmp_funding_hor$PartyAb %in% c("LP", "LNP", "NP", "CLP")] <- "COAL"
  tmp_funding_hor$group <- as.factor(tmp_funding_hor$group)

  return(tmp_funding_hor)
}

seat_summary <- function(save_as_csv="N") {
  
  tmp_enrolments <- enrolment_div %>% 
    select(DivisionID, Enrolment)
  
  tmp_total <- derived_pp_all %>% 
    group_by(StateAb, DivisionID, DivisionNm) %>% 
    summarise(total.votes = sum(OrdinaryVotes)) %>% 
    left_join(tmp_enrolments, by = "DivisionID") %>% 
    mutate(turnout = total.votes / Enrolment * 100) %>% 
    left_join(derived_pp_all %>% 
                filter(HistoricElected == "Y") %>% 
                group_by(DivisionID) %>% 
                select(DivisionID, PartyAb) %>% 
                summarise(HistoricElected = first(PartyAb)), 
              by = "DivisionID") %>% 
    left_join(derived_pp_all %>% 
                filter(Elected == "Y") %>% 
                group_by(DivisionID) %>% 
                select(DivisionID, PartyAb) %>% 
                summarise(Elected = first(PartyAb)), 
              by = "DivisionID")
  
  tmp_tcp <- tcp_by_vote_type_by_division_percent()
  tmp_tcp_leading <- tmp_tcp %>% 
    filter(div.percent.TotalVotes >= 50) %>% 
    select(DivisionID, leading.party = PartyAb, leading.tcp = div.percent.TotalVotes, margin = margin.votes)
  
  tmp_tcp_lagging <- tmp_tcp %>% 
    filter(div.percent.TotalVotes < 50) %>% 
    select(DivisionID, lagging.party = PartyAb, lagging.tcp = div.percent.TotalVotes)
  
  tmp_tcp_all <- tmp_tcp_leading %>%
    left_join(tmp_tcp_lagging, by = "DivisionID")
  
  tmp_total <- tmp_total %>% 
    left_join(tmp_tcp_all, by = "DivisionID") %>% 
    select(-margin, margin) %>%
    left_join(hor_tcp_type %>% select(DivisionID, leading.party = PartyAb, Swing), by = c("DivisionID", "leading.party")) %>% 
    mutate_if(is.numeric, funs(round(., digits = 2)))
  
  if(save_as_csv == "Y"){
    filename <- paste("HoR-", format(vtr_time_hor, "%Y%m%d-%H%M"), ".csv", sep="")
    write.csv(tmp_total, file = filename, row.names = FALSE)
  }
    
  return(tmp_total)
}

tcp_comparison <- function() {
  
  # Compare tcp votes across vote types.
  # Should do another to do state-by-state.
  
  tmp_comp <- tcp_by_vote_type_by_division_percent()
  
  tmp_tcp_summary <- tmp_comp %>% 
    filter(PartyAb == "ALP") %>% 
    select(DivisionNm, PartyAb, starts_with("tpp.percent.")) %>% 
    summarise_if(is.numeric, funs(mean=mean(., na.rm=TRUE),sd=sd(., na.rm=TRUE), n=n())) %>% 
    gather(value="ALP")
  
  tmp_tcp_summary <- tmp_tcp_summary %>% 
    left_join(tmp_comp %>%
                filter(PartyAb %in% CoalitionAb) %>% 
                select(DivisionNm, PartyAb, starts_with("tpp.percent.")) %>% 
                summarise_if(is.numeric, funs(mean=mean(., na.rm=TRUE),sd=sd(., na.rm=TRUE), n=n())) %>% 
                gather(value="COAL"), by = "key")

  tmp_tcp_summary <- tmp_tcp_summary %>% 
    left_join(tmp_comp %>%
                filter(PartyAb == "GRN") %>% 
                select(DivisionNm, PartyAb, starts_with("tpp.percent.")) %>% 
                summarise_if(is.numeric, funs(mean=mean(., na.rm=TRUE),sd=sd(., na.rm=TRUE), n=n())) %>% 
                gather(value="GRN"), by = "key")

  tmp_tcp_summary <- tmp_tcp_summary %>% 
    separate(key, into = c("Type", "Stat"), "_")
  
  tmp_tcp_summary$Type <- gsub("tpp.percent.", "", tmp_tcp_summary$Type)
  
  return(tmp_tcp_summary %>% mutate_if(is.numeric, funs(round(., digits = 2))))

#   tcp_comp_leading <- tmp_comp %>% 
#     filter(div.percent.TotalVotes >= 50) 
#   names(tcp_comp_leading)[-1:-3] <- gsub("^", "leading.", names(tcp_comp_leading)[-1:-3])
#   tcp_comp_lagging <- tmp_comp %>% 
#     filter(div.percent.TotalVotes < 50) %>% 
#     select(-StateAb, -DivisionNm, -Enrolment, -fifty.percent.enrolment)
#   names(tcp_comp_lagging)[-1] <- gsub("^", "lagging.", names(tcp_comp_lagging)[-1])
#   tmp_comp_all <- tcp_comp_leading %>% 
#     left_join(tcp_comp_lagging, by = "DivisionID")
#   
#   tmp_comp_tbl <- tmp_comp_all %>% 
#     select(DivisionID, DivisionNm, 
#            leading.PartyAb, leading.OrdinaryVotes:leading.TotalVotes, leading.tpp.percent.OrdinaryVotes:leading.tpp.percent.TotalVotes,
#            lagging.PartyAb, lagging.OrdinaryVotes:lagging.TotalVotes, lagging.tpp.percent.OrdinaryVotes:lagging.tpp.percent.TotalVotes,
#            TotalVotes = leading.total.TotalVotes, Enrolment = leading.Enrolment)
#   
#   tmp_comp_tbl <- tmp_comp_tbl %>% 
#     mutate(diff.OrdinaryVotes = leading.OrdinaryVotes - lagging.OrdinaryVotes,
#            diff.PrepollOrdinary = leading.PrepollOrdinary - lagging.PrepollOrdinary,
#            diff.Absent = leading.AbsentVotes - lagging.AbsentVotes,
#            diff.Provisional = leading.ProvisionalVotes - lagging.ProvisionalVotes,
#            diff.PrePollVotes = leading.PrePollVotes - lagging.PrePollVotes,
#            diff.PostalVotes = leading.PostalVotes - lagging.PostalVotes,
#            diff.TotalVotes = leading.TotalVotes - lagging.TotalVotes)
}

update_hor_tables <- function(){
  get_hor_results()
  
  derived_pp_all <<- polling_place_results_by_candidate()
  derived_formal_votes_div <<- formal_hor_votes_by_party_by_div()
  derived_tcp_pp <<- tcp_by_party_by_pollingplace()
  derived_hor_tcp_type <<- tcp_by_vote_type_by_division()
  derived_div_total_type <<- vote_type_by_div_inc_ppord()
  derived_tcp_type <<- tcp_by_vote_type_by_division_percent()

}

update_hor_tables()

##
## Senate Analysis ----
##

get_sen_results <- function(){
  
  vtr_time_sen <<-  Sys.time()
  
  # Senate first preference by division
  sen_fp_div <<- get_vtr_file("SenateFirstPrefsByDivisionByVoteTypeDownload-20499.csv")
  
  # Senate first preference by group by vote type
  sen_fp_group_type <<- get_vtr_file("SenateFirstPrefsByStateByGroupByVoteTypeDownload-20499.csv")
  
  # Senate First preferences by state by group by vote type
  sen_fp_state_group_type <<- get_vtr_file("SenateFirstPrefsByDivisionByVoteTypeDownload-20499.csv")
  
}

get_sen_results()

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
    select(StateAb, GroupAb, TotalVotes, num_quotas) %>% 
    arrange(StateAb, desc(TotalVotes))
  
  return(tmp_sen_quotas)
}

# # To show both full and half senate quotas in the one table:
# tmp <- calculate_senate_quotas() %>% 
#   rename(quotas.hs=num_quotas) %>% 
#   left_join(calculate_senate_quotas(12) %>% select(StateAb, GroupAb, quotas.fs=num_quotas)) %>% 
#   filter(StateAb == "TAS") %>% 
#   arrange(desc(TotalVotes))
# 

# Senate quotas by candidate
senate_quotas_candidate <- function(vacancies){
  tmp_quotas <- sen_fp_state_group_type %>% 
    group_by(StateAb) %>% 
    summarise(StateTotal = sum(TotalVotes)) %>% 
    mutate(quota=ifelse(StateAb == "ACT"|StateAb == "NT", (StateTotal/3)+1, (StateTotal/(vacancies+1))+1))

  tmp_candidate_quotas <- sen_fp_state_group_type %>% 
    group_by(StateAb, Ticket, BallotPosition, PartyAb, PartyName, CandidateDetails, Elected, HistoricElected) %>% 
    summarise(OrdinaryVotes = sum(OrdinaryVotes),
              AbsentVotes = sum(AbsentVotes),
              ProvisionalVotes = sum(ProvisionalVotes),
              PrePollVotes = sum(PrePollVotes),
              PostalVotes = sum(PostalVotes),
              TotalVotes = sum(TotalVotes)) %>% 
    left_join(tmp_quotas, by = "StateAb") %>% 
    mutate(Quotas = TotalVotes/quota) %>% 
    ungroup() %>% 
    select(StateAb:TotalVotes, Quotas)
  
  return(tmp_candidate_quotas)
}

# # Copy a table to the clipboard
# write.table(tmp, "clipboard", sep="\t", row.names=FALSE)
