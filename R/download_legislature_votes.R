# Run this file first 
# All NJ Legislature Site: https://www.njleg.state.nj.us/downloads.asp
# Senate / Assembly / Committee Votes ftp://www.njleg.state.nj.us/votes

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Usage example
packages<-c("Hmisc", "tidyverse", "lubridate", "RCurl", "purrrlyr", "readr", "plotly")
check.packages(packages)

get_vote_record <- function(TYPE, YEAR.SEQUENCE) {
  # TYPE <- c("A","CA", "S", "CS")
  # YEAR.SEQUENCE  -    2010:2019 or c("2010-2011", "2012-2013", "2014-2015", "2016-2017", "2018-2019")
  YEAR.SEQUENCE %>% map_df(function(YEAR){
    ftpfilename <- paste0("ftp://www.njleg.state.nj.us/votes/",TYPE,YEAR,".zip")
    zipfile <- paste0("./DATA/",TYPE,YEAR,".zip")
    txtfile <- paste0("./DATA/",TYPE,YEAR,".txt")
    download.file(ftpfilename, destfile = zipfile )
    unzip(zipfile, exdir = "./DATA")
    data.out <- read_csv(txtfile) %>% data.frame() %>% 
      mutate(Year = as.numeric(substr(YEAR,1,4)))
    data.out
  })
}

dir.create("./DATA", showWarnings = FALSE)

assembly_votes <- get_vote_record("A", 2010:2019)

assembly_committee_votes <- get_vote_record("CA", c("2010-2011", "2012-2013", "2014-2015", "2016-2017", "2018-2019"))

senate_votes <- get_vote_record("S", 2010:2019)

senate_committee_votes <- get_vote_record("CS",c("2010-2011", "2012-2013", "2014-2015", "2016-2017", "2018-2019"))

dir.create("./MDB_DATA", showWarnings = FALSE)
#setwd("MDB_DATA")
DB.YEARS <- seq(2010,2018, by = 2)
url.mdb <- "ftp://www.njleg.state.nj.us/ag"

MainBill <- DB.YEARS %>% map_df(.f=function(YEAR){
  filename <- paste0(url.mdb, "/", YEAR,"data/DB",YEAR,".zip")
  zipfile <- paste0("./MDB_DATA/DB",YEAR, ".zip")
  mdbfile <- paste0("./MDB_DATA/DB",YEAR, ".mdb")
  download.file(filename, destfile = zipfile)
  unzip(zipfile, exdir = "./MDB_DATA")
  one_year_db <- mdb.get(paste0("./MDB_DATA/DB", YEAR, ".mdb"))
  one_year_db[[10]] %>% mutate(Year = YEAR)
  
})  %>% select(Year, Bill = ActualBillNumber, Abstract, Synopsis) %>% 
  mutate(Bill = word(Bill, 1)) 


bucco_assembly_votes_w_synopsis <- assembly_votes %>% 
  filter(Full_Name == "Bucco, Anthony M.") %>% 
  mutate(Session_Date = mdy(Session_Date)) %>%     
  mutate(Year = floor(Year /2) * 2 ) %>% 
  left_join(MainBill %>% select(-Abstract)) %>% 
  distinct() %>% 
  select(Bill, Year, Synopsis, Bucco = Legislator_Vote )

Votes_all_years <- assembly_votes %>%                           # assembly_votes from download files 
  filter(Action == "3RDG FINAL PASSAGE") %>% 
  group_by(Bill, Year, Abstract, Legislator_Vote) %>% 
  dplyr::summarize(Count = n()) %>% ungroup() %>%   
  spread(key = Legislator_Vote, value = Count) %>% 
  mutate(Year = floor(Year /2) * 2 ) %>% distinct() %>% 
  left_join(bucco_assembly_votes_w_synopsis) %>% distinct()  %>% 
  mutate(Bill_Synopsis = paste(Bill, Synopsis), 
         Bill_Synopsis =  gsub('(.{1,30})(\\s|$)', '\\1\n', Bill_Synopsis)) 

save(Votes_all_years, file="Votes_all_years.RData")  # use this .Rmd files and write a yaml file to 
# generate a site for netlify 

