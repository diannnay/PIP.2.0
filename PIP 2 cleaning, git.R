
# rm(list=ls())

library(tidyverse)
library(dplyr)
library(readr)

# Organization  - READ before running the script  -----------------------------------------
getwd()
# All cleaning results files should be stored in "Clinical results" folder, by date" 
# "C:\Users\Diana\OneDrive - University Of Massachusetts Medical School\FMCH\PIP, Dan Mullin\DATA\Analysi and cleaning\Cleaning results"
# Analyzis code is taking files from there. 

# Step 1. reading the file as it came from Qx. 
pip<- as_tibble (read_csv("C:\\Users\\Diana\\OneDrive - University Of Massachusetts Medical School\\FMCH\\PIP, Dan Mullin\\DATA\\downloaded\\2023\\PIP.download.Jan.03.2023_labels.csv"))

#Step 2. saving data dictionary 
Dict <- as.data.frame(cbind(colnames(pip), as.character(pip[1,]))) #Variable names are in column names, codebook is in first row
write.csv(Dict, "temp.dictionary.csv")

#Step 3. Saving the dataset with correctly identified data types.
temp.file <- pip[-(1:2),] # this makes all variables as character vars. 
write.csv(temp.file,"temp.file.csv") #write the short dataframe. 
temp.file.2 <- read_csv("temp.file.csv") #let dplyr be smart by importing short dataframe again and identifying  the correct variable types. 
temp.file.2 <- subset(temp.file.2, select = -c(`...1`) )
pip<-temp.file.2

#   colnames(pip)
#   str(pip)
#   head(pip)
#   dim(pip)

# Data formatting  -----------------------------------------
  
  #1. Renaming  inconvenient variables 
  pip <-pip %>%
    rename(cons = `QID69`, 
           email =`QID62`,
           exp = `Real vs. Experiment`,
           date = EndDate, 
           dur=`Duration (in seconds)`,
           pr.name =`D1`,
           pr.group = `D2`,
           pr.org =`D3`,
           pr.org.other = `D3_17_TEXT`,
           pr.specialty =`D4`,
           pr.specialty.other =`D4_6_TEXT`,
           size.cat =`D5`,
           size.num =`D5_5_TEXT`,
           zip =`D6`,
           state = `D7`, 
           bh.percent =`D8`,
           position = `D9`, 
           position.other = `D9_7_TEXT`,
           work.yrs = `D10`,
           response.type = `D11`,
           pr.group.other = `D2_4_TEXT`
           )
           
  # Changing variables' format: 
  
  pip$cons <- as.factor(pip$cons) #tranmsferring from numeric to factor
  # typeof(pip$date)
  pip$date<-mdy_hm(pip$date) # transferring from str to dttm
 
  pip <- pip %>%
    replace_na(list(exp = "na")) 
  
  pip$exp <-as.factor(pip$exp)
  pip$pr.specialty <- as.factor(pip$pr.specialty)
  pip$pr.group <- as.factor(pip$pr.group)
  pip$size.num <-as.numeric(pip$size.num)
  pip$size.cat <- as.factor(pip$size.cat)
  pip$state  <- as.factor (pip$state)
  pip$pr.group.other<- str_to_lower(pip$pr.group.other)
  pip$email <- str_to_lower(pip$email)
  pip$pr.org.other <- str_to_lower(pip$pr.org.other)
  pip$pr.specialty.other <- str_to_lower(pip$pr.specialty.other)
  pip$position.other <- str_to_lower(pip$position.other)
  pip$pr.name <- str_to_lower(pip$pr.name)
  pip$position.other <- str_to_lower(pip$position.other)
  
  # Replacing missing data in str variables - WHY?  
  
  str_replace_na(pip$pr.org.other)
  str_replace_na(pip$pr.specialty.other)
  str_replace_na(pip$size.num)
  str_replace_na(pip$position.other)
  
  # Removing NAs - WHY? 
    
   #  pip <- pip %>%
   #  replace_na( list (rve = 3, D2= 0)) 
   #  # preview to check
   # pip$rve    
   # 
   # Cleaning data set and saving numbers for cleaning diagram: 
  
  
  # Cleaning steps  -----------------------------------------
 
  
  s.0 <- nrow(pip) 
  pip.1<-pip %>%
   filter (cons == "I give my assent to participate in the PIP 2.0"| is.na(cons))
 
  table (pip.1$cons)
  s.1 <- nrow(pip.1)
  
  
  pip.2<-pip.1 %>%
        filter (exp =='I want my "real" PIP 2.0 score for my practice' | exp =="na") 
  
  table (pip.2$exp )
    s.2.real <-sum(pip.2$exp == 'I want my "real" PIP 2.0 score for my practice') 
  
    s.2.na <-sum(pip.2$exp == 'na')
  s.2<-s.2.na+s.2.real
  
# pip$Progress 
  pip.3<-pip.2 %>%
    filter (Progress >= 20) 
  table(pip.3$Progress)
    s.3 <- nrow(pip.3)
  
# pip.3$dur
  pip.4<-pip.3 %>%
      filter (dur > 200) 
  
    s.4 <- nrow(pip.4)
 
# pip.4$pr.group
levels (pip.4$pr.group)
  pip.5<-pip.4 %>%
      filter (pr.group != "ASU DBH" & pr.group != "UH IBH Course" ) 
  
    s.5 <-nrow(pip.5)
  
# pip.5$pr.name
pip.6.names <-pip.5 %>%
  filter (!grepl ("test", pr.name) )%>%
  filter (!grepl ("asu", pr.name)) %>%
  filter (!grepl ("dbh", pr.name)) %>%
  filter (!grepl ("made up", pr.name)) %>%
  filter (!grepl ("scenario", pr.name)) %>%
  filter (!grepl ("scen.rio", pr.name))%>%
  filter (!grepl ("example", pr.name))  %>%
  filter (!grepl ("dbh", pr.name) ) %>%
  filter (!grepl ("fjfjgfjg", pr.name) ) %>%
  filter (!grepl ("course", pr.name) ) 

(pip.6.names$pr.name)
s.6 <- nrow(pip.6.names)

pip.7 <-pip.6.names %>%
  filter (!grepl ("test", pr.group.other) )%>%
  filter (!grepl ("asu", pr.group.other)) %>%
  filter (!grepl ("dbh", pr.group.other)) %>%
  filter (!grepl ("made up", pr.group.other)) %>%
  filter (!grepl ("scenario", pr.group.other)) %>%
  filter (!grepl ("scen.rio", pr.group.other))%>%
  filter (!grepl ("example", pr.group.other))  %>%
  filter (!grepl ("dbh", pr.group.other) ) %>%
  filter (!grepl ("course", pr.group.other) ) 

s.7 <- nrow(pip.7)


pip.8 <-pip.7 %>%
  filter (!grepl ("diana.rinker@umasmed.edu", email) )%>%
  filter (!grepl ("mpmarti6@asu.edu", email)) %>%
  filter (!grepl ("benjamin.Littenberg@uvm.edu", email)) %>%
  filter (!grepl ("constance.van-eeghen@med.uvm.edu", email)) %>%
  filter (!grepl ("gail.rose@uvm.edu", email)) %>%
  filter (!grepl ("rodger.kessler@cuanschutz.edu", email))%>%
  filter (!grepl ("crmacchi@asu.edu", email))  %>%
  filter (!grepl ("mindy.mcentee@asu.edu", email) ) %>%
  filter (!grepl ("daniel.mullin@umassmed.edu", email) ) %>%
  filter (!grepl ("aauxier5@gmail.com", email) ) %>%
  filter (!grepl ("tara.weldon@icloud.com", email) ) 

s.8 <- nrow(pip.8)

# pip.9$pr.specialty.other

pip.8$pr.specialty.other

pip.9 <-pip.8 %>%
  filter (!grepl ("test", pr.specialty.other) )%>%
  filter (!grepl ("asu", pr.specialty.other)) %>%
  filter (!grepl ("dbh", pr.specialty.other)) %>%
  filter (!grepl ("made up", pr.specialty.other)) %>%
  filter (!grepl ("scenario", pr.specialty.other)) %>%
  filter (!grepl ("scen.rio", pr.specialty.other))%>%
  filter (!grepl ("example", pr.specialty.other))  %>%
  filter (!grepl ("dbh", pr.specialty.other) ) %>%
  filter (!grepl ("course", pr.specialty.other) ) 


s.9<- nrow(pip.9)

# pip.9$position.other

pip.10 <-pip.9 %>%
  filter (!grepl ("test", position.other))%>%
  filter (!grepl ("asu", position.other)) %>%
  filter (!grepl ("dbh", position.other)) %>%
  filter (!grepl ("made up", position.other)) %>%
  filter (!grepl ("scenario", position.other)) %>%
  filter (!grepl ("scen.rio", position.other))%>%
  filter (!grepl ("example", position.other))  %>%
  filter (!grepl ("dbh", position.other)) %>%
  filter (!grepl ("course", position.other)) 


s.10 <- nrow(pip.10)

# pip.10$pr.org.other

pip.11 <-pip.10 %>%
  filter (!grepl ("test", pr.org.other))%>%
  filter (!grepl ("asu", pr.org.other)) %>%
  filter (!grepl ("dbh", pr.org.other)) %>%
  filter (!grepl ("made up", pr.org.other)) %>%
  filter (!grepl ("scenario", pr.org.other)) %>%
  filter (!grepl ("scen.rio", pr.org.other))%>%
  filter (!grepl ("example", pr.org.other))  %>%
  filter (!grepl ("dbh", pr.org.other)) %>%
  filter (!grepl ("course", pr.org.other)) 

s.11 <- nrow(pip.11)


# pip.11$Progress =100
pip.12<-pip.11 %>%
  filter (Progress == 100) 
s.12<- nrow(pip.12)


  final_numbers <- c(s.0, s.1, s.2, s.3, s.4, s.5, s.6, s.7, s.8, s.9, s.10, s.11, s.12)
  final_labels <- c("raw", "step 1", "step 2", "step 3", "step 4", "step 5", "step 6","step 7","step 8","step 9","step 10","step 11","step 12" )
  
  report.pip <- data.frame (final_labels,
                        final_numbers) # see finished code in DI dataset

  # write.csv (report.pip, file = "C:\\Users\\Diana\\OneDrive - University Of Massachusetts Medical School\\FMCH\\PIP, Dan Mullin\\Qualtrics data\\Analysi and cleaning\\Cleaning results\\9.14.22\\PIP.Cleaning summary.DR,9.14.22.csv")     
  
  # write.csv (pip.11, file = "C:\\Users\\Diana\\OneDrive - University Of Massachusetts Medical School\\FMCH\\PIP, Dan Mullin\\Qualtrics data\\Analysi and cleaning\\Cleaning results\\9.14.22\\PIP.Cleaned data(R).DR,9.14.22.csv")    
  
  #_____________________ END   OF CLEANING SCRIPT ____________________
  
  
  # Unique practices-DONE  -----------------------------------------
  
  
  #### p.name column for all records is summative of External reference,  practice name,pactice name _other ,  practice group, practice group_other. 
  
  # This whole section is for identifying unique records. Once its done, document is saved and re-used for future pulls.  The secion is commented out. 
 
  # filling External Reference column with practice names, when empty:
#  
#   groups <-pip.12
# #   
#   groups$p.name  <-  ifelse (is.na(groups$p.name), groups$pr.name, groups$ExternalReference)
#   groups$p.name  <-  ifelse (is.na(groups$p.name ), paste(groups$pr.group, groups$pr.group.other, sep=" "), groups$p.name )
#   groups <- select (groups, ExternalReference, pr.name, p.name, email, ResponseId )
#   
#  
#   
#   # Group.summary has group IDs based on external reference name and practice name
#   groups.summary <-groups %>%
#     group_by (p.name)%>%
#     summarise (name.count = n())
#   
#   groups.summary<- transform (groups.summary, group.id = as.numeric(factor(p.name))) # CREATING `GROUP.ID` here 
#   
#   groups <-groups %>%
#     left_join (groups.summary, c( "p.name" ="p.name"))
#    groups <- select (groups, p.name, email, ResponseId,group.id )
#   
#   unique.practices <-groups %>%
#     group_by (p.name, group.id)%>%
#     summarise (count = n())
#   write.csv (unique.practices, file = "C:\\Users\\Diana\\OneDrive - University Of Massachusetts Medical School\\FMCH\\PIP, Dan Mullin\\Qualtrics data\\Analysi and cleaning\\Cleaning results\\10.7.22 for manual cleaning\\PIP.summary.BEfFORE manual cleaning, DR, 10.12.22.csv") - #why am i exporting it? to be able to compare for after hand cleaning 
#   
#   # To export for manual cleaning: 
# pip.all.record <-groups %>%
#     group_by (p.name, email,ResponseId, group.id)%>%
#     summarise (count = n())
#   write.csv (pip.all.record, file = "C:\\Users\\Diana\\OneDrive - University Of Massachusetts Medical School\\FMCH\\PIP, Dan Mullin\\Qualtrics data\\Analysi and cleaning\\Cleaning results\\10.7.22 for manual cleaning\\PIP.all recds. FOR manual cleaning, DR, 10.14.22.csv")    
#   
# Manual cleaning
# Make sure each group id connected with record ID
  
  
# Group IDs  -adding old (previously manually identified):  -----------------------------------------
 
  
  pip.cleaned.ids<- as_tibble (read_csv("PIP.all recds.handcleaned TO KEEP  DR and  DM, 10.20.22.csv"))  
  
  # pip.cleaned.ids <- subset(pip.cleaned.ids, select = -c(email, count) )

  
  # Adding corrected group_IDs to all records: 
pip.new.id <-pip.12 %>%
        left_join (pip.cleaned.ids, c( "ResponseId" ="ResponseId"))

# New records - group IDs  -----------------------------------------
#### View only new records   -----------------------------------------
new.records <- pip.new.id %>%
      filter(is.na(group.id))%>%
      select (`date`, `ResponseId`, `RecipientFirstName`, `RecipientFirstName`,`ExternalReference`, pr.name, pr.group,pr.group.other, p.name, everything() )

#### Populating p.name for new records   -----------------------------------------
new.records$p.name  <-  ifelse (is.na(new.records$`ExternalReference`), new.records$pr.name, new.records$ExternalReference)
new.records$p.name  <-  ifelse (is.na(new.records$p.name ), paste(new.records$pr.group, new.records$pr.group.other, sep=" "), new.records$p.name )


pip.new.id <-pip.new.id%>% 
        arrange(group.id)

#Manually entering ID values for new records: 
pip.new.id$group.id[170:186]
pip.new.id$group.id[185]<-121
pip.new.id$group.id[185]
pip.new.id$group.id[186]<-122
pip.new.id$group.id[186]

# Fxing p.name
pip.new.id$p.name  <-  ifelse (is.na(pip.new.id$p.name), pip.new.id$ExternalReference, pip.new.id$p.name)
pip.new.id$p.name  <-  ifelse (is.na(pip.new.id$p.name), pip.new.id$pr.name, pip.new.id$p.name)
pip.new.id$p.name  <-  ifelse (is.na(pip.new.id$p.name), paste(groups$pr.group, groups$pr.group.other, sep=" "), pip.new.id$p.name)
# checking if code worked correctly
# pip.new.id<- pip.new.id[170:186,]
# a<-select(pip.new.id, group.id, p.name, `ExternalReference`, `pr.name`,`pr.group`, `pr.group.other` )
pip.new.id$p.name[121:122]

# One group per row                         
groups.summary <- pip.new.id%>%
  group_by (group.id)%>%
  summarise (count = n())

# Groups with the names column : 
groups.summary <- pip.new.id%>%
  group_by (group.id, p.name  )%>%
  summarise (count = n())


# Checking repeats by email: 
#This section also commented out, as it was part of identifying uniqe practices 
#                   # To see how many groups per each email: only some emails should have few groups in in (for Martha)
#                         a <- pip.all.id %>%
#                                   group_by (email, group.id )%>%
#                                   summarise (count = n())
#                         a <- subset( a, select = -c(count) )
#                               
#                   # To see email that have few IDs, I count IDs
#                         emails.with.few.practices  <- a %>%
#                                   group_by (email )%>%
#                                   summarise (count.IDs = n( ))%>%
#                                   filter(count.IDs>1)
# # write.csv ( emails.with.few.practices , file = "C:\\Users\\Diana\\OneDrive - University Of Massachusetts Medical School\\FMCH\\PIP, Dan Mullin\\Qualtrics data\\Analysi and cleaning\\Cleaning results\\10.7.22 for manual cleaning\\PIP. emails.with.few.practices , DR, 10.7.22.csv")     # review manually, correcting manual document, re-import.
# 
# # Number of groups and records in each group:
# group.summary.short  <- pip.all.id %>%
#             group_by (group.id )%>%
#             summarise (count = n())



### all records:   -----------------------------------------
#this dataset to use for averaging group scores. All records in each group are here
dim(pip.new.id)

write.csv (pip.new.id, file = "C:\\Users\\Diana\\OneDrive - University Of Massachusetts Medical School\\FMCH\\PIP, Dan Mullin\\Qualtrics data\\Analysi and cleaning\\Cleaning results\\11.17.22\\PIP.groups.all.csv") 


# One record per group: ---------------------------------

### by last date:   -----------------------------------------
#This dataset only has one record per grpoup - last date. 
pip.groups.Last<- pip.new.id %>%
          group_by(group.id)%>%
          arrange(desc(date))%>%
          filter(row_number()==1)

pip.groups.Last.summary <- pip.groups.Last%>%
  group_by (group.id,p.name,`ResponseId`, date)%>%
  summarise (count = n())


write.csv (pip.groups.Last, file = "C:\\Users\\Diana\\OneDrive - University Of Massachusetts Medical School\\FMCH\\PIP, Dan Mullin\\Qualtrics data\\Analysi and cleaning\\Cleaning results\\11.17.22\\PIP.groups.last.date.csv")  


### by first date:   -----------------------------------------
#This dataset only has one record per grpoup - last date. 
pip.groups.first<- pip.new.id %>%
  group_by(group.id)%>%
  arrange(date)%>%
  filter(row_number()==1)

pip.groups.first.summary <- pip.groups.first%>%
  group_by (group.id,p.name,`ResponseId`, date)%>%
  summarise (count = n())


write.csv (pip.groups.Last, file = "C:\\Users\\Diana\\OneDrive - University Of Massachusetts Medical School\\FMCH\\PIP, Dan Mullin\\Qualtrics data\\Analysi and cleaning\\Cleaning results\\11.17.22\\PIP.groups.first.date.csv")  



  





# extra __ By email and IP address   -----------------------------------------
   # creating base table for Master 
  master <-pip.12 %>%
    group_by (ExternalReference, pr.name, email)%>%
    summarise (name.count = n())
    # colnames(pip.12)
    # By email address only,  >2 in a group 
  pip.email.groups <-pip.12 %>%
    group_by (email)%>%
    summarise (email.count = n())%>%
    filter (email.count>1)
  
  
  
  # By IP address,  >2 in a group 
  pip.ip.groups <-pip.12 %>%
    group_by (IPAddress, email)%>%
    summarise (ip.count = n())%>%
    filter (ip.count>1)

  
#this summary tells me how many repeating emails contain repeating ip addresses (when >1).  
  pip.groups <-pip.email.groups %>%
    full_join (pip.ip.groups, c( "email" ="email"))
  
  
  ### By practice name   ----------------------------------------- 
# approaching by practice name: 
  
  

  ##### Exref IDs   -----------------------------------------   

  

      
 
  # Grouping and adding exref IDs.
  pip.ex.ref <-pip.12 %>%
        group_by (ExternalReference)%>%
        summarise (ex.ref.count = n())
  group.id <- transform (pip.ex.ref, group.id = as.numeric(factor(ExternalReference)))
  
  # Creating Master list. Adding exref. group IDs. 
  master<- master %>%
        left_join(group.id, c("ExternalReference" ="ExternalReference"))
   
  master<- master %>%
     select (ExternalReference, pr.name, email, group.id )
  
  
  str(master)
  ##### Exref IDs   ----------------------------------------- 
  
  
  # List of Practice names and emails , grouped: 
  pip.pname <-pip.12 %>%
    group_by (pr.name)%>%
    summarise (name.count = n()) 
  pname.id <- transform (pip.pname, pname.id = as.numeric(factor(pr.name)))
  
  master<- master %>%
    left_join(pname.id , c("pr.name" ="pr.name"))
  master<- master %>%
    select (ExternalReference, pr.name, email, group.id, pname.id ) 
  
  
  # NEED TO CONTNUE id NUMBERTING AFTER EX.REF.id TO PNAME.id, SO THEY DONT REPEAT 
  
  # Summarizing by IDs
  
  Gr.summary <-  master %>%
    group_by (ExternalReference, group.id,pr.name, pname.id)%>%
    summarise (count = n()) 
  
  
  
  
 
 