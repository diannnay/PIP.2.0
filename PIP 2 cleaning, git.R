
# rm(list=ls())

library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(tibble)

# Organization  - READ before running the script  -----------------------------------------
getwd()
# All cleaning results files should be stored in "Clinical results" folder, by date" 
# "C:\Users\Diana\OneDrive - University Of Massachusetts Medical School\FMCH\PIP, Dan Mullin\DATA\Analysi and cleaning\Cleaning results"
# Analyzis code is taking files from there. 

# Step 1. reading the file as it came from Qx. 
pip<- as_tibble (read_csv("C:\\Users\\Diana\\OneDrive - University Of Massachusetts Medical School\\FMCH\\PIP, Dan Mullin\\DATA\\downloaded\\2023\\PIP.download.Jan.03.2023_labels.csv"))

#Step 2. saving data dictionary 
Dict <- as.data.frame(cbind(colnames(pip), as.character(pip[1,]))) #Variable names are in column names, codebook is in first row
write_csv(Dict, "temp.dictionary.csv")

#Step 3. Saving the dataset with correctly identified data types.
temp.file <- pip[-(1:2),] # this makes all variables as character vars. 
write_csv(temp.file,"temp.file.csv") #write the short dataframe. 
temp.file.2 <- read_csv("temp.file.csv") #let dplyr be smart by importing short dataframe again and identifying  the correct variable types. 
# temp.file.2 <- subset(temp.file.2, select = -c(`...1`) )
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
  
  
  
# I. Adding manually identified group.id to all records:  -----------------------------------------
 
cleaned.ids<- as_tibble (read_csv("PIP.all recds.handcleaned TO KEEP  DR and  DM, 10.20.22.csv"))  
# pip.cleaned.ids <- subset(pip.cleaned.ids, select = -c(email, count) )
colnames(cleaned.ids) 
  
   cleaned.ids.1<- cleaned.ids%>%
    select(group.id, p.name, ResponseId) 

# shortening p.12 data for easy checking
     main.short<- pip.12  
     # %>%
      # select(ResponseId, ExternalReference , pr.name,pr.group, pr.group.other, date, `RecipientFirstName`, `RecipientLastName`)

# Adding group_IDs to all records: 
     main.short <-main.short %>%
     left_join (cleaned.ids.1, c( "ResponseId" ="ResponseId"))  
  # added 2 new variables: group.id and p.name. New records are still missing  group id and p.name 

     main.short$group.id

# II.  # New records   ----------------------------------------- 
     ## Generating p.name for all records including new   -----------------------------------------  
      
main.short$p.name  <-  ifelse (is.na(main.short$`ExternalReference`), main.short$pr.name, main.short$ExternalReference)
main.short$p.name  <-  ifelse (is.na(main.short$p.name ), paste(main.short$pr.group, main.short$pr.group.other, sep=" "), main.short$p.name )
main.short$p.name  <-  ifelse (is.na(main.short$p.name), paste(main.short$pr.group, main.short$pr.group.other, sep=" "), main.short$p.name)
      
  
    ## Assigning IDs from previously existing external reference  ----------------------------------------

ext.ref.list.exist<- main.short %>%
  filter(!is.na(ExternalReference) & !is.na(group.id) )%>% 
  group_by(ExternalReference, group.id)%>%
  select ( group.id, ExternalReference)%>%
    filter(row_number()==1)

  main.short<- main.short%>%
      left_join (ext.ref.list.exist,c("ExternalReference" ="ExternalReference"))%>%
      rename (ex.ref.group.id = group.id.y, 
                    group.id = group.id.x)
  
main.short$group.id
main.short$ex.ref.group.id
  
  for (i in 1:length(main.short$group.id)) {
    if (is.na(main.short$group.id[i]  & !is.na(main.short$ex.ref.group.id[i]) )) {
      main.short$group.id[i]<- main.short$ex.ref.group.id[i]
                                            }
                                        }
main.short<- subset(main.short , select = -c(`ex.ref.group.id`) )
colnames(main.short)
   
  ## New IDS for new ExternalReference group   -----------------------------------------
ext.ref.list<- main.short %>%
    filter(!is.na(ExternalReference))%>% 
    group_by(ExternalReference, group.id)%>%
    select ( group.id, ExternalReference)%>%
    filter(row_number()==1)
  
temp<-main.short %>%
    arrange(group.id)%>%
    filter(group.id<100000)
main.short$group.id  
temp$group.id
  all.ids <- na.omit(unique(c(temp$group.id)))

  
  for (i in 1:length(ext.ref.list$group.id)) {
    if (is.na(ext.ref.list$group.id[i])) { 
      new.id <- max(all.ids) + 1
      ext.ref.list$group.id[i] <- new.id
      all.ids <- append(all.ids, new.id)
    }
  } 
  
  main.short<- main.short%>%
    left_join (ext.ref.list,c("ExternalReference" ="ExternalReference"))          %>%
    rename (ex.ref.group.id = group.id.y, 
            group.id = group.id.x)
   
  main.short$group.id
  main.short$ex.ref.group.id
  
  for (i in 1:length(main.short$group.id)) {
    if (is.na(main.short$group.id[i]  & !is.na(main.short$ex.ref.group.id[i]) )) {
      main.short$group.id[i]<- main.short$ex.ref.group.id[i]
    }
  }
   main.short<- subset(main.short , select = -c(`ex.ref.group.id`) )
    colnames(main.short)

# Creating new IDs for the rest of NAs    -----------------------------------------

for (i in 1:length(main.short$group.id)) {
  if (is.na(main.short$group.id[i])) { 
                  new.id <- max(all.ids) + 1
                  main.short$group.id[i] <- new.id
                  all.ids <- append(all.ids, new.id)
                                        }
}
    #Final check: 
main.short$group.id


#__________________________________END_____________________________________


# One group per row                         
groups.summary <- main.short%>%
  group_by (group.id)%>%
  summarise (count = n())

# Groups with the names column : 
groups.summary <- main.short%>%
  group_by (group.id, p.name  )%>%
  summarise (count = n())


### All records:   -----------------------------------------

dim(main.short)

write_csv (main.short, file = "PIP.all records.csv") 


# One record per group: ---------------------------------

### by last date:   -----------------------------------------
#This dataset only has one record per group - last date. 
pip.groups.last<- main.short %>%
          group_by(group.id)%>%
          arrange(desc(date))%>%
          filter(row_number()==1)

dim(pip.groups.last)
pip.groups.last.summary <- pip.groups.last%>%
  group_by (group.id,p.name,`ResponseId`, date)%>%
  summarise (count = n())


write_csv (pip.groups.last, file = "pip.last per group.csv")  

# 
# ### by first date:   -----------------------------------------
# #This dataset only has one record per grpoup - last date. 
# pip.groups.first<- pip.new.id %>%
#   group_by(group.id)%>%
#   arrange(date)%>%
#   filter(row_number()==1)
# 
# pip.groups.first.summary <- pip.groups.first%>%
#   group_by (group.id,p.name,`ResponseId`, date)%>%
#   summarise (count = n())
# 
# 
# write.csv (pip.groups.Last, file = "C:\\Users\\Diana\\OneDrive - University Of Massachusetts Medical School\\FMCH\\PIP, Dan Mullin\\Qualtrics data\\Analysi and cleaning\\Cleaning results\\11.17.22\\PIP.groups.first.date.csv")  

  
  
  
 
 