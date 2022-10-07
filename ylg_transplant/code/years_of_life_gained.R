setwd("~/Opioid_project/ylg_transplant")
source("functions.R")
library(tidyverse)
library("readxl")


##### --------------------------------------------------------------------------
#--------- Import data
##### --------------------------------------------------------------------------

### Select desired organ and year
selectYear <- "2020"
selectOrgan <- "liver" #select liver, kidney, heart or lung
lifetableYear <- "2019" #lifetable data is delayed, as of 04/22 most recent LT data is 2019

### Import data
if (TRUE) {
  # Imports NVSS lifetable from the year set by variable 'lifetableYear'
  df.lifetable.nvss <- read.csv(file = paste("data/Lifetable",lifetableYear,".csv",
                                          sep=''),
                             header = TRUE) 
  # Remove commas from NVSS data and drop NAs
  for (i in 3:ncol(df.lifetable.nvss)){
    df.lifetable.nvss[,i] <- as.numeric(as.character(gsub(",","", 
                                                       df.lifetable.nvss[,i])))
  }
  df.lifetable.nvss <- df.lifetable.nvss %>%
    filter(!is.na(Ages))
  
  # Waitlist mortality data (death rate by age and by year from 2006 to 2020)
  df.waitlist.mortality <- read.csv(file = paste("data/SRTR/Waitlist.Mortality.",
                                                 selectOrgan, "2020.csv",
                                                 sep = ""),
                                    header = TRUE)
 
  
  # Post-transplant survival rate (death rate by age and by year post-transplant)
  # 0-5 year post-transplant survival from 2020 report (transplants performed in 2013-2015)
  df.transplant.survival <- read.csv(file = paste("data/SRTR/Survival.", 
                                                  selectOrgan, ".2020.csv", 
                                                    sep = ""), header = TRUE)
    
  
  ### Format transplant recipient data -------------------
  df.recipients <- read_xlsx("data/OPTN_data.xlsx",
                             range = "B8:AD140",
                             col_types=c(c("text", "text"), rep("numeric", 27))) 
  #import range selects 2018 and 2019 and 2020
  df.recipients <- df.recipients %>%
    rename(donorMOD = 'MOD', recipientAges = ...1) %>%
    filter(recipientAges != "Total") 
  
  # create vector from Recipient ages column and remove the "Recipient Age at Transplant"
  ages.vec <- df.recipients %>%
    filter(recipientAges != "Recipient Age at\r\nTransplant")
  ages.vec <- ages.vec$recipientAges 
  # Remove NAs lines due to merged rows in the xlsx file and replace the now 
  # screwed up age column with the ages.vec
  df.recipients <- na.omit(df.recipients)
  df.recipients$recipientAges <- ages.vec
  
  # select columns with 2019 data and rename columns to labels that actually make sense
  df.recipients2019 <- df.recipients[,12:20]
  df.recipients2019$donorMOD <- df.recipients$donorMOD
  df.recipients2019$recipientAges <- df.recipients$recipientAges
  df.recipients2019 <- df.recipients2019 %>%
    rename(heartLung = `Heart-Lung...12`, heart = `Heart...13`, 
           intestine = `Intestine...14`, kidney = `Kidney...15`, 
           kidneyPancreas = `Kidney-Pancreas...16`, liver = `Liver...17`,
           lung = `Lung...18`, pancreas = `Pancreas...19`, total = `...20`)
  
  df.recipients2018 <- df.recipients[,1:11]
  df.recipients2018$donorMOD <- df.recipients$donorMOD
  df.recipients2018$recipientAges <- df.recipients$recipientAges
  df.recipients2018 <- df.recipients2018 %>%
    rename(heartLung = `Heart-Lung...3`, heart = `Heart...4`, 
           intestine = `Intestine...5`, kidney = `Kidney...6`, 
           kidneyPancreas = `Kidney-Pancreas...7`, liver = `Liver...8`,
           lung = `Lung...9`, pancreas = `Pancreas...10`, total = `...11`)
  
  df.recipients2020 <- df.recipients[,21:29]
  df.recipients2020$donorMOD <- df.recipients$donorMOD
  df.recipients2020$recipientAges <- df.recipients$recipientAges
  df.recipients2020 <- df.recipients2020 %>%
    rename(heartLung = `Heart-Lung...21`, heart = `Heart...22`,
           intestine = `Intestine...23`,
           kidney = `Kidney...24`, kidneyPancreas = `Kidney-Pancreas...25`,
           liver = `Liver...26`,
           lung = `Lung...27`, pancreas = `Pancreas...28`, total = `...29`)
  
}


##### --------------------------------------------------------------------------
#--------- Find life-expectancy on wait list, pre-transplant
##### --------------------------------------------------------------------------


## ------ Create abridged baseline lifetable
  # returns NVSS lifetable abridged to match df.waitlist.mortality age groups
  # and adds waitlist mortality column mWaitlist
df.lifetable.waitlist <- create.abr.lifetable(df.waitlist.mortality)


## ------ Calculate waitlist mortality
# take abridged lifetable df.lifetable.waitlist, calculate baseline mortality 
# rates m, for those new age groups, and find excess mortality of waitlist
# patients relative to baseline
df.lifetable.waitlist <- df.lifetable.waitlist %>%
  mutate(m = d/L) %>%
  mutate(mExcess = mWaitlist - m)


## ------ Calculate single-age waitlist life expectancy, pre-transplant
# Use NVSS lifetable to get baseline mortality of entire population (single-age)
df.life.exp.pre <- df.lifetable.nvss
df.life.exp.pre$a <- 0.5
df.life.exp.pre$a[1] <- 0.2
df.life.exp.pre <- df.life.exp.pre %>%
  mutate(mBaseline = q/(1 - (1 - a) * q))

# Combine single-age NVSS baseline mortality of population with multi-year age 
# waitlist mortality to find excess waitlist mortality
waitlist.lifetable.temp <- df.lifetable.waitlist %>%
  select(Ages, mExcess)
df.life.exp.pre <- df.life.exp.pre %>%
  left_join(waitlist.lifetable.temp, by = "Ages")

#Fill in missing excess mortality values from the most recent existing value
for (i in 1:nrow(df.life.exp.pre)) {
  if (is.na(df.life.exp.pre$mExcess[i]) == TRUE){
    df.life.exp.pre$mExcess[i] = df.life.exp.pre$mExcess[i-1]
  }
}

# total single year waitlist mortality estimate: baseline + excess
df.life.exp.pre$m <- df.life.exp.pre$mBaseline + df.life.exp.pre$mExcess

# calculate resulting life expectancy (and other lifetable values) from m 
# (the new single-age estimate of waitlist mortality rates)
df.life.exp.pre <- create.lifetable.from.m(df.life.exp.pre)


if (FALSE){
  ggplot(data = df.life.exp.pre, aes(x = Ages, y = e))+
    geom_point()+
    ylim(c(0, 40))+
    scale_x_continuous(breaks = c(0,1,6,12,18, 35, 50, 65)) +
    labs(y = 'Remaining life expectancy (years)',
         title = paste(selectOrgan,'waitlist patients remaining life expectancy'))
  ggplot(data = df.life.exp.pre, aes(x = Ages, y = mExcess))+
    geom_point()+
    ylim(c(0, 0.35))+
    scale_x_continuous(breaks = c(0,1, 6, 12, 18, 35, 50, 65)) + #c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    #labels = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
    labs(#y = 'Remaining life expectancy (years)',
      title = paste('M excess,',selectOrgan,'waitlist'))
  ggplot(data = df.lifetable.waitlist, aes(x = Ages, y = mExcess))+
    geom_point()+
    scale_x_continuous(breaks = c(0,1, 6, 12, 18, 35, 50, 65))+
    labs(#y = 'Remaining life expectancy (years)',
      title = 'M excess, lung waitlist')
  ggplot(data = df.life.exp.pre, aes(x = Ages, y = m))+
    geom_point()+
    ylim(c(0,0.4))+
    scale_x_continuous(breaks = c(0,1, 6, 12, 18, 35, 50, 65))+
    labs(#y = 'Remaining life expectancy (years)',
      title = paste('m', selectOrgan, 'waitlist'))
  ggplot(data = df.lifetable.nvss, aes(x = Ages, y = q))+
    geom_point()+
    #ylim(c(0,0.6))+
    scale_x_continuous(breaks = c(0,1, 6, 12, 18, 35, 50, 65))+
    labs(#y = 'Remaining life expectancy (years)',
      title = 'q, nvss ')
  ggplot(data = df.life.exp.pre, aes(x = Ages, y = `T`))+
    geom_point()+
    scale_x_continuous(limits = c(0,100),
                       breaks = c(0,1, 6, 12, 18, 35, 50, 65))+#c(0, 1, 6, 12, 18))+
    # labels = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
    labs(#y = 'Remaining life expectancy (years)',
      title = 'T, lung waitlist')
  ggplot(data = df.life.exp.pre, aes(x = Ages, y = `m`))+
    geom_point()+
    ylim(c(0,0.6))+
    scale_x_continuous(limits = c(0,100),
                       breaks = c(0,1, 6, 12, 18, 35, 50, 65))+
    labs(#y = 'Remaining life expectancy (years)',
      title = 'm, lung waitlist')
  ggplot(data = df.life.exp.pre, aes(x = Ages, y = l))+
    geom_point()+
    scale_x_continuous(limits = c(0,100),
                       breaks = c(0,1, 6, 12, 18, 35, 50, 65))+#c(0, 1, 6, 12, 18))+
    labs(#y = 'Remaining life expectancy (years)',
      title = 'l, lung waitlist')
  ggplot(data = df.life.exp.pre, aes(x = Ages, y = log(L)))+
    geom_point()+
    scale_x_continuous(limits = c(0,100),
                       breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                       labels = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
    labs(#y = 'Remaining life expectancy (years)',
      title = 'L, lung waitlist')
  ggplot(data = df.life.exp.pre, aes(x = Ages, y = `T`/l))+
    geom_point()+
    scale_x_continuous(limits = c(0,100),
                       breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                       labels = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
    labs(#y = 'Remaining life expectancy (years)',
      title = 'T/l, lung waitlist')
  ggplot(data = df.life.exp.pre, aes(x = Ages, y = mBaseline))+
    geom_point()+
    ylim(c(0,0.4))+
    scale_x_continuous(limits = c(0,100),
                       breaks = c(0,1, 6, 12, 18, 35, 50, 65))+
    labs(#y = 'Remaining life expectancy (years)',
      title = 'mBaseline, lung waitlist')
  #write_csv(df.life.exp.pre, file = "kidney_lifeexp_pre.csv")
}


##### --------------------------------------------------------------------------
#--------- Calculate post-transplant life expectancy
##### --------------------------------------------------------------------------

# Make a new abridged lifetable from NVSS lifetable, since age groups could be different
df.lifetable.survival <- create.abr.lifetable(df.transplant.survival, 
                                              year = 0, 
                                              Colname = "Survival")
df.lifetable.survival <- df.lifetable.survival %>%
  select(-c(mWaitlist)) %>%
  mutate(m = d / L, Survival = NULL)

#create empty data frame to later store all data in
name.vector <- c("Ages.Transplant", "Ages.Text", "Ages", "e")
df.life.exp.post.combine <- data.frame(matrix(ncol = length(name.vector), nrow = 0))
colnames(df.life.exp.post.combine) <- name.vector


for (j in 1:nrow(df.lifetable.nvss)) {
  # iterate through each age of recipient at time of transplant
  # find excess mortality post-transplant relative to baseline, for each age and
  # each year post-transplant
  
  #j=10
  transplantAge <- j - 1
  
  # Use NVSS lifetable to get baseline mortality (mBaseline) for single-year age groups
  df.life.exp.post <- df.lifetable.nvss
  
  #set initial conditions
  df.life.exp.post$a <- 0.5
  df.life.exp.post$mPostExcess <- 0
  df.life.exp.post$a[1] <- 0.2
  df.life.exp.post <- df.life.exp.post %>%
    mutate(mBaseline = q / (1 - (1 - a) * q))

  #Use abridged NVSS lifetable to get baseline mortality rate (mBaseline) for this multi-year age group
  m <- df.lifetable.survival$m[match(TRUE, df.lifetable.survival$Ages > transplantAge,
                                     nomatch = nrow(df.lifetable.survival) + 1) - 1]
  
  #apply survival rates for a given year post-transplant. Assume after 5 years,
  #excess rates are constant
  for (yearPost in unique(df.transplant.survival$Year[df.transplant.survival$Year > 0])) {
    # iterates through first 5 post-transplant years after transplantAge
    
    agePostTransp <- transplantAge + yearPost - 1
    
    #get excess mortality for this age group and time after transplant
    df.surv.subset <- subset(df.transplant.survival, Year == yearPost)
    df.surv.subset.prev <- subset(df.transplant.survival, Year == (yearPost - 1))
    
    qPost <- 1 - df.surv.subset$Survival[match(TRUE, 
                                                 df.surv.subset$Ages > transplantAge, 
                                                 nomatch = nrow(df.surv.subset) + 1) - 1] / 
      df.surv.subset.prev$Survival[match(TRUE, 
                                         df.surv.subset.prev$Ages > transplantAge, 
                                          nomatch = nrow(df.surv.subset.prev) + 1) - 1]
    
    # --- convert from probability of death in 1 year (qPost) to mortality 
    #rate (mPost) for a single year age group
    
    if (yearPost == 1){ #assumes deaths concentrated near beginning of interval in first year
      aPost = 0.25
    }else{
      aPost = 0.5
    }
    
    mPost <- qPost / (1 - (1 - aPost) * qPost)
    
    #--------convert this to an excess mortality (mPostExcess)
    mPostExcess <- max(mPost - m, 0)
    
    #add this excess mortality to get the post-transplant mortality for this later age
    df.life.exp.post$mPostExcess[match(TRUE, df.life.exp.post$Ages == agePostTransp)] = mPostExcess
    
  }
  
  # for all subsequent survival years, set post-transplant mortality to the 5 year
  # value
  df.life.exp.post$mPostExcess[df.life.exp.post$Ages > agePostTransp] = mPostExcess
  
  #get total post transplant mortality for single-year age groups
  df.life.exp.post$m = df.life.exp.post$mBaseline + df.life.exp.post$mPostExcess
  
  # re-calculate lifetable values (including life expectancy e) from the newly-found
  # single-age post-transplant mortality
  df.life.exp.post = create.lifetable.from.m(df.life.exp.post)
  
  # enter it into master dataframe, #comb.cols=c("Ages.Transplant","Ages.Text","Ages","e"). 
  # Only enter values for ages AFTER transplant
  temp <- data.frame(Ages.Transplant = transplantAge, #age at time of transplant (numeric)
             Ages.Text = df.life.exp.post$Ages.Text[df.life.exp.post$Ages >= transplantAge], # age at time of transplant (text)
             Ages = df.life.exp.post$Ages[df.life.exp.post$Ages >= transplantAge],
             e = df.life.exp.post$e[df.life.exp.post$Ages >= transplantAge])
  
  df.life.exp.post.combine = rbind(df.life.exp.post.combine, temp)
  
  # make new variable that is remaining life expectancy + current age
  
}


# Add variable timeSinceTransp
df.life.exp.post.combine <- df.life.exp.post.combine %>%
  mutate(timeSinceTransp = Ages - Ages.Transplant)


# Find life expectancy gain due to transplants e.gained
df.ylg <- df.life.exp.post.combine %>%
  filter(timeSinceTransp == 0) %>%
  select(Ages, e) %>%
  rename(ePost = e)
df.ylg$ePre = df.life.exp.pre$e
df.ylg$eGained = df.ylg$ePost - df.ylg$ePre 

ggplot(df.ylg, aes(Ages, eGained)) +
  geom_line() +
  scale_x_continuous(breaks=c(0,1,6,12,18,35,50,65))+
  labs(title = paste("Life expectancy gained due to", selectOrgan, "transplant"), 
       x = "Age at time of transplant", 
       y = "Increase in life expectancy (years)")



#####----------------------------
# Calculating total years of life gained per organ
#####----------------------------

age.vec.mid = c(0, 3, 8, 14, 26, 42, 57, 70) #select ages in the middle of age ranges in the donor MOD data
df.ylg.select <- df.ylg %>%
  filter(Ages %in% age.vec.mid)

donorModSelect = "all" # options are all, drugInt, medical and accidental

if (selectYear=='2020'){
  df.recipients.select <- df.recipients2020
} else if (selectYear=='2019'){
  df.recipients.select <- df.recipients2019
} else if (selectYear=='2018'){
  df.recipients.select<-df.recipients2018
}


## Formatting ages, and separating by donor MOD
df.recipients.select <- df.recipients.select %>%
  mutate(recipientAges = replace(recipientAges, recipientAges == "< 1", 0)) %>%
  group_by(recipientAges = gsub("[[:punct:]]\\d\\d$", "", recipientAges)) %>%
  group_by(recipientAges = gsub("[[:punct:]]\\d$", "", recipientAges)) %>%
  group_by(recipientAges = gsub("[[:punct:]]$", "", recipientAges)) %>%
  mutate(recipientAges = as.numeric(recipientAges)) %>%
  select(paste(selectOrgan), recipientAges, donorMOD) 

if (donorModSelect == 'drugInt'){
  df.recipients.select <- df.recipients.select %>%
    filter(donorMOD == "DRUG INTOXICATION")
} else if (donorModSelect == 'medical'){
  df.recipients.select <- df.recipients.select %>%
    filter(donorMOD == "CARDIOVASCULAR" |
             donorMOD == "INTRACRANIAL\r\nHEMORRHAGE/STROKE" |
             donorMOD == "DEATH FROM NATURAL\r\nCAUSES")
} else if (donorModSelect == 'accidental'){
  df.recipients.select <- df.recipients.select %>%
    filter(donorMOD == "BLUNT INJURY"| 
             donorMOD == "GUNSHOT WOUND" | 
             donorMOD == "ASPHYXIATION")
} else if (donorModSelect == 'all'){
  df.recipients.select <- df.recipients.select
}

if (selectOrgan == "kidney"){
  df.recipients.select <- aggregate(kidney ~ recipientAges,
                                    data = df.recipients.select, sum)
  df.ylg.select$numberTransp = df.recipients.select$kidney
}else if (selectOrgan == "liver"){
  df.recipients.select <- aggregate(liver ~ recipientAges, 
                                    data = df.recipients.select, sum)
  df.ylg.select$numberTransp = df.recipients.select$liver
}else if (selectOrgan == "heart"){
  df.recipients.select <- aggregate(heart ~ recipientAges,
                                    data = df.recipients.select, sum)
  df.ylg.select$numberTransp = df.recipients.select$heart
}else if (selectOrgan == "lung"){
  df.recipients.select <- aggregate(lung ~ recipientAges, 
                                    data = df.recipients.select, sum)
  df.ylg.select$numberTransp = df.recipients.select$lung
}

df.ylg.select <- df.ylg.select %>%
  mutate(YLG = numberTransp * eGained)

sum(df.ylg.select$YLG)
sum(df.ylg.select$numberTransp)


