setwd("~/Opioid_project/ylg_transplant")
source("code/functions.R")
library(tidyverse)
library("readxl")


##### --------------------------------------------------------------------------
#--------- Import data
##### --------------------------------------------------------------------------

### Select desired organ and year
selectYear <- "2020"
selectOrgan <- "lung" #select liver, kidney, heart or lung
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


### Error estimation

df.error.estimates <-  read.delim(file='data/error_estimate_data.txt', header=TRUE,
                                    sep='\t')
df.error.estimates <- df.error.estimates %>%
  filter(Year == selectYear)


if (selectOrgan == "kidney"){
  # kidney waitlist
  nWaitlist <- df.error.estimates$Kidney.waitlist
  nWaitlist[2] <- nWaitlist[1] + nWaitlist[2] #kidney numbers combine first two age groups
  nWaitlist <- nWaitlist[2:8]
  # kidney recipients
  nRecipients <- df.error.estimates$Kidney.recipients
  nRecipients[2] <- nRecipients[1] + nRecipients[2] #kidney numbers combine first two age groups
  nRecipients <- nRecipients[2:8]
}
if (selectOrgan == "heart"){
  # heart waitlist
  nWaitlist <- df.error.estimates$Heart.waitlist
  # heart recipients
  nRecipients <- df.error.estimates$Heart.recipients
}
if (selectOrgan == "lung"){
  # lung waitlist
  nWaitlist <- df.error.estimates$Lung.waitlist
  # lung recipients
  nRecipients <- df.error.estimates$Lung.recipients
  if (selectYear=='2020'){ #2020 has all pediatric lung data combined
    nRecipients <- nRecipients[4:8]
    nWaitlist <- nWaitlist[4:8]
  }
}
if (selectOrgan == "liver"){
  # liver waitlist
  nWaitlist <- df.error.estimates$Liver.waitlist
  # liver recipients
  nRecipients <- df.error.estimates$Liver.recipients
}

df.transplant.survival$numberRecipients = rep(nRecipients, 6)

if (selectOrgan=='kidney'){
  df.waitlist.mortality.yearsubset <- df.waitlist.mortality %>%
    filter(Year == selectYear) %>%
    filter(Ages != 0) #for kidney, filter out age =0
} else {
  df.waitlist.mortality.yearsubset <- df.waitlist.mortality %>%
    filter(Year == selectYear)
}

# add column with width of mortality distribution
df.transplant.survival$error <- 100*sigma_p(df.transplant.survival$Survival/100,
                                            df.transplant.survival$numberRecipients)
df.waitlist.mortality.yearsubset$error <- 100*sigma_p(df.waitlist.mortality.yearsubset$Rate/100,
                                                      nWaitlist)

# p <- ggplot(df.waitlist.mortality.yearsubset, aes(x=Ages, y=Rate)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=Rate-error, ymax=Rate+error), width=.2,
#                 position=position_dodge(.9))+
#   labs(title = paste(selectOrgan,'waitlist mortality rates'))+
#   scale_x_continuous(breaks=c(0,1,6,12,18,35,50,65))+
#   ylim(c(0,35))
# p

ages = df.lifetable.nvss$Ages
lePreVec = c()
mPreVec = c()
mPostVec = c()
yearsPostVec =c()
ylgVec = c()
df.sel.waitlist.mortality <- df.waitlist.mortality.yearsubset

for (sampling_ind in 1:500){
  random.dist.vec=c()
  for (mean in 1:length(df.sel.waitlist.mortality$Rate)){
    random.dist.vec[mean] = rnorm(1, mean = df.sel.waitlist.mortality$Rate[mean], 
                                  sd = df.sel.waitlist.mortality$error[mean])}
  
  random.dist.vec[random.dist.vec<0]<- 0
  df.sel.waitlist.mortality$Random = random.dist.vec
  mPreVec <- append(mPreVec, df.sel.waitlist.mortality$Random)

  # if (selectOrgan=='lung'){
  #   test <- data.frame(mPreVec, rep(c(1,18,35,50,65),1000))
  # } else { test <- data.frame(mPreVec, rep(c(0,1,6,12,18,35,50,65),1000))}
  # 
  # ggplot(test, aes(x = rep(c(0,18,35,50,65),1000), y = mPreVec))+
  #     scale_x_continuous(breaks=c(0,18,35,50,65))+
  #     labs(title=paste('Sampling from',selectOrgan,'waitlist mortality distribution'),
  #          y='Deaths per 100 waitlist-years',
  #          x='Age groups')+
  #     geom_point(alpha =0.005)+
  #     ylim(c(0,42))

  random.dist.survival.vec = c()
  for (mean in 1:length(df.transplant.survival$Survival)){
    #print(mean)
    random.dist.survival.vec[mean] = rnorm(1, mean = df.transplant.survival$Survival[mean], 
                                           sd = df.transplant.survival$error[mean])
    #print(random.dist.survival.vec[mean])
  }
  random.dist.survival.vec[random.dist.survival.vec>100]<- 100
  random.dist.survival.vec[random.dist.survival.vec<0]<- 0
  df.transplant.survival$Random = random.dist.survival.vec
  
# p <- ggplot(df.transplant.survival, aes(x=Ages, y=Random, color = Year)) +
  #   geom_point() +
  #   geom_errorbar(aes(ymin=Survival-error, ymax=Survival+error), width=.2,
  #                 position=position_dodge(.9))
  # p
  
  
  if (selectYear=='2020' & selectOrgan == 'lung'){
    df.transplant.survival$Random[1:5] <- 100
  } else if (selectYear=='2020' & selectOrgan == 'kidney'){
    df.transplant.survival$Random[1:7] <- 100
  } else if (selectYear=='2020' & (selectOrgan == 'liver' | selectOrgan == 'heart')){
    df.transplant.survival$Random[1:7] <- 100
  }
  
 
  ##### --------------------------------------------------------------------------
  #--------- Find life-expectancy on wait list, pre-transplant
  ##### --------------------------------------------------------------------------
  
  
  ## ------ Create abridged baseline lifetable
  # returns NVSS lifetable abridged to match df.waitlist.mortality age groups
  # and adds waitlist mortality column mWaitlist
  df.lifetable.waitlist.randomsample <- create.abr.lifetable(df.sel.waitlist.mortality,
                                                             Colname = all_of("Random"))
  
  
  ## ------ Calculate waitlist mortality
  # take abridged lifetable df.lifetable.waitlist, calculate baseline mortality 
  # rates m, for those new age groups, and find excess mortality of waitlist
  # patients relative to baseline
  df.lifetable.waitlist.randomsample <- df.lifetable.waitlist.randomsample %>%
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
  waitlist.lifetable.temp <- df.lifetable.waitlist.randomsample %>%
    select(Ages, mExcess)
  df.life.exp.pre <- df.life.exp.pre %>%
    left_join(waitlist.lifetable.temp, by = "Ages")
  
  
  if ( !(selectYear== '2020' & selectOrgan=='lung')){
    df.life.exp.pre$mExcess[1] <- df.life.exp.pre$mExcess[2]#since age groups changed a bit added this
  }
  
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
  
  lePreVec = append(lePreVec, df.life.exp.pre$e)
  
  
  
  ##### --------------------------------------------------------------------------
  #--------- Calculate post-transplant life expectancy
  ##### --------------------------------------------------------------------------
  
  # Make a new abridged lifetable from NVSS lifetable, since age groups could be different
  df.lifetable.survival <- create.abr.lifetable(df.transplant.survival, 
                                                year = 0, 
                                                Colname = "Random")
  df.lifetable.survival <- df.lifetable.survival %>%
    select(-c(mWaitlist)) %>%
    mutate(m = d / L, Random = NULL)
  
  #create empty data frame to later store all data in
  name.vector <- c("Ages.Transplant", "Ages.Text", "Ages", "e")
  df.life.exp.post.combine <- data.frame(matrix(ncol = length(name.vector), nrow = 0))
  colnames(df.life.exp.post.combine) <- name.vector
  
  
  for (j in 1:nrow(df.lifetable.nvss)) {
    # iterate through each age of recipient at time of transplant
    # find excess mortality post-transplant relative to baseline, for each age and
    # each year post-transplant
    
   # j=1
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
      
      qPost <- 1 - df.surv.subset$Random[match(TRUE, 
                                                 df.surv.subset$Ages > transplantAge, 
                                                 nomatch = nrow(df.surv.subset) + 1) - 1] / 
        df.surv.subset.prev$Random[match(TRUE, 
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
  
  mPostVec <- append(mPostVec, df.transplant.survival$Random)
  yearsPostVec <- append(yearsPostVec, df.transplant.survival$Year)
  
  # Find life expectancy gain due to transplants e.gained
  df.ylg <- df.life.exp.post.combine %>%
    filter(timeSinceTransp == 0) %>%
    select(Ages, e) %>%
    rename(ePost = e)
  df.ylg$ePre = df.life.exp.pre$e
  df.ylg$eGained = df.ylg$ePost - df.ylg$ePre 
  
  
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
  
  ylgVec[sampling_ind]<-sum(df.ylg.select$YLG)
}


