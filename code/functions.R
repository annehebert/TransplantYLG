create.abr.lifetable <- function(df.data, year = selectYear, Colname = all_of("Rate")) {
  # abridges the (preloaded) single year lifetable to match the age groups in
  # the mortality data df.data and joins the Colname column of df.data to the
  # new lifetable (also filters to select mortality data for select.year)
  
  #select year from mortality data
  df.sel.waitlist.mortality <- df.data %>%
    filter(Year == year) %>%
    select("Ages.Text","Ages", Colname)
  #merge lifetable with waitlist data
  df.lifetable.waitlist <- df.sel.waitlist.mortality %>%
    left_join(df.lifetable.nvss, by = "Ages", suffix = c("", ".nvss")) %>%
    rename(mWaitlist = Colname) %>%
    mutate(mWaitlist = mWaitlist/100) %>%
    select(-c(Ages.Text.nvss))
  
  #Correct the abridged q, d, and L values (l,T,e remain the same)
  for (i in 2:(nrow(df.lifetable.waitlist) - 1)){ #First age group always 0-1
    #print(i)
    current.age <- df.lifetable.waitlist$Ages[i]
    next.age <- df.lifetable.waitlist$Ages[i+1]
    df.lifetable.waitlist$d[i] <- sum(df.lifetable.nvss$d[
      df.lifetable.nvss$Ages >= current.age & df.lifetable.nvss$Ages < next.age])
    df.lifetable.waitlist$L[i] <- sum(df.lifetable.nvss$L[
      df.lifetable.nvss$Ages >= current.age & df.lifetable.nvss$Ages < next.age])
    df.lifetable.waitlist$q[i] <- 1 - df.lifetable.waitlist$l[i+1] / df.lifetable.waitlist$l[i]
  }
  
  i <- nrow(df.lifetable.waitlist)
  current.age <- df.lifetable.waitlist$Ages[i]
  df.lifetable.waitlist$d[i] <- sum(df.lifetable.waitlist$d[df.lifetable.waitlist$Ages >= current.age])
  df.lifetable.waitlist$L[i] <- sum(df.lifetable.waitlist$L[df.lifetable.waitlist$Ages >= current.age])
  df.lifetable.waitlist$q[i] <- 1
  
  return(df.lifetable.waitlist)
}



create.lifetable.from.m <- function(lifetable) {
  # lifetable: a dataframe with single year ages and mortality rates
  # returns a lifetable from input mortality rates

  end.index = length(lifetable$m)
  
  # Create lifetable, initialize with all 0s. Lifetable entries are: 
    #q (probability of dying), l (number surviving), 
    # d (number dying), L (person-years lived), T (total number of person-years
    # lived above given age), e (life expectancy)
  
  m = lifetable$m
  a = rep(0.5, end.index) #for single year age groups
  q = rep(0, end.index) 
  l = rep(0, end.index)
  d = rep(0, end.index)
  L = rep(0, end.index)
  T = rep(0, end.index)
  e = rep(0, end.index)
  
  l0 = 1e5; #initial population assumed to be 10,000
  l[1] = l0;
  a[1] = 0.25; # infant age group
  q[1] = m[1] / (1 + (1-a[1]) * m[1])
  d[1] = q[1] * l[1]
  
  # fill in values from mortality data
  for (i in 2:(end.index-1)) {
    
    q[i] = m[i] / (1+(1-a[i]) * m[i]/l0)
    l[i] = l[i-1] * (1-q[i-1])
    d[i] = q[i] * l[i]
    L[i-1] = l[i] + d[i-1] * a[i-1]
    
  }
  
  # for last row
  q[end.index] = 1
  l[end.index] = l[end.index-1] * (1-q[end.index-1])
  d[end.index] = q[end.index] * l[end.index]
  L[end.index-1] = l[end.index] + d[end.index-1] * a[end.index-1]
  a[end.index] = 1 / m[end.index]
  L[end.index] = a[end.index] * d[end.index]
  
  # Calculate T and e
  for (i in 1:end.index) {
    
    T[i] = sum(L[i:end.index])
    e[i] = T[i] / l[i]
 
  }
  
  lifetable$a <- a
  lifetable$q <- q
  lifetable$l <- l
  lifetable$d <- d
  lifetable$L <- L
  lifetable$T <- T
  lifetable$e <- e
  
  return(lifetable) 
}


sigma_p <- function(p, n){
  #x is count of individuals in the sample with a certain characteristic
  #n total number of individuals in the sample
  # https://www.statology.org/standard-error-of-proportion/
  #p <- x/n
  #returns standard error of proportion
  sigma = sqrt(p*(1-p)/n)
  return(sigma)
}
