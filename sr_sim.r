# simulate series message arrival very simply

# load template
a <- read.table('0_data/sr.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE)

# constants
totd <- 30
npro <- 6
nday <- 14
totm <- npro * nday

# calculated values
totn <- sum(a$Count)
nrat <- totn / totd


srsim <- function() {
  dat <- data.frame(msg = 1:totm, cnt = 0, act = 0)
  day <- 1
  
  while(any(dat$cnt == 0)) {
    # set more active for the first 14 days
    if(day <= nday) {
      dat$act[(npro*(day-1)+1):((npro*(day-1)+1)+npro-1)] <- 1
    }
    
    # sample by nrat
    dese <- which(dat$act == 1)
    incr <- sample(dese, nrat, replace = TRUE)

    # increment the sampled msgs
    for(i in 1:length(incr)) {
      dat$cnt[dese][incr[i]] <- dat$cnt[dese][incr[i]] + 1
    }
    
    # increment day
    day <- day + 1
  }
  
  list(dat = dat, day = day)
}

# set a seed from my orcid
set.seed(chf::orcid_wrc())

# run 1000 sims
nsim <- 1000
out <- list()
pb <- txtProgressBar(style = 3)
for(i in 1:nsim) {
if((i %% 10) == 0) setTxtProgressBar(pb, i/nsim)
  out[[i]] <- srsim()
}  
close(pb)

# look at days to complete
daystocomplete <- sapply(out, '[[', 'day')
quantile(daystocomplete, c(0.25, 0.5, 0.75, 0.975))

# take a peak
hist(daystocomplete, nclass = 20, col = 'grey85', las = 1)