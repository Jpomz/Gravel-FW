#Inferring food web structure
# a la Gravel et al. 2016 and 2013
# J Pomz

#need to look at biomass estimates = e.g. one archichauliodes at stony is fucked up

source("C:\\Users\\Justin\\Documents\\Data\\FW modelling Petchey Github\\ttl-resources-master\\food_web\\FoodWebFunctions.r")

pairs <- readRDS("Taieri food web pred prey bodymass.rds")
#pairs <- readRDS("Taieri pred prey bodymass test fish.rds")

data1 <- pairs %>% ldply(function (x){
  rbind(x)
})



data <- data1 %>% select(M.res, M.con) %>% mutate(log.res = log10(M.res), log.con = log10(M.con)) %>% select(log.res, log.con)

#functions from supplementary code Gravel et al 2013
#reg_fn ####
reg_fn = function(Bprey,Bpred,quartil) {
  
  library(quantreg)
  mean_reg = lm(Bprey~Bpred)			# For the n parameter
  qrsup = rq(Bprey~Bpred,tau = quartil[2])	# For the higher limit of the range
  qrinf = rq(Bprey~Bpred,tau = quartil[1])	# For the lower limit of the range
  
  return(list(mean_reg$coef,qrsup$coef,qrinf$coef))
  
}

#get_pars_niche ####
get_pars_Niche = function(pars,Ball) {
  
  # Unwrap the input parameters
  mean_reg = pars[[1]]
  qrsup = pars[[2]]
  qrinf = pars[[3]]
  
  # Estimate parameters for the allometric relationships
  delta = mean_reg[2]
  b1 = mean_reg[1]
  b2 = delta	
  
  # Estimate the parameters for the niche model
  n = Ball						# The niche n
  c = b1 + b2*Ball				# The centroid c
  low = qrinf[1] + qrinf[2]*Ball	# The lower limit of the range
  high = qrsup[1] + qrsup[2]*Ball	# The higher limit of the range
  
  return(cbind(n,c,low,high))	
}

L_fn = function(n,c,low,high) {
  
  S = length(n)   	
  L = matrix(0,nr=S,nc=S)
  
  for(i in 1:S)
    for(j in 1:S)
      if(n[j]>low[i] && n[j]<high[i]) L[j,i] = 1
  
  return(L)	
  
}


#
##############################################

Bprey <- data$log.res
Bpred <- data$log.con
#Ball <- c(seq(-5, -1, length.out = 30),0.7, 1.2)
Ball <- c(Bpred, Bprey) %>% sort

pars_reg = reg_fn(Bprey,Bpred,quartil = c(0.05,0.95))

pars_niche = get_pars_Niche(pars_reg,Ball)

Treshold  <-  which(pars_niche[,"n"]<=-4.)
pars_niche[,"low"][Treshold] <- 0
pars_niche[,"high"][Treshold] <- 0


L = L_fn(pars_niche[,1],pars_niche[,2],pars_niche[,3],pars_niche[,4])

Plot.matrix(L)


#list ####

#working with a list
pars <- pairs %>%llply(function (x) {
  reg_fn(x$M.res, x$M.con, quartil = c(0.05,0.95))})

Ball <- Bprey

pars.niche <- pars %>% llply(function (x){
  get_pars_Niche(x,Ball)})

L <- pars.niche %>% llply(function (x){
  L_fn(x[,1], x[,2], x[,3], x[,4])
})

for(i in 1:length(L)){
  Plot.matrix(L[[i]])
}


