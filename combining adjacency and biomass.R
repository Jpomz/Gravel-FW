#combining Taieri adjacency matrices and biomass
# J Pomz
#30 Jan 2017

#script for combining biomass data with food web matrices for Taieri food webs from Ross Thompson's data

source("C:\\Users\\Justin\\Documents\\Data\\FW modelling Petchey Github\\ttl-resources-master\\food_web\\FoodWebFunctions.r")

biomass <- readRDS("Taieri 10 webs biomass.rds")
adj <- readRDS("C:\\Users\\Justin\\Documents\\Data\\FW modelling Petchey Github\\food web compilation\\FW compilation\\tt1.list.genus.xtabs.rds")

#pull out food web matrices that have associated biomass data
adj2 <- adj[intersect(names(biomass), names(adj))]
biomass.taxa <- llply(biomass, function (x){x$taxa})

#subset only predator:prey interactions from
#food web matrices
adj3 <- NULL
for (i in 1:length(biomass.taxa)){
  a <- adj2[[i]]
  a.names <- rownames(a)
  b.names <- biomass.taxa[[i]]
  sub.names <- intersect(a.names, b.names)
  a2 <- a[sub.names,sub.names]
  adj3[[i]] <- a2
  rm(a, a.names, b.names, sub.names, a2)
}
names(adj3) <- names(biomass.taxa)%>%
  gsub(".csv","",.) #remove ".csv" from names

#turn matrices into table with resource / consumer pairs
pairs <- adj3 %>% llply(function (x){
  Matrix.to.list(x) %>% as.data.frame()
})

#add names to columns
pairs <- pairs %>% llply(function (x){
  colnames(x) <- c("resource", "consumer");x
})

#merge paired tables with information on body mass
for (i in 1:length(pairs)){
  pairs[[i]] <- merge(pairs[[i]], biomass[[i]][,c(1,6)], by.x = "resource", by.y = "taxa")
  pairs[[i]] <- merge(pairs[[i]], biomass[[i]][,c(1,6)], by.x = "consumer", by.y = "taxa")
}

#name columns for clarity
#double check that col names / order is in agreement
pairs <- pairs %>% llply(function (x){
  colnames(x) <- c("consumer", "resource", "M.res", "M.con");x
})

#save data ####
#saveRDS(pairs, file = "Taieri food web pred prey bodymass.rds")
#pairs <- readRDS(file = "Taieri food web pred prey bodymass.rds")
#saveRDS(adj3, file = "adjacency matrix with only pred-prey interactions.rds")
#adj3 <- readRDS(file = "adjacency matrix with only pred-prey interactions.rds")



#all sp pairs####

# read in data ####
pairs <- readRDS(file = "Taieri food web pred prey bodymass.rds")
adj3 <- readRDS(file = "adjacency matrix with only pred-prey interactions.rds")
biomass <- readRDS("Taieri 10 webs biomass.rds")

#make resource and consumer columns into character
#union()
#df %>% group %>% summarise(sum()))
cons.names <- pairs %>% ldply(function (x){
  rbind(x) 
})%>% select(consumer)%>%unique()
cons.names <- cons.names$consumer %>% as.character

#remove M.x columns from pairs and make character
pairs.only <- pairs %>% llply(function (x){
  x$M.res <- NULL
  x$M.con <- NULL
  x$resource <- as.character(x$resource)
  x$consumer <- as.character(x$consumer)
  x$link <- 1 #link presence = 1
  x
})

#make list with all species co-occurence
all.sp.pairs <- adj3 %>% llply( function (x){
  data.frame(resource = rownames(which(x >= 0, arr.ind = T)),
             consumer = colnames(x)[as.data.frame(which(x >=0, arr.ind = T))[[2]]])
})

#make resource / consumer character value
all.sp.pairs <- all.sp.pairs %>% llply(function(x){
  x$resource <- as.character(x$resource)
  x$consumer <- as.character(x$consumer)
  x$link <- 0 #link = 0
  x
})

#combine 2 lists
combined <- NULL
for (i in 1:length(all.sp.pairs)){
  df <- union(all.sp.pairs[[i]], pairs.only[[i]]) %>% #unite 2 df's
    group_by(resource, consumer)%>% #group by resource and consumer variables
    summarise(link = sum(link)) #"sum" link to remove duplicate res-con combos
  combined[[i]] <- df #add to list
  rm(df)
}

#add relative abundances of consumers and resources
#multiply two rel.ab together
combined.ab <- NULL
for (i in 1:length(combined)){
 test <- merge(combined[[i]], biomass[[i]][,c(1,2)], by.x = "resource", by.y = "taxa") #add resource abundande
  names(test)[4] <- "res.rel.ab" #name column
  test <- merge(test, biomass[[i]][,1:2], by.x = "consumer", by.y = "taxa")
  #add consumer abundance
  names(test)[5] <- "con.rel.ab" # name column
  test <- test %>% mutate(rel.ab = res.rel.ab * con.rel.ab) #calculate relative abundances (Ni * Nj)
  combined.ab[[i]] <- test #add to list
  rm(test)
}

#remove 'cannibal' links
# test2 <- combined.ab %>% llply(function (x){
#   x[x$consumer!= x$resource,] 
# })


# for (i in 1:length(test2)){
#   plot(link~rel.ab, data = test2[[i]])
# }

#try and subset so only looking at secondary consumers
#not currently working...
test3 <- combined.ab %>% llply(function (x){
  x[x$consumer %in% cons.names,] 
})

for (i in 1:length(test3)){
  plot(link~rel.ab, data = test3[[i]])
}

pred.names <- cons.names[c(2:7,9, 12,13,19,22,23,24,31)]

test4 <- combined.ab %>% llply(function (x){
  x[x$consumer %in% pred.names,] 
})

for (i in 1:length(test4)){
  plot(link~rel.ab, data = test4[[i]])
}

model <- glm(link ~rel.ab,family=binomial(link='logit'),data=test)



#test making committs in github
#why is this not woring?
test4 %>% ldply

