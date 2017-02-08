#Fish data
#J Pomz
# 31 Jan 2017

#formula for converting length to DW
#from Jellyman et al 2013 NZJMFR
#see citation for coefficients
#log10(w) = log(a) + b * log(l)

#downloaded all fish data from Taireri Catchment
#downloaded from https://www.niwa.co.nz/our-services/online-services/freshwater-fish-database
#~3200 rows, 22 columns

fish <- read.csv("Taieri NZ Fish datbase.csv")

fish %>%  
  filter(minl >0 & maxl >0) %>%
  group_by(locality, spcode) %>% 
  summarize(mean.min = mean(minl),
            mean.max = mean(maxl)) %>%
  select(locality, spcode, mean.min,mean.max) %>% arrange(spcode) %>% as.data.frame()

fish %>% select(spcode) %>% unique %>% arrange(spcode)

fish %>% filter(minl >0 & maxl >0) %>% select(spcode) %>% unique


####
#mean max saltru = 171.6028
#mean min saltru = 83.526

-5.63941 + 3.05 * log10(83.52)
#log(dw) = 0.222051
10**0.222051
#1.667443

-5.63941 + 3.05 * log10(171.6028)
#log(dw) = 1.175889
10**1.175889
#14.99302

salmo <- data.frame(taxa = "Salmo", rel.ab = NA, ab.m2 = NA, rel.dw = NA, dw.m2 = NA, ind.dw = 14.99302)

source("C:\\Users\\Justin\\Documents\\Data\\FW modelling Petchey Github\\ttl-resources-master\\food_web\\FoodWebFunctions.r")

biomass <- readRDS("Taieri 10 webs biomass.rds")
adj <- readRDS("C:\\Users\\Justin\\Documents\\Data\\FW modelling Petchey Github\\food web compilation\\FW compilation\\tt1.list.genus.xtabs.rds")

#add salmo to biomass 
biomass <- llply(biomass, function (x){rbind(x, salmo)})

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

#save data
saveRDS(pairs, file = "Taieri pred prey bodymass test fish.rds")

                