#taxa names in thompson FWs with biomass


#rough draft of steps
#1 fix typos
#2 aggregate to genus
#3 intersection of FW rownames and biomass taka names
#4 matrix to list
#5 add biomass information to dietary links 

#this script == steps ~1-3?

#specific steps
#1 read in all biomass csv into a list
#2 replace "." for " "  in taxa column
#3 pull out all biomass taxa names
#4 extract names not already in "corrected" column of translation file %>%
# extract names not in "wrong" column of translation file
#5 add these names to translation file
# re-do step #4 above and make sure all names are in translation file
#6 rename biomass$taxa so that they all match
#7 compile biomass data into genus
# (see "species to basal" script from FW compilation project for a template)

#reccoderFunc ####
#recorder function
# rename col/rownames with translated vector
# https://susanejohnston.wordpress.com/2012/10/01/find-and-replace-in-r-part-2-how-to-recode-many-values-simultaneously/
# use translated vector to rename webs


#recoderFunc from S Johnston
recoderFunc <- function(data, oldvalue, newvalue) {
  # convert any factors to characters
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  # create the return vector
  newvec <- data
  # put recoded values into the correct position in the return vector
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  newvec}

#read in all files ####

#list upper directory
upperdir <- "C:\\Users\\Justin\\Documents\\Data\\Food web modelling M-N Gravel et al 2016\\Biomass Taieri food webs\\"
#list files in upper directory
files <- list.files(upperdir, full.names = T) #full path
filenames <- list.files(upperdir) #just names
#make empty list
biomass.list <- NULL
#read in all .csv's into empty list
#use object with full path names
for (i in 1:length(files)){
  j <- read.csv(files[i], header = T)
  biomass.list[i] <- list(j)
  rm(j)
}
#name elements of list using filenames
names(biomass.list) <- c(filenames[1:length(filenames)])


#make list of all taxa
biomass.list.names <- llply(biomass.list,
                        function(x){
                          x$taxa %>%
                          gsub(" ", "\\.", .)
                          #replace " " to "."
                        })
#replacing " " with a "." to match old tranlsation file which read spaces in as a period


#translation file ####
translate <- read.csv("C:\\Users\\Justin\\Documents\\Data\\FW modelling Petchey Github\\food web compilation\\FW compilation\\compiled.corrected.translation.csv")

#update translation file ####
#4 extract names not already in "corrected" column of translation file 
biomass.list.correct <- llply(biomass.list.names,
                              function (x){
                                setdiff(x, translate$Corrected)
                              })

#make list of all taxa not in "Wrong" column 
add.to.wrong <- llply(biomass.list.correct,
                      function (x){
                        setdiff(x, translate$Wrong)
                      })

#get unique values from add.to.wrong object
#make csv and add to translation file
unlist(add.to.wrong) %>% unique #%>% write.csv(file = "add these to translation file.csv")


###
### added "new" corrections to translation file in excel 27 jan 2017

#6 ####
#6 rename biomass$taxa so that they all match

biomass.rename <- llply(biomass.list.names,
                        function (x){
                          recoderFunc(x,translate$Wrong, translate$Corrected)
                        })
#replace taxa values with rename
for (i in 1:length(biomass.list)){
  biomass.list[[i]][1] <- biomass.rename[[i]]
}

#compile identical taxa
biomass.list <- llply(biomass.list,
      function(x){
        x %>% 
          group_by(taxa) %>% 
          summarize(
            rel.ab = sum(rel.ab),
            ab.m2 = sum(ab.m2),
            rel.dw = sum(rel.dw),
            dw.m2 = sum(dw.m2)) %>%
          mutate(
            ind.dw = dw.m2 / ab.m2)
      })

#rename to Genus level ####
#species to genus dictionary
#same $Species and $Genus columns....
sp.gen <- read.csv("C:\\Users\\Justin\\Documents\\Data\\FW modelling Petchey Github\\food web compilation\\FW compilation\\species genus category ffg.csv")

biomass.sp <- llply(biomass.list,
                    function(x){
                      x$taxa 
                    })

biomass.genus <- llply(biomass.sp,
                       function (x){
                         recoderFunc(x,sp.gen$Species, sp.gen$Genus)
                       })
#replace taxa values with genus
for (i in 1:length(biomass.list)){
  biomass.list[[i]][1] <- biomass.genus[[i]]
}

#compile identical taxa
biomass.list <- llply(biomass.list,
                      function(x){
                        x %>% 
                          group_by(taxa) %>% 
                          summarize(
                            rel.ab = sum(rel.ab),
                            ab.m2 = sum(ab.m2),
                            rel.dw = sum(rel.dw),
                            dw.m2 = sum(dw.m2),
                            ind.dw = sum(ind.dw))
                      })


saveRDS(biomass.list, file = "Taieri 10 webs biomass.rds")


