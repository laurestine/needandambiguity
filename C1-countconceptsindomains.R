# Set the working directory appropriately.
setwd("")
suppressMessages(
  library(RSQLite)) # access clics.sqlite database
suppressMessages(
  library(dplyr)) # dplyr and dbplyr make it easier to use databases
suppressMessages(
  library(dbplyr)) # dplyr and dbplyr make it easier to use databases
suppressMessages(
  library(tidyr)) # dplyr works best with tidyr to organize stuff

bigconceptframe = read.csv("2-annotatedconceptlist.csv")

# connect to databases
clicsdb <- dbConnect(RSQLite::SQLite(), "../clics.sqlite")
wordsdb = tbl(clicsdb, "FormTable")
conceptsdb = tbl(clicsdb, "ParameterTable")

# Read in language codes from a file:
languagecodes = read.csv("4-clicslanguagecodes.csv",
                         header=TRUE,
                         quote="\"")
# We're only looking at these languages:
languages = c(#"Arabic",
              #"Indonesian",
              "Basque",
              #"Tamil",
              "Albanian",
              #"Hindi",
              "Russian",
              #"Spanish",
              "Swahili",
              "Turkish",
              "Hungarian",
              "English"
              #"Thai"
)

bigconceptframe$Kinship = factor(bigconceptframe$Kinship)
bigconceptframe$Number = factor(bigconceptframe$Number)
bigconceptframe$Clothing = factor(bigconceptframe$Clothing)
bigconceptframe$Speech = factor(bigconceptframe$Speech)
bigconceptframe$Time = factor(bigconceptframe$Time)
bigconceptframe$Emotion = factor(bigconceptframe$Emotion)
bigconceptframe$Body = factor(bigconceptframe$Body)
bigconceptframe$physGeo = factor(bigconceptframe$physGeo)
bigconceptframe$Plant = factor(bigconceptframe$Plant)
bigconceptframe$Animal = factor(bigconceptframe$Animal)

# For each language,
# we will get the full list of concepts with wordfords in CLICS3.
# Then we'll mark each concept in our table
# as lexified in that language or not.
for (languagename in languages){
  langcodes = languagecodes$Code[languagecodes$Language==languagename]
  wordsdb %>%
    select(Language_ID, dataset_ID, clics_form, Form, Parameter_ID) %>%
    left_join(conceptsdb,
              by=c("Parameter_ID"="ID","dataset_ID"="dataset_ID")) %>%
    select(dataset_ID, Language_ID, clics_form, Form, Parameter_ID,Concepticon_Gloss) %>%
    mutate(full_lang_ID = paste(dataset_ID, Language_ID,sep="-")) %>%
    filter(full_lang_ID %in% langcodes) %>%
    filter(Concepticon_Gloss %in% allconcepts) %>%
    # remove words with spaces:
    filter(!(Form %like% "% %")) %>%
    select(Concepticon_Gloss) %>%
    collect() %>%
    unique() ->
    lexifiedconcepts
  bigconceptframe[paste0('lex_in_',languagename)] =
    bigconceptframe$name %in% lexifiedconcepts$Concepticon_Gloss
}

# Now, to make our life easier,
# we'll collapse that big frame into a shorter
# simple list of which concepts from which domains
# are lexified in CLICS3 in which languages.
shortframe = data.frame(Concept = "placeholder", Domain = "placeholder", Language="placeholder")

for (language in languages){
  for (domain in c("Kinship", "Number", "Clothing", "Speech", "Time", "Emotion", "Body", "physGeo", "Plant", "Animal")){
    for (i in 1:nrow(bigconceptframe)){
      if((bigconceptframe[i,'ontological_category']=="Person/Thing") &
         (bigconceptframe[i,paste0("lex_in_",language)]==TRUE) &
         (bigconceptframe[i,domain] != "")){
        shortframe = rbind(shortframe, c(bigconceptframe[i,'name'],domain,language))
      }
    }
  }
}
shortframe = shortframe[2:nrow(shortframe),]

# That will ultimately give us our list of concept counts by domain.
table(shortframe$Language,shortframe$Domain)
shortframe %>%
  select(Language, Domain) %>%
  group_by(Language, Domain) %>%
  summarize(count=n()) %>%
  data.frame() -> countframe

write.csv(countframe,"C2-conceptcountbydomain.csv",quote=TRUE,row.names=FALSE)

dbDisconnect(clicsdb)
rm(clicsdb, conceptsdb, wordsdb)