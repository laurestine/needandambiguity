# This is the place where we compute
# ambiguity scores for each domain-language pair.

# Set the working directory appropriately.
# setwd("")
suppressMessages(
  library(RSQLite)) # access clics.sqlite database
suppressMessages(
  library(dplyr)) # dplyr and dbplyr make it easier to use databases
suppressMessages(
  library(dbplyr)) # dplyr and dbplyr make it easier to use databases
suppressMessages(
  library(tidyr)) # dplyr works best with tidyr to organize stuff
suppressMessages(
  library(ggplot2)) # making pictures :)


# The broader-narrowest concept pairs we made in a previous program.
conceptframe = read.csv("A2-adjusteddomains.csv",header=TRUE,quote='"')

# Connect to databases.
clicsdb <- dbConnect(RSQLite::SQLite(), "../clics.sqlite")
wordsdb = tbl(clicsdb, "FormTable")
conceptsdb = tbl(clicsdb, "ParameterTable")

# Read in language codes from a file.
languagecodes = read.csv("clicslanguagecodes.csv",
                         header=TRUE,
                         quote="\"")


# This function makes a list of form-concept pairs
# with adjustment for the broader-narrower relations
# for one domain.
# It returns a database tibble.
domaincolexdb = function(domainname, languagename){
  domainframe = conceptframe[conceptframe$Domain == domainname,c("BroadGloss","NarrowGloss")]
  domain = unique(domainframe$BroadGloss)
  langcodes = languagecodes$Code[languagecodes$Language==languagename]
  # Now we know which language IDs and concepts to look for,
  # so we can join and filter the CLICS tables.
  wordsdb %>%
    select(Language_ID, dataset_ID, clics_form, Form, Parameter_ID) %>%
    left_join(conceptsdb,
              by=c("Parameter_ID"="ID","dataset_ID"="dataset_ID")) %>%
    select(dataset_ID, Language_ID, clics_form, Form, Parameter_ID,Concepticon_Gloss) %>%
    mutate(full_lang_ID = paste(dataset_ID, Language_ID,sep="-")) %>%
    filter(Concepticon_Gloss %in% domain) %>%
    filter(full_lang_ID %in% langcodes) %>%
    # remove words with spaces:
    filter(!(Form %like% "% %")) %>%
    # And join on the adjustment table that we made previously.
    select(full_lang_ID, clics_form, Form, Concepticon_Gloss) %>%
    left_join(domainframe,
              by=c("Concepticon_Gloss"="BroadGloss"),
              copy=TRUE) ->
    answer
  return(answer)
}

# This function calls the above function
# and uses it to compute ambiguities for each concept
# in a domain, in a language.
domainambigframe = function(domainname, languagename){
  colexdb = domaincolexdb(domainname, languagename)
  colexdb %>%
    select(full_lang_ID, clics_form, NarrowGloss) %>%
    group_by(full_lang_ID, clics_form) %>%
    # We will count how many concepts have each clics_form
    # (and full_lang_ID so we don't mix things from different datasets)
    summarize(n_concepts = n_distinct(NarrowGloss), .groups="keep") %>%
    right_join(select(colexdb, clics_form, NarrowGloss),by="clics_form") %>%
    ungroup() %>%
    # Now we select, for each concept gloss, all the different ambiguities
    # of words lexifying that concept.
    select(NarrowGloss, n_concepts) %>%
    group_by(NarrowGloss) %>%
    # We take the minimum of those as our score for the concept gloss.
    summarize(ambiguity=min(n_concepts, na.rm=TRUE), .groups="keep") ->
    answer
  return(data.frame(collect(answer)))
}

# Then this function is just a wrapper
# that will build rows for our domain ambiguity table.
newambigrow = function(domainname, languagename){
  tempambig = mean(domainambigframe(domainname,languagename)$ambiguity)
  return(c(
    Domain=domainname,
    Language=languagename,
    Ambiguity=tempambig))
}

# Now we can assemble the table we're looking for.

domainambigframe = data.frame(Domain = c("placeholder"),Language = c("placeholder"),Ambiguity=0)
domains = unique(conceptframe$Domain)
languages = unique(languagecodes$Language)
for (languagename in languages){
  for (domainname in domains){
    domainambigframe=rbind(domainambigframe,
                           newambigrow(domainname,
                                       languagename))
  }
}

domainambigframe = domainambigframe[2:nrow(domainambigframe),]
domainambigframe$Ambiguity <- as.numeric(domainambigframe$Ambiguity)

write.csv(domainambigframe,
          "A4-domainambiguities.csv",
          row.names=FALSE,
          quote=TRUE)

# Disconnect from databases.
dbDisconnect(clicsdb)
rm(clicsdb, conceptsdb, wordsdb)