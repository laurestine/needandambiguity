# This program takes the input list of Concepticon concepts,
# and searches for BabelNet and OmegaWiki words corresponding to each one,
# adding them to a .csv and outputting it.

# You have to run it multiple times on multiple days to get all of the words from BabelNet,
# because BabelNet has a query limit per day.
# The program will print how far it got;
# to re-run it, have it read in its previous output,
# and start wherever it got to on the last run.

import re # regular expressions!
import requests  # http://github.com/kennethreitz/requests # getting HTML websites
import os # manipulate working directory
import pandas as pd # working with dataframes
import numpy as np # just some useful methods like "unique"
import csv # helps write csvs
from ast import literal_eval # turning strings into lists; useful when reading in old output

# Make sure we're in the right directory
# os.chdir("")

# Set up list of languages, and codes for the databases
languages = [#"Arabic",
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
            #"Thai",
            "English"]
babelnetlangs = dict(Arabic = "AR", Mandarin = "ZH", Albanian = "SQ", Armenian = "HY",
                        Basque = "EU", Hindi = "HI", Hungarian = "HU", Indonesian = "ID",
                        Japanese = "JA", Russian = "RU", Spanish = "ES", Swahili = "SW",
                        Tamil = "TA", Thai = "TH", Turkish = "TR", English = "EN"
                    ) # ZH is actually all Chinese, not just Mandarin.
omegawikilangs = dict(Arabic = "120", Mandarin = "107", Basque = "96", Hindi = "163",
                        Russian = "88", Tamil = "164", Turkish = "137", Spanish = "87",
                        Hungarian = "102", Indonesian = "165", Swahili = "145", Thai = "130",
                        Albanian = "354", Armenian = "123", Japanese = "112", English = "85"
                    ) # Traditional Mandarin is 135. Also, 87 is actually Castilian specifically.

# Get the concept list
bigconceptlist = pd.read_csv("3-annotatedconceptlist.csv", index_col="id",quotechar='"')

# Filter by ontological category and whether it's in the set we care about (i.e. one of the domains)
bclontfilt = bigconceptlist[(bigconceptlist['ontological_category']=='Person/Thing') & (bigconceptlist['included?']==True)]

# Insert a column for each of the languages we're looking at.
for lang in languages:
    bclontfilt.insert(len(bclontfilt.columns),"words_"+lang,'')

# ************************************************************************************
# IF THIS IS THE SECOND+ TIME RUNNING IT: Read in the concept list we've already been working on.
# bclontfilt = pd.read_csv("3-annotatedconceptlistwithwords.csv",index_col="id",quotechar='"')
startindex = 0 # Change this if you want to skip the first few rows (because you've already been working on it)
# ************************************************************************************

# If any of the language-concept pairs pose problems, it will write them down.
problempairs = []

# This variable will tell us how far it gets
# before running out of BabelNet queries.
limitreached = 0

# This just lets the user know if there's been a problem,
# before it finishes running.
problemtoggle = 0

for rownum in [i for i in bclontfilt.index if i>=startindex]:
#    if rownum in range(0,4000,5): # this little conditional gives progress reports on multiples of 5.
#        print(rownum)
    # We first get the Concepticon page for the concept
    try:
        concepticonid=str(rownum)
        concepticonreq = requests.get('https://concepticon.clld.org/parameters/'+concepticonid)
    except:
        problempairs = problempairs + [(rownum, '')]
        continue
    for lang in languages:
        try:
            # This next line is only useful if we already have all the OmegaWiki words
            # and are going for the BabelNet words.
            # omegawikiwords = literal_eval(bclontfilt.at[rownum,"words_"+lang])
            # We find the OmegaWiki link on the Concepticon concept page,
            # if there is one.
            omegawikiwords = []
            omegawikigrep = re.search('Mapping to OmegaWiki</a></th></tr><tr><td>OMEGAWIKI ID</td><td><a href="(http://www.omegawiki.org/DefinedMeaning:\d+)"',concepticonreq.text)
            # We follow the OmegaWiki link and read the resulting table for our language's words.
            if omegawikigrep:
                omegawikireq = requests.get(omegawikigrep.group(1))
                omegawikiwords = re.findall('<td class="language" langid="'+omegawikilangs[lang]+'">[ \w\(\)]*?</td><td class="spelling"><a href=".*?">(.*?)</a>',omegawikireq.text)
            # We find the BabelNet link on the Concepticon concept page,
            # if there is one.
            babelnetwords = []
            babelnetgrep = re.search('Mapping to BabelNet</a></th></tr><tr><td>BABELNET ID</td><td><a href="http://babelnet.org/synset\?word=(bn:\d+\w)"',concepticonreq.text)
            # We follow the BabelNet link and read the resulting page for our language's words.
            if babelnetgrep:
                params = dict(id=babelnetgrep.group(1),
                    orig=babelnetgrep.group(1),
                    lang="EN", # English interface language
                    transLang = babelnetlangs[lang], # Target language
                    )
                babelnetreq = requests.get('https://babelnet.org/synset',params=params)
                babelnetlimit = re.search('<div class="alert alert-danger">\n\t\t\t\tDaily request limit reached\n\t\t\t</div>',babelnetreq.text)
                if not babelnetlimit:
                    babelnetwords = re.findall('--><span class="synonim" \n\t{8}data-lemma-type="HIGH_QUALITY"\n\t{7}>(.*?)\u200e?</span>',babelnetreq.text)
                else: # If we've reached the limit of what BabelNet will allow, stop everything now.
                    limitreached = rownum
                    break
            allwords = list(np.unique(omegawikiwords + babelnetwords))
            bclontfilt.at[rownum,"words_"+lang] = allwords
        except:
            problempairs = problempairs + [(rownum,lang)]
            if not problemtoggle:
                print("WARNING: There has been some kind of problem.")
                problemtoggle=1
    else: # basically if we finish the for-loop normally we're gonna flip around to restart this loop
        continue
    break


# Print how far we got, and whether any language-concept pairs threw exceptions.
print("Limit reached:")
print(limitreached)

print("Number of problem pairs:")
print(len(problempairs))
if len(problempairs) > 0:
    if len(problempairs) < 11:
        print(problempairs)
    else:
        print(problempairs[0:5])
        print("...")
        print(problempairs[-5:])

# Then output the resulting table, of course.
bclontfilt.to_csv("3-annotatedconceptlistwithwords.csv",quoting=csv.QUOTE_NONNUMERIC)
