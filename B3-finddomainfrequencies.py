# This script will read in a .csv of Concepticon concepts,
# their domains,
# and their associated words,
# and will tally up the total frequency of each domain in each language,
# writing the result to an output file.

import re # regular expressions!
import requests # http://github.com/kennethreitz/requests # getting HTML websites (most corpora)
import xml.etree.ElementTree as ET # Reading xml format. (for Helsinki corpus)
import os # manipulate working directory (to read files in)
from selenium import webdriver # Working with JavaScript stuff on websites (for Albanian)
from selenium.common.exceptions import NoSuchElementException # for checking certain errors in selenium
from selenium.webdriver.common.keys import Keys
import time # lets the program wait a few seconds or check the time
import pandas as pd # dataframe stuff
import numpy as np # some useful methods
from ast import literal_eval # reading lists out of list-looking strings
import csv # csv printing helper
from selenium.webdriver.common.action_chains import ActionChains # clicks in precise locations

# Set the working directory appropriately.
# os.chdir("")

# Read the file
bigconceptlist = pd.read_csv("B2-annotatedconceptlistwithwords.csv",index_col="id",quotechar='"')

# Break it into domains - label each row with its domain.
bigconceptlist.insert(len(bigconceptlist.columns),"Domain","")
domains = ["Kinship","Number","Clothing","Speech","Time","Emotion","Body","physGeo","Plant","Animal"]
for i in bigconceptlist.index:
    for domain in domains:
        if not pd.isna(bigconceptlist.at[i,domain]):
            bigconceptlist.at[i,"Domain"] = domain

# The following function takes a list of words,
# and increases it so that each word has copies
# with each possible Unicode encoding of the diacritics in that word.
# For example, the Spanish word Yucatán would have two versions,
# one with á encoded as a single character and one with á encoded as two characters.
# It also tries alternatives in cases where a character is optional,
# e.g. in Arabic vowels.
def cleandiacritics(words):
    words = list(words)
    # Substitutions to make:
    substits = [["ɲ","ñ","ñ"], # palatal nasal
                ["á","á"],["é","é"],["í","í"],["ó","ó"],["ú","ú"], # acute accents
                ["à","à"],["è","è"],["ì","ì"],["ò","ò"],["ù","ù"], # grave accents
                ["ä","ä"],["ë","ë"],["ï","ï"],["ö","ö"],["ü","ü"],
                ["â","â"],["ê","ê"],["î","î"],["ô","ô"],["û","û"], # circumflexes
                ["ğ","ğ"],["й","й"], # breves
                ["ç","ç"],["ş","ş"], # cedillas
                ["ő","ő"],["ű","ű"], # accent-umlauts
                ["क़","क़"],["ख़","ख़"],["ग़","ग़"],["ज़","ज़"],["ड़","ड़"],["ढ़","ढ़"], # devanagari dot https://r12a.github.io/scripts/devanagari/
                ["फ़","फ़"],["य़","य़"],["ऴ","ऴ"],["ऩ","ऩ"],["ऱ","ऱ"],["श़","झ़","झ़"], # devanagari dot
                ["ो","ाे"],["ौ","ाै"],["आ","अा"],["ओ","अो"],["औ","अौ"],["ऐ","एे"],] # devanagari vowels
    deletes = ["\u0670","\u064B","\u064C","\u064D", # optional arabic vowels
                "\u064E","\u064F","\u0650","\u0651" # optional arabic vowels
                ]
    for substit in substits:
        for char1 in substit:
            for char2 in substit:
                if char2 != char1:
                    for word in words:
                        words = words + [re.sub(char1,char2,word)]
                        words = list(np.unique(words))
    for delete in deletes: # things that just need to be deleted (not added)
        for word in words:
            words = words + [re.sub(delete,"",word)]
            words = list(np.unique(words))
    return(words)

# The first arg is a list of strings to find,
# the second arg is a list of the same length,
# with each distinct value corresponding to membership in a distinct group.
# Returns a pair of lists with the first the trimmed list of strings, the second
# the groups of the trimmed list.
# The function ensures that within a group, no string is a superstring of any other.
# If this does happen, the function discards the superstring,
# and keeps the substring.
# This helps avoid overcounting in corpus queries.
def cleansuperstrings(stringlist, groups=None):
    if groups is None:
        newlist = []
        for newstring in stringlist: # string we are considering adding
            if newstring in newlist: # if it's already there, skip it
                continue
            # else compare it to the other strings
            for i in range(0,len(newlist)):
                if newlist[i] in newstring: # if there is already a substring of our new string, skip this
                    break
                elif newstring in newlist[i]: # if we are a substring of something there, replace it
                    newlist[i] = newstring
            else: # if we didn't find a substring of our new string, add new string
                newlist = newlist + [newstring]
            # we may have introduced duplicates; remove them
            newlist = list(np.unique(newlist))
        return newlist
    else:
        newlist = []
        newgroups = []
        for i in range(0,len(stringlist)): # index of string we are considering adding
            # if it's already there IN THAT GROUP, skip it
            if stringlist[i] in [newlist[j] for j in range(0,len(newlist)) if newgroups[j] == groups[i]]:
                continue
            # else compare it to the other strings
            for j in range(0,len(newlist)):
                # if there is already a substring of our new string IN THAT GROUP, skip this
                if (newlist[j] in stringlist[i]) and (newgroups[j] == groups[i]): 
                    break
                # if we are a substring of something IN OUR GROUP, replace it
                elif (stringlist[i] in newlist[j]) and (newgroups[j] == groups[i]):
                    newlist[j] = stringlist[i]
            else: # if we didn't find a substring of our new string, add new string
                newlist = newlist + [stringlist[i]]
                newgroups = newgroups + [groups[i]]
            # we may have introduced duplicates; remove them
            uniquefied,indeces = np.unique(list(zip(newlist,newgroups)),axis=0,return_index=True)
            newlist = [newlist[k] for k in indeces]
            newgroups = [newgroups[k] for k in indeces]
        return newlist, newgroups

# Make space in the big frame to put frequencies
languages = [
    "Arabic", # diacritics taken care of
    "Indonesian", # diacritics taken care of
    "Basque", # diacritics taken care of
    "Tamil", # diacritics taken care of
    "Albanian", # diacritics taken care of
    "Hindi", # diacritics taken care of
    "Russian", # diacritics taken care of (?)
    "Spanish", # diacritics taken care of
    "Swahili", # diacritics taken care of
    "Turkish", # diacritics taken care of
    "Hungarian",# diacritics taken care of
#    "Thai",
    "English" # diacritics taken care of
    ]
for lang in languages:
    bigconceptlist.insert(len(bigconceptlist.columns),"freq_"+lang,'')

# Make a list of words-with-domains for each language.
worddomainlist = pd.DataFrame(columns = ["Word","Domain","Language"])
for lang in languages:
    for domain in domains:
        try:
            domainwords = []
            bclcells = bigconceptlist[bigconceptlist["Domain"] == domain]["words_"+lang]
            for cell in bclcells:
                if type(cell) == type(""):
                    if len(cell) > 0:
                        if type(literal_eval(cell))==type([]):
                            domainwords = domainwords + literal_eval(cell)
            # Ensure we try every possible encoding of relevant diacritics.
            domainwords=cleandiacritics(domainwords)
            worddomainlist = worddomainlist.append(pd.DataFrame(
                data={
                    "Word":domainwords,
                    "Domain":list(np.repeat(domain,len(domainwords))),
                    "Language":list(np.repeat(lang,len(domainwords)))
                }
            ),ignore_index=True)
        except:
            continue

# To get the list of words to look up in a language,
# we will make sure that we are not looking up any superstring of another string
# (as this will overcount) with the cleansuperstrings method.
def languagewords(language):
    return cleansuperstrings(list(worddomainlist[worddomainlist["Language"]==language]["Word"]),
                                list(worddomainlist[worddomainlist["Language"]==language]["Domain"]))

# There's a login for some of these sites.
# It should be anonymized for now, so it won't actually work.
username = "NOTHING"
password = "NOTHING"


# We'll put all the frequencies in a dataframe as we find them.
freqdomainlist = pd.DataFrame(columns = ["Frequency","Domain","Language"])
# MARK Basque
if "Basque" in languages:
    print("Basque")
    totalsize = 203800000
    wordlist, domainlist = languagewords("Basque")
    for domain in domains:
        print(domain)
        domaintokens = 0
        domainwords = [wordlist[i] for i in range(len(wordlist)) if domainlist[i] == domain]
        starttime = time.time()
        for word in domainwords:
            if(time.time()-starttime) > 600:
                print("Timed out on this domain.") # Just a little failsafe so it doesn't run forever.
                break
            params = dict(bila=word) # search by lemma
            # params = dict(bila="-"+word) # wordform
            req = requests.get("https://www.ehu.eus/etc/",params=params)
            res = re.search('prentsa</b></td>\n\t\t<td class="datuak"><div class="geziBila"><img .*?></div>((\d+.)*\d+)</td>',req.text)
            count=0
            if res:
                count=int(res.group(1).replace('.', ''))
            domaintokens = domaintokens + count
        freqdomainlist = freqdomainlist.append(pd.DataFrame(
                data={
                    "Frequency":[domaintokens/totalsize],
                    "Domain":[domain],
                    "Language":["Basque"]
                }), ignore_index = True)
# MARK English
if "English" in languages:
    print("English")
    # Connect to Lancaster site.
    payload = {"username": username,
            "password":password,
            "userAction":"userLogin",
            "persist":"1"}
    lancastersession = requests.session()
    login_url = "https://cqpweb.lancs.ac.uk/usr/useracct-act.php"
    loginresult = lancastersession.post(login_url,
                data=payload)
    # These next few lines help to check whether you successfully logged in.
    #mainpage = lancastersession.get(
    #   "https://cqpweb.lancs.ac.uk/emilletamwbnws/"
    #)
    #re.search("You are (not )?logged in.",mainpage.text).group(0)
    totalsize = 10594781
    wordlist, domainlist = languagewords("English")
    for domain in domains:
        print(domain)
        domaintokens = 0
        domainwords = [wordlist[i] for i in range(len(wordlist)) if domainlist[i] == domain]
        for word in domainwords:
            params = dict(qdata="{"+word+"}", # curly brackets for lemmatization
                            #qdata=word, # not lemmatized
                            qmode="sq_nocase", # case-insensitive
                            pp=50, # Show 50 results on first page
                            qstrategy=0, # standard match strategy
                            t="~sc~28044" # my custom press subcorpus
                        )
            req = lancastersession.get("https://cqpweb.lancs.ac.uk/bncxmlweb/concordance.php",params=params)
            res = re.search("returned ((\d+,)*\d+) matches in ",req.text)
            count = 0
            if res:
                count=int(res.group(1).replace(',', ''))
            domaintokens = domaintokens + count
        freqdomainlist = freqdomainlist.append(pd.DataFrame(
                data={
                    "Frequency":[domaintokens/totalsize],
                    "Domain":[domain],
                    "Language":["English"]
                }), ignore_index = True)
# MARK Albanian
# This one needs you to have a Firefox webdriver installed,
# as well as actual Firefox.
# Opening an actual browser window is the only way I found
# to systematically query this database.
# See https://pythonbasics.org/selenium-firefox/
if "Albanian" in languages:
    print("Albanian")
    totalsize = 23400000
    wordlist, domainlist = languagewords("Albanian")
    browser = webdriver.Firefox()
    for domain in domains:
        print(domain)
        domaintokens = 0
        domainwords = [wordlist[i] for i in range(len(wordlist)) if domainlist[i] == domain]
        for word in domainwords:
            # We will physically search the website for each word on our list.
            browser.get('http://albanian.web-corpora.net/albanian_corpus/search')
            starttime=time.time()
            while (time.time() - starttime) <= 60:
                try:
                    searchbox = browser.find_element_by_id("wf1")
                    searchbox.clear()
                    searchbox.send_keys(word)
                    break
                except:
                    continue
            else: # if we time out trying to find the search box, just skip the word.
                print("timed out on finding search box:"+word)
                continue
            # Choose subcorpus.
            subcorpusbutton = browser.find_element_by_id("search_doc")
            subcorpusbutton.click()
            time.sleep(1)
            subcorpusbox = browser.find_elements_by_id("genre")[1] # there's two boxes, we want the second.
            subcorpusbox.clear()
            subcorpusbox.send_keys("press newspaper") # subcorpus
            time.sleep(1) # takes a moment for the "press newspaper" input to send to send
            subcorpusselect = browser.find_element_by_id("subcorpus_selector_ok")
            subcorpusselect.click()
            # Click on the "search lemmata" button as soon as you can.
            # If it takes more than 1.5 minutes, just give up.
            starttime = time.time()
            while (time.time() - starttime) <= 90:
                try:
                    lemmatabutton = browser.find_element_by_id("search_lemma") # search by lemma
                    lemmatabutton.click() # search by lemma
                    # Word Forms button is annoying, we have to click the edge
                    # wordformsbutton = browser.find_element_by_id("search_word")
                    # action = ActionChains(browser)
                    # move to the middle of the right edge to click
                    # action.move_to_element_with_offset(
                    #    wordformsbutton,
                    #    wordformsbutton.rect['width']-2,
                    #    wordformsbutton.rect['height']/2
                    #    ).click().perform()
                    break
                except:
                    continue
            else:
                print("timed out on pressing search button:" + word)
                continue
            # Get the final count as soon as you can.
            # If it takes more than 1 minute, just give up.
            starttime = time.time()
            while (time.time() - starttime) <= 60:
                try:
                    resultsinfo = browser.find_element_by_id("results_info")
                    break
                except NoSuchElementException:
                    continue
            else:
                print("timed out on searching for results:" + word)
                continue
            try:
                # lemma and wordform have results in slightly different places
                # this is for lemma:
                resultsquare = browser.find_element_by_xpath("//table[@class='words_list_table']/tbody/tr[1]/td[3]")
                # this is for wordform:
                #resultsquare = browser.find_element_by_xpath("//table[@class='words_list_table']/tbody/tr[1]/td[4]")
                count=int(resultsquare.text)
            except NoSuchElementException:
                count=0
            domaintokens = domaintokens + count
        freqdomainlist = freqdomainlist.append(pd.DataFrame(
                data={
                    "Frequency":[domaintokens/totalsize],
                    "Domain":[domain],
                    "Language":["Albanian"]
                }), ignore_index = True)
    browser.quit()
# MARK Russian
if "Russian" in languages:
    print("Russian")
    totalsize = 332645828
    wordlist, domainlist = languagewords("Russian")
    for domain in domains:
        print(domain)
        domaintokens = 0
        domainwords = [wordlist[i] for i in range(len(wordlist)) if domainlist[i] == domain]
        for word in domainwords:
            # This is for searching lemmatized:
            params = dict(env="alpha", 
                            api="1.0",
                            mycorp="",
                            mysent="",
                            mysize="",
                            mysentsize="",
                            dpp="",
                            spp="",
                            spd="",
                            mydocsize="",
                            mode="paper", # search in newspaper database
                            sort="i_grtagging",
                            lang="en", # English interface language
                            nodia=1,
                            text="lexgramm", # lemma search
                            parent1=0,
                            level1=0,
                            lex1="+"+word,
                            gramm1="", # set to "S" to only look for nouns
                            sem1="",
                            flags1="",
                            parent2=0,
                            level2=0,
                            min2=1,
                            max2=1,
                            lex2="",
                            gramm2="",
                            sem2="",
                            flags2=""
                            )
            # Non-lemmatized:
            # params = dict(env="alpha", 
            #                api="1.0",
            #                mycorp="",
            #                mysent="",
            #                mysize="",
            #                mysentsize="",
            #                dpp="",
            #                spp="",
            #                spd="",
            #                mydocsize="",
            #                mode="paper", # search in newspaper database
            #                sort="i_grtagging",
            #                lang="en", # English interface language
            #                nodia=1,
            #                text="lexform", # wordform, not lemma
            #                req=word
            #                )
            req = requests.get("https://processing.ruscorpora.ru/search.xml",params=params)
            res = re.search('<span class="stat-number">((\d+ )*\d+)</span> <span class="stat-caption">contexts</span>',req.text)
            count=0
            if res:
                count=int(res.group(1).replace(' ', ''))
            domaintokens = domaintokens + count
        freqdomainlist = freqdomainlist.append(pd.DataFrame(
                data={
                    "Frequency":[domaintokens/totalsize],
                    "Domain":[domain],
                    "Language":["Russian"]
                }), ignore_index = True)
# MARK Swahili
# This one is run a little differently,
# since it queries text corpora stored on your own computer,
# not on the internet.
# Consequently, it's easier to do all the domains at once,
# one text at a time.
if "Swahili" in languages:
    helsinkisourcename1 = "Helsinki corpus\\hcs2_new_news.vrt"
    helsinkisourcename2 = "Helsinki corpus\\hcs2_old_news.vrt"
    totalsize = 15374467
    # These first few lines are dedicated to parsing the Helsinki corpus format.
    with open(helsinkisourcename1, "r", encoding="utf8") as infile:
        helsinkisource1 = infile.read()
    with open(helsinkisourcename2, "r", encoding="utf8") as infile:
        helsinkisource2 = infile.read()
    helsinkisourcesplit1 = helsinkisource1.split("</text>\n<text")
    helsinkisourcesplit2 = helsinkisource2.split("</text>\n<text")
    helsinkitexts1 = ([helsinkisourcesplit1[0]+"</text>"] + 
                    [ "<text"+x+"</text>" for x in helsinkisourcesplit1[1:-1]] + 
                    ["<text"+helsinkisourcesplit1[-1]])
    helsinkitexts2 = ([helsinkisourcesplit2[0]+"</text>"] + 
                    [ "<text"+x+"</text>" for x in helsinkisourcesplit2[1:-1]] + 
                    ["<text"+helsinkisourcesplit2[-1]])
    helsinkitreeroots1 = [ET.fromstring(x) for x in helsinkitexts1]
    helsinkitreeroots2 = [ET.fromstring(x) for x in helsinkitexts2]
    counts = list(np.repeat(0,len(domains)))
    wordlist, domainlist = languagewords("Swahili")
    # Now we can loop thorugh the texts.
    for rootnum in range(len(helsinkitreeroots1)):
        print("root: " + str(rootnum))
        for sentence in helsinkitreeroots1[rootnum]:
            textrows = sentence.text.split("\n")
            nonzerorows = list(filter(lambda x: len(x)>0, textrows))
            for row in nonzerorows:
                token = row.split("\t")
                for domainnum in range(len(domains)):
                    domainwords = [wordlist[i] for i in range(len(wordlist)) if domainlist[i] == domains[domainnum]]
                    if token[1] in domainwords: # lemma
                    #if token[0] in domainwords: # wordform
                        counts[domainnum] = counts[domainnum] + 1
    for rootnum in range(len(helsinkitreeroots2)):
        print("root: " + str(rootnum))
        for sentence in helsinkitreeroots2[rootnum]:
            textrows = sentence.text.split("\n")
            nonzerorows = list(filter(lambda x: len(x)>0, textrows))
            for row in nonzerorows:
                token = row.split("\t")
                for domainnum in range(len(domains)):
                    domainwords = [wordlist[i] for i in range(len(wordlist)) if domainlist[i] == domains[domainnum]]
                    #if token[1] in domainwords: # lemma
                    if token[0] in domainwords:
                        counts[domainnum] = counts[domainnum] + 1
    for domainnum in range(len(domains)):
        freqdomainlist = freqdomainlist.append(pd.DataFrame(
                data={
                    "Frequency":[counts[domainnum]/totalsize],
                    "Domain":[domains[domainnum]],
                    "Language":["Swahili"]
                }), ignore_index = True)
# MARK Turkish
if "Turkish" in languages:
    print("Turkish")
    totalsize = 18164832
    # Log in.
    payload = {"username": username,
            "password":password,
            "redirect":"userLogin",
            "uT":"y",
            "persist":"1"}
    turkishsession = requests.session()
    login_url = "http://cqpweb.tscorpus.com/cqpweb/usr/redirect.php"
    loginresult = turkishsession.post(login_url,
                data=payload)
    # These couple of lines are useful to make sure if you're logged in or not.
    #mainpage = turkishsession.get(
    #    "http://cqpweb.tscorpus.com/cqpweb/usr/index.php"
    #)
    #re.search("You are (not )?logged in",mainpage.text).group(0)
    wordlist, domainlist = languagewords("Turkish")
    for domain in domains:
        print(domain)
        domaintokens = 0
        domainwords = [wordlist[i] for i in range(len(wordlist)) if domainlist[i] == domain]
        for word in domainwords:
            params = dict(theData="{"+word+"}", # The curly brackets mean lemmatization.
                            #theData=word, # no lemmatization with this version
                            qmode="sq_nocase", # case-insensitive
                            pp=50, # Show 50 results on first page
                            t="", # no subcorpus
                            uT="y"
                        )
            req = turkishsession.get("http://cqpweb.tscorpus.com/cqpweb/columns_v2/concordance.php",params=params)
            res = re.search("returned ((\d+,)*\d+) matches in ",req.text)
            count = 0
            if res:
                count=int(res.group(1).replace(',', ''))
            domaintokens = domaintokens + count
        freqdomainlist = freqdomainlist.append(pd.DataFrame(
                data={
                    "Frequency":[domaintokens/totalsize],
                    "Domain":[domain],
                    "Language":["Turkish"]
                }), ignore_index = True)
# MARK Hungarian
if "Hungarian" in languages:
    print('Hungarian')
    totalsize = 84500000
    wordlist, domainlist = languagewords('Hungarian')
    for domain in domains:
        print(domain)
        domaintokens = 0
        domainwords = [wordlist[i] for i in range(len(wordlist)) if domainlist[i] == domain]
        for word in domainwords:\
            # For searching by lemma:
            params = {"corpname":"MNSZ2", "reload":"",
                "queryselector":"lemmarow", "iquery":"",
                "lemma":word,"word":"", "char":"", "cql":"", "default_attr":"word", "m_attrib":"word", "m_szotoszoalak":"", "m_foszofaj":"", "m_szofaj":"(FN|FN_NM|MN|MN_NM|SZN|SZN_NM|SZN_DIGIT|ROMAN|HA|HA_NM|DET_NM|IGE\._MIB|IGE\._OKEP|IGE\._MIA)", "m_fok":"(FF\.)?,(\._FOK)?","m_szam":"(\.(PL|FAM))?","m_birt":"(\.PS)?","m_birt_szam":"[et]?","m_birt_szemely":"[123]?","m_birt_tobbesito":"i?","m_anaf":"(\.POS|\.POSi)?","m_anaf_tobbesito":"(\.POS|\.POSi)","m_eset":"\.(...|KEPP|KEPPEN|_TMP_ANTE|_TMP_INL|INL)","m_ige_igekoto":"(IK\.)*","m_ige_hato":"(_HAT\.)?","m_ige_ragozas":"[TI]?","m_ige_idomod":"[MPF]?","m_ige_szam":"[et]?","m_ige_szemely":"[123]?","m_fni_igekoto":"(IK\.)*","m_fni_szam":"F","m_fni_szemely":"[123]?\*?","m_hati_igekoto":"(IK\.)*","m_punct_tipus":".PUNCT","mf_morph":"","mf_morpheme":"","fc_lemword_window_type":"both","fc_lemword_wsize":5,"fc_lemword":"","fc_lemword_type":"all","sca_doc.file":"",
                "sca_doc.style":"sajtó", "sca_div.column":"", "sca_div.type":""}
            # Not by lemma:
            # params = {"corpname":"MNSZ2", "reload":"",
            #     "queryselector":"wordrow", "iquery":"",
            #     "lemma":"","word":word, "char":"", "cql":"", "default_attr":"word", "m_attrib":"word", "m_szotoszoalak":"", "m_foszofaj":"", "m_szofaj":"(FN|FN_NM|MN|MN_NM|SZN|SZN_NM|SZN_DIGIT|ROMAN|HA|HA_NM|DET_NM|IGE\._MIB|IGE\._OKEP|IGE\._MIA)", "m_fok":"(FF\.)?,(\._FOK)?","m_szam":"(\.(PL|FAM))?","m_birt":"(\.PS)?","m_birt_szam":"[et]?","m_birt_szemely":"[123]?","m_birt_tobbesito":"i?","m_anaf":"(\.POS|\.POSi)?","m_anaf_tobbesito":"(\.POS|\.POSi)","m_eset":"\.(...|KEPP|KEPPEN|_TMP_ANTE|_TMP_INL|INL)","m_ige_igekoto":"(IK\.)*","m_ige_hato":"(_HAT\.)?","m_ige_ragozas":"[TI]?","m_ige_idomod":"[MPF]?","m_ige_szam":"[et]?","m_ige_szemely":"[123]?","m_fni_igekoto":"(IK\.)*","m_fni_szam":"F","m_fni_szemely":"[123]?\*?","m_hati_igekoto":"(IK\.)*","m_punct_tipus":".PUNCT","mf_morph":"","mf_morpheme":"","fc_lemword_window_type":"both","fc_lemword_wsize":5,"fc_lemword":"","fc_lemword_type":"all","sca_doc.file":"",
            #     "sca_doc.style":"sajtó", "sca_div.column":"", "sca_div.type":""}
            req = requests.get("http://clara.nytud.hu/mnsz2-dev/bonito/run.cgi/first",
                params=params,
                auth=(username,password))
            res = re.search("Hits: <strong>(\d+)</strong",req.text)
            count = 0
            if res:
                count=int(res.group(1).replace(',', ''))
            domaintokens = domaintokens + count
        freqdomainlist = freqdomainlist.append(pd.DataFrame(
                data={
                    "Frequency":[domaintokens/totalsize],
                    "Domain":[domain],
                    "Language":['Hungarian']
                }), ignore_index = True)

freqdomainlist.to_csv("B4-domainfrequencies.csv",quoting=csv.QUOTE_NONNUMERIC)
