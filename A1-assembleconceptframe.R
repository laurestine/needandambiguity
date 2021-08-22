# Set working directory to wherever is appropriate.
# setwd("")
# This is where the program will put its ultimate output.

# This function takes the broader-narrower relations given by Concepticon ("conceptkidslist"),
# and making a dataframe
# with each concept and all of the narrower concepts
# which are at the narrowest level ("basicconcepts").
relframemaker = function(conceptkidslist,basicconcepts){
  answerframe = data.frame(BroadGloss = character(), NarrowGloss = character())
  broaders = names(conceptkidslist)
  for (concept in broaders){ # add each thing and its descendants
    descendants = unique(basicdescendants(concept,concept,conceptkidslist,basicconcepts))
    answerframe = rbind(answerframe,data.frame(BroadGloss =rep(concept,length(descendants)), NarrowGloss = descendants))
  }
  for (concept in basicconcepts){
    # each basic concept is its own parent,
    # but we ignore any which are already broader than something.
    if (!(concept %in% names(conceptkidslist))){
      answerframe = rbind(answerframe,data.frame(BroadGloss =concept,NarrowGloss =concept))
    }
  }
  return(answerframe)
}

# This is a helper to "relframemaker" above.
# It finds the basic ("basicconcepts") descendants (along the broader-narrower tree)
# of a concept, by depth-first search.
# The argument "visitedalready" lets us detect cycles and stop them, so we don't
# traverse the tree forever.
# In this function, too, a descendant isn't treated as "basic" if it has its own descendants.
basicdescendants = function(member,visitedalready,conceptkidslist,basicconcepts){
  answer = c()
  children = conceptkidslist[[member]]
  for(concept in children){
    if(concept %in% visitedalready){
      # If we've traversed this node before, ignore it.
      next
    } else if(concept %in% names(conceptkidslist)){
      # If we haven't, and it has children, go find all of its descendants and add them.
      answer = c(answer,
                 basicdescendants(concept,
                                  c(visitedalready,concept),
                                  conceptkidslist,
                                  basicconcepts))
    } else if(concept %in% basicconcepts){
      # If it has no descendants, then add it to the list of descendants to return,
      # assuming it is in the set of "basicconcepts" that we can do that with.
      answer = c(answer,concept)
      next
    }
  }
  return(answer)
}

domainframe = data.frame(BroadGloss = character(), NarrowGloss = character(), Domain = character())

#### Now we will create such a frame of broader-narrowest relations for each domain. ####
# The relations here are all taken directly from the ConcepticonRelations file,
# with some "Other" concepts added (e.g. "OTHER UNCLE"), as described in the text.
#### KINSHIP ####
kinrelations = list()

### LET US NOW do the ALGORITHM propertly
basickinconcepts = c("OTHER PATERNAL UNCLE","OTHER UNCLE","OTHER PATERNAL AUNT (FATHER'S SISTER)","OTHER MATERNAL AUNT","OTHER AUNT OR MOTHER-IN-LAW","OTHER GRANDMOTHER","OTHER SISTER-IN-LAW","OTHER GRANDCHILD","OTHER GRANDSON","FATHER'S SISTER","MOTHER'S SISTER","SIBLING'S CHILD","GRANDPARENTS","PARENTS-IN-LAW","OLDER SIBLING","YOUNGER SIBLING","PARENTS","BROTHER (OF MAN)","BROTHER (OF WOMAN)","SISTER (OF MAN)","SISTER (OF WOMAN)","TWINS","MOTHER-IN-LAW","FATHER-IN-LAW","SON-IN-LAW","DAUGHTER-IN-LAW","NEPHEW","WIFE","HUSBAND","MOTHER","FATHER","BROTHER","SISTER","AUNT","UNCLE","DAUGHTER","GRANDFATHER","GRANDMOTHER","GRANDCHILD","GRANDSON","GRANDDAUGHTER","SON","SIBLING","NIECE","COUSIN","RELATIVES","OLDER SISTER","OLDER BROTHER","YOUNGER BROTHER","YOUNGER SISTER","CHILD (DESCENDANT)","SIBLING-IN-LAW","MOTHER'S BROTHER","PATERNAL UNCLE (FATHER'S BROTHER)","PATERNAL AUNT (WIFE OF FATHER'S OLDER BROTHER)","PATERNAL AUNT (WIFE OF FATHER'S YOUNGER BROTHER)","MATERNAL AUNT (WIFE OF MOTHER'S BROTHER)","PATERNAL UNCLE (FATHER'S OLDER BROTHER)","PATERNAL UNCLE (FATHER'S YOUNGER BROTHER)","PATERNAL AUNT","MATERNAL AUNT","MATERNAL GRANDMOTHER","MATERNAL GRANDFATHER","FATHER-IN-LAW (OF WOMAN)","FATHER-IN-LAW (OF MAN)","MOTHER-IN-LAW (OF WOMAN)","MOTHER-IN-LAW (OF MAN)","BROTHER-IN-LAW","SISTER-IN-LAW","BROTHER-IN-LAW (OF MAN)","AUNT OR MOTHER-IN-LAW","COUSIN OR SISTER-IN-LAW","UNCLE OR FATHER-IN-LAW","BOY OR SON","DAUGHTER OR GIRL","GRANDPARENT","OLDER BROTHER (OF MAN)","OLDER BROTHER (OF WOMAN)","YOUNGER BROTHER (OF MAN)","YOUNGER BROTHER (OF WOMAN)","OLDER SISTER (OF MAN)","OLDER SISTER (OF WOMAN)","YOUNGER SISTER (OF MAN)","YOUNGER SISTER (OF WOMAN)","PATERNAL GRANDFATHER","GRANDFATHER OR OLDER BROTHER","HUSBAND OR MALE PERSON","AUNT OR UNCLE","UNCLE (HUSBAND OF FATHER'S SISTER)","PATERNAL AUNT (FATHER'S SISTER)","MATERNAL UNCLE (MOTHER'S BROTHER)","MATERNAL AUNT (MOTHER'S SISTER)","MATERNAL UNCLE (HUSBAND OF ONE'S MATERNAL AUNT)","SISTER-IN-LAW (OLDER BROTHER'S WIFE)","BROTHER-IN-LAW (OLDER SISTER'S HUSBAND)","BROTHER-IN-LAW (YOUNGER SISTER'S HUSBAND)","BROTHER-IN-LAW (HUSBAND'S OLDER BROTHER)","BROTHER-IN-LAW (HUSBAND'S YOUNGER BROTHER)","SISTER-IN-LAW (HUSBAND'S OLDER SISTER)","SISTER-IN-LAW (HUSBAND'S YOUNGER SISTER)","GRANDSON (SON'S SON)","GRANDCHILD (DAUGHTER'S OFFSPRING)","FEMALE PERSON OR MOTHER","PATERNAL GRANDMOTHER","SAME-SEX OLDER SIBLING","SAME-SEX YOUNGER SIBLING","DIFFERENT-SEX OLDER SIBLING","DIFFERENT-SEX YOUNGER SIBLING","PATERNAL AUNT (FATHER'S OLDER SISTER)","MATERNAL AUNT (MOTHER'S OLDER SISTER)","MATERNAL AUNT (MOTHER'S YOUNGER SISTER)","CROSS-COUSIN","MATERNAL UNCLE","MATERNAL UNCLE (MOTHER'S OLDER BROTHER)","MATERNAL UNCLE (MOTHER'S YOUNGER BROTHER)","PATERNAL UNCLE","MALE COUSIN","FEMALE COUSIN","NEPHEW OR NIECE","FATHER (OF A WOMAN)","FATHER (OF A MAN)","MOTHER (OF A WOMAN)","MOTHER (OF A MAN)","GRANDCHILD (SON'S OFFSPRING)")

kinrelations[['FEMALE PERSON OR MOTHER']] = 'MOTHER'
kinrelations[['SPOUSE']] = c('HUSBAND','WIFE')
kinrelations[['HUSBAND OR MALE PERSON']] = c('HUSBAND')
kinrelations[['NEPHEW OR NIECE']] = c('NEPHEW','NIECE')
kinrelations[['BROTHER-IN-LAW']] = c("BROTHER-IN-LAW (HUSBAND'S YOUNGER BROTHER)","BROTHER-IN-LAW (HUSBAND'S OLDER BROTHER)",
                                     "BROTHER-IN-LAW (YOUNGER SISTER'S HUSBAND)","BROTHER-IN-LAW (OLDER SISTER'S HUSBAND)",
                                     "BROTHER-IN-LAW (OF MAN)") #exhaustive assuming gender binary + hetero
kinrelations[['DAUGHTER-IN-LAW']]=c('DAUGHTER-IN-LAW (OF WOMAN)','DAUGHTER-IN-LAW (OF MAN)')
kinrelations[['SON-IN-LAW']] = c('SON-IN-LAW (OF WOMAN)', 'SON-IN-LAW (OF MAN)')
kinrelations[["GRANDSON"]]=c("GRANDSON (SON'S SON)","OTHER GRANDSON")
kinrelations[["GRANDCHILD"]]=c("GRANDCHILD (DAUGHTER'S OFFSPRING)","OTHER GRANDCHILD")
kinrelations[['CHILD']]='CHILD (DESCENDANT)'
kinrelations[['CHILD (DESCENDANT)']]=c('SON','DAUGHTER')
kinrelations[['BOY OR SON']]='SON'
kinrelations[['DAUGHTER OR GIRL']]='DAUGHTER'
kinrelations[['MATERNAL UNCLE']]=c("MATERNAL UNCLE (MOTHER'S BROTHER)",
                                   "MATERNAL UNCLE (HUSBAND OF ONE'S MATERNAL AUNT)") # exhaustive assuming hetero
kinrelations[["MATERNAL UNCLE (MOTHER'S BROTHER)"]] = c("MATERNAL UNCLE (MOTHER'S OLDER BROTHER)",
                                                        "MATERNAL UNCLE (MOTHER'S YOUNGER BROTHER)")
kinrelations[['COUSIN OR SISTER-IN-LAW']] = c("COUSIN","SISTER-IN-LAW")
kinrelations[['COUSIN']] = c("MALE COUSIN","FEMALE COUSIN","CROSS-COUSIN")
kinrelations[['SISTER-IN-LAW']] = c("SISTER-IN-LAW (HUSBAND'S YOUNGER SISTER)","SISTER-IN-LAW (HUSBAND'S OLDER SISTER)",
                                    "SISTER-IN-LAW (OLDER BROTHER'S WIFE)","OTHER SISTER-IN-LAW")
kinrelations[['SIBLING']] = c('OLDER SIBLING','YOUNGER SIBLING','BROTHER','SISTER')
kinrelations[['OLDER SIBLING']] = c('OLDER SISTER','OLDER BROTHER')
kinrelations[['YOUNGER SIBLING']] = c('YOUNGER SISTER','YOUNGER BROTHER')
kinrelations[['SISTER']] = c('OLDER SISTER','YOUNGER SISTER','SISTER (OF MAN)','SISTER (OF WOMAN)')
kinrelations[['BROTHER']] = c('OLDER BROTHER','YOUNGER BROTHER','BROTHER (OF MAN)','BROTHER (OF WOMAN)')
kinrelations[['OLDER SISTER']] = c('OLDER SISTER (OF WOMAN)','OLDER SISTER (OF MAN)')
kinrelations[['YOUNGER SISTER']] = c('YOUNGER SISTER (OF WOMAN)','YOUNGER SISTER (OF MAN)')
kinrelations[['OLDER BROTHER']] = c('OLDER BROTHER (OF WOMAN)','OLDER BROTHER (OF MAN)')
kinrelations[['YOUNGER BROTHER']] = c('YOUNGER BROTHER (OF WOMAN)','YOUNGER BROTHER (OF MAN)')
kinrelations[['SISTER (OF MAN)']] = c('OLDER SISTER (OF MAN)','YOUNGER SISTER (OF MAN)')
kinrelations[['SISTER (OF WOMAN)']] = c('OLDER SISTER (OF WOMAN)','YOUNGER SISTER (OF WOMAN)')
kinrelations[['BROTHER (OF MAN)']] = c('OLDER BROTHER (OF MAN)','YOUNGER BROTHER (OF MAN)')
kinrelations[['BROTHER (OF WOMAN)']] = c('OLDER BROTHER (OF WOMAN)','YOUNGER BROTHER (OF WOMAN)')
kinrelations[['GRANDFATHER OR OLDER BROTHER']] = c('GRANDFATHER','OLDER BROTHER')
kinrelations[['GRANDPARENT']] = c('GRANDFATHER','GRANDMOTHER')
kinrelations[['GRANDFATHER']] = c('PATERNAL GRANDFATHER','MATERNAL GRANDFATHER')
kinrelations[['GRANDMOTHER']] = c('MATERNAL GRANDMOTHER','OTHER GRANDMOTHER')
kinrelations[['UNCLE OR FATHER-IN-LAW']] = c('UNCLE','FATHER-IN-LAW')
kinrelations[['FATHER-IN-LAW']] = c("FATHER-IN-LAW (OF MAN)","FATHER-IN-LAW (OF WOMAN)")
kinrelations[['AUNT OR MOTHER-IN-LAW']] = c('AUNT','MOTHER-IN-LAW (OF WOMAN)','OTHER AUNT OR MOTHER-IN-LAW')
kinrelations[['MOTHER-IN-LAW']] = c("MOTHER-IN-LAW (OF MAN)","MOTHER-IN-LAW (OF WOMAN)")
kinrelations[['AUNT OR UNCLE']] = c('AUNT','UNCLE')
kinrelations[['AUNT']] = c('PATERNAL AUNT','MATERNAL AUNT')
kinrelations[['MATERNAL AUNT']] = c("MATERNAL AUNT (WIFE OF MOTHER'S BROTHER)", "OTHER MATERNAL AUNT")
kinrelations[['PATERNAL AUNT']] = c("PATERNAL AUNT (FATHER'S SISTER)",
                                    "PATERNAL AUNT (WIFE OF FATHER'S OLDER BROTHER)",
                                    "PATERNAL AUNT (WIFE OF FATHER'S YOUNGER BROTHER)") #exhsive if hetero
kinrelations[["PATERNAL AUNT (FATHER'S SISTER)"]] = c("PATERNAL AUNT (FATHER'S OLDER SISTER)",
                                                      "OTHER PATERNAL AUNT (FATHER'S SISTER)")
kinrelations[["UNCLE"]] = c("PATERNAL UNCLE (FATHER'S BROTHER)","OTHER UNCLE")
kinrelations[["PATERNAL UNCLE (FATHER'S BROTHER)"]] = c("PATERNAL UNCLE (FATHER'S OLDER BROTHER)",
                                                        "PATERNAL UNCLE (FATHER'S YOUNGER BROTHER)")
kinrelations[["PATERNAL UNCLE"]] = c("PATERNAL UNCLE (FATHER'S OLDER BROTHER)",
                                     "PATERNAL UNCLE (FATHER'S YOUNGER BROTHER)",
                                     "OTHER PATERNAL UNCLE")

kinframe = relframemaker(kinrelations,basickinconcepts)
kinframe$Domain = 'Kinship'
domainframe = rbind(domainframe,kinframe)

#### TIME ####
timerelations = list()
basictimeconcepts = c("MIDDAY","PERIOD","SEASON","WHILE","DAWN","END (OF TIME)","MENSTRUATION","TIME","LOW TIDE","DAY (NOT NIGHT)","YEAR","DAY (24 HOURS)","AUTUMN","MORNING","SUMMER","HOUR","MONTH","WEEK","WINTER","SPRINGTIME","EVENING","FRIDAY","THURSDAY","WEDNESDAY","TUESDAY","MONDAY","SUNDAY","SATURDAY","AFTERNOON","JANUARY","FEBRUARY","DRAGON BOAT FESTIVAL","MID-AUTUMN FESTIVAL","NEW YEAR'S EVE","SUNSET","SUNRISE","DUSK","FUTURE","NOON","MIDNIGHT","FORENOON","THIS MONTH","LAST MONTH","NEXT MONTH","MARCH","APRIL","MAY (MONTH)","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER","HOT SEASON","MONSOON SEASON","THIRD DAY OF THE MONTH","FIRST DAY OF THE MONTH","SECOND DAY OF THE MONTH","HARVEST OR HARVEST SEASON","FULL MOON","NEW MOON","WAXING MOON","WANING MOON")

timerelations[['DAY OR SUN']] = 'DAY (NOT NIGHT)'
timerelations[['MOON OR MONTH']] = 'MONTH'

timeframe = relframemaker(timerelations,basictimeconcepts)
timeframe$Domain = 'Time'
domainframe = rbind(domainframe,timeframe)

#### CLOTHING ####
clothingrelations = list()
basicclothingconcepts = c("SNOWSHOE","CLOAK","PONCHO","GRASS-SKIRT","OTHER STOCKING","HEADBAND","HEADDRESS","CUSHMA","BOUBOU","WRAPPER","G-STRING","DRESS","FUR","BOOT","COAT","HAT","TROUSERS","GLOVE","VEIL","OTHER SHOE","SOCK","SKIRT","SHIRT","HELMET","CLOTHES","LOINCLOTH","MASK","COLLAR","APRON","STRAW SANDAL","OTHER HEADGEAR","BAMBOO HAT","SARONG","PENIS SHEATH","JACKET","SHAWL","SLEEVE","SCARF OR TURBAN","VEST","STRAW RAINCOAT","MOCCASIN","RAINCOAT")

clothingrelations[['SHOE']] = c('MOCCASIN','OTHER SHOE')
clothingrelations[['STOCKING']] = c('SOCK','OTHER STOCKING')
clothingrelations[['HEADGEAR']] = c('CAP', 'HAT', 'HEADBAND OR HEADDRESS','OTHER HEADGEAR')
clothingrelations[['HEADBAND OR HEADDRESS']] = c('HEADBAND', 'HEADDRESS')

clothingframe = relframemaker(clothingrelations,basicclothingconcepts)
clothingframe$Domain = 'Clothing'
domainframe = rbind(domainframe,clothingframe)

#### SPEECH ####
speechrelations = list()
basicspeechconcepts = c("RUMOUR","BLAME","UNTRUTH","SONG","SPEECH","PRAISE","OATH","QUARREL","FOLK SONG","STORY","SCREAM (NOISE)","HOWLING","LIE (FALSEHOOD)","CONVERSATION","CALL (APPEAL)","LAUGHTER","SERMON","QUESTION","JOKE","PROVERB","BATTLE CRY (WAR WHOOP)") #*************

# no relations here, lol.

speechframe = relframemaker(speechrelations,basicspeechconcepts)
speechframe$Domain = 'Speech'
domainframe = rbind(domainframe,speechframe)

#### PHYSGEO ####
physgeorelations = list()
basicphysgeoconcepts = c("OTHER SHORE","MAINLAND","OTHER FIELD","SAVANNA","GULF","HILL","OTHER PRECIPICE","ISLAND","LAGOON","LAKE","LAND","MOUNTAIN","OCEAN","PASTURE","PLAIN","REEF","BAY","RIVER","SPRING (OF WATER)","VALLEY","WATERFALL","OTHER STREAM","WORLD","CREEK (TIDAL)","CLIFF","RIVER BED","SWAMP","CAVE","SEA","COAST","SUMMIT","STRAIT","CAPE","RAPIDS","RIVER MOUTH","RIDGE","RAVINE","WHIRLPOOL","POND","MOUNTAIN FOOT","MOUNTAIN RIDGE","GROUND","CLEARING","ESTUARY","PUDDLE","VOLCANO","WOODS","LANDSCAPE","WATERSIDE","TRENCH (FOSSE)","PIT (POTHOLE)","HEADWATERS","FIELD (UPLAND)","FORD (CROSSING)","OTHER GRASSLAND","CURRENT (STREAM)","ELEVATION","SAND HILL","SHORE OR BANK","OTHER FLOWING BODY OF WATER","SACRED GROVE","RIVERBANK","SHORE OF LAKE","OTHER SHORE OF SEA","EARTH OR LAND","DESERT","MARSH","JUNGLE","BROOK")

physgeorelations[['GRASSLAND']]=c('PASTURE','OTHER GRASSLAND')
physgeorelations[['POND OR LAKE']]=c('POND','LAKE')
physgeorelations[['SHORE']]=c('SHORE OF SEA','SHORE OF LAKE','RIVERBANK','OTHER SHORE')
physgeorelations[['SHORE OF SEA']]=c('BEACH','OTHER SHORE OF SEA')
physgeorelations[['EARTH OR LAND']]=c('LAND')
physgeorelations[['SPRING OR WELL']]=c('SPRING (OF WATER)')
physgeorelations[['PRECIPICE OR SLOPE']]=c('PRECIPICE','SLOPE')
physgeorelations[['PRECIPICE']]=c('CLIFF','OTHER PRECIPICE')
physgeorelations[['SEA OR OCEAN']]=c('SEA','OCEAN')
physgeorelations[['MOUNTAIN OR HILL']]=c('MOUNTAIN','HILL')
physgeorelations[['FIELD']]=c('FIELD (UPLAND)','OTHER FIELD')
physgeorelations[['RIVER OR WATER']]='RIVER'
physgeorelations[['FLOWING BODY OF WATER']]=c('RIVER','STREAM','OTHER FLOWING BODY OF WATER')
physgeorelations[['STREAM']]=c('BROOK','OTHER STREAM')

physgeoframe = relframemaker(physgeorelations,basicphysgeoconcepts)
physgeoframe$Domain = 'physGeo'
domainframe = rbind(domainframe,physgeoframe)

#### PLANT ####
plantrelations = list()
basicplantconcepts = c("OTHER BEAN","OTHER PALM","OTHER BARLEY","OTHER SEEDLING","OTHER VINE","OTHER MILLET","OTHER MELON","RYE","NETTLE","TAMARIND","OAT","LARCH","COCA","SUGAR CANE","COCONUT TREE","SWEET POTATO","BETEL PEPPER VINE","VINE","BANYAN","SORGHUM","MANGROVE","TARO","CHONTA PALM","SHEA NUT TREE","MAHOGANY TREE","ALANG-ALANG GRASS","PANDANUS","MAIZE","MANZANITA","TULE","DOG-ROSE","SPRUCE","POTATO","GRASS","MOSS","OAK","PLANT (VEGETATION)","REED","SHRUB","CABBAGE","WILLOW","POPLAR","BEAN","ASPEN","TREE","VEGETABLES","MANIOC","MILLET","BARLEY","SPINACH","BAOBAB TREE","CHESTNUT TREE","WHEAT","FRAXINUS","AUBERGINE","HAWTHORN","PALM TREE","BUSH","PINE","BANANA TREE","DANDELION","BIRCH","FIR","BAMBOO","CORNFLOWER","RIBES","PEA","RICE PLANT","SOYA","BUCKWHEAT","HEMP","WALNUT","TREE OR WOOD","BEER BANANA","CHINESE PLUM","APRICOT TREE","PEACH TREE","LOTUS","SWEET OLIVE","MUGWORT","PEANUT","RADISH","ONION","MANDARINE","GARLIC","BETEL NUT","BREADFRUIT","SAGO PALM","LIANA","ACAI PALM","TUCUMA PALM","PINEAPPLE","CASHEW","KAPOK TREE","GINGER","HERB","CACTUS","ASPARAGUS","JACKFRUIT","OSMANTHUS FRAGRANS","WATERMELON","LITCHI","SESAME","BLUEBERRY","TURMERIC","CAULIFLOWER","EUCALYPT","SUGAR PALM","LONTAR PALM","MORINGA TREE","SEEDLING","TURNIP","RAMIE","SWALLOW (BIRD)","HIGHLAND BARLEY","CARROT","GEBANG PALM","PLANTAGO","CORIANDER","CYPRESS","MAPLE TREE","PRICKLY ASH","BITTER BUCKWHEAT","FINGER MILLET","FOXTAIL MILLET","JOB'S TEARS","WEED","FLAX","HOPS","ELM","WILD RICE","FLAX OR LINEN","BROCCOLI","CELERY","LETTUCE","RICE SEEDLING","GRAPEVINE","FERN","MIRITI PALM","CALABASH","PEACH PALM","GENIPA") #*************

plantrelations[['MELON']] = c('WATERMELON','OTHER MELON')
plantrelations[['MILLET']] = c('SORGHUM','OTHER MILLET')
plantrelations[['FLAX OR LINEN']] = 'FLAX'
plantrelations[['VINE']] = c('GRAPEVINE','OTHER VINE')
plantrelations[['SEEDLING']] = c('RICE SEEDLING','OTHER SEEDLING')
plantrelations[['TREE OR WOOD']] = 'TREE'
plantrelations[['BARLEY']] = c('HIGHLAND BARLEY','OTHER BARLEY')
plantrelations[['PALM TREE']] = c('SAGO PALM', 'GEBANG PALM','OTHER PALM')
plantrelations[['BEAN OR PEA']] = c('BEAN','PEA')
plantrelations[['BEAN']] = c('BROAD BEAN','OTHER BEAN')

plantframe = relframemaker(plantrelations,basicplantconcepts)
plantframe$Domain = 'Plant'
domainframe = rbind(domainframe,plantframe)

#### ANIMAL ####
animalrelations = list()
basicanimalconcepts = c("OTHER WOLF-LIKE ANIMAL","OTHER MONKEY","APE","OTHER INSECT","OTHER FISH","OTHER CROCODILE","OTHER DEER","OTHER LARGE WILD HERBIVORE","OTHER DUCK","OTHER WATER BUFFALO","OTHER SNAKE","OTHER DOLPHIN","OTHER FROG","FEMALE CAT","OTHER CROW","OTHER LEECH","OTHER CHICK","OTHER LIZARD","OTHER BEAR","OTHER RODENT","GAZELLE","OTTER","PUPPY","SANDFLY","TURTLE","PORPOISE","KANGAROO","WALLABY","ANTEATER","FIREFLY","FISH","FLEA","FLYING FOX","PRAWNS","HE-GOAT","FOWL","HAWK","TOUCAN","OPOSSUM","HEAD LOUSE","BODY LOUSE","TAPIR","AGOUTI","CAPYBARA","CURASSOW","PACA","PIRANHA","HOWLER MONKEY","SPIDER MONKEY","COLLARED PECCARY","WHITE-LIPPED PECCARY","GUAN","CHIGGOE (JIGGER FLEA)","CEBUS MONKEY","GUINEA FOWL","CASSOWARY","CRAYFISH","POSSUM","PELICAN","FROG","WOLF","BARBEL","ANT","GRASSHOPPER","HORSE","ANIMAL","INSECT","LIZARD","SEAL","BEE","RODENT","SNAKE","OWL","HERON","TROUT","LOCUST","LAMB","RACCOON","CORMORANT","SPIDER","TIGER","RATTLESNAKE","PARROT","TERMITE","PARAKEET","MULE","TOAD","TORTOISE","ANACONDA (WATER BOA)","COYOTE","CROW","BUFFALO","STALLION","BIRD","MARE","SALMON","WATER LEECH","SEAGULL","WHALE","WILD ANIMAL","COW","BULL","FRESHWATER EEL","BUG","SHARK","RABBIT","LEOPARD","CATTLE","OX","VULTURE","GOOSE","HARE","BEAVER","BABOON","CAT","WORM","JAGUAR","ELEPHANT","FOX","CHICKEN","SHEEP","PIG","RAM","EWE","BOAR (MALE PIG)","MONKEY","SOW (FEMALE PIG)","DUCK","LION","LOUSE","DOLPHIN","RAT","MOUSE","GOAT","FLY (INSECT)","MOSQUITO","ROOSTER","HEN","WASP","TICK","SCORPION","SNAIL","CHAMELEON","PYTHON","COCKROACH","IGUANA","ALLIGATOR","HUMMINGBIRD","SQUIRREL","MACAW","FOAL","OKRA","ELK","BUTTERFLY","BAT","HEDGEHOG","HYENA","BEAR","CRANE","DOVE","SPARROW","CROCODILE","DONKEY","BUZZARD","CAMEL","DUIKER","REINDEER","ARMADILLO","PUMA","QUAIL","EAGLE","MOLE","STINGRAY","DEER","BEECH","KID","CENTIPEDE","CALF","DOG","MAGPIE","ANIMAL OR MEAT","MUROID (MOUSE OR RAT)","CORAL SNAKE","WILD CAT","BEETLE","CATERPILLAR","LEECH","LOUSE OR NIT","LEPORID (RABBIT OR HARE)","CRESTED MYNA","YELLOW CROAKER","CRAB","SHRIMP","EARTHWORM","SILKWORM","DRAGONFLY","CICADA","CRICKET","GECKO","GUAN OR TURKEY","TURKEY","COATI (COATIMUNDI)","PECCARY","LOBSTER","ROBIN","CAIMAN","ELECTRIC EEL","KINGFISHER","SLOTH","WOODPECKER","WILD DOG","LARGE WILD HERBIVORE","DINGO","WOLF-LIKE ANIMAL","MALE GOAT","FEMALE GOAT","YAK","MALE YAK","FEMALE YAK","MALE DOG","FEMALE DOG","MALE PIG","FEMALE PIG","PANGOLIN","PORCUPINE","GIBBON","CHICK","CHICKEN CHICK","CARABAO","WATER BUFFALO","RAVEN","PIGLET","CASTRATED BOAR","GELDING","CAPON","OCTOPUS","SEAHORSE","RHINOCEROS","LARVA","VIPER","BIRD OF PARADISE","EAGLE OR HAWK","CAPRINE (GOAT OR SHEEP)","CARP (FISH)","CRUSTACEANS (PRAWNS OR SHRIMP)","WILD GOOSE","CRUCIAN CARP","BIVALVIA (MUSSELS OR OYSTERS)","SAGO GRUB","CRAYFISH (FRESHWATER)","CUTTLEFISH","EMU","KANGAROO RAT","MOTH","MUSSELS","OYSTERS","PENGUIN","SWAN","WOMBAT","CUCKOO","PERCH (FISH)","PIKE (FISH)","ECHIDNA","WHITE COCKATOO","WAGTAIL","GRUB","BANDICOOT","BLACK COCKATOO","BLACK DUCK","MONITOR LIZARD","NATIVE CAT","BLUE TONGUED SKINK","STARFISH","BOVINE","PHEASANT","GIRAFFE","LADYBUG","BISON","POLAR BEAR","GORILLA","HIPPOPOTAMUS","CHEETAH","HAMSTER","MEERKAT","MUNTJACS","WILD BOAR","CIVET","RICE EAR BUG","YOUNG ANIMAL","FALCON","GORAL","HORNBILL","HORNET","HORSEFLY","SEROW","FLYING SQUIRREL","TAKIN","MANTIS","APE OR MONKEY","JACKAL","LYNX","YEARLING (HORSE)","HUNTING DOG","PEACOCK","WEASEL","ROUNDWORM","STURGEON","MALLARD","AMERICAN BLACK BEAR","INTESTINAL WORM","ANTELOPE","ZEBRA","SHREW","MONGOOSE","PARTRIDGE","BADGER","GUINEA PIG","MARMOT","BUDGERIGAR","FINCH","FLAMINGO","NIGHTINGALE","STORK","CHIMPANZEE","GRIZZLY BEAR","MARTEN","CATFISH","PET","ALBATROSS","OSTRICH","HINNY","MALE CAT","ROE DEER","TARANTULA","SKUNK","TINAMOU","PACU")

animalrelations[['PIG']] = c('FEMALE PIG','MALE PIG') #*************
animalrelations[['RODENT']] = c('MUROID (MOUSE OR RAT)','OTHER RODENT')
animalrelations[['MUROID (MOUSE OR RAT)']] = c('MOUSE','RAT')
animalrelations[['GUAN OR TURKEY']] = c('GUAN','TURKEY')
animalrelations[['ANIMAL OR MEAT']] = 'ANIMAL'
animalrelations[['BEAR']] = c('AMERICAN BLACK BEAR','OTHER BEAR')
animalrelations[['LIZARD']] = c('BLUE TONGUED SKINK','OTHER LIZARD')
animalrelations[['CHICK']] = c('CHICKEN CHICK','OTHER CHICK')
animalrelations[['LEECH']] = c('WATER LEECH','OTHER LEECH')
animalrelations[['LOUSE OR NIT']] = c('LOUSE','NIT')
animalrelations[['CROW']] = c('RAVEN','OTHER CROW')
animalrelations[['CAT']] = c('MALE CAT','FEMALE CAT')
animalrelations[['CAPRINE (GOAT OR SHEEP)']] = c('GOAT','SHEEP')
animalrelations[['GOAT']] = c('FEMALE GOAT','MALE GOAT')
animalrelations[['FROG']] = c('FROG (SMALL)','OTHER FROG')
animalrelations[['DOLPHIN']] = c('PORPOISE','OTHER DOLPHIN')
animalrelations[['SNAKE']] = c('VIPER','OTHER SNAKE')
animalrelations[['WATER BUFFALO']] = c('CARABAO','OTHER WATER BUFFALO')
animalrelations[['YAK']] = c('MALE YAK','FEMALE YAK')
animalrelations[['DUCK']] = c('MALLARD','OTHER DUCK')
animalrelations[['BIVALVIA (MUSSELS OR OYSTERS)']] = c('MUSSELS','OYSTERS')
animalrelations[['EAGLE OR HAWK']] = c('EAGLE','HAWK')
animalrelations[['LEPORID (RABBIT OR HARE)']] = c('RABBIT','HARE')
animalrelations[['LARGE WILD HERBIVORE']] = c('DEER','KANGAROO','OTHER LARGE WILD HERBIVORE')
animalrelations[['DEER']] = c('ROE DEER','OTHER DEER')
animalrelations[['CROCODILE']] = c('CAIMAN','ALLIGATOR','OTHER CROCODILE')
animalrelations[['FISH']] = c('STURGEON','WILD CROAKER','OTHER FISH')
animalrelations[['INSECT']] = c('VERMIN','RICE EAR BUG', 'BUG', 'OTHER INSECT')
animalrelations[['APE OR MONKEY']] = c('MONKEY','APE')
animalrelations[['MONKEY']] = c('SPIDER MONKEY','HOWLER MONKEY','CEBUS MONKEY','OTHER MONKEY')
animalrelations[['CRUSTACEANS (PRAWNS OR SHRIMP)']] = c('SHRIMP','PRAWNS','LOBSTER','CRAB','CRAYFISH')
animalrelations[['WOLF-LIKE ANIMAL']] = c('WOLF','DOG','DINGO','JACKAL','WILD DOG','OTHER WOLF-LIKE ANIMAL')
animalrelations[['DOG']] = c('MALE DOG','FEMALE DOG')

animalframe = relframemaker(animalrelations,basicanimalconcepts)
animalframe$Domain = 'Animal'
domainframe = rbind(domainframe,animalframe)

#### EMOTION ####
emotionrelations = list()
basicemotionconcepts = c("ANGER","NEED (NOUN)","FEAR (FRIGHT)","ANXIETY","FATIGUE","PITY","GRIEF","SHAME","JEALOUSY","JOY","BLESSED","FEELING (EMOTION)","HAPPINESS","LONGING (WISH)","COURAGE","HOPE (FAITH)","MOURNING","HATE (LOATHING)","LOVE (AFFECTION)","CONTEMPT","DISAPPOINTMENT","EXCITEMENT","INTEREST (FEELING)","SURPRISE (FEELING)","TRIUMPH","AMUSEMENT","AWE","CONCENTRATION","CONFUSION","SADNESS","DEPRESSION","PRIDE","PLEASURE") #*************

emotionrelations[['ENVY']] = c('JEALOUSY','OTHER ENVY') #*************

emotionframe = relframemaker(emotionrelations,basicemotionconcepts)
emotionframe$Domain = 'Emotion'
domainframe = rbind(domainframe,emotionframe)

#### BODY ####
bodyrelations = list()
basicbodyconcepts = c("OTHER HAIR (BODY)","OTHER GENITALIA","OTHER GUTS","GUTS OR HEART","OTHER BLOOD VESSEL","OTHER FINGER","EYELID","EARLOBE","PUBIC HAIR","TATTOO","FOREHEAD","GUMS","SHOULDERBLADE","SHIN","LOIN","HAIR (BODY)","GENITALIA","UPPER ARM","FRONT TOOTH (INCISOR)","TIP (OF TONGUE)","LONG HAIR","UPPER BACK","LOWER ARM","LOWER LEG","UPPER LEG (THIGH)","LIP","KIDNEY","SCAR","CALF OF LEG","BACK OF HEAD","BLOOD VESSEL","FRECKLE","HUMP","PALATE","FOREFINGER","MOUTH","BEARD","LUNG","TRUNK OF TREE","BOIL (OF SKIN)","NIPPLE","TESTICLES","JAW","WRIST","THIGH","RIB","ADAM'S APPLE","ANKLE","WOMB","SPINE","STOMACH","BUTTOCKS","HEEL","ELBOW","JOINT","MUSCLE","HAIR","BRAID","MOLAR TOOTH","WOUND","TENDON","BRUISE","MOUSTACHE","EYEBROW","NOSTRIL","PALM OF HAND","ULCER","TONGUE","NOSE","PENIS","HEART","LIVER","EAR","EYE","BELLY","HEAD","FINGERNAIL","HAND","BACK","LEG","FOOT","FINGER","NECK","GUTS","THROAT","NAPE (OF NECK)","KNEE","OTHER TOOTH","TOE","BONE","BREAST","SHOULDER","CHIN","EYELASH","FACE","BRAIN","BLISTER","CHEST","SKULL","ARM","WAIST","CHEEK","SPLEEN","HIP","BLADDER","BIRTHMARK","THUMB","PIMPLE","VAGINA","VULVA","NAVEL","ARMPIT","FINGERNAIL OR TOENAIL","VEIN","ARTERY","ANUS","GOITER","INTESTINES","COLLARBONE","HAIR OR FOREHEAD","BELLY OR STOMACH","FINGER OR TOE","ARM OR HAND","EAR OR HEAR","LEFT HAND","RIGHT HAND","BODY PART","UVULA","WRINKLE","FEATHER OR FUR OR HAIR","SKIN (HUMAN)","HAIR (HEAD)","TOENAIL","UMBILICAL CORD","SCAB","WART","SOLE (FOOT)","TORSO","SKELETON","ORAL CAVITY","HAIR OR FUR","FINGERTIP","LITTLE FINGER","BREASTBONE","HIP BONE","KNUCKLE","SCROTUM","PANCREAS","CLITORIS","LAP","SIDE (BODY)","MIDDLE FINGER","PLACENTA","LOWER BACK","BURN (INJURY)","PHARYNX","LARYNX","BELLYBUTTON","OVARY","INSTEP","EYEBALL","EARHOLE") #*************

bodyrelations[['BREAST OR MILK']] = 'BREAST'
bodyrelations[['SIDE']] = 'SIDE (BODY)'
bodyrelations[['BARK OR SKIN']]='SKIN'
bodyrelations[['SKIN']]='SKIN (HUMAN)'
bodyrelations[['TOOTH']]=c('MOLAR TOOTH','FRONT TOOTH (INCISOR)', 'OTHER TOOTH')
bodyrelations[['FOOT OR WALK']] = c('FOOT OR LEG','WALK')
bodyrelations[['FOOT OR LEG']] = c('FOOT','LEG')
bodyrelations[['CLAW OR NAIL']] = 'FINGERNAIL OR TOENAIL'
bodyrelations[['FINGERNAIL OR TOENAIL']] = c('FINGERNAIL','TOENAIL')
bodyrelations[['FINGER OR TOE']] = c('FINGER','TOE')
bodyrelations[['FINGER']] = c('LITTLE FINGER','FOREFINGER','MIDDLE FINGER','THUMB','OTHER FINGER')
bodyrelations[['ARM OR HAND']] = c('ARM','HAND')
bodyrelations[['HAND']] = c('LEFT HAND','RIGHT HAND')
bodyrelations[['BELLY OR STOMACH']] = c('BELLY','STOMACH')
bodyrelations[['BLOOD VESSEL']] = c('VEIN','ARTERY','OTHER BLOOD VESSEL')
bodyrelations[['GUTS OR HEART']] = c('GUTS','HEART')
bodyrelations[['GUTS']] = c('INTESTINES','OTHER GUTS')
bodyrelations[['GENITALIA']] = c('VULVA','SCROTUM','PENIS','OTHER GENITALIA')
bodyrelations[['HAIR OR FOREHEAD']]=c('HAIR','FOREHEAD')
bodyrelations[['FEATHER OR FUR OR HAIR']]=c('HAIR OR FUR','HAIR')
bodyrelations[['HAIR OR FUR']]=c('HAIR','HAIR (BODY)')
bodyrelations[['HAIR']]=c('HAIR (HEAD)','HAIR (BODY)')
bodyrelations[['HAIR (BODY)']]=c('PUBIC HAIR','OTHER HAIR (BODY)')
bodyrelations[['EAR OR HEAR']]='EAR'

bodyframe = relframemaker(bodyrelations,basicbodyconcepts)
bodyframe$Domain = 'Body'
domainframe = rbind(domainframe,bodyframe)

write.csv(domainframe,"adjusteddomains.csv",row.names=FALSE,quote=TRUE)