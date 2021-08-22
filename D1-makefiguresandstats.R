# Set working directory appropriately
# setwd("")

##### FIGURE 1: Colexification Graph Visualization #####
suppressMessages(
  library(igraph)) # Working with graphs

# We have identified PRECIPICE, REEF, and VALLEY
# as being good examples for explaining our measures
# of ambiguity.

# We can manually draw their colexification graphs;
# this is the same as what the CLICS database would say,
# it's just faster.

minidomain = c("PRECIPICE","VALLEY","REEF")
russianmini = graph_from_literal(1,2,3)
swahilimini = graph_from_literal(1,2,3)
swahilimini = add_edges(swahilimini,c(1,3))
hungarianmini = graph_from_literal(1,2,3)
hungarianmini = add_edges(hungarianmini,c(1,3))
vertex_attr(russianmini)$Gloss <- minidomain
vertex_attr(swahilimini)$Gloss <- minidomain
vertex_attr(hungarianmini)$Gloss <- minidomain

# Hungarian:
#   Precipice: szirt, szikla
#   Reef: szirt, zátony
#   Valley: völgy
# Swahili:
#   Precipice: mwamba
#   Reef: mwamba
#   Valley: bonde
# Russian:
#   Precipice: утёс
#   Reef: риф
#   Valley: долина

# We associate each word with its meanings from these three.
russiangroups = list()
russiangroups[['утёс']] = 1
russiangroups[['долина']] = 2
russiangroups[['риф']] = 3
swahiligroups = list()
swahiligroups[['mwamba']]=c(1,3)
swahiligroups[['bonde']]=2
hungariangroups = list()
hungariangroups[['szirt']]=c(1,3)
hungariangroups[['völgy']]=2
hungariangroups[['zátony']]=3
hungariangroups[['szikla']]=1

# Colors to outline words' meanings in.
colorscale = c("darkblue","red","darkgreen","orange")

# Russian example figure
cairo_pdf("russianexample.pdf",width=3.3,height=3.3,family="DejaVu Sans")#encoding="Cyrillic.enc")
plot.igraph(russianmini,
            margin=0.3,
            vertex.label=vertex_attr(russianmini)$Gloss,
            vertex.label.color="black",
            vertex.label.dist=c(15,13,15),
            vertex.label.degree=c(-pi/3,-3*pi/5,-3*pi/4),
            vertex.color="grey50",
            vertex.size=25,
            edge.color="black",
            edge.width=4,
            mark.groups=russiangroups,
            mark.expand=c(120,120,120),
            mark.shape=1,
            mark.col=rgb(0.5,0.5,0.5,0.1),
            mark.border=colorscale,
            layout=layout_in_circle)
legend(x=-2.6,y=-2, legend = names(russiangroups),
       col=colorscale,
       pch = 15, bty = "n", pt.cex=1.5,cex = 1,
       x.intersp=0.7, text.width=1.2,
       text.col = "black", horiz = TRUE)
dev.off()

# Swahili example figure
cairo_pdf("swahiliexample.pdf",width=3.3,height=3.3,family="DejaVu Sans")
plot.igraph(swahilimini,
            margin=0.3,
            vertex.label=vertex_attr(swahilimini)$Gloss,
            vertex.label.color="black",
            vertex.label.dist=c(15,13,15),
            vertex.label.degree=c(-pi/3,-3*pi/5,-3*pi/4),
            vertex.color="grey50",
            vertex.size=25,
            edge.color="black",
            edge.width=4,
            mark.groups=swahiligroups,
            mark.expand=c(145,120),
            mark.shape=1,
            mark.col=rgb(0.5,0.5,0.5,0.1),
            mark.border=colorscale,
            layout=layout_in_circle)
legend(x=-1.9,y=-2, legend = names(swahiligroups),
       col=colorscale,
       pch = 15, bty = "n", pt.cex=1.5,cex = 1,
       x.intersp=0.7, text.width=1.6,
       text.col = "black", horiz = TRUE)
dev.off

# Hungarian example figure
cairo_pdf("hungarianexample.pdf",width=3.3,height=3.3,family="DejaVu Sans")
plot.igraph(hungarianmini,
            margin=0.3,
            vertex.label=vertex_attr(hungarianmini)$Gloss,
            vertex.label.color="black",
            vertex.label.dist=c(15,13,15),
            vertex.label.degree=c(-pi/3,-3*pi/5,-3*pi/4),
            vertex.color="grey50",
            vertex.size=25,
            edge.color="black",
            edge.width=4,
            mark.groups=hungariangroups,
            mark.expand=c(145,120,120,120),
            mark.shape=1,
            mark.col=rgb(0.5,0.5,0.5,0.1),
            mark.border=colorscale,
            layout=layout_in_circle)
legend(x=-2.9,y=-2, legend = names(hungariangroups),
       col=colorscale,
       pch = 15, bty = "n", pt.cex=1.5,cex = 1,
       x.intersp=0.7, text.width=0.9,
       text.col = "black", horiz = TRUE)
dev.off()


##### FIGURE 2: Linear mixed models, and associated statistics #####
suppressMessages(
  library(ggplot2)) # making pictures :)
suppressMessages(
  library(dplyr)) # gluing/filtering dataframes
suppressMessages(
  library(tidyr)) # gluing/filtering dataframes
suppressMessages(
  library(ggpubr)) # multiple plots together
suppressMessages(
  library(lme4)) # for lmer models
suppressMessages(
  library(sjPlot)) # for plotting lmer models
suppressMessages(
  library(effects)) # for plotting best-fit lines
suppressMessages(
  library(robustlmm)) # robust lmer models

ambigframe=read.csv("A4-domainambiguities.csv",header=TRUE,quote="\"")
frequencyframe=read.csv("B4-domainfrequencies.csv",header=TRUE,quote="\"")
conceptcountframe=read.csv("C2-conceptcountbydomain.csv",header=TRUE,quote="\"")
domainframe = suppressMessages(
  inner_join(
    inner_join(ambigframe, frequencyframe)
    ,conceptcountframe)
)

# Only use domain-language pairs
# with at least 10 concepts.
domainframe %>%
  filter(count>10) ->
  domframefiltered

# The shapes we'll use to encode domains
# in the scatterplots.
shapescale = c(1,8,19,6,3,
               7,10,11,12,4,
               17,14,15,5,9)

# First we do a linear mixed model with random slopes
# for all the data.
fulllmer = lmer(Ambiguity ~ log(Frequency) + (log(Frequency) + 0 | Language), data=domframefiltered)
domframewithfulllm = domframefiltered
domframewithfulllm$fit = predict(fulllmer)
eff_fulllmer <- effects::effect(term= "log(Frequency)", mod= fulllmer)
efffulldf = as.data.frame(eff_fulllmer)
# This is where we get most of the summary stats:
tab_model(fulllmer)
# Here's a plot of just that model:
ggplot() + 
  geom_ribbon(data=efffulldf, aes(x=log(Frequency), ymin=lower, ymax=upper), alpha= 0.3, fill="darkgrey")+
  geom_point(data=domframewithfulllm, aes(x=log(Frequency), y=Ambiguity,color=Language,shape=Language))+
  geom_line(data=domframewithfulllm, aes(x=log(Frequency), y=fit,color=Language),alpha=0.9,size=0.1)+
  scale_shape_manual(values = c(1,8,19,6,3,
                                7,10,11,12,4,
                                17,14,15,5,9)) +
  geom_line(data=efffulldf, aes(x=log(Frequency), y=fit), color="grey40",size=0.1) +
  labs(x="log(Frequency)",y="Ambiguity")

# The same model but with Kinship removed:
domframenokin = filter(domframefiltered,Domain != "Kinship")
nokinlmer = lmer(Ambiguity ~ log(Frequency) + (log(Frequency) + 0 | Language), data=domframenokin)
domframewithnokinlm = domframenokin
domframewithnokinlm$fit = predict(nokinlmer)
eff_nokinlmer <- effects::effect(term= "log(Frequency)", mod= nokinlmer)
effnokindf = as.data.frame(eff_nokinlmer)
# This is where we get most of the summary stats:
tab_model(nokinlmer)
# Here's a plot of just that model:
ggplot() + 
  geom_ribbon(data=effnokindf, aes(x=log(Frequency), ymin=lower, ymax=upper), alpha= 0.3, fill="darkgrey")+
  geom_point(data=domframewithnokinlm, aes(x=log(Frequency), y=Ambiguity,color=Language,shape=Language))+
  geom_line(data=domframewithnokinlm, aes(x=log(Frequency), y=fit,color=Language),alpha=0.9,size=0.1)+
  scale_shape_manual(values = c(1,8,19,6,3,
                                7,10,11,12,4,
                                17,14,15,5,9)) +
  geom_line(data=effnokindf, aes(x=log(Frequency), y=fit), color="grey40",size=0.1) +
  labs(x="log(Frequency)",y="Ambiguity")

# Both plots side-by-side:
domframewithfulllm$Model = "(a) Kinship Included"
domframewithnokinlm$Model = "(b) Kinship Removed"
efffulldf$Model = "(a) Kinship Included"
effnokindf$Model = "(b) Kinship Removed"
domframecombined = rbind(domframewithfulllm,domframewithnokinlm)
effdfcombined = rbind(efffulldf,effnokindf)
# We change legend labels for plotting.
domframecombined$Domain[domframecombined$Domain == "Animal"] <- "Animals"
domframecombined$Domain[domframecombined$Domain == "Plant"] <- "Plants"
domframecombined$Domain[domframecombined$Domain == "physGeo"] <- "Physical\nGeography"
pdf("combinedlmerplot.pdf",width=9.5,height=3.5)
ggplot() + 
  geom_ribbon(data=effdfcombined, aes(x=log(Frequency), ymin=lower, ymax=upper), alpha= 0.3, fill="darkgrey")+
  geom_point(data=domframecombined, aes(x=log(Frequency), y=Ambiguity,color=Language,shape=Domain),size=2)+
  geom_line(data=domframecombined, aes(x=log(Frequency), y=fit,color=Language),alpha=0.9,size=0.5)+
  scale_shape_manual(values = shapescale) +
  geom_line(data=effdfcombined, aes(x=log(Frequency), y=fit), color="grey40",size=0.5,alpha=0.8) +
  labs(x="log(Frequency)",y="Ambiguity") +
  facet_wrap(vars(Model),ncol=2,scales="free") +
  theme(legend.position="right",
        legend.direction="vertical",
        legend.box="horizontal") +
  theme(text=element_text(size=15),
        legend.text=element_text(size=13),
        strip.text=element_text(size=13),
        legend.key.height=unit(0.8,'cm'))
dev.off()

# Finally, the robust linear mixed model
# that shows that Kinship points are outliers.
roblmer = rlmer(Ambiguity ~ log(Frequency) + (log(Frequency) + 0 | Language), data=domframefiltered)
domframewithrob = domframefiltered
domframewithrob$robust = getME(roblmer,"w_e")
domframewithrob %>%
  select(Domain, robust) %>%
  group_by(Domain) %>%
  summarize(mostrobust = max(robust),leastrobust=min(robust))
# We see that the most robust point in Kinship is 0.399,
# and the least robust other point is 0.932.
# A stark difference.