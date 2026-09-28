library(stm)
library(tm)
library(SnowballC)
library(Rtsne)
library(rsvd) 
library(geometry)
library(scales)

setwd("/Users/jannawilloughby/Google Drive/My Drive/Willoughby lab/projects - archive/hunting and trust/hunting-trust/")
#load(file="output/trust_topic_models.RData")

####clean and set up data####
#removed , . / ' from text response cells

#read in data
fulldata = read.table("input/Binary_with_Qual_Trust_Clean_jrw22May2026.csv", sep=",", header=T, comment.char = "")
data     = fulldata #can add filter here if needed?

#combine trust responses into broader categories
data$TRUST2_cat = rep(NA, nrow(data))
data$TRUST4_cat = rep(NA, nrow(data))
data$TRUST6_cat = rep(NA, nrow(data))

trustcats = data.frame(response = c("Agree", "Strongly agree", "Disagree", "Strongly disagree"),
                       category = c("Agree", "Agree", "Disagree", "Disagree"))
trust_lookup = setNames(trustcats$category, trustcats$response)
data$TRUST2_cat = trust_lookup[data$TRUST2_text]
data$TRUST4_cat = trust_lookup[data$TRUST4_text]
data$TRUST6_cat = trust_lookup[data$TRUST6_text]

#set variables and dataset 
lowerthreshold = 10 #c(10,20) #words that do not appear in at least this many responses will be removed
stopwords = c("get", "like", "one", "re", "said", "say", "says", "answer", "previous", "thing", "make", "take")
catKs = 10 # number of categories

####analysis 1 - TRUST2 and TRUST3####
#1.1 trust cats only
redata = data.frame(docs  = data$TRUST3_text, level = data$TRUST2_cat)
redata$docs[redata$docs == ""] = NA
redata$level[redata$level == ""] = NA
redata = redata[complete.cases(redata), ]
redata$level = as.factor(redata$level)
metadata = data.frame(level = redata$level)

#analyze data to identify topics
# process text
text_23 = textProcessor(documents = redata$docs, metadata = metadata, removestopwords = TRUE, customstopwords=stopwords)
output_23 = prepDocuments(text_23$documents, text_23$vocab, text_23$meta, lower.thresh = lowerthreshold)
output.stm_23 = stm(documents = output_23$documents, vocab = output_23$vocab, prevalence = ~ level, K = catKs, max.em.its = 10000, data = output_23$meta, init.type = "Spectral", seed = 2112)
output.sel_23 = selectModel(output_23$documents, output_23$vocab, prevalence =~ level, K = catKs, max.em.its = 10000,  data = output_23$meta, init.type = "Spectral", runs = 20, seed = 2112)
summary(output.stm_23)

# output
trust_levels = levels(output_23$meta$level)
#dir.create("output/trust_23")
#sink(paste("output/trust_23/output_", lowerthreshold, "_TRUST3_by_TRUST2cat.txt", sep=""), append = FALSE)
sink(paste("output/output_", lowerthreshold, "_TRUST3_by_TRUST2cat.txt", sep=""), append = FALSE)
print(text_23)
print(table(output_23$meta$level))
print(summary(output.stm_23))

coherence_23 = semanticCoherence(model = output.stm_23, documents = output_23$documents)
exclusivity_23 = exclusivity(output.stm_23)
print("coherence")
print(coherence_23)
print(mean(coherence_23))
print("exclusivity")
print(exclusivity_23)
print(mean(exclusivity_23))

for(i in 1:catKs){
  topic = paste(summary(output.stm_23)[[1]][i,], collapse = " ")
  level.eff = estimateEffect(c(i) ~ level - 1, output.stm_23, metadata = output_23$meta, uncertainty = "Global")
  print(paste("Topic", i))
  print(topic)
  print(summary(level.eff))
  
  # regressions
  reg = summary(level.eff)$tables[[1]]
  reg = as.data.frame(reg)
  reg$category = gsub("level", "", rownames(reg))
  reg$lower = reg$Estimate - 1.96 * reg$`Std. Error`
  reg$upper = reg$Estimate + 1.96 * reg$`Std. Error`
  print(reg)
}
sink()

#1.2. with residency
##trust cats + residency
redata = data.frame(docs  = data$TRUST3_text, level = data$TRUST2_cat, resident_binary = data$Resident_binary)
redata$resident = NA
redata$resident[redata$resident_binary == 1] = "Resident"
redata$resident[redata$resident_binary == 0] = "Non-resident"

redata$docs[redata$docs == ""] = NA
redata$level[redata$level == ""] = NA
redata$resident[redata$resident == ""] = NA
redata = redata[complete.cases(redata[,c("docs","level","resident")]), ]

redata$level = as.factor(redata$level)
redata$resident = as.factor(redata$resident)
redata$group = paste(redata$level, redata$resident)
redata$group = factor(redata$group, levels = c("Agree Resident",
                                               "Agree Non-resident",
                                               "Disagree Resident",
                                               "Disagree Non-resident"))
metadata = data.frame(level = redata$level, resident = redata$resident, group = redata$group)

#analyze data to identify topics
text_23r = textProcessor(documents = redata$docs, metadata = metadata, removestopwords = TRUE, customstopwords=stopwords)
output_23r = prepDocuments(text_23r$documents, text_23r$vocab, text_23r$meta, lower.thresh = lowerthreshold)
output.stm_23r = stm(documents = output_23r$documents, vocab = output_23r$vocab, prevalence = ~ group - 1, K = catKs, max.em.its = 10000, data = output_23r$meta, init.type = "Spectral", seed = 2112)
output.sel_23r = selectModel(output_23r$documents, output_23r$vocab, prevalence =~ group - 1, K = catKs, max.em.its = 10000,  data = output_23r$meta, init.type = "Spectral", runs = 20, seed = 2112)
summary(output.stm_23r)

# output
#dir.create("output/trust_23r")
#sink(paste("output/trust_23r/output_", lowerthreshold, "_TRUST3_by_TRUST2cat_resident.txt", sep=""), append = FALSE)
sink(paste("output/output_", lowerthreshold, "_TRUST3_by_TRUST2cat_resident.txt", sep=""), append = FALSE)
print(text_23r)
print(table(output_23r$meta$level))
print(table(output_23r$meta$resident))
print(table(output_23r$meta$group))
print(summary(output.stm_23r))

coherence_23r = semanticCoherence(model = output.stm_23r, documents = output_23r$documents)
exclusivity_23r = exclusivity(output.stm_23r)
print("coherence")
print(coherence_23r)
print(mean(coherence_23r))
print("exclusivity")
print(exclusivity_23r)
print(mean(exclusivity_23r))

for(i in 1:catKs){
  topic = paste(summary(output.stm_23r)[[1]][i,], collapse = " ")
  group.eff = estimateEffect(c(i) ~ group - 1, output.stm_23r, metadata = output_23r$meta, uncertainty = "Global")
  print(paste("Topic", i))
  print(topic)
  print(summary(group.eff))
  
  # extract estimates
  reg = summary(group.eff)$tables[[1]]
  reg = as.data.frame(reg)
  reg$group = gsub("group", "", rownames(reg))
  reg$lower = reg$Estimate - 1.96 * reg$`Std. Error`
  reg$upper = reg$Estimate + 1.96 * reg$`Std. Error`
  print(reg)
}
sink()


####analysis 2 - TRUST4 and TRUST5####
#2.1 trust cats only
redata = data.frame(docs  = data$TRUST5_text, level = data$TRUST4_cat)
redata$docs[redata$docs == ""] = NA
redata$level[redata$level == ""] = NA
redata = redata[complete.cases(redata), ]
redata$level = as.factor(redata$level)
metadata = data.frame(level = redata$level)

#analyze data to identify topics
# process text
text_45 = textProcessor(documents = redata$docs, metadata = metadata, removestopwords = TRUE, customstopwords=stopwords)
output_45 = prepDocuments(text_45$documents, text_45$vocab, text_45$meta, lower.thresh = lowerthreshold)
output.stm_45 = stm(documents = output_45$documents, vocab = output_45$vocab, prevalence = ~ level, K = catKs, max.em.its = 10000, data = output_45$meta, init.type = "Spectral", seed = 2112)
output.sel_45 = selectModel(output_45$documents, output_45$vocab, prevalence =~ level, K = catKs, max.em.its = 10000,  data = output_45$meta, init.type = "Spectral", runs = 20, seed = 2112)
summary(output.stm_45)

# output 
#dir.create("output/trust_45")
#sink(paste("output/trust_45/output_", lowerthreshold, "_TRUST5_by_TRUST4cat.txt", sep=""), append = FALSE)
sink(paste("output/output_", lowerthreshold, "_TRUST5_by_TRUST4cat.txt", sep=""), append = FALSE)
print(text_45)
print(table(output_45$meta$level))
print(summary(output.stm_45))

coherence_45 = semanticCoherence(model = output.stm_45, documents = output_45$documents)
exclusivity_45 = exclusivity(output.stm_45)
print("coherence")
print(coherence_45)
print(mean(coherence_45))
print("exclusivity")
print(exclusivity_45)
print(mean(exclusivity_45))

for(i in 1:catKs){
  topic = paste(summary(output.stm_45)[[1]][i,], collapse = " ")
  level.eff = estimateEffect(c(i) ~ level - 1, output.stm_45, metadata = output_45$meta, uncertainty = "Global")
  print(paste("Topic", i))
  print(topic)
  print(summary(level.eff))
  
  # regressions
  reg = summary(level.eff)$tables[[1]]
  reg = as.data.frame(reg)
  reg$category = gsub("level", "", rownames(reg))
  reg$lower = reg$Estimate - 1.96 * reg$`Std. Error`
  reg$upper = reg$Estimate + 1.96 * reg$`Std. Error`
  print(reg)
}
sink()

#2.2. with residency
##trust cats + residency
redata = data.frame(docs  = data$TRUST5_text, level = data$TRUST4_cat, resident_binary = data$Resident_binary)
redata$resident = NA
redata$resident[redata$resident_binary == 1] = "Resident"
redata$resident[redata$resident_binary == 0] = "Non-resident"

redata$docs[redata$docs == ""] = NA
redata$level[redata$level == ""] = NA
redata$resident[redata$resident == ""] = NA
redata = redata[complete.cases(redata[,c("docs","level","resident")]), ]

redata$level = as.factor(redata$level)
redata$resident = as.factor(redata$resident)
redata$group = paste(redata$level, redata$resident)
redata$group = factor(redata$group, levels = c("Agree Resident",
                                               "Agree Non-resident",
                                               "Disagree Resident",
                                               "Disagree Non-resident"))
metadata = data.frame(level = redata$level, resident = redata$resident, group = redata$group)

#analyze data to identify topics
text_45r = textProcessor(documents = redata$docs, metadata = metadata, removestopwords = TRUE, customstopwords=stopwords)
output_45r = prepDocuments(text_45r$documents, text_45r$vocab, text_45r$meta, lower.thresh = lowerthreshold)
output.stm_45r = stm(documents = output_45r$documents, vocab = output_45r$vocab, prevalence = ~ group - 1, K = catKs, max.em.its = 10000, data = output_45r$meta, init.type = "Spectral", seed = 2112)
output.sel_45r = selectModel(output_45r$documents, output_45r$vocab, prevalence =~ group - 1, K = catKs, max.em.its = 10000,  data = output_45r$meta, init.type = "Spectral", runs = 20, seed = 2112)
summary(output.stm_45r)

# output
#dir.create("output/trust_45r")
#sink(paste("output/trust_45r/output_", lowerthreshold, "_TRUST5_by_TRUST4cat_resident.txt", sep=""), append = FALSE)
sink(paste("output/output_", lowerthreshold, "_TRUST5_by_TRUST4cat_resident.txt", sep=""), append = FALSE)
print(text_45r)
print(table(output_45r$meta$level))
print(table(output_45r$meta$resident))
print(table(output_45r$meta$group))
print(summary(output.stm_45r))

coherence_45r = semanticCoherence(model = output.stm_45r, documents = output_45r$documents)
exclusivity_45r = exclusivity(output.stm_45r)
print("coherence")
print(coherence_45r)
print(mean(coherence_45r))
print("exclusivity")
print(exclusivity_45r)
print(mean(exclusivity_45r))

for(i in 1:catKs){
  topic = paste(summary(output.stm_45r)[[1]][i,], collapse = " ")
  group.eff = estimateEffect(c(i) ~ group - 1, output.stm_45r, metadata = output_45r$meta, uncertainty = "Global")
  print(paste("Topic", i))
  print(topic)
  print(summary(group.eff))
  
  # extract estimates
  reg = summary(group.eff)$tables[[1]]
  reg = as.data.frame(reg)
  reg$group = gsub("group", "", rownames(reg))
  reg$lower = reg$Estimate - 1.96 * reg$`Std. Error`
  reg$upper = reg$Estimate + 1.96 * reg$`Std. Error`
  print(reg)
}
sink()

####analysis 3 - TRUST6 and TRUST7####
#3.1 trust cats only
redata = data.frame(docs  = data$TRUST7_text, level = data$TRUST6_cat)
redata$docs[redata$docs == ""] = NA
redata$level[redata$level == ""] = NA
redata = redata[complete.cases(redata), ]
redata$level = as.factor(redata$level)
metadata = data.frame(level = redata$level)

#analyze data to identify topics
# process text
text_67 = textProcessor(documents = redata$docs, metadata = metadata, removestopwords = TRUE, customstopwords=stopwords)
output_67 = prepDocuments(text_67$documents, text_67$vocab, text_67$meta, lower.thresh = lowerthreshold)
output.stm_67 = stm(documents = output_67$documents, vocab = output_67$vocab, prevalence = ~ level, K = catKs, max.em.its = 10000, data = output_67$meta, init.type = "Spectral", seed = 2112)
output.sel_67 = selectModel(output_67$documents, output_67$vocab, prevalence =~ level, K = catKs, max.em.its = 10000,  data = output_67$meta, init.type = "Spectral", runs = 20, seed = 2112)
summary(output.stm_67)

# output
#dir.create("output/trust_67")
#sink(paste("output/trust_67/output_", lowerthreshold, "_TRUST7_by_TRUST6cat.txt", sep=""), append = FALSE)
sink(paste("output/output_", lowerthreshold, "_TRUST7_by_TRUST6cat.txt", sep=""), append = FALSE)
print(text_67)
print(table(output_67$meta$level))
print(summary(output.stm_67))

coherence_67 = semanticCoherence(model = output.stm_67, documents = output_67$documents)
exclusivity_67 = exclusivity(output.stm_67)
print("coherence")
print(coherence_67)
print(mean(coherence_67))
print("exclusivity")
print(exclusivity_67)
print(mean(exclusivity_67))

for(i in 1:catKs){
  topic = paste(summary(output.stm_67)[[1]][i,], collapse = " ")
  level.eff = estimateEffect(c(i) ~ level - 1, output.stm_67, metadata = output_67$meta, uncertainty = "Global")
  print(paste("Topic", i))
  print(topic)
  print(summary(level.eff))
  
  # regressions
  reg = summary(level.eff)$tables[[1]]
  reg = as.data.frame(reg)
  reg$category = gsub("level", "", rownames(reg))
  reg$lower = reg$Estimate - 1.96 * reg$`Std. Error`
  reg$upper = reg$Estimate + 1.96 * reg$`Std. Error`
  print(reg)
}
sink()

#3.2. with residency
##trust cats + residency
redata = data.frame(docs  = data$TRUST7_text, level = data$TRUST6_cat, resident_binary = data$Resident_binary)
redata$resident = NA
redata$resident[redata$resident_binary == 1] = "Resident"
redata$resident[redata$resident_binary == 0] = "Non-resident"

redata$docs[redata$docs == ""] = NA
redata$level[redata$level == ""] = NA
redata$resident[redata$resident == ""] = NA
redata = redata[complete.cases(redata[,c("docs","level","resident")]), ]

redata$level = as.factor(redata$level)
redata$resident = as.factor(redata$resident)
redata$group = paste(redata$level, redata$resident)
redata$group = factor(redata$group, levels = c("Agree Resident",
                                               "Agree Non-resident",
                                               "Disagree Resident",
                                               "Disagree Non-resident"))
metadata = data.frame(level = redata$level, resident = redata$resident, group = redata$group)

#analyze data to identify topics
text_67r = textProcessor(documents = redata$docs, metadata = metadata, removestopwords = TRUE, customstopwords=stopwords)
output_67r = prepDocuments(text_67r$documents, text_67r$vocab, text_67r$meta, lower.thresh = lowerthreshold)
output.stm_67r = stm(documents = output_67r$documents, vocab = output_67r$vocab, prevalence = ~ group - 1, K = catKs, max.em.its = 10000, data = output_67r$meta, init.type = "Spectral", seed = 2112)
output.sel_67r = selectModel(output_67r$documents, output_67r$vocab, prevalence =~ group - 1, K = catKs, max.em.its = 10000,  data = output_67r$meta, init.type = "Spectral", runs = 20, seed = 2112)
summary(output.stm_67r)

# output
#dir.create("output/trust_67r")
#sink(paste("output/trust_67r/output_", lowerthreshold, "_TRUST7_by_TRUST6cat_resident.txt", sep=""), append = FALSE)
sink(paste("output/output_", lowerthreshold, "_TRUST7_by_TRUST6cat_resident.txt", sep=""), append = FALSE)
print(text_67r)
print(table(output_67r$meta$level))
print(table(output_67r$meta$resident))
print(table(output_67r$meta$group))
print(summary(output.stm_67r))

coherence_67r = semanticCoherence(model = output.stm_67r, documents = output_67r$documents)
exclusivity_67r = exclusivity(output.stm_67r)
print("coherence")
print(coherence_67r)
print(mean(coherence_67r))
print("exclusivity")
print(exclusivity_67r)
print(mean(exclusivity_67r))

for(i in 1:catKs){
  topic = paste(summary(output.stm_67r)[[1]][i,], collapse = " ")
  group.eff = estimateEffect(c(i) ~ group - 1, output.stm_67r, metadata = output_67r$meta, uncertainty = "Global")
  print(paste("Topic", i))
  print(topic)
  print(summary(group.eff))
  
  # extract estimates
  reg = summary(group.eff)$tables[[1]]
  reg = as.data.frame(reg)
  reg$group = gsub("group", "", rownames(reg))
  reg$lower = reg$Estimate - 1.96 * reg$`Std. Error`
  reg$upper = reg$Estimate + 1.96 * reg$`Std. Error`
  print(reg)
}
sink()

####save model outputs####
save.image(file="output/trust_topic_models.RData")




