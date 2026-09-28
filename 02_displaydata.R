library(stm)
library(tm)
library(SnowballC)
library(Rtsne)
library(rsvd) 
library(geometry)
library(scales)
library(dplyr)
library(nnet)
library(lavaan)
library(ggplot2)
library(ggalluvial)
library(semPlot)
library(ggnewscale)

setwd("/Users/jannawilloughby/Google Drive/My Drive/Willoughby lab/projects - archive/hunting and trust/hunting-trust/")
load(file="output/trust_topic_models.RData")

####analysis 1 - TRUST2 and TRUST3####
#1.1 trust cats only
theta_23 = output.stm_23$theta
meta_23  = output_23$meta
concord_23 = data.frame(level = meta_23$level, topic_profile_distance = NA)

for(g in levels(meta_23$level)){
  group_index = which(meta_23$level == g)
  group_mean = colMeans(theta_23[group_index, , drop = FALSE])
  concord_23$topic_profile_distance[group_index] =
    apply(theta_23[group_index, , drop = FALSE], 1, function(x){
      sqrt(sum((x - group_mean)^2))
    })
}
concord_23$concordance_score = 1 - scales::rescale(concord_23$topic_profile_distance)
concord_23$dominant_topic = apply(theta_23, 1, which.max)
concord_23$dominant_topic_prop = apply(theta_23, 1, max)

#Which topic best represents response divided by agree/disagree?
table(concord_23$level, concord_23$dominant_topic) #count
prop.table(table(concord_23$level, concord_23$dominant_topic), margin = 1) #proportion

#How strongly does this response belong to a particular discourse thread?
hist(apply(output.stm_23$theta, 1, max))
abline(v=mean(apply(output.stm_23$theta, 1, max)), col="firebrick3", lwd=2)

#Which discourse thread does this response belong to?
table(apply(output.stm_23$theta, 1, which.max))

#top topic, and strength of that repsonse in that topic
response_strength = apply(output.stm_23$theta, 1, max)
response_topic = apply(output.stm_23$theta, 1, which.max)
topic_membership = data.frame(topic = apply(output.stm_23$theta, 1, which.max), strength = apply(output.stm_23$theta, 1, max))


#1.2. with residency
##trust cats + residency
theta_23r = output.stm_23r$theta
meta_23r  = output_23r$meta

concord_23r = data.frame(level = meta_23r$group, topic_profile_distance = NA)

for(g in levels(meta_23r$group)){
  group_index = which(meta_23r$group == g)
  group_mean = colMeans(theta_23r[group_index, , drop = FALSE])
  
  concord_23r$topic_profile_distance[group_index] =
    apply(theta_23r[group_index, , drop = FALSE], 1, function(x){
      sqrt(sum((x - group_mean)^2))
    })
}

concord_23r$concordance_score = 1 - scales::rescale(concord_23r$topic_profile_distance)
concord_23r$dominant_topic = apply(theta_23r, 1, which.max)
concord_23r$dominant_topic_prop = apply(theta_23r, 1, max)

#Which topic best represents response divided by agree/disagree/residency?
table(concord_23r$level, concord_23r$dominant_topic) # count
prop.table(table(concord_23r$level, concord_23r$dominant_topic), margin = 1) #proportion

#How strongly do responses belong to a particular discourse thread?
hist(apply(theta_23r, 1, max))
abline(v = mean(apply(theta_23r, 1, max)), col = "firebrick3", lwd = 2)

#Which discourse threads do these response belong to?
table(apply(theta_23r, 1, which.max))

#top topic, and strength of that response in that topic
response_strength_23r = apply(theta_23r, 1, max)
response_topic_23r = apply(theta_23r, 1, which.max)
topic_membership_23r = data.frame(topic = response_topic_23r,strength = response_strength_23r,group = meta_23r$group)
head(topic_membership_23r)
write.csv(topic_membership_23r, "topic_membership_23r.csv")


####analysis 2 - TRUST4 and TRUST5####
#2.1 trust cats only
theta_45 = output.stm_45$theta
meta_45  = output_45$meta
concord_45 = data.frame(level = meta_45$level, topic_profile_distance = NA)

for(g in levels(meta_45$level)){
  group_index = which(meta_45$level == g)
  group_mean = colMeans(theta_45[group_index, , drop = FALSE])
  concord_45$topic_profile_distance[group_index] =
    apply(theta_45[group_index, , drop = FALSE], 1, function(x){
      sqrt(sum((x - group_mean)^2))
    })
}
concord_45$concordance_score = 1 - scales::rescale(concord_45$topic_profile_distance)
concord_45$dominant_topic = apply(theta_45, 1, which.max)
concord_45$dominant_topic_prop = apply(theta_45, 1, max)

#Which topic best represents response divided by agree/disagree?
table(concord_45$level, concord_45$dominant_topic) #count
prop.table(table(concord_45$level, concord_45$dominant_topic), margin = 1) #proportion

#How strongly does this response belong to a particular discourse thread?
hist(apply(output.stm_45$theta, 1, max))
abline(v=mean(apply(output.stm_45$theta, 1, max)), col="firebrick3", lwd=2)

#Which discourse thread does this response belong to?
table(apply(output.stm_45$theta, 1, which.max))

#top topic, and strength of that repsonse in that topic
response_strength = apply(output.stm_45$theta, 1, max)
response_topic = apply(output.stm_45$theta, 1, which.max)
topic_membership = data.frame(topic = apply(output.stm_45$theta, 1, which.max), strength = apply(output.stm_45$theta, 1, max))


#2.2. with residency
##trust cats + residency
theta_45r = output.stm_45r$theta
meta_45r  = output_45r$meta

concord_45r = data.frame(level = meta_45r$group, topic_profile_distance = NA)

for(g in levels(meta_45r$group)){
  group_index = which(meta_45r$group == g)
  group_mean = colMeans(theta_45r[group_index, , drop = FALSE])
  
  concord_45r$topic_profile_distance[group_index] =
    apply(theta_45r[group_index, , drop = FALSE], 1, function(x){
      sqrt(sum((x - group_mean)^2))
    })
}

concord_45r$concordance_score = 1 - scales::rescale(concord_45r$topic_profile_distance)
concord_45r$dominant_topic = apply(theta_45r, 1, which.max)
concord_45r$dominant_topic_prop = apply(theta_45r, 1, max)

#Which topic best represents response divided by agree/disagree/residency?
table(concord_45r$level, concord_45r$dominant_topic) # count
prop.table(table(concord_45r$level, concord_45r$dominant_topic), margin = 1) #proportion

#How strongly do responses belong to a particular discourse thread?
hist(apply(theta_45r, 1, max))
abline(v = mean(apply(theta_45r, 1, max)), col = "firebrick3", lwd = 2)

#Which discourse threads do these response belong to?
table(apply(theta_45r, 1, which.max))

#top topic, and strength of that response in that topic
response_strength_45r = apply(theta_45r, 1, max)
response_topic_45r = apply(theta_45r, 1, which.max)
topic_membership_45r = data.frame(topic = response_topic_45r,strength = response_strength_45r,group = meta_45r$group)
head(topic_membership_45r)
write.csv(topic_membership_45r, "topic_membership_45r.csv")

####analysis 3 - TRUST6 and TRUST7####
#3.1 trust cats only
theta_67 = output.stm_67$theta
meta_67  = output_67$meta
concord_67 = data.frame(level = meta_67$level, topic_profile_distance = NA)

for(g in levels(meta_67$level)){
  group_index = which(meta_67$level == g)
  group_mean = colMeans(theta_67[group_index, , drop = FALSE])
  concord_67$topic_profile_distance[group_index] =
    apply(theta_67[group_index, , drop = FALSE], 1, function(x){
      sqrt(sum((x - group_mean)^2))
    })
}
concord_67$concordance_score = 1 - scales::rescale(concord_67$topic_profile_distance)
concord_67$dominant_topic = apply(theta_67, 1, which.max)
concord_67$dominant_topic_prop = apply(theta_67, 1, max)

#Which topic best represents response divided by agree/disagree?
table(concord_67$level, concord_67$dominant_topic) #count
prop.table(table(concord_67$level, concord_67$dominant_topic), margin = 1) #proportion

#How strongly does this response belong to a particular discourse thread?
hist(apply(output.stm_67$theta, 1, max))
abline(v=mean(apply(output.stm_67$theta, 1, max)), col="firebrick3", lwd=2)

#Which discourse thread does this response belong to?
table(apply(output.stm_67$theta, 1, which.max))

#top topic, and strength of that repsonse in that topic
response_strength = apply(output.stm_67$theta, 1, max)
response_topic = apply(output.stm_67$theta, 1, which.max)
topic_membership = data.frame(topic = apply(output.stm_67$theta, 1, which.max), strength = apply(output.stm_67$theta, 1, max))


#3.2. with residency
##trust cats + residency
theta_67r = output.stm_67r$theta
meta_67r  = output_67r$meta

concord_67r = data.frame(level = meta_67r$group, topic_profile_distance = NA)

for(g in levels(meta_67r$group)){
  group_index = which(meta_67r$group == g)
  group_mean = colMeans(theta_67r[group_index, , drop = FALSE])
  
  concord_67r$topic_profile_distance[group_index] =
    apply(theta_67r[group_index, , drop = FALSE], 1, function(x){
      sqrt(sum((x - group_mean)^2))
    })
}

concord_67r$concordance_score = 1 - scales::rescale(concord_67r$topic_profile_distance)
concord_67r$dominant_topic = apply(theta_67r, 1, which.max)
concord_67r$dominant_topic_prop = apply(theta_67r, 1, max)

#Which topic best represents response divided by agree/disagree/residency?
table(concord_67r$level, concord_67r$dominant_topic) # count
prop.table(table(concord_67r$level, concord_67r$dominant_topic), margin = 1) #proportion

#How strongly do responses belong to a particular discourse thread?
hist(apply(theta_67r, 1, max))
abline(v = mean(apply(theta_67r, 1, max)), col = "firebrick3", lwd = 2)

#Which discourse threads do these response belong to?
table(apply(theta_67r, 1, which.max))

#top topic, and strength of that response in that topic
response_strength_67r = apply(theta_67r, 1, max)
response_topic_67r = apply(theta_67r, 1, which.max)
topic_membership_67r = data.frame(topic = response_topic_67r,strength = response_strength_67r,group = meta_67r$group)
head(topic_membership_67r)
write.csv(topic_membership_67r, "topic_membership_67r.csv")


####across topics r####
# add respondent_ids
data$respondent_id = 1:nrow(data)

#23
redata_23r_id = data.frame(respondent_id = data$respondent_id, docs  = data$TRUST3_text,level = data$TRUST2_cat,resident_binary = data$Resident_binary)
redata_23r_id$resident = NA
redata_23r_id$resident[redata_23r_id$resident_binary == 1] = "Resident"
redata_23r_id$resident[redata_23r_id$resident_binary == 0] = "Non-resident"
redata_23r_id$docs[redata_23r_id$docs == ""] = NA
redata_23r_id$level[redata_23r_id$level == ""] = NA
redata_23r_id$resident[redata_23r_id$resident == ""] = NA
redata_23r_id = redata_23r_id[complete.cases(redata_23r_id[, c("docs", "level", "resident")]),]
redata_23r_id$level = as.factor(redata_23r_id$level)
redata_23r_id$resident = as.factor(redata_23r_id$resident)
redata_23r_id$group = paste(redata_23r_id$level, redata_23r_id$resident)
redata_23r_id$group = factor(redata_23r_id$group,  levels = c("Agree Resident", "Agree Non-resident", "Disagree Resident", "Disagree Non-resident" ))

metadata_23r_id = data.frame(respondent_id = redata_23r_id$respondent_id, level = redata_23r_id$level, resident = redata_23r_id$resident, group = redata_23r_id$group)
text_23r_id = textProcessor(documents = redata_23r_id$docs, metadata = metadata_23r_id, removestopwords = TRUE, customstopwords = stopwords)
output_23r_id = prepDocuments(text_23r_id$documents, text_23r_id$vocab, text_23r_id$meta, lower.thresh = lowerthreshold)

topic_membership_23r_id = topic_membership_23r
topic_membership_23r_id$respondent_id = output_23r_id$meta$respondent_id
topic_membership_23r_id = topic_membership_23r_id[, c("respondent_id", "topic", "strength", "group")]

#45
redata_45r_id = data.frame(respondent_id = data$respondent_id, docs  = data$TRUST5_text,level = data$TRUST4_cat,resident_binary = data$Resident_binary)
redata_45r_id$resident = NA
redata_45r_id$resident[redata_45r_id$resident_binary == 1] = "Resident"
redata_45r_id$resident[redata_45r_id$resident_binary == 0] = "Non-resident"
redata_45r_id$docs[redata_45r_id$docs == ""] = NA
redata_45r_id$level[redata_45r_id$level == ""] = NA
redata_45r_id$resident[redata_45r_id$resident == ""] = NA
redata_45r_id = redata_45r_id[complete.cases(redata_45r_id[, c("docs", "level", "resident")]),]
redata_45r_id$level = as.factor(redata_45r_id$level)
redata_45r_id$resident = as.factor(redata_45r_id$resident)
redata_45r_id$group = paste(redata_45r_id$level, redata_45r_id$resident)
redata_45r_id$group = factor(redata_45r_id$group,  levels = c("Agree Resident", "Agree Non-resident", "Disagree Resident", "Disagree Non-resident" ))

metadata_45r_id = data.frame(respondent_id = redata_45r_id$respondent_id, level = redata_45r_id$level, resident = redata_45r_id$resident, group = redata_45r_id$group)
text_45r_id = textProcessor(documents = redata_45r_id$docs, metadata = metadata_45r_id, removestopwords = TRUE, customstopwords = stopwords)
output_45r_id = prepDocuments(text_45r_id$documents, text_45r_id$vocab, text_45r_id$meta, lower.thresh = lowerthreshold)

topic_membership_45r_id = topic_membership_45r
topic_membership_45r_id$respondent_id = output_45r_id$meta$respondent_id
topic_membership_45r_id = topic_membership_45r_id[, c("respondent_id", "topic", "strength", "group")]

#67
redata_67r_id = data.frame(respondent_id = data$respondent_id, docs  = data$TRUST7_text,level = data$TRUST6_cat,resident_binary = data$Resident_binary)
redata_67r_id$resident = NA
redata_67r_id$resident[redata_67r_id$resident_binary == 1] = "Resident"
redata_67r_id$resident[redata_67r_id$resident_binary == 0] = "Non-resident"
redata_67r_id$docs[redata_67r_id$docs == ""] = NA
redata_67r_id$level[redata_67r_id$level == ""] = NA
redata_67r_id$resident[redata_67r_id$resident == ""] = NA
redata_67r_id = redata_67r_id[complete.cases(redata_67r_id[, c("docs", "level", "resident")]),]
redata_67r_id$level = as.factor(redata_67r_id$level)
redata_67r_id$resident = as.factor(redata_67r_id$resident)
redata_67r_id$group = paste(redata_67r_id$level, redata_67r_id$resident)
redata_67r_id$group = factor(redata_67r_id$group,  levels = c("Agree Resident", "Agree Non-resident", "Disagree Resident", "Disagree Non-resident" ))

metadata_67r_id = data.frame(respondent_id = redata_67r_id$respondent_id, level = redata_67r_id$level, resident = redata_67r_id$resident, group = redata_67r_id$group)
text_67r_id = textProcessor(documents = redata_67r_id$docs, metadata = metadata_67r_id, removestopwords = TRUE, customstopwords = stopwords)
output_67r_id = prepDocuments(text_67r_id$documents, text_67r_id$vocab, text_67r_id$meta, lower.thresh = lowerthreshold)

topic_membership_67r_id = topic_membership_67r
topic_membership_67r_id$respondent_id = output_67r_id$meta$respondent_id
topic_membership_67r_id = topic_membership_67r_id[, c("respondent_id", "topic", "strength", "group")]

# merge all three models by respondent_id
trust_cross_stm = topic_membership_23r_id %>%
  rename(
    topic_23 = topic,
    strength_23 = strength,
    group_23 = group
  ) %>%
  full_join(
    topic_membership_45r_id %>%
      rename(
        topic_45 = topic,
        strength_45 = strength,
        group_45 = group
      ),
    by = "respondent_id"
  ) %>%
  full_join(
    topic_membership_67r_id %>%
      rename(
        topic_67 = topic,
        strength_67 = strength,
        group_67 = group
      ),
    by = "respondent_id"
  )

head(trust_cross_stm)
dim(trust_cross_stm)
nrow(trust_cross_stm[complete.cases(trust_cross_stm), ])
#allresponses = trust_cross_stm[complete.cases(trust_cross_stm), ] #this takes only rows where respondents left text in all three questions
allresponses = trust_cross_stm  #this is all rows, so when we look at the transition tables we shoudl use only proportions since the denoms change

# add topic interpretations and combined topic codes
topic23_labels = c(
  "1"  = "Political process",
  "2"  = "Game population decline/tag allocations",
  "3"  = "General satisfaction",
  "4"  = "Science-based agency trust",
  "5"  = "Wyoming-specific management",
  "6"  = "Money and tag accessibility",
  "7"  = "Land access & private land",
  "8"  = "Herd control/predators/elk",
  "9"  = "Best practice for wildlife management",
  "10" = "Trust professionals & biologists"
)

allresponses$topic_23_interpretation = topic23_labels[as.character(allresponses$topic_23)]
allresponses$topic_23_combined = as.character(allresponses$topic_23)
allresponses$topic_23_combined[allresponses$topic_23 %in% c(4, 9, 10)] = "4910"
allresponses$topic_23_combined[allresponses$topic_23 %in% c(3, 5)] = "35"

# 45r topic interpretations
topic45_labels = c(
  "1"  = "Science/data-based decisions",
  "2"  = "General trust",
  "3"  = "Wildlife quality",
  "4"  = "Land access & private lands",
  "5"  = "Outfitter/hunter interests",
  "6"  = "Draw system & tags",
  "7"  = "Revenue & political pressure",
  "8"  = "Hunting opportunity quality",
  "9"  = "Resident/non-resident pricing & access",
  "10" = "Season structure & deer numbers"
)

allresponses$topic_45_interpretation = topic45_labels[as.character(allresponses$topic_45)]
allresponses$topic_45_combined = as.character(allresponses$topic_45)
allresponses$topic_45_combined[allresponses$topic_45 %in% c(1, 2)] = "12"
allresponses$topic_45_combined[allresponses$topic_45 %in% c(3, 8)] = "38"

# 67r topic interpretations
topic67_labels = c(
  "1"  = "Science & wildlife rationale",
  "2"  = "Skepticism/doubt",
  "3"  = "General trust",
  "4"  = "Use of feedback & survey responses",
  "5"  = "Resident/non-resident divide & money & politics",
  "6"  = "Personal comment experience",
  "7"  = "Feeling unheard/dismissed",
  "8"  = "Public input process",
  "9"  = "Observed management changes",
  "10" = "General process skepticism"
)

allresponses$topic_67_interpretation = topic67_labels[as.character(allresponses$topic_67)]
allresponses$topic_67_combined = as.character(allresponses$topic_67)
allresponses$topic_67_combined[allresponses$topic_67 %in% c(2, 10)] = "210"
allresponses$topic_67_combined[allresponses$topic_67 %in% c(6, 7, 8)] = "678"

# make combined topic columns factors
allresponses$topic_23_combined = as.factor(allresponses$topic_23_combined)
allresponses$topic_45_combined = as.factor(allresponses$topic_45_combined)
allresponses$topic_67_combined = as.factor(allresponses$topic_67_combined)

# Save
write.csv(allresponses,"trust_cross_stm_by_respondent_with_topic_labels.csv", row.names = FALSE)


#### respondent level trust consistency analysis ####

# create A/D trust variables
allresponses$trust23 = ifelse(grepl("^Agree", allresponses$group_23), "A", "D")
allresponses$trust45 = ifelse(grepl("^Agree", allresponses$group_45), "A", "D")
allresponses$trust67 = ifelse(grepl("^Agree", allresponses$group_67), "A", "D")
allresponses$trust_profile = paste(allresponses$trust23, allresponses$trust45, allresponses$trust67, sep = "")

table(allresponses$trust_profile)
prop.table(table(allresponses$trust_profile))

# consistency categories
allresponses$trust_consistency = "Mixed"
allresponses$trust_consistency[allresponses$trust_profile == "AAA"] = "Consistent trust"
allresponses$trust_consistency[allresponses$trust_profile == "DDD"] = "Consistent distrust"
allresponses$trust_consistency = factor(allresponses$trust_consistency, levels = c("Consistent trust", "Mixed", "Consistent distrust"))
table(allresponses$trust_consistency)
prop.table(table(allresponses$trust_consistency))

# agreement count (0-3)
allresponses$trust_score =
  (allresponses$trust23 == "A") +
  (allresponses$trust45 == "A") +
  (allresponses$trust67 == "A")
table(allresponses$trust_score)
prop.table(table(allresponses$trust_score))

# consistency associated with STM narratives
topic_cols = c("topic_23_combined", "topic_45_combined", "topic_67_combined")
for(t in topic_cols){
  print(t)
  print(table(allresponses$trust_consistency, allresponses[[t]]))
  print(prop.table(table(allresponses$trust_consistency, allresponses[[t]]), margin = 1))
  test = chisq.test(table(allresponses$trust_consistency, allresponses[[t]]))
  print(test)
  print(test$stdres)
}

# trust score associated with STM narratives
for(t in topic_cols){
  print(t)
  print(table(allresponses$trust_score, allresponses[[t]]))
  print(prop.table(table(allresponses$trust_score, allresponses[[t]]), margin = 1))
  test = chisq.test(table(allresponses$trust_score, allresponses[[t]]), simulate.p.value = TRUE, B = 10000)
  print(test)
  print(test$stdres)
}

# compare narrative distributions among consistent trusters and distrusters -- we have 0s so if we want p values will have to fix this
for(t in topic_cols){
  print(t)
  temp = subset(allresponses, trust_consistency != "Mixed")
  print(table(temp$trust_consistency, temp[[t]]))
  print(prop.table(table(temp$trust_consistency, temp[[t]]), margin = 1))
  chisq.test(table(temp$trust_consistency, temp[[t]]), simulate.p.value = TRUE,  B = 10000)
  print(test)
  print(test$stdres)
}

# identify narratives most associated with consistency
# consistent trust versus everyone else
allresponses$consistent_trust = ifelse(allresponses$trust_profile == "AAA", 1, 0)

# consistent distrust versus everyone else
allresponses$consistent_distrust = ifelse(allresponses$trust_profile == "DDD", 1, 0)

for(t in topic_cols){
  print(paste("Consistent trust:", t))
  test = chisq.test(table(allresponses$consistent_trust,allresponses[[t]]))
  print(test)
  print(test$stdres)
}

for(t in topic_cols){
  print(paste("Consistent distrust:", t))
  test = chisq.test(table(allresponses$consistent_distrust,allresponses[[t]]))
  print(test)
  print(test$stdres)
}

# mean topic strength by consistency
aggregate(cbind(strength_23, strength_45, strength_67) ~ trust_consistency,data = allresponses,FUN = mean)

# mean topic strength by trust score
aggregate(cbind(strength_23, strength_45, strength_67) ~ trust_score,data = allresponses,FUN = mean)



#### respondent level trust switching analysis ####
# create A/D trust variables
allresponses$trust23 = ifelse(grepl("^Agree", allresponses$group_23), "A", "D")
allresponses$trust45 = ifelse(grepl("^Agree", allresponses$group_45), "A", "D")
allresponses$trust67 = ifelse(grepl("^Agree", allresponses$group_67), "A", "D")

# overall profile across the three dimensions
allresponses$trust_profile = paste(allresponses$trust23, allresponses$trust45, allresponses$trust67, sep = "")
table(allresponses$trust_profile)
prop.table(table(allresponses$trust_profile))

# switching summary
allresponses$n_switches = (allresponses$trust23 != allresponses$trust45) + (allresponses$trust45 != allresponses$trust67)
allresponses$any_switch = ifelse(allresponses$trust_profile %in% c("AAA", "DDD"), "No switch", "Switch")
allresponses$any_switch = factor(allresponses$any_switch, levels = c("No switch", "Switch"))
table(allresponses$any_switch)
prop.table(table(allresponses$any_switch))
table(allresponses$n_switches)
prop.table(table(allresponses$n_switches))

# named pairwise transitions
pairs = list(
  "23_to_45" = c("trust23", "trust45"),
  "23_to_67" = c("trust23", "trust67"),
  "45_to_67" = c("trust45", "trust67")
)

for(p in names(pairs)){
  from = pairs[[p]][1]
  to = pairs[[p]][2]
  allresponses[[p]] = paste(allresponses[[from]], allresponses[[to]], sep = "_")
  allresponses[[p]] = factor(allresponses[[p]], levels = c("A_A", "A_D", "D_A", "D_D"))
}

# pairwise transition tables and McNemar tests
for(p in names(pairs)){
  from = pairs[[p]][1]
  to = pairs[[p]][2]
  
  print(p)
  print(table(allresponses[[from]], allresponses[[to]]))
  print(prop.table(table(allresponses[[from]], allresponses[[to]]), margin = 1))
  print(mcnemar.test(table(allresponses[[from]], allresponses[[to]])))
}

# topics associated with any switching
topic_cols = c("topic_23_combined", "topic_45_combined", "topic_67_combined")

for(t in topic_cols){
  keep = complete.cases(allresponses[, c("any_switch", t)])
  print(t)
  print(table(allresponses$any_switch[keep], allresponses[[t]][keep]))
  print(prop.table(table(allresponses$any_switch[keep], allresponses[[t]][keep]), margin = 1))
  test = chisq.test(table(allresponses$any_switch[keep], allresponses[[t]][keep]), simulate.p.value = TRUE, B = 10000)
  print(test)
  print(test$stdres)
}

# These ask whether topics from each STM are associated with switching involving that dimension.
switch_tests = list(
  "23_to_45_by_topic23" = c("23_to_45", "topic_23_combined"),
  "23_to_45_by_topic45" = c("23_to_45", "topic_45_combined"),
  "23_to_67_by_topic23" = c("23_to_67", "topic_23_combined"),
  "23_to_67_by_topic67" = c("23_to_67", "topic_67_combined"),
  "45_to_67_by_topic45" = c("45_to_67", "topic_45_combined"),
  "45_to_67_by_topic67" = c("45_to_67", "topic_67_combined")
)

for(s in names(switch_tests)){
  switch_col = switch_tests[[s]][1]
  topic_col = switch_tests[[s]][2]
  keep = complete.cases(allresponses[, c(switch_col, topic_col)])
  print(s)
  print(table(allresponses[[switch_col]][keep], allresponses[[topic_col]][keep]))
  print(prop.table(table(allresponses[[switch_col]][keep], allresponses[[topic_col]][keep]), margin = 1))
  test = chisq.test(table(allresponses[[switch_col]][keep], allresponses[[topic_col]][keep]), simulate.p.value = TRUE, B = 10000)
  print(test)
  print(test$stdres)
}

# switch/no-switch tests for each pair

allresponses$switch_23_to_45 = ifelse(allresponses$trust23 != allresponses$trust45, "Switch", "No switch")
allresponses$switch_23_to_67 = ifelse(allresponses$trust23 != allresponses$trust67, "Switch", "No switch")
allresponses$switch_45_to_67 = ifelse(allresponses$trust45 != allresponses$trust67, "Switch", "No switch")

allresponses$switch_23_to_45 = factor(allresponses$switch_23_to_45, levels = c("No switch", "Switch"))
allresponses$switch_23_to_67 = factor(allresponses$switch_23_to_67, levels = c("No switch", "Switch"))
allresponses$switch_45_to_67 = factor(allresponses$switch_45_to_67, levels = c("No switch", "Switch"))

binary_switch_tests = list(
  "switch_23_to_45_by_topic23" = c("switch_23_to_45", "topic_23_combined"),
  "switch_23_to_45_by_topic45" = c("switch_23_to_45", "topic_45_combined"),
  "switch_23_to_67_by_topic23" = c("switch_23_to_67", "topic_23_combined"),
  "switch_23_to_67_by_topic67" = c("switch_23_to_67", "topic_67_combined"),
  "switch_45_to_67_by_topic45" = c("switch_45_to_67", "topic_45_combined"),
  "switch_45_to_67_by_topic67" = c("switch_45_to_67", "topic_67_combined")
)

for(b in names(binary_switch_tests)){
  switch_col = binary_switch_tests[[b]][1]
  topic_col = binary_switch_tests[[b]][2]
  keep = complete.cases(allresponses[, c(switch_col, topic_col)])
  print(b)
  print(table(allresponses[[switch_col]][keep], allresponses[[topic_col]][keep]))
  print(prop.table(table(allresponses[[switch_col]][keep], allresponses[[topic_col]][keep]), margin = 1))
  test = chisq.test(table(allresponses[[switch_col]][keep], allresponses[[topic_col]][keep]), simulate.p.value = TRUE, B = 10000)
  print(test)
  print(test$stdres)
}


#### model ####
#Does trust in science/management propagate through trust in decision-making and ultimately into trust in participation?
#Is trust in public participation largely mediated through trust in agency decision-making? (45->67)
allresponses$trust23 = ifelse(grepl("^Agree", allresponses$group_23), "A", "D")
allresponses$trust45 = ifelse(grepl("^Agree", allresponses$group_45), "A", "D")
allresponses$trust67 = ifelse(grepl("^Agree", allresponses$group_67), "A", "D")
allresponses$trust23_num = ifelse(allresponses$trust23 == "A", 1, 0)
allresponses$trust45_num = ifelse(allresponses$trust45 == "A", 1, 0)
allresponses$trust67_num = ifelse(allresponses$trust67 == "A", 1, 0)
semdata = allresponses[complete.cases(allresponses[, c( "trust23_num", "trust45_num", "trust67_num")]), ]

sem_model = '
trust45_num ~ a*trust23_num
trust67_num ~ b*trust45_num + c*trust23_num
indirect := a*b
total := c + (a*b)
'
fit = sem(sem_model, data = semdata, ordered = c("trust45_num", "trust67_num"), estimator = "WLSMV")
summary(fit, standardized = TRUE)

####plots ####

# alluvial
allresponses$trust23_plot = ifelse(grepl("^Agree", allresponses$group_23), "Agree", "Disagree")
allresponses$trust45_plot = ifelse(grepl("^Agree", allresponses$group_45), "Agree", "Disagree")
allresponses$trust67_plot = ifelse(grepl("^Agree", allresponses$group_67), "Agree", "Disagree")
allresponses$residency_plot = ifelse(grepl("Non-resident", allresponses$group_23), "Non-resident", "Resident")
allresponses$trust23_residency_plot = paste(allresponses$trust23_plot, allresponses$residency_plot)
allresponses$trust23_residency_plot = factor(allresponses$trust23_residency_plot, levels = c("Agree Resident", "Agree Non-resident", "Disagree Resident", "Disagree Non-resident"))

#check if these are the correct labels???
alluvial_data = as.data.frame(table(
  Biological_management = allresponses$trust23_residency_plot,
  Agency_decision_making = allresponses$trust45_plot,
  Public_participation = allresponses$trust67_plot
))

names(alluvial_data)[4] = "n"
alluvial_data = alluvial_data[alluvial_data$n > 0, ]
alluvial_data$profile = paste(substr(as.character(alluvial_data$Biological_management), 1, 1), substr(alluvial_data$Agency_decision_making, 1, 1), substr(alluvial_data$Public_participation, 1, 1), sep = "")

# Build residency-aware alluvial data while keeping axes as Agree/Disagree only
alluvial_data = as.data.frame(table(
  Biological_management = allresponses$trust23_plot,
  Agency_decision_making = allresponses$trust45_plot,
  Public_participation = allresponses$trust67_plot,
  Residency = ifelse(grepl("Non-resident", allresponses$group_23),
                     "Non-resident", "Resident")
))

names(alluvial_data)[5] = "n"
alluvial_data = alluvial_data[alluvial_data$n > 0, ]

# Trust profile based on A/D pattern
alluvial_data$profile = paste(
  substr(as.character(alluvial_data$Biological_management), 1, 1),
  substr(as.character(alluvial_data$Agency_decision_making), 1, 1),
  substr(as.character(alluvial_data$Public_participation), 1, 1),
  sep = ""
)

# Set order explicitly
profile_levels = c("AAA", "AAD", "ADA", "ADD", "DAA", "DAD", "DDA", "DDD")
alluvial_data$profile = factor(alluvial_data$profile, levels = profile_levels)
alluvial_data$Residency = factor(alluvial_data$Residency,levels = c("Resident", "Non-resident"))
alluvial_data$profile_residency = paste(alluvial_data$profile, alluvial_data$Residency, sep = "_")
profile_residency_levels = as.vector(t(outer(profile_levels, c("Resident", "Non-resident"), paste, sep = "_")))
alluvial_data$profile_residency = factor(alluvial_data$profile_residency, levels = profile_residency_levels)
alluvial_data$draw_order = as.numeric(alluvial_data$profile_residency)
alluvial_data = alluvial_data[order(alluvial_data$draw_order), ]

trust_alluvial = ggplot(
  alluvial_data,
  aes(axis1 = Biological_management,
      axis2 = Agency_decision_making,
      axis3 = Public_participation,
      y = n)
) +
  geom_alluvium(
    aes(fill = profile_residency,
        order = draw_order),
    alpha = 0.85,
    width = 0.12,
    discern = TRUE
  ) +
  geom_stratum(
    width = 0.12,
    fill = "gray88",
    color = "black",
    discern = TRUE
  ) +
  scale_fill_manual(
    values = c(
      "AAA_Resident" = "forestgreen",
      "AAA_Non-resident" = "darkseagreen2",
      "AAD_Resident" = "chartreuse3",
      "AAD_Non-resident" = "darkolivegreen2",
      "ADA_Resident" = "goldenrod1",
      "ADA_Non-resident" = "khaki2",
      "ADD_Resident" = "darkorange2",
      "ADD_Non-resident" = "sandybrown",
      "DAA_Resident" = "firebrick3",
      "DAA_Non-resident" = "indianred2",
      "DAD_Resident" = "darkred",
      "DAD_Non-resident" = "indianred3",
      "DDA_Resident" = "orchid4",
      "DDA_Non-resident" = "plum3",
      "DDD_Resident" = "darkorchid4",
      "DDD_Non-resident" = "mediumpurple3"
    ),
    breaks = profile_residency_levels,
    name = "Trust profile and residency"
  ) +
  scale_x_discrete(
    limits = c("Biological management",
               "Agency decision-making",
               "Public participation"),
    expand = c(0.04, 0.04)
  ) +
  labs(x = "", y = "Number of respondents") +
  theme_classic()

trust_alluvial

#legend
library(ggplot2)
library(dplyr)

legend_colors = data.frame(
  profile = c("AAA", "AAD", "ADA", "ADD", "DAA", "DAD", "DDA", "DDD"),
  resident = c(
    "forestgreen",
    "chartreuse3",
    "goldenrod2",
    "darkorange2",
    "firebrick3",
    "darkred",
    "orchid4",
    "darkorchid4"
  ),
  nonresident = c(
    "darkseagreen2",
    "darkolivegreen2",
    "khaki2",
    "sandybrown",
    "indianred2",
    "indianred3",
    "plum3",
    "mediumpurple3"
  ),
  y = rev(1:8)
)

resident_triangles = legend_colors %>%
  rowwise() %>%
  do(data.frame(
    profile = .$profile,
    fill = .$resident,
    x = c(-0.4, -0.4, 0.4),
    y = .$y + c(0.4, -0.4, 0.4)
  ))

nonresident_triangles = legend_colors %>%
  rowwise() %>%
  do(data.frame(
    profile = .$profile,
    fill = .$nonresident,
    x = c(0.4, -0.4, 0.4),
    y = .$y + c(-0.4, -0.4, 0.4)
  ))

legend_plot =
  ggplot() +
  geom_polygon(
    data = resident_triangles,
    aes(x = x, y = y, group = profile, fill = fill),
    color = NA
  ) +
  geom_polygon(
    data = nonresident_triangles,
    aes(x = x, y = y, group = profile, fill = fill),
    color = NA
  ) +
  geom_rect(
    data = legend_colors,
    aes(xmin = -0.4, xmax = 0.4,
        ymin = y - 0.4, ymax = y + 0.4),
    fill = NA,
    color = "black",
    linewidth = 0.3
  ) +
  geom_text(
    data = legend_colors,
    aes(x = 0.7, y = y, label = profile),
    hjust = 0,
    size = 4
  ) +
  scale_fill_identity() +
  coord_equal(clip = "off") +
  theme_void()

legend_plot





# make agree/disagree variables
allresponses$trust23_plot = ifelse(grepl("^Agree", allresponses$group_23), "Agree", "Disagree")
allresponses$trust45_plot = ifelse(grepl("^Agree", allresponses$group_45), "Agree", "Disagree")
allresponses$trust67_plot = ifelse(grepl("^Agree", allresponses$group_67), "Agree", "Disagree")

# make residency variables for each question
allresponses$residency23_plot = ifelse(grepl("Non-resident", allresponses$group_23), "Non-resident", "Resident")
allresponses$residency45_plot = ifelse(grepl("Non-resident", allresponses$group_45), "Non-resident", "Resident")
allresponses$residency67_plot = ifelse(grepl("Non-resident", allresponses$group_67), "Non-resident", "Resident")

# combine trust + residency for each axis
allresponses$trust23_residency_plot = paste(allresponses$trust23_plot, allresponses$residency23_plot)
allresponses$trust45_residency_plot = paste(allresponses$trust45_plot, allresponses$residency45_plot)
allresponses$trust67_residency_plot = paste(allresponses$trust67_plot, allresponses$residency67_plot)

axis_levels = c(
  "Agree Resident",
  "Agree Non-resident",
  "Disagree Resident",
  "Disagree Non-resident"
)

allresponses$trust23_residency_plot = factor(allresponses$trust23_residency_plot, levels = axis_levels)
allresponses$trust45_residency_plot = factor(allresponses$trust45_residency_plot, levels = axis_levels)
allresponses$trust67_residency_plot = factor(allresponses$trust67_residency_plot, levels = axis_levels)

# build alluvial data
alluvial_data_residency = as.data.frame(table(
  Biological_management = allresponses$trust23_residency_plot,
  Agency_decision_making = allresponses$trust45_residency_plot,
  Public_participation = allresponses$trust67_residency_plot
))

names(alluvial_data_residency)[4] = "n"
alluvial_data_residency = alluvial_data_residency[alluvial_data_residency$n > 0, ]

# keep trust profile colors based only on A/D, not residency
alluvial_data_residency$profile = paste(
  substr(as.character(alluvial_data_residency$Biological_management), 1, 1),
  substr(as.character(alluvial_data_residency$Agency_decision_making), 1, 1),
  substr(as.character(alluvial_data_residency$Public_participation), 1, 1),
  sep = ""
)

trust_alluvial_residency = ggplot(
  alluvial_data_residency,
  aes(axis1 = Biological_management,
      axis2 = Agency_decision_making,
      axis3 = Public_participation,
      y = n)
) +
  geom_alluvium(
    aes(fill = profile),
    alpha = 0.8,
    width = 0.12,
    discern = TRUE
  ) +
  geom_stratum(
    width = 0.12,
    fill = "gray88",
    color = "black",
    discern = TRUE
  ) +
  scale_fill_manual(
    values = c(
      "AAA" = "forestgreen",
      "AAD" = "chartreuse3",
      "ADA" = "goldenrod2",
      "ADD" = "darkorange2",
      "DAA" = "firebrick3",
      "DAD" = "darkred",
      "DDA" = "orchid4",
      "DDD" = "darkorchid4"
    ),
    name = "Trust profile"
  ) +
  scale_x_discrete(
    limits = c("Biological management",
               "Agency decision-making",
               "Public participation"),
    expand = c(0.04, 0.04)
  ) +
  labs(x = "", y = "Number of respondents") +
  theme_classic()

trust_alluvial_residency

# trust_alluvial = ggplot(
#   alluvial_data,
#   aes(axis1 = Biological_management, axis2 = Agency_decision_making, axis3 = Public_participation, y = n)
# ) +
#   geom_alluvium(aes(fill = profile), alpha = 0.8, width = 0.18) +
#   geom_stratum(width = 0.25, fill = "gray90", color = "gray30") +
#   geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
#   scale_x_discrete(limits = c("Biological management", "Agency decision-making", "Public participation"), expand = c(0.08, 0.08)) +
#   scale_fill_manual(
#     values = c(
#       "AAA" = "forestgreen",
#       "AAD" = "chartreuse3",
#       "ADA" = "goldenrod2",
#       "ADD" = "darkorange2",
#       "DAA" = "firebrick3",
#       "DAD" = "darkred",
#       "DDA" = "orchid4",
#       "DDD" = "darkorchid4"
#     ),
#     name = "Trust profile"
#   ) +
#   labs(x = "", y = "Number of respondents") +
#   theme_classic()
# trust_alluvial
#ggsave("trust_switching_alluvial_profile_colors.pdf", trust_alluvial, width = 9, height = 5)
#ggsave("trust_switching_alluvial_profile_colors.png", trust_alluvial, width = 9, height = 5, dpi = 300)


allresponses$residency_plot = ifelse(
  grepl("Non-resident", allresponses$group_23),
  "Non-resident",
  "Resident"
)

alluvial_data_split = as.data.frame(table(
  Residency = allresponses$residency_plot,
  Biological_management = allresponses$trust23_plot,
  Agency_decision_making = allresponses$trust45_plot,
  Public_participation = allresponses$trust67_plot
))

names(alluvial_data_split)[5] = "n"
alluvial_data_split = alluvial_data_split[alluvial_data_split$n > 0, ]

alluvial_data_split$profile = paste(
  substr(as.character(alluvial_data_split$Biological_management), 1, 1),
  substr(as.character(alluvial_data_split$Agency_decision_making), 1, 1),
  substr(as.character(alluvial_data_split$Public_participation), 1, 1),
  sep = ""
)

trust_alluvial_split = ggplot(
  alluvial_data_split,
  aes(axis1 = Biological_management,
      axis2 = Agency_decision_making,
      axis3 = Public_participation,
      y = n)
) +
  geom_alluvium(
    aes(fill = profile),
    alpha = 0.8,
    width = 0.12,
    discern = TRUE
  ) +
  geom_stratum(
    width = 0.12,
    fill = "gray88",
    color = "black",
    discern = TRUE
  ) +
  scale_fill_manual(
    values = c(
      "AAA" = "forestgreen",
      "AAD" = "chartreuse3",
      "ADA" = "goldenrod2",
      "ADD" = "darkorange2",
      "DAA" = "firebrick3",
      "DAD" = "darkred",
      "DDA" = "orchid4",
      "DDD" = "darkorchid4"
    ),
    name = "Trust profile"
  ) +
  scale_x_discrete(
    limits = c("Biological management",
               "Agency decision-making",
               "Public participation"),
    expand = c(0.04, 0.04)
  ) +
  facet_wrap(~ Residency, ncol = 1) +
  labs(x = "", y = "Number of respondents") +
  theme_classic()

trust_alluvial_split



library(dplyr)
library(ggplot2)

transition_summary = allresponses %>%
  mutate(
    residency_plot = ifelse(grepl("Non-resident", group_23), "Non-resident", "Resident"),
    first_two = paste(trust23, trust45, sep = ""),
    public_participation = trust67
  ) %>%
  filter(!is.na(first_two), !is.na(public_participation)) %>%
  group_by(residency_plot, first_two, public_participation) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(residency_plot, first_two) %>%
  mutate(percent = 100 * n / sum(n))

ggplot(
  transition_summary,
  aes(x = first_two, y = percent, fill = public_participation)
) +
  geom_col(color = "black", width = 0.75) +
  facet_wrap(~ residency_plot) +
  scale_fill_manual(
    values = c("A" = "forestgreen", "D" = "darkorchid4"),
    labels = c("A" = "Agree public participation",
               "D" = "Disagree public participation"),
    name = ""
  ) +
  labs(
    x = "Trust pattern in first two dimensions",
    y = "Percent within group"
  ) +
  theme_classic()
transition_summary %>%
  arrange(residency_plot, first_two, public_participation)

# SEM figure?

