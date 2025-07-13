library(meta)
library(metafor)
library(dplyr)
library(grid)

setwd("z:/barker/ATS abstract scripts") #location of csv files

#random effects meta-analysis: 
#12h proning; ref group COVID period
#COVID vs pre-COVID periods
meta<- read.csv("meta_analysis_primary_6_25.csv") #csv file with health system level results of outcomes (n, estimate, standard error)
meta_pre<- meta %>% filter(is.na(preCOVID_COVID_compare12)==F) #only run analysis on hospitals with COVID and post-COVID data
res<- rma(yi=meta_pre$preCOVID_COVID_compare12, sei=meta_pre$preCOVID_COVID_compare12se, method = "REML")
summary(res)
meta1<- metagen(meta_pre$preCOVID_COVID_compare12 , meta_pre$preCOVID_COVID_compare12se , common=F, random=T, method.tau = "REML", sm="OR")
forest(meta1, layout = "JAMA", backtransf = TRUE)
grid.text("Favor Pre-Pandemic", x=.525, y=.8, gp=gpar(fontsize=10, fontface="bold"))
grid.text("Favor Pandemic", x=.7, y=.8, gp=gpar(fontsize=10, fontface="bold"))

#COVID vs post-COVID periods
meta_post<- meta %>% filter(is.na(postCOVID_COVID_compare12)==F) #only run analysis on hospitals with COVID and post-COVID data
res<- rma(yi=meta_post$postCOVID_COVID_compare12, sei=meta_post$postCOVID_COVID_compare12se, method = "REML")
summary(res)
meta1<- metagen(TE=meta_post$postCOVID_COVID_compare12 , seTE=meta_post$postCOVID_COVID_compare12se , common=F, random=T, method.tau = "REML", sm="OR")
forest(meta1, layout = "JAMA", backtransf = TRUE)
grid.text("Favor Post-Pandemic", x=.525, y=.68, gp=gpar(fontsize=10, fontface="bold"))
grid.text("Favor Pandemic", x=.75, y=.68, gp=gpar(fontsize=10, fontface="bold"))
 
#12h proning; 4 categories: ref group non-SARS-CoV2 positive, during COVID period
#COVID - vs pre-COVID
meta_pre<- meta %>% filter(is.na(preCOVID_COVID_compare12)==F)
res<- rma(yi=meta_pre$preCOVID_COVID_compare12_4, sei=meta_pre$preCOVID_COVID_compare12se_4, method = "REML")
summary(res)
meta1<- metagen(TE=meta_pre$preCOVID_COVID_compare12_4 , seTE=meta_pre$preCOVID_COVID_compare12se_4 , common=F, random=T, method.tau = "REML", sm="OR")
forest(meta1, layout = "JAMA", backtransf = TRUE)

#COVID + vs COVID - during COVID period
res<- rma(yi=meta$COVID._COVID_compare12_4, sei=meta$COVID._COVID_compare12se_4, method = "REML")
summary(res)
meta1<- metagen(TE=meta$COVID._COVID_compare12_4 , seTE=meta$COVID._COVID_compare12se_4 , common=F, random=T, method.tau = "REML", sm="OR")
forest(meta1, layout = "JAMA", backtransf = TRUE)

#COVID - vs post-COVID
meta_post<- meta %>% filter(is.na(postCOVID_COVID_compare12)==F)
res<- rma(yi=meta_post$postCOVID_COVID_compare12_4, sei=meta_post$postCOVID_COVID_compare12se_4, method = "REML")
summary(res)
meta1<- metagen(TE=meta_post$postCOVID_COVID_compare12_4 , seTE=meta_post$postCOVID_COVID_compare12se_4 , common=F, random=T, method.tau = "REML", sm="OR")
forest(meta1, layout = "JAMA", backtransf = TRUE)

#Sensitivity Analyses: 
#severe ARDS, subset of patients with p/f<100 
meta<- read.csv("meta_analysis_severe.csv")
meta_pre<- meta %>% filter(is.na(preCOVID_COVID_compare12)==F)
res<- rma(yi=meta_pre$preCOVID_COVID_compare12, sei=meta_pre$preCOVID_COVID_compare12se, method = "REML")
summary(res)
meta1<- metagen(meta_pre$preCOVID_COVID_compare12 , meta_pre$preCOVID_COVID_compare12se , common=F, random=T, method.tau = "REML", sm="OR")
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_post<- meta %>% filter(is.na(postCOVID_COVID_compare12)==F)
res<- rma(yi=meta_post$postCOVID_COVID_compare12, sei=meta_post$postCOVID_COVID_compare12se, method = "REML")
summary(res)
meta1<- metagen(TE=meta_post$postCOVID_COVID_compare12 , seTE=meta_post$postCOVID_COVID_compare12se , common=F, random=T, method.tau = "REML", sm="OR")
forest(meta1, layout = "JAMA", backtransf = TRUE)

#72h outcome proning
#Extend eligibility for proning window from 12 -> 72 hours
meta<- read.csv("meta_analysis_72.csv")
meta_pre<- meta %>% filter(is.na(preCOVID_COVID_compare72)==F)
res<- rma(yi=meta_pre$preCOVID_COVID_compare72, sei=meta_pre$preCOVID_COVID_compare72se, method = "REML")
summary(res)
meta1<- metagen(meta_pre$preCOVID_COVID_compare72 , meta_pre$preCOVID_COVID_compare72se , common=F, random=T, method.tau = "REML", sm="OR")
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_post<- meta %>% filter(is.na(postCOVID_COVID_compare72)==F)
res<- rma(yi=meta_post$postCOVID_COVID_compare72, sei=meta_post$postCOVID_COVID_compare72se, method = "REML")
summary(res)
meta1<- metagen(TE=meta_post$postCOVID_COVID_compare72 , seTE=meta_post$postCOVID_COVID_compare72se , common=F, random=T, method.tau = "REML", sm="OR")
forest(meta1, layout = "JAMA", backtransf = TRUE)

#Health systems with all data periods
meta<- read.csv("meta_analysis_primary_6_25.csv")
meta_pre<- meta %>% filter(is.na(preCOVID_COVID_compare12)==F & is.na(postCOVID_COVID_compare12)==F)
res<- rma(yi=meta_pre$preCOVID_COVID_compare12, sei=meta_pre$preCOVID_COVID_compare12se, method = "REML")
summary(res)
meta1<- metagen(meta_pre$preCOVID_COVID_compare12 , meta_pre$preCOVID_COVID_compare12se , common=F, random=T, method.tau = "REML", sm="OR")
forest(meta1, layout = "JAMA", backtransf = TRUE)

meta_post<- meta %>% filter(is.na(postCOVID_COVID_compare12)==F & is.na(preCOVID_COVID_compare12)==F )
res<- rma(yi=meta_post$postCOVID_COVID_compare12, sei=meta_post$postCOVID_COVID_compare12se, method = "REML")
summary(res)
meta1<- metagen(TE=meta_post$postCOVID_COVID_compare12 , seTE=meta_post$postCOVID_COVID_compare12se , common=F, random=T, method.tau = "REML", sm="OR")
forest(meta1, layout = "JAMA", backtransf = TRUE)