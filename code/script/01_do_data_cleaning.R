#unzip file downloaded from kaggle repository https://www.kaggle.com/osmi/mental-health-in-tech-2016
zipFile <- "~/Documents/Data_Science/projects/medium/data_cleaning/data/raw_data/archive.zip"
outDir <-"~/Documents/Data_Science/projects/medium/data_cleaning/data/raw_data"
unzip(zipFile, exdir = outDir)

file.info("./data/raw_data/mental-heath-in-tech-2016_20161114.csv")$size

#------------------------------------------------------------------------------------------------------
#loading packages
library(tidyverse)
library(naniar)
library(ggplot2)
library(gridExtra)

source("./code/script/functions/funs_do_data_cleaning.R")

#reading in unzipped data file. Unzipped files include word doc with variable/project description
df <- read.csv("./data/raw_data/mental-heath-in-tech-2016_20161114.csv", stringsAsFactors = FALSE)

#first look at data
dim(df)
head(df)
tail(df)

#------------------------------------------------------------------------------------------------------

#checking for duplicated data
df[duplicated(df) | duplicated(df, fromLast=TRUE)] #no duplicates

#renaming variable names
names(df)

df <- df %>% rename(self_employed = Are.you.self.employed.,
                    employees = How.many.employees.does.your.company.or.organization.have.,
                    company_role = Is.your.employer.primarily.a.tech.company.organization.,
                    primary_role = Is.your.primary.role.within.your.company.related.to.tech.IT.,
                    mh_benefit_options = Do.you.know.the.options.for.mental.health.care.available.under.your.employer.provided.coverage.,
                    mh_benefits = Does.your.employer.provide.mental.health.benefits.as.part.of.healthcare.coverage.,
                    mh_discussion = Has.your.employer.ever.formally.discussed.mental.health..for.example..as.part.of.a.wellness.campaign.or.other.official.communication..,
                    mh_resources = Does.your.employer.offer.resources.to.learn.more.about.mental.health.concerns.and.options.for.seeking.help.,
                    mh_anonymity = Is.your.anonymity.protected.if.you.choose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources.provided.by.your.employer.,
                    mh_medical_leave = If.a.mental.health.issue.prompted.you.to.request.a.medical.leave.from.work..asking.for.that.leave.would.be.,
                    mh_discussion_negative = Do.you.think.that.discussing.a.mental.health.disorder.with.your.employer.would.have.negative.consequences.,
                    ph_discussion_negative = Do.you.think.that.discussing.a.physical.health.issue.with.your.employer.would.have.negative.consequences.,
                    mh_discussion_coworkers = Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.,
                    mh_discussion_supervisor = Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s..,
                    mh_ph_serious = Do.you.feel.that.your.employer.takes.mental.health.as.seriously.as.physical.health.,
                    mh_coworker_consequences = Have.you.heard.of.or.observed.negative.consequences.for.co.workers.who.have.been.open.about.mental.health.issues.in.your.workplace.,
                    mh_coverage_private = Do.you.have.medical.coverage..private.insurance.or.state.provided..which.includes.treatment.of..mental.health.issues.,
                    mh_local_online_resources = Do.you.know.local.or.online.resources.to.seek.help.for.a.mental.health.disorder.,
                    mh_dx_reveal_contacts = If.you.have.been.diagnosed.or.treated.for.a.mental.health.disorder..do.you.ever.reveal.this.to.clients.or.business.contacts.,
                    mh_dx_reveal_contacts_impact = If.you.have.revealed.a.mental.health.issue.to.a.client.or.business.contact..do.you.believe.this.has.impacted.you.negatively.,
                    mh_dx_reveal_coworkers = If.you.have.been.diagnosed.or.treated.for.a.mental.health.disorder..do.you.ever.reveal.this.to.coworkers.or.employees.,
                    mh_dx_reveal_coworkers_impact = If.you.have.revealed.a.mental.health.issue.to.a.coworker.or.employee..do.you.believe.this.has.impacted.you.negatively.,
                    mh_productivity = Do.you.believe.your.productivity.is.ever.affected.by.a.mental.health.issue.,
                    mh_productivity_percent = If.yes..what.percentage.of.your.work.time..time.performing.primary.or.secondary.job.functions..is.affected.by.a.mental.health.issue.,
                    previous_employers = Do.you.have.previous.employers.,
                    previous_employers_mhbenefits = Have.your.previous.employers.provided.mental.health.benefits.,
                    previous_employers_mhbenefits_aware = Were.you.aware.of.the.options.for.mental.health.care.provided.by.your.previous.employers.,
                    previous_employers_mhbenefits_discuss = Did.your.previous.employers.ever.formally.discuss.mental.health..as.part.of.a.wellness.campaign.or.other.official.communication..,
                    previous_employers_resources = Did.your.previous.employers.provide.resources.to.learn.more.about.mental.health.issues.and.how.to.seek.help.,
                    previous_employers_anonymtity = Was.your.anonymity.protected.if.you.chose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources.with.previous.employers.,
                    previous_employers_mh_discuss_impact = Do.you.think.that.discussing.a.mental.health.disorder.with.previous.employers.would.have.negative.consequences.,
                    previous_employers_ph_discuss_impact = Do.you.think.that.discussing.a.physical.health.issue.with.previous.employers.would.have.negative.consequences.,
                    previous_coworkers_mh_discuss = Would.you.have.been.willing.to.discuss.a.mental.health.issue.with.your.previous.co.workers.,
                    previous_supervisor_mh_discuss = Would.you.have.been.willing.to.discuss.a.mental.health.issue.with.your.direct.supervisor.s..,
                    previous_employers_mh_ph_serious = Did.you.feel.that.your.previous.employers.took.mental.health.as.seriously.as.physical.health.,
                    previous_employers_mh_coworkers_consequences = Did.you.hear.of.or.observe.negative.consequences.for.co.workers.with.mental.health.issues.in.your.previous.workplaces.,
                    ph_interview = Would.you.be.willing.to.bring.up.a.physical.health.issue.with.a.potential.employer.in.an.interview.,
                    ph_interview_why =Why.or.why.not.,
                    mh_interview = Would.you.bring.up.a.mental.health.issue.with.a.potential.employer.in.an.interview.,
                    mh_interview_why = Why.or.why.not..1,
                    mh_id_career_impact = Do.you.feel.that.being.identified.as.a.person.with.a.mental.health.issue.would.hurt.your.career.,
                    mh_id_coworkers_impact = Do.you.think.that.team.members.co.workers.would.view.you.more.negatively.if.they.knew.you.suffered.from.a.mental.health.issue.,
                    mh_family_friends = How.willing.would.you.be.to.share.with.friends.and.family.that.you.have.a.mental.illness.,
                    mh_unsupportive_workplace = Have.you.observed.or.experienced.an.unsupportive.or.badly.handled.response.to.a.mental.health.issue.in.your.current.or.previous.workplace.,
                    mh_other_impact = Have.your.observations.of.how.another.individual.who.discussed.a.mental.health.disorder.made.you.less.likely.to.reveal.a.mental.health.issue.yourself.in.your.current.workplace.,
                    mi_family = Do.you.have.a.family.history.of.mental.illness.,
                    mh_past = Have.you.had.a.mental.health.disorder.in.the.past.,
                    mh_current = Do.you.currently.have.a.mental.health.disorder.,
                    mh_current_specify = If.yes..what.condition.s..have.you.been.diagnosed.with.,
                    mh_maybe_specify = If.maybe..what.condition.s..do.you.believe.you.have.,
                    mh_dx = Have.you.been.diagnosed.with.a.mental.health.condition.by.a.medical.professional.,
                    mh_dx_specify = If.so..what.condition.s..were.you.diagnosed.with.,
                    mh_treatment = Have.you.ever.sought.treatment.for.a.mental.health.issue.from.a.mental.health.professional.,
                    mh_interfere_treated = If.you.have.a.mental.health.issue..do.you.feel.that.it.interferes.with.your.work.when.being.treated.effectively.,
                    mh_interfere_not_treated = If.you.have.a.mental.health.issue..do.you.feel.that.it.interferes.with.your.work.when.NOT.being.treated.effectively.,
                    age = What.is.your.age.,
                    gender = What.is.your.gender.,
                    resident_country = What.country.do.you.live.in.,
                    resident_state_territory = What.US.state.or.territory.do.you.live.in.,
                    work_state_territory = What.US.state.or.territory.do.you.work.in.,
                    work_country = What.country.do.you.work.in.,
                    work_position = Which.of.the.following.best.describes.your.work.position.,
                    work_remote = Do.you.work.remotely.) 

#------------------------------------------------------------------------------------------------------

#identifying and correcting faulty data types  
str(df)

#to factor data type
cols_factor <- c("mh_benefits", "mh_benefit_options", "mh_discussion", "mh_resources", "mh_anonymity", "mh_medical_leave",
                 "mh_discussion_negative", "ph_discussion_negative", "mh_discussion_coworkers", "mh_discussion_supervisor", 
                 "mh_coworker_consequences", "mh_local_online_resources", "mh_dx_reveal_contacts","mh_dx_reveal_contacts_impact", 
                 "mh_dx_reveal_coworkers", "mh_dx_reveal_coworkers_impact", "mh_productivity", "previous_employers_mhbenefits", 
                 "previous_employers_mhbenefits_aware", "previous_employers_mhbenefits_discuss",
                 "previous_employers_resources", "previous_employers_anonymtity", "previous_employers_mh_discuss_impact",
                 "previous_employers_ph_discuss_impact", "previous_coworkers_mh_discuss", "previous_supervisor_mh_discuss",
                 "previous_employers_mh_ph_serious", "previous_employers_mh_coworkers_consequences", "ph_interview", 
                 "mh_interview", "mh_id_career_impact", "mh_id_coworkers_impact", "mh_family_friends", 
                 "mh_unsupportive_workplace", "mh_other_impact", "mi_family", "mh_past", "mh_current", "mh_dx", "mh_interfere_treated",
                 "mh_interfere_not_treated", "work_remote")

df[, cols_factor] <- lapply(df[, cols_factor], factor)

#------------------------------------------------------------------------------------------------------
#cleaning irregular gender values
df$gender <- CleanGenderVar(df$gender)

unique(df$gender)
table(df$gender) #significantly more men
df$gender <- as.factor(df$gender)

#------------------------------------------------------------------------------------------------------
#cleaning up mental health specification values 
df$mh_dx_specify <- strsplit(df$mh_dx_specify, "\\|")
df$mh_dx_specify <- gsub(" ", "", df$mh_dx_specify)
df <- SeparateDx(df, df$mh_dx_specify)

df$mh_maybe_specify <- strsplit(df$mh_maybe_specify, "\\|")
df$mh_maybe_specify <- gsub(" ", "", df$mh_maybe_specify)
df <- SeparateNotDx(df, df$mh_maybe_specify)

df <- df %>% unnest(c(64:95))
df[64:95]<- df[64:95] * 1
head(df[64:95], 5)

#creating new columns for sum total of dx and believed dx 
df <- df %>% mutate(total_dx = rowSums(df[,c(64:80)]),
                    total_dx_belief = rowSums(df[,c(81:95)]))

head(df[96:97], 5)


#------------------------------------------------------------------------------------------------------
#checking for irregularities 
table(lapply(df[1:2], function(x) ifelse(x>0, 1, 0))) #aligns

sum(is.na(df$primary_role))
sum(df$primary_role == 0, na.rm = T) #15 (248 self-employed people are in tech)
df <- subset(df, primary_role != 0 | is.na(primary_role)) #removing observations from participants not within tech
dim(df) #1418 observations remaining


#------------------------------------------------------------------------------------------------------
#filtering self-employed folks into separate data frame
df_self <- df %>% filter(self_employed == 1)
df <- df %>% filter(self_employed == 0)

#removing cols with duplicate information or only applicable to self-employed or company-employed respondents
df <- df %>% select(-c("mh_local_online_resources", "mh_dx_reveal_contacts", "mh_dx_reveal_contacts_impact",
                       "mh_dx_reveal_coworkers", "mh_dx_reveal_coworkers_impact", "mh_productivity", 
                       "mh_productivity_percent", "mh_maybe_specify", "mh_dx_specify", "self_employed"))
df_self <- df_self %>% select(-c("employees", "mh_benefits", "mh_benefit_options", "mh_discussion", "mh_resources",
                                 "mh_anonymity", "mh_medical_leave", "mh_discussion_negative", "ph_discussion_negative",
                                 "mh_discussion_coworkers", "mh_discussion_supervisor", "mh_ph_serious", 
                                "mh_coworker_consequences", "mh_maybe_specify", "mh_dx_specify", "self_employed"))


#------------------------------------------------------------------------------------------------------
#checking for outliers/strange values in continuous variables (total_dx, total_dx_belief, age)

#company-employed
DataSummary(df$total_dx)
DataSummary(df$total_dx_belief)
DataSummary(df$age)

df <- df[-c(which(df$age > 80 | df$age < 15)), ]

h1 <- PlotVars(df, df$total_dx)
h2<- PlotVars(df, df$total_dx_belief)
h3<- PlotVars(df, df$age)

grid.arrange(h1, h2, h3, nrow = 1)

#self-employed
DataSummary(df_self$total_dx)
DataSummary(df_self$total_dx_belief)
DataSummary(df_self$age)

h4 <- PlotVars(df_self, df_self$total_dx)
h5 <- PlotVars(df_self, df_self$total_dx_belief)
h6 <- PlotVars(df_self, df_self$age)

grid.arrange(h4, h5, h6, nrow = 1)


#------------------------------------------------------------------------------------------------------

#initial look at missing values
sum(is.na(df)) #2009
sum(is.na(df_self)) #574

#cols with greater than 30% missing data
apply(df, 2, function(col)sum(is.na(col))/length(col)) #primary_role 78% missing, mh_coverage_private 100% missing, ph_interview_why .8% missing
which(is.na(df$ph_interview_why)) #914
df <- df[-914, -c(3, 16)]
sum(is.na(df)) #no NA's left

apply(df_self, 2, function(col)sum(is.na(col))/length(col)) #company_role & primary_role have 100% values missing
df_self <- df_self[, -c(1, 2)]
sum(is.na(df_self)) #no NA's left

#writing clean dataframes to the working directory
write_csv(df, "./data/processed_data/mental-health-in-tech-2016-clean.csv")
write_csv(df_self, "./data/processed_data/mental-health-in-tech-2016-self-employed-clean.csv")