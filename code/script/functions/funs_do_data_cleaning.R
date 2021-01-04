#importing libraries
library(tidyverse)
library(naniar)
library(ggplot2)
library(gridExtra)

#cleaning gender variable
CleanGenderVar <- function(column){
    column <- gsub("(?i)Trans*|*(?i)Queer|*(?i)binary|(?i)Genderfluid|(?i)Fluid", "2", column)
    column <- gsub("(?i)F|(?i)Female", "1", column)
    column <- gsub("(?i)Woman", "1", column)
    column <- gsub("(?i)Man|(?i)mail|(?i)dude", "0", column)
    column <- gsub("(?i)M|(?i)Male", "0", column)
    
    column <- ifelse(grepl("2", column), "Other", 
                    ifelse(grepl("1", column), "Female", 
                           ifelse(grepl("0", column), "Male", "Other")))
    return(column)
}

#cleaning mental health specification values
SeparateDx <- function(dataframe, column){
    dataframe <- dataframe %>% mutate(anxiety_dx = grepl("Anxiety*", column),
                        mood_dx = grepl("Mood*", column),
                        substance_dx = grepl("Substance*", column),
                        stress_dx = grepl("Stress*", column),
                        ocd_dx = grepl("Obsessive*", column),
                        personality_dx = grepl("Personality*", column),
                        adhd_dx = grepl("Attention*", column),
                        ptsd_dx = grepl("Post*", column),
                        addictive_dx = grepl("Addictive*", column),
                        ed_dx = grepl("Eating*", column),
                        sad_dx = grepl("Seasonal*", column),
                        suicidal_dx = grepl("Suicidal*", column),
                        dissociative_dx = grepl("Dissociative*", column),
                        gender_dx = grepl("Gender*", column),
                        psychotic_dx = grepl("Psychotic*", column),
                        schizotypal_dx = grepl("Schizotypal*", column))
    
    return(dataframe)
}

SeparateNotDx <- function(dataframe, column){
    dataframe <- dataframe %>% mutate(anxiety_belief = grepl("Anxiety*", column),
                                      mood_belief = grepl("Mood*", column),
                                      substance_belief = grepl("Substance*", column),
                                      stress_belief = grepl("Stress*", column),
                                      ocd_belief = grepl("Obsessive*", column),
                                      personality_belief = grepl("Personality*", column),
                                      adhd_belief = grepl("Attention*", column),
                                      ptsd_belief = grepl("Post*", column),
                                      addictive_belief = grepl("Addictive*", column),
                                      ed_belief = grepl("Eating*", column),
                                      sad_belief = grepl("Seasonal*", column),
                                      suicidal_belief = grepl("Suicidal*", column),
                                      dissociative_belief = grepl("Dissociative*", column),
                                      gender_belief = grepl("Gender*", column),
                                      psychotic_belief = grepl("Psychotic*", column),
                                      schizotypal_belief = grepl("Schizotypal*", column))
    
    return(dataframe)
}

#Continuous data summaries
DataSummary <- function(column){
    print("summary of variable data:")
    print(summary(column))
    print("categorical sum of data:")
    print(table(column))
    print("Number of data points above 99th quantile:")
    print(quantile(column, .99))
    return(hist(column))
}

#plotting continuous variables
PlotVars <- function(df, column){
   return(ggplot(df, aes(x=column)) + 
              geom_histogram(aes(y=..density..), binwidth = 1, colour="blue", fill="lightblue")+
              geom_vline(aes(xintercept=mean(column)),color="black", linetype="dashed", size=.5))
}