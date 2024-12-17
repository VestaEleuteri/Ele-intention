#Experiment data sorting ----
rm(list=ls()) 

#Load packages
library(ggplot2)
library(here)
library(dplyr)
setwd("/Users/ve/Desktop/PhD/Analysis/Chapter 2 - Experiment")
load("Experiment data sorting.Rdata") #Add Rdata

#Data sorting----
#Change some Signal records names----
setwd("/Users/ve/Desktop/PhD/Analysis/Chapter 2 - Experiment/dataframes")
xdata=read.table("exp_all_records.csv", header=T, sep=",", fill=T, dec=",", stringsAsFactors=T)
xdata <- xdata %>%
  filter(!Sig_Dur_Analysis %in% c("Exclude")) 
levels(xdata$Signal_Record)
table(xdata$Signal_Record, xdata$Subject)

levels(xdata$Experimental_Condition)[levels(xdata$Experimental_Condition)=="Goal Not Met"] <- "Goal not met" #relevel
levels(xdata$Experimental_Condition)[levels(xdata$Experimental_Condition)=="Goal Met"] <- "Goal met" #relevel
levels(xdata$Experimental_Condition)[levels(xdata$Experimental_Condition)=="Goal Partially Met"] <- "Goal partially met" #relevel

levels(xdata$Signal_Record)
levels(xdata$Signal_Record)[levels(xdata$Signal_Record)=="Object-on-Head"] <- "Stick-on-Head" #relevel
levels(xdata$Signal_Record)[levels(xdata$Signal_Record)=="Object-on-Tusk"] <- "Stick-on-Tusk" #relevel
levels(xdata$Signal_Record)[levels(xdata$Signal_Record)=="Object-on-TG"] <- "Stick-on-TG" #relevel
levels(xdata$Signal_Record)[levels(xdata$Signal_Record)=="Present-Object"] <- "Present-Stick" #relevel
levels(xdata$Signal_Record)[levels(xdata$Signal_Record)=="Trunk-Hitting-Self"] <- "Trunk-Hitting-Head" #relevel
levels(xdata$Signal_Record)[levels(xdata$Signal_Record)=="Trunk-Swing-Touch-Self"] <- "Trunk-Swing-Touch-Body" #relevel
table(xdata$Signal_Record, xdata$Gest_Direction)

levels(xdata$Signal_Record)

xdata <- xdata %>% #change name signals directed both at exp and apples to discriminate into different actions
  dplyr::mutate(Signal_Record = as.character(Signal_Record),  #Convert factor 1 to character for manipulation
  Signal_Record = ifelse(Gest_Direction == "Experimenter" & Signal_Record == "Lean-Towards", "Lean-Towards-Ex", Signal_Record), 
  Signal_Record = ifelse(Gest_Direction == "Apple-tray" & Signal_Record == "Lean-Towards", "Lean-Towards-Obj", Signal_Record), 
  Signal_Record = factor(Signal_Record))  # Convert back to factor
levels(xdata$Signal_Record)

xdata <- xdata %>% #change name signals directed both at exp and apples to discriminate into different actions
  dplyr::mutate(Signal_Record = as.character(Signal_Record),  #Convert factor 1 to character for manipulation
  Signal_Record = ifelse(Gest_Direction == "Experimenter" & Signal_Record == "Trunk-Blow", "Trunk-Blow-Ex", Signal_Record),
  Signal_Record = factor(Signal_Record))  # Convert back to factor

xdata <- xdata %>% #change name signals directed both at exp and apples to discriminate into different actions
  dplyr::mutate(Signal_Record = as.character(Signal_Record),  #Convert factor 1 to character for manipulation
  Signal_Record = ifelse(Gest_Direction == "Experimenter" & Signal_Record == "Trunk-Throw-Sand", "Trunk-Throw-Sand-Ex", Signal_Record), 
  Signal_Record = ifelse(Gest_Direction == "Self" & Signal_Record == "Trunk-Throw-Sand", "Trunk-Throw-Sand-Self", Signal_Record), 
  Signal_Record = factor(Signal_Record))  # Convert back to factor
levels(xdata$Signal_Record)

xdata <- xdata %>% #change name signals directed both at exp and apples to discriminate into different actions
  dplyr::mutate(Signal_Record = as.character(Signal_Record),  #Convert factor 1 to character for manipulation
  Signal_Record = ifelse(Gest_Direction == "Experimenter" & Signal_Record == "Trunk-Throw-Stick", "Trunk-Throw-Stick-Ex", Signal_Record), 
  Signal_Record = factor(Signal_Record))  # Convert back to factor

#Add age categories, group, and sex columns per individuals----
#sex
xdata <- xdata %>%
  dplyr::mutate(Sex = case_when(
    Subject == "Doma" ~ "Male",
    Subject == "Laduma" ~ "Male",
    Subject == "Detema" ~ "Male",
    Subject == "Mainos" ~ "Male",
    Subject == "Moka" ~ "Male",
    Subject == "Jock" ~ "Male",
    Subject == "Jambo" ~ "Male",
    Subject == "Pfumo" ~ "Male",
    TRUE ~ "Female"  # Default value if none of the conditions are met
  ))
levels(xdata$Subject)
table(xdata$Subject, xdata$Sex)

#group
xdata <- xdata %>%
  dplyr::mutate(Group = case_when(
    Subject == "Doma" ~ "eleCREW",
    Subject == "Laduma" ~ "eleCREW",
    Subject == "Detema" ~ "eleCREW",
    Subject == "Mainos" ~ "eleCREW",
    Subject == "Moka" ~ "eleCREW",
    Subject == "Tatu" ~ "eleCREW",
    Subject == "Hwange" ~ "eleCREW",
    Subject == "Emely" ~ "WildHorizons1",
    Subject == "Jock" ~ "WildHorizons1",
    Subject == "Jenet" ~ "WildHorizons1",
    Subject == "Naledi" ~ "WildHorizons1",
    Subject == "Pfumo" ~ "WildHorizons1",
    TRUE ~ "WildHorizons2"  # Default value if none of the conditions are met
  ))

table(xdata$Subject, xdata$Group)

#Age (Old>=35, Young<35)
xdata <- xdata %>%
  dplyr::mutate(Age = case_when(
    Subject == "Doma" ~ "Old adult",
    Subject == "Moka" ~ "Old adult",
    Subject == "Tatu" ~ "Old adult",
    Subject == "Jock" ~ "Old adult",
    Subject == "Jambo" ~ "Old adult",
    Subject == "Coco" ~ "Old adult",
    TRUE ~ "Young adult"  # Default value if none of the conditions are met
  ))

table(xdata$Subject, xdata$Age)

write.csv(xdata2, "exp_sig_df.csv") #save signal main df

##Extract summary dataset with N gesture actions, N gesture types, N repetitions prev actions for GLMMs (?maybe remove this repetition as not very interesting)----
##First rename signals not gestures or unclear/unk as "0"
xdata3 <- xdata2 %>%
  dplyr::mutate(Signal_Record = recode(Signal_Record, 
                                "Trunk-Burp" = "0", 
                                "None" = "0", 
                                "Unknown" = "0", 
                                "Croaking" = "0",
                                "Head-Shake" = "0",
                                "Trunk-Fling_Unc" = "0", 
                                "Trunk-Reach_Unc" = "0",
                                "Lean-Back" = "0",
                                "Trunk-Touch-Leg" = "0",
                                "Lean-Towards-Ex" = "0",
                                "Trunk-Touch-Ear" = "0",
                                "Trunk-Touch-TG" = "0",
                                "Trunk-Touch-Face" = "0"
                                )) 
table(xdata3$Signal_Record, xdata3$Delivery_Phase)
levels(xdata3$Signal_Record)
levels(xdata3$Subject)
table(xdata3$Signal_Record, xdata3$Delivery_Phase)

str(xdata3)
summary_gest_data <- xdata3 %>%
  dplyr::group_by(File, Subject, Sex, Age, Group, Experimental_Condition, Delivery_Phase) %>%
  dplyr::summarize(
    Gesture_Record_Count = sum(Signal_Record != "0", na.rm = TRUE),  # Count the total number of Signal_Record entries, excluding "0"
    Gesture_Type_Count = n_distinct(Signal_Record[Signal_Record != "0"]), # Count the total number of Signal_Record levels, excluding "0"
    Repetition_Count = sum(Repetition, na.rm = TRUE), # Count number of Repetitions=TRUE
    .groups = 'drop'  # Ensure ungrouped output
  ) %>%
  dplyr::mutate(
    Gesture_Record_Count = ifelse(Gesture_Record_Count == 0, 0, Gesture_Record_Count),  # Ensure counts are 0 if no Signal_Record present
    Gesture_Type_Count = ifelse(Gesture_Record_Count == 0, 0, Gesture_Type_Count)  # Ensure type counts are 0 if no Signal_Record present
  )  %>%
  dplyr::arrange(Subject, Experimental_Condition)  # Sort by Subject and Experimental condition
write.csv(summary_gest_data, "exp_gest_summary_df.csv", row.names = FALSE) 

##Extract summary dataset for Novel gesture action types in post-delivery phase (not sure if keep as only Yes/No if 1,2,3 novel gest types used)----
xdata4 <- xdata2 %>%
  dplyr::filter(!Signal_Record %in% c("None", "Unknown", "Trunk-Burp", 
                               "Croaking", "Head-Shake", "Trunk-Reach_Unc", "Trunk-Fling_Unc", 
                               "Lean-Back", "Trunk-Touch-Leg",
                               "Lean-Towards-Ex", "Trunk-Touch-Ear",
                               "Trunk-Touch-TG", "Trunk-Touch-Face")) %>%
  dplyr::filter(!Bout_part %in% c("Unclear")) #all rows for no gesture record

gest_by_phase <- xdata4 %>%
  group_by(File_Name, Experimental_Condition, Subject, Sex, Age, Group) %>%
  dplyr::summarize(
    Pre_Delivery_Gest_Types = list(as.character(Signal_Record[Delivery_Phase == "Pre-delivery"])),
    Post_Delivery_Gest_Types = list(as.character(Signal_Record[Delivery_Phase == "Post-delivery"])),
    Pre_Delivery_Gest_Types_Count = length(Pre_Delivery_Gest_Types[[1]]),
    Post_Delivery_Gest_Types_Count = length(Post_Delivery_Gest_Types[[1]]),
    .groups = 'drop'
  )

## Step 2: Check for novel gestures used and repeated gestures used in post-delivery phase
novel_rep_gest_comparison <- gest_by_phase %>%
  dplyr::mutate(
    Novel_Gest_Types_Used = sapply(seq_along(Pre_Delivery_Gest_Types), function(i) {
      pre_types <- Pre_Delivery_Gest_Types[[i]]
      post_types <- Post_Delivery_Gest_Types[[i]]
      
      # Handling cases where pre or post-delivery gestures are missing
      if (length(pre_types) == 0 & length(post_types) == 0) {
        return("NV")
      } else if (length(post_types) == 0) {
        return("NV")
      } else if (length(pre_types) == 0) {
        return("Yes")
      }
      
      novel_types <- setdiff(post_types, pre_types)
      return(ifelse(length(novel_types) > 0, "Yes", "No"))
    }),
    Repeated_Gest_Types_Used = sapply(seq_along(Pre_Delivery_Gest_Types), function(i) {
      pre_types <- Pre_Delivery_Gest_Types[[i]]
      post_types <- Post_Delivery_Gest_Types[[i]]
      
      # Handling cases where pre or post-delivery gestures are missing
      if (length(pre_types) == 0 & length(post_types) == 0) {
        return("NV")
      } else if (length(post_types) == 0) {
        return("NV")
      } else if (length(pre_types) == 0) {
        return("No")
      }
      
      repeated_in_post <- intersect(pre_types, post_types)
      return(ifelse(length(repeated_in_post) > 0, "Yes", "No"))
    }),
    Novel_Gest_Types_Count = sapply(seq_along(Post_Delivery_Gest_Types), function(i) {
      post_types <- Post_Delivery_Gest_Types[[i]]
      pre_types <- Pre_Delivery_Gest_Types[[i]]
      return(sum(post_types %in% setdiff(post_types, pre_types)))
    }),
    Repeated_Gest_Types_Count = sapply(seq_along(Post_Delivery_Gest_Types), function(i) {
      post_types <- Post_Delivery_Gest_Types[[i]]
      pre_types <- Pre_Delivery_Gest_Types[[i]]
      return(sum(post_types %in% intersect(post_types, pre_types)))
    })
  )

# Convert list columns to character vectors
novel_rep_gest_comparison <- novel_rep_gest_comparison %>%
  dplyr::mutate(
    Pre_Delivery_Gest_Types = sapply(Pre_Delivery_Gest_Types, function(x) paste(x, collapse = ",")),
    Post_Delivery_Gest_Types = sapply(Post_Delivery_Gest_Types, function(x) paste(x, collapse = ","))
  )

# Arrange by subject
novel_rep_gest_comparison <-  novel_rep_gest_comparison %>%
  dplyr::arrange(Subject, Experimental_Condition) 
  
write.csv(novel_rep_gest_comparison, "exp_nov_rep_gest_summary_df.csv", row.names = FALSE)

## Step 3: Summarize counts and frequencies where novel gestures are used and repeated gestures are used x condition
summary_novel_repeated_cond <- novel_rep_gest_comparison %>%
  group_by(Experimental_Condition) %>%
  dplyr::summarize(
    Novel_Gest_Types_Freq_Total = sum(Novel_Gest_Types_Count, na.rm = TRUE),
    Repeated_Gest_Types_Freq_Total = sum(Repeated_Gest_Types_Count, na.rm = TRUE),
    Post_Delivery_Gest_Types_Freq_Total= sum(Post_Delivery_Gest_Types_Count, na.rm = TRUE),
    Novel_Gest_Types_Used_Percent = sum(Novel_Gest_Types_Count) / Post_Delivery_Gest_Types_Freq_Total,  # Frequency of novel gestures used
    Repeated_Gest_Types_Used_Percent = sum(Repeated_Gest_Types_Count) / Post_Delivery_Gest_Types_Freq_Total,  # Frequency of repeated gestures used
    .groups = 'drop'
  )
setwd("/Users/ve/Desktop/PhD/Analysis/Chapter 2 - Experiment/results")
write.csv(summary_novel_repeated_cond, "cond_nov_rep_gest_summary.csv", row.names = FALSE)


#Extract Experiment Trial durations across conditions (only if excluding Sig_dur_ana !=Exclude not done)----
xdata5 <- xdata4 %>%
  dplyr::filter(!Delivery_Phase %in% c("No-experimenter")) 

ET_dur <- xdata5 %>%
  group_by(Experimental_Condition, Delivery_Phase) %>%
  dplyr::summarise(
    mean_value = round(mean(ET_Duration, na.rm = TRUE)/1000, 3), #convert to secs dividing by 1000 and 3 dec places
    sd_value = round(sd(ET_Duration, na.rm = TRUE)/1000, 3),
    max_value = round(max(ET_Duration, na.rm = TRUE)/1000),
    min_value = round(min(ET_Duration, na.rm = TRUE)/1000),
    range_value = round(max(ET_Duration, na.rm = TRUE)/1000 - min(ET_Duration, na.rm = TRUE)/1000, 3)
  )

write.csv(ET_dur, "Exp_Trial_dur_mean_range.csv", row.names = FALSE, quote=FALSE)

#Save Rdata----
setwd("/Users/ve/Desktop/PhD/Analysis/Chapter 2 - Experiment")
save.image("Experiment data sorting.Rdata") #Add Rdata


