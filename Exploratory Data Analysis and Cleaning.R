require(lattice)
require(mosaic)

#Import Lead.DAT
lead = read.csv("~/Desktop/Biostatistical Methods 1/Course Data Sources/LEAD.DAT.txt", header = TRUE)

#Clean up headers
names(lead) = gsub("X.", "", names(lead))
names(lead) = gsub("_2pla", "pla", names(lead))
names(lead) = gsub("\\.", "", names(lead))
names(lead)

#Check variable types
str(lead)
#Variables to change to factor
change_list = c('area', 'sex', 'iq_type', 'lead_grp', 'Group', 'fst2yrs',
                'pica', 'colic', 'clumsi', 'irrit', 'convul')
#Change the variables
lead[, change_list] = lapply(lead[,change_list], factor)
#Success Check
str(lead[,change_list])

###############
#Exploratory Analysis

###Question 1: How does lead exposure (Group Variable), affect the number of finger-wrist taps (maxfwt variable) in the dominant hand?
#This leads to a multi-variant (1 x quantiative & 1 x categoric variable) analysis.

#Number Summary
favstats(~maxfwt|Group, data = lead)
#Boxplot (Group 1 == Unexposed, Group 2 == Exposed)
bwplot(Group~maxfwt, data = lead)
#Density Plot
densityplot(~maxfwt, groups = Group, data = lead, auto.key = TRUE)

###Question 2: How does lead exposure (Group) affect IQ scores (iqf), as measured by Wechsler full-scale IQ scores?
#Multi-variant (1 x quantiative & 1 x categoric variable) analysis

#Number Summary
favstats(~iqf|Group, data = lead)
#Boxplot (Group 1 == Unexposed, Group 2 == Exposed)
bwplot(Group~iqf, data = lead)
#Density Plot
densityplot(~iqf, groups = Group, data = lead, auto.key = TRUE)
