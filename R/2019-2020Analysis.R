library(googledrive)
library(googlesheets4)
library(tidyverse)

fall_freire <- read_sheet("https://docs.google.com/spreadsheets/d/1K7s8WhhStppogW9Ig4DWFj3POGi8-eZIUtG-EWhR45w/edit#gid=0", sheet = 1)
fall_d11 <- read_sheet("https://docs.google.com/spreadsheets/d/12HTjzizH4yoyju_gUeBrYtNkmFMgjSojuxDH8z0c3AE/edit#gid=1145074088", sheet = 1)
fall_allOther <- read_sheet("https://docs.google.com/spreadsheets/d/1WFwDKQ74ibnLVHtt1p1-xcxVpivXT7wT1opmw17ktws/edit#gid=0", sheet = 1)
fall_massacheussets <- read_sheet("https://docs.google.com/spreadsheets/d/1VxVwFU_BQ3p2eybGSGOBiVDoDV7sRYcD9CNwMldZ0cw/edit#gid=0", sheet = 1)
needsAssessment_allOther <- read_sheet("https://docs.google.com/spreadsheets/d/1xlP7wRgBwcwhhLjypVnR6X-XmwJMgGHBtPtrCqPqQ1U/edit#gid=724018753", sheet = 1)
needsAssessment_delaware <- read_sheet("https://docs.google.com/spreadsheets/d/1XWteLXnOnFN2ulvB5P0RpmXT6uYuEc43LWHhlsZ2kss/edit#gid=0", sheet = 1)
spring_allOther <- read_sheet("https://docs.google.com/spreadsheets/d/1j-qLYFM9fU0ox3N-3sX5dHSJOIayTluWQFk2h_mfZTU/edit#gid=0", sheet = 1)
fall_edSurvey <- read_sheet("https://docs.google.com/spreadsheets/d/1956U1s3n31fD5o0FbNjnfqt0OjqnQGvZVrhTpnE_shg/edit#gid=0", sheet = 1)

#### Initial look at data indicates that start date, end date, ip address, and id are best joins

# START CLEAN








