# app project

# Loading our libraries

library(tidyverse)
library(lubridate)

# Here's our data, thanks to these folks from the Kaggle community:

# Gautham Prakash - gauthamp10.github.io
# Jithin Koshy - https://koshcreates.blogspot.com/

# Loading the data

appleAppData <- read.csv("appleAppData.csv")

Google.Playstore <- read.csv("Google-Playstore.csv")

# The two stores have different, but similar genres of apps, so let's standardize them

appleAppData <- appleAppData %>%
  rename(
    "Genre" = Primary_Genre
  )

appleAppData$Genre <- gsub("Reference", "Book", appleAppData$Genre)
appleAppData$Genre <- gsub("Book", "Books & Reference", appleAppData$Genre)
appleAppData$Genre <- gsub("Books & Reference & Reference", "Books & Reference", appleAppData$Genre)

appleAppData$Genre <- gsub("News", "Magazines & Newspapers", appleAppData$Genre)
appleAppData$Genre <- gsub("Magazines & Magazines & Newspaperspapers", "Magazines & Newspapers", appleAppData$Genre)

# Now Google's genres

Google.Playstore <- Google.Playstore %>%
  rename(
    "Genre" = Category
  )

# Google differentiated all of their games by genres, so that's a large part of the work

Google.Playstore$Genre <- gsub("Action", "Games", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Adventure", "Games", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Arcade", "Games", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Board", "Games", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Card", "Games", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Casino", "Games", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Casual", "Games", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Puzzle", "Games", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Racing", "Games", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Role Playing", "Games", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Simulation", "Games", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Strategy", "Games", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Trivia", "Games", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Word", "Games", Google.Playstore$Genre)

Google.Playstore$Genre <- gsub("Art & Design", "Graphics & Design", Google.Playstore$Genre)

Google.Playstore$Genre <- gsub("Auto & Vehicles", "Shopping", Google.Playstore$Genre)

Google.Playstore$Genre <- gsub("Beauty", "Lifestyle", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("House & Home", "Lifestyle", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Parenting", "Lifestyle", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Personalization", "Lifestyle", Google.Playstore$Genre)

Google.Playstore$Genre <- gsub("Comics", "Books & Reference", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Libraries & Demo", "Books & Reference", Google.Playstore$Genre)

Google.Playstore$Genre <- gsub("Communication", "Social Networking", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Dating", "Social Networking", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Events", "Social Networking", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Social", "Social Networking", Google.Playstore$Genre)

Google.Playstore$Genre <- gsub("Educational", "Education", Google.Playstore$Genre)

Google.Playstore$Genre <- gsub("Maps & Navigation", "Navigation", Google.Playstore$Genre)

Google.Playstore$Genre <- gsub("Music & Audio", "Music", Google.Playstore$Genre)

Google.Playstore$Genre <- gsub("News & Magazines", "Magazines & Newspapers", Google.Playstore$Genre)

Google.Playstore$Genre <- gsub("Photography", "Photo & Video", Google.Playstore$Genre)
Google.Playstore$Genre <- gsub("Video Players & Editors", "Photo & Video", Google.Playstore$Genre)

Google.Playstore$Genre <- gsub("Tools", "Utilities", Google.Playstore$Genre)

Google.Playstore$Genre <- gsub("Travel & Local", "Travel", Google.Playstore$Genre)

# weird glitch with gsub, replace again

Google.Playstore$Genre <- gsub("Social Networking Networking", "Social Networking", Google.Playstore$Genre)

# check differences between datasets - only ones that remain are 'stickers' and 'developer tools' for Apple

setdiff(appleAppData$Genre, Google.Playstore$Genre)

# consolidation notes

# 1. content_restriction

# google = get rid of adults 18+ in Google; only 136 & no matching restriction in Apple
# apple = consolidate into below

# Apple - Google:
  
#  4+ = "Everyone"
#  9+ = "Everyone 10+"
#  12+ = "Teen"
#  17+ = "Mature 17+"
#  Not yet rated = "Unrated"

# 2. size

# apple = consolidate into decimal around MB
# split google at letter, filter rows by number & letter, consolidate into MB (less than 1 MB = decimal)

# 3. released/last.updated

# apple = split after 'T', keep date
# google = date format to match apple? yyyy-mm-dd

# 4. average rating, rating_count

# apple = combine current version count with review count, average user rating w/ current version avg.
# google = match column names

# start of consolidation

a <- appleAppData
g <- Google.Playstore

# Change #1 - content_restrictions

# Google's changes to content restrictions

g <- g %>%
  filter(
    Content.Rating != "Adults only 18+"
  ) %>%
  rename(
    "Content_Restriction" = Content.Rating
  )

# Apple's changes to content restrictions

a <- a %>%
  rename(
    "Content_Restriction" = Content_Rating
  )

a$Content_Restriction <- gsub("4+", "Everyone", a$Content_Restriction, fixed = TRUE)
a$Content_Restriction <- gsub("9+", "Everyone 10+", a$Content_Restriction, fixed = TRUE)
a$Content_Restriction <- gsub("12+", "Teen", a$Content_Restriction, fixed = TRUE)
a$Content_Restriction <- gsub("17+", "Mature 17+", a$Content_Restriction, fixed = TRUE)
a$Content_Restriction <- gsub("Not yet rated", "Unrated", a$Content_Restriction, fixed = TRUE)

# Change #2 - size

# Google's changes

# 1. need to separate size into numbers and letters

unique(str_length(g$Size))

# why 18?

g %>%
  filter(
    str_length(Size) > 17
  )

# Oh! Some vary with size, remove? only accounts for ~3% of the data set
# First, let's check what length 6 has

g %>%
  filter(
    str_length(Size) > 5
  )

# fixed value, so let's keep everything at 6 and below. But why 0?

g %>%
  filter(
    str_length(Size) == 0
  )

# errors and only 196 values, let's remove

g <- g %>%
  filter(
    str_length(Size) != 0, 
    str_length(Size) < 7
  )

# need to split size into numbers & letters, but some values have commas

g$Size <- gsub(",", "", g$Size)

# there we go, now on to the split 

g <- g %>%
  mutate(
    Size2 = str_split_fixed(Size, '[a-zA-Z]', n = 2),
    Size3 = str_extract(Size, '[a-zA-Z]')
  ) %>%
  select(
    App.Name:Size, Size2, Size3, Minimum.Android:Scraped.Time
  )

# Size2 has become its own data frame; combine and remove ""

g$Size2 <- as.numeric(paste(g$Size2[,1], g$Size2[,2], sep = ""))

# need to remove scientific notation before standardizing sizes

options(scipen = 100)

# Need to standardize by letter into megabytes size:

# k = x*10^-3
# G = x*10^3

g <- g %>%
  group_by_all() %>%
  summarise(
    'Size (MBs)' = 
      if(Size3 == "k"){
        'Size (MBs)' = Size2*10^-3
      } else if(Size3 == "G"){
        'Size (MBs)' = Size2*10^3
      } else{
        'Size (MBs)' = Size2
      }
  ) %>%
  ungroup() %>%
  select(
    App.Name:Currency, 'Size (MBs)', Minimum.Android:Scraped.Time
  )

# Apple's changes to app size

# megabytes = (bytes)*10^-6

a1 <- a %>%
  mutate(
    'Size (MBs)' = (Size_Bytes) * 10^-6
  ) %>%
  select(
    App_Id:Content_Restriction, 'Size (MBs)', Required_IOS_Version:Current_Version_Reviews
  )

# check for nulls

sum(is.na(a1$`Size (MBs)`))

# nulls before?

sum(is.na(a$Size_Bytes))

# remove nulls from data set

a <- a1 %>%
  filter(
    !is.na(a1$`Size (MBs)`)
  )

# Change #3 - release / last.updated

# Apple's changes to dates

a1 <- a[1:10,] %>%
  separate(Released, 
           into = c("Released"),
           sep = '[T]'
  ) %>%
  separate(
    Updated,
    into = c("Updated"),
    sep = '[T]'
  ) %>%
  select(
    Released, Updated
  )

# We need lubridate library

library(lubridate)

# test on Apple

ymd(a1$Released)

# good format, let's test on Google

g1 <- g[1:10,]
ymd(g1$Released)

# not so good (NAs), let's try mdy()

mdy(g1$Released)

# looking good, let's standardize dates in both sets and change column names

# Apple's changes

a <- a %>%
  separate(Released, 
           into = c("Released"),
           sep = '[T]'
  ) %>%
  separate(
    Updated,
    into = c("Updated"),
    sep = '[T]'
  ) %>%
  rename(
    "Last.Updated" = Updated
  )

a$Released <- ymd(a$Released)
a$Last.Updated <- ymd(a$Last.Updated)

# google's

g$Released <- mdy(g$Released)
g$Last.Updated <- mdy(g$Last.Updated)

# remove samples

remove(a1, g1)

# Change #4 - average rating + rating count in Apple

# Apple sample

a1 <- a[1:1000,] %>%
  select(
    Average_User_Rating, Reviews, Current_Version_Score, Current_Version_Reviews
  )

# difference between Reviews & Current_Version_Reviews totals?

setdiff(a1$Reviews, a1$Current_Version_Reviews)

# nope! what about averages?

setdiff(a1$Average_User_Rating, a1$Current_Version_Score)

# in the larger set?

setdiff(a$Reviews, a$Current_Version_Reviews)
setdiff(a$Average_User_Rating, a$Current_Version_Score)

# no difference, let's just get rid of the 'current versions' and change headers

a <- a %>%
  select(
    App_Id:Reviews
  ) %>%
  rename(
    "Rating.Count" = Reviews,
    "Rating" = Average_User_Rating
  )

# remove sample

remove(a1)

# column renaming before combining data sets

a <- a %>%
  select(
    App_Id, App_Name, Genre, Content_Restriction, Price, Currency, Free, `Size (MBs)`, Released, Last.Updated, Developer, Developer_Website, Rating, Rating.Count
  )

g <- g %>%
  select(
    App.Id, App.Name, Genre, Content_Restriction, Price, Currency, Free, `Size (MBs)`, Released, Last.Updated, Developer.Id, Developer.Website, Rating, Rating.Count
  ) %>%
  rename(
    "App_Id" = App.Id,
    "App_Name" = App.Name,
    "Developer" = Developer.Id,
    "Developer_Website" = Developer.Website
  )

# improving Id columns

store <- c("A", "G")
nA <- seq(nrow(a))
nG <- seq(nrow(g))

a <- a %>%
  mutate(
    App_Id = paste(store[[1]], nA, sep = ".")
  )

g <- g %>%
  mutate(
    App_Id = paste(store[[2]], nG, sep = ".")
  )

g <- data.frame(g)
g <- g %>%
  rename(
    "Size (MBs)" = Size..MBs.
  )

# combining data sets

clean_apple <- a
clean_google <- g

AG_Stores <- rbind(clean_apple, clean_google)

# save the file

# library(data.table)
# fwrite(AG_Stores, "AG_stores.csv")