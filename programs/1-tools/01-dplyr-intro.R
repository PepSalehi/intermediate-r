## make sure your working directory ends in "/programs/1-tools"
getwd()

source("../header.R")

## ============================================================================
## DATA PREPARATION
## ============================================================================
dfz <- read.csv(paste0(datadir, "/outlays.csv"))
dfz <- tbl_df(dfz)

## ## convert to numeric
## dim(dfz)
## dft <- select(dfz, X1962:X2020)
## dft <- dft %>% mutate_each(funs(as.character(.))) %>%
##     mutate_each(funs(gsub(",", "", .))) %>%
##         mutate_each(funs(as.numeric(.)))
## dfz <- select(dfz, -(X1962:X2020))
## dfz <- tbl_df(bind_cols(dfz,dft))
## dim(dfz)
## write.csv(dfz, paste0(datadir, "/outlays_edit.csv"))

## df2 <- select(dfz, Agency.Code, Agency.Name, Bureau.Code, Bureau.Name,
##                Account.Code, Account.Name, X2012, X2013, X2014, X2015)




## ============================================================================
## DATA OVERVIEW
## also see https://github.com/WhiteHouse/2016-budget-data/blob/master/USER_GUIDE.md
## ============================================================================

## column names
names(dfz)

## with tbl_df type, don't have to worry about printing huge table
dfz

## dplyr::glimpse. what does this tell us?
glimpse(dfz)

## other ways to see data?
## QQ


## ============================================================================
## DPLYR FUNCTIONS
## ============================================================================
vignette("introduction", "dplyr")

## Single table verbs

## dplyr aims to provide a function for each basic verb of data manipulating:
## (1) filter() (and slice())
## (2) arrange()
## (3) select() (and rename())
## (4) distinct()
## (5) mutate() (and transmute())
## (6) summarise()
## sample_n() and sample_frac()
## If you’ve used plyr before, many of these will be familar.



## (1) "filter rows with filter()"
filter(dfz, Agency.Code==11)
filter(dfz, Bureau.Code==10)

filter(dfz, Agency.Code==11,  Bureau.Code==10)
filter(dfz, Agency.Code==11 & Bureau.Code==10)



## (2) "select columns with select()"
select(dfz, Agency.Name, Bureau.Name, Account.Name, X2014)
select(dfz, Agency.Name, Bureau.Name, Account.Name, X2010:X2020)
head(select(dfz, Agency.Name, Bureau.Name, Account.Name, X2010:X2020))



## (3) "arrange rows with arrange()"
## uncomment "df2" at top of this file ("Data preparation")
arrange(df2, Bureau.Code)
arrange(df2, desc(Bureau.Code))
arrange(df2, desc(X2015))

## QQ
## exercise: obtain this data view from dfz:

##   Agency.Code Bureau.Code Account.Code      X2015
## 1           9          25         9915 29,398,000
## 2           9          25         9915    167,000
## 3           9          25         9915          0

## [your code here]

## ** INTERLUDE: PIPES (... they are your friend and you will love them!)
## "piping with %>% makes code more readable"
## see PDF handout

## repeat exercise
dfz %>% select(Agency.Code, Bureau.Code, Account.Code, X2015) %>%
    filter(Agency.Code==9 & Bureau.Code==25) %>%
        arrange(desc(X2015))



## (4) "extract distinct (unique) rows"
select(dfz, Agency.Name)

distinct(select(dfz, Agency.Name))
## or
select(dfz, Agency.Name) %>% distinct()

dim(distinct(select(dfz, Agency.Name)))
## or
select(dfz, Agency.Name) %>% distinct() %>% dim()

## NOTE: you can use %>% with ANY function, not just dplyr functions!

## qq
## arrange (sort) unique names
df <- distinct(select(dfz, Agency.Name))
print(arrange(df, Agency.Name), n=20)

## qq exercise: try with pipes!

## [your code here]



## (5) "add new columns with mutate()"
head(df2)
## need numeric variables:
## uncomment and run the chunk of dfz code at top ("Data Preparation")
## and then, also run again the df2 code at top

mutate(df2, X12_15 = X2012 + X2013 + X2014 + X2015)

## with pipes
mutate(df2, X12_15 = X2012 + X2013 + X2014 + X2015) %>% as.data.frame() %>%
    head()



## (6) "summarise values with summarise()" [minimizes output]
summarise(dfz, outlays2014=sum(X2014))

## summarise() is really powerful when working in groups
dfz %>% group_by(Agency.Name, Bureau.Name) %>%
    summarise(outlays1999=sum(X1999), outlays2001=sum(X2001))

## number of Account Codes (categories) by bureau-agency
dfz %>% group_by(Agency.Name, Bureau.Name) %>%
    summarise(count=n())

dfz %>% group_by(Agency.Name, Bureau.Name) %>%
    summarise(count=n()) %>% arrange(desc(count))

dfz %>% group_by(Agency.Name, Bureau.Name) %>%
    summarise(count=n()) %>% as.data.frame() %>% arrange(desc(count)) %>%
        head(n=20)

## (see other summary functions on PDF handout)



