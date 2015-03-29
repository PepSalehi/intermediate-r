source("header.R")

## ============================================================================
## EXERCISES
## ============================================================================

## qq
## (1) starting with dfz, obtain view of the following [6 rows]
##      (try to use pipes: %>%)
##      Agency: Department of Treasury
##      Bureau: Interest on the Public debt
##      columns: in addition to above, "Account Name" and all year variables
##              from 2010-2019
##      then, add new column "delta" of % change from 2012 to 2013
##      and finally, sort by "delta", descending
## ref: https://github.com/WhiteHouse/2016-budget-data/blob/master/USER_GUIDE.md

dfz %>% filter(Agency.Code==15, Bureau.Code==60) %>%
    select(Agency.Name, Bureau.Name, Account.Name, starts_with("X201")) %>%
        mutate(delta=100*(X2013-X2012)/abs(X2012)) %>% as.data.frame() %>%
            arrange(desc(delta))

## (2) obtain the "outlays" row from Table S-1 (data up to 2020):
## https://medium.com/budget-document/summary-tables-1bfa22a85812
df <- dfz %>% select(starts_with("X")) %>% summarise_each(funs(sum)) %>%
    as.data.frame()
df2 <- dfz %>% select(X2016:X2020) %>% summarise(tot_16_20=sum(.))
bind_cols(df,df2) %>% as.data.frame()
