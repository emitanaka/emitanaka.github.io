library(RSelenium)
library(tidyverse)
library(rvest)


rD <- rsDriver(browser="firefox", port=4563L, verbose=F)
remDr <- rD[["client"]]
gotos <- c("2021 - 22 Regular Season" = "206/275", 
           "2021 Pre Season" = "206/274", 
           "2020 - 21 Regular Season" = "206/273",
           "2019 - 20 Regular Season" = "206/270", 
           "2019 Pre Season" = "206/269",
           "2019 WHL Playoffs" = "206/267",
           "2018 - 19 Regular Season" = "206/266",
           "2018 Pre Season" = "206/265",
           "2018 WHL Playoffs" = "206/264",
           "2017 - 18 Regular Season" = "206/262",
           "2017 Pre Season" = "206/261",
           "2017 WHL Playoffs" = "206/260",
           "2016 - 17 Regular Season" = "206/257",
           "2016 Pre Season" = "206/256",
           "16 Tie Break" = "206/255",
           "2016 WHL Playoffs" = "206/254",
           "2015-16 Regular Season" = "206/251",
           "2015 Preseason" = "206/252",
           "2015 WHL Playoffs" = "206/250",
           "2014-15 Regular Season" = "206/249",
           "2014 Preseason" = "206/248",
           "2014 WHL Playoffs" = "206/246",
           "2013-14 Regular Season" = "206/245",
           "2013 WHL Preseason" = "206/244",
           "2013 WHL Playoffs" = "206/243",
           "2012-13 Regular Season" = "206/242",
           "2012 WHL Preseason" = "206/241",
           "2012 WHL Playoffs" = "206/240",
           "2011-12 Regular Season" = "206/238",
           "2011 WHL Preseason" = "206/239",
           "2011 WHL Playoffs" = "206/237",
           "2010-11 Regular Season" = "206/236",
           "2010 WHL Playoffs" = "206/235",
           "2009-10 Regular Season" = "206/234",
           "2009 WHL Playoffs" = "206/232",
           "2008-09 Regular Season" = "206/231",
           "2008 WHL Playoffs" = "206/230",
           "2007-08 Regular Season" = "206/229",
           "2007 WHL Playoffs" = "206/228",
           "2006-07 WHL Season" = "206/227",
           "2006 WHL Playoffs" = "206/226",
           "2005-06 WHL Season" = "206/225",
           "2005 WHL Playoffs" = "206/224",
           "2004-05 WHL Season" = "206/223",
           "2004 WHL Playoffs" = "206/222",
           "2003-2004 WHL Season" = "206/220",
           "2003 WHL Playoffs" = "206/219",
           "2002-03 WHL Season" = "206/217",
           "2001-2002 WHL Season" = "206/215",
           "2000-2001 WHL Season" = "206/213",
           "1999-2000 WHL Season" = "206/211",
           "1998-99 WHL Season" = "206/209",
           "1997-98 WHL Season" = "206/206",
           "1997 WHL Playoffs" = "206/205",
           "1996-97 WHL Season" = "206/204",
           "1996 WHL Playoffs" = "206/202") 

data <- imap_dfr(gotos, ~{
  remDr$navigate(paste0("https://tigershockey.com/roster/", .x))
  Sys.sleep(5)
  html <- remDr$getPageSource()[[1]]
  read_html(html) %>% 
    html_table() %>% 
    .[[1]] %>% 
    mutate(whl = names(gotos[.y]))
})



data %>% 
  filter(Rookie!="Rookie") %>% 
  rename("No" = "#",
         "WHL" = "whl") %>% 
  mutate(Weight = as.numeric(Weight),
         DOB = as.Date(DOB)) %>% 
  saveRDS(file = "_posts/2022-03-12-maximum-potential/roster.rds")
