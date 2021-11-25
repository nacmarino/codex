library(RSelenium)
library(wdman)

selCommand <- selenium(jvmargs = c("-Dwebdriver.chrome.verboseLogging=true"), 
                       retcommand = TRUE)
cat(selCommand)

remDr <- remoteDriver(port = 4567L, browserName = "firefox", 
                      #extraCapabilities = list("moz:firefoxOptions" = list(args = list('--headless')))
)
remDr$open()
remDr$navigate('http://www.dpi.inpe.br/calcula/')
foo <- remDr$findElement(using = 'xpath', value = '//html/body/div[1]/center/table/tbody/tr[3]')
foo$getPageSource()
remDr$
  