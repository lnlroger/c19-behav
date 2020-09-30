install.packages("devtools")
install.packages("RCurl")
install.packages("XML")
devtools:::install_github("duncantl/XMLRPC")
devtools:::install_github("duncantl/RWordPress")

library(RWordPress)

options(WordPressLogin = c(lionelroger = 'heyheyw0rd'), # your user name & password
        WordPressURL = 'https://lionelroger.wordpress.com/xmlrpc.php') # your WP url + /xmlrpc.php at the end


library(knitr)
knit2wp('C:/Users/lnlro/Dropbox/Research/COVID-19/COVID-19/CoronaNet/explore_coronanet_shiny.rmd',
        title = 'I dont know', publish = FALSE) # your filename and blog post title
