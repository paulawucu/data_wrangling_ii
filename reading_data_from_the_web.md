Read data from the web
================

I’m an R Markdown document!

## NSDUH data

``` r
url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

drug_use_html = 
  read_html(url)  # r has imported the html page

 # we got the data
drug_use_df = 
  drug_use_html %>% # drug_use_html now becomes a local var
  html_table()  %>% #extract tables and try to format of it
  first() %>%  # take a look at the first table
  slice(-1)  # slice out the first row
```

## Star wars

Get some star wars data

``` r
sw_url = "https://www.imdb.com/list/ls070150896/"
sw_html = 
  read_html(sw_url)

sw_titles = 
  sw_html %>% 
  html_elements(".lister-item-header a") %>% # select the gadget
  html_text() # turn to text

sw_revenue = 
  sw_html %>% 
  html_elements(".text-muted .ghost~ .text-muted+ span") %>% 
  html_text()

sw_df = 
  tibble(
    title = sw_titles,
    revenue = sw_revenue
  )
```

## Napoleon dynamite

Dynamite reviews

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = 
  read_html(dynamite_url)

dynamite_review_titles = 
  dynamite_html %>% 
  html_elements(".a-text-bold span") %>% 
  html_text()

dynamite_stars = 
  dynamite_html %>% 
  html_elements("#cm_cr-review_list .review-rating") %>% 
  html_text()

dynamite_df = 
  tibble(
    titles = dynamite_review_titles,
    stars = dynamite_stars
  )
```