Strings and Factors
================

## String vectors

``` r
string_vec = c("my", "name", "is", "paula")

# whether or not something match
str_detect(string_vec, "m")
```

    ## [1]  TRUE  TRUE FALSE FALSE

``` r
str_detect(string_vec, "paula")  # case sensitive
```

    ## [1] FALSE FALSE FALSE  TRUE

``` r
str_detect(string_vec, "PAULA")
```

    ## [1] FALSE FALSE FALSE FALSE

``` r
# replace
str_replace(string_vec, "paula", "Paula")
```

    ## [1] "my"    "name"  "is"    "Paula"

``` r
string_vec = c(
  "i think we all rule for participating",
  "i think i have been caught",
  "i think this will be quite fun actually",
  "it will be fun, i think"
  )  # "i think" in every element

str_detect(string_vec, "^i think") # ^: at the beginning of the line
```

    ## [1]  TRUE  TRUE  TRUE FALSE

``` r
str_detect(string_vec, "i think$") # $: at the end of the line
```

    ## [1] FALSE FALSE FALSE  TRUE

``` r
string_vec = c(
  "Y'all remember Pres. HW Bush?",
  "I saw a green bush",
  "BBQ and Bushwalking at Molonglo Gorge",
  "BUSH -- LIVE IN CONCERT!!"
  )
str_detect(string_vec, "[Bb]ush")  # Bush or bush both true, BUSH false
```

    ## [1]  TRUE  TRUE  TRUE FALSE

``` r
string_vec = c(
  '7th inning stretch',
  '1st half soon to begin. Texas won the toss.',
  'she is 5 feet 4 inches tall',
  '3AM - cant sleep :('
  )  # look for pattern: number, followed by a letter

str_detect(string_vec, "[0-9][a-zA-Z][a-zA-Z]")  # false for "5 feet"
```

    ## [1]  TRUE  TRUE FALSE  TRUE

``` r
string_vec = c(
  'Its 7:11 in the evening',
  'want to go to 7-11?',
  'my flight is AA711',
  'NetBios: scanning ip 203.167.114.66'
  )
str_detect(string_vec, "7.11")  # . is the free space, "anything" you want it to be
```

    ## [1]  TRUE  TRUE FALSE  TRUE

``` r
string_vec = c(
  'The CI is [2, 5]',
  ':-]',
  ':-[',
  'I found the answer on pages [6-7]'
  )  # we want to find [ and ]
# str_detect(string_vec, "[") # not working!!
str_detect(string_vec, "\\[")  # \\ before special character 
```

    ## [1]  TRUE FALSE  TRUE  TRUE

## Why factors are weird

``` r
factor_vec = factor(c("male", "male", "female", "female"))  # have two levels, put it in alphabetical order

as.numeric(factor_vec)  # male = 2, female = 1
```

    ## [1] 2 2 1 1

``` r
factor_vec = fct_relevel(factor_vec, "male")  # just specify one, R will take it to the front

as.numeric(factor_vec)  # male = 1, female = 2
```

    ## [1] 1 1 2 2

## NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

table_marj = 
  read_html(nsduh_url) %>% 
  html_table() %>% 
  first() %>%
  slice(-1) #%>% 
  #view  # some untidiness here
```

Let’s clean this up!

``` r
marj_df = 
  table_marj %>% 
  select(-contains("P value")) %>%  # get rid of all columns with "P value"
  pivot_longer(-State, names_to = "age_year", values_to = "percent") %>% 
  separate(age_year, into = c("age", "year"), -11) %>% # form the 11th place to the last, OR use "\\("
  mutate(
    year = str_replace(year, "\\(", ""),
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),  # letter at the end of the line
    percent = as.numeric(percent)
  ) %>% 
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

Do data frame stuff

``` r
marj_df %>% 
  filter(age == "12-17") %>% 
  mutate(
    State = fct_reorder(State, percent)  # put factor variables in a new order
  ) %>% 
  ggplot(aes(x = State, y = percent, color = year)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust= 1))
```

![](strings_and_factors_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Data on restaurant inspection

``` r
data("rest_inspec")
```

``` r
rest_inspec %>% 
  slice(1:100) %>% view  # take a look at the first 100 rows
```

``` r
rest_inspec %>% 
  janitor::tabyl(boro, grade)
```

    ##           boro     A     B    C Not Yet Graded   P    Z   NA_
    ##          BRONX 13688  2801  701            200 163  351 16833
    ##       BROOKLYN 37449  6651 1684            702 416  977 51930
    ##      MANHATTAN 61608 10532 2689            765 508 1237 80615
    ##        Missing     4     0    0              0   0    0    13
    ##         QUEENS 35952  6492 1593            604 331  913 45816
    ##  STATEN ISLAND  5215   933  207             85  47  149  6730

``` r
rest_inspec %>% 
  group_by(boro, grade) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = grade, values_from = n)
```

    ## `summarise()` has grouped output by 'boro'. You can override using the `.groups` argument.

    ## # A tibble: 6 × 8
    ## # Groups:   boro [6]
    ##   boro              A     B     C `Not Yet Graded`     P     Z  `NA`
    ##   <chr>         <int> <int> <int>            <int> <int> <int> <int>
    ## 1 BRONX         13688  2801   701              200   163   351 16833
    ## 2 BROOKLYN      37449  6651  1684              702   416   977 51930
    ## 3 MANHATTAN     61608 10532  2689              765   508  1237 80615
    ## 4 Missing           4    NA    NA               NA    NA    NA    13
    ## 5 QUEENS        35952  6492  1593              604   331   913 45816
    ## 6 STATEN ISLAND  5215   933   207               85    47   149  6730

``` r
rest_inspec = 
  rest_inspec %>% 
  filter(
    str_detect(grade, "[ABC]"),
    !(boro == "Missing")
  ) %>% 
  mutate(boro = str_to_title(boro))
```

``` r
rest_inspec %>% 
  filter(str_detect(dba, "[Pp][Ii][Zz][Zz][Aa]")) %>% 
  janitor::tabyl(boro, grade)
```

    ##           boro    A   B  C
    ##          Bronx 1170 305 56
    ##       Brooklyn 1948 296 61
    ##      Manhattan 1983 420 76
    ##         Queens 1647 259 48
    ##  Staten Island  323 127 21

``` r
rest_inspec %>% 
  filter(str_detect(dba, "[Pp][Ii][Zz][Zz][Aa]")) %>% 
  mutate(
    boro = fct_infreq(boro)  # from most frequent to least
  ) %>% 
  ggplot(aes(x = boro, fill = grade)) +
  geom_bar()
```

![](strings_and_factors_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

What about changing a label?

``` r
rest_inspec %>% 
  filter(str_detect(dba, "[Pp][Ii][Zz][Zz][Aa]")) %>% 
  mutate(
    boro = fct_infreq(boro),  # from most frequent to least
    boro = fct_recode(boro, "The City" = "Manhattan")  # if we use fct_replace, boro will be a character vector; use fct_recode
  ) %>% 
  ggplot(aes(x = boro, fill = grade)) +  # sometimes you can put in ggplot to look if you've done it successfully
  geom_bar()
```

![](strings_and_factors_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
