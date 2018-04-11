---
  title             : "Comparing MTurk and the US Population’s Occupational Diversity: A Replication of Mahmoud et al. (2017)"
shorttitle        : 
  
  author: 
  - name          : "Sarah Avet"
affiliation   : "1"
corresponding : yes    # Define only one corresponding author
email         : ""
- name          : "Kathryn Daigle"
affiliation   : "2"
- name          : "Bernard Wezeman"
affiliation   : "3"
- name          : "Christopher Castille"
affiliation   : "4"
  - id            : "1"
institution   : "Nicholls State University"
- id            : "2"
institution   : "Nicholls State University"
- id            : "3"
institution   : "Nicholls State University"
- id            : "4"
institution   : "Nicholls State University"

author_note: >
  All correspondence should be sent to the lead author at .

abstract: >
  'Over the past couple years, ...' 

keywords          : "MTurk, Occupational Diversity"
wordcount         : "3000"

bibliography      : ["references.bib"]

figsintext        : no
figurelist        : no
tablelist         : yes
footnotelist      : no
lineno            : no

lang              : "english"
class             : "man"
output            : papaja::apa6_pdf
---
  
  ```{r message = FALSE, warning = FALSE}
#The packages below will allow you to access "papaja", and if it fails, then try running all of the code at once. It should work then. 
library("papaja")
```
#Introduction


#Review of the Literature
##What is MTurk?


##Why is MTurk Valuable for Researchers Studying Working Populations?


*Research Question: Is the occupational diversity of the MTurk population representative of the broader US economy?*
  
  # Methods
  ##US Population Occupational Summary Statistics
  
  
  #- - - - - - - - - - - - - 
  #insert table 1 about here
  #- - - - - - - - - - - - - 
  
  ```{r,BLS, include=FALSE, results='asis'}
#Load packages
library(readr)
library(readxl)
library(data.table)
library(openxlsx)

#Load datasets. Note: these datasets were not taken direclty from their respective sources (i.e., BLS or Qualtrics survey, respectively). The BLS dataset is a table describing the broader occupational categories, number of employees working in jobs that fall under one of these categories, and the relative frequency of jobs falling under said occupational category. The LATech dataset comes from a separate investigaiton using MTurk whereby individuals who were flagged for inattentive responding were removed from the dataset. 
#Build the BLS Table, but first establish a temp file. 

##BLS Data
library(readr)
BLS <- read_csv("2017 BLS Data.csv")
BLS <- BLS[c(7,8,10)]

#Include data on the size of the military (taken from (https://www.bls.gov/ooh/military/military-careers.htm))
BLS <- rbind(BLS, c("Military specific occupations", 1085286))

#Only major occupation groups are included in BLS
BLS <- 
  BLS[c(2,71,127,159,219,290,316,332,428,490,581,607,648,682,700,760,803,916,943,1046,1122,1288,427267,1),]

#Rearrange line 23 so it resembles rest of code
BLS[23, 1] = 55-0000
BLS[23, 2] = "Military Specific Occupations"
BLS[23, 3] = 1085286

###Rename rows and aggregate.
BLS[1, 1] = "11-13 Management, Business, and Financial Occupations"
BLS[2, 1] = "11-13 Management, Business, and Financial Occupations"
BLS[3, 1] = "15-19 Computer, Engineering, and Science Occupations"
BLS[4, 1] = "15-19 Computer, Engineering, and Science Occupations"
BLS[5, 1] = "15-19 Computer, Engineering, and Science Occupations"
BLS[6, 1] = "21-27 Education, Legal, Community Service, Arts, and Media Occupation"
BLS[7, 1] = "21-27 Education, Legal, Community Service, Arts, and Media Occupation"
BLS[8, 1] = "21-27 Education, Legal, Community Service, Arts, and Media Occupation"
BLS[9, 1] = "21-27 Education, Legal, Community Service, Arts, and Media Occupation"
BLS[10, 1] = "29 Healthcare Practitioners and Technical Occupations"
BLS[11, 1] = "31-39 Service Occupations"
BLS[12, 1] = "31-39 Service Occupations"
BLS[13, 1] = "31-39 Service Occupations"
BLS[14, 1] = "31-39 Service Occupations"
BLS[15, 1] = "31-39 Service Occupations"
BLS[16, 1] = "41 Sales and Related Occupations"
BLS[17, 1] = "43 Office and Administrative Support Occupations"
BLS[18, 1] = "45 Farming, Fishing, and Forestry Occupations"
BLS[19, 1] = "47 Construction and Extraction Occupation"
BLS[20, 1] = "49 Installation, Maintenance, and Repair Occupations"
BLS[21, 1] = "51 Production Occupation"
BLS[22, 1] = "53 Transportation and Material Moving Occupation"
BLS[23, 1] = "55 Military Specific Occupations"
names(BLS) <- c("Var1","Var2")
BLS$Var2 <- as.numeric(BLS$Var2)
BLS <- aggregate(.~Var1,BLS,sum)

#Add military occupation number to the overall
BLS$Var2[14] <- BLS$Var2[13] + BLS$Var2[14]

#Rename variables
names(BLS) <- c("Intermediate Aggregation Title", "Freq")

#Only keep occupational categories (no all). 
BLS <- BLS[-14,]

#Compute relative frequencies. Call the object "tab". 
tab <- transform(BLS, relative = prop.table(Freq))

#Define new BLS. "e" is for "relative frequency". 
BLS <- tab[c(1,3)]
names(BLS) <- c("Intermediate Aggregation Title", "e")

##LATech data
LATech <- read_csv("2017 Data.csv")

###Delete unneeded data.
LATech <- LATech[c(12, 20:21)]
names(LATech) <- c("Self-Reported Job Title", "O*NET Occupation Title", "O*NET Job Code")

###Edit data. Some participants provided incorrect codes or no codes.
LATech[644, 3] = "41-9031.00"
LATech[274,] = c("analyst", "analyst", "13-0000.00")

###Remove unemployed cases.
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
LATech <- completeFun(LATech, "O*NET Job Code")

###Abbreviate data, consolidate into intermediate categories, and bind.
LO <- LATech[c(3)]
LO <- read.fwf(textConnection(LO$`O*NET Job Code`), 2)
names(LO) <- c("Int.Agg.Cat")
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 11] <- "11-13 Management, Business, and Financial Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 13] <- "11-13 Management, Business, and Financial Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 15] <- "15-19 Computer, Engineering, and Science Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 17] <- "15-19 Computer, Engineering, and Science Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 19] <- "15-19 Computer, Engineering, and Science Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 21] <- "21-27 Education, Legal, Community Service, Arts, and Media Occupation"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 23] <- "21-27 Education, Legal, Community Service, Arts, and Media Occupation"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 25] <- "21-27 Education, Legal, Community Service, Arts, and Media Occupation"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 27] <- "21-27 Education, Legal, Community Service, Arts, and Media Occupation"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 29] <- "29 Healthcare Practitioners and Technical Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 31] <- "31-39 Service Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 33] <- "31-39 Service Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 35] <- "31-39 Service Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 37] <- "31-39 Service Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 39] <- "31-39 Service Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 41] <- "41 Sales and Related Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 43] <- "43 Office and Administrative Support Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 45] <- "45 Farming, Fishing, and Forestry Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 47] <- "47 Construction and Extraction Occupation"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 49] <- "49 Installation, Maintenance, and Repair Occupations"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 51] <- "51 Production Occupation"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 53] <- "53 Transportation and Material Moving Occupation"
LO$Int.Agg.Cat[LO$Int.Agg.Cat == 55] <- "55 Military Specific Occupations"
LATech <- cbind(LATech,LO)

###Build relative frequency table.
tab <- as.data.frame(table(LO$Int.Agg.Cat, exclude=NULL))
tab <- transform(tab, relative = prop.table(Freq))
names(tab) <- c("Intermediate Aggregation Title","n","f")
tab <- merge(tab,BLS, by="Intermediate Aggregation Title", all=TRUE) 

#Chi-square function that quantifies the discrepancy between expected and observed proportions.
chi2 <- function(x) {
  o <- (x$n)
  e <- (x$e)
  n <- sum(x$n, na.rm=TRUE)
  ((o-(e)*(n))^2)/((e)*(n))
}

#Confidence interval formula for proportions with assymptotic intervals. Intervals are assymptotic because proportions cannot be less than 0 or greater than 1, which becomes problematic as proportions near 0 or 1. 
require("binom")
CI <- binom.confint(table$n, sum(table$n), conf.level = 0.95, methods = "exact")
CI <- CI[c(5:6)]
table <- cbind(table,CI)
table <- table[,c(1,2,3,5,4,6)]
ch <- as.data.frame(chi2(table))
ch <- na.omit(ch)
names(table) <- c("Intermediate Aggregation Title","Exp(p)","n1","LCL1","Obs(p)1","UCL1")

#Sum chi-square values.
sum(ch$`chi2(table)`, na.rm=TRUE)

##Validate using chi-square function in r.
#Function for differences in relative percentages. This is not printed in the tables, but could be used to help readers see the disparity between expected and observed proportions.
f <- function(x) {
  f   <- (x$f)
  BLS <- (x$e)
  (f-BLS)
}
f1 <- as.data.frame(f(table))

#Print chi-square APA style.
require(devtools)
install_github("dgromer/apa")
require(apa)
##Sample 1#
Table2 <- merge(table,tab2, by="Intermediate Aggregation Title", all=TRUE)
observed1 <- Table2$n1
observed1 <- observed1[c(1:13)]
expected1 <- Table2$`Exp(p)`
expected1 <- expected1[c(1:13)]
c1 <- chisq_apa(chisq.test(x = observed1, p = expected1, correct = TRUE))

#Post-hoc power analysis.
##Convert chi-squares to correlation coefficient to allow meaningful statistic.
library(pwr)
###Study 1
X21 <-sum(ch$`chi2(table)`, na.rm=TRUE)
r1 <- sqrt((X21/sum(tab$n)))
pwr.chisq.test(w=r1,N=(sum(tab$n)),df=12,sig.level = .01)
###Study 2
X22 <- sum(ch2$`chi2(tab2)`, na.rm=TRUE)
r2 <- sqrt((X21/sum(tab2$n2)))
pwr.chisq.test(w=r2,N=(sum(tab2$n2)),df=12,sig.level = .01)
##Conclusion - both studies possess enough power to detect the disparities that we have observed; namely that the observed distribution of occupations in MTurk does not match the distribution of the larger economy. 
###When interpreting the differences, point out the magnitude of the disparity between expectations and observations. 

```

##MTurk Occupational Summary Statistics
Participants’ data were collected from two independently conducted studies (n~1~ = `r nrow(LATech)`; n~2~ = `r nrow(UGA)`). In one study (n~1~ =`r nrow(LATech)`), occupational data was self-reported by participants who were asked to described their job title. They were then brought to ONET online and asked to retrieve the  six-digit occupational classification code that most closely corresponded to their job title. In the second study (n~2~ = `r nrow(UGA)`), participants again self-reported their job title. However, the six-digit occupational classification codes were obtained by training research assistants to match the self-reported job titles to those in the ONET database. Using this data, we described the occupational diversity of each sample using relative frequencies of occupational category representation as described by Table 1. 

##Analysis
A chi-square test of goodness-of-fit was used to test whether proportions from MTurk samples deviated from expected proportions as derived from the BLS. Specifically, we tested whether MTurk’s occupational diversity deviated (p < .05) from that expected if we assume the BLS population parameters generalize to the MTurk population. This tests the null hypothesis that the populations are identical in terms of occupational diversity. To facilitate interpretation of our data, we also caluculated 95% confidence intervals for our sample proportions, which allows a visual inspection of whether or not the population value is within sampling error.

#Results
Results shown in our table indicate that MTurk’s occupational frequency distribution does not consistently align with the US economy for both sample 1 ($\chi^2(12) = 368.58, p < .001$) and sample 2 ($\chi^2(12) = 486.07, p < .001$). Examining the 95% confidence intervals across both samples, certain occupations were consistently over- or under-represented. More specifically individuals sampled from MTurk occupied generally more white-collar jobs (e.g., Management, Business, and Financial Occupations; Computer, Engineering, and Science Occupations) and generally fewer blue-collar jobs (e.g., Construction and Extraction Occupations;  Installation, Maintenance, and Repair Occupations; Production Occupations) than what would be expected based on BLS data. For instance, 10.13% of workers in the US economy in 2015 worked in “Management, Business, and Financial Occupations.” By contrast, approximately 17% and 23% of workers from the MTurk samples fell under this classification. These trends were generally consistent across both study 1 and 2.

#- - - - - - - - - - - - - 
#insert table 2 about here
#- - - - - - - - - - - - - 

#Discussion
What started our investigation was a simple research question: is the occupational diversity of the MTurk population representative of the broader US population? Our data suggest that the answer is not consistently so: certain occupations are over-represented, some fall within expectations, and still others are under-represented. Thus, OB researchers using convenient sampling procedures that draw on the MTurk population should consider whether occupational features might play a role in their investigation, which is point that we will elaborate upon in greater detail. Simply put, our results suggest that MTurk’s (US) occupational diversity does not align with the broader US economy. 

#Recommendations for Researchers Using MTurk
Given that OB researchers will likely wish to continue using MTurk as a means of gathering diverse, convenient, and low-cost survey sample data, researchers should consider how differences in the occupational diversity observed amongst MTurk workers relative to the US population might impinge upon the generalizability of their findings (@newman2015external). Specifically, given the over-representation of workers from (1) Management, Business, and Financial, (2) Computer, Engineering, and Science, and (3) Education, Legal, Community Service, Arts, and Media occupations, we might safely assume that many of our findings from MTurk will generalize across jobs classified under these broader over-represented occupational categories. However, as many occupations were under-represented [e.g., (1) Service, (2) Construction and Extraction, (3) Installation, Maintenance, and Repair, (4) Production; and (5) Transportation and Material Moving], questions may arise as to whether findings will generalize to jobs falling under these broad classifications. 
To address this issue, researchers should consider capturing occupational data and modeling theoretically derived moderators. Person-environment (P-E) fit theories should help guide researchers in proposing how certain occupational groupings might influence individual-level relationships, lending to more nuanced and contextualized results (@how2009dr). Further, O*NET might be useful in this regard as it provides researchers a means for quantifying key occupational differences (e.g., level of competition requirements, social skills requirements) and thereby testing P-E fit hypotheses pertaining to occupational group differences (see @judge2015person). 

##Limitations and Future Research
One key limitation is that we assumed that O*NET’s classification system perfectly captured all occupations during 2015. Additionally, as both studies involved convenient sampling procedures, it remains uncertain the extent to which these samples adequately reflect the larger MTurk population. Still, the consistency of our findings should be considered by future OB/HR researchers hoping to draw on MTurk for their research with the hopes of generalizing to the larger (or a specific) working US population. Additionally, as our studies were conducted in the US, it remains unclear how other MTurk populations might reflect (however imperfectly) their respective broader economy. Future larger scale studies should also consider lower levels of BLS occupational classification to examine the extent to which narrower occupations might not be represented by MTurk. Lastly, examining diversity in other respects (age, racio-ethnic, income, education, etc.) should also be considered. Though MTurk offers a practically feasible method of attaining high quality data, future research will be needed to understand the limitations of this tool.

Given that our data were gathered and analyzed in 2015, it might be insightful to have our procedures replicated annually to examine how the demographics of MTurk, specifically occupational characteristics, change over time, especially as access to technology becomes even easier and more commonplace. Thus, we encourage researchers to continue examining the occupational diversity of MTurk relative to the US economy. We also encourage researchers to consider novel ways of approximating the occupational diversity of both the US economy and MTurk because the currently existing taxonomies that we have used (i.e., BLS-based) probably do not capture all occupations. Lastly, our results raise a new and interesting research question: Why might MTurk possess the occupational diversity makeup that it does (i.e., why are some occupations over-represented while others are under-represented)? Similarly, what factors (e.g., access to technology, interest in psychology) might make it easier for workers coming from certain occupations to participate in OB studies hosted on MTurk? Answering these questions might unearth more constraints that impinge on the findings of OB studies using MTurk. 

#Conclusion
We called into question the diversity of MTurk as a key strength and suggested one key area where diversity may be lacking: occupational diversity. Our results suggest that, compared to the broader US population, MTurk is not as diverse, both over- and underrepresenting certain occupations. Future research drawing on MTurk should consider how occupational differences might be theoretically relevant and incorporate these effects in analyses using MTurk sample data to help overcoming these shortcomings.

\newpage

# References
```{r}
require(papaja)
r_refs(file = "/Users/ccasti02/Documents/Research/Research on MTurk/MTurk's Occupational Diversity/r-references.bib")
my_citation <- cite_r(file = "/Users/ccasti02/Documents/Research/Research on MTurk/MTurk's Occupational Diversity/r-references.bib")

```
\setlength{\parindent}{-0.5in}
\setlength{\leftskip}{0.5in}
