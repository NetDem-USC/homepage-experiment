# Replication materials for Guess et al (2021) PNAS

Replication materials for "[The consequences of online partisan media](https://www.pnas.org/content/118/14/e2013464118)", by Andrew Guess, Pablo Barberá, Simon Munzert, and JungHwan Yang, published in the _Proceedings of the National Academy of Sciences_.

> __Abstract:__
> What role do ideologically extreme media play in the polarization of society? Here, we report results from a randomized longitudinal field experiment embedded in a nationally representative online panel survey (N = 1,037) in which participants were incentivized to change their browser default settings and social media following patterns, boosting the likelihood of encountering news with either a left-leaning (HuffPost) or right-leaning (Fox News) slant during the 2018 U.S. midterm election campaign. Data on approximately 19m web visits by respondents indicate that resulting changes in news consumption persisted for at least eight weeks. Greater exposure to partisan news can cause immediate but short-lived increases in website visits and knowledge of recent events. After adjusting for multiple comparisons, however, we find little evidence of a direct impact on opinions or affect. Still, results from later survey waves suggest that both treatments produce a lasting and meaningful decrease in trust in the mainstream media up to one year later. Consistent with the minimal-effects tradition, direct consequences of online partisan media are limited, though our findings raise questions about the possibility of subtle, cumulative dynamics.  The combination of experimentation and computational social science techniques illustrates a powerful new approach for studying the long-term consequences of exposure to partisan news.

> __Significance statement:__
> Popular wisdom suggests that the internet plays a major role in influencing people's attitudes and behaviors related to politics, such as by providing slanted sources of information. Yet evidence for this proposition is elusive due to methodological difficulties and the multifaceted nature of online media effects. This study breaks new ground by demonstrating a "nudge"-like approach for exploring these effects through a combination of real-world experimentation and computational social science techniques. The results confirm that it is difficult for people to be persuaded by competing media accounts during a contentious election campaign. At the same time, data from a longer time span suggest that the real consequence of online partisan media may be an erosion of trust in mainstream news.

This README file provides an overview of the replications materials for the article. The [Data](https://github.com/NetDem-USC/homepage_experiment#data) section describes the main dataset required to reproduce the tables and figures in the paper. The [Analysis](https://github.com/NetDem-USC/homepage_experiment#code) section summarizes the purpose of each R or python script.

__Note__: In compliance with YouGov's Terms of Service and IRB requirements, we do not publicly share the raw files containing the complete survey responses and URL-level web tracking data. Instead, we provide respondent-level aggregates and the subset of survey variables we use in the study, along with an anonymized panelist ID.

## Data
  - `data/survey_data.csv`: merged dataset with relevant variables from all seven survey waves in the study.
      
 - `data/daily_pulse_data.csv`: respondent-day-level counts of visits to different categories of news sites.
 
## Analysis
  - [01-first-stage](https://github.com/NetDem-USC/homepage_experiment/blob/master/code/01-first-stage.R) to replicate Figures 2 and 3 of the paper, where we show how our encouragement increased news visits to Fox News and Huffington Post, as well as visits to other sites, link shares on Twitter, and event knowledge.

<img src = "https://github.com/NetDem-USC/homepage-experiment/raw/main/graphs/main-fig2.png">
<img src = "https://github.com/NetDem-USC/homepage-experiment/raw/main/graphs/main-fig3.png">


  - [02-main-results](https://github.com/NetDem-USC/homepage_experiment/blob/master/code/02-main-results) to replicate Figure 4 of the paper, which contains our main experimental findings.

<img src = "https://github.com/NetDem-USC/homepage-experiment/raw/main/graphs/main-fig4.png">

  - [03-descriptive-statistics](https://github.com/NetDem-USC/homepage_experiment/blob/master/code/03-descriptive-statistics) to replicate the analysis in the appendix regarding panel attrition, the creation of the issue scales and the agenda-setting metrics, as well as the tables with descriptive statistics.

  - [04-compliance](https://github.com/NetDem-USC/homepage_experiment/blob/master/code/04-compliance) to replicate the analysis in the appendix regarding compliance with encouragement.

  - [05-full-results](https://github.com/NetDem-USC/homepage_experiment/blob/master/code/05-full-results) to replicate the analysis in the appendix that estimates the Naive, ITT, and CACE treatment effects of our experiment.

  - [06-additional-results](https://github.com/NetDem-USC/homepage_experiment/blob/master/code/06-additional-results) to replicate the analysis in the appendix with treatment effects with outcomes on later waves.

  - [07-other-appendix-graphs](https://github.com/NetDem-USC/homepage_experiment/blob/master/code/07-other-appendix-graphs) to replicate the analysis in the appendix that created additional graphs; e.g. with heterogeneous treatment effects.

  - [08-treatment-group-comparisons](https://github.com/NetDem-USC/homepage_experiment/blob/master/code/08-treatment-group-comparisons) to replicate the analysis in the appendix with all treatment effects, but comparing the two treatment groups instead of each treatment group with respect to the control group.

 The output of running all of our code, along with detailed information on the version of R and all required packages we used, can be found in the `log` folder (see `01-first-stage.log` for more information).
