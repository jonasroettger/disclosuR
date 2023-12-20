---
title: 'DisclosuR: R Package for Processing Firm Press Releases and Earnings Calls from Nexis Uni' 
tags:
  - R
  - Management Science 
  - Natural Language Processing
authors:
  - name: Jonas Röttger
    orcid: 0009-0001-2666-4441
    equal-contrib: true
    affiliation: '1'
  - name: Rick Aalbers
    orcid: 0000-0002-9461-7591
    equal-contrib: true 
    affiliation: '1'
affiliations:
 - name: Radboud University
   index: '1'
date: 20 December 2023
bibliography: paper.bib

---

# Summary
Research on firm strategy and executive personality often utilizes press releases and earnings call transcripts. However, accessing these data sources from Nexis Uni often results in unstructured formats unsuitable for detailed statistical analysis. To address this, we developed an R package that converts these documents from PDFs into structured data frames in R. This tool enhances reproducibility in strategic management research and allows for advanced text analysis, including speaker-specific language analysis in earnings calls and analysis of temporal communication patterns in press releases. 

# Statement of need
The growing interest in textual analytics has spurred management research using company news and conference call transcripts [@konig2018silver]. This has led to insights like uncovering market manipulations and approximating CEO personalities [@harrison2019measuring, @pan2018straight]. Despite its value, the full potential of text data remains underutilized due to a lack of field-specific analytical tools, impacting reproducibility and analytical standards [@bergh2017credibility]. The rise of programming languages for statistical computing offers tailored text data analysis [@culpepper2011revolution], but this customization often leads to unique solutions that may affect data comparability and reproducibility.

Addressing this, our paper introduces the R package disclosuR, designed to standardize the analysis of press-wire news and conference call transcripts. It provides functions for converting earnings calls transcripts from PDF to structured data frames in R, ensuring consistent analysis across studies. The package supports various text analyses, including sentiment analysis and the identification of strategic and personality-related language patterns. This unified approach enhances comparability and reproducibility in management research.

# Data collection
## Nexis Uni
Press releases and earnings call transcripts, critical for firm communication research, are accessible through data providers like Nexis Uni, Factiva, and Seeking Alpha [@pan2018straight, @pollock2023celebrity]. We focus on Nexis Uni, often available in educational institutions, as it offers comprehensive news wire data and conference call transcripts. This platform is instrumental for analyzing firm communication and executive language patterns, providing insights into strategies and personality traits [@pan2018straight, @malhotra2018acquisitive].

Nexis Uni allows for targeted searches of news and press releases by firm, industry, or topic, and retrieval of conference call transcripts, including executive remarks and analyst interactions. It's valuable for studying communication strategies, stakeholder interactions, and the impact of significant events on firm narratives.

The platform's keyword-based search and advanced filters facilitate tailored research. Scholars frequently use it to access earnings call transcripts from FairDisclosure and analyze organizational communication from Business Newswire and PR Newswire [@gamache2019impression, @guo2020impact]. Our package disclosuR streamlines preprocessing these data types, enabling new research avenues in firm communication analysis.

## Data Preparation
### Conference call analysis
The FairDisclosure outlet offers corporate earnings call transcripts in PDF format. Analyzing these large volumes of data is challenging, particularly for researchers extracting specific details. The disclosuR package addresses this with the conference_call_segmenter function. It efficiently converts PDF transcripts into structured R data frames, with each quote in a separate row and additional details like speaker's name, role, company, and call section.

```r
# Install `discosuR` from CRAN
install.packages("disclosuR")
library(disclosuR)
```

```r
# Example of how to apply the `conference_call_segmenter` function
df_earnings_call <- conference_call_segmenter(file = "example_earnings_call.pdf")
```

Researchers can leverage the data frame created by the disclosuR package to delve into corporate communication strategies and financial performance using various text analysis techniques. The conference_call_segmenter function includes optional arguments like sentiment, which, when set to TRUE, utilizes the \CRANpkg{SentimentAnalysis} [@feuerriegel2018sentimentanalysis] for sentiment scoring. This feature incorporates three renowned dictionaries - Harvard-IV, Henry’s Financial [@henry2008influenced], and Loughran-McDonald Financial [@loughran2011liability] - to provide nuanced sentiment analysis. This enriches strategic data science research by facilitating deeper insights from corporate communications.

```r
# Output of the `conference_call_segmenter` function 
```

|                 | 1               | 2               | 3               | 
|-----------------|-----------------|-----------------|-----------------|
| year            | 2015            | 2015            | 2015            |
| quarter         | Q4              | Q4              | Q4              |
| date            | 2015-02-12      | 2015-02-12      | 2015-02-12      |
| weekday         | Thursday        | Thursday        | Thursday        |
| section         | presentation    | presentation    | presentation    |
| speaker_name    | OPERATOR        | ZAHEED MAWANI   | DARREN JACKSON  |
| speaker_role    | OPERATOR        | VP of IR        | CEO             |
| speaker_company | OPERATOR        | Advance Auto... | Advance Auto... |
| host_company    | OPERATOR        | Advance Auto... | Advance Auto... |
| host_CEO        | 0               | 0               | 1               |
| host_CFO        | 0               | 0               | 0               |
| host_Chairman   | 0               | 0               | 0               |
| quote_index     | 1               | 2               | 3               |
| quote           | Welcome to the...| Good morning,...| Thank you, Zah...|
| ...             | ...             | ...             | ...             |


### Press release analysis
Researchers are increasingly analyzing press releases for insights into firms' strategic communications [@aalbers2021market]. These analyses reveal patterns and motivations in corporate actions like acquisitions and leadership changes. For instance, press releases are used to study impression management in acquisition announcements [@graffin2016ready] and to understand messaging around leadership transitions.

To aid this research, we created the newswire_segmenter function in our R package. This function converts PDF newswire documents into structured R data frames. Each article becomes a row in the data frame, with details like title, date, and text. It offers optional analyses, including sentiment, emotion, regulatory focus, laughter, narcissism, and text clustering. The function processes the PDF, removing line breaks and extra spaces, then identifies the newswire source and extracts key information. This tool simplifies the analysis of press releases, providing a robust method for researchers to explore strategic communication in the business world. 


```r
# Example of how to apply the `newswire_segmenter` function 
df_newswire <- newswire_segmenter(file = "example_newswire.pdf", 
                                  sentiment = TRUE)
```
The newswire_segmenter function performs optional analyses tailored to the researchers' requirements. For sentiment analysis, it employs the analyzeSentiment function to assign sentiment scores to each quote [@feuerriegel2018sentimentanalysis]. Emotion analysis is conducted using the get_nrc_sentiment function [@mohammad2013nrc], which assesses emotional content. Regulatory focus is analyzed by counting words that signal promotion and prevention focus, guided by a specific dictionary [@gamache2015motivated]. Laughter analysis quantifies instances of laughter in the text, while narcissism analysis evaluates the use of pronouns, particularly the ratio of first-person singular to first-person plural pronouns, to gauge narcissistic tendencies [@zhu2015narcissism]. This suite of analyses enriches the understanding of the nuances in press release texts.

```r
# Output of the `newswire_segmenter` function 
```

| Data Category                     | Value                                                                                                                                                   |
|-----------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|
| Title                             | Advance Auto Parts Announces Closing of Acquisition of BWP Distributors                                                                                  |
| Newswire                          | Business Wire                                                                                                                                           |
| Date                              | 2015-12-31                                                                                                                                              |
| Text                              | Advance Auto Parts Announces Closing of Acquisition of BWP Distributors Business Wire December 31, 2012 Monday 9:15 PM GMT Copyright 2012 Business Wire... |
| Weekday                           | Monday                                                                                                                                                  |
| WordCount                         | 355                                                                                                                                                     |
| SentimentGI                       | 0.1408451                                                                                                                                               |
| NegativityGI                      | 0.03943662                                                                                                                                              |
| PositivityGI                      | 0.1802817                                                                                                                                               |
| SentimentHE                       | 0.01690141                                                                                                                                              |
| NegativityHE                      | 0.005633803                                                                                                                                             |
| PositivityHE                      | 0.02253521                                                                                                                                              |
| SentimentLM                       | 0.02253521                                                                                                                                              |
| NegativityLM                      | 0.03380282                                                                                                                                              |
| PositivityLM                      | 0.05633803                                                                                                                                              |
| RatioUncertaintyLM                | 0.02816901                                                                                                                                              |
| SentimentQDAP                     | 0.09295775                                                                                                                                              |
| NegativityQDAP                    | 0.01971831                                                                                                                                              |
| PositivityQDAP                    | 0.1126761                                                                                                                                               |

# Acknowledgements
As part of the FINDER program this project has received funding from the European Union's Horizon 2020 research and innovation programme under the Marie Skłodowska-Curie grant agreement No 813095. The funders had no role in study design, data collection and analysis, decision to publish, or preparation of the manuscript.

# References
