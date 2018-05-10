**Project Description**
How does the sentiment vary between news outlets portraying the same events?  

**Authors**
Henry Knopf, Richard Pham, Emerson Webb

**Timeline**
- 3/29: Proposal Due
- *interval* : __work__
- 5/10: Final Presentation


**Higher-level steps**
1. access API called newsapi to get metadata (urls and their corresponding news sources and dates);
    please see `url_retrieve.R`
2. gather raw text data; please see `html_text_extract.R`
3. parse data, store this in folder `shiny_data`.
4. Run app, which retrieves local data from `shiny_data` to plot according to user-specifications. 
