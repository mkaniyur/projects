# Project Studio

WEB SCRAPING & ANALYSIS CODE FILES

"WSJ Web Scraper.py" & "NYT Web Scraper.py"
Scraped their respective websites (the API for NYT or the direct website for WSJ) for a complete list of headlines from the winter of 2020-2021, assigned each headline a valence, and exported the data into "WSJ.txt" and "NYT.txt" respectively. WARNING: RUNNING THESE FILES WILL RE-EXPORT THE DATA INTO FILES NAMED NYT.txt or WSJ.txt. If your files NYT.txt or WSJ.txt already contain the full data set, it will write the headlines again into the file and you will have duplicates.

"WSJ Analyzer.py" & "NYT Analyzer.py"
For their respective newspaper text files (NYT.txt or WSJ.txt), this will sort headlines into positive, negative, and neutral. It has functions that can return the 20 most frequent words found in each of those categories, and report the average valence of any given word.

"WSJ Distributions.py" & "NYT Distributions.py"
This contains the appropriate code for copying the pair of valence and date data for each headline containing some specified word into a file named "newspaper_word.txt" (ex. "NYT_biden.txt" or "WSJ_covid.txt"). Copies all headlines from a newspaper with a word in the following format: Each headline takes up one line, with date first (counted not by date as before, but ascending from October 1, which is "Day 1") and valence second, separated by a tab. Note that the actual headline is not copied over, only date valence pairs. The results of this code can be found in the "PS Distribution Analyses Text Files". WARNING: RUNNING THESE FILES WILL RE-EXPORT THE DATA INTO FILES. This information is then used to make the R graphs.

TEXT & IMAGE FILES CONTAINING DATA

"NYT.txt"
Contains all headlines from the New York Times from October to January (inclusive) scraped from the NYT API. Format is as follows. Each headline takes up 3 lines in the file, first line is headline, second line is date, third line is valence value.

"WSJ.txt"
Contains all headlines from the Wall Street Journal October to January (inclusive) scraped from the WSJ website. Format is as follows. Each headline takes up 3 lines in the file, first line is headline, second line is date, third line is valence value.

Note: NYT dates are formatted like "2020-10-01" and WSJ dates are formatted like "2020/10/01"

"PS Distribution Analyses Text Files"
Each file contains a subset of headline data copied over from the main "NYT.txt" or "WSJ.txt" files. A file named "WSJ_word.txt" will have the data for all headlines from the WSJ containing word in the following format: Day (counted not by date as before, but ascending from October 1, which is "Day 1"), tab, and valence value. The first line has no headline data, but rather "Date", tab, and "Val". This will automatically give the columns names when the file is read into R as a dataframe. Similarly for a file titled "NYT_word.txt"

"PS Distribution Analyses Pics"
Each file, named either "WSJ_word.png" or "NYT_word.png", shows the R scatter plot of all headlines from the specified newspaper containing the specified word, with the y axis representing valence, and the x axis representing date.
