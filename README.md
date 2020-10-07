# covid19-v2

This is a major rewrite and update of the orignal [covid-19 project](https://github.com/robhanssen/covid-19).

* Data analysis is more centered around a single JHU dataset, so the input file only has to be read once. This will offer some speed-up of the code execution
* Location selection has been completely relegated to CSV input files instead of modifying the code.
* The US has been split into the usual regions instead of looking at individual states.
* There is a new view by continent, both for spread and growth.
* All views involving time have been moved to show dates instead of the 'days from Jan 22'.

To be implemented:
* Deaths and infections per unit of population