#******Natural Language Processing using R******#


#1. STRING MANIPULATION AND REGULAR EXPRESSION

#a. Basic string manipulation using base R
text <- "north carolina"
typeof(text)

num<- c("2018", "2019", "2020")
typeof(num)

ml<- paste("nlp", "deepl", sep="-")
ml

paste(1:5, c("nlp", "deepl"), sep="-")

#print and concatenate strings without quotes
cat(text, "Durham", sep="-")

cat(month.name[1:5], sep=" ")

#convert non-character value to a string using toString
toString(1:10)

#b. Using base r and stringr library for string manipulation on text data
library(stringr)
string <- "Duke U in North Carolina was created in 1924 by James Buchanan Duke as a memorial to his father, Washington Duke. 
The Dukes, a Durham family that built a worldwide financial empire in the manufacture of tobacco products 
and developed electricity production in the Carolina, long had been interested in Trinity College. In December 1924, 
the provisions of indenture by Benjamin’s brother, James B. Duke, created the family philanthropic foundation, 
The Duke Endowment, which provided for the expansion of Trinity College into Duke U." 

strwrap(string)

#count number of characters
nchar(string)
str_length(string)

#convert to lower
tolower(string)
str_to_lower(string)

#convert to upper
toupper(string)
str_to_upper(string)

#replace strings
chartr("and", "for", x=string) #letters a, n, d replaced by f, o, r
str_replace_all(string=string, pattern=c("U"), replacement="University") # this is case sensitive

#extract parts of a string
substr(x=string, start=5, stop=25)

#get difference between two vectors
setdiff(c("ml", "deepl", "ai"), c("ml", "iot", "cloudcomp"))

#check if strings are equal
setequal(c("ml", "deepl", "ai"), c("ml", "deepl", "ai"))
setequal(c("ml", "deepl", "ai"), c("ml", "deepl", "iot"))

#abbreviate strings
abbreviate(c("university", "college", "department"), minlength=4)

#split strings
strsplit(x=c("Nov 10", "Dec 21", "Jan 6", "Feb 18"), split=" ")
str_split(string=c("Nov 10", "Dec 21", "Jan 6", "Feb 18"), pattern=" ", simplify=T)

#find and replace matches
sub(pattern="C", replacement="K", x=string, ignore.case=T) #replace first match only
gsub(pattern="Carolina", replacement="Karolina", x=string, ignore.case=T) #replaces all matches

#c. Regular Expressions detection
#c.1 Metacharacters (use both grep and gsub function with double backslash to detect the characters)

ws<-c("percent%", "percent")
grep(pattern="percent\\%", x=ws, value=T)

ws<-c("nlp?", "ai$", "ml&")
grep(pattern="[a-z][\\?-\\$-\\&]", x=ws, value=T)

gsub(pattern="[\\?-\\$-\\&]", replacement=" ", x=ws)

#c.2 Quantifiers
number <- "1010000000010"

regmatches(number, gregexpr(pattern="1.*1", text=number))

regmatches(number, gregexpr(pattern="1.?1", text=number))

states <- c("virginia", "florida", "pennsylvania", "tennessee", "texas")

#must match i one or more times
grep(pattern= "i+", x=states, value=T)

#must match n two times
grep(pattern="n{2}", x=states, value=T)

#c.3 Sequences
mqm<- "The MQM program began 2 years ago"

#match a digit
gsub(pattern="\\d+", replacement="_", x=mqm)
regmatches(mqm, regexpr(pattern="\\d+", text=mqm))

#match a non-digit
gsub(pattern="\\D+", replacement="_", x=mqm)
regmatches(mqm, regexpr(pattern="\\D+", text=mqm))

#match a space - returns positions
gregexpr(pattern="\\s+", text=mqm)

#match a non space
gsub(pattern="\\S+", replacement="app", x=mqm)

#match a word character
gsub(pattern="\\w", replacement="k", x=mqm)

#match a non-word character
gsub(pattern="\\W", replacement="k", x=mqm)

#c.4 Character Classes
mqm <- "The MQM program is for 10 months. Students can choose 4 different tracks."

#extract numbers
regmatches(x=mqm, gregexpr("[0-9]+", text=mqm))

#extract without digits
regmatches(x=mqm, gregexpr("[^0-9]+", text=mqm))

#POSIX character classes
mqm <- c("There are over 200 students\n, in the MQM program", "The students\n can choose 4 tracks.", "The program is for\t 10 months?")

#get digits
unlist(regmatches(mqm, gregexpr("[[:digit:]]+", text=mqm)))

#remove punctuations
gsub(pattern="[[:punct:]]+", replacement=" ", x=mqm)

#remove spaces
gsub(pattern="[[:blank:]]", replacement="-", x=mqm)

#remove control characters
gsub(pattern="[[:cntrl:]]+", replacement="", x=mqm)

#remove nongraphical characters
gsub(pattern="[^[:graph:]]+", replacement="", x=mqm)









  