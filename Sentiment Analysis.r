################
# 0. Prepare R #
################

# 0.1 Load libraries
library(tm)
library(ggplot2)
library(dplyr)
library(syuzhet)
library(gridExtra)
library(wordcloud)
library(magrittr)
library(SnowballC)
library(ggrepel)

# 0.2 Set options
options(stringsAsFactors = FALSE)

# 0.3 Set seed
set.seed(9370707)

# 0.4 Set working directory
set.wd("C:\\Users\\Guillaume\\Documents\\No Man's Sky")
















##########################
# 1. Prepare data tables #
##########################

# 1.1 Import data extracted using Google Big Query
Reddit.Comments = read.csv("results-20170401-014241.csv", stringsAsFactors = FALSE)

# 1.2 Transform the date strings to datetime objects
Reddit.Comments$created_utc.2 = as.POSIXct(Reddit.Comments$created_utc, origin = '1970-01-01 00:00.00 UTC')

# 1.3 Read list of positive/negative words
Negative.Words = read.table("negative-words.txt", skip =35)
Positive.Words = read.table("positive-words.txt", skip =35)

# 1.4 Stem both lists of words
Stemmed.Negative.Words = tm_map(Negative.Words$V1 %>% VectorSource() %>% Corpus(), stemDocument) %>% .$content
Stemmed.Positive.Words = tm_map(Positive.Words$V1 %>% VectorSource() %>% Corpus(), stemDocument) %>% .$content

# 1.5 Count the number of comments per week
Reddit.Comments.Agg = 
	Reddit.Comments %>%
	mutate(
		week = as.Date(cut(created_utc.2, breaks="week")) #Cut dates by week
	) %>%
	group_by(week) %>%
	summarise(count = n())

# 1.6 Manually create a data frame of important dates
Important_dates = 
	data.frame(
		Dates = as.Date(c('2016-03-03', '2016-05-25', '2016-07-29', '2016-08-09', '2016-08-18','2016-11-27')),
		Comment = c('Launch date\nannounced', 'Launch delayed', 'Leaked copies\nappear online', 'Official launch\ndate', 'First patch"', 'Game-changing patch:\n"Foundation update"')
	)

# 1.7 Manually create a data frame of important date ranges
df.ranges = 
	data.frame(
		date = 
			c(	'2016-01-01', '2016-05-25', '2016-05-25', '2016-07-29','2016-07-29', '2016-08-09', '2016-08-09', '2016-08-18', '2016-08-18', '2016-11-26',
				'2016-11-26','2017-01-01') %>% as.Date(),
		Col = 
			c('0', '0', '1', '1', '2', '2', '3', '3', '4', '4', '5', '5' ),
		stringsAsFactors = TRUE
	)	

# 1.8 Vector of date ranges separator
Ranges = 
	df.ranges$date %>%
	unique() %>%
	as.POSIXct()
	
# 1.9 Get the comments' sentiment using the syuzhet package
nrc_data = get_nrc_sentiment(Reddit.Comments$body)
	
# 1.10 Get the mood of the comments by positive - negative words
Reddit.Comments.2 = 
	Reddit.Comments %>%
	mutate(valence = sign(nrc_data$positive - nrc_data$negative))	
	
	
	
	
	
	
	
	
	
	
	
	
	
####################################
# 2. Plot the introduction figures #
####################################
	
# 2.1 Plot the count of comments per week
ggplot(data = Reddit.Comments.Agg) + 
	geom_bar(aes(x = week, y = count), stat = "identity", color = 'black', alpha = 0.8, fill = '#008837') + 
	geom_vline(data = Important_dates, aes(xintercept = as.numeric(Dates)), linetype = 2, size = 1.2, color = '#7b3294') +
	geom_label_repel(data = Important_dates, aes(x = Dates, y = 1750, label = Comment), alpha = 0.8, nudge_y = 200, nudge_x = -10)+
	scale_x_date(date_breaks = "1 month", minor_breaks  = NULL, date_labels = "%b %y", name = "Date", expand = c(0, 0.005)) + 
	scale_y_continuous(minor_breaks  = (0:30) * 100, expand = c(0.005, 0), name = 'Number of Comments') +
	theme_bw()+
	theme(
		panel.border = element_blank(),
		axis.line = element_line(colour = "black"),
		plot.margin = margin(5,5,5,5),
		panel.grid.minor.y = element_line( linetype = 'dashed', color = 'grey90'),
		panel.grid.major.y = element_line( linetype = 'dashed', color = 'grey70'),
		panel.grid.minor.x = element_blank(),
		panel.grid.major.x = element_blank()
	)

# 2.2 Plot the evolution of the valence over time	
ggplot() + 	
	geom_vline(data = Important_dates, aes(xintercept = as.numeric(Dates)), linetype = 2, size = 1.2, color = '#7b3294', alpha = 0.5) +
	geom_smooth(data = Reddit.Comments.2, aes(x = as.Date(created_utc.2), y = valence), method = 'loess', span = 0.4, se = FALSE, color = '#008837', size = 1.3)+
	geom_label_repel(data = Important_dates, aes(x = Dates, y = 0.25, label = Comment), alpha = 0.5, nudge_y = 0.04, nudge_x = -10)+
	scale_y_continuous(name = 'Comments polarity   (mood)', breaks = c(0.1, 0.31), labels = c("negative :-(", "positive :-)"))+
	scale_x_date(date_breaks = "1 month", minor_breaks  = NULL, date_labels = "%B %y", name = "Date", expand = c(0, 0.005))+
	coord_cartesian(xlim = as.Date(c('2016-01-01', '2016-12-31')), ylim = c(0.07, 0.33))+
	scale_fill_brewer(palette = "Set1")+
	theme_bw()+
	theme(
		panel.border = element_blank(),
		axis.line = element_line(colour = "black"),
		plot.margin = margin(5,5,5,5),
		panel.grid = element_blank(),
		axis.text.y = element_text(angle = 90, hjust=0.5),
	)

# 2.3 Plot for the combined graph highlighting sections
png('Comments separation.png', width = 700, height = 480)
g = 
	ggplot() +
	scale_x_date(date_breaks = "1 month", minor_breaks  = NULL, date_labels = "%B %y", name = "Date", expand = c(0, 0.005))+
	coord_cartesian(xlim = as.Date(c('2016-01-01', '2016-12-31')))+
	scale_fill_brewer(palette = "Set1")+
	theme_bw() + 
	theme(
		legend.position="none",
		axis.text.y = element_text(angle = 90, hjust=0.5),
		panel.grid = element_blank(),
		panel.border = element_blank()
	)+
	geom_ribbon(data = df.ranges, aes(x = date, ymin = 0, ymax = Inf, fill = Col), alpha = 0.60)
	
g1 = 
	g + 		
	geom_smooth(data = Reddit.Comments.2, aes(x = as.Date(created_utc.2), y = valence), method = 'loess', span = 0.4, se = FALSE, color = 'grey30', size = 1.2)+
	scale_y_continuous(name = 'Comments polarity', minor_breaks = seq(0.1,0.23, length.out = 4) ,  breaks = c(0.1, 0.23), labels = c("negative :-(", "positive :-)"))+
	coord_cartesian(ylim = c(0.07, 0.25))+
	theme(
		axis.ticks.x = element_blank(),
		axis.text.x = element_blank(),
		axis.title.x = element_blank(),

		axis.ticks.y = element_blank(),
		
		panel.grid.minor.y = element_line( linetype = 'dashed', color = 'grey70'),
		panel.grid.major.y = element_line( linetype = 'dashed', color = 'grey70')

	)
	
g2 = 
	g +
	geom_bar(data = Reddit.Comments.Agg, aes(x = week, y = count), stat = "identity", color = 'black') + 
	scale_y_continuous(minor_breaks  = (0:15) * 200, expand = c(0.005, 0), name = 'Number of comments per day') +
	theme(
		panel.grid.minor.y = element_line( linetype = 'dashed', color = 'grey90'),
		panel.grid.major.y = element_line( linetype = 'dashed', color = 'grey70')
		
	)	
grid.arrange(g1,g2)
dev.off()

	
	
	
	
	
	
	
	
	
	
	

#################################################################################
# 3. Prepare text for the word clouds and text analysis for individual sections #
#################################################################################

# 3.1 Create text cleaning functions
RemoveWeirdWords	= function(x) gsub("[Ââ€˜Ã]", "", x, perl = TRUE)
CleanWords 			= function(x) removeWords(x, c(stopwords("english"), 'ive'))
RemoveWebsite 		= function(x) gsub("(f|ht)tp(s?)://([^)\\s\\r])*", "", x, perl = TRUE)

# 3.2 Clean + stem the words in the comments and only keep one copy of each word
Reddit.Comments$body2 =
	Reddit.Comments$body %>%
	VectorSource() %>%
	Corpus() %>%
	tm_map(
		Text,
		FUN = tm_reduce,
		tmFuns = 
			list(
				stripWhitespace,
				stemDocument,
				removeNumbers,
				removePunctuation,
				CleanWords,
				RemoveWeirdWords,
				RemoveWebsite,
				tolower
			)
	) %>%
	TermDocumentMatrix(
		control = list(weighting = weightBin) #Indicator if the word appeared
	) %>%
	as.matrix() %>%
	apply(2,function(x){ names(x[x==1]) %>%
	paste(collapse = ' ')})
	
# 3.3 Group comments by period and use the TfIdf weighting algorithm to create a document-term matrix
DDD.matrix = 
	Reddit.Comments %>%
	mutate(Date_Range = cut(created_utc.2, Ranges)) %>%
	group_by(Date_Range) %>%
	summarise(body3 = paste0(body2, collapse = ' ')) %>%
	{.$body3} %>%
	VectorSource() %>%
	Corpus() %>%
	TermDocumentMatrix(
		control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), bounds = list(global = c(4,Inf)))

	) %>%
	as.matrix()
	
# 3.4 A document-term matrix only for positive and negative words
DDD.matrix.polarity = 
	DDD.matrix %>%
	{.[rownames(.)%in% c(Stemmed.Positive.Words,Stemmed.Negative.Words),]}

# 3.5 Assign the green color to positive words and the red color to negative words
ColorVec = nrow(DDD.matrix.polarity) %>% character()
ColorVec[rownames(DDD.matrix.polarity) %in% Stemmed.Negative.Words] = 'darkred'
ColorVec[rownames(DDD.matrix.polarity) %in% Stemmed.Positive.Words] = 'forestgreen'
	
	







###########################
# 4. Plot the word clouds #
###########################
	
# 4.1 Function that creates word clouds
SuperWordcloud = function(timerange, expo) {

	# 4.1.1 Adjust the color so smaller words are more transparent
	br = 
		brewer.pal(6,"Set1")[timerange] %>%
		rep(300) %>% 
		{sapply(seq(.), function(x) adjustcolor(.[x], alpha.f = (x/300)^0.3 ))}

	# 4.1.2 Actual word cloud
	wordcloud(
		rownames(DDD.matrix),
		DDD.matrix[,timerange]^expo,
		scale=c(3.5,.1),
		random.order = FALSE, #necessary for the color vector to work
		max.words=50,
		color = br
	)
}

# 4.2 Creates the png file
png('wordcloud.png', width = 720, height = 480)

# 4.3 Separates the figure into 6 clouds + 6 titles
layout(matrix(c(1:3,7:9,4:6,10:12), nrow = 4, byrow = TRUE), heights=c(1, 6, 1, 6))
#layout.show(12) # to visualize where each plot goes where

# 4.4 No margin
par(mar=rep(0, 4))

# 4.5 Plot the titles -> dates
plot.new(); text(x=0.5, y=0.2, "January 1 - May 24")
plot.new(); text(x=0.5, y=0.2, "May 25 - July 28")
plot.new(); text(x=0.5, y=0.2, "July 29 - August 8")
plot.new(); text(x=0.5, y=0.2, "August 9 - August 18")
plot.new(); text(x=0.5, y=0.2, "August 19 - November 26")
plot.new(); text(x=0.5, y=0.2, "November 27 - December 31")

# 4.6 Plot the actual word cloud
SuperWordcloud(1,1.1)
SuperWordcloud(2,0.6)
SuperWordcloud(3,0.5)
SuperWordcloud(4,1.1)
SuperWordcloud(5,0.9)

# 4.7 the sixth figure needs manual adjustements because the color yellow is hard to see 
xx = 6
br = 
	brewer.pal(6,"Set1")[xx] %>%
	c('#000000') %>%
	{colorRampPalette(colors = .)(8)[4]} %>%
	rep(300) %>% 
	{sapply(seq(.), function(x) adjustcolor(.[x], alpha.f = (x/300)^0.3 ))}

wordcloud(
	rownames(DDD.matrix),
	DDD.matrix[,xx]^0.85,
	scale=c(3.5,.1),
	random.order = FALSE,
	max.words=50,
	color = br
)

dev.off()


# 4.8 Function that creates sentiments word clouds
SuperWordcloud2 = function(timerange, expo)  {
	wordcloud(
							rownames(DDD.matrix.polarity),
							DDD.matrix.polarity[,timerange]^expo,
		scale 			= 	c(5,0.1),
		random.order 	= 	FALSE,
		max.words 		= 	30,
		color 			= 	ColorVec,
		ordered.colors  = 	TRUE
	)	

}

# 4.9 Creates the png file
png('Sentiment wordcloud.png', width = 720, height = 480)

# 4.10 Separates the figure into 6 clouds + 6 titles
layout(matrix(c(1:3,7:9,4:6,10:12), nrow = 4, byrow = TRUE), heights=c(1, 6, 1, 6))
#layout.show(12)

# 4.11 No margin
par(mar=rep(0, 4))

# 4.12 Plot the titles -> dates
plot.new(); text(x=0.5, y=0.2, "January 1 - May 24")
plot.new(); text(x=0.5, y=0.2, "May 25 - July 28")
plot.new(); text(x=0.5, y=0.2, "July 29 - August 8")
plot.new(); text(x=0.5, y=0.2, "August 9 - August 18")
plot.new(); text(x=0.5, y=0.2, "August 19 - November 26")
plot.new(); text(x=0.5, y=0.2, "November 27 - December 31")

# 4.13 Plot the actual word cloud
SuperWordcloud2(1,1.4)
SuperWordcloud2(2,0.6)
SuperWordcloud2(3,0.5)
SuperWordcloud2(4,1.1)
SuperWordcloud2(5,0.8)
SuperWordcloud2(6,1.3)

dev.off()


############################################################
# 5. Create a csv of top words and commentaries associated #
############################################################
	
# 5.1 Get the top words with Tf-Idf
top.words = apply(DDD.matrix, 2, function(x) row.names(DDD.matrix)[order(-x)][1:10])

# 5.2 Get the periods 
Reddit.Comments.byrange = 
	Reddit.Comments %>%
	mutate(Date_Range = cut(created_utc.2, Ranges, labels = 1:6))
	
# 5.3 Loop over the top words for each period
df.top.words = data.frame()

for(h in 1:ncol(top.words)){ 
	for(v in 1:nrow(top.words)){ 
		unstemmedword = paste0('\\b', gsub('i', '(i|y)', top.words[v,h]))
		df.top.words = 
			rbind(
				df.top.words,
				data.frame(
					word = top.words[v,h],
					Date_Range = h,
					comment = Reddit.Comments.byrange %>% filter(Date_Range == h, grepl(unstemmedword, body, ignore.case = TRUE)) %>% {.$body}
				)
			)
	}
}
	
# 5.4 Output the table
write.csv(df.top.words, "topwords.csv")