## Read the tetris.csv file into the environment.
tetris  = read.csv ("tetris.csv")

## We should check the head and tail to see what our columns are.
head(tetris)
tail(tetris)

## Data cleaning: It looks like we have a column for IDs just called ID, let's get rid of that weird ID column as we won't need it.
tetris$ID = NULL

## We have a condition column. Use the factor() command to factorize it, then check its levels with the levels command.
tetris$condition = factor(tetris$condition)
levels (tetris$condition)

## It looks like we have two levels, as described. Use the subset command to make two subsets of scores, one for each condition. 
auditory = subset (tetris$score, tetris$condition=="auditory")
auditory

visual = subset (tetris$score, tetris$condition=="visual")
visual

## Load in the summary stats script and run summary stats on each subset.
source ('summarystats.R')
summary.stats(auditory)
summary.stats(visual)

## Make boxplots for each condition and for the scores overall.
boxplot (tetris$score,main="boxplot of gaming scores", ylim= c(0,100), ylab = "Score")
boxplot(tetris$score~tetris$condition, main="boxplot of gaming scores by condition", ylim= c(0,100),xlab = "condition", ylab = "Score")

## Make some histograms for each condition. Put two histograms, one for each condition, right on top of each other.
hist (auditory, xlab="Gaming Scores", main="Gaming scores for visual (green) and visual (auditory) conditions", xlim=c(0,100), breaks=seq(0,100,10), col=rgb(1,.5,0,1/3))
hist (visual, breaks=seq(0,100,10), col=rgb(0,1,0,1/3), add=TRUE, )
legend("topleft",c("Auditory","Visual"),fill=c(rgb(1,.5,0,1/3),rgb(0,1,0,1/3)))

## we'll use a statistical test, the Shapiro-Wilk's test, to say for sure whether these distributions are normal. 
## The shapiro.test command runs this test.
shapiro.test (auditory)
shapiro.test (visual)

## Install the "car" package so we can use Levene's test to test for homogeneity of variance, our other assumption.
install.packages ('car')
library ('car')
leveneTest(tetris$score, tetris$condition)

##As we might have guessed from the histograms and box plots, the variance isn't
##quite the same between conditions, but it's close enough that our Levene's
##test also just barely misses being significant. Since both assumptions are
##met, we can run our t-test.

## The t.test command let's us specify our two data frames or vectors. As a third
## argument, we can tell R whether this is a paired (dependent means) t-test or
## not.
t.test (auditory, visual, paired=FALSE)

## The mean gaming score for the auditory condition (M = 67.94) was
## significantly greater than the mean gaming score for the visual
## condition (M = 46.63),  t(61.74) = 7.46, p < .001.

## Now that we've addressed our hypothesis, let's visualize it! Barplots with
## error bars are the best way to visualize a difference between two means. To
## make our barplots and error bars, we'll need to make new objects in our
## data frame that hold the means and SDs for both conditions.
tetris.mean = c( mean(auditory), mean(visual) )
tetris.sd = c( sd(auditory), sd(visual) )

## We'll also use the names command to label our means.
names(tetris.mean) = c("Auditory", "Visual")

## We'll use the barplot command to make our barplot. The first argument is our
## means. Like other plots, we can specify the aesthetic stuff like labels,
## titles, limits, and colors too.
barplot (tetris.mean, main = "Graph of Condition Means", xlab= "Gaming Condition", ylab="Gaming Score", ylim=c(0,100),col=c(col=rgb(1,.5,0,1),rgb(0,1,0,1)))

## Add error bars to the barplot.
se.bar = function(x, y, sds, n, upper=sds/sqrt(n), lower=upper, length=0.1,...)
{
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

## Add your barplot to your environment by making a new variable name and
## setting it equal to the barplot. Then run the se.bar command, specifying your
## barplot, your list of means, your list of SDs, and your N
br = barplot (tetris.mean, main = "Graph of Condition Means", xlab= "Gaming Condition", ylab="Gaming Score", ylim=c(0,100),col=c(col=rgb(1,.5,0,1),rgb(0,1,0,1)))

se.bar(br,tetris.mean,tetris.sd,32)







