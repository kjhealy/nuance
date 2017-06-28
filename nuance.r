###--------------------------------------------------
### Fuck Nuance with Data
###--------------------------------------------------

library(gtable)
library(tidyverse)
library(lubridate)
library(stringr)
library(splines)

theme_set(theme_minimal())

## Make a 'figures' subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")), dir.create(file.path("figures")), FALSE)


###--------------------------------------------------
### Unit names
###--------------------------------------------------

fields <- list("Sociology", "Economics", "Politics", "Anthropology", "History", "Philosophy",
    "Psychology")

journals <- c("AER", "ASR", "AJPS", "APSR", "AJS", "Demography", "JOP", "JPE", "QJE",
    "SF")

jl.names <- c("Amer. Economic Rev.", "Amer. Sociological Rev.", "Amer. Jl of Political Science",
    "Amer. Political Science Rev.", "Amer. Jl Sociology", "Demography", "Jl of Politics",
    "Jl of Political Economy", "Quarterly Jl of Economics", "Social Forces")



###--------------------------------------------------
### Functions
###--------------------------------------------------

### Generates keyword data frame for a single keyword in a given unit
proc.unit <- function(unit = "Sociology", subdir = "terms", keyword = "nuance", ...) {

    ### Fix for AJS misclassifications
    fix.ajs <- function(x, ...) {

        ## First get all the AJS articles with the term in them
        fname <- file.path("data", "manual", paste0("ajs-control-", keyword, "-cites.csv"))
        ajs.df <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)

        ajs.df <- ajs.df %>% select(id, journaltitle, pubdate, pagerange)

        ajs.df$pubdate <- as.Date(ajs.df$pubdate)
        ajs.df$year <- year(ajs.df$pubdate)
        ajs.df$journaltitle <- factor(ajs.df$journaltitle)

        pages <- clean.pages(ajs.df$pagerange)

        ajs.df$startpage <- pages$startpage
        ajs.df$endpage <- pages$endpage
        ajs.df$length <- pages$length

        ## Tabulate how many 'articles' are actually book reviews
        tab.bookreviews <- ajs.df %>% group_by(year) %>% filter(length < 4) %>% group_by(year) %>%
            tally() %>% filter(year %in% c(1997:1999))
        colnames(tab.bookreviews) <- c("year", "bookrev.n")

        ## Tabulate how many real articles have the keyword
        tab.keyword <- ajs.df %>% group_by(year) %>% filter(length > 3) %>% tally() %>%
            filter(year %in% c(1997:1999))
        colnames(tab.keyword) <- c("year", "keyword.n")

        ## Uncorrected AJS article count for 97-99
        ajs.ac <- read.csv("data/article-counts/ac-AJS.csv")
        colnames(ajs.ac) <- c("year", "article.n")
        tab.uncorrected <- ajs.ac %>% filter(year %in% c(1997:1999))

        correct.counts <- merge(tab.uncorrected, tab.bookreviews)
        correct.counts$corr.article.n <- correct.counts$article.n - correct.counts$bookrev.n

        correct.counts <- merge(correct.counts, tab.keyword)

        x[, "articles"][x$year %in% correct.counts$year] <- correct.counts$corr.article.n
        x[, keyword][x$year %in% correct.counts$year] <- correct.counts$keyword.n
        return(x)
    }

    fn.articles <- file.path("data", "article-counts", paste0("ac-", unit, ".csv"))
    fn.term <- file.path("data", subdir, paste0(keyword, "-", unit, ".csv"))

    message("Term file path is ", fn.term, "; keyword is ", keyword)

    tmp1 <- read.csv(fn.articles)
    colnames(tmp1) <- c("year", "articles")

    tmp2 <- read.csv(fn.term)
    colnames(tmp2) <- c("year", keyword)

    data <- merge(tmp1, tmp2, all = TRUE)

    if (unit == "AJS") {
        data <- fix.ajs(data, ...)
    }


    ind <- which(is.na(data[, keyword]))
    data[, keyword][ind] <- 0
    data$rate <- round((data[, keyword]/data$articles), 3)
    data$unit <- unit
    return(data)
}

### raw pagerange column to df with start and end pages as numbers fixes page
### numbering inconsistency
clean.pages <- function(raw.pages) {
    pages <- str_replace_all(raw.pages, "S", "")  # supplements
    pages <- str_replace(pages, "pp. ", "")
    pages <- str_split_fixed(pages, "-", 2)
    pages <- data.frame(pages, stringsAsFactors = FALSE)
    colnames(pages) <- c("startpage", "endpage")

    ## AJS is sometimes inconsistent with page ranges e.g. saying 1125-27 instead of
    ## 1125-1127

    ## length(endpage) should not be shorter than length(startpage)
    p.start <- str_length(pages$startpage)
    p.end <- str_length(pages$endpage)
    p.diff <- p.start - p.end
    ind <- p.diff > 0  ## bad page ranges

    ## figure out where to cut the string
    s.width <- str_length(pages[ind, "startpage"])
    e.width <- str_length(pages[ind, "endpage"])
    cut.point <- s.width - e.width

    ## correct numeric prefix
    x <- substr(pages[ind, "startpage"], 1, cut.point)
    new.end <- paste0(x, pages[ind, "endpage"])

    pages[ind, "endpage"] <- new.end
    pages <- apply(pages, 2, as.numeric)
    pages <- data.frame(pages)
    pages$length <- pages$endpage - (pages$startpage - 1)
    return(pages)
}

### Apply proc.unit() to a vector of unit names for a given keyword, clean it up,
### and calculate the relative rate for the keyword
make.keyword.df <- function(unit.names, full.names = NULL, new.labels = FALSE, keyword = "nuance",
    subdir = "manual", ...) {

    out <- lapply(unit.names, proc.unit, keyword = keyword, subdir = subdir, ...)
    data <- bind_rows(out)

    if (new.labels == TRUE) {
        lab.lookup <- data.frame(unit.names, full.names)
        colnames(lab.lookup) <- c("old", "new")
        ind <- match(data$unit, lab.lookup$old)
        data$longlab <- factor(lab.lookup$new[ind])
    }

    ### Reorder for labeling purposes
    unit.tab <- data %>% group_by(unit) %>% filter(year > 2001) %>% summarize(Med = median(rate)) %>%
        ungroup() %>% arrange(desc(Med))

    data$unit <- factor(data$unit, levels = unit.tab$unit, ordered = TRUE)

    if (new.labels == TRUE) {
        ind <- match(unit.tab$unit, lab.lookup$old)
        data$longlab <- factor(data$longlab, levels = lab.lookup$new[ind], ordered = TRUE)
    }

    data.total <- proc.unit("Total", keyword = keyword, subdir = subdir, ...)
    colnames(data.total) <- c("year", "total.articles", paste0("total.", keyword),
        "total.rate", "total")

    ## Calculate difference relative to the base rate
    data <- merge(data, data.total)
    relative.keyword <- paste0("relative.", keyword)
    data$relative.keyword <- data$rate - data$total.rate

    return(data)
}


###--------------------------------------------------
### Nuance across the disciplines
###--------------------------------------------------

data.fields <- make.keyword.df(unit.names = fields, subdir = "terms")

### Basic field-level rates Too many fields for the shape aesthetic

pdf(file = "figures/nuance-rate-by-discipline.pdf", width = 9, height = 8)
p <- ggplot(subset(data.fields, year < 2014 & year > 1860), aes(x = year, y = rate,
    color = unit, fill = unit))

p + geom_point(alpha = 0.7) + geom_smooth(se = FALSE) + scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "right") + labs(x = "Year", y = "Journal Articles mentioning 'Nuance' or 'Nuanced'",
    fill = "Discipline", color = "Discipline") + ggtitle("Rates of Nuance Across the Disciplines, 1860-2013")
credit("Data: JSTOR.")
dev.off()

### Relative Rates

## For the in-plot labels
linelab.df <- data.frame(unit = c("Economics", "Psychology", "Philosophy", "Politics",
    "History", "Sociology", "Anthropology"), y.loess = c(-1, 1.1, 2.65, 3.75, 4.4,
    4.7, 5.75), y.gam = c(-0.8, 0.9, 2.7, 3.25, 4.2, 4.5, 5.1))



pdf(file = "figures/nuance-rate-by-discipline-relative.pdf", width = 9, height = 8.2,
    pointsize = 10)
p <- ggplot(subset(data.fields, year < 2014 & year > 1860), aes(x = year, y = relative.keyword *
    100, color = unit, fill = unit))

p1 <- p + geom_hline(yintercept = 0, color = "gray70") + geom_point(size = 0.5) +
    geom_smooth(se = FALSE) + geom_text(data = linelab.df, aes(x = 2014, y = y.loess,
    label = unit), hjust = 0, size = 3) + theme(legend.position = "right") + labs(x = "Year",
    y = "Percentage Point Difference from Base Rate", fill = "Discipline", color = "Discipline") +
    xlim(1860, 2020) + ylim(-1.5, 7.5) + guides(fill = FALSE, color = FALSE) + ggtitle("Nuance Across the Disciplines, 1860-2013")

## Turn off the clipping so the labels are legible
gt <- ggplot_gtable(ggplot_build(p1))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
credit("Kieran Healy. Data: JSTOR. Base rate is percent mentions across all research articles in the JSTOR corpus.",
    size = 0.5)
dev.off()


pdf(file = "figures/nuance-rate-by-discipline-relative-SE-jit-gam.pdf", width = 9,
    height = 9, pointsize = 10)
p <- ggplot(subset(data.fields, year < 2014 & year > 1860), aes(x = year, y = relative.keyword *
    100, color = unit, fill = unit))

p1 <- p + geom_hline(yintercept = 0, color = "gray20") + geom_jitter(size = 0.5,
    alpha = 0.8) + geom_smooth(method = "gam", formula = y ~ ns(x, 3)) + geom_text(data = linelab.df,
    aes(x = 2014, y = y.gam, label = unit), hjust = 0, size = 3) + theme(legend.position = "right") +
    labs(x = "Year", y = "Percentage Points Difference from Base Rate", fill = "Discipline",
        color = "Discipline") + xlim(1860, 2020) + guides(fill = FALSE, color = FALSE) +
    ggtitle("Relative Rates of Nuance Across the Disciplines, 1860-2013")

## Turn off the clipping so the labels are legible
gt <- ggplot_gtable(ggplot_build(p1))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
credit("")
dev.off()

###--------------------------------------------------
### Multiple journals
###--------------------------------------------------

data.journals <- make.keyword.df(unit.names = journals, full.names = jl.names, subdir = "terms",
    new.labels = TRUE)

pdf(file = "figures/nuance-rate-by-journal-relative-facet.pdf", width = 10, height = 5,
    pointsize = 10)
p <- ggplot(subset(data.journals, year < 2014 & year > 1899), aes(x = year, y = relative.keyword *
    100))

p1 <- p + geom_hline(yintercept = 0, color = "gray20") + geom_point(size = 0.5, alpha = 0.5) +
    geom_smooth(method = "gam", formula = y ~ ns(x, 3)) + facet_wrap(~longlab, ncol = 5) +
    labs(x = "", y = "Percentage Point Difference from Base Rate") + theme(axis.title.y = element_text(size = rel(0.6)),
    axis.text.y = element_text(size = rel(0.6)), axis.title.x = element_text(size = rel(0.6)),
    axis.text.x = element_text(size = rel(0.6)), plot.title = element_text(size = rel(0.8)),
    strip.text.x = element_text(size = rel(0.8))) + ggtitle("Nuance in Selected Social Science Journals, 1900-2013")

print(p1)
credit("Kieran Healy. Data: JSTOR. Base rate is percent mentions across all research articles in the JSTOR corpus.")
dev.off()

ggsave("figures/nuance-rate-by-journal-relative-facet.png", plot = p1, width = 10, height = 5)

###--------------------------------------------------
### Soc Journals
###--------------------------------------------------

soc.journals <- c("ASR", "AJS", "SF", "ST", "TS")
soc.names <- c("ASR", "AJS", "Social Forces", "Sociological Theory", "Theory and Society")

data.socjournals <- make.keyword.df(soc.journals, full.names = soc.names, subdir = "terms",
    new.labels = TRUE)

pdf(file = "figures/nuance-rate-by-soc-journal-relative-facet.pdf", width = 9, height = 3,
    pointsize = 10)
p <- ggplot(subset(data.socjournals, year < 2014 & year > 1949), aes(x = year, y = relative.keyword *
    100))

p1 <- p + geom_hline(yintercept = 0, color = "gray20") + geom_point(size = 0.5, alpha = 0.5) +
    geom_smooth(method = "gam", formula = y ~ ns(x, 3)) + facet_wrap(~longlab, ncol = 5) +
    labs(x = "Year", y = "Percentage Point Difference from Base Rate") + theme(axis.title.y = element_text(size = rel(0.8)),
    axis.text.y = element_text(size = rel(0.6)), axis.title.x = element_text(size = rel(0.8)),
    axis.text.x = element_text(size = rel(0.6))) + ggtitle("Nuance in Five Sociology Journals, 1950-2013")

print(p1)
credit("Kieran Healy. Data: JSTOR. Base rate is percent mentions across all research articles in the JSTOR corpus.")
dev.off()

ggsave("figures/nuance-rate-by-soc-journal-relative-facet.png", plot = p1, width = 8, height = 3)



soc.alljournals <- c("ASQ", "ASR", "AJS", "Demography", "JHSB", "JMF", "GS", "SF",
    "SM", "SPQ", "ST", "TS")

soc.names <- c("Administrative Sci. Q.", "ASR", "AJS", "Demography", "JHSB", "J. Marriage & Family",
    "Gender and Society", "Social Forces", "Soc. Methodology", "Soc. Psych. Q.",
    "Soc. Theory", "Theory and Society")


data.allsocjournals <- make.keyword.df(soc.alljournals, full.names = soc.names, new.labels = TRUE,
    subdir = "terms")

pdf(file = "figures/nuance-rate-by-allsoc-journal-relative-facet.pdf", width = 12,
    height = 6, pointsize = 10)

p <- ggplot(subset(data.allsocjournals, year < 2014 & year > 1959), aes(x = year,
    y = relative.keyword * 100))

p1 <- p + geom_hline(yintercept = 0, color = "gray20") + geom_point(size = 0.5, alpha = 0.5) +
    geom_smooth(method = "gam", formula = y ~ ns(x, 3)) + ylim(-10, 30) + facet_wrap(~longlab,
    ncol = 6) + labs(x = "", y = "Percentage Point Difference from Base Rate") +
    theme(axis.title.y = element_text(size = rel(0.8)), axis.text.y = element_text(size = rel(0.6)),
        axis.title.x = element_text(size = rel(0.8)), axis.text.x = element_text(size = rel(0.6))) +
    ggtitle("Nuance in Twelve Sociology Journals, 1960-2013")

print(p1)
credit("Kieran Healy. Data: JSTOR. Base rate is percent mentions across all research articles in the JSTOR corpus.")
dev.off()

ggsave("figures/nuance-rate-by-allsoc-journal-relative-facet.png", plot = p1, width = 12, height = 6)



###--------------------------------------------------
### Our Three Original Journals
###--------------------------------------------------

soc.journals <- c("ASR", "AJS", "SF")
soc.names <- c("American Sociological Review", "American Journal of Sociology", "Social Forces")

data.socjournals <- make.keyword.df(soc.journals, keyword = "nuance", full.names = soc.names,
    new.labels = TRUE)

pdf(file = "figures/nuance-rate-relative.pdf", width = 7, height = 7)

p <- ggplot(subset(data.socjournals, year < 2013), aes(x = year, y = relative.keyword,
    color = longlab, fill = longlab, shape = longlab))

p1 <- p + geom_jitter() + geom_smooth(se = FALSE) + scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "bottom", plot.title = element_text(size = rel(0.8))) +
    labs(x = "", y = "Percentage Point Difference from Base Rate", fill = "Journal",
        shape = "Journal", color = "Journal") + scale_color_manual(values = my.colors("bly")) +
    ggtitle("Changing Rates of Nuance in Three Journals")

print(p1)
credit("Data: JSTOR. Base rate is percent mentions across all research articles in the JSTOR corpus.")

dev.off()



###--------------------------------------------------
### Some control terms
###--------------------------------------------------

data.nuance <- make.keyword.df(soc.journals, full.names = soc.names, new.labels = TRUE,
    subdir = "terms", keyword = "nuance")


data.soph <- make.keyword.df(soc.journals, full.names = soc.names, new.labels = TRUE,
    subdir = "terms", keyword = "sophisticated")

data.subtle <- make.keyword.df(soc.journals, full.names = soc.names, new.labels = TRUE,
    subdir = "terms", keyword = "subtle")

data.subtlety <- make.keyword.df(soc.journals, full.names = soc.names, new.labels = TRUE,
    subdir = "terms", keyword = "subtlety")

## Across the disciplines
data.theory <- make.keyword.df(fields, subdir = "terms", keyword = "theory")



p <- ggplot(subset(data.soph, year < 2013), aes(x = year, y = rate, color = longlab,
    fill = longlab, shape = longlab))

pdf(file = "figures/sophisticated-rate.pdf", width = 7, height = 7)
p + geom_jitter() + geom_smooth(se = FALSE) + scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "bottom", plot.title = element_text(size = rel(0.8))) +
    labs(x = "", y = "Articles mentioning 'Sophisticated'", fill = "Journal", shape = "Journal",
        color = "Journal") + scale_color_manual(values = my.colors("bly")) + ggtitle("'Sophisticated'")
credit("Data: JSTOR.")
dev.off()

p <- ggplot(subset(data.subtle, year < 2013), aes(x = year, y = rate, color = longlab,
    fill = longlab, shape = longlab))

pdf(file = "figures/subtle-rate.pdf", width = 7, height = 7)
p + geom_jitter() + geom_smooth(se = FALSE) + scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "bottom", plot.title = element_text(size = rel(0.8))) +
    labs(x = "", y = "Articles mentioning 'Subtle'", fill = "Journal", shape = "Journal",
        color = "Journal") + scale_color_manual(values = my.colors("bly")) + ggtitle("'Subtle'")
credit("Data: JSTOR.")
dev.off()

p <- ggplot(subset(data.subtlety, year < 2013), aes(x = year, y = rate, color = longlab,
    fill = longlab, shape = longlab))

pdf(file = "figures/subtlety-rate.pdf", width = 7, height = 7)
p + geom_jitter() + geom_smooth(se = FALSE) + scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "bottom", plot.title = element_text(size = rel(0.8))) +
    labs(x = "", y = "Articles mentioning 'Subtlety'", fill = "Journal", shape = "Journal",
        color = "Journal") + scale_color_manual(values = my.colors("bly")) + ggtitle("'Subtlety'")
credit("Data: JSTOR.")
dev.off()

p <- ggplot(subset(data.nuance, year < 2013), aes(x = year, y = rate, color = longlab,
    fill = longlab, shape = longlab))

pdf(file = "figures/nuance-rate.pdf", width = 7, height = 5)
p + geom_jitter() + geom_smooth(se = FALSE) + scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "bottom", plot.title = element_text(size = rel(0.9))) +
    labs(x = "", y = "Articles mentioning 'Nuance' or 'Nuanced'", fill = "Journal",
        shape = "Journal", color = "Journal") + scale_color_manual(values = my.colors("bly"))
credit("Data: JSTOR.")
dev.off()




pdf(file = "figures/nuance-rate-by-discipline.pdf", width = 9, height = 8)
p <- ggplot(subset(data.theory, year < 2014 & year > 1860), aes(x = year, y = rate,
    color = unit, fill = unit))

p + geom_point(alpha = 0.7) + geom_smooth(se = FALSE) + scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "right") + labs(x = "Year", y = "Journal Articles mentioning 'Theory'",
    fill = "Discipline", color = "Discipline") + ggtitle("Rates of Nuance Across the Disciplines, 1860-2013")
credit("Data: JSTOR.")
dev.off()


library(ggrepel)

pdf(file = "figures/theory-rate-by-discipline-relative.pdf", width = 9,
    height = 9, pointsize = 10)

p <- ggplot(subset(data.theory, year < 2014 & year > 1860),
            aes(x = year, y = relative.keyword * 100,
                color = unit, fill = unit))

p1 <- p + geom_hline(yintercept = 0, color = "gray20") +
    geom_jitter(size = 0.5,
                alpha = 0.8) + geom_smooth(method = "loess") +
    geom_text_repel(data = subset(data.theory, year == 2012),
                    aes(x = year + 1, y = relative.keyword*100, label = unit),
                    segment.color = NA, nudge_x = 30, size = 2.8) +
    theme(legend.position = "right") +
    labs(x = "Year", y = "Percentage Points Difference from Base Rate", fill = "Discipline",
        color = "Discipline") + xlim(1860, 2020) + ylim(-50, 500) + guides(fill = FALSE, color = FALSE) +
    ggtitle("Relative Incidents of 'Theory' Across the Disciplines, 1860-2013")

## Turn off the clipping so the labels are legible
gt <- ggplot_gtable(ggplot_build(p1))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
credit("")
dev.off()


pdf(file = "figures/theory-rate-by-discipline-relative-excl-psych.pdf", width = 9.5,
    height = 6, pointsize = 10)

## Get average rate at the tail end, for better label placement
lab.df <- data.theory %>% filter(year > 2002 & unit %nin% "Psychology")
lab.df <- lab.df %>% group_by(unit) %>% summarize(lab_pos = mean(relative.keyword*100))
lab.df$year <- 2014

p <- ggplot(subset(data.theory, year < 2014 & year > 1860 & unit %nin% "Psychology"),
            aes(x = year, y = relative.keyword * 100,
                color = unit, fill = unit))

p1 <- p + geom_hline(yintercept = 0, color = "gray20") +
    geom_jitter(size = 0.5,
                alpha = 0.8) + geom_smooth(method = "loess") +
    geom_text_repel(data = lab.df,
                    aes(x = year + 0.2, y = lab_pos, label = unit),
                    segment.color = NA, nudge_x = 30, size = 3) +
    theme(legend.position = "right") +
    labs(x = "Year", y = "Percentage Points Difference from Base Rate", fill = "Discipline",
        color = "Discipline") + xlim(1860, 2020) + guides(fill = FALSE, color = FALSE) +
    ggtitle("Relative Incidents of 'Theory' Across the Disciplines, 1860-2013")

## Turn off the clipping so the labels are legible
gt <- ggplot_gtable(ggplot_build(p1))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
credit("Kieran Healy. Data: JSTOR. Base rate is percent mentions across all research articles in the JSTOR corpus.")
dev.off()
