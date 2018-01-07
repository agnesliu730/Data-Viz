library(readxl)
library(plyr)
library(ggplot2)
library(ggrepel)

# ---------First graph-----------
# Upload dataset 
CEO_data <- read_excel("C:/Users/agnes/Desktop/Top 100 CEOs Final 2016.xlsx")

# Delete columns (x1 and x2) with NAs and make sure NA are omited
CEO_data[c(12:13)] <- list(NULL)
CEO_data <- na.omit(CEO_data)

# Create dataset with Industry and number of CEOs by industry
Ind_data <- count(CEO_data, "Industry")
Ind_data

# Create basic horizontal bar chart
p1 <- ggplot(Ind_data, aes(x=reorder(Industry, freq), y=freq, fill=freq)) + 
      geom_bar(stat = "identity") + coord_flip() 

# Fill the bar with color, add labels to bar and set axis range
p1 <- p1 + geom_text(aes(label=freq), vjust=0.3, hjust= -0.2, size=3.5) +
           scale_fill_continuous(low="skyblue1", high="skyblue4", guide = FALSE) +
           scale_y_continuous(limit = c(0, 20))
  
# Add axis labels, title and subtitle
p1 <- p1 + labs(y="Number of CEOs", x="Industry", title = "Top 100 Best-Performing CEOs by industry, 2016",
                caption="\nData Source: Harvard Business Review",
                subtitle="\nHarvard Business Review compiled a list of world's top 100 performing CEOs from 14 industries in 2016. \nBased on the data, one-third of top 100 performing CEOs come from Consumer goods and Financial services industries.")

# Design labels, title and subtitle
p1 <- p1 + theme_classic() + 
           theme(plot.title = element_text(size=20, hjust = 0.4, face="bold"), axis.title = element_text(face="bold", hjust=0.45, size = 13), 
                 axis.text = element_text(face="bold"),
                 plot.subtitle = element_text(size = 13, face = "bold", color = "grey35", hjust = 0.4))
p1


# ---------Second graph-----------
# Uplod new dataset for second graph
Rank_data <- read_excel("C:/Users/agnes/Desktop/Rank Data.xlsx")

# Create bubble chart
p2 <- ggplot(Rank_data, aes(x=`Industry Financial Rank`, y=`Industry ESG Rank`, size=COUNT)) +
      geom_point(shape=21, colour="grey10", fill="lightcoral", alpha = 0.9)

# Flip axis range
p2 <- p2 + scale_y_reverse(lim=c(14, 0)) + scale_x_reverse(lim=c(14, 0)) +
           scale_size_area(max_size = 28, guide=FALSE)

# Add axis labels, title and subtitle
p2 <- p2 + labs(x="Financial Ranking by Industry (With 1 being the best)", y="ESG Ranking by industry (With 1 being the best)",
                title="Financial Performance vs. Non-Financial Performance by industry, 2016",
                caption="\nData Source: Harvard Business Review",
                subtitle="\nDo industries have good financial performance also have good non-financial performance?")

# Design labels, title and subtitle
p2 <- p2 + theme(plot.title = element_text(size=22, hjust=0.5, face="bold"), axis.title = element_text(face="bold", size = 13),
                 plot.subtitle = element_text(size = 14, face = "bold", color = "grey35", hjust=0.5),
                 axis.text = element_text(face="bold")) +
           theme_classic() 

# Repel overlapping text labels away from each other 
## and repel away from the data points that they label
p2 <- p2 + geom_text_repel(aes(label=INDUSTRY), size=3.5, segment.color = 'grey35', 
                           nudge_y=0.2, nudge_x=1, point.padding = unit(1.1, "lines"), 
                           box.padding = unit(0.9, "lines"))
p2


