Folders:
data - to store our datasets
scripts - to store our RScript
output - to store outputs such as plots

For example, to import mydata.csv from data folder and save it to df variable, you can run:
df <- read.csv("data/mydata.csv")

For example, to save pie_chart as pie.png in output folder, you can run:
ggsave("output/pie.png", plot = pie_chart, width = 6, height = 4, dpi = 300)
