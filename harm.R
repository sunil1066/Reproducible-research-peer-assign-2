storm = read.csv("repdata-data-StormData.csv")
df = storm[c("EVTYPE","FATALITIES" ,"INJURIES","PROPDMG", "PROPDMGEXP" ,"CROPDMG"  ,  "CROPDMGEXP" )]
df$PROPDMGEXP = as.character(df$PROPDMGEXP)
df$CROPDMGEXP = as.character(df$CROPDMGEXP)
df$PROPDMGEXP[df$PROPDMGEXP=="0"] = 1
df$PROPDMGEXP[df$PROPDMGEXP==""] = 0
df$PROPDMGEXP[df$PROPDMGEXP=="-"] = 0
df$PROPDMGEXP[df$PROPDMGEXP=="+"] = 0
df$PROPDMGEXP[df$PROPDMGEXP=="?"] = 0
df$PROPDMGEXP[df$PROPDMGEXP=="1"] = 10
df$PROPDMGEXP[df$PROPDMGEXP=="2"] = 100
df$PROPDMGEXP[df$PROPDMGEXP=="3"] = 1000
df$PROPDMGEXP[df$PROPDMGEXP=="4"] = 10000
df$PROPDMGEXP[df$PROPDMGEXP=="5"] = 100000
df$PROPDMGEXP[df$PROPDMGEXP=="6"] = 1000000
df$PROPDMGEXP[df$PROPDMGEXP=="7"] = 10000000
df$PROPDMGEXP[df$PROPDMGEXP=="8"] = 100000000
df$PROPDMGEXP[df$PROPDMGEXP=="H"] = 100
df$PROPDMGEXP[df$PROPDMGEXP=="h"] = 100
df$PROPDMGEXP[df$PROPDMGEXP=="K"] = 1000
df$PROPDMGEXP[df$PROPDMGEXP=="B"] = 1000000000
df$PROPDMGEXP[df$PROPDMGEXP=="m"] = 1000000
df$PROPDMGEXP[df$PROPDMGEXP=="M"] = 1000000

df$CROPDMGEXP[df$CROPDMGEXP=="0"] = 1
df$CROPDMGEXP[df$CROPDMGEXP==""] = 0
df$CROPDMGEXP[df$CROPDMGEXP=="?"] = 0
df$CROPDMGEXP[df$CROPDMGEXP=="2"] = 100
df$CROPDMGEXP[df$CROPDMGEXP=="k"] = 1000
df$CROPDMGEXP[df$CROPDMGEXP=="K"] = 1000
df$CROPDMGEXP[df$CROPDMGEXP=="m"] = 1000000
df$CROPDMGEXP[df$CROPDMGEXP=="M"] = 1000000
df$CROPDMGEXP[df$CROPDMGEXP=="B"] = 1000000000

df$PROPDMGEXP = as.numeric(df$PROPDMGEXP)
df$CROPDMGEXP = as.numeric(df$CROPDMGEXP)

df$crop = df$CROPDMG * df$CROPDMGEXP
df$prop = df$PROPDMG * df$PROPDMGEXP

# to get the whole effect on population health we just sum up both fatalities and injuries
df$fisum = rowSums(df[,2:3])
# to get the whole effect on economic consequences we just sum up both property and crop damages
df$pcsum = rowSums(df[,8:9])

t = tapply(df$fisum, df$EVTYPE, sum)
f = tapply(df$FATALITIES, df$EVTYPE, sum)
i = tapply(df$INJURIES, df$EVTYPE, sum)
d = data.frame(sum =t, EVTYPE=as.character(rownames(t)), FATALITIES = f, INJURIES = i)
d= d[order(d$sum, decreasing = T),]

# to convert wide data format into long format just to make stacks into bar plot
library(reshape2)
m = melt(d, id.vars = c("EVTYPE", "sum"))
m = m[order(m$sum, decreasing = T),]
ggplot(head(m,20), aes(y=value, x=EVTYPE, fill=variable)) + geom_bar(stat = "identity") + coord_flip() + labs(x = "", y="number of people impacted")+ ggtitle("Harmful events")

t1 = tapply(df$pcsum, df$EVTYPE, sum)
cr = tapply(df$crop, df$EVTYPE, sum)
pr = tapply(df$prop, df$EVTYPE, sum)
d1= data.frame(sum=t1, EVTYPE=as.character(rownames(t)), crop = cr, prop= pr)
d1 = d1[order(d1$sum, decreasing = T),]

library(reshape2)
m1 = melt(d1, id.vars = c("EVTYPE", "sum"))
m1 = m1[order(m1$sum, decreasing = T),]
ggplot(head(m1,20), aes(y=value, x=EVTYPE, fill=variable)) + geom_bar(stat = "identity") + coord_flip() + labs(x = "", y="cost of damages in dollar")+ ggtitle("Economic consequences")



