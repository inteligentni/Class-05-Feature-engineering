# Normalized chart position 
#   if (<chart position> == 0) {
#     <normalized chart position> <- 0
#   }
#   if (<chart position> in (1:<max value>)) {
#     <normalized chart position> <- (<max value> - <chart position> + 1) / <max value>
#   }
getNormalizedChartPosition <- function(ch.pos) {      # ch.pos: int vector of chart positions, [0:n]
  max.value <- max(ch.pos)
  norm.ch.pos <- ch.pos
  for (i in 1:length(ch.pos)) {
    if (norm.ch.pos[i] != 0) {
      norm.ch.pos[i] <- (max.value - ch.pos[i] + 1) / max.value
    } else {
      norm.ch.pos[i] <- 0
    }
  }
  norm.ch.pos
}

# Normalized weeks-on-chart
#   <normalized weeks-on-chart> <- <weeks-on-chart> / <max value>
getNormalizedWeeksOnChart <- function (weeks.on.chart) {
  norm.weeks.on.chart <- weeks.on.chart / max(weeks.on.chart)
  norm.weeks.on.chart
}

# Normalized weeks-at-No.1
#   <normalized weeks-at-No.1> <- <weeks-at-No.1> / <max value>
getNormalizedWeeksAtNo1 <- function (weeks.at.No.1) {
  norm.weeks.at.No.1 <- weeks.at.No.1 / max(weeks.at.No.1)
  norm.weeks.at.No.1
}

# Idea: chart presence
# Chart presence in UK/US: 
#   <chart presence> <- 
#     (<a> * <normalized chart position> + 
#      <b> * <normalized weeks-on-chart> + 
#      <c> * normalized weeks-at-no-1) / 
#     (<a> + <b> + <c>)
getChartPresence <- function(norm.ch.pos, norm.weeks.on.chart, norm.weeks.at.No.1,  # normalized data
                             a, b, c) {                                             # weights
  ch.presence <- (a * norm.ch.pos + b * norm.weeks.on.chart + c * norm.weeks.at.No.1) / (a + b + c)
  ch.presence
}

# Chart presence in both UK and US (overall): (a * ch.pr.1 + b * ch.pr.2) / (a + b)
#   <chart presence> <- 
#     (<a> * <chart presence in UK> + 
#      <b> * <chart presence in US>) / 
#     (<a> + <b>)

getChartPresenceOverall <- function(ch.pr.1, ch.pr.2, a, b) {
  ch.pr.overall <- (a * ch.pr.1 + b * ch.pr.2) / (a + b)
  ch.pr.overall
}
