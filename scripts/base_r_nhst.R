dir.create("./Output")
process_data <- function(filename) {
  filename <- read.csv(paste('https://homes.cs.washington.edu/~emjun/tea-lang/datasets/', filename,'.csv', sep=""))
  colnames(filename) <- gsub(" ", ".", colnames(filename))
  return (filename)
}

all_cors <- function() {
  tests <- c("pearson", "kendall", "spearman") #point-biserial is equivalent to pearson
  for (val in tests) {
    res <- cor.test(process_data("statex77")$Illiteracy, process_data("statex77")$Life.Exp, method = val)
    capture.output(res, file = "./Output/correlation_tests.txt", append = T )
    res <- cor.test(process_data("liar")$Position, process_data("liar")$Creativity, method=val)
    capture.output(res, file = "./Output/correlation_tests.txt", append = T )
    res <- cor.test(process_data("pbcorr")$time, process_data("pbcorr")$gender, method = val)
    capture.output(res, file = "./Output/correlation_tests.txt", append = T ) 
  }
  
}

all_bivariate <- function() {
  #subsetting alcohol data to be readable by t-test function
  sunday <- subset(process_data("alcohol"), day == "sundayBDI")
  sunday <- sunday$value
  wednesday <- subset(process_data("alcohol"), day == "wedsBDI")
  wednesday <- wednesday$value
  
  picture <- subset(process_data("spiderLong")[2:3], Group == "Picture")
  picture <- picture$Anxiety
  spider <- subset(process_data("spiderLong")[2:3], Group == "Real Spider")
  spider <- spider$Anxiety
  So <- process_data("UScrime")$So
  Prob <- process_data("UScrime")$Prob
  
  res_student <- t.test(So, Prob, var.equal=TRUE)
  res_paired <- t.test(picture, spider, paired = T)
  res_wilcoxon <- wilcox.test(sunday, wednesday)
  res_welch <- t.test(So, Prob) 
  res_mann <- wilcox.test(sunday, wednesday)
  
  capture.output(res_student, file = "./Output/bivariate_tests.txt")
  capture.output(res_paired, file = "./Output/bivariate_tests.txt", append = T)
  capture.output(res_wilcoxon, file = "./Output/bivariate_tests.txt", append = T)
  capture.output(res_welch, file = "./Output/bivariate_tests.txt", append = T)
  capture.output(res_mann, file = "./Output/bivariate_tests.txt", append = T)

}

all_multivariate <- function() {
  drugD <- subset(process_data("cholesterol"), trt == "drugD")
  drugD <- drugD$response
  drugE <- subset(process_data("cholesterol"), trt == "drugE")
  drugE <-drugE$response
  
  res_f <- var.test(drugD, drugE)
  rm_oneway_anova <- aov(conc ~ uptake + Plant, data = process_data("co2"))
  factorial_anova <- aov(attractiveness ~ alcohol + gender, data = process_data("gogglesData")) #gogglesData: gender, alcohol, attractiveness
  two_way_anova <- aov(conc ~ Type * uptake, data = process_data("co2"))
  kruskal_wallis <- kruskal.test(Sperm ~ Soya, data = process_data("soya"))
  
  capture.output(res_f, file = "./Output/multivariate_tests.txt")
  capture.output(rm_oneway_anova, file = "./Output/multivariate_tests.txt", append = T)
  capture.output(factorial_anova, file = "./Output/multivariate_tests.txt", append = T)
  capture.output(two_way_anova, file = "./Output/multivariate_tests.txt", append = T)
  capture.output(kruskal_wallis, file = "./Output/multivariate_tests.txt", append = T)
}

all_proportions <- function() {
  chi_square <- chisq.test(process_data("catsData")$Training, process_data("catsData")$Dance, correct = FALSE)
  fishers <- fisher.test(process_data("catsData")$Training, process_data("catsData")$Dance)
  capture.output(chi_square, file = "./Output/proportion_tests.txt", append)
  capture.output(fishers, file = "./Output/proportion_tests.txt", append = TRUE)
}
all_cors()
all_bivariate()
all_multivariate()
all_proportions()