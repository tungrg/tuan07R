setwd('D:/18120258')
getwd()
##
p-value nho hon alpha thi bac bo H0
##
pt lon hon hoac bang thi header = FALSE
##
data<-read.csv("volume.csv", header = TRUE)
attach(data)
x<-data$machine1
y<-data$machine2
t.test(x, y, alternative = "two.sided", mu = 0, conf.level = 0.95)
test.le.oneside = function(x,y,mu0, sig1, sig2, alpha)
{
	if(sig1 != sig2){
	t0 = ((mean(x) - mean(y))/sqrt(((sig1^2)/length(x))+(sig2^2)/length(y))
		if(t0 > t)
		{ cat("bac bo")}
	else
		{cat("khong bac bo")}
	pvalue = pt(t0,length(x) + length(y) -2))}
	else
	{
		sp = ((length(x) - 1)*sig1^2 + (length(y) - 1)*sig2^2)/(length(x) + length(y) -2)
		t0 = (mean(x) - mean(y))/(sp/(1/length(x) + 1/length(y))
		t = qt(1-alpha/2, length(x) + length(y) -2)
		if(abs(t0) > t)
			{ cat(("bac bo")}
		else
			{cat("khong bac bo")}
		pvalue = pt(t0, length(x) + length(y) -2)
	}
}

a = test.le.oneside(x,y, 0, sqrt(var(x)),sqrt(var(y)), 0.05)