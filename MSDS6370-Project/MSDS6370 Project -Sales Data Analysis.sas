/* MSDS 6370 - Statistical Sampling */
/* Spring Project */
/* Team - Ekaterina Pirogova, Hari Narayan Sanadhya */

/* Drop the file if already exists */
%web_drop_table(WORK.industryData);
%web_drop_table(WORK.projectOutputSummary);
%web_drop_table(WORK.projectSummary);
%web_drop_table(WORK.projectOutInventory);

/* Path to the file */
FILENAME datafile '/home/harisanadhya0/sasuser.v94/MSDS 6370/projectSpring2018/projectData.csv';

/* Import the dataset */

data WORK.industryData;
infile datafile delimiter = ',' MISSOVER DSD firstobs=2 ;
informat
coID best32.
sales best32.
inventory best32. ;
format
coID best16.
sales best16.
inventory best16. ;
input coID sales inventory;
run;

/* Print the first 10 records of the dataset to verify the data imported */
title "Raw Dataset (First 10 records)";
proc print data=work.industrydata(obs=10);
run;
/* Generate the descriptive statistics */
title "Descriptive Statistics";
proc means data=work.industrydata n min q1 median q3 max mean var std sum skewness kurtosis missing nmiss;
run;

/* Worked on Sales variable so sorting by Sales */
proc sort data=work.industrydata out=work.industrydata;
by sales;
run;
/* Descriptive statistics for the sales output shows the distribution */
/* is right skewness and has a large tail (Kurtosis value)*/
/* Plot histogram and qqplot to view confirm the same */
title "Distribution of Sales Data";
proc univariate data=work.industrydata noprint;
var sales;
histogram sales;
qqplot Sales/normal(mu=est sigma=est color=red l=2) square;
run;
/* Since the sales data is rightskewed, perform log transformation to reduce the skewness */
data work.industrydata;
set work.industrydata;
logSales = log10(sales);
logInventory = log10(inventory);
run;

/* Plot the scatter plot showing Correlation between Sales and Inventory */
ods exclude all;
ods select where=(_name_ ? 'ScatterPlot');
proc corr data=work.industrydata plots=scatter plots(maxpoints = 10000);
var sales inventory;
title "Correlation between Sales and Inventory";
run;
ods exclude all;
ods select where=(_name_ ? 'ScatterPlot');
proc corr data=work.industrydata plots(maxpoints = 10000) plots=scatter ;
var sales inventory;
where sales<200000 and inventory<200000;
title "Correlation between Sales and Inventory(without outliers)";
run;
ods exclude all;
ods select where=(_name_ ? 'ScatterPlot');
proc corr data=work.industrydata plots=all plots(maxpoints = 10000);
var logsales loginventory;
title "Correlation between logSales and logInventory";
run;
ods select all;

/* Plot histogram and qqplot */
/* Distribution of Sales Data after log transformation */
title "Distribution of Log of Sales Data";
proc univariate data=work.industrydata noprint;
var logSales;
histogram logsales;
qqplot logSales/ normal(mu=est sigma=est color=red l=2) square;
run;

/* Get the population count, total, mean, minimum and maximum value for sales variable */
%macro getStats();
%global recordCount minSalesValue maxSalesValue meanPopSales totalPopSales;
proc sql noprint;
select count(logSales), min(logSales), max(logSales), avg(sales), sum(sales)
into :recordCount, :minSalesValue, :maxSalesValue, :meanPopSales, :totalPopSales
from work.industrydata;
quit;
%mend;

%getStats();

/* Interval is the range of logsales value based of which 10 statrums are created */
%let interval = %sysevalf((&maxSalesValue. - &minSalesValue.)/10);
%put &recordCount &minSalesValue &maxSalesValue &interval;
/* Group data based of the log transformed column */

data work.industrydata;
set work.industrydata;
group = 10;
if logSales<=(&minSalesValue + &interval) then group=1;
else if logSales<=(&minSalesValue + (2 * &interval)) then group=2;
else if logSales<=(&minSalesValue + (3 * &interval)) then group=3;
else if logSales<=(&minSalesValue + (4 * &interval)) then group=4;
else if logSales<=(&minSalesValue + (5 * &interval)) then group=5;
else if logSales<=(&minSalesValue + (6 * &interval)) then group=6;
else if logSales<=(&minSalesValue + (7 * &interval)) then group=7;
else if logSales<=(&minSalesValue + (8 * &interval)) then group=8;
else if logSales<=(&minSalesValue + (9 * &interval)) then group=9;
run;
title "Descriptive Statistics with logSales";
ods exclude all;

proc means data=work.industrydata n min max mean var std skewness kurtosis;
var sales logsales;
by group;
ods output Summary = groupedSummary;
run;
ods select all;

proc print data=groupedSummary;
run;
title '';
/* Get the variance for each group and save them in the variable */
%macro findVar();
%global varSalesStrata;
proc sql noprint;
select var(sales) format 16.
into :varSalesStrata separated by ','
from work.industrydata group by group;
run;
%mend;

%findVar();

/* Print the variance of reach strata (comma separated) */
%put &varSalesStrata;
/* View the distribution of the group - i.e. number of obs within each group */

proc freq data=work.industrydata(keep=group);
title "Frequency stats for each group";
run;

/* Seeds used */
%let seed1 = 306609547;
%let seed2 = 606868685;
%let seed3 = 621046942;
%let seed4 = 635888608;
%let seed5 = 252341977;

/* Weights for SRS */
data work.industrydata;
set work.industrydata;
samplingWeight = &recordCount/500;
run;

/* Macro running the sampling and then estimation process 5 times */
%macro srs_sys_Sampling(srs_sys);
%do i=1 %to 5;
/* Estimation of Sales using SRS */
proc surveyselect data=work.industrydata sampsize=500 out=SRSSAMPLE method= &srs_sys seed=&&seed&i;
%let title = Sampling using &srs_sys;
title &title;
run;

proc surveymeans data=SRSSAMPLE total=&recordCount mean stderr clm sum std clsum;
var sales;
weight samplingWeight;
ods output Statistics = surveyMeansoutput;
run;
%let temp=%TSLIT(&srs_sys);
%put &temp;
%if %sysfunc(exist(WORK.projectOutputSummary)) %then %do;
proc sql noprint;
insert into work.projectOutputSummary
select 'Non-Stratified' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
run;
%end;
%else %do;
proc sql noprint;
create table work.projectOutputSummary as
select 'Non-Stratified' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
run;
%end;
%end;
%mend;
%srs_sys_sampling(srs);
%srs_sys_sampling(sys);
/* Drop the samplingWeights that was used for SRS */
data work.industrydata;
set work.industrydata;
drop samplingWeight;
run;

%macro srs_sys_sampling_neyman(srs_sys);
%local temp;
%do i=1 %to 5;
/* Estimation of Sales using SRS */
proc surveyselect data=work.industrydata sampsize=500 out=SRSSAMPLE method= &srs_sys seed=&&seed&i;
%let title = Neyman Sampling using &srs_sys;
strata group/ alloc=neyman var=(&varSalesStrata) allocmin=2;
title &title;
run;

proc surveymeans data=SRSSAMPLE total=&recordCount mean stderr clm sum std clsum;
var sales;
strata group;
weight samplingWeight;
ods output Statistics = surveyMeansoutput;
run;
%let temp=%TSLIT(&srs_sys);
%put &temp;
%if %sysfunc(exist(WORK.projectOutputSummary)) %then %do;
proc sql noprint;
insert into work.projectOutputSummary
select 'Neyman' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
run;
%end;
%else %do;
proc sql noprint;
create table work.projectOutputSummary as
select 'Neyman' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
run;
%end;
%end;
%mend;

%srs_sys_sampling_neyman(srs);
%srs_sys_sampling_neyman(sys);

/* Cumulative Frequency method */
title "Certainity group data - LogSales>=7 (Highest Outlier values)";
proc sql;
/* These are the records in the certainity group */
select * from WORK.industryData where logSales>=7.0;
run;

/* Get the total of the sales column without including the data in certainity data */
proc sql noprint;
select sum(sales), min(Sales) into :totalSales, :minSales from WORK.industryData where logSales<7.0;
run;

%let stratumRange = %sysevalf(&totalSales/8);
%put &stratumRange &minSales;

/* Get the Cumulative sum of sales */
data work.industrydata;
set work.industrydata;
retain sum 0;
sum = sum + sales;
keep sum coID sales logSales group inventory;
run;

/* Divide data into stratums - First 2 stratums are the certainity data */
data work.industrydata;
set work.industrydata;
retain count 0;
CFGroup = 10;
if logSales>=7.0 then do;
if count<2 then do;
CFGroup = 1;
count = count + 1;
end;
else CFGroup = 2;
end;
else if sum <=(&minSales + &stratumRange) then CFGroup=3;
else if sum <=(&minSales + (2 * &stratumRange)) then CFGroup=4;
else if sum <=(&minSales + (3 * &stratumRange)) then CFGroup=5;
else if sum <=(&minSales + (4 * &stratumRange)) then CFGroup=6;
else if sum <=(&minSales + (5 * &stratumRange)) then CFGroup=7;
else if sum <=(&minSales + (6 * &stratumRange)) then CFGroup=8;
else if sum <=(&minSales + (7 * &stratumRange)) then CFGroup=9;
run;

/* View the distribution of the group - i.e. number of obs within each group created using Cumulative frequency */
proc freq data=work.industrydata(keep=CFGroup);
title "Frequency stats for each Cumulative frequency group";
run;

proc sort data=work.industrydata;
by cfgroup;
run;
/* Increase the size of the sampling column in work.projectOutputSummary dataset */
proc sql noprint;
alter table work.projectOutputSummary modify sampling varchar(50);
run;

proc print data=work.projectOutputSummary ;
run;

/* Get the variance for each group and save them in the variable */

%macro findVar();
%global varSalesStrata;
proc sql noprint;
select var(sales) format 16.
into :varSalesStrata separated by ','
from work.industrydata group by CFGroup;
run;
%mend;
%findVar();
%put &varSalesStrata;

%macro srs_sys_sampling_neyman_cf(srs_sys);
%local temp;
%do i=1 %to 5;
/* Estimation of Sales using SRS */
proc surveyselect data=work.industrydata sampsize=500 out=SRSSAMPLE method= &srs_sys seed=&&seed&i;
%let title = Neyman Sampling using &srs_sys - Stratified using Cumulative Frequency;
strata CFGroup/ alloc=neyman var=(&varSalesStrata) allocmin=2;
title &title;
run;
proc surveymeans data=SRSSAMPLE total=&recordCount mean stderr clm sum std clsum;
var sales;
strata CFGroup;
weight samplingWeight;
ods output Statistics = surveyMeansoutput;
run;
%let temp=%TSLIT(&srs_sys);
%put &temp;
%if %sysfunc(exist(WORK.projectOutputSummary)) %then %do;
proc sql noprint;
insert into work.projectOutputSummary
select 'CUM Freq Stratified Neyman' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
run;
%end;
%else %do;
proc sql noprint;
create table work.projectOutputSummary as
select 'CUM Freq Stratified Neyman' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
run;
%end;
%end;
%mend;

%srs_sys_sampling_neyman_cf(srs);
%srs_sys_sampling_neyman_cf(sys);

/* Since all the algorithms were executed 5 times - Compute the average estimated value of mean and sum of sales */

ods exclude all;
proc means mean data=work.projectoutputsummary(keep=sum mean sampling method);
by sampling method notsorted;
ods output summary=projectSummary;
run;
ods select all;

/* Combine the sampling and method columns in the projectoutputsummary dataset to create the samplingMethod column */
proc sql noprint;
alter table work.projectoutputsummary add samplingMethod varchar(50);
update work.projectoutputsummary set samplingMethod= cat(strip(sampling),cat(" ", strip(method)));
run;

proc print data=work.projectoutputsummary;
title "Estimation Data generated using all the algorithms that were applied";
run;

/* Plot the estimated total sales value obtained by each algorithm for every iteration */

proc sgplot data=work.projectoutputsummary des="Plot of Estimated Total Sales";
series x = iteration y = sum /group=samplingMethod markers markerattrs=(symbol=circlefilled) lineattrs=(pattern=2)
name="Legend2" legendlabel="Sampling Method";;
refline &totalPopSales/AXIS=y legendlabel="Population Total" name="Legend1" label;
keylegend "Legend1"/location=inside position=topright;
keylegend "Legend2"/location=outside position=bottom title="Sampling Method";
title "Plot of Estimated Total Sales";
run;

/* Plot the estimated average sales value obtained by each algorithm for every iteration */
proc sgplot data=work.projectoutputsummary des="Plot of Estimated Mean Sales";
refline &meanPopSales/AXIS=y legendlabel="Population Mean" name="Legend1" label;
series x = iteration y = mean / group=samplingMethod markers markerattrs=(symbol=circlefilled) lineattrs=(pattern=2)
name="Legend2" legendlabel="Sampling Method";
keylegend "Legend1"/location=inside position=topright;
keylegend "Legend2"/location=outside position=bottom title="Sampling Method";
title "Plot of Estimated Mean Sales";
run;

/* Above plots showed that of all the algorithms applied, Non-stratified design gave the worst predictions */
/* Remove the Non-stratified design predictions and replot the data for more clear visualization of the estimations */

proc sql noprint;
delete from work.projectoutputsummary where sampling= 'Non-Stratified';
run;

/* Plot the estimated total sales value obtained by each algorithm for every iteration */
proc sgplot data=work.projectoutputsummary des="Plot of Estimated Total Sales";
series x = iteration y = sum /group=samplingMethod markers markerattrs=(symbol=circlefilled) lineattrs=(pattern=2)
name="Legend2" legendlabel="Sampling Method";;
refline &totalPopSales/AXIS=y legendlabel="Population Total" name="Legend1" label;
keylegend "Legend1"/location=inside position=topright;
keylegend "Legend2"/location=outside position=bottom title="Sampling Method";
title "Plot of Estimated Total Sales";
run;

/* Plot the estimated mean sales value obtained by each algorithm for every iteration */
proc sgplot data=work.projectoutputsummary des="Plot of Estimated Mean Sales";
refline &meanPopSales/AXIS=y legendlabel="Population Mean" name="Legend1" label;
series x = iteration y = mean / group=samplingMethod markers markerattrs=(symbol=circlefilled) lineattrs=(pattern=2)
name="Legend2" legendlabel="Sampling Method";
keylegend "Legend1"/location=inside position=topright;
keylegend "Legend2"/location=outside position=bottom title="Sampling Method";
title "Plot of Estimated Mean Sales";
run;

/* Combine the sampling and method columns in the projectsummary dataset to create the samplingMethod column */
/* This dataset has the average estimation received from all the iterations for a given algorithm */
proc sql noprint;
alter table work.projectsummary add samplingMethod varchar(50);
update work.projectsummary set samplingMethod= cat(strip(sampling),cat(" ", strip(method)));
run;

/* Plot the estimated mean sales value obtained by each algorithm averaged over the iterations */
proc sgplot data=work.projectsummary des="Plot of Estimated Mean Sales - Summarized Info";
scatter x = samplingMethod y = Mean_Mean;
refline &meanPopSales/AXIS=y legendlabel="Population Mean" name="Legend1" label;
title "Plot of Estimated Mean Sales - Average statistics";
run;

/* Plot the estimated total sales value obtained by each algorithm averaged over the iterations */

proc sgplot data=work.projectsummary des="Plot of Estimated Total Sales - Summarized Info";
scatter x = samplingMethod y = Sum_Mean;
refline &totalPopSales/AXIS=y legendlabel="Population Total" name="Legend1" label;
title "Plot of Estimated Total Sales - Average Statistics";
run;

/*_________________________________________________________________________________________________________________________________*/
/***************************************************************************************************************/
/*inventory variable calculation with sales stratification*/
/* Get the variance for each group and save them in the variable */
proc sort data=work.industrydata out=work.industrydata;
by group;
run;

/* Get the population count, total, mean, minimum and maximum value for inventory variable */
%macro getStats();
	%global recordCount mininventoryValue maxinventoryValue meanPopInventory totalPopInventory meanPopSales totalPopSales;
    proc sql noprint;
        select  count(inventory), min(inventory), max(inventory),avg(inventory), sum(inventory), avg(sales), sum(sales)
         into :recordCount, :mininventoryValue, :maxinventoryValue, :meanPopInventory, :totalPopInventory, :meanPopSales, :totalPopSales
        from work.industrydata;
    quit;
%mend;

%getStats();

%macro findVar();
	%global varSalesStrata;
	proc sql noprint;
	select var(sales)  format 16.
	into :varSalesStrata separated by ',' 
	from work.industrydata group by group;
	run;
%mend;
%findVar();
/* Print the variance of reach strata (comma separated) */
%put &varSalesStrata;
%macro srs_sys_sampling_neyman(srs_sys);
%local temp;
	%do i=1 %to 5;
		/* Estimation of Sales using SRS */
		proc surveyselect data=work.industrydata sampsize=500 out=SRSSAMPLE method= &srs_sys seed=&&seed&i;
		%let title = Neyman Sampling using &srs_sys;
		strata group/ alloc=neyman var=(&varSalesStrata) allocmin=2;
		title &title;
		run;

		proc surveymeans data=SRSSAMPLE total=&recordCount mean stderr clm sum std clsum;
		var inventory;
		strata group;
		weight samplingWeight;
		ods output Statistics = surveyMeansoutput;
		run;
		%let temp=%TSLIT(&srs_sys);
		%put &temp;
		%if %sysfunc(exist(WORK.projectOutInventory)) %then %do;
			proc sql noprint;
			insert into work.projectOutInventory 
			select 'Neyman' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
			run;
		%end;
		%else %do;
			proc sql noprint;
			create table work.projectOutInventory as
			select 'Neyman' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
			run;
		%end;
	%end;
%mend;

%srs_sys_sampling_neyman(srs);
%srs_sys_sampling_neyman(sys);

/* Get the variance for each group and save them in the variable */
proc sort data=work.industrydata out=work.industrydata;
by CFGroup;
run;
%macro findVar();
	%global varSalesStrata;
	proc sql noprint;
	select var(sales)  format 16.
	into :varSalesStrata separated by ',' 
	from work.industrydata group by CFGroup;
	run;
%mend;
%findVar();
%put &varSalesStrata;

%macro srs_sys_sampling_neyman_cf(srs_sys);
%local temp;
	%do i=1 %to 5;
		/* Estimation of Sales using SRS */
		proc surveyselect data=work.industrydata sampsize=500 out=SRSSAMPLE method= &srs_sys seed=&&seed&i;
		%let title = Neyman Sampling using &srs_sys - Stratified using Cumulative Frequency;
		strata CFGroup/ alloc=neyman var=(&varSalesStrata) allocmin=2;
		title &title;
		run;

		proc surveymeans data=SRSSAMPLE total=&recordCount mean stderr clm sum std clsum;
		var inventory;
		strata CFGroup;
		weight samplingWeight;
		ods output Statistics = surveyMeansoutput;
		run;
		%let temp=%TSLIT(&srs_sys);
		%put &temp;
		%if %sysfunc(exist(WORK.projectOutInventory)) %then %do;
			proc sql noprint;
			insert into work.projectOutInventory 
			select 'CUM Freq Stratified Neyman' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
			run;
		%end;
		%else %do;
			proc sql noprint;
			create table work.projectOutInventory as
			select 'CUM Freq Stratified Neyman' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
			run;
		%end;
	%end;
%mend;

%srs_sys_sampling_neyman_cf(srs);
%srs_sys_sampling_neyman_cf(sys);

/* Since all the algorithms were executed 5 times - Compute the average estimated value of mean and sum of sales */
ods exclude all;
proc means mean data=work.projectOutInventory(keep=sum mean sampling method);
by sampling method notsorted;
ods output summary=projectSummary;
run;
ods select all;

/* Combine the sampling and method columns in the projectOutInventory dataset to create the samplingMethod column */
proc sql noprint;
alter table work.projectOutInventory add samplingMethod varchar(50);
update work.projectOutInventory set samplingMethod= cat(strip(sampling),cat(" ", strip(method)));
run;
proc print data=work.projectOutInventory;
title "Estimation Data generated using all the algorithms that were applied";
run;

/* Plot the estimated total inventory value obtained by each algorithm for every iteration */
proc sgplot data=work.projectOutInventory des="Plot of Estimated Total Inventory";
series x = iteration y = sum /group=samplingMethod markers markerattrs=(symbol=circlefilled) lineattrs=(pattern=2) 
	name="Legend2" legendlabel="Sampling Method";;
refline &totalPopInventory/AXIS=y legendlabel="Population Total" name="Legend1" label;
keylegend "Legend1"/location=inside position=topright;
keylegend "Legend2"/location=outside position=bottom title="Sampling Method";
title "Plot of Estimated Total Inventory";
run;

/* Plot the estimated average inventory value obtained by each algorithm for every iteration */
proc sgplot data=work.projectOutInventory des="Plot of Estimated Mean Inventory";
refline &meanPopinventory/AXIS=y legendlabel="Population Mean" name="Legend1" label;
series x = iteration y = mean / group=samplingMethod markers markerattrs=(symbol=circlefilled) lineattrs=(pattern=2) 
	name="Legend2" legendlabel="Sampling Method";
keylegend "Legend1"/location=inside position=topright;
keylegend "Legend2"/location=outside position=bottom title="Sampling Method";
title "Plot of Estimated Mean Inventory";
run;

/* Above plots showed that of all the algorithms applied, Non-stratified design gave the worst predictions */
/* Remove the Non-stratified design predictions and replot the data for more clear visualization of the estimations */
proc sql noprint;
delete from work.projectOutInventory where sampling= 'Non-Stratified';
run;

/* Plot the estimated total Inventory value obtained by each algorithm for every iteration */
proc sgplot data=work.projectOutInventory des="Plot of Estimated Total Inventory";
series x = iteration y = sum /group=samplingMethod markers markerattrs=(symbol=circlefilled) lineattrs=(pattern=2) 
	name="Legend2" legendlabel="Sampling Method";;
refline &totalPopInventory/AXIS=y legendlabel="Population Total" name="Legend1" label;
keylegend "Legend1"/location=inside position=topright;
keylegend "Legend2"/location=outside position=bottom title="Sampling Method";
title "Plot of Estimated Total Inventory";
run;

/* Plot the estimated mean Inventory value obtained by each algorithm for every iteration */
proc sgplot data=work.projectOutInventory des="Plot of Estimated Mean Inventory";
refline &meanPopInventory/AXIS=y legendlabel="Population Mean" name="Legend1" label;
series x = iteration y = mean / group=samplingMethod markers markerattrs=(symbol=circlefilled) lineattrs=(pattern=2) 
	name="Legend2" legendlabel="Sampling Method";
keylegend "Legend1"/location=inside position=topright;
keylegend "Legend2"/location=outside position=bottom title="Sampling Method";
title "Plot of Estimated Mean Inventory";
run;

/* Combine the sampling and method columns in the projectsummary dataset to create the samplingMethod column */
/*  This dataset has the average estimation received from all the iterations for a given algorithm */
proc sql noprint;
alter table work.projectsummary add samplingMethod varchar(50);
update work.projectsummary set samplingMethod= cat(strip(sampling),cat(" ", strip(method)));
run;
