/* MSDS 6370 - Statistical Sampling */
/* Spring Project */
/* Team - Ekaterina Pirogova, Hari Narayan Sanadhya */

/* Drop the file if already exists */
%web_drop_table(WORK.industryData);
%web_drop_table(WORK.projectOutputSummary);
%web_drop_table(WORK.projectSummary);
%web_drop_table(WORK.projectOutSales);


/* Path to the file */
FILENAME datafile '/home/irisstark77610/Data/data.csv';

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
input coID sales inventory stratumWS stratumLH;
run;

/* Print the first 10 records of the dataset to verify the data imported */
title "Raw Dataset (First 10 records)";
proc print data=work.industrydata(obs=10);
run;

/* Generate the descriptive statistics */
title "Descriptive Statistics";
proc means data=work.industrydata n min q1 median q3 max mean std sum skewness kurtosis;
var sales inventory;
run;

/* Worked on inventory variable so sorting by inventory */
proc sort data=work.industrydata out=work.industrydata;
by inventory;
run;

/* Descriptive statistics for the inventory output shows the distribution */
/*       is right skewness and has a large tail (Kurtosis value)*/
/* Plot histogram and qqplot to view confirm the same */
title "Distribution of Sales and Inventory variables";
proc univariate data=work.industrydata noprint;
var sales inventory;
histogram sales inventory;
qqplot sales inventory/normal(mu=est sigma=est color=red l=2) square;
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




/* Get the variance for each group and save them in the variable for equal WhSh method*/
%macro findVar();
	%global stratumWS;
	proc sql noprint;
	select var(inventory)  format 16.
	into :stratumWS separated by ',' 
	from work.industrydata group by stratumWS;
	run;
%mend;
%findVar();
/* Print the variance of reach strata (comma separated) */
%put &stratumWS;

/* Get the variance for each group and save them in the variable for LH method*/
%macro findVar();
	%global stratumLH;
	proc sql noprint;
	select var(inventory)  format 16.
	into :stratumLH separated by ',' 
	from work.industrydata group by stratumLH;
	run;
%mend;
%findVar();
/* Print the variance of reach strata (comma separated) */
%put &stratumLH;

/* View the distribution of the group - i.e. number of obs within each group */
proc freq data=work.industrydata(keep=stratumWS);
title "Frequency stats for equal WhSh method";
run;
proc freq data=work.industrydata(keep=stratumLH);
title "Frequency stats for LH method";
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
		/* Estimation of inventory using SRS */
		proc surveyselect data=work.industrydata sampsize=500 out=SRSSAMPLE method= &srs_sys seed=&&seed&i;
		%let title = Sampling using &srs_sys;
		title &title;
		run;
		proc surveymeans data=SRSSAMPLE total=&recordCount mean stderr clm sum std clsum;
		var inventory;
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
		/* Estimation of inventory using SRS */
		proc surveyselect data=work.industrydata sampsize=500 out=SRSSAMPLE method= &srs_sys seed=&&seed&i;
		%let title = Equal WhSh method using &srs_sys;
		strata stratumWS/ alloc=neyman var=(&stratumWS) allocmin=2;
		title &title;
		run;

		proc surveymeans data=SRSSAMPLE total=&recordCount mean stderr clm sum std clsum;
		var inventory;
		strata stratumWS;
		weight samplingWeight;
		ods output Statistics = surveyMeansoutput;
		run;
		%let temp=%TSLIT(&srs_sys);
		%put &temp;
		%if %sysfunc(exist(WORK.projectOutputSummary)) %then %do;
			proc sql noprint;
			insert into work.projectOutputSummary 
			select 'Equal WhSh method' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
			run;
		%end;
		%else %do;
			proc sql noprint;
			create table work.projectOutputSummary as
			select 'Equal WhSh method' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
			run;
		%end;
	%end;
%mend;

%srs_sys_sampling_neyman(srs);
%srs_sys_sampling_neyman(sys);




%macro srs_sys_sampling_neyman_LH(srs_sys);
%local temp;
	%do i=1 %to 5;
		/* Estimation of Inventory using SRS */
		proc surveyselect data=work.industrydata sampsize=500 out=SRSSAMPLE method= &srs_sys seed=&&seed&i;
		%let title = Neyman Sampling using &srs_sys - Stratified using LH method;
		strata stratumLH/ alloc=neyman var=(&stratumLH) allocmin=2;
		title &title;
		run;

		proc surveymeans data=SRSSAMPLE total=&recordCount mean stderr clm sum std clsum;
		var inventory;
		strata stratumLH;
		weight samplingWeight;
		ods output Statistics = surveyMeansoutput;
		run;
		%let temp=%TSLIT(&srs_sys);
		%put &temp;
		%if %sysfunc(exist(WORK.projectOutputSummary)) %then %do;
			proc sql noprint;
			insert into work.projectOutputSummary 
			select 'LH method Stratified Neyman' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
			run;
		%end;
		%else %do;
			proc sql noprint;
			create table work.projectOutputSummary as
			select 'LH method Stratified Neyman' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
			run;
		%end;
	%end;
%mend;

%srs_sys_sampling_neyman_LH(srs);
%srs_sys_sampling_neyman_LH(sys);

/* Since all the algorithms were executed 5 times - Compute the average estimated value of mean and sum of inventory */
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

/* Plot the estimated total inventory value obtained by each algorithm for every iteration */
proc sgplot data=work.projectoutputsummary des="Plot of Estimated Total inventory";
series x = iteration y = sum /group=samplingMethod markers markerattrs=(symbol=circlefilled) lineattrs=(pattern=2) 
	name="Legend2" legendlabel="Sampling Method";;
refline &totalPopInventory/AXIS=y legendlabel="Population Total" name="Legend1" label;
keylegend "Legend1"/location=inside position=topright;
keylegend "Legend2"/location=outside position=bottom title="Sampling Method";
title "Plot of Estimated Total Inventory";
run;

proc print data=work.projectoutputsummary;
run;

/* Plot the estimated average inventory value obtained by each algorithm for every iteration */
proc sgplot data=work.projectoutputsummary des="Plot of Estimated Mean inventory";
refline &meanPopInventory/AXIS=y legendlabel="Population Mean" name="Legend1" label;
series x = iteration y = mean / group=samplingMethod markers markerattrs=(symbol=circlefilled) lineattrs=(pattern=2) 
	name="Legend2" legendlabel="Sampling Method";
keylegend "Legend1"/location=inside position=topright;
keylegend "Legend2"/location=outside position=bottom title="Sampling Method";
title "Plot of Estimated Mean inventory";
run;

/* Above plots showed that of all the algorithms applied, Non-stratified design gave the worst predictions */
/* Remove the Non-stratified design predictions and replot the data for more clear visualization of the estimations */
proc sql noprint;
delete from work.projectoutputsummary where sampling= 'Non-Stratified';
run;

/* Plot the estimated total inventory value obtained by each algorithm for every iteration */
proc sgplot data=work.projectoutputsummary des="Plot of Estimated Total inventory";
series x = iteration y = sum /group=samplingMethod markers markerattrs=(symbol=circlefilled) lineattrs=(pattern=2) 
	name="Legend2" legendlabel="Sampling Method";;
refline &totalPopInventory/AXIS=y legendlabel="Population Total" name="Legend1" label;
keylegend "Legend1"/location=inside position=topright;
keylegend "Legend2"/location=outside position=bottom title="Sampling Method";
title "Plot of Estimated Total inventory";
run;

/* Plot the estimated mean inventory value obtained by each algorithm for every iteration */
proc sgplot data=work.projectoutputsummary des="Plot of Estimated Mean inventory";
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

/* Plot the estimated mean inventory value obtained by each algorithm averaged over the iterations */
proc sgplot data=work.projectsummary des="Plot of Estimated Mean Inventory - Summarized Info";
scatter x = samplingMethod y = Mean_Mean;
refline &meanPopInventory/AXIS=y legendlabel="Population Mean" name="Legend1" label;
title "Plot of Estimated Mean inventory - Average statistics";
run;

/* Plot the estimated total inventory value obtained by each algorithm averaged over the iterations */
proc sgplot data=work.projectsummary des="Plot of Estimated Total inventory - Summarized Info";
scatter x = samplingMethod y = Sum_Mean;
refline &totalPopInventory/AXIS=y legendlabel="Population Total" name="Legend1" label;
title "Plot of Estimated Total inventory - Average Statistics";
run;

/*_________________________________________________________________________________________________________________________________*/
/***************************************************************************************************************/
/*sales variable calculation with inventory stratification*/
%macro srs_sys_sampling_neyman(srs_sys);
%local temp;
	%do i=1 %to 5;
		/* Estimation of sales using SRS */
		proc surveyselect data=work.industrydata sampsize=500 out=SRSSAMPLE method= &srs_sys seed=&&seed&i;
		%let title = Equal WhSh method using &srs_sys;
		strata stratumWS/ alloc=neyman var=(&stratumWS) allocmin=2;
		title &title;
		run;

		proc surveymeans data=SRSSAMPLE total=&recordCount mean stderr clm sum std clsum;
		var sales;
		strata stratumWS;
		weight samplingWeight;
		ods output Statistics = surveyMeansoutput;
		run;
		%let temp=%TSLIT(&srs_sys);
		%put &temp;
		%if %sysfunc(exist(WORK.projectOutSales)) %then %do;
			proc sql noprint;
			insert into work.projectOutSales 
			select 'Equal WhSh method' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
			run;
		%end;
		%else %do;
			proc sql noprint;
			create table work.projectOutSales as
			select 'Equal WhSh method' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
			run;
		%end;
	%end;
%mend;

%srs_sys_sampling_neyman(srs);
%srs_sys_sampling_neyman(sys);



%macro srs_sys_sampling_neyman_LH(srs_sys);
%local temp;
	%do i=1 %to 5;
		/* Estimation of sales using SRS */
		proc surveyselect data=work.industrydata sampsize=500 out=SRSSAMPLE method= &srs_sys seed=&&seed&i;
		%let title = Neyman Sampling using &srs_sys - Stratified using LH method;
		strata stratumLH/ alloc=neyman var=(&stratumLH) allocmin=2;
		title &title;
		run;

		proc surveymeans data=SRSSAMPLE total=&recordCount mean stderr clm sum std clsum;
		var sales;
		strata stratumLH;
		weight samplingWeight;
		ods output Statistics = surveyMeansoutput;
		run;
		%let temp=%TSLIT(&srs_sys);
		%put &temp;
		%if %sysfunc(exist(WORK.projectOutSales)) %then %do;
			proc sql noprint;
			insert into work.projectOutSales 
			select 'LH method Stratified Neyman' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
			run;
		%end;
		%else %do;
			proc sql noprint;
			create table work.projectOutSales as
			select 'LH method Stratified Neyman' as sampling, &temp as method, &i as iteration, outtable.* from surveyMeansoutput as outtable;
			run;
		%end;
	%end;
%mend;

%srs_sys_sampling_neyman_LH(srs);
%srs_sys_sampling_neyman_LH(sys);
ods exclude all;
proc means mean data=work.projectOutSales(keep=sum mean sampling method);
by sampling method notsorted;
ods output summary=projectSummary;
run;
ods select all;

/* Combine the sampling and method columns in the Sales dataset to create the samplingMethod column */
proc sql noprint;
alter table work.projectOutSales add samplingMethod varchar(50);
update work.projectOutSales set samplingMethod= cat(strip(sampling),cat(" ", strip(method)));
run;
proc print data=work.projectOutSales;
title "Estimation Data generated using all the algorithms that were applied";
run;

/* Plot the estimated average Sales value obtained by each algorithm for every iteration */
proc sgplot data=work.projectOutSales des="Plot of Estimated Mean Sales";
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
delete from work.projectOutSales where sampling= 'Non-Stratified';
run;

/* Plot the estimated total Sales value obtained by each algorithm for every iteration */
proc sgplot data=work.projectOutSales des="Plot of Estimated Total Sales";
series x = iteration y = sum /group=samplingMethod markers markerattrs=(symbol=circlefilled) lineattrs=(pattern=2) 
	name="Legend2" legendlabel="Sampling Method";;
refline &totalPopSales/AXIS=y legendlabel="Population Total" name="Legend1" label;
keylegend "Legend1"/location=inside position=topright;
keylegend "Legend2"/location=outside position=bottom title="Sampling Method";
title "Plot of Estimated Total sales";
run;


/* Combine the sampling and method columns in the Sales dataset to create the samplingMethod column */
/*  This dataset has the average estimation received from all the iterations for a given algorithm */
proc sql noprint;
alter table work.projectOutSales add samplingMethod varchar(50);
update work.projectOutSales set samplingMethod= cat(strip(sampling),cat(" ", strip(method)));
run;

