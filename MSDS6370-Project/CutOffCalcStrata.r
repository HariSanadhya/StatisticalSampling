#this data is already sorted
data = read.csv('C:/Users/25355/OneDrive/Study/SMU/Term4/Statistical Sampling/Project/data.csv',sep=",",header=TRUE)



#drop rows with certanity value
maxIn = max(data$inventory)
data = data[!data$inventory==maxIn,]
maxIn = max(data$inventory)
data = data[!data$inventory==maxIn,]
#data ordered in increasing order by stratification variable
popSize = 9760
#initial values for stratum size
strata.cumrootf(data$inventory,n=500, Ls=5)
strata.LH(data$inventory,n=500, Ls=5)
#initial values for stratum size cum sqrt(f)
s1=8779
s2=725
s3=229
s4=25
s5=2

#LH
s1=4718
s2=2928
s3=1378
s4=495
s5=241
#initial stratum standard deviation
Sh1 = sd(data$inventory[1:s1])
Sh2 = sd(data$inventory[(s1+1):(s1+s2)])
Sh3 = sd(data$inventory[(s1+s2+1):(s1+s2+s3)])
Sh4 = sd(data$inventory[(s1+s2+s3+1):(s1+s2+s3+s4)])
Sh5 = sd(data$inventory[(s1+s2+s3+s4+1):9760])


a=TRUE
i=0
while (a){
  k=1/(1/Sh1+1/Sh2+1/Sh3+1/Sh4+1/Sh5)
  s1 = round(k*popSize/Sh1,0)
  s2 = round(k*popSize/Sh2,0)
  s3 = round(k*popSize/Sh3,0)
  s4 = round(k*popSize/Sh4,0)
  s5 = round(k*popSize/Sh5,0)

  if((s1+s2+s3+s4+s5)<9760){
    less=9760-(s1+s2+s3+s4+s5)
    s1 = round((k*popSize/Sh1)+less,0)
  }
  if((s1+s2+s3+s4+s5)>9760){
    more=(s1+s2+s3+s4+s5)-9760
    s1 = round((k*popSize/Sh1)-more,0)
  }
  
  Sh1 = sd(data$inventory[1:s1])
  Sh2 = sd(data$inventory[(s1+1):(s1+s2)])
  Sh3 = sd(data$inventory[(s1+s2+1):(s1+s2+s3)])
  Sh4 = sd(data$inventory[(s1+s2+s3+1):(s1+s2+s3+s4)])
  Sh5 = sd(data$inventory[(s1+s2+s3+s4+1):9760])
  i=i+1
  k1=s1*Sh1/popSize
  k2=s2/popSize*Sh2
  k3=s3/popSize*Sh3
  k4=s4/popSize*Sh4
  k5=s5/popSize*Sh5
  if ( min(k2,k1)/max(k2,k1)>0.992 &
       min(k2,k3)/max(k2,k3)>0.992 &
       min(k3,k4)/max(k3,k4)>0.992 &
       min(k4,k5)/max(k4,k5)>0.992){
    print("finished in")
    print(i)
    print(s1)
    print(s2)
    print(s3)
    print(s4)
    print(s5)
    print(s6)
    print(s1+s2+s3+s4+s5)
    print(Sh1)
    print(Sh2)
    print(Sh3)
    print(Sh4)
    print(Sh5)
    a=FALSE
  }
  if(i==100000){
    print("didn't find")
    a=FALSE
  }
  
}
print(k1)
print(k2)
print(k3)
print(k4)
print(k5)



#6
#_____________________________________________________________________________________________________
strata.cumrootf(data$inventory,n=500, Ls=6)
strata.LH(data$inventory,n=500, Ls=6)
#initial values for stratum size cum sqrt(f)
s1=8543
s2=893
s3=252
s4=67
s5=3
s6=2

#LH
s1=4603
s2=2351
s3=1464
s4=747
s5=353
s6=242

#initial stratum standard deviation
Sh1 = sd(data$inventory[1:s1])
Sh2 = sd(data$inventory[(s1+1):(s1+s2)])
Sh3 = sd(data$inventory[(s1+s2+1):(s1+s2+s3)])
Sh4 = sd(data$inventory[(s1+s2+s3+1):(s1+s2+s3+s4)])
Sh5 = sd(data$inventory[(s1+s2+s3+s4+1):(s1+s2+s3+s4+s5)])
Sh6 = sd(data$inventory[(s1+s2+s3+s4+s5+1):(s1+s2+s3+s4+s5+s6)])

disrepancy=0.42
a=TRUE
i=0
while (a){
  k=1/(1/Sh1+1/Sh2+1/Sh3+1/Sh4+1/Sh5+1/Sh6)
  s1 = round(k*popSize/Sh1,0)
  s2 = round(k*popSize/Sh2,0)
  s3 = round(k*popSize/Sh3,0)
  s4 = round(k*popSize/Sh4,0)
  s5 = round(k*popSize/Sh5,0)
  s6 = round(k*popSize/Sh6,0)
  if((s1+s2+s3+s4+s5+s6)<9760){
    less=9760-(s1+s2+s3+s4+s5+s6)
    s1 = round((k*popSize/Sh1)+less,0)
  }
  if((s1+s2+s3+s4+s5+s6)>9760){
    more=(s1+s2+s3+s4+s5+s6)-9760
    s1 = round((k*popSize/Sh1)-more,0)
  }
  Sh1 = sd(data$inventory[1:s1])
  Sh2 = sd(data$inventory[(s1+1):(s1+s2)])
  Sh3 = sd(data$inventory[(s1+s2+1):(s1+s2+s3)])
  Sh4 = sd(data$inventory[(s1+s2+s3+1):(s1+s2+s3+s4)])
  Sh5 = sd(data$inventory[(s1+s2+s3+s4+1):(s1+s2+s3+s4+s5)])
  Sh6 = sd(data$inventory[(s1+s2+s3+s4+s5+1):9760])
  i=i+1
  k1=s1*Sh1/popSize
  k2=s2/popSize*Sh2
  k3=s3/popSize*Sh3
  k4=s4/popSize*Sh4
  k5=s5/popSize*Sh5
  k6=s6/popSize*Sh6
  if ( min(k2,k1)/max(k2,k1)>disrepancy &
       min(k2,k3)/max(k2,k3)>disrepancy &
       min(k3,k4)/max(k3,k4)>disrepancy &
       min(k4,k5)/max(k4,k5)>disrepancy &
       min(k5,k6)/max(k5,k6)>disrepancy){
    print("finished in")
    print(i)
    print(s1)
    print(s2)
    print(s3)
    print(s4)
    print(s5)
    print(s6)
    print(s1+s2+s3+s4+s5+s6)
    print(Sh1)
    print(Sh2)
    print(Sh3)
    print(Sh4)
    print(Sh5)
    print(Sh6)
    a=FALSE
  }
  if(i==100000){
    print("didn't find")
    a=FALSE
  }
  
}
print(k1)
print(k2)
print(k3)
print(k4)
print(k5)
print(k6)

#7
#_____________________________________________________________________________________________________
strata.cumrootf(data$inventory,n=500, Ls=7)
strata.LH(data$inventory,n=500, Ls=7)
#initial values for stratum size
#cum root f
s1=8354
s2=777
s3=358
s4=204
s5=61
s6=4
s7=2

#LH method
s1=4556
s2=2135
s3=1116
s4=784
s5=597
s6=328
s7=244
#initial stratum standard deviation
Sh1 = sd(data$inventory[1:s1])
Sh2 = sd(data$inventory[(s1+1):(s1+s2)])
Sh3 = sd(data$inventory[(s1+s2+1):(s1+s2+s3)])
Sh4 = sd(data$inventory[(s1+s2+s3+1):(s1+s2+s3+s4)])
Sh5 = sd(data$inventory[(s1+s2+s3+s4+1):(s1+s2+s3+s4+s5)])
Sh6 = sd(data$inventory[(s1+s2+s3+s4+s5+1):(s1+s2+s3+s4+s5+s6)])
Sh7 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+1):9760])

disrepancy=0.38

a=TRUE
i=0
while (a){
  k=1/(1/Sh1+1/Sh2+1/Sh3+1/Sh4+1/Sh5+1/Sh6+1/Sh7)
  s1 = round(k*popSize/Sh1,0)
  s2 = round(k*popSize/Sh2,0)
  s3 = round(k*popSize/Sh3,0)
  s4 = round(k*popSize/Sh4,0)
  s5 = round(k*popSize/Sh5,0)
  s6 = round(k*popSize/Sh6,0)
  s7 = round(k*popSize/Sh7,0)
  if((s1+s2+s3+s4+s5+s6+s7)<9760){
    less=9760-(s1+s2+s3+s4+s5+s6+s7)
    s1 = round((k*popSize/Sh1)+less,0)
  }
  if((s1+s2+s3+s4+s5+s6+s7)>9760){
    more=(s1+s2+s3+s4+s5+s6+s7)-9760
    s1 = round((k*popSize/Sh1)-more,0)
  }
  Sh1 = sd(data$inventory[1:s1])
  Sh2 = sd(data$inventory[(s1+1):(s1+s2)])
  Sh3 = sd(data$inventory[(s1+s2+1):(s1+s2+s3)])
  Sh4 = sd(data$inventory[(s1+s2+s3+1):(s1+s2+s3+s4)])
  Sh5 = sd(data$inventory[(s1+s2+s3+s4+1):(s1+s2+s3+s4+s5)])
  Sh6 = sd(data$inventory[(s1+s2+s3+s4+s5+1):(s1+s2+s3+s4+s5+s6)])
  Sh7 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+1):9760])
  i=i+1
  k1=s1*Sh1/popSize
  k2=s2/popSize*Sh2
  k3=s3/popSize*Sh3
  k4=s4/popSize*Sh4
  k5=s5/popSize*Sh5
  k6=s6/popSize*Sh6
  k7=s7/popSize*Sh7
  if ( min(k2,k1)/max(k2,k1)>disrepancy &
       min(k2,k3)/max(k2,k3)>disrepancy &
       min(k3,k4)/max(k3,k4)>disrepancy &
       min(k4,k5)/max(k4,k5)>disrepancy &
       min(k5,k6)/max(k5,k6)>disrepancy &
       min(k7,k6)/max(k7,k6)>disrepancy){
    print("finished in")
    print(i)
    print(s1)
    print(s2)
    print(s3)
    print(s4)
    print(s5)
    print(s6)
    print(s7)
    print(s1+s2+s3+s4+s5+s6+s7)
    print(Sh1)
    print(Sh2)
    print(Sh3)
    print(Sh4)
    print(Sh5)
    print(Sh6)
    print(Sh7)
    a=FALSE
  }
  if(i==100000){
    print("didn't find")
    a=FALSE
  }
  
}
print(k1)
print(k2)
print(k3)
print(k4)
print(k5)
print(k6)
print(k7)

#8
#_____________________________________________________________________________________________________
strata.cumrootf(data$inventory,n=500, Ls=8) #didn't work because produced value 0 for 8th stratum
strata.LH(data$inventory,n=500, Ls=8)
#initial values for stratum size


#LH method
s1=1426
s2=3139
s3=2126
s4=1123
s5=775
s6=599
s7=324
s8=248
#initial stratum standard deviation
Sh1 = sd(data$inventory[1:s1])
Sh2 = sd(data$inventory[(s1+1):(s1+s2)])
Sh3 = sd(data$inventory[(s1+s2+1):(s1+s2+s3)])
Sh4 = sd(data$inventory[(s1+s2+s3+1):(s1+s2+s3+s4)])
Sh5 = sd(data$inventory[(s1+s2+s3+s4+1):(s1+s2+s3+s4+s5)])
Sh6 = sd(data$inventory[(s1+s2+s3+s4+s5+1):(s1+s2+s3+s4+s5+s6)])
Sh7 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+1):(s1+s2+s3+s4+s5+s6+s7)])
Sh8 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+s7+1):9760])

disrepancy=0.1

a=TRUE
i=0
while (a){
  k=1/(1/Sh1+1/Sh2+1/Sh3+1/Sh4+1/Sh5+1/Sh6+1/Sh7+1/Sh8)
  s1 = round(k*popSize/Sh1,0)
  s2 = round(k*popSize/Sh2,0)
  s3 = round(k*popSize/Sh3,0)
  s4 = round(k*popSize/Sh4,0)
  s5 = round(k*popSize/Sh5,0)
  s6 = round(k*popSize/Sh6,0)
  s7 = round(k*popSize/Sh7,0)
  s8 = round(k*popSize/Sh8,0)
  if((s1+s2+s3+s4+s5+s6+s7+s8)<9760){
    less=9760-(s1+s2+s3+s4+s5+s6+s7+s8)
    s1 = round((k*popSize/Sh1)+less,0)
  }
  if((s1+s2+s3+s4+s5+s6+s7+s8)>9760){
    more=(s1+s2+s3+s4+s5+s6+s7+s8)-9760
    s1 = round((k*popSize/Sh1)-more,0)
  }
  Sh1 = sd(data$inventory[1:s1])
  Sh2 = sd(data$inventory[(s1+1):(s1+s2)])
  Sh3 = sd(data$inventory[(s1+s2+1):(s1+s2+s3)])
  Sh4 = sd(data$inventory[(s1+s2+s3+1):(s1+s2+s3+s4)])
  Sh5 = sd(data$inventory[(s1+s2+s3+s4+1):(s1+s2+s3+s4+s5)])
  Sh7 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+1):(s1+s2+s3+s4+s5+s6+s7)])
  Sh8 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+s7+1):9760])
  
  i=i+1
  k1=s1*Sh1/popSize
  k2=s2/popSize*Sh2
  k3=s3/popSize*Sh3
  k4=s4/popSize*Sh4
  k5=s5/popSize*Sh5
  k6=s6/popSize*Sh6
  k7=s7/popSize*Sh7
  k8=s8/popSize*Sh8
  if ( min(k2,k1)/max(k2,k1)>disrepancy &
       min(k2,k3)/max(k2,k3)>disrepancy &
       min(k3,k4)/max(k3,k4)>disrepancy &
       min(k4,k5)/max(k4,k5)>disrepancy &
       min(k5,k6)/max(k5,k6)>disrepancy &
       min(k7,k6)/max(k7,k6)>disrepancy &
       min(k7,k8)/max(k7,k8)>disrepancy){
    print("finished in")
    print(i)
    print(s1)
    print(s2)
    print(s3)
    print(s4)
    print(s5)
    print(s6)
    print(s7)
    print(s8)
    print(s1+s2+s3+s4+s5+s6+s7+s8)
    print(Sh1)
    print(Sh2)
    print(Sh3)
    print(Sh4)
    print(Sh5)
    print(Sh6)
    print(Sh7)
    print(Sh8)
    a=FALSE
  }
  if(i==100000){
    print("didn't find")
    a=FALSE
  }
  
}
print(k1)
print(k2)
print(k3)
print(k4)
print(k5)
print(k6)
print(k7)
print(k8)

#9
#_____________________________________________________________________________________________________
strata.cumrootf(data$inventory,n=500, Ls=9) #didn't work because produced value 0 for 8th stratum
strata.LH(data$inventory,n=500, Ls=9)
#initial values for stratum size


#LH method
s1=1174
s2=3163
s3=1089
s4=1455
s5=1029
s6=703
s7=575
s8=316
s9 =256
#initial stratum standard deviation
Sh1 = sd(data$inventory[1:s1])
Sh2 = sd(data$inventory[(s1+1):(s1+s2)])
Sh3 = sd(data$inventory[(s1+s2+1):(s1+s2+s3)])
Sh4 = sd(data$inventory[(s1+s2+s3+1):(s1+s2+s3+s4)])
Sh5 = sd(data$inventory[(s1+s2+s3+s4+1):(s1+s2+s3+s4+s5)])
Sh6 = sd(data$inventory[(s1+s2+s3+s4+s5+1):(s1+s2+s3+s4+s5+s6)])
Sh7 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+1):(s1+s2+s3+s4+s5+s6+s7)])
Sh8 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+s7+1):(s1+s2+s3+s4+s5+s6+s7+s8)])
Sh9 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+s7+s8+1):9760])

disrepancy = 0.91
a=TRUE
i=0
while (a){
  k=1/(1/Sh1+1/Sh2+1/Sh3+1/Sh4+1/Sh5+1/Sh6+1/Sh7+1/Sh8 +1/Sh9)
  s1 = round(k*popSize/Sh1,0)
  s2 = round(k*popSize/Sh2,0)
  s3 = round(k*popSize/Sh3,0)
  s4 = round(k*popSize/Sh4,0)
  s5 = round(k*popSize/Sh5,0)
  s6 = round(k*popSize/Sh6,0)
  s7 = round(k*popSize/Sh7,0)
  s8 = round(k*popSize/Sh8,0)
  s9 = round(k*popSize/Sh9,0)
  if((s1+s2+s3+s4+s5+s6+s7+s8+s9)<9760){
    less=9760-(s1+s2+s3+s4+s5+s6+s7+s8+s9)
    s1 = round((k*popSize/Sh1)+less,0)
  }
  if((s1+s2+s3+s4+s5+s6+s7+s8+s9)>9760){
    more=(s1+s2+s3+s4+s5+s6+s7+s8+s9)-9760
    s1 = round((k*popSize/Sh1)-more,0)
  }
  Sh1 = sd(data$inventory[1:s1])
  Sh2 = sd(data$inventory[(s1+1):(s1+s2)])
  Sh3 = sd(data$inventory[(s1+s2+1):(s1+s2+s3)])
  Sh4 = sd(data$inventory[(s1+s2+s3+1):(s1+s2+s3+s4)])
  Sh5 = sd(data$inventory[(s1+s2+s3+s4+1):(s1+s2+s3+s4+s5)])
  Sh7 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+1):(s1+s2+s3+s4+s5+s6+s7)])
  Sh8 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+s7+1):(s1+s2+s3+s4+s5+s6+s7+s8)])
  Sh9 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+s7+s8+1):9760])
  

  i=i+1
  k1=s1/popSize*Sh1
  k2=s2/popSize*Sh2
  k3=s3/popSize*Sh3
  k4=s4/popSize*Sh4
  k5=s5/popSize*Sh5
  k6=s6/popSize*Sh6
  k7=s7/popSize*Sh7
  k8=s8/popSize*Sh8
  k9=s9/popSize*Sh9
  if ( min(k2,k1)/max(k2,k1)>disrepancy &
       min(k2,k3)/max(k2,k3)>disrepancy &
       min(k3,k4)/max(k3,k4)>disrepancy &
       min(k4,k5)/max(k4,k5)>disrepancy &
       min(k5,k6)/max(k5,k6)>disrepancy &
       min(k7,k6)/max(k7,k6)>disrepancy &
       min(k7,k8)/max(k7,k8)>disrepancy &
       min(k9,k8)/max(k9,k8)>disrepancy){
    print("finished in")
    print(i)
    print(s1)
    print(s2)
    print(s3)
    print(s4)
    print(s5)
    print(s6)
    print(s7)
    print(s8)
    print(s9)
    print(s1+s2+s3+s4+s5+s6+s7+s8+s9)
    print(Sh1)
    print(Sh2)
    print(Sh3)
    print(Sh4)
    print(Sh5)
    print(Sh6)
    print(Sh7)
    print(Sh8)
    print(Sh9)
    a=FALSE
  }
  if(i==100000){
    print("didn't find")
    a=FALSE
  }
  
}
print(k1)
print(k2)
print(k3)
print(k4)
print(k5)
print(k6)
print(k7)
print(k8)
print(k9)

#10
#_____________________________________________________________________________________________________
strata.cumrootf(data$inventory,n=500, Ls=10) #didn't work because produced value 0 for 9th stratum
strata.LH(data$inventory,n=500, Ls=10)
#initial values for stratum size


#LH method
s1=1165
s2=3165
s3=1072
s4=1397
s5=982
s6=724
s7=535
s8=297
s9 =180
s10 = 243
#initial stratum standard deviation
Sh1 = sd(data$inventory[1:s1])
Sh2 = sd(data$inventory[(s1+1):(s1+s2)])
Sh3 = sd(data$inventory[(s1+s2+1):(s1+s2+s3)])
Sh4 = sd(data$inventory[(s1+s2+s3+1):(s1+s2+s3+s4)])
Sh5 = sd(data$inventory[(s1+s2+s3+s4+1):(s1+s2+s3+s4+s5)])
Sh6 = sd(data$inventory[(s1+s2+s3+s4+s5+1):(s1+s2+s3+s4+s5+s6)])
Sh7 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+1):(s1+s2+s3+s4+s5+s6+s7)])
Sh8 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+s7+1):(s1+s2+s3+s4+s5+s6+s7+s8)])
Sh9 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+s7+s8+1):(s1+s2+s3+s4+s5+s6+s7+s8+s9)])
Sh10 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+s7+s8+s9+1):9760])

disrepancy = 0.7
a=TRUE
i=0
while (a){
  k=1/(1/Sh1+1/Sh2+1/Sh3+1/Sh4+1/Sh5+1/Sh6+1/Sh7+1/Sh8 +1/Sh9 + 1/Sh10)
  s1 = round(k*popSize/Sh1,0)
  s2 = round(k*popSize/Sh2,0)
  s3 = round(k*popSize/Sh3,0)
  s4 = round(k*popSize/Sh4,0)
  s5 = round(k*popSize/Sh5,0)
  s6 = round(k*popSize/Sh6,0)
  s7 = round(k*popSize/Sh7,0)
  s8 = round(k*popSize/Sh8,0)
  s9 = round(k*popSize/Sh9,0)
  s10 = round(k*popSize/Sh10,0)
  if((s1+s2+s3+s4+s5+s6+s7+s8+s9+s10)<9760){
    less=9760-(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10)
    s1 = round((k*popSize/Sh1)+less,0)
  }
  if((s1+s2+s3+s4+s5+s6+s7+s8+s9+s10)>9760){
    more=(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10)-9760
    s1 = round((k*popSize/Sh1)-more,0)
  }
  Sh1 = sd(data$inventory[1:s1])
  Sh2 = sd(data$inventory[(s1+1):(s1+s2)])
  Sh3 = sd(data$inventory[(s1+s2+1):(s1+s2+s3)])
  Sh4 = sd(data$inventory[(s1+s2+s3+1):(s1+s2+s3+s4)])
  Sh5 = sd(data$inventory[(s1+s2+s3+s4+1):(s1+s2+s3+s4+s5)])
  Sh7 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+1):(s1+s2+s3+s4+s5+s6+s7)])
  Sh8 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+s7+1):(s1+s2+s3+s4+s5+s6+s7+s8)])
  Sh9 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+s7+s8+1):(s1+s2+s3+s4+s5+s6+s7+s8+s9)])
  Sh10 = sd(data$inventory[(s1+s2+s3+s4+s5+s6+s7+s8+s9+1):9760])
  
  i=i+1
  k1=s1*Sh1/popSize
  k2=s2/popSize*Sh2
  k3=s3/popSize*Sh3
  k4=s4/popSize*Sh4
  k5=s5/popSize*Sh5
  k6=s6/popSize*Sh6
  k7=s7/popSize*Sh7
  k8=s8/popSize*Sh8
  k9=s9/popSize*Sh9
  k10=s10/popSize*Sh10
  if ( min(k2,k1)/max(k2,k1)>disrepancy &
       min(k2,k3)/max(k2,k3)>disrepancy &
       min(k3,k4)/max(k3,k4)>disrepancy &
       min(k4,k5)/max(k4,k5)>disrepancy &
       min(k5,k6)/max(k5,k6)>disrepancy &
       min(k7,k6)/max(k7,k6)>disrepancy &
       min(k7,k8)/max(k7,k8)>disrepancy &
       min(k9,k8)/max(k9,k8)>disrepancy &
       min(k9,k10)/max(k9,k10)>disrepancy){
    print("finished in")
    print(i)
    print(s1)
    print(s2)
    print(s3)
    print(s4)
    print(s5)
    print(s6)
    print(s7)
    print(s8)
    print(s9)
    print(s10)
    print(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10)
    print(Sh1)
    print(Sh2)
    print(Sh3)
    print(Sh4)
    print(Sh5)
    print(Sh6)
    print(Sh7)
    print(Sh8)
    print(Sh9)
    print(Sh10)
    a=FALSE
  }
  if(i==100000){
    print("didn't find")
    a=FALSE
  }
  
}
print(k1)
print(k2)
print(k3)
print(k4)
print(k5)
print(k6)
print(k7)
print(k8)
print(k9)
print(k10)
