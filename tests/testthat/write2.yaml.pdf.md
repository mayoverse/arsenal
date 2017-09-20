---
title: My title
author: Ethan P Heinzen
header-includes:
- \usepackage[labelformat=empty]{caption}
---



# Header 1



This is a small paragraph.




---------------------------------------------------------------------------------------
                     Male (N=916)     Female (N=583)   Total (N=1499)   p value        
------------------- ---------------- ---------------- ---------------- ----------------
**Age in Years**                                                                  0.048

&nbsp;&nbsp;&nbsp;  60.5 (11.4)      59.2 (11.7)      60 (11.5)       
Mean (SD)                                                             

&nbsp;&nbsp;&nbsp;  53, 69           52, 68           52, 68          
Q1, Q3                                                                

&nbsp;&nbsp;&nbsp;  19 - 88          22 - 88          19 - 88         
Range                                                                 

---------------------------------------------------------------------------------------



```

Call:
lm(formula = age ~ sex, data = mockstudy)

Residuals:
    Min      1Q  Median      3Q     Max 
-41.455  -7.455   0.753   8.545  28.753 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  60.4552     0.3802 159.001   <2e-16 ***
sexFemale    -1.2082     0.6097  -1.982   0.0477 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 11.51 on 1497 degrees of freedom
Multiple R-squared:  0.002617,	Adjusted R-squared:  0.00195 
F-statistic: 3.927 on 1 and 1497 DF,  p-value: 0.04769


```



