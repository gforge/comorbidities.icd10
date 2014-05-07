comorbidities.icd10
===================

Comorbidities calculator for R using ICD-9 and ICD-10 based scores. Currently only icd-9 codes are implemented and tested.

Methods to categorize ICD-9-CM codes into sensible disease categories have
been developed and published by numerous authors.  Two of the most widely
used such methods are the Deyo adaptation of Charlson index and the
Elixhauser index.  This package has functions to categorize comorbidites
into the Deyo-Charlson index, the original Elixhauser index of 30
comorbidities, and the AHRQ comorbidity index (an update to the original
Elixhauser method).

In this remake the main comorbidity calculator function is the cmrbdt.calc() function. You should try to use this instead of the deprecated deyo(), elixhauser(), and ahrq() functions. The function is more flexible and allows a more unified approach to calculating comorbidities. 

The value returned consists of a vector and one or two data frames. The
vector is the total comorbidity count, or in the case of the Charlson score, the total Charlson score.  Each row in the data frame is devoted to a particular patient according to the corresponding ID-codes provided to the function. The data frame codes a
0 if the patient does not have that diagnosis and 1 if the patient does have
that diagnosis. 

This package is a work upon Paul Gerrards original comorbidities package.