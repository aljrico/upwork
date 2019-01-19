# 20190119

* New *DEATH* column defined as following: If the Survival time is smaller than 730 days, the 'Outcome' column determines whether the animal survived or not. If the survival time is above 730, the animal is labelled as ALIVE. In case of NA on the Survival column, we estimate as the difference between the last follow up and the Date1.

* CKD Stages from 1 to 4 defined by Crea_BQ threshold, amongst those whose CKD diagnosis was 'Y'. The rest are labelled as stage 0.