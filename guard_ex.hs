module BMI_Calc where

bmi_calc :: Double -> Double -> String
bmi_calc weight height
    | bmi <= 18.5 = "You underweight emo-fuck"
    | bmi <= 25 = "Normie"
    | bmi <= 30 = "You fat-fuck"
    | otherwise = "According to our evidence you might be one big ass whale"
    
    where bmi = weight / height ^ 2


