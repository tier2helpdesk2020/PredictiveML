```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library(ggplot2)
library(corrplot)
library(randomForest)
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

 assing the data from pml-training.csv and pml-testing.csv files to the variables.



```r
setwd("C:/Users/nicolas/Downloads")

training <- read.csv("pml-training.csv",na.strings = c("NA", ""))

test <- read.csv("pml-testing.csv",na.strings = c("NA", ""))
```

 We search in the variables those columns that dont contibute to the analysis 
 because they are character or non-numerical.


```r
sapply(test,class)
```

```
##                        X                user_name     raw_timestamp_part_1 
##                "integer"                 "factor"                "integer" 
##     raw_timestamp_part_2           cvtd_timestamp               new_window 
##                "integer"                 "factor"                 "factor" 
##               num_window                roll_belt               pitch_belt 
##                "integer"                "numeric"                "numeric" 
##                 yaw_belt         total_accel_belt       kurtosis_roll_belt 
##                "numeric"                "integer"                "logical" 
##      kurtosis_picth_belt        kurtosis_yaw_belt       skewness_roll_belt 
##                "logical"                "logical"                "logical" 
##     skewness_roll_belt.1        skewness_yaw_belt            max_roll_belt 
##                "logical"                "logical"                "logical" 
##           max_picth_belt             max_yaw_belt            min_roll_belt 
##                "logical"                "logical"                "logical" 
##           min_pitch_belt             min_yaw_belt      amplitude_roll_belt 
##                "logical"                "logical"                "logical" 
##     amplitude_pitch_belt       amplitude_yaw_belt     var_total_accel_belt 
##                "logical"                "logical"                "logical" 
##            avg_roll_belt         stddev_roll_belt            var_roll_belt 
##                "logical"                "logical"                "logical" 
##           avg_pitch_belt        stddev_pitch_belt           var_pitch_belt 
##                "logical"                "logical"                "logical" 
##             avg_yaw_belt          stddev_yaw_belt             var_yaw_belt 
##                "logical"                "logical"                "logical" 
##             gyros_belt_x             gyros_belt_y             gyros_belt_z 
##                "numeric"                "numeric"                "numeric" 
##             accel_belt_x             accel_belt_y             accel_belt_z 
##                "integer"                "integer"                "integer" 
##            magnet_belt_x            magnet_belt_y            magnet_belt_z 
##                "integer"                "integer"                "integer" 
##                 roll_arm                pitch_arm                  yaw_arm 
##                "numeric"                "numeric"                "numeric" 
##          total_accel_arm            var_accel_arm             avg_roll_arm 
##                "integer"                "logical"                "logical" 
##          stddev_roll_arm             var_roll_arm            avg_pitch_arm 
##                "logical"                "logical"                "logical" 
##         stddev_pitch_arm            var_pitch_arm              avg_yaw_arm 
##                "logical"                "logical"                "logical" 
##           stddev_yaw_arm              var_yaw_arm              gyros_arm_x 
##                "logical"                "logical"                "numeric" 
##              gyros_arm_y              gyros_arm_z              accel_arm_x 
##                "numeric"                "numeric"                "integer" 
##              accel_arm_y              accel_arm_z             magnet_arm_x 
##                "integer"                "integer"                "integer" 
##             magnet_arm_y             magnet_arm_z        kurtosis_roll_arm 
##                "integer"                "integer"                "logical" 
##       kurtosis_picth_arm         kurtosis_yaw_arm        skewness_roll_arm 
##                "logical"                "logical"                "logical" 
##       skewness_pitch_arm         skewness_yaw_arm             max_roll_arm 
##                "logical"                "logical"                "logical" 
##            max_picth_arm              max_yaw_arm             min_roll_arm 
##                "logical"                "logical"                "logical" 
##            min_pitch_arm              min_yaw_arm       amplitude_roll_arm 
##                "logical"                "logical"                "logical" 
##      amplitude_pitch_arm        amplitude_yaw_arm            roll_dumbbell 
##                "logical"                "logical"                "numeric" 
##           pitch_dumbbell             yaw_dumbbell   kurtosis_roll_dumbbell 
##                "numeric"                "numeric"                "logical" 
##  kurtosis_picth_dumbbell    kurtosis_yaw_dumbbell   skewness_roll_dumbbell 
##                "logical"                "logical"                "logical" 
##  skewness_pitch_dumbbell    skewness_yaw_dumbbell        max_roll_dumbbell 
##                "logical"                "logical"                "logical" 
##       max_picth_dumbbell         max_yaw_dumbbell        min_roll_dumbbell 
##                "logical"                "logical"                "logical" 
##       min_pitch_dumbbell         min_yaw_dumbbell  amplitude_roll_dumbbell 
##                "logical"                "logical"                "logical" 
## amplitude_pitch_dumbbell   amplitude_yaw_dumbbell     total_accel_dumbbell 
##                "logical"                "logical"                "integer" 
##       var_accel_dumbbell        avg_roll_dumbbell     stddev_roll_dumbbell 
##                "logical"                "logical"                "logical" 
##        var_roll_dumbbell       avg_pitch_dumbbell    stddev_pitch_dumbbell 
##                "logical"                "logical"                "logical" 
##       var_pitch_dumbbell         avg_yaw_dumbbell      stddev_yaw_dumbbell 
##                "logical"                "logical"                "logical" 
##         var_yaw_dumbbell         gyros_dumbbell_x         gyros_dumbbell_y 
##                "logical"                "numeric"                "numeric" 
##         gyros_dumbbell_z         accel_dumbbell_x         accel_dumbbell_y 
##                "numeric"                "integer"                "integer" 
##         accel_dumbbell_z        magnet_dumbbell_x        magnet_dumbbell_y 
##                "integer"                "integer"                "integer" 
##        magnet_dumbbell_z             roll_forearm            pitch_forearm 
##                "integer"                "numeric"                "numeric" 
##              yaw_forearm    kurtosis_roll_forearm   kurtosis_picth_forearm 
##                "numeric"                "logical"                "logical" 
##     kurtosis_yaw_forearm    skewness_roll_forearm   skewness_pitch_forearm 
##                "logical"                "logical"                "logical" 
##     skewness_yaw_forearm         max_roll_forearm        max_picth_forearm 
##                "logical"                "logical"                "logical" 
##          max_yaw_forearm         min_roll_forearm        min_pitch_forearm 
##                "logical"                "logical"                "logical" 
##          min_yaw_forearm   amplitude_roll_forearm  amplitude_pitch_forearm 
##                "logical"                "logical"                "logical" 
##    amplitude_yaw_forearm      total_accel_forearm        var_accel_forearm 
##                "logical"                "integer"                "logical" 
##         avg_roll_forearm      stddev_roll_forearm         var_roll_forearm 
##                "logical"                "logical"                "logical" 
##        avg_pitch_forearm     stddev_pitch_forearm        var_pitch_forearm 
##                "logical"                "logical"                "logical" 
##          avg_yaw_forearm       stddev_yaw_forearm          var_yaw_forearm 
##                "logical"                "logical"                "logical" 
##          gyros_forearm_x          gyros_forearm_y          gyros_forearm_z 
##                "numeric"                "numeric"                "numeric" 
##          accel_forearm_x          accel_forearm_y          accel_forearm_z 
##                "integer"                "integer"                "integer" 
##         magnet_forearm_x         magnet_forearm_y         magnet_forearm_z 
##                "integer"                "integer"                "integer" 
##               problem_id 
##                "integer"
```


 Using What we saw in the previous part we should remove some of the columns that not make sence to have with the girp function, because thay are not numeric. and create a new vector from this operation.
 

```r
training_aux <- training[, -(grep("timestamp|X|user_name|num_window|new_window", names(training)))]
test_aux <- test[, -(grep("timestamp|X|user_name|num_window|new_window", names(test)))]
```
 
 
Many of the columns we load have . NA`s values. We take out  from the variables those data with NA's values
The NA`s records make our machine learning algorithm less precise. Finally we update the vector with out any NA`s values.

```r
NAs <- apply(training_aux, 2, function(x) {
    sum(is.na(x))})

training_aux <- training_aux[, which(NAs == 0)]
test_aux <- test_aux[, which(NAs == 0)]
```


 We split the data into two variables. First the training data with the 60 % and the test data with 40%. 


```r
training.idx <- training_aux[createDataPartition(y = training_aux$classe, p = 0.6, list = FALSE), ]  
test.idx <- training_aux[-createDataPartition(y = training_aux$classe, p = 0.6, list = FALSE), ]  
```


We take a look how our variables shrink, with out the NA`s values.


```r
dim(training.idx)
```

```
## [1] 11776    53
```

```r
head(training.idx)
```

```
##    roll_belt pitch_belt yaw_belt total_accel_belt gyros_belt_x
## 2       1.41       8.07    -94.4                3         0.02
## 6       1.45       8.06    -94.4                3         0.02
## 8       1.42       8.13    -94.4                3         0.02
## 9       1.43       8.16    -94.4                3         0.02
## 10      1.45       8.17    -94.4                3         0.03
## 12      1.43       8.18    -94.4                3         0.02
##    gyros_belt_y gyros_belt_z accel_belt_x accel_belt_y accel_belt_z
## 2             0        -0.02          -22            4           22
## 6             0        -0.02          -21            4           21
## 8             0        -0.02          -22            4           21
## 9             0        -0.02          -20            2           24
## 10            0         0.00          -21            4           22
## 12            0        -0.02          -22            2           23
##    magnet_belt_x magnet_belt_y magnet_belt_z roll_arm pitch_arm yaw_arm
## 2             -7           608          -311     -128      22.5    -161
## 6              0           603          -312     -128      22.0    -161
## 8             -2           603          -313     -128      21.8    -161
## 9              1           602          -312     -128      21.7    -161
## 10            -3           609          -308     -128      21.6    -161
## 12            -2           602          -319     -128      21.5    -161
##    total_accel_arm gyros_arm_x gyros_arm_y gyros_arm_z accel_arm_x
## 2               34        0.02       -0.02       -0.02        -290
## 6               34        0.02       -0.03        0.00        -289
## 8               34        0.02       -0.02        0.00        -289
## 9               34        0.02       -0.03       -0.02        -288
## 10              34        0.02       -0.03       -0.02        -288
## 12              34        0.02       -0.03        0.00        -288
##    accel_arm_y accel_arm_z magnet_arm_x magnet_arm_y magnet_arm_z
## 2          110        -125         -369          337          513
## 6          111        -122         -369          342          513
## 8          111        -124         -372          338          510
## 9          109        -122         -369          341          518
## 10         110        -124         -376          334          516
## 12         111        -123         -363          343          520
##    roll_dumbbell pitch_dumbbell yaw_dumbbell total_accel_dumbbell
## 2          13.13         -70.64       -84.71                   37
## 6          13.38         -70.82       -84.47                   37
## 8          12.75         -70.35       -85.10                   37
## 9          13.15         -70.43       -84.92                   37
## 10         13.33         -70.85       -84.45                   37
## 12         13.10         -70.46       -84.89                   37
##    gyros_dumbbell_x gyros_dumbbell_y gyros_dumbbell_z accel_dumbbell_x
## 2                 0            -0.02                0             -233
## 6                 0            -0.02                0             -234
## 8                 0            -0.02                0             -234
## 9                 0            -0.02                0             -232
## 10                0            -0.02                0             -235
## 12                0            -0.02                0             -233
##    accel_dumbbell_y accel_dumbbell_z magnet_dumbbell_x magnet_dumbbell_y
## 2                47             -269              -555               296
## 6                48             -269              -558               294
## 8                46             -272              -555               300
## 9                47             -269              -549               292
## 10               48             -270              -558               291
## 12               47             -270              -554               291
##    magnet_dumbbell_z roll_forearm pitch_forearm yaw_forearm
## 2                -64         28.3         -63.9        -153
## 6                -66         27.9         -63.9        -152
## 8                -74         27.8         -63.8        -152
## 9                -65         27.7         -63.8        -152
## 10               -69         27.7         -63.8        -152
## 12               -65         27.5         -63.8        -152
##    total_accel_forearm gyros_forearm_x gyros_forearm_y gyros_forearm_z
## 2                   36            0.02            0.00           -0.02
## 6                   36            0.02           -0.02           -0.03
## 8                   36            0.02           -0.02            0.00
## 9                   36            0.03            0.00           -0.02
## 10                  36            0.02            0.00           -0.02
## 12                  36            0.02            0.02           -0.03
##    accel_forearm_x accel_forearm_y accel_forearm_z magnet_forearm_x
## 2              192             203            -216              -18
## 6              193             203            -215               -9
## 8              193             205            -213               -9
## 9              193             204            -214              -16
## 10             190             205            -215              -22
## 12             191             203            -215              -11
##    magnet_forearm_y magnet_forearm_z classe
## 2               661              473      A
## 6               660              478      A
## 8               660              474      A
## 9               653              476      A
## 10              656              473      A
## 12              657              478      A
```

 this graph is usefull to look at the correlation with some variables to each others. Note that the dark blue indicated strong correlation and red negative correlation. Our dataset is 11776 X 53 and one of those is Classe, the variable we want to predict.
 This graph shows us nice information to implement a Random Forest Study.


```r
Graph <- training.idx

NAs <- apply(training.idx, 2, function(x) {
    sum(is.na(x))
})
Graph<- training.idx[, which(NAs == 0)]



CorrePlot = cor( Graph[,-c(grep("timestamp|X|user_name|num_window|new_window",names(Graph)), length(Graph))])
corrplot(CorrePlot, method="circle",tl.cex=0.9)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

At this point we create our model using Random Forest and the set of data originally given but reduced.
We train our model over the dataset using the "boot" method inside trainControl.


```r
set.seed(4242)

MyModel.Forest<- train(training.idx$classe ~ ., data = training.idx, method = "rf", 
    prof = TRUE, trControl = trainControl(method = "boot", number = 5, allowParallel = TRUE))

summary(MyModel.Forest)
```

```
##                 Length Class      Mode     
## call                5  -none-     call     
## type                1  -none-     character
## predicted       11776  factor     numeric  
## err.rate         3000  -none-     numeric  
## confusion          30  -none-     numeric  
## votes           58880  matrix     numeric  
## oob.times       11776  -none-     numeric  
## classes             5  -none-     character
## importance         52  -none-     numeric  
## importanceSD        0  -none-     NULL     
## localImportance     0  -none-     NULL     
## proximity           0  -none-     NULL     
## ntree               1  -none-     numeric  
## mtry                1  -none-     numeric  
## forest             14  -none-     list     
## y               11776  factor     numeric  
## test                0  -none-     NULL     
## inbag               0  -none-     NULL     
## xNames             52  -none-     character
## problemType         1  -none-     character
## tuneValue           1  data.frame list     
## obsLevels           5  -none-     character
```

```r
MyModel.Forest
```

```
## Random Forest 
## 
## 11776 samples
##    52 predictors
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Bootstrapped (5 reps) 
## 
## Summary of sample sizes: 11776, 11776, 11776, 11776, 11776 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##   2     1         1      0.003        0.003   
##   30    1         1      0.002        0.003   
##   50    1         1      0.003        0.004   
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 27.
```

```r
MyModel.Forest.Final<- MyModel.Forest$results
round(max(MyModel.Forest.Final$Accuracy), 3) * 100
```

```
## [1] 98.6
```

We got a very nice result of the occuracy at the first attemp: 98.7 %. That`s ok.


```r
varImpPlot(MyModel.Forest$finalModel, 
            main = "Principal Components with high importance")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

## The cross validation study of our model.


```r
test.idx$predRight <- (predict(MyModel.Forest, test.idx))== test.idx$classe
table(predict(MyModel.Forest, test.idx), test.idx$classe)
```

```
##    
##        A    B    C    D    E
##   A 2229    2    0    0    0
##   B    2 1513    1    0    1
##   C    0    2 1366   13    2
##   D    0    1    1 1271    1
##   E    1    0    0    2 1438
```

```r
CrossValidated<- postResample((predict(MyModel.Forest, test.idx)), test.idx$classe)
CrossValidated
```

```
## Accuracy    Kappa 
##   0.9963   0.9953
```

We got a nice high level of accuracy.

# We try out with the ConfussionMatrix

```r
set.seed(4242)
CrossValidatedError <- confusionMatrix((predict(MyModel.Forest, test.idx)), test.idx$classe)
CrossValidatedError
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2229    2    0    0    0
##          B    2 1513    1    0    1
##          C    0    2 1366   13    2
##          D    0    1    1 1271    1
##          E    1    0    0    2 1438
## 
## Overall Statistics
##                                         
##                Accuracy : 0.996         
##                  95% CI : (0.995, 0.998)
##     No Information Rate : 0.284         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.995         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.999    0.997    0.999    0.988    0.997
## Specificity             1.000    0.999    0.997    1.000    1.000
## Pos Pred Value          0.999    0.997    0.988    0.998    0.998
## Neg Pred Value          0.999    0.999    1.000    0.998    0.999
## Prevalence              0.284    0.193    0.174    0.164    0.184
## Detection Rate          0.284    0.193    0.174    0.162    0.183
## Detection Prevalence    0.284    0.193    0.176    0.162    0.184
## Balanced Accuracy       0.999    0.998    0.998    0.994    0.998
```

```r
postResample((predict(MyModel.Forest, test.idx)), test.idx$classe)[[1]]
```

```
## [1] 0.9963
```

```r
1- postResample((predict(MyModel.Forest, test.idx)), test.idx$classe)[[1]]  
```

```
## [1] 0.003696
```

we can see that our calculus were very close with the result of the matrix. 
The accurance is 99.6%

# The 20 cases to predict



```r
Model.Prediction <- predict(MyModel.Forest, test)
Model.Prediction
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```
