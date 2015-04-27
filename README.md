Letter Recognition
==================

The objective is to identify each of a large number of black-and-white
rectangular pixel displays as one of the 26 capital letters in the English
alphabet. The character images were based on 20 different fonts and each
letter within these 20 fonts was randomly distorted to produce a file of
20.000 unique stimuli. Each stimulus was converted into 16 primitive numerical
attributes (statistical moments and edge counts) which were then scaled to fit
into a range of integer values from 0 through 15.


Data Set Information
--------------------

 - [Letter Recognition Data Set](https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.names)
 - [Data Folder](https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/)

<table>
  <tr>
    <td>Data Set Characteristics</td>
    <td>Multivariate</td>
  </tr>
  <tr>
    <td>Attribute Characteristics</td>
    <td>Integer</td>
  </tr>
  <tr>
    <td>Number of Attributes</td>
    <td>16</td>
  </tr>
  <tr>
    <td>Number of Instances</td>
    <td>20.000</td>
  </tr>
  <tr>
    <td>Associated Tasks</td>
    <td>Classification</td>
  </tr>
</table>

Results
-------
We are going to measure the accuracy rate into the *test*
subset(4.000 instances)

| Technique      | Test Rate |
|----------------|-----------|
|            LDA |  0.8955   |
|            QDA |  0.9497   |
|            KNN |  0.9641   |
|   Tree(simple) |  0.4799   |
|        Bagging |  0.9454   |
| Random Forests |  0.9915   |
|       Boosting |  0.5805   |
|            SVM |  0.9487   |

Source
------

David J. Slate

Odesta Corporation;

1890 Maple Ave; Suite 115;

Evanston, IL 60201
