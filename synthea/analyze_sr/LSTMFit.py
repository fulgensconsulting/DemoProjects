""" Using the temporal data generated in BuildLSTMData.py, fit recurrent neural
    network for sequence classification on Myocardial infarction. Using tensorflow
    with keras layers.

    Author: Seth Rhoades
"""

import sys
import pandas as pd
import numpy as np
import tensorflow as tf

def main(xtrain, ytrain, xtest, ytest, batchsize, nepochs):

    model = tf.keras.models.Sequential([
            tf.keras.layers.LSTM(256, return_sequences=True, 
                input_shape = (xtrain.shape[1], xtrain.shape[2])),
            tf.keras.layers.LSTM(64),
            tf.keras.layers.Dense(64, activation=tf.nn.relu),
            tf.keras.layers.BatchNormalization(),
            tf.keras.layers.Dropout(0.25),
            tf.keras.layers.Dense(32, activation=tf.nn.relu),
            tf.keras.layers.BatchNormalization(),
            tf.keras.layers.Dropout(0.25),
            tf.keras.layers.Dense(1, activation='sigmoid'),
    ])
    optimizer = tf.train.AdamOptimizer(1e-4)
    model.compile(optimizer=optimizer, loss='binary_crossentropy', 
        metrics=['accuracy'])

    with tf.device('/device:GPU:0'):
        model.fit(xtrain, ytrain, epochs=nepochs, batch_size=batchsize, verbose=1)

    ypreds = model.predict(xtest)

    ypreds = ypreds.reshape(ytest.shape)
    accDF = pd.DataFrame([ytest, ypreds]).T
    accDF.columns = ['yTest', 'yPredScores']

    return accDF

if __name__ == '__main__':

    try:
        xtrainfile = sys.argv[1]
        ytrainfile = sys.argv[2]
        xtestfile = sys.argv[3]
        ytestfile = sys.argv[4]
    except IndexError:
        xtrainfile = 'xTrain.txt'
        ytrainfile = 'yTrain.txt'
        xtestfile = 'xTest.txt'
        ytestfile = 'yTest.txt'

    trainLength = 3357
    testLength =  1119
    nSteps = 5
    nVars = 258
    batchSize = 256
    nEpochs = 200

    xTrain = np.loadtxt(xtrainfile).reshape(trainLength, nSteps, nVars)
    xTest = np.loadtxt(xtestfile).reshape(testLength, nSteps, nVars)
    yTrain = np.loadtxt(ytrainfile).reshape(trainLength, )
    yTest = np.loadtxt(ytestfile).reshape(testLength, )

    modelPredictions = main(xTrain, yTrain, xTest, yTest, batchSize, nEpochs)
    
    modelPredictions.to_csv('yPredictions.csv')

#Low 70s seems to be the best I can do here, complex vs simple model doesn't matter
#much, although 2-layer LSTM helps a bit, beyond that it levels off