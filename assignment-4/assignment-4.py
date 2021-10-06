# -*- coding: utf-8 -*-
"""
Created on Tue Oct  8 12:20:41 2019

@author: Mike
"""

from tensorflow.keras.preprocessing.image import ImageDataGenerator
from tensorflow.keras.applications import MobileNetV2
from tensorflow.keras.layers import Convolution2D
from tensorflow.keras.layers import MaxPooling2D
from keras.utils.np_utils import to_categorical
from tensorflow.keras.layers import Dropout
from tensorflow.keras.layers import Flatten
from tensorflow.keras.layers import Dense
from tensorflow.keras.layers import Input
from tensorflow.keras.models import Model
from tensorflow.keras.models import Sequential
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.applications.mobilenet_v2 import preprocess_input
from tensorflow.keras.preprocessing.image import img_to_array
from tensorflow.keras.preprocessing.image import load_img
from tensorflow.keras.utils import to_categorical
from keras import models
from keras.activations import softmax
from keras.optimizer_v1 import SGD
import numpy as np
import matplotlib.pyplot as plt
from keras.datasets import cifar10
from keras.utils import np_utils
from keras import backend as K
from keras.models import Sequential
from keras.layers import *

# In *older* versions of Tensorflow/Keras you may need to adjust the image 
# dimension ordering. Read about channel ordering here:
#    https://machinelearningmastery.com/a-gentle-introduction-to-channels-first-and-channels-last-image-formats-for-deep-learning/

# This is the main function. You need to write the getModel and fitModel functions to pass to this.
# Call your functions 'myGetModel' and 'myFitModel'.
# The getModel function should accept an object of the CIFAR class, and return a compiled Keras CNN model. 
# In this function you will specify the network structure (including regularization) and the optimizer to 
# be used (and its parameters like learning rate), and run compile the model (in the Keras sense of running 
# model.compile).
# The fitModel function should accect two arguments. The first is the CNN model you return from your getModel 
# function, and the second is the CIFAR classed data object. It will return a trained Keras CNN model, which 
# will then be applied to the test data. In this function you will train the model, using the Keras model.fit 
# function. You will need to specify all parameters of the training algorithm (batch size, etc), and the 
# callbacks you will use (EarlyStopping and ModelCheckpoint). You will need to make sure you save and load 
# into the model the weight values of its best performing epoch.
def runImageClassification(getModel=None,fitModel=None,seed=7):
    # Fetch data. You may need to be connected to the internet the first time this is done.
    # After the first time, it should be available in your system. On the off chance this
    # is not the case on your system and you find yourself repeatedly downloading the data, 
    # you should change this code so you can load the data once and pass it to this function. 
    print("Preparing data...")
    data=CIFAR(seed)
        
    # Create model 
    print("Creating model...")
    model=getModel(data)

    # Fit model
    print("Fitting model...")
    model=fitModel(model,data)

    # Evaluate on test data
    print("Evaluating model...")
    score = model.evaluate(data.x_test, data.y_test, verbose=1)
    print('Test accuracy:', score[1])

# This is the class that wraps the CIFAR data. You will probably need to be connected to the
# internet the first time you create an object of this class, as the data will be downloaded.
# After that, the data should be stored by Keras and no downloading will be required. 
# Important fields that you will need to use are: x_train, y_train, x_valid, y_valid, input_dim and 
# num_classes. The first four of these are the training and validation data (split into features and
# target). Note that these have been made ready for use with a Keras network - check out the code
# if you are interested. The last two are the number of input features and the number of target 
# classes. These will be needed when defining your CNN.
# The only public method is the showImages function, which you can use to see some labelled images
# from the (validation) data.
# Remember that the x_test and y_test fields will be blank when your functions are run in evaluation -
# so you cannot peek at these cases!
class CIFAR:
    def __init__(self,seed=0):
        # Get and split data
        data = self.__getData(seed)
        self.x_train_raw=data[0][0]
        self.y_train_raw=data[0][1]
        self.x_valid_raw=data[1][0]
        self.y_valid_raw=data[1][1]
        self.x_test_raw=data[2][0]
        self.y_test_raw=data[2][1]
        # Record input/output dimensions
        self.num_classes=10
        self.input_dim=self.x_train_raw.shape[1:]
         # Convert data
        self.y_train = np_utils.to_categorical(self.y_train_raw, self.num_classes)
        self.y_valid = np_utils.to_categorical(self.y_valid_raw, self.num_classes)
        self.y_test = np_utils.to_categorical(self.y_test_raw, self.num_classes)
        self.x_train = self.x_train_raw.astype('float32')
        self.x_valid = self.x_valid_raw.astype('float32')
        self.x_test = self.x_test_raw.astype('float32')
        self.x_train  /= 255
        self.x_valid  /= 255
        self.x_test /= 255
        # Class names
        self.class_names=['airplane','automobile','bird','cat','deer',
               'dog','frog','horse','ship','truck']

    def __getData (self,seed=0):
        (x_train, y_train), (x_test, y_test) = cifar10.load_data()
        return self.__shuffleData(x_train,y_train,x_test,y_test,seed)
    
    def __shuffleData (self,x_train,y_train,x_test,y_test,seed=0):
        tr_perc=.75
        va_perc=.15
        x=np.concatenate((x_train,x_test))
        y=np.concatenate((y_train,y_test))
        np.random.seed(seed)
        np.random.shuffle(x)
        np.random.seed(seed)
        np.random.shuffle(y)
        indices = np.random.permutation(len(x))
        tr=round(len(x)*tr_perc)
        va=round(len(x)*va_perc)
        self.tr_indices=indices[0:tr]
        self.va_indices=indices[tr:(tr+va)]
        self.te_indices=indices[(tr+va):len(x)]
        x_tr=x[self.tr_indices,]
        x_va=x[self.va_indices,]
        x_te=x[self.te_indices,]
        y_tr=y[self.tr_indices,]
        y_va=y[self.va_indices,]
        y_te=y[self.te_indices,]
        return ((x_tr,y_tr),(x_va,y_va),(x_te,y_te))

    # Print 25 random figures from the validation data
    def showImages(self):
        images=self.x_valid_raw
        labels=self.y_valid_raw
        class_names=['airplane', 'automobile', 'bird', 'cat', 'deer',
               'dog', 'frog', 'horse', 'ship', 'truck']
        plt.figure(figsize=(10,10))
        indices=np.random.randint(0,images.shape[0],25)
        for i in range(25):
            plt.subplot(5,5,i+1)
            plt.xticks([])
            plt.yticks([])
            plt.grid(False)
            plt.imshow(images[indices[i]], cmap=plt.cm.binary)
            # The CIFAR labels happen to be arrays, 
            # which is why we need the extra index
            plt.xlabel(class_names[labels[indices[i]][0]])
        plt.show()



def myGetModel(data):
    # Specify network structure
    # Regularization
    # Specify used optimizer + parameters & learning rate
    # model.compile
    #return compiled Keras CNN model

    ## -- Kernel size: How many pixels do we minimum need to recognize the object?
    ## -- 
    model = Sequential()
    model.add(Convolution2D(16, (3, 3), activation='relu', input_shape=(32, 32, 3)))
    model.add(Convolution2D(16, (1, 1), activation='relu'))
    model.add(Convolution2D(32, (7, 7), activation='relu'))
    model.add(Convolution2D(32, (1, 1), activation='relu'))
    model.add(Dropout(0.25))
    model.add(Flatten())
    model.add(Dense(256, activation='relu'))
    model.add(Dense(10, activation='softmax'))
    model.compile(optimizer = 'adam', loss = 'categorical_crossentropy', metrics=['accuracy'])
    model.summary()

    return model

def myFitModel(model, data: CIFAR):
    # Early Stopping
    callback = tf.keras.callbacks.EarlyStopping(monitor='loss', patience=10, restore_best_weights=True)


    hist = model.fit(data.x_train, data.y_train, batch_size=16, steps_per_epoch=400, epochs=10, validation_data=(data.x_valid, data.y_valid), callbacks=[callback])
    model.save("C:/Projects/artificial-intelligence/assignment-4/models/model_1.model", save_format="h5")

    print(model.summary())

    plt.plot(hist.history['accuracy'])
    plt.plot(hist.history['val_accuracy'])
    plt.title('Model Accuracy')
    plt.ylabel('Accuracy')
    plt.xlabel('Epoch')
    plt.legend(['Train', 'Val'], loc='upper right')
    plt.show()

    return model


if __name__ == "__main__":
    runImageClassification(myGetModel, myFitModel)