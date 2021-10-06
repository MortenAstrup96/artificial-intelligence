# TensorFlow and tf.keras
import tensorflow as tf

from tensorflow.keras.applications.resnet50 import preprocess_input, decode_predictions
from tensorflow.keras.preprocessing import image

# Helper libraries
import numpy as np
import matplotlib.pyplot as plt
from tensorflow.python.keras.backend import switch

#img_path = "dog4.png"


print(tf.__version__)

def classify(img_path):
    img = image.load_img(img_path, target_size=(32, 32))
    #plt.imshow(img)
    #plt.show()
    img_array = image.img_to_array(img)

    img_batch = np.expand_dims(img_array, axis=0)

    img_preprocessed = preprocess_input(img_batch)

    model = tf.keras.models.load_model("C:/Projects/artificial-intelligence/assignment-4/models/model_1.model")
    (airplane, automobile, bird, cat, deer, dog, frog, horse, ship, truck) = model.predict(img_preprocessed)[0]
    print("Airplane: ", airplane)
    print("Automobile: ", automobile)
    print("Bird: ", bird)
    print("Cat: ", cat)
    print("Deer: ", deer)
    print("Dog: ", dog)
    print("Frog: ", frog)
    print("Horse: ", horse)
    print("Ship: ", ship)
    print("Truck: ", truck)
    

    
    #print(prediction)[0]
    
    #print(decode_predictions(prediction, top=3)[0])

print("Airplane --")
classify("C:/Projects/artificial-intelligence/assignment-4/images/airplane1.png")
print("")
print("")
print("Automobile --")
classify("C:/Projects/artificial-intelligence/assignment-4/images/automobile1.png")
print("")
print("")
print("Bird --")
classify("C:/Projects/artificial-intelligence/assignment-4/images/bird1.png")
print("")
print("")
print("Cat --")
classify("C:/Projects/artificial-intelligence/assignment-4/images/cat1.png")
print("")
print("")
print("Deer --")
classify("C:/Projects/artificial-intelligence/assignment-4/images/deer1.png")
print("")
print("")
print("Dog --")
classify("C:/Projects/artificial-intelligence/assignment-4/images/dog1.png")
print("")
print("")
print("Frog --")
classify("C:/Projects/artificial-intelligence/assignment-4/images/frog1.png")
print("")
print("")
print("Horse --")
classify("C:/Projects/artificial-intelligence/assignment-4/images/horse1.png")
print("")
print("")
print("Ship --")
classify("C:/Projects/artificial-intelligence/assignment-4/images/ship1.png")
print("")
print("")
print("Truck --")
classify("C:/Projects/artificial-intelligence/assignment-4/images/truck1.png")