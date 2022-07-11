# -*- coding: utf-8 -*-
"""
Created on Thu Mar 17 16:30:00 2022

@author: Guillaume WACQUET
"""

def imgClassification(modelName, imgPath, modelPath, batch_size, 
                      img_width, img_height):
    
    ########## LIBRARIES IMPORTATION ##########
    from tensorflow.keras.preprocessing.image import ImageDataGenerator
    from tensorflow.keras.models import load_model
    from tensorflow.keras import backend as K
    import numpy as np
    
    if modelName == "DenseNet201":
        from tensorflow.keras.applications.densenet import preprocess_input
    if modelName == "InceptionV3":
        from tensorflow.keras.applications.inception_v3 import preprocess_input
    if modelName == "ResNet50":
        from tensorflow.keras.applications.resnet50 import preprocess_input
    if modelName == "VGG16":
        from tensorflow.keras.applications.vgg16 import preprocess_input
    if modelName == "VGG19":
        from tensorflow.keras.applications.vgg19 import preprocess_input
        
    input_img = (img_width,img_height,3)
    if K.image_data_format() == 'channels_first':
        input_img = (3, img_width, img_height)
    else:
        input_img = (img_width, img_height, 3)
        
    ########## METRICS ##########
    def precision_m(y_true, y_pred):
        true_positives = K.sum(K.round(K.clip(y_true * y_pred, 0, 1)))
        predicted_positives = K.sum(K.round(K.clip(y_pred, 0, 1)))
        precision = true_positives / (predicted_positives + K.epsilon())
        return precision
    
    def recall_m(y_true, y_pred):
        true_positives = K.sum(K.round(K.clip(y_true * y_pred, 0, 1)))
        possible_positives = K.sum(K.round(K.clip(y_true, 0, 1)))
        recall = true_positives / (possible_positives + K.epsilon())
        return recall
    
    def f1_m(y_true, y_pred):
        precision = precision_m(y_true, y_pred)
        recall = recall_m(y_true, y_pred)
        return 2*((precision*recall)/(precision+recall+K.epsilon()))
    
    ########## MODEL ##########
    model = load_model(modelPath, custom_objects = {'f1_m': f1_m})
    
    ########## PREDICTION ##########
    generator = ImageDataGenerator(preprocessing_function=preprocess_input)
    prediction_generator = generator.flow_from_directory(imgPath,
                                                         target_size=input_img[:-1],
                                                         color_mode="rgb",
                                                         batch_size = batch_size,
                                                         class_mode="categorical", 
                                                         shuffle=False)
    
    test_steps_per_epoch = np.math.ceil(prediction_generator.samples / 
                                        prediction_generator.batch_size)
    predictions = model.predict(prediction_generator)
    imgNames = prediction_generator.filenames
    
    return predictions, imgNames
