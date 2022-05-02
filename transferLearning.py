# -*- coding: utf-8 -*-
"""
Created on Thu Mar 17 16:30:00 2022

@author: Guillaume WACQUET
"""

def transferLearning(modelName, trainPath, testPath, batch_size):
    
    ########## LIBRARIES IMPORTATION ##########
    import tensorflow as tf
    from tensorflow.keras.layers import Flatten, Dense, Dropout, Input, BatchNormalization
    from tensorflow.keras.preprocessing.image import ImageDataGenerator
    from tensorflow.keras import applications
    from tensorflow.keras.callbacks import EarlyStopping, ModelCheckpoint, ReduceLROnPlateau, CSVLogger
    from tensorflow.keras import Model
    from tensorflow.keras.optimizers import Adam
    from tensorflow.keras import backend as K
    import numpy as np
    from sklearn.model_selection import train_test_split
    from sklearn.preprocessing import label_binarize
    import pandas as pd
    
    if modelName == "DenseNet201":
        from tensorflow.keras.applications.densenet import DenseNet201
        from tensorflow.keras.applications.densenet import preprocess_input
    if modelName == "InceptionV3":
        from tensorflow.keras.applications.inception_v3 import InceptionV3
        from tensorflow.keras.applications.inception_v3 import preprocess_input
    if modelName == "ResNet50":
        from tensorflow.keras.applications.resnet50 import ResNet50
        from tensorflow.keras.applications.resnet50 import preprocess_input
    if modelName == "VGG16":
        from tensorflow.keras.applications.vgg16 import VGG16
        from tensorflow.keras.applications.vgg16 import preprocess_input
    if modelName == "VGG19":
        from tensorflow.keras.applications.vgg19 import VGG19
        from tensorflow.keras.applications.vgg19 import preprocess_input
    
    ########## GLOBAL VARIABLES ##########
    trainPath = "/home1/datawork/gwacquet/data_flowcam/train"
    testPath = "/home1/datawork/gwacquet/data_flowcam/test"
    save_dir = "/home1/datawork/gwacquet/data_flowcam/saved_models/"
    training_data = pd.read_csv("/home1/datawork/gwacquet/data_flowcam/train.csv", dtype=str)
    test_data = pd.read_csv("/home1/datawork/gwacquet/data_flowcam/test.csv", dtype=str)
    print(training_data.shape)
    print(test_data.shape)
    
    target_names = ['aggregates','Asterionellopsis','bubbles','C_curvisetus',
                    'C_danicus','C_socialis','Ciliophora','clear','Dactyliosolen',
                    'dark','Dytilum','fiber','G_flaccida','G_striata','Gymnodinium',
                    'Lauderia','Leptocylindrus','membranous','Odontella','P_globosa',
                    'Pleuro_Gyrosigma','Prorocentrum','PseudoNitzschia','Rhizosolenia',
                    'Thalassionema','Thalassiosira']
    
    ########## OBJECTS FOR MODEL ##########
    EPOCH = 20
    CLASS_SIZE = 26
    BATCH_SIZE = 20
    img_width, img_height = 128,128
    input_img = (128,128,3)
    num_of_test_samples = 15242
    
    if K.image_data_format() == 'channels_first':
        input_img = (3, img_width, img_height)
    else:
        input_img = (img_width, img_height, 3)
    
    ########## ALLOW MEMORY GROWTH ON GPU ##########
    physical_devices = tf.config.experimental.list_physical_devices('GPU')
    try:
        tf.config.experimental.set_memory_growth(physical_devices[0], True)
    except:
        #Invalid device
        pass
    
    ########## MODEL METRICS ##########
    def recall_m(y_true, y_pred):
        global recall
        true_positives = K.sum(K.round(K.clip(y_true * y_pred, 0, 1)))
        possible_positives = K.sum(K.round(K.clip(y_true, 0, 1)))
        recall = true_positives / (possible_positives + K.epsilon())
        return recall
    
    def precision_m(y_true, y_pred):
        global precision
        true_positives = K.sum(K.round(K.clip(y_true * y_pred, 0, 1)))
        predicted_positives = K.sum(K.round(K.clip(y_pred, 0, 1)))
        precision = true_positives / (predicted_positives + K.epsilon())
        return precision
    
    def f1_m(y_true, y_pred):
        global f1
        precision = precision_m(y_true, y_pred)
        recall = recall_m(y_true, y_pred)
        f1 = 2*((precision*recall)/(precision+recall+K.epsilon()))
        return f1
    
    Metrics = [
            tf.keras.metrics.CategoricalAccuracy(name='accuracy'),
            tf.keras.metrics.Recall(name='recall'),
            tf.keras.metrics.Precision(name='precision'),
            f1_m,
            tf.keras.metrics.AUC(name='auc'),
            tf.keras.metrics.FalseNegatives(name='fn'),
            tf.keras.metrics.TrueNegatives(name='tn'),
            tf.keras.metrics.TruePositives(name='tp'),
            tf.keras.metrics.FalsePositives(name='fp'),
            ]
    
    ########## MODEL ##########
    def DenseNet201_model():
        global model
        input_tensor = Input(shape=(224, 224, 3))
        #Transfer learning
        DenseNet201_model = applications.DenseNet201(weights="/home1/datawork/gwacquet/densenet201_weights_tf_dim_ordering_tf_kernels_notop.h5",
                                                     include_top=False,
                                                     input_tensor=input_tensor,
                                                     pooling='max')
        #Freeze all previous layer for first rounds of training
        for layer in DenseNet201_model.layers:
            if isinstance(layer, BatchNormalization):
                layer.trainable = True
            else:
                layer.trainable = False
                
        # On top of the last last max pooling layer
        # add: flatten layer, two fully connected (fc) layers, implement dropout, softmax classifier
        last = DenseNet201_model.layers[-1].output
        x = Flatten()(last)
        x = Dense(1024, activation='relu', name='fc1')(x)
        x = Dropout(0.3)(x)
        x = Dense(CLASS_SIZE, activation='softmax', name='predictions')(x)
        model = Model(DenseNet201_model.input, x)
        # Compile the model
        model.compile(optimizer=Adam(lr=0.01), loss='categorical_crossentropy', metrics=[Metrics])
        return model
    
    model = DenseNet201_model()
    model.summary()
    
    ########## SKF, IMAGE GENERATOR, COMPILE AND FIT ##########
    Train_Accuracy = []
    Train_Precision = []
    Train_Recall = []
    Train_F1 = []
    Train_fpr = []
    Train_loss = []
    
    Val_Accuracy = []
    Val_Precision = []
    Val_Recall = []
    Val_F1 = []
    Val_auc = []
    Val_Loss = []
    Val_fpr = []
    
    acc_scores = []
    precis_scores = []
    rec_scores = []
    f1_scores = []
    
    #Stratified Kfold cross validation, and random state for reproducibility
    #skf = StratifiedKFold(n_splits=k, random_state=7, shuffle=True)
    Y = training_data[['Label']]
    X = training_data[['Image']]
    Y_test = test_data[['Label']].values
    # binarize the labels for ROC calculation
    Y_test_binarized = label_binarize(Y_test, classes=target_names)
    X_test = test_data[['Image']].values
    
    generator = ImageDataGenerator(preprocessing_function=preprocess_input, 
                                   rotation_range=45,
                                   fill_mode='nearest',
                                   horizontal_flip=True)
                                   #, shear_range = 0.2,
                                   #zoom_range = 0.2,
                                   #width_shift_range=0.2,
                                   #height_shift_range=0.2)
                               
    def get_model_name(k):
        global mod_name
        mod_name = 'model_DenseNet201_' + str(k) + '.h5'
        return mod_name
    fold_var = 1
    
    a = np.array(range(0,training_data.shape[0]))
    train_dat, valid_dat, y_train, y_valid = train_test_split(training_data, a, 
                                                              random_state=7, shuffle=True)
    train_data = training_data.iloc[y_train]
    valid_data = training_data.iloc[y_valid]
    
    train_data_generator = generator.flow_from_dataframe(dataframe=train_data, directory=trainPath,
                                                         x_col="Image", y_col="Label", target_size=input_img[:-1],
                                                         color_mode="rgb", class_mode="categorical", classes=target_names,
                                                         batch_size=BATCH_SIZE, shuffle=True)
    valid_data_generator = generator.flow_from_dataframe(dataframe=valid_data, directory=trainPath,
                                                         x_col="Image", y_col="Label", target_size=input_img[:-1],
                                                         color_mode="rgb", class_mode="categorical", classes=target_names,
                                                         batch_size=BATCH_SIZE, shuffle=True)
    model = DenseNet201_model()
    # Compile
    model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=[Metrics])
    # Create callbacks
    # ReduceLROnPlateau: Reduce learning rate when auc stops improving for x epochs
    # ModelCheckpoint:
    # Early stopping: Cease training loop if after x epochs achieves no defined improvement
    RLRP = ReduceLROnPlateau(
            monitor="val_loss",
            factor=0.1,
            patience=5,
            verbose=1,
            mode="min",
            min_delta=0.0001)
    CSV = CSVLogger(
            filename='history_DenseNet201.csv',
            separator=';',
            append=True)
    #Checkpoint = ModelCheckpoint(
    #    filepath=save_dir + get_model_name(fold_var),
    #    monitor='val_loss',
    #    verbose=1,
    #    save_best_only=True, mode='min')
    Checkpoint = ModelCheckpoint(
            filepath=save_dir + get_model_name(fold_var),
            verbose=1,
            save_freq ='epoch')
    Stopping = EarlyStopping(
            monitor='val_loss',
            min_delta=1e-3,
            patience=10,
            verbose=1,
            mode='min',
            restore_best_weights=True)
    callbacks = [
            RLRP,
            CSV,
            Checkpoint,
            Stopping
    ]
    
    # This saves the best model
    # FIT THE MODEL
    history = model.fit(train_data_generator,
                        epochs=EPOCH,
                        steps_per_epoch=train_data_generator.samples/BATCH_SIZE,
                        validation_data=valid_data_generator,
                        validation_steps= valid_data_generator.samples/BATCH_SIZE,
                        callbacks=callbacks,
                        verbose=2)
    
    # LOAD BEST MODEL to evaluate the performance of the model
    model.load_weights(save_dir + "model_DenseNet201_" + str(fold_var) + ".h5")
    
    Train_Accuracy.append(history.history['accuracy'])
    Train_Precision.append(history.history['precision'])
    Train_Recall.append(history.history['recall'])
    Train_F1.append(history.history['f1_m'])
    Train_fpr.append(history.history['fp'])
    Train_loss.append(history.history['loss'])
    
    Val_Accuracy.append(history.history['val_accuracy'])
    Val_Precision.append(history.history['val_precision'])
    Val_Recall.append(history.history['val_recall'])
    Val_F1.append(history.history['val_f1_m'])
    Val_auc.append(history.history['val_auc'])
    Val_Loss.append(history.history['val_loss'])
    Val_fpr.append(history.history['val_fp'])
    
    test_generator = generator.flow_from_dataframe(dataframe=test_data, directory=testPath,
                                                   x_col="Image", y_col="Label", target_size=input_img[:-1],
                                                   color_mode="rgb", class_mode="categorical", classes=target_names,
                                                   batch_size=BATCH_SIZE, shuffle=False)
    Score = model.evaluate(x=test_generator, steps=num_of_test_samples / BATCH_SIZE, verbose=1)
    # evaluate the model
    acc_scores.append(Score[1])
    rec_scores.append(Score[2])
    precis_scores.append(Score[3])
    f1_scores.append(Score[4])