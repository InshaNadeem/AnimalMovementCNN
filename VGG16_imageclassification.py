import numpy as np
import os
from keras.src.applications.vgg16 import VGG16, preprocess_input
from keras.api.preprocessing.image import img_to_array, load_img
from keras.src.models import Model
from sklearn.cluster import KMeans, DBSCAN, AgglomerativeClustering

# Preprocess the Images and Extract Features Using CNNs-VGG16 (here)
base_model = VGG16(weights='imagenet')
model = Model(inputs=base_model.input, outputs=base_model.get_layer('fc1').output)

def extract_features(image_path, model):
    img = load_img(image_path, target_size=(224, 224))
    img = img_to_array(img)
    img = np.expand_dims(img, axis=0)
    img = preprocess_input(img)
    features = model.predict(img)
    return features.flatten()

def extract_label(image_name):
   return int(image_name.split('_')[0].replace('model', ''))

image_dir = 'C:/Users/nadeem56/Desktop/INadeem/Artificial_Dataset' 
feature_list = []
true_labels = []
image_names = []

for image_name in os.listdir(image_dir):
    if image_name.endswith('.png'): 
        image_path = os.path.join(image_dir, image_name)
        features = extract_features(image_path, model)
        feature_list.append(features)
        true_labels.append(extract_label(image_name))
        image_names.append(image_name)

feature_array = np.array(feature_list)
true_labels = np.array(true_labels)

# Step 2: Cluster the Images
num_clusters = 4  # Adjust the number of clusters as needed

# K-Means Clustering
kmeans = KMeans(n_clusters=num_clusters, random_state=42)
kmeans_labels = kmeans.fit_predict(feature_array)+ 1

# DBSCAN Clustering
dbscan = DBSCAN(eps=0.5, min_samples=5)  # Adjust parameters as needed
dbscan_labels = dbscan.fit_predict(feature_array)

# Hierarchical Clustering
hierarchical = AgglomerativeClustering(n_clusters=num_clusters)  # Adjust the number of clusters as needed
hierarchical_labels = hierarchical.fit_predict(feature_array)

# Step 3: Evaluate the Clusters using Correctness Ratios
def calculate_correctness_ratio(y_true, y_pred, num_clusters):
    correctness_ratios = []
    for true_label in range(1, num_clusters + 1):
        ni = np.sum(y_true == true_label)
        ni_bar = np.sum((y_true == true_label) & (y_pred == true_label))
        correctness_ratio = ni_bar / ni if ni > 0 else 0
        correctness_ratios.append(correctness_ratio)
    average_correctness_ratio = np.mean(correctness_ratios)
    return average_correctness_ratio

# Calculate correctness ratios for each clustering method
kmeans_correctness_ratio = calculate_correctness_ratio(true_labels, kmeans_labels, num_clusters)
dbscan_correctness_ratio = calculate_correctness_ratio(true_labels, dbscan_labels, num_clusters)
hierarchical_correctness_ratio = calculate_correctness_ratio(true_labels, hierarchical_labels, num_clusters)

print("True Results",true_labels)
print("predicted Results",kmeans_labels)
# Step 4: Compare the Models
print(f'K-Means Average Correctness Ratio: {kmeans_correctness_ratio}')
print(f'DBSCAN Average Correctness Ratio: {dbscan_correctness_ratio}')
print(f'Hierarchical Clustering Average Correctness Ratio: {hierarchical_correctness_ratio}')