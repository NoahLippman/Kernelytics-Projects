# xBA.py: Compute expected batting average (xBA) for college baseball using XGBoost
# Saves all plots as PNGs in an 'xBA' directory instead of displaying them

# Import libraries
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score, log_loss
from sklearn.preprocessing import StandardScaler
from xgboost import XGBClassifier
import matplotlib.pyplot as plt
import seaborn as sns
import pickle
from sklearn.calibration import calibration_curve
import os

# Create directory for saving plots
plot_dir = 'xBA'
if not os.path.exists(plot_dir):
    os.makedirs(plot_dir)

# Load data with specified dtypes to handle mixed types
dtypes = {
    'PlayResult': 'object',
    'TaggedHitType': 'object',
    'ExitSpeed': 'float64',
    'Angle': 'float64',
    'Direction': 'float64',
    'HitSpinRate': 'float64',
    'Distance': 'float64',
    'Bearing': 'float64',
    'HangTime': 'float64'
}
data = pd.read_csv('CornBelters/Data/2025.csv', dtype=dtypes, low_memory=False)

# Select relevant columns
df = data[['PlayResult', 'TaggedHitType', 'ExitSpeed', 'Angle']]

# Inspect PlayResult for unexpected values
print("Unique PlayResult values before cleaning:", df['PlayResult'].unique())
print("PlayResult value counts:\n", df['PlayResult'].value_counts())
print("Missing values in PlayResult:", df['PlayResult'].isna().sum())

# Clean PlayResult: Keep only valid outcomes, encode to 0/1
valid_hits = ['HomeRun', 'Single', 'Double', 'Triple']
df = df[df['PlayResult'].isin(valid_hits + ['Out'])]  # Remove invalid outcomes (e.g., Error)
df['PlayResult'] = df['PlayResult'].replace(valid_hits, 1)
df['PlayResult'] = df['PlayResult'].replace('Out', 0)

# Convert PlayResult to numeric to ensure no strings remain
df['PlayResult'] = pd.to_numeric(df['PlayResult'], errors='coerce')
print("Missing PlayResult after encoding:", df['PlayResult'].isna().sum())
df = df.dropna(subset=['PlayResult'])
df['PlayResult'] = df['PlayResult'].astype('int64')

# Verify cleaning
print("Unique PlayResult values after cleaning:", df['PlayResult'].unique())
print("PlayResult data type:", df['PlayResult'].dtype)

# Split into hit-type DataFrames
dgb = df[df['TaggedHitType'] == 'GroundBall'].dropna().reset_index(drop=True)
dpu = df[df['TaggedHitType'] == 'Popup'].dropna().reset_index(drop=True)
dld = df[df['TaggedHitType'] == 'LineDrive'].dropna().reset_index(drop=True)
dfb = df[df['TaggedHitType'] == 'FlyBall'].dropna().reset_index(drop=True)

# Check shapes and PlayResult distribution
print("GroundBall shape:", dgb.shape, "\nPlayResult counts:\n", dgb['PlayResult'].value_counts())
print("Popup shape:", dpu.shape, "\nPlayResult counts:\n", dpu['PlayResult'].value_counts())
print("LineDrive shape:", dld.shape, "\nPlayResult counts:\n", dld['PlayResult'].value_counts())
print("FlyBall shape:", dfb.shape, "\nPlayResult counts:\n", dfb['PlayResult'].value_counts())
print("Columns:", df.columns.tolist())
print("Data types:\n", df.dtypes)

# Define features for modeling (excluding non-numerical TaggedHitType)
features = ['ExitSpeed', 'Angle']

# Initialize dictionaries to store models, scalers, and test results
models = {}
scalers = {}
test_results = {}

# Train and evaluate models for each hit type
for df_hit, hit_type in [(dgb, 'GroundBall'), (dpu, 'Popup'), (dld, 'LineDrive'), (dfb, 'FlyBall')]:
    print(f"\nTraining model for {hit_type}...")
    X = df_hit[features]
    y = df_hit['PlayResult']
    
    # Check if enough data and both classes exist
    if len(df_hit) < 10:
        print(f"Warning: {hit_type} has too few samples: {len(df_hit)}")
        continue
    if len(y.unique()) < 2:
        print(f"Warning: {hit_type} has only one class: {y.unique()}")
        continue
    
    # Train-test split with stratification
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42, stratify=y)
    print(f"{hit_type} test set size:", len(y_test))
    print(f"{hit_type} test set classes:", y_test.unique())
    
    # Scale features
    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train)
    X_test_scaled = scaler.transform(X_test)
    
    # Train XGBoost
    xgb_model = XGBClassifier(eval_metric='logloss', random_state=42)
    xgb_model.fit(X_train_scaled, y_train)
    
    # Predict probabilities (xBA)
    y_pred_proba = xgb_model.predict_proba(X_test_scaled)[:, 1]
    
    # Evaluate if both classes are present
    if len(np.unique(y_test)) > 1:
        print(f"{hit_type} AUC-ROC:", roc_auc_score(y_test, y_pred_proba))
        print(f"{hit_type} Log Loss:", log_loss(y_test, y_pred_proba))
    else:
        print(f"Cannot compute AUC-ROC: Only one class in {hit_type} test set")
    
    # Store model, scaler, and results
    models[hit_type] = xgb_model
    scalers[hit_type] = scaler
    test_results[hit_type] = {'y_test': y_test, 'y_pred_proba': y_pred_proba}
    
    # Save feature importance plot
    plt.figure(figsize=(8, 6))
    pd.Series(xgb_model.feature_importances_, index=features).sort_values().plot(kind='barh')
    plt.title(f'Feature Importance for {hit_type}')
    plt.xlabel('Importance')
    plt.savefig(os.path.join(plot_dir, f'feature_importance_{hit_type}.png'))
    plt.close()

# Function to predict xBA for a single row
def predict_xba(row, models, scalers):
    hit_type = row['TaggedHitType']
    if hit_type not in models:
        return np.nan
    features_values = row[features].values.reshape(1, -1)
    features_scaled = scalers[hit_type].transform(features_values)
    return models[hit_type].predict_proba(features_scaled)[0, 1]

# Apply predictions
df['xBA'] = df.apply(lambda row: predict_xba(row, models, scalers), axis=1)

# Verify xBA
print("\nxBA Summary:")
print(df['xBA'].describe())

# Save scatter plot for FlyBall
if 'FlyBall' in test_results:
    plt.figure(figsize=(8, 6))
    plt.scatter(test_results['FlyBall']['y_test'], test_results['FlyBall']['y_pred_proba'], alpha=0.5)
    plt.xlabel('Actual PlayResult (0 = Out, 1 = Hit)')
    plt.ylabel('Predicted xBA')
    plt.title('Predicted xBA vs. Actual PlayResult (FlyBall)')
    plt.savefig(os.path.join(plot_dir, 'scatter_flyball.png'))
    plt.close()

# Save calibration curve for FlyBall
if 'FlyBall' in test_results and len(np.unique(test_results['FlyBall']['y_test'])) > 1:
    prob_true, prob_pred = calibration_curve(test_results['FlyBall']['y_test'], 
                                             test_results['FlyBall']['y_pred_proba'], 
                                             n_bins=10)
    plt.figure(figsize=(8, 6))
    plt.plot(prob_pred, prob_true, marker='o', label='Calibration Curve')
    plt.plot([0, 1], [0, 1], linestyle='--', label='Perfectly Calibrated')
    plt.xlabel('Predicted Probability (xBA)')
    plt.ylabel('True Probability of Hit')
    plt.title('Calibration Curve for FlyBall')
    plt.legend()
    plt.savefig(os.path.join(plot_dir, 'calibration_flyball.png'))
    plt.close()
else:
    print("Cannot save calibration curve: FlyBall model not trained or single class")

if 'LineDrive' in test_results and len(np.unique(test_results['LineDrive']['y_test'])) > 1:
    prob_true, prob_pred = calibration_curve(test_results['LineDrive']['y_test'], 
                                             test_results['LineDrive']['y_pred_proba'], 
                                             n_bins=10)
    plt.figure(figsize=(8, 6))
    plt.plot(prob_pred, prob_true, marker='o', label='Calibration Curve')
    plt.plot([0, 1], [0, 1], linestyle='--', label='Perfectly Calibrated')
    plt.xlabel('Predicted Probability (xBA)')
    plt.ylabel('True Probability of Hit')
    plt.title('Calibration Curve for LineDrive')
    plt.legend()
    plt.savefig(os.path.join(plot_dir, 'calibration_LineDrive.png'))
    plt.close()
else:
    print("Cannot save calibration curve: LineDrive model not trained or single class")

# Save models and scalers
for hit_type, model in models.items():
    with open(f'CornBelters/xBA/xgb_model_{hit_type}.pkl', 'wb') as f:
        pickle.dump(model, f)
    with open(f'CornBelters/xBA/scaler_{hit_type}.pkl', 'wb') as f:
        pickle.dump(scalers[hit_type], f)
print("Models and scalers saved successfully.")

# Save DataFrame with xBA
df.to_csv('CornBelters/xBA/2025_with_xBA.csv', index=False)
print("DataFrame with xBA saved to 2025_with_xBA.csv")