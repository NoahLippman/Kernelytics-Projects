import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score, log_loss
from xgboost import XGBClassifier
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import StandardScaler

data = pd.read_csv('CornBelters/Data/2025.csv')
df = data[['PlayResult','TaggedHitType','ExitSpeed','Angle','Direction','HitSpinRate','Distance','Bearing','HangTime']]

# Ensure PlayResult is encoded and Error rows are dropped (for completeness)
df = df[df['PlayResult'] != 'Error']
df['PlayResult'] = df['PlayResult'].replace(['HomeRun', 'Single', 'Double', 'Triple'], 1)
df['PlayResult'] = df['PlayResult'].replace(['Out'], 0)

# Rebuild hit-type DataFrames to ensure consistency (keeping ExitSpeed)
dgb = df[df['TaggedHitType'].isin(['GroundBall'])].dropna().reset_index(drop=True)
dpu = df[df['TaggedHitType'].isin(['Popup'])].dropna().reset_index(drop=True)
dld = df[df['TaggedHitType'].isin(['LineDrive'])].dropna().reset_index(drop=True)
dfb = df[df['TaggedHitType'].isin(['FlyBall'])].dropna().reset_index(drop=True)

# Features for modeling (excluding TaggedHitType, which is non-numerical)
features = ['ExitSpeed', 'Angle', 'Direction', 'HitSpinRate', 'Distance', 'Bearing', 'HangTime']

# Dictionary to store models and scalers for each hit type
models = {}
scalers = {}
test_results = {}

# Train and evaluate models for each hit type
for df_hit, hit_type in [(dgb, 'GroundBall'), (dpu, 'Popup'), (dld, 'LineDrive'), (dfb, 'FlyBall')]:
    print(f"\nTraining model for {hit_type}...")
    
    # Prepare features and target
    X = df_hit[features]
    y = df_hit['PlayResult']
    
    # Train-test split
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)
    
    # Scale features (important for XGBoost to ensure consistent feature ranges)
    scaler = StandardScaler()
    X_train_scaled = scaler.fit_transform(X_train)
    X_test_scaled = scaler.transform(X_test)
    
    # Train XGBoost model
    xgb_model = XGBClassifier(use_label_encoder=False, eval_metric='logloss', random_state=42)
    xgb_model.fit(X_train_scaled, y_train)
    
    # Predict probabilities (xBA) on test set
    y_pred_proba = xgb_model.predict_proba(X_test_scaled)[:, 1]
    
    # Evaluate model
    auc = roc_auc_score(y_test, y_pred_proba)
    logloss = log_loss(y_test, y_pred_proba)
    print(f"{hit_type} AUC-ROC: {auc:.4f}")
    print(f"{hit_type} Log Loss: {logloss:.4f}")
    
    # Store model, scaler, and test results
    models[hit_type] = xgb_model
    scalers[hit_type] = scaler
    test_results[hit_type] = {'y_test': y_test, 'y_pred_proba': y_pred_proba}
    
    # Plot feature importance
    plt.figure(figsize=(8, 6))
    pd.Series(xgb_model.feature_importances_, index=features).sort_values().plot(kind='barh')
    plt.title(f'Feature Importance for {hit_type}')
    plt.xlabel('Importance')
    plt.show()

# Combine predictions to compute xBA for the entire dataset
def predict_xba(row, models, scalers):
    hit_type = row['TaggedHitType']
    if hit_type not in models:
        return np.nan  # Return NaN for unknown hit types
    features_values = row[features].values.reshape(1, -1)
    features_scaled = scalers[hit_type].transform(features_values)
    return models[hit_type].predict_proba(features_scaled)[0, 1]

# Apply predictions to the entire DataFrame
df['xBA'] = df.apply(lambda row: predict_xba(row, models, scalers), axis=1)
pd.save_csv('2025wxBA',index=False)
# Verify xBA values are between 0 and 1
print("\nxBA Summary:")
print(df['xBA'].describe())

# Plot predicted xBA vs. actual PlayResult for FlyBall (example)
plt.figure(figsize=(8, 6))
plt.scatter(test_results['FlyBall']['y_test'], test_results['FlyBall']['y_pred_proba'], alpha=0.5)
plt.xlabel('Actual PlayResult (0 = Out, 1 = Hit)')
plt.ylabel('Predicted xBA')
plt.title('Predicted xBA vs. Actual PlayResult (FlyBall)')
plt.show()

# Calibration curve for FlyBall (example)
from sklearn.calibration import calibration_curve
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
plt.show()

# Save models for future use
#import pickle
#for hit_type, model in models.items():
#    with open(f'xgb_model_{hit_type}.pkl', 'wb') as f:
#        pickle.dump(model, f)
#    with open(f'scaler_{hit_type}.pkl', 'wb') as f:
#        pickle.dump(scalers[hit_type], f)